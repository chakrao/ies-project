unit LuaRemoteExecutor;
{
Remote Executor is a thread that runs inside the target process. It waits for a "HasFunctionAndParametersReadyForExecution" event, and executes it according to ExecuteMethod parameters

Each executor has it's own parameter store in shared memory accessible by both CE and the target, and only executes one function at a time

A process can have more than one executor thread at the same time

}

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}

  {$ifdef darwin}
  macport,
  {$endif}

  Classes, SysUtils, math;    //todo: port to mac

type
  TStubdata=record
    address: ptruint;
    parameters: array of integer;
  end;



type
  TRemoteExecutorSharedMemory=record
    HasDataEventHandle: QWORD;       //0
    HasProcessedDataEventHandle: QWORD; //8
    Command: QWORD; //10
    Address: QWORD; //18  (inithandle)
    ReturnValue: QWORD;
    ParamStart: byte;
    //....   (pointers to strings will point inside this block, based on the target process's executor map address)
  end;

  PRemoteExecutorSharedMemory=^TRemoteExecutorSharedMemory;

  TExecBuffer=record
    parameternr: integer;
    data: ptruint;
    length: integer;
  end;

  Tvalueinfo=record
    value: qword;
    bytesize: integer; //only used for bytetables
  end;

  TExecBufferArray=array of TExecBuffer;


  TRemoteExecutor=class
  private
    targetedProcessID: dword;
    HasDataEventHandle: THandle;
    HasProcessedDataEventHandle: THandle;

    sharedMemorySize: integer;
    sharedMemory: PRemoteExecutorSharedMemory;
    sharedMemoryClientLocation: QWORD;


    memmap: THandle;
    remoteMemMapHandle: THandle;

    ExecutorThreadHandle: THandle;
    ExecutorThreadExecMemory: ptruint;

    lastBuffers: TExecBufferArray;
    procedure waitForProcessedDataEvent(timeout: dword);
    procedure growSharedMemorySize(newsize: integer; timeout: DWORD);
    function LocalToRemoteAddress(LocalAddress: ptruint): ptruint;
  public
    procedure executeStub(stubdata: TStubdata; values: array of Tvalueinfo; timeout: dword);
    function waitTillDoneAndGetResult(timeout: integer; var Buffers: TExecBufferArray): QWORD;
    constructor create;
    destructor destroy; override;
  end;

procedure InitializeLuaRemoteExecutor;

implementation

uses ProcessHandlerUnit, autoassembler, commonTypeDefs, lua, luahandler, luaclass,
  LuaObject, LuaByteTable, Clipbrd;

const
  CMD_RELOADMEM = 0;
  CMD_EXECUTE = 1;
  CMD_QUIT = 2;


procedure TRemoteExecutor.waitForProcessedDataEvent(timeout: dword);
//the HasProcessedDataEventHandle is a manualreset event, so this can be called whenever
var
  starttime: qword;
  r: dword;
begin
  {$ifdef windows}
  starttime:=gettickcount64;
  repeat
    r:=WaitForSingleObject(HasProcessedDataEventHandle, min(timeout,100));
    if (r<>WAIT_OBJECT_0) and (timeout>0) then
      CheckSynchronize;
  until (r<>WAIT_TIMEOUT) or (timeout=0) or ((timeout<>$ffffffff) and (gettickcount64>starttime+timeout));

  if r<>WAIT_OBJECT_0 then
    raise exception.Create('Timeout');
  {$else}
  raise exception.Create('Not yet implemented');
  {$endif}
end;

function TRemoteExecutor.waitTillDoneAndGetResult(timeout: integer; var Buffers: TExecBufferArray): QWORD;
begin
  waitForProcessedDataEvent(timeout);

  buffers:=lastBuffers;
  result:=sharedMemory^.ReturnValue;
end;

function TRemoteExecutor.LocalToRemoteAddress(LocalAddress: ptruint): ptruint;
begin
  result:=sharedMemoryClientLocation+(LocalAddress-ptruint(sharedMemory));
end;

procedure TRemoteExecutor.executeStub(stubdata: TStubdata; values: array of Tvalueinfo; timeout: dword);
var
  i: integer;
  r: dword;
  len: integer;

  paramsizeneeded: integer; //after this space the buffers can be placed

  sizeneeded: integer;

  currentParam: ptruint;

  dataPosition: ptruint;

  ExecBuffer: TExecBuffer;
begin
  {$ifdef windows}
  if length(stubdata.parameters)<>length(values) then
    raise exception.create('Incorrect parameter count');

  sizeneeded:=0;
  paramsizeneeded:=0;

  //size check
  for i:=0 to length(stubdata.parameters)-1 do
  begin
    //0: integer/pointer
    //1: float
    //2: double
    //3: asciistring (turns into 0:pointer after writing the string)
    //4: widestring

    //<0: pointer to input/output buffer:
    //  bit31: Pointer to buffer
    //  bit30: Output Only. Needs no init
    //  bit29: Input Only. Don't return when getting results



    case stubdata.parameters[i] of
      0,1: //integer/pointer/float
      begin
        inc(sizeneeded, processhandler.pointersize);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      2: //double
      begin
        inc(sizeneeded, 8);
        inc(paramsizeneeded,8); //32-bit uses 8 bytes for double params
      end;
      3:
      begin
        inc(sizeneeded, processhandler.pointersize+strlen(pchar(values[i].value))+32);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      4:
      begin
        inc(sizeneeded, processhandler.pointersize+StrLen(pwidechar(values[i].value))+32);
        inc(paramsizeneeded, processhandler.pointersize);
      end;

      else
      begin
        if stubdata.parameters[i]<0 then
          inc(sizeneeded, processhandler.pointersize+32+values[i].bytesize)
        else
          inc(sizeNeeded, processhandler.pointersize);

        inc(paramsizeneeded, processhandler.pointersize);
      end;
    end;
  end;

  if sizeneeded>sharedMemorySize then
    growSharedMemorySize(sizeneeded*2+1024, timeout);

  //enter the stub data into shared memory
  //still here, so everything is ok

  waitForProcessedDataEvent(timeout);
  //fill in the call data

  sharedMemory^.Command:=CMD_EXECUTE;
  sharedMemory^.Address:=stubdata.address;
  sharedMemory^.ReturnValue:=0;
  currentParam:=ptruint(@sharedMemory^.ParamStart);
  dataPosition:=align(ptruint(@sharedMemory^.ParamStart)+paramsizeneeded,16);

  setlength(lastbuffers,0);

  for i:=0 to length(stubdata.parameters)-1 do
  begin
    //0: integer/pointer
    //1: float
    //2: double
    //3: asciistring (turns into 0:pointer after writing the string)
    //4: widestring

    case stubdata.parameters[i] of
      0,1: //integer/pointer/float
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=values[i].value
        else
          pdword(currentparam)^:=values[i].value;

        inc(currentparam, processhandler.pointersize);
      end;

      2: //double
      begin
        pqword(currentparam)^:=values[i].value;
        inc(currentparam, 8);
      end;

      3: //pchar
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
        else
          pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


        len:=strlen(pchar(values[i].value))+1;
        strcopy(pchar(dataPosition),pchar(values[i].value));
        dataposition:=align(DataPosition+len,16);

        inc(currentparam, processhandler.pointersize);
      end;

      4: //widechar
      begin
        if processhandler.is64Bit then
          pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
        else
          pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


        len:=strlen(pwidechar(values[i].value))+2;
        strcopy(pwidechar(dataPosition),pwidechar(values[i].value));
        dataposition:=align(DataPosition+len,16);

        inc(currentparam, processhandler.pointersize);
      end;

      else
      begin
        if stubdata.parameters[i]<0 then
        begin
          len:=values[i].bytesize;
          if processhandler.is64Bit then
            pqword(currentparam)^:=LocalToRemoteAddress(DataPosition)
          else
            pdword(currentparam)^:=LocalToRemoteAddress(DataPosition);


          if (stubdata.parameters[i] and (1 shl 29))=0 then //it's not an input only param, record it for output
          begin
            Execbuffer.parameternr:=i;
            Execbuffer.length:=len;
            Execbuffer.data:=DataPosition;

            setlength(lastbuffers, length(lastBuffers)+1);
            lastbuffers[length(lastBuffers)-1]:=ExecBuffer;
          end;


          if (stubdata.parameters[i] and (1 shl 30))=0 then //not an output only param, 
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


          if (stubdata.parameters[i] and (1 shl 30))=0 then //not an output only param, copy the data
            copymemory(pointer(dataPosition),pointer(values[i].value),len);

          dataposition:=align(DataPosition+len,16);

          inc(currentparam, processhandler.pointersize);
        end
        else
        begin
          //unknown type
          if processhandler.is64Bit then
            pqword(currentparam)^:=values[i].value
          else
            pdword(currentparam)^:=values[i].value;

          inc(currentparam, processhandler.pointersize);
        end;
      end;
    end;

  end;


  ResetEvent(HasProcessedDataEventHandle);
  SetEvent(HasDataEventHandle);  //do the call command

  {$else}
  raise exception.create('Not yet implemented');
  {$endif}
end;

procedure TRemoteExecutor.growSharedMemorySize(newsize: integer; timeout: DWORD);
var
  newmemmap: THandle;
  newSharedMemory: pointer;

  oldSharedMemory: PRemoteExecutorSharedMemory;
  oldmemmap: THandle;
begin
  {$ifdef windows}
  if newsize>sharedMemorySize then
  begin
    oldSharedMemory:=sharedmemory;
    oldmemmap:=memmap;


    waitForProcessedDataEvent(timeout); //in case there was a previous no-wait call going on


    newmemmap:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,newsize,nil);
    if (newmemmap=0) or (newmemmap=INVALID_HANDLE_VALUE) then raise exception.create('Remap: Failure creating remote executor filemapping');

    newsharedMemory:=MapViewOfFile(newmemmap,FILE_MAP_READ or FILE_MAP_WRITE,0,0,newsize);
    if newsharedMemory=nil then
      raise exception.create('Remap: Failure mapping memorymap into memory');

    if DuplicateHandle(GetCurrentProcess, newmemmap, processhandle, @remoteMemMapHandle, 0, false, DUPLICATE_SAME_ACCESS )=false then
      raise exception.create('Remap: Failure duplicating memmap handle');

    zeromemory(newSharedMemory, newsize);
    copymemory(newSharedMemory, sharedmemory, sharedmemorysize);


    sharedMemory^.Address:=remoteMemMapHandle;
    sharedMemory^.Command:=CMD_RELOADMEM;

    ResetEvent(HasProcessedDataEventHandle);
    setevent(HasDataEventHandle);

    waitForProcessedDataEvent(timeout);

    //after this, the map has swapped to the new one, so the return is in the new one
    memmap:=newmemmap;
    sharedMemory:=newSharedMemory;
    sharedMemoryClientLocation:=sharedmemory^.ReturnValue;
    sharedmemorysize:=newsize;

    UnmapViewOfFile(oldsharedMemory);
    CloseHandle(oldmemmap);


  end;
  {$endif}
end;

constructor TRemoteExecutor.create;
var
  h: THandle;

  disableinfo: TDisableInfo;
  executorThreadID: dword;
  script: TStringlist=nil;
begin
  {$ifdef windows}
  sharedmemorysize:=5*8; //sizeof(TRemoteExecutorSharedMemory); //can grow if needed
  memmap:=CreateFileMapping(INVALID_HANDLE_VALUE,nil,PAGE_READWRITE,0,sharedmemorysize,nil);
  if (memmap=0) or (memmap=INVALID_HANDLE_VALUE) then raise exception.create('Failure creating remote executor filemapping');

  sharedMemory:=MapViewOfFile(memmap,FILE_MAP_READ or FILE_MAP_WRITE,0,0,sharedmemorysize);
  if sharedmemory=nil then
    raise exception.create('Failure mapping memorymap into memory');

  zeromemory(sharedMemory, sharedmemorysize);


  HasDataEventHandle:=CreateEvent(nil,false,false,nil);
  HasProcessedDataEventHandle:=CreateEvent(nil,true,false,nil);

  if DuplicateHandle(GetCurrentProcess, HasDataEventHandle, processhandle, @(sharedmemory^.HasDataEventHandle), 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasDataEventHandle');

  if  DuplicateHandle(GetCurrentProcess, HasProcessedDataEventHandle, processhandle, @(sharedmemory^.HasProcessedDataEventHandle), 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasProcessedDataEventHandle');

  if  DuplicateHandle(GetCurrentProcess, MemMap, processhandle, @remoteMemMapHandle, 0, false, DUPLICATE_SAME_ACCESS )=false then
    raise exception.create('Failure duplicating HasProcessedDataEventHandle');


  //inject the executor code
  disableinfo:=TDisableInfo.create;
  script:=tstringlist.create;
  try
    script.add('alloc(Executor,2048)');
    script.add('Executor:'); //At entrypoint the first param ([esp+4 or RCX) contains a filemapping handle (created by ce and duplicated to the target, like veh)
    if processhandler.is64Bit then
    begin
      script.add('sub rsp,18');  //allocate local vars
      script.add('mov rbp,rsp'); //[rbp: +0-7: storage of the mapped memory address, 8-f: filemapping handle; 10-17: old filehandle on reinit; 18-1f: return address; 20-28: scratchspace for param1
      script.add('mov [rbp],0');  //mapped base address
      script.add('mov [rbp+8],rcx'); //filemapping handle
      //rbp+10 will contain the old 
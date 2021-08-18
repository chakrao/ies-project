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

function TRemoteExecutor.LocalToRemote
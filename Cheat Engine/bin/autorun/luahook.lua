--Routine that scans and hooks lua functions

--return debug.getinfo(function).source



--luaTypes-- 
LUA_TNONE          = -1
LUA_TNIL           = 0
LUA_TBOOLEAN       = 1
LUA_TLIGHTUSERDATA = 2
LUA_TNUMBER        = 3
LUA_TSTRING        = 4
LUA_TTABLE         = 5
LUA_TFUNCTION      = 6
LUA_TUSERDATA      = 7
LUA_TTHREAD        = 8
LUA_NUMTAGS        = 9
  

function hookLua()
  --get the most commonly used lua functions
  
  --create an executor
  LuaExecutor=createRemoteExecutor()
  
  
  GetCurrentThreadIDStub=createExecuteCodeExStub(0,'GetCurrentThreadID') --todo, add an Executor.ThreadID...
  LuaExecutorThreadID=LuaExecutor.executeStub(GetCurrentThreadIDStub)
  
  
  LuaMutexHandle=executeCodeLocalEx('CreateMutexA',0,0,0)
  LuaRemoteMutexHandle=duplicateHandle(LuaMutexHandle)
  
  LuaNotifyCEEvent=executeCodeLocalEx('CreateEventA',0,0,0,0)
  LuaCEDoneEvent=executeCodeLocalEx('CreateEventA',0,1,0,0)
  
  LuaRemoteNotifyCEEvent=duplicateHandle(LuaNotifyCEEvent)
  LuaRemoteCEDoneEvent=duplicateHandle(LuaCEDoneEvent)

  --create a lua hook callroutine (todo: 32-bit)
success, aainfo=autoAssemble(string.format([[
alloc(new_lua_gettop,256)
alloc(original_lua_gettop,8)
alloc(last_lua_state,8)
alloc(mutex,8)
alloc(ceevent,8) //notify CE that lua has paused for it to start executing
alloc(cedone,8) //wait for this. CE sets it when done
alloc(executorthreadid,4) //the executor threadid just goes, it doesn't wait

alloc(hascommands,1)
label(afterwait)

new_lua_gettop:

push rcx
sub rsp,20



call GetCurrentThreadID
cmp eax,[executorthreadid]
je afterwait


//obtain the mutex
mov rcx,[mutex] //only for the process, not for CE (even though CE created it...)
mov rdx,ffffffff
call waitForSingleObject

//mutex obtained

cmp byte [hascommands],0
je nocommands

mov rcx,[rsp+20] //remember the push rcx ?
mov [last_lua_state],rcx
//signal CE that lua is frozen

mov rcx,[ceevent]
call setEvent

mov rcx,[cedone]
mov rdx,ffffffff
call waitForSingleObject

//ce is done with the lua state, continue the normal program


nocommands:
mov rcx,[mutex]
call ReleaseMutex

afterwait:
add rsp,20
pop rcx
jmp [original_lua_gettop]

original_lua_gettop:
dq 0

mutex:
dq %x

ceevent:
dq %x

cedone:
dq %x

executorthreadid:
dd %x

]],LuaRemoteMutexHandle, LuaRemoteNotifyCEEvent, LuaRemoteCEDoneEvent, LuaExecutorThreadID))



  s=generateAPIHookScript('lua_gettop', string.format("%x",aainfo.allocs.new_lua_gettop.address), string.format("%x",aainfo.allocs.original_lua_gettop.address)  )
  return autoAssemble(s)  
end

function lua_gettop()
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_gettop,{lua_state})
  end
end

function lua_settop(index)
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_settop,{lua_state, index})
  end
end

function lua_pop(n)
  return lua_settop(-n-1)
end

function lua_pushinteger(value)
  if luastubs and lua_state then
    return LuaExecutor.executeStub(luastubs.lua_pushinteger,{lua_state, value})
  end  
end

function lua
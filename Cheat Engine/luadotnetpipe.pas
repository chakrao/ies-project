unit LuaDotNetPipe;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure initializeLuaDotNetPipe;

implementation

uses luahandler, lua, lauxlib, lualib, luaclass, LuaObject, symbolhandler, dotnetpipe, Maps;

function dotnetpipe_enumDomains(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  domains: TDotNetDomainArray;
  i: integer;
  arraytable: integer;
begin
  dnp:=luaclass_getClassObject(L);

  setlength(domains,0);
  dnp.EnumDomains(domains);
  lua_createtable(L,length(domains),0);

  for i:=0 to length(domains)-1 do
  begin
    lua_pushinteger(L,i+1);
    lua_createtable(L,0,2);

    lua_pushstring(L, 'DomainHandle');
    lua_pushinteger(L, domains[i].hDomain);
    lua_settable(L,-3); //entry

    lua_pushstring(L, 'Name');
    lua_pushstring(L, domains[i].Name);
    lua_settable(L,-3); //entry

    lua_settable(L,-3); //array
  end;

  result:=1;

end;

function dotnetpipe_enumModuleList(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  domain: uint64;
  modules: TDotNetModuleArray;
  i: integer;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    domain:=lua_tointeger(L,1);

    setlength(modules,0);
    dnp.EnumModuleList(domain, modules);

    lua_createtable(L,length(modules),0);

    for i:=0 to length(modules)-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_createtable(L,0,3);

      lua_pushstring(L, 'ModuleHandle');
      lua_pushinteger(L, modules[i].hModule);
      lua_settable(L,-3);

      lua_pushstring(L, 'BaseAddress');
      lua_pushinteger(L, modules[i].baseaddress);
      lua_settable(L,-3);

      lua_pushstring(L, 'Name');
      lua_pushstring(L, modules[i].Name);
      lua_settable(L,-3);

      lua_settable(L,-3);
    end;

    result:=1;
  end;
end;

function dotnetpipe_enumTypeDefs(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  modulehandle: uint64;
  typedefs: TDotNetTypeDefArray;
  i: integer;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=1 then
  begin
    modulehandle:=lua_tointeger(L,1);

    setlength(typedefs,0);
    dnp.EnumTypeDefs(modulehandle, typedefs);

    lua_createtable(L,length(typedefs),0);

    for i:=0 to length(typedefs)-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_createtable(L,0,4);

      lua_pushstring(L, 'TypeDefToken');
      lua_pushinteger(L, typedefs[i].token);
      lua_settable(L,-3);

      lua_pushstring(L, 'Name');
      lua_pushstring(L, typedefs[i].Name);
      lua_settable(L,-3);

      lua_pushstring(L, 'Flags');
      lua_pushinteger(L, typedefs[i].flags);
      lua_settable(L,-3);

      lua_pushstring(L, 'Extends');
      lua_pushinteger(L, typedefs[i].extends);
      lua_settable(L,-3);

      lua_settable(L,-3);
    end;

    result:=1;
  end;
end;

function dotnetpipe_getTypeDefMethods(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  modulehandle: uint64;
  typedeftoken: uint64;
  methods: TDotNetMethodArray;
  i,j: integer;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    modulehandle:=lua_tointeger(L,1);
    typedeftoken:=lua_tointeger(L,2);

    lua_pop(L,lua_gettop(L));

    setlength(methods,0);

    dnp.GetTypeDefMethods(modulehandle, typedeftoken, methods);
    lua_createtable(L,length(methods),0);

    for i:=0 to length(methods)-1 do
    begin
      //MethodToken, Name, Attributes, ImplementationFlags, ILCode, NativeCode, SecondaryNativeCode[]

      lua_pushinteger(L,i+1);
      lua_createtable(L,0,7);


      lua_pushstring(L, 'MethodToken');
      lua_pushinteger(L, methods[i].token);
      lua_settable(L,-3);

      lua_pushstring(L, 'Name');
      lua_pushstring(L, methods[i].Name);
      lua_settable(L,-3);

      lua_pushstring(L, 'Attributes');
      lua_pushinteger(L, methods[i].Attributes);
      lua_settable(L,-3);

      lua_pushstring(L, 'ImplementationFlags');
      lua_pushinteger(L, methods[i].implflags);
      lua_
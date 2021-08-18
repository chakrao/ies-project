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
      lua_settable(L,-3);

      lua_pushstring(L, 'ILCode');
      lua_pushinteger(L, methods[i].ILCode);
      lua_settable(L,-3);

      lua_pushstring(L, 'NativeCode');
      lua_pushinteger(L, methods[i].NativeCode);
      lua_settable(L,-3);

      lua_pushstring(L, 'SecondaryNativeCode');
      lua_createtable(L, length(methods[i].SecondaryNativeCode),0);

      for j:=0 to length(methods[i].SecondaryNativeCode)-1 do
      begin
        lua_pushinteger(L,j+1);
        lua_pushinteger(L,methods[i].SecondaryNativeCode[j].address);
        lua_settable(L,-3); //methods[i].SecondaryNativeCode[j+1]=address
      end;
      lua_settable(L,-3); //methods[i].SecondaryNativeCode={}


      lua_settable(L,-3); //methods[i]={}
    end;

    result:=1;
  end;
end;

function dotnetpipe_getMethodParameters(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  modulehandle: uint64;
  methoddef: dword;
  methodparameters: TDotNetMethodParameters;
  i: integer;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    modulehandle:=lua_tointeger(L,1);
    methoddef:=lua_tointeger(L,2);
    dnp.getmethodparameters(modulehandle, methoddef,methodparameters);

    lua_createtable(L, length(methodparameters), 0);
    for i:=0 to length(methodparameters)-1 do
    begin
      lua_pushinteger(L,i+1);
      lua_createtable(L,0,2);

      lua_pushString(L,'Name');
      lua_pushString(L,methodparameters[i].name);
      lua_settable(L,-3);

      lua_pushString(L,'CType');
      lua_pushinteger(L,methodparameters[i].ctype);
      lua_settable(L,-3);

      lua_settable(L,-3);
    end;

    result:=1;
  end;
end;

function dotnetpipe_getTypeDefParent(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  module: QWORD;
  typedef: dword;
  tdpi: TTypeDefInfo;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    module:=lua_tointeger(L,1);
    typedef:=lua_tointeger(L,2);
    dnp.GetTypeDefParent(module, typedef, tdpi);
    if tdpi.module=0 then exit(0);

    lua_pop(L,lua_gettop(L));

    lua_createtable(L,0,2);
    lua_pushstring(L,'ModuleHandle');
    lua_pushinteger(L, tdpi.module);
    lua_settable(L,-3);

    lua_pushstring(L,'TypedefToken');
    lua_pushinteger(L, tdpi.token);
    lua_settable(L,-3);
    exit(1);
  end;

end;

function dotnetpipe_getTypeDefData(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  module: QWORD;
  typedef: dword;
  typedata: TTypeData;
  i: integer;
begin
  result:=0;
  dnp:=luaclass_getClassObject(L);
  if lua_gettop(L)>=2 then
  begin
    module:=lua_tointeger(L,1);
    typedef:=lua_tointeger(L,2);

    FillByte(typedata, sizeof(typedata),0);

    dnp.getTypeDefData(module,typedef, typedata);

    if typedata.classname='' then exit(0);

    lua_createtable(L,0,7);
    lua_pushstring(L,'ObjectType');
    lua_pushinteger(L, typedata.ObjectType);
    lua_settable(L,-3);

    lua_pushstring(L,'ElementType');
    lua_pushinteger(L, typedata.ElementType);
    lua_settable(L,-3);

    lua_pushstring(L,'CountOffset');
    lua_pushinteger(L, typedata.CountOffset);
    lua_settable(L,-3);

    lua_pushstring(L,'ElementSize');
    lua_pushinteger(L, typedata.ElementSize);
    lua_settable(L,-3);

    lua_pushstring(L,'FirstElementOffset');
    lua_pushinteger(L, typedata.FirstElementOffset);
    lua_settable(L,-3);

    lua_pushstring(L,'ClassName');
    lua_pushstring(L, typedata.ClassName);
    lua_settable(L,-3);

    lua_pushstring(L,'Fields');
    lua_createtable(L, length(typedata.fields),0);
    for i:=0 to length(typedata.Fields)-1 do
    begin
      lua_pushinteger(L, i+1);
      lua_createtable(L,0,7 );

      lua_pushstring(L,'Token');
      lua_pushinteger(L, typedata.fields[i].Token);
      lua_settable(L,-3);

      lua_pushstring(L,'Offset');
      lua_pushinteger(L, typedata.fields[i].offset);
      lua_settable(L,-3);

      lua_pushstring(L,'FieldType');
      lua_pushinteger(L, typedata.fields[i].fieldtype);
      lua_settable(L,-3);

      lua_pushstring(L,'Name');
      lua_pushstring(L, typedata.fields[i].name);
      lua_settable(L,-3);

      lua_pushstring(L,'FieldTypeClassName');
      lua_pushstring(L, typedata.fields[i].fieldTypeClassName);
      lua_settable(L,-3);

      lua_pushstring(L,'IsStatic');
      lua_pushboolean(L, typedata.fields[i].IsStatic);
      lua_settable(L,-3);

      lua_pushstring(L,'Attribs');
      lua_pushinteger(L, typedata.fields[i].attribs);
      lua_settable(L,-3);


      lua_settable(L,-3);
    end;
    lua_settable(L,-3);



    result:=1;

  end;
end;

function dotnetpipe_getAddressData(L: PLua_state): integer; cdecl;
var
  dnp: TDotNetPipe;
  address: uint64;
  addressD
if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'autosave.po')
end

require("lfs")

autosave={} --todo make local

local AutoSaveSettings=getSettings('Auto Save')
local AutoSaveVersion=1

autosave.getPath=function()
  local path=AutoSaveSettings['SavePath']
  if (path==nil) or (path=='') then
  
    path=os.getenv("LOCALAPPDATA")
    if (path==nil) or (path=='') then
      path=getCheatEngineDir() --last attempt  
    end
  end
  
  if string.sub(path,#path)~='\\' then
    path=path..'\\'
  end
  
  return path
end

function autosave.saveState()

  
  local pid=AutoSaveSettings['ProcessID']
  if pid and pid~='' then
    pid=tonumber(pid)
    if pid~=getCheatEngineProcessID() then
      --another CE has done an autosave
      if getProcessList()[pid]==nil then
        --it doesn't exist anymore.
        messageDialog(translate('Another instance of Cheat Engine has crashed and it created an autosave.  Autosave disabled until you go to settings and click ok'), mtWarning, mbOk)
        return false
      end
    end
  end

  AutoSaveSettings['ProcessID']=getCheatEngineProcessID()
  local path, err=autosave.getPath()
  if path==nil then
    messageDialog(translate('Failure to obtain a location to save the autosave data'), mtError, mbOk)  
    return false
  end
    
  path=path..'Cheat Engine AutoSave';
  lfs.mkdir(path)
  
  
  local r,err=saveTable(path..[[\table.ct]])
  if (r==nil) or (r==false) then
    if err==nil then
      err=translate('Unknown Reason')    
    end      
    messageDialog(translate('Failure to autosave table : ')..err, mtError, mbOk)
    return false
  end

  --save the rest (open luascripts and aa scripts, excluding aa scripts belonging to memrecs)
  --feel free to add more if you like, keep in mind to apply the same to loadState
  local cestate,err=createFileStream(path..[[\state.dat]], fmCreate)
  if cestate==nil then
    messageDialog(translate('Failure to autosave state : ')..err, mtError, mbOk)
    return false
  end
  
  local i
  local luaforms={}
  local aaforms={}
  for i=getFormCount()-1,0,-1 do --negative order so the creation order will be correct (the form order is the z-order)
    local f=getForm(i)
   
    if (f.ClassName=='TfrmLuaEngine') and f.Visible then      
      local e={}
      e.history=f.mOutput.Lines.Text
      e.script=f.mScript.Lines.Text
      e.filename=f.savedialog1.FileName
      e.Left=f.Left
      e.Top=f.Top
      e.Width=f.Width
      e.Height=f.Height      
      
      
      if f.MiView.Visible then --first one
        local temp=luaforms[1]
        luaforms[1]=e
        temp=e
      end
      
      if e then      
        table.insert(luaforms,e)
      end
      
    elseif (f.ClassName=='TfrmAutoInject') and f.Visible then
      if (f.IsEditing==false) and (f.ScriptMode~='smLua') then --Editing means it's editing a memoryrecord saved in the table, not saving that
        --standalone AA window, not the tablescript        
        e={}
        e.Filename=f.savedialog1.FileName
        e.Left=f.Left
        e.Top=f.Top
        e.Width=f.Width
        e.Height=f.Height        
        e.ScriptCount=f.TabCount
        e.Scripts={}
        local j
        for j=0,f.TabCount-1 do
          e.Scripts[j+1]=f.TabScript[j]
        end

        table.insert(aaforms,e)
      end    
    end       
  end 
  
  cestate.writeDword(AutoSaveVersion)
  cestate.writeDword(#luaforms)
  for i=1,#luaforms do
    cestate.writeAnsiString(luaforms[i].history)
    cestate.writeAnsiString(luaforms[i].script)
    cestate.writeAnsiString(luaforms[i].filename)
    cestate.writeDword(luaforms[i].Left)
    cestate.writeDword(luaforms[i].Top)
    cestate.writeDword(luaforms[i].Width)
    cestate.writeDword(luaforms[i].Height)    
  end 
  
  cestate.writeDword(#aaforms)
  for i=1,#aaforms do
    cestate.writeDword(aaforms[i].ScriptCount)
    for j=1,#aaforms[i].Scripts do
      cestate.writeAnsiString(aaforms[i].Scripts[j])
    end
    cestate.writeAnsiString(aaforms[i].Filename)
    cestate.writeDword(aaforms[i].Left)
    cestate.writeDword(aaforms[i].Top)
    cestate.writeDword(aaforms[i].Width)
    cestate.writeDword(aaforms[i].Height)        
  end  

  cestate.destroy()
  
  return true
end

function autosave.loadState()
 -- print("loadState()")
  
  AutoSaveSettings['ProcessID']=getCheatEngineProcessID()
  local path=autosave.getPath()..'Cheat Engine AutoSave\\';  
  
  r,err=loadTable(path..'table.ct')
  if (r==nil) or (r==false) then
    if err==nil then
      err=translate('Unknown Reason')    
    end      
    messageDialog(string.format(translate('Failure to load autosave at %s  . Error: %s'), path, err), mtError, mbOk)
    return false
  end
  
  --print("Table loaded. Loading rest")
  
  --load the rest
  local cestate,err=createFileStream(path..[[state.dat]], fmOpenRead)
  if cestate then  
    local i
    local version=cestate.readDword()
    if version~=AutoSaveVersion then
      --check how to load older versions, and if not supported then:
      messageDialog(translate('The saved state belongs to a different autosave version that is not currently implemented'), mtError, mbOk)  
      return false
    end
    local
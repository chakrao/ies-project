local function isWindowVisible(winhandle)
  return executeCodeLocal('IsWindowVisible',winhandle)~=0
end

local function getBaseParentFromWindowHandle(winhandle)
  local i=0
  local last=winhandle

  while winhandle and (winhandle~=0) and (i<10000) do
    last=winhandle
    winhandle=getWindow(winhandle, GW_HWNDOWNER)
    i=i+1
  end;

  return last
end

function ceshare.getProcessTitle(pid)
  local w=getWindow(getForegroundWindow(), GW_HWNDFIRST)

  local bases={}

  while w and (w~=0) do
    if getWindowProcessID(w)==pid then
      if isWindowVisible(w) then
        local h=getBaseParentFromWindowHandle(w)
        local c=getWindowCaption(h)
        if isWindowVisible(h) and (c~='') then
          bases[h]=c
        end
      end
    end
    w=getWindow(w,GW_HWNDNEXT)
  end


  for h,n in pairs(bases) do
    return n --just hope for the best...
  end
end

function ceshare.getCurrentProcessTitle()
  return ceshare.getProcessTitle(getOpenedProcessID())
end



function ceshare.Delete(entry)
  if entry then
    if messageDialog(translate('Are you sure you wish to delete this table?'),mtWarning,mbYes,mbNo)==mrYes then
      local r=ceshare.QueryXURL('DeleteTable.php','id='..entry.ID)  
      if r then
        if ceshare.CheatBrowserFrm and ceshare.CheatBrowserFrm.Visible then
          ceshare.CheckForCheatsClick()
        end
        
        if ceshare.UpdateOrNewFrm and ceshare.UpdateOrNewFrm.Visible then
          ceshare.PublishCheatClick()
        end      
        showMessage(translate('Table successfuly deleted')) --meanie
      end
    end
  end
end

function ceshare.PublishCheat(data,title,processname, headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5, url)


  local parameters=''
   
  if (processname==nil) or (processname=='') then
    ceshare.showError(translate('processname is empty'))
    return  
  end
  
  if (title==nil) or (title=='') then
    ceshare.showError(translate('title is empty'))
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError(translate('data is empty'))
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError(translate('description is empty'))
    return
  end
  


  parameters='data='..ceshare.url_encode(data)
  parameters=parameters..'&title='..ceshare.url_encode(title)
  parameters=parameters..'&processname='..ceshare.url_encode(processname);
  parameters=parameters..'&description='..ceshare.url_encode(description)   
  if headermd5~=nil then parameters=parameters..'&headermd5='..ceshare.url_encode(headermd5) end    
  if public~=nil then parameters=parameters..'&public='..ceshare.url_encode(public) end  
  if versionindependent~=nil then parameters=parameters..'&versionindependent='..ceshare.url_encode(versionindependent) end
  if fullfilehash~=nil then parameters=parameters..'&fullfilehash='..ceshare.url_encode(fullfilehash) end
  if secondarymodulename~=nil then parameters=parameters..'&secondarymodulename='..ceshare.url_encode(secondarymodulename) end
  if secondaryfullfilehashmd5~=nil then parameters=parameters..'&secondaryfullfilehashmd5='..ceshare.url_encode(secondaryfullfilehashmd5) end
  if url~=nil then parameters=parameters..'&url='..ceshare.url_encode(url) end
 
  if isKeyPressed(VK_CONTROL)==false then  --control lets you get a new script if needed
    local secondaryIdentifierCode=ceshare.secondaryIdentifierCode.Value[processname:lower()]
    if secondaryIdentifierCode and secondaryIdentifierCode~='' then
      local value,param=loadstring(secondaryIdentifierCode)()
      if value and param then
        parameters=parameters..'&secondaryidentifier='..ceshare.url_encode(param)
      end
    end
  end    
  
 
  local r=ceshare.QueryXURL('PublishTable.php',parameters)
  
  if r then
    showMessage(translate('Thank you, your table has been published'));
    return true
  end
end

function ceshare.UpdateCheat(id,data,title,headermd5, versionindependent, description, public, fullfilehash, secondarymodulename, secondaryfullfilehashmd5, url)
  local parameters=''
  
  if id==nil then
    ceshare.showError(translate('No id given'))
    return
  end
    
  if (title==nil) or (title=='') then
    ceshare.showError(translate('title is empty'))
    return  
  end

  if (data==nil) or (data=='') then
    ceshare.showError(translate('data is empty'))
    return
  end
  
  if (description==nil) or (description=='') then
    ceshare.showError(translate('description is empty'))
    return
  end
  
  if ceshare.LoggedIn==nil then
    if not ceshare.spawnLoginDialog() then 
      return
    end
  end
  

  parameters=parameters..'id='..id
  parameters=parameters..'&data='..ceshare.url_encode(data)
  parameters=parameters..'&title='..ceshare.url_encode(title)
  parameters=parameters..'&description='..ceshare.url_encode(description)   
  if headermd5~=nil then parameters=parameters..'&headermd5='..ceshare.url_encode(headermd5) end    
  if public~=nil then parameters=parameters..'&public='..ceshare.url_encode(public) end  
  if versionindependent~=nil then parameters=parameters..'&versionindependent='..ceshare.url_encode(versionindependent) end
  if fullfilehash~=nil then parameters=parameters..'&fullfilehash='..ceshare.url_encode(fullfilehash) end
  if secondarymodulename~=nil then parameters=parameters..'&secondarymodulename='..ceshare.url_encode(secondarymodulename) end
  if secondaryfullfilehashmd5~=nil then parameters=parameters..'&secondaryfullfilehashmd5='..ceshare.url_encode(secondaryfullfilehashmd5) end
  if url~=nil then parameters=parameters..'&url='..ceshare.url_encode(url) end
  
  
  local r=ceshare.QueryXURL('EditTable.php',parameters)
  
  if r then
    showMessage(translate('Thank you, your table has been updated'));
    return true
  end

end

function ceshare.PublishCheatClick(sender, cheatinfo) 
  local loggedin=ceshare.LoggedIn or false
  --if not logged in, log in now
  if not loggedin then
    if not ceshare.spawnLoginDialog() then 
      return
    end
  end
  
  if cheatinfo then
    ceshare.publishOrUpdate(cheatinfo)  
    return    
  end
  
  --spawn a window that shows all tables with this processname that the current user has modify right
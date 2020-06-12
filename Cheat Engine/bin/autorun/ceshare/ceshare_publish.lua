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

function c
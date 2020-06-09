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
      path
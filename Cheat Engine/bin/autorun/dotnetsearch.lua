--dotnetsearch
if getTranslationFolder()~='' then
  loadPOFile(getTranslationFolder()..'dotnetsearch.po')
end

function spawnDotNetSearchDialog(DataSource, frmDotNetInfo, searchtype)

  local currentScan --rule: only writable in mainthread
  local searchresults={}
  
  
  --spawns a searchdialog. searchtype has 3 options: 0-ClassName, 1-FieldName, 2-MethodName
  local frmSearch=createFormFromFile(getAutorunPath()..'forms'..pathsep..'DotNetSearch.frm')  
  
  _G.frmSearch=frmSearch
  
  if searchtype==0 then
    frmSearch.Caption=translate('Find Class')    
    frmSearch.cbLimitToCurrentBase.Caption=translate('Limit to current image')
    
    frmSearch.cbLimitToCurrentBase.Enabled=frmDotNetInfo.lbImages.ItemIndex>=0      
    frmSearch.lvResults.Columns.delete(2)
  elseif searchtype==1 then
    frmSearch.Caption=translate('Find Field')    
    frmSearch.cbLimitToCurrentBase.Caption=translate('Limit to current class')
    frmSearch.cbLimitToCurrentBase.Enabled=frmDotNetInfo.lbClasses.ItemIndex>=0 
    frmSearch.lvResults.Columns[2].Caption='Field'
  elseif searchtype==2 then
    frmSearch.Caption=translate('Find Method')  
 
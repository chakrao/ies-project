local DPIMultiplier=(getScreenDPI()/96)
local ThumbnailWidth=240*DPIMultiplier
local ThumbnailHeight=80*DPIMultiplier

local DummyBitmap=createBitmap()
DummyBitmap.Canvas.Font.Size=12

local getListItemData,getThumbnail,generateListItemBitmap,getListItemBitmap
local cleanPage, setPage,getFullProcessList,filterList

--[[
ceshare.FullProcessList is the downloaded list which c
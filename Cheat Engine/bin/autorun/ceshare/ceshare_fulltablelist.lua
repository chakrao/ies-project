local DPIMultiplier=(getScreenDPI()/96)
local ThumbnailWidth=240*DPIMultiplier
local ThumbnailHeight=80*DPIMultiplier

local DummyBitmap=createBitmap()
DummyBitmap.Canvas.Font.Size=12

local getListItemData,getThumbnail,generateListItemBitmap,getListItemBitmap
local cleanPage, setPage,getFullProcessList,filterList

--[[
ceshare.FullProcessList is the downloaded list which contains all entries
ceshare.FullProcessListView is the searchresult

--]]

local backgroundcolor

local darkMode=1
local windowColor,listColor,searchFieldColor,fontColor, fontSize, linkColor, highlightColor
if darkMode==1 then
  listColor=clBlack
  searchFieldColor=clBlack
  fontColor=clWhite
  windowColor=clBlack
  linkColor=0x0000ff
  highlightColor=0x00ff00
else 
  listColor=clDefault
  searchFieldColor=clDefault
  fontColor=clDefault
  windowColor=clDefault
  linkColor=0xff0000
  highlightColor=clDefault
end

fontSize=12



function getListItemData(index)
  local width
  local height
  local data=ceshare.FullProcessListView[index]

  local text=data.process..' - '..data.title
  local width=2*DPIMultiplier+DummyBitmap.Canvas.getTextWidth(text) 
  local height=2*DPIMultiplier+DummyBitmap.Canvas.getTextHeight(text) 
  
  local text2
  if data.count>1 then      
    text2=string.format(translate('  and %d more tables...'),data.count)
    local w=2*DPIMultiplier+DummyBitmap.Canvas.getTextWidth(text2)       
    if w>width then width=w end      
    height=height+2*DPIMultiplier+DummyBitmap.Canvas.getTextHeight(text2) 
  end 
  
  if data.thumbnail then
    if ThumbnailWidth>width then width=ThumbnailWidth end
    height=height+ThumbnailHeight
  end
  
  return width,height, text,text2
end

function getThumbnail(data)


  if data.thumbnail then
    if ceshare.ThumbnailCache==nil then
      ceshare.ThumbnailCache={}
    end
    
    --check cache
    local p=ceshare.ThumbnailCache[data.thumbnail]
    
    if p and isKeyPressed(VK_CONTROL) then --force reload      
      p.destroy() 
      ceshare.ThumbnailCache[data.thumbnail]=nil
      p=nil
    end
    
    if p==nil then --not cached yet
      local thumbnailtext=ceshare.getInternet().getURL(data.thumbnail)
      local ss=createStringStream(thumbnailtext)
      p=createPicture()
      if p.loadFromStream(ss)==false then
        --X  (not found/valid)
        p.Bitmap.Width=10
        p.Bitmap.Heigth=10
        p.Bitmap.Canvas.Width=10
        p.Bitmap.Canvas.Heigth=10
        p.Bitmap.Canvas.Pen.Color=0x0000ff
        p.Bitmap.Canvas.Brush.Color=0xffffff
        p.Bitmap.Canvas.clear()
        p.Bitmap.Canvas.line(0,0,10,10)
        p.Bitmap.Canvas.line(10,0,0,10)        
      end
      
      ss.destroy()
      
      ceshare.ThumbnailCache[data.thumbnail]=p 
    end    
    return p
  end
end

function generateListItemBitmap(index)
  --generate a bitmap
  local data=ceshare.FullProcessListView[index]
  local result
  if data then
    local w,h,text,text2=getListItemData(index)
    local thumbnail=getThumbnail(data)
    local y=0
    
    result=createBitmap(w,h)    
    if backgroundcolor==nil then
      backgroundcolor=ceshare.AllTableForm.List.RGBColor         
    end
    result.Canvas.Brush.Color=backgroundcolor --0xff00
    result.Canvas.fillRect(0,0,w,h)
    
    result.Canvas.Font.Size=12
    result.Canvas.Font.Color=fontColor
    
    if thumbnail then
      local r={}
      r.Top=0
      r.Left=0
      r.Right=ThumbnailWidth
      r.Bottom=ThumbnailHeight
      result.Canvas.stretchDraw(r,thumbnail.Bitmap) 
      --result.Canvas.draw(0,0, thumbnail.Bitmap) --works
      --result.Canvas.copyRect(0,0,ThumbnailWidth,ThumbnailHeight,thumbnail.PNG,0,0,thumbnail.PNG.Width,thumbnail.PNG.Height)
      y=ThumbnailHeight
    end
    result.Canvas.textOut(0,y,text)
    if text2 and text2~='' then
      y=y+result.Canvas.getTextHeight(text)
      result.Canvas.textOut(0,y,text2)
    end
  end
  
  return result
end


function getListItemBitmap(index)
  --generates or gets a previously generated bitmap for the given listitem
  local data=ceshare.FullProcessListView[index] 
  if data then
    if data.bitmap==nil then
      data.bitmap=generateListItemBitmap(index)
    end
    
    return data.bitmap
  else
    print("index "..index.." is not in the list");
  end  
end

function cleanPage(index)
  --cleans a single page (usually the previous page)
  for i=1+index*25,math.min(1+index*25+25, #ceshare.FullProcessListView) do
    if ceshare.FullProcessListView[i].bitmap then
      ceshare.FullProcessListView[i].bitmap.destroy()
      ceshare.FullProcessListView[i].bitmap=nil
    end
  end  
end

function setPage(index)
  if ceshare.AllTableForm.Page then
    cleanPage(ceshare.AllTableForm.Page)
  end

  if index<0 then index=0 end
  if 1+index*25>#ceshare.FullProcessListView then
    index=(#ceshare.FullProcessListView / 25) 
  end
  ceshare.FullProcessListPage=index
  
  --add ceshare.FullProcessListView items to the list  
  ceshare.AllTableForm.List.Items.clear()
  ceshare.AllTableForm.Page=index
  local start=1+index*25
  local stop=math.min(index*25+25, #ceshare.FullProcessListView) 
  loc
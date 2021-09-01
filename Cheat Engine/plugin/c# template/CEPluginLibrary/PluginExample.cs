﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using CESDK;

namespace CEPluginLibrary
{
    class PluginExample : CESDKPluginClass
    {
        public override string GetPluginName()
        {
            return "C# Plugin Template for Cheat Engine 7.1+";
        }

        public override bool DisablePlugin() //called when disabled
        {
            
            return true;
        }
        
        public override bool EnablePlugin() //called when enabled
        {
            //you can use sdk here
            //sdk.lua.dostring("print('I am alive')");
            

            sdk.lua.Register("pluginexample1", MyFunction);
            sdk.lua.Register("pluginexample2", MyFunction2);
            sdk.lua.Register("pluginexample3", MyFunction3);
            sdk.lua.Register("pluginexample4", MyFunction4);
            sdk.lua.Register("formymcformface", MyFunction4);

            sdk.lua.DoString(@"local m=MainForm.Menu
local topm=createMenuItem(m)
topm.Caption='C# Example plugin'
m.Items.insert(MainForm.miHelp.MenuIndex,topm)

local mf1=createMenuItem(m)
mf1.Caption='Example 1: Weee';
mf1.OnClick=function(s)
  local r1,r2=pluginexample1(100)

  print('pluginexample1(100) returned: '..r1..','..r2)
end
topm.add(mf1)


local mf2=createMenuItem(m)
mf2.Caption='Example 2: Scripting and ignoring the state';
mf2.OnClick=function(s)
  local r1,r2=pluginexample2(100)

  print('pluginexample2() returned: '..r1..','..r2)
end
topm.add(mf2)


local mf3=createMenuItem(m)
mf3.Caption='Example 3: Threading';
mf3.OnClick=function(s)
  local i=pluginexample3(100)

  print('pluginexample3() went through the wait loop '..i..' times')
end
topm.add(mf3)


local mf4=createMenuItem(m)
mf4.Caption='Example 4: Forms and whatnot';
mf4.OnClick=function(s)
  local newthread=MessageDialog('Open the form in a new thread? (If not it will open inside the main GUI)',mtConfirmation,mbYes,mbNo)==mrYes
  local i=pluginexample4(newthread)

  print('pluginexample4() finally returned with the value '..i..' (should be 100)')
end
topm.add(mf4)");

            return true;            
        }

        int MyFunction()
        {
            var l = sdk.lua;
            l.PushString("WEEEE");                        
            if (l.GetTop() > 0)
            {
                if (l.IsInteger(1))
                {
                    int i = (int)l.ToInteger(1);
                    l.PushInteger(i * 2);
                    return 2;
                }
            }

            return 1;
        }

        int MyFunction2(IntPtr L)
        {
            var l = sdk.lua;

            l.DoString("MainForm.Caption='Changed by test2()'");

            l.PushString(L, "Works");
            l.PushString("And this as well");

           return 2;
        }

        public void NewThreadExample()
        {
            //return;
            sdk.lua.DoString("print('Running from 
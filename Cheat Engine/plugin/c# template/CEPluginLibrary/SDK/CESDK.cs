using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;


//CE SDK wrapper.  You usually don't need to be here, so close your eyes and walk away

namespace CESDK
{
 
    public abstract class CESDKPluginClass
    {
        public CESDK sdk;
        public abstract String GetPluginName();
        public abstract Boolean EnablePlugin();
        public abstract Boolean DisablePlugin();
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct TExportedFunctions
    {
        public int sizeofExportedFunctions;
        public IntPtr GetLuaState;
        public IntPtr LuaRegister;
        public IntPtr LuaPushClassInstance;
        public IntPtr ProcessMessages;
        public IntPtr CheckSynchronize;
    }

    public class CESDK
    {
        public static CESDKPluginClass currentPlugin;
        public CESDKLua lua;
        private const int PLUGINVERSION = 6; //CE SDK plugin
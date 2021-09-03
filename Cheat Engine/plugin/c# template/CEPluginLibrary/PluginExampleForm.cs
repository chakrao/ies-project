﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using CESDK;

namespace CEPluginLibrary
{
    public partial class PluginExampleForm : Form
    {
        MemScan ms;
        FoundList fl;

        public PluginExampleForm()
        {
            InitializeComponent();
        }

        private void button1_Click(object sender, EventArgs e)
        {           
            MessageBox.Show("WEEEEEEE");
            GC.Collect();
        }

        

        private void MemScanDone(object sender)
        {
            //called from CE's main UI thread. Problematic if the form was created using a new thread
            if (this.InvokeRequired)
            {                
                this.BeginInvoke(((MemScan)sender).OnScanDone,sender);
            }
            else
            {
                int count;
                fl.Initialize();
                
                count = fl.Count;
                listView1.VirtualListSize = count;
                
                button2.Enabled = true;
                button3.Enabled = true;
                progressBar1.Value = 0;
            }

        }

        private void MemScanGuiUpdate(object sender, UInt64 TotalAddressesToScan, UInt64 CurrentlyScanned, UInt64 ResultsFound)
        {
            //called from CE's main UI thread. Problematic if the form was created using a new thread
            if (this.InvokeRequired)
            {
                this.BeginInvoke(((MemScan)sender).OnGuiUpdate, sender, TotalAddressesToScan, CurrentlyScanned, ResultsFound);
            }
            else
            {
                if (TotalAddressesToScan > 0)
                {
                    int percentage = (int)((double)(CurrentlyScanned/TotalAddressesToScan ) * 100);
                    progressBar1.Value = percentage;
                }
                else
                    progressBar1.Value = 0;
            }
        }

        private VarTypes SelectedVarType()
        {
            switch (comboBox1.SelectedIndex)
            {
                case 0: return VarTypes.vtByte;
                case 1: return VarTypes.vtWord;
                case 2: return VarTypes.vtDword;
                case 3: return VarTypes.vtQword;
                case 4: return VarTypes.vtSingle;
                case 5: return VarTypes.vtDouble;
                case 6: return VarTypes.vtString;
                case 7: return VarTypes.vtByteArray;
                default:
                    return VarTypes.vtDword;

            }


        }

        private void button2_Click(object sender, EventArgs e)
        {
            ms.Scan(new ScanParameters
            {
                Value = textBox1.Text,
                VarType = SelectedVarType()
            });        
            button2.Enabled = false;           
        }

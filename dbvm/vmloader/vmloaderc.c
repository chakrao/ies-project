
/*
vmloaderc.c:  This is called by vmloader.asm, which has it's base at 0x30100
_vmloader_main is the entry in this c file

This is the high level part that will copy the vmm to the highest memorylocation
Then setup the pagetables to point to the vmm (identify pagemapping btw...)
enable paging, and jump to the vmm entry (which has it's base at virtual address 0x0)
*/
#include "common.h"

#ifndef VMMSIZE
  #error VMMSIZE must be provided
#endif

extern int reservedmem_listcount;
extern PPDPTE_PAE PageMapLevel4;
extern void gotoVMM(void) __attribute__((stdcall));
extern int readsectorasm(void) __attribute__((stdcall));
extern void halt(void) __attribute__((stdcall));

unsigned int isAP=0;

unsigned short int VMMlocation;
unsigned short int LOGOlocation;

extern int a20state;

//disk
extern int readerror;
extern int sectorsread;

extern int bootdisk;
extern int SectorsPerTrack;
extern int NumberOfHeads;
extern int NumberOfCylinders;

extern unsigned int vmmPA;

extern DWORD GDTVA;


int readsector(int sectornr, void *destination)
{
	struct
	{
		BYTE sector;
		BYTE head;
		BYTE cylinder;
		BYTE drive;
	} __attribute__((__packed__)) *readstruct=(void *)0x00070000;




  //configure parameters at 0x00070000
	readstruct->sector=(sectornr % (SectorsPerTrack-1));
	readstruct->head=(sectornr/(SectorsPerTrack-1)) % NumberOfHeads;
	readstruct->cylinder=(sectornr/(SectorsPerTrack-1))/NumberOfHeads;
	readstruct->drive=bootdisk;
	readstruct->sector++;

	//displayline("Read: disk=%2 cylinder=%d head=%d sector=%d...",readstruct->drive, readstruct->head, readstruct->cylinder, readstruct->sector );

	zeromemory((void *)0x00060000,512);


	if (readsectorasm()==1)
	{
		//sendstringf("Successfull read. Writing to %8\n\r",(ULONG)destination);
		//displayline("Success\n");
		copymem(destination,(void *)0x00060000,512);
		return 1;
	}
	else
	{
		//displayline("Failure\n");
		return 0;
	}
}

int _vmloader_main(void)
{
	PARD p;


	unsigned char bootsectorm[1024]; //extra alignment
	unsigned char *bootsector=(unsigned char *)(((DWORD)bootsectorm+512) & 0xfffffe00);
	//unsigned char bootsector[512]; //extra alignment
	int i;
	unsigned long long maxAvailableAddress=0;
	unsigned long long tempbase=0,templength=0;
	unsigned int extramemory;
	int chosenregion=-1;
	nosendchar[getAPICID()]=0;

	zeromemory(bootsectorm, 1024);




	sendstringf("\n\n--------------------------------\n\r");
	sendstringf("Welcome to Dark Byte\'s vmloader\n\r");
	sendstringf("--------------------------------\n\r");

	//waitforchar();


	sendstringf("a=%8\n\r",readerror);
	sendstringf("b=%8\n\r",sectorsread);


	sendstringf("_vmloader_main got loaded at address %8\n\r",(unsigned int)_vmloader_main);
	sendstringf("reservedmem_listcount=%d (address of it = %8 ) \n\r",reservedmem_listcount,&reservedmem_listcount);

	sendstringf("Going to read the VMM into memory...\n\r");


  //isAP is obsolete


	sendstringf("isAP value=%2  (address=%8)\n\r",isAP,(ULONG)&isAP);


	if (!isAP)
	{
		printstring("\311\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\273", 0,0,15, 1);
		printstring("\272                                                                             \272", 0,1,15, 1);
		printstring("\272                                                                             \272", 0,2,15, 1);
		printstring("\272                                                                             \272", 0,3,15, 1);
		printstring("\272                                                                             \272", 0,4,15, 1);
		printstring("\310\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\315\274", 0,5,15, 1);

		printstring("This tool originated at",40-11,2,10,1);
		printstring("www.cheatengine.org",40-8,3,12,1);

		currentdisplayline=7;

		displayline("a20state=%d (should be 1)\n",a20state);
		displayline("bootdrive=%2 (stored at %p)\n",bootdisk, &bootdisk);
		displayline("SectorsPerTrack=%d\n",SectorsPerTrack);
		displayline("NumberOfHeads=%d\n",NumberOfHeads);
		displayline("NumberOfCylinders=%d\n",NumberOfCylinders);

		displayline("&bootsector=%p\n", &bootsector);

		displayline("Testing diskread ability....");


  //  waitforkeypress();





		if (!readsector(0, bootsector))
		{
		  sendstringf("Error loading the bootsector.\n");
			displayline("Error loading the bootsector.\n");
			while (1) ;

			readsector(-1, bootsector);
			readsector(0, bootsector);
			readsector(1, bootsector);
			readsector(2, bootsector);


			halt();
		}
		else
		{
		  displayline("Successfully loaded the bootsector\n");

		  /*
		  for (i=0; i<512; i++)
		    displayline("%2 ", bootsector[i]);
*/
	  //  waitforkeypress();
	  //  waitforkeypress();
		}


		VMMlocation=*(unsigned short int *)&bootsector[0x8];
		LOGOlocation=*(unsigned short int *)&bootsector[0x10];

		zeromemory(bootsectorm, 1024);
		sendstringf("VMM starts at sector %d\n\r",VMMlocation);
		sendstringf("LOGO starts at sector %d\n\r",LOGOlocation);

		displayline("VMM starts at sector %d\n",VMMlocation);


		isAP=1;
		p=(PARD)0x80000;
		for (i=0; i<reservedmem_listcount;i++)
		{
			tempbase=((unsigned long long)p[i].BaseAddrHigh << 32)+p[i].BaseAddrLow;
			templength=((unsigned long long)p[i].LengthHigh << 32)+p[i].LengthLow;

			sendstringf("i=%d : BaseAddress=%6, Length=%6, Type=%d ",i, tempbase, templength, p[i].Type);
			displayline("i=%d : BaseAddress=%6, Length=%6, Type=%d \n\r",i, tempbase, templength, p[i].Type);

			if (((tempbase+templength) < 0x100000000ULL ) && (templength>=0xc00000) && (p[i].Type==1) && (tempbase+templength>maxAvailableAddress) )
			{
				maxAvailableAddress=tempbase+templength;
				chosenregion=i;

				sendstringf(" < 'new' potential region");

			}

			sendstring("\n\r");
		}

    //waitforkeypress();
    //waitforkeypress();

		if (chosenregion==-1)
		{
			displayline("Failure in picking a fitting region\n");
			halt();
		}

		/* adjust memory map */
		p[reservedmem_listcount].BaseAddrHigh=0;
		p[reservedmem_listcount].BaseAddrLow=0;
		p[reservedmem_listcount].LengthHigh=0;
		p[reservedmem_listcount].LengthLow=0;
		p[reservedmem_listcount].Type=255;  //mark as end of list (for vmm)



		if (maxAvailableAddress==0)
		{
			sendstringf("Not enough usable memory (at end)\n\r");
			displayline("Not enough usable memory (at end)\n\r");
		}
		else
		{
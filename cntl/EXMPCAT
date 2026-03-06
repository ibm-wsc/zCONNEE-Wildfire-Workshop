//DEFKSDS EXEC PGM=IDCAMS,REGION=64M,COND=(0,LT)
//SYSPRINT DD SYSOUT=*
//AMSDUMP  DD SYSOUT=*
//SYSIN DD *
   DELETE CICSTS.CICS53Z.EXMPLAPP.EXMPCAT PURGE CLUSTER
   SET MAXCC=0
   DEFINE CLUSTER (NAME(CICSTS.CICS53Z.EXMPLAPP.EXMPCAT)-
          TRK(1 1) -
          VOL(CIC130) -
          KEYS(4 0) -
          RECORDSIZE(80,80) -
          SHAREOPTIONS(2 3) -
          INDEXED -
          ) -
          DATA (NAME(CICSTS.CICS53Z.EXMPLAPP.EXMPCAT.DATA) -
          ) -
          INDEX (NAME(CICSTS.CICS53Z.EXMPLAPP.EXMPCAT.INDEX) -
          )
//*
//CPYKSDS  EXEC PGM=IDCAMS,REGION=64M,COND=(0,LT)
//SYSPRINT DD SYSOUT=*
//AMSDUMP  DD SYSOUT=*
//INFILE   DD *
0010Ball Pens Black 24pk                    010002.900135000
0020Ball Pens Blue 24pk                     010002.900006050
0030Ball Pens Red 24pk                      010002.900106000
0040Ball Pens Green 24pk                    010002.900080000
0050Pencil with eraser 12pk                 010001.780083000
0060Highlighters Assorted 5pk               010003.890013040
0070Laser Paper 28-lb 108 Bright 500/ream   010007.440102020
0080Laser Paper 28-lb 108 Bright 2500/case  010033.540025000
0090Blue Laser Paper 20lb 500/ream          010005.350022000
0100Green Laser Paper 20lb 500/ream         010005.350003020
0110IBM Network Printer 24 - Toner cart     010169.560012000
0120Standard Diary: Week to view 8 1/4x5 3/4010025.990007000
0130Wall Planner: Eraseable 36x24           010018.850003000
014070 Sheet Hard Back wire bound notepad   010005.890084000
0150Sticky Notes 3x3 Assorted Colors 5pk    010005.350036045
0160Sticky Notes 3x3 Assorted Colors 10pk   010009.750067030
0170Sticky Notes 3x6 Assorted Colors 5pk    010007.550064030
0180Highlighters Yellow 5pk                 010003.490088010
0190Highlighters Blue 5pk                   010003.490076020
020012 inch clear rule 5pk                  010002.120014010
0210Clear sticky tape 5pk                   010004.270073000
//OUTFILE  DD DSN=CICSTS.CICS53Z.EXMPLAPP.EXMPCAT,DISP=SHR
//SYSIN DD *
 REPRO INFILE(INFILE) OUTFILE(OUTFILE)
//

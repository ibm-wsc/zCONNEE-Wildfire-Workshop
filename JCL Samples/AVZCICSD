//*********************************************************************
//*  JCL SETTINGS CUSTOMER LIBRARIES
//*********************************************************************
// SET SDFHLOAD='CICSTS54.CICS.SDFHLOAD'   <- CICSTS CICS LOAD LIBRARY
// SET SDFHCSD='CICSTS.CICS54Z.DFHCSD'      <- CICSTS CICS CSD LIBRARY
//*********************************************************************
//DEFINE   EXEC PGM=DFHCSDUP,REGION=0M,
//         PARM='CSD(READWRITE),PAGESIZE(60),NOCOMPAT'
//STEPLIB  DD DISP=SHR,DSN=&SDFHLOAD
//DFHCSD   DD DISP=SHR,DSN=&SDFHCSD
//SYSUT1   DD UNIT=SYSDA,SPACE=(1024,(100,100))
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
**********************************************************************
* AVZ Interface for CICS/TS                                          *
**********************************************************************
*
  DEFINE CONNECTION(CICA)
         DESCRIPTION(AVZ CONNECTION USING EXCI)
         ACCESSMETHOD(IRC)
         PROTOCOL(EXCI)
         ATTACHSEC(IDENTIFY)
         CONNTYPE(SPECIFIC)
         GROUP(AVZCTS)
         NETNAME(CICAAVZS)

  DEFINE SESSIONS(CICA)
         DESCRIPTION(AVZ SESSION USING EXCI)
         PROTOCOL(EXCI)
         CONNECTION(CICA)
         MAXIMUM(0,0) RECEIVECOUNT(21)
         RECEIVEPFX(SD)
         GROUP(AVZCTS)

  DEFINE TRANSACTION(AVZT)
         DESCRIPTION(AVZ/CICS TRANSACTION)
         PROGRAM(DFHMIRS)
         PROFILE(AVZPROF)
         TASKDATALOC(ANY)
         TASKDATAKEY(USER)
         GROUP(AVZCTS)
*
**********************************************************************
* AVZ Interface for Natural/ACI                                      *
**********************************************************************
*
  DEFINE TRANSACTION(ACI1)
         DESCRIPTION(AVZ ACI NATURAL TRANSACTION)
         PROGRAM(SDCINAT)
         PROFILE(AVZPROF)
         TASKDATALOC(ANY)
         TASKDATAKEY(USER)
         TWASIZE(128)
         GROUP(AVZCTS)

  DEFINE TRANSACTION(ACIF)
         DESCRIPTION(AVZ ACI NATURAL FRONTEND TRANSACTION)
         PROGRAM(AVZCIFEN)                                              0
         PROFILE(AVZPROF)
         TASKDATALOC(ANY)
         TASKDATAKEY(USER)
         TWASIZE(8192)
         GROUP(AVZCTS)

  DEFINE TRANSACTION(ACI2)
         DESCRIPTION(AVZ ACI NATURAL FRONTEND TRANSACTION)
         PROGRAM(SDCIAC2)
         PROFILE(AVZPROF)
         TASKDATALOC(ANY)
         TASKDATAKEY(USER)
         TWASIZE(8192)
         GROUP(AVZCTS)

  DEFINE TDQUEUE(NLST)
         DESCRIPTION(AVZ ACI NATURAL INTRA TDQ)
         TYPE(INTRA)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZRTX)
         DESCRIPTION(AVZ ACI ROUTING TABLE)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(BELOW)
         EXECKEY(USER)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZCIIVP)
         DESCRIPTION(AVZ ACI IVP PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(USER)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZCINAT)
         DESCRIPTION(AVZ ACI NATURAL FRONTEND)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(USER)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZCIFEN)
         DESCRIPTION(AVZ ACI NATURAL FRONTEND)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(CICS)
         GROUP(AVZCTS)
         CONCURRENCY(THREADSAFE)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(SDCIACI)
         DESCRIPTION(AVZ EXCI TO ACI FRONTEND)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(CICS)
         GROUP(AVZCTS)
         CONCURRENCY(THREADSAFE)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(SDCIAC2)
         DESCRIPTION(AVZ EXCI TO ACI FRONTEND)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(CICS)
         GROUP(AVZCTS)
         CONCURRENCY(THREADSAFE)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCISTR)
         DESCRIPTION(AVZ ACI EXCI START PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(USER)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZCITRU)
         DESCRIPTION(AVZ ACI TASK RELATED USER EXIT)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(CICS)
         GROUP(AVZCTS)
         CONCURRENCY(THREADSAFE)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCTRUI)
         DESCRIPTION(AVZ ACI TRUE INITIALIZATION PGM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(CICS)
         GROUP(AVZCTS)
**********************************************************************
* AVZ Utility Request Processor for management of DPL requests       *
**********************************************************************
*
  DEFINE PROGRAM(AVZCUTAS)
         DESCRIPTION(AVZ UTILITY FUNCTION PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         EXECKEY(USER)
         GROUP(AVZCTS)

*
**********************************************************************
* AVZ Interface for CICS/VSAM                                        *
**********************************************************************
*
  DEFINE PROFILE(AVZPROF)
         DESCRIPTION(AVZ/CICS STANDARD PROFILE)
         INBFMH(ALL)
         GROUP(AVZCTS)

  DEFINE TRANSACTION(AVZS)
         DESCRIPTION(AVZ/CICS TRANSACTION IDENTIFIER)
         PROGRAM(DFHMIRS)
         PROFILE(AVZPROF)
         TASKDATALOC(ANY)
         TASKDATAKEY(USER)
         GROUP(AVZCTS)

  DEFINE PROGRAM(AVZCVSAM)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSTR)
         DESCRIPTION(AVZ/CICS VSAM TRACE)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSBT)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSMS)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSIN)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSUP)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)

  DEFINE PROGRAM(AVZCVSSE)
         DESCRIPTION(AVZ/CICS PROGRAM)
         LANGUAGE(ASSEMBLER)
         DATALOCATION(ANY)
         GROUP(AVZCTS)
*ETHS*   API(OPENAPI)
*
**********************************************************************
* AVZ Installation and Verification Definitions                      *
**********************************************************************
*
**********************************************************************
* Change the hlq.FILEA data set name in the FILEA definition         *
* to use the FILEA data set name you defined.  If you already        *
* have the FILEA defined in CICSTS RDO, then you should remove       *
* the one contained below.  FILEA is used for the installation       *
* verification process (IVP) in the Installation Guide Appendix      *
* for the CICS/TS interface and Streams for CICS/VSAM feature.       *
**********************************************************************

* DEFINE FILE(FILEA)
*         ADD(YES) BROWSE(YES) DELETE(YES) READ(YES)
*         UPDATE(YES) DESCRIPTION(FILE FILEA DEFINITION)
*         DSNAME(hlq.FILEA)
*         RECORDFORMAT(F) STRINGS(3)
*         RECORDSIZE(80) KEYLENGTH(6)
*         DATABUFFERS(4) INDEXBUFFERS(3)
*         GROUP(AVZCTS)

  ADD    GROUP(AVZCTS)   LIST(CICSLAB)
/*

//****************************************************************
//*  Step IEBCOPY - Set common environment variables
//****************************************************************
//IEBGENER EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1   DD *
JAVA_HOME=/usr/lpp/java/J8.0_64
ZCEEPATH=/usr/lpp/IBM/zosconnect/v3r0
//SYSUT2   DD DSN=&&STDENV,DISP=(,PASS),
//   DCB=(RECFM=FB,LRECL=80,BLKSIZE=80),SPACE=(TRK,(1,1))
//SYSIN    DD DUMMY
//****************************************************************
//*  Step ZCSETUP - Invoke the zconsetup script
//****************************************************************
//ZCSETUP  EXEC PGM=IKJEFT01,REGION=0M
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//STDENV   DD DSN=&&STDENV,DISP=(OLD,DELETE)
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 BPXBATCH SH $ZCEEPATH/bin/zconsetup install

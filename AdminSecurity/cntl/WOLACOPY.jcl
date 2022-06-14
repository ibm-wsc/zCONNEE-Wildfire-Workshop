//****************************************************************
//*  SET SYMBOLS
//****************************************************************
//EXPORT EXPORT SYMLIST=(*)
// SET DSNAME='USER1.WOLA2008.LOADLIB'
// SET ZCEEPATH='/usr/lpp/IBM/zosconnect/v3r0'
// SET JAVAHOME='/usr/lpp/java/J8.0_64'
//****************************************************************
//*  Step ALLOC    - Allocate a PDSE load library
//****************************************************************
//ALLOC EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *,SYMBOLS=EXECSYS
  DELETE '&DSNAME'
  SET MAXCC=0
  ALLOC DSNAME('&DSNAME') -
     NEW CATALOG SPACE(2,1) DSORG(PO) CYLINDERS -
     RECFM(U) DSNTYPE(LIBRARY)
//****************************************************************
//*  Step WOLACOPY - copy the WOLA executables to the PDSE
//****************************************************************
//WOLACOPY EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=EXECSYS
BPXBATCH SH +
  export JAVA_HOME=&JAVAHOME; +
  export DSNAME=&DSNAME; +
  cp -Xv &ZCEEPATH/wlp/clients/zos/* "//'$DSNAME'"

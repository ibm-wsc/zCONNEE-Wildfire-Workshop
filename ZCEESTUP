//ZCEESTUP JOB 'ZCEE',CLASS=A,REGION=0M,MSGCLASS=H,NOTIFY=&SYSUID
//******************************************************
//*  SET SYMBOLS
//******************************************************
//EXPORT EXPORT SYMLIST=(*)
// SET JAVAHOME='/usr/lpp/java/J8.0_64'
// SET ZCEEPATH='/usr/lpp/IBM/zosconnect/v3r0'
//******************************************************
//*  STEP ZCSETUP - INVOKE THE ZCONSETUP SCRIPT
//******************************************************
//ZCSETUP  EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//STDENV   DD DUMMY
//SYSTSIN  DD *,SYMBOLS=EXECSYS
 BPXBATCH SH +
 export JAVA_HOME=&JAVAHOME; +
 &ZCEEPATH/bin/zconsetup install

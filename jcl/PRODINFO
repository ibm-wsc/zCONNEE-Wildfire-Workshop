//****************************************************************
//*  SET SYMBOLS
//****************************************************************
//EXPORT EXPORT SYMLIST=(*)
// SET WLPDIR='/usr/lpp/IBM/zosconnect/v3r0/wlp'
//PRODINFO EXEC PGM=IKJEFT01,REGION=0M,MEMLIMIT=4G
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=EXECSYS
BPXBATCH SH +
export WLPDIR=&WLPDIR; +
$WLPDIR/bin/productInfo validate | grep 'zosconnect'

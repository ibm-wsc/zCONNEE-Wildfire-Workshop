//COPYARA JOB 'ZCEE',CLASS=A,REGION=0M,MSGCLASS=H,NOTIFY=&SYSUID
//****************************************************************
//*  Step COPYARA  - Use the cp command to copy an ARA file
//****************************************************************
//COPYARA EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSTSIN  DD *
 BPXBATCH SH +
cp /var/zcee/shared/apiRequesters/miniloan.ara +
   /var/zcee/zceeapir/resources/zosconnect/apiRequesters

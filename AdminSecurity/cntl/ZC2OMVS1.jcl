//ZC2OMVS1 JOB (0),'ZCEE',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//****************************************************************
//*  Step ZC2OMVS1 - Create the data transform directory structure
//****************************************************************
//ZC2OMVS1 EXEC PGM=IKJEFT01,REGION=0M
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//STDENV   DD *
DATAXFORM=/var/ats/zosconnect/servers/server1
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 BPXBATCH SH mkdir $DATAXFORM/dataXform
 BPXBATCH SH mkdir $DATAXFORM/dataXform/json
 BPXBATCH SH mkdir $DATAXFORM/dataXform/bind
 BPXBATCH SH mkdir $DATAXFORM/dataXform/sars
 BPXBATCH SH chown -R ATSSERV:SYS1 $DATAXFORM/dataXform
 BPXBATCH SH ls -al $DATAXFORM
 BPXBATCH SH ls -al $DATAXFORM/dataXform

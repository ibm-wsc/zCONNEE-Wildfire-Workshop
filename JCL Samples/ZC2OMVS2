//ZC2OMVS2 JOB (0),'ZCEE',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//*****************************************************************
//*  Step ZC2OMVS1 - Create the z/OS Connect resource API structure
//*****************************************************************
//ZCEEZRVR EXEC PGM=IKJEFT01,REGION=0M
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//STDENV   DD *
RESOURCES=/var/ats/zosconnect/servers/server1
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 BPXBATCH SH mkdir $RESOURCES/resources
 BPXBATCH SH mkdir $RESOURCES/resources/zosconnect
 BPXBATCH SH mkdir $RESOURCES/resources/zosconnect/apis
 BPXBATCH SH chown -R ATSSERV:SYS1 $RESOURCES
 BPXBATCH SH ls -al $RESOURCES/resources
 BPXBATCH SH ls -al $RESOURCES/resources/zosconnect
 BPXBATCH SH ls -al $RESOURCES/resources/zosconnect/apis

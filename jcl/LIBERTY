//LIBERTY JOB 'ZCEE',CLASS=A,REGION=0M,MSGCLASS=H,NOTIFY=&SYSUID
//****************************************************************
//*  SET SYMBOLS
//****************************************************************
//EXPORT EXPORT SYMLIST=(*)
// SET JAVAHOME='/usr/lpp/java/J8.0_64'
// SET WLPPATH='/usr/lpp/IBM/zosconnect/v3r0/wlp'
// SET SERVER='wlpopsrv'
// SET WLPUSER='/var/wlp'
// SET USER='ATSSERV'
// SET GROUP='ATSGRP'
//****************************************************************
//*  Step SERVER - Use the server command to create a server
//****************************************************************
//SERVER EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=EXECSYS
 BPXBATCH SH +
 export JAVA_HOME=&JAVAHOME; +
 export WLP_USER_DIR=&WLPUSER; +
 &WLPPATH/bin/server create &SERVER
//*****************************************************************
//*  Step CHOWN - Change directory and file ownership
//*****************************************************************
//CHOWN  EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSERR   DD SYSOUT=*
//STDOUT   DD SYSOUT=*
//SYSTSIN  DD *,SYMBOLS=EXECSYS
 BPXBATCH SH +
 export WLP_USER_DIR=&WLPUSER; +
 chown -R &USER:&GROUP $WLP_USER_DIR/servers/&SERVER

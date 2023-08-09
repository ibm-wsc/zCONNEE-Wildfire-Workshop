//CICSRCF2 JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//CLEANUP EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 /* Create CICS key ring and connect CA and personal certificates */
racdcert id(cicsstc) addring(CICS.KeyRing)

racdcert id(cicsstc) connect(ring(CICS.KeyRing) +
          label('CICS CA') certauth usage(certauth))

 /* Connect default personal certificiate */
racdcert id(cicsstc) connect(ring(CICS.KeyRing) +
          label('CICS Client Cert') default

racdcert id(cicsstc) connect(ring(CICS.KeyRing) +
          label('Liberty CA') certauth usage(certauth))

setropts raclist(digtring,digtnmap) refresh

connect  cicsstc group(zceeusrs)

connect  cicsstc group(gminvoke)

/*

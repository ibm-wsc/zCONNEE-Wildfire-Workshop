//CICSRCF3 JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//CLEANUP EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 /* Connect the CICS CA certificate to the Liberty inbound key ring */
racdcert id(libserv) connect(ring(Liberty.KeyRing) +
          label('CICS CA') certauth usage(certauth))

setropts raclist(digtring,digtnmap) refresh

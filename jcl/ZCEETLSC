//ZCEETLSC JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,
//             MSGCLASS=H,NOTIFY=&SYSUID
//CLEANUP EXEC PGM=IKJEFT01,REGION=0M
//SYSTSPRT DD SYSOUT=*
//SYSTSIN  DD *
 /* Create personal certficate for zCEE outbound client request */
racdcert id(libserv) gencert subjectsdn(cn('zCEE Client Cert') +
ou('ATS') o('IBM')) withlabel('zCEE Client Cert') signwith(certauth +
label('zCEE CA')) notafter(date(2022/12/31))

 /* Create zCEE outbound key ring and connect certificates */
racdcert id(libserv) addring(zCEE.KeyRing)

racdcert id(libserv) connect(ring(zCEE.KeyRing) +
          label('zCEE CA') certauth usage(certauth))

racdcert id(libserv) connect(ring(zCEE.KeyRing) +
          label('Liberty CA') certauth usage(certauth))

 /* Connect CA certificate to Liberty inbound key ring */
racdcert id(libserv) connect(ring(Liberty.KeyRing) +
          label('zCEE CA') certauth usage(certauth))

 /* Connect default personal certificate */
racdcert id(libserv) connect(ring(zCEE.KeyRing) +
          label('zCEE Client Cert') default)

setr raclist(digtcert digtring) refresh

connect   libserv  group(zceeusrs)
connect   libserv  group(gminvoke)

racdcert id(libserv) listring(zCEE.KeyRing)
racdcert id(libserv) list

//MQTLS   JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,                     00010000
//             MSGCLASS=H,NOTIFY=&SYSUID                                00020000
//RACDCERT EXEC PGM=IKJEFT01,REGION=0M                                  00030000
//SYSTSPRT DD SYSOUT=*                                                  00040000
//SYSTSIN  DD *                                                         00050000
 /* Create a CA certficate for MQ */                                    00060002
racdcert certauth gencert subjectsdn(cn('MQ CA') ou('ATS') +            00070002
o('IBM')) withlabel('MQ CA') keyusage(certsign) +                       00080002
notafter(date(2022/12/31))                                              00090002
                                                                        00100002
 /* Create a server certficate for MQ TLS request */                    00110002
racdcert id(MQSTC) gencert subjectsdn(cn('wg31.washington.ibm.com') +   00120002
ou('ATS') o('IBM')) withlabel('MQ CHIN') signwith(certauth +            00130002
label('MQ CA')) notafter(date(2021/12/31)                               00140002
                                                                        00150002
setropts raclist(digtring,digtnmap) refresh                             00160002
                                                                        00170002
 /* Create a personal certficate for MQ TLS request */                  00180002
racdcert id(user1) gencert subjectsdn(cn('user1') +                     00190002
ou('ATS') o('IBM')) withlabel('MQ Client Cert') signwith(certauth +     00200002
label('MQ CA')) notafter(date(2022/12/31))                              00210002
                                                                        00220002
 /* Create MQ key ring and connect CA and personal certificates */      00230002
 racdcert id(MQSTC) addring(MQ.KEYRING)                                 00240004
                                                                        00250002
 racdcert id(MQSTC) connect(ring(MQ.KEYRING) +                          00260004
          label('MQ CA') certauth usage(certauth))                      00270002
                                                                        00280002
 racdcert id(MQSTC) connect(ring(MQ.KEYRING) +                          00290004
          label('Liberty CA') certauth usage(certauth))                 00300006
                                                                        00310002
 racdcert id(MQSTC) connect(ring(MQ.KEYRING) +                          00320006
          label('zCEE CA') certauth usage(certauth))                    00330006
                                                                        00340006
 racdcert id(libserv) connect(ring(Liberty.KeyRing) +                   00350005
           label('MQ CA') certauth usage(certauth))                     00360002
                                                                        00400002
 racdcert id(libserv) connect(ring(zCEE.KeyRing) +                      00401005
           label('MQ CA') certauth usage(certauth))                     00402005
                                                                        00403005
  /* Connect default personal certificiate */                           00410002
 racdcert id(MQSTC) connect(ring(MQ.KEYRING) +                          00420002
           label('MQ CHIN') default                                     00430002
                                                                        00440002
 racdcert certauth EXPORT(LABEL('MQ CA')) -                             00450002
   DSN('USER1.MQCACERT.PEM')                                            00460002
                                                                        00470002
 racdcert id(user1) export(label('MQ Client Cert')) -                   00480003
   DSN('USER1.MQCERT.P12') FORMAT(PKCS12DER) PASSWORD('secret')         00490002
                                                                        00500002
 setropts raclist(digtring,digtnmap) refresh                            00510002

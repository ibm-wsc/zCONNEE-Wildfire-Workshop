//IMSTLS  JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,                     00010003
//             MSGCLASS=H,NOTIFY=&SYSUID                                00020000
//RACDCERT EXEC PGM=IKJEFT01,REGION=0M                                  00030000
//SYSTSPRT DD SYSOUT=*                                                  00040000
//SYSTSIN  DD *                                                         00050000
 /* Create a CA certficate for IMS */                                   00060000
racdcert certauth gencert subjectsdn(cn('IMS CA') ou('ATS') +           00070000
ou('ATS') o('IBM')) withlabel('IMS CA') keyusage(certsign) +            00080000
notafter(date(2022/12/31))                                              00090000
                                                                        00100000
 /* Create a server certficate for IMS client request */                00110000
racdcert id(IMSSTC) gencert subjectsdn(cn('wg31.washington.ibm.com') +  00120002
ou('ATS') o('IBM')) withlabel('IMSSTC') signwith(certauth +             00130002
label('IMS CA')) notafter(date(2021/12/31)                              00140000
                                                                        00150000
setr raclist(digtcert,digtnmap) refresh                                 00160000
                                                                        00170000
 /* Create IMS key ring and connect CA and personal certificates */     00180000
racdcert id(IMSSTC) addring(IMS.KeyRing)                                00190002
                                                                        00200000
racdcert id(IMSSTC) connect(ring(IMS.KeyRing) +                         00210002
          label('IMS CA') certauth usage(certauth))                     00220000
                                                                        00230000
racdcert id(IMSSTC) connect(ring(IMS.KeyRing) +                         00240002
          label('zCEE CA') certauth usage(certauth))                    00250000
                                                                        00260000
racdcert id(libserv) connect(ring(zCEE.KeyRing) +                       00270000
          label('IMS CA') certauth usage(certauth))                     00280000
                                                                        00290000
 /* Connect default personal certificiate */                            00300000
racdcert id(IMSSTC) connect(ring(IMS.KeyRing) +                         00310002
          label('IMSSTC') default                                       00320002
                                                                        00330000
setropts raclist(digtring,digtnmap) refresh                             00340000
                                                                        00350000

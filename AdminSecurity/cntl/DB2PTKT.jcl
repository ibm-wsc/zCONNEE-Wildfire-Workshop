//DB2PTKT  JOB CLASS=A,REGION=0M,                                       00010005
//             MSGCLASS=H,NOTIFY=&SYSUID                                00020000
//RACDCERT EXEC PGM=IKJEFT01,REGION=0M                                  00030000
//SYSTSPRT DD SYSOUT=*                                                  00040000
//SYSTSIN  DD *                                                         00050000
                                                                        00350000
SETROPTS CLASSACT(PTKTDATA) RACLIST(PTKTDATA)                           00360000
SETROPTS GENERIC(PTKTDATA)                                              00370000
                                                                        00380000
RDEFINE PTKTDATA DSN2APPL SSIGNON(KEYMASK(123456789ABCDEF0)) +          00390001
APPLDATA('NO REPLAY PROTECTION')                                        00400000
                                                                        00410000
RDEFINE PTKTDATA IRRPTAUTH.DSN2APPL.* UACC(NONE)                        00420000
PERMIT IRRPTAUTH.DSN2APPL.* ID(LIBSERV) CLASS(PTKTDATA) ACC(UPDATE)     00430000
                                                                        00440000
SETROPTS RACLIST(PTKTDATA) REFRESH                                      00450001

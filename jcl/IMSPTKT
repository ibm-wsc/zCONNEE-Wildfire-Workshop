//IMSPTKT JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,                     00010006
//             MSGCLASS=H,NOTIFY=&SYSUID                                00020000
//RACDCERT EXEC PGM=IKJEFT01,REGION=0M                                  00030000
//SYSTSPRT DD SYSOUT=*                                                  00040000
//SYSTSIN  DD *                                                         00050000
ADDGROUP ZCEEIMS                                                        00051003
                                                                        00051103
CONNECT (FRED,USER1,LIBSERV) GROUP(ZCEEIMS)                             00052005
                                                                        00060000
SETROPTS CLASSACT(PTKTDATA) RACLIST(PTKTDATA)                           00070000
SETROPTS GENERIC(PTKTDATA)                                              00080000
                                                                        00090000
RDEFINE PTKTDATA IMSAPPL SSIGNON(KEYMASK(123456789ABCDEF0)) +           00100001
APPLDATA('NO REPLAY PROTECTION')                                        00110000
                                                                        00120000
RDEFINE PTKTDATA IRRPTAUTH.IMSAPPL.* UACC(NONE)                         00130000
PERMIT IRRPTAUTH.IMSAPPL.* ID(ZCEEIMS) CLASS(PTKTDATA) ACC(UPDATE)      00140003
                                                                        00150000
SETROPTS RACLIST(PTKTDATA) REFRESH                                      00160001

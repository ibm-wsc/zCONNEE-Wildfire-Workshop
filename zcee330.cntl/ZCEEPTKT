//ZCEEPTKT JOB (0),'LIBERTY RACF',CLASS=A,REGION=0M,                    00010002
//             MSGCLASS=H,NOTIFY=&SYSUID                                00020000
//RACDCERT EXEC PGM=IKJEFT01,REGION=0M                                  00030000
//SYSTSPRT DD SYSOUT=*                                                  00040000
//SYSTSIN  DD *                                                         00050000
RDEFINE PTKTDATA BBGZDFLT SSIGNON(KEYMASK(123456789ABCDEF0))            00100001
                                                                        00120000
RDEFINE PTKTDATA IRRPTAUTH.BBGZDFLT.* UACC(NONE)                        00130000
PERMIT IRRPTAUTH.BBGZDFLT.* ID(ZCEEUSRS) CLASS(PTKTDATA) ACC(UPDATE)    00140001
                                                                        00150000
SETROPTS RACLIST(PTKTDATA) REFRESH                                      00160000

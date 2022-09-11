       01 PART-REC.                                                     00010000
          03 PART-KEY PIC X(17).                                        00020000
          03 PART-KEY-DETAIL REDEFINES PART-KEY.                        00030000
             05 PART-PREFIX            PIC X(02).                       00040000
             05 PART-NUMBER            PIC X(15).                       00050000
          03 FILLER.                                                    00060000

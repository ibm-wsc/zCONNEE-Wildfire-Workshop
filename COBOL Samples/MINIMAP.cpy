       01  MINIMAPI.
           02  FILLER PIC X(12).
           02  NameL    COMP  PIC  S9(4).
           02  NameF    PICTURE X.
           02  FILLER REDEFINES NameF.
             03 NameA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  NameI  PIC X(20).
           02  AMOUNTL    COMP  PIC  S9(4).
           02  AMOUNTF    PICTURE X.
           02  FILLER REDEFINES AMOUNTF.
             03 AMOUNTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AMOUNTI  PIC X(18).
           02  AgeL    COMP  PIC  S9(4).
           02  AgeF    PICTURE X.
           02  FILLER REDEFINES AgeF.
             03 AgeA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  AgeI  PIC X(10).
           02  YRPAYMNTL    COMP  PIC  S9(4).
           02  YRPAYMNTF    PICTURE X.
           02  FILLER REDEFINES YRPAYMNTF.
             03 YRPAYMNTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  YRPAYMNTI  PIC X(18).
           02  CRDSCREL    COMP  PIC  S9(4).
           02  CRDSCREF    PICTURE X.
           02  FILLER REDEFINES CRDSCREF.
             03 CRDSCREA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  CRDSCREI  PIC X(18).
           02  INTERESTL    COMP  PIC  S9(4).
           02  INTERESTF    PICTURE X.
           02  FILLER REDEFINES INTERESTF.
             03 INTERESTA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INTERESTI  PIC X(5).
           02  INCOMEL    COMP  PIC  S9(4).
           02  INCOMEF    PICTURE X.
           02  FILLER REDEFINES INCOMEF.
             03 INCOMEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  INCOMEI  PIC X(18).
           02  EFDATEL    COMP  PIC  S9(4).
           02  EFDATEF    PICTURE X.
           02  FILLER REDEFINES EFDATEF.
             03 EFDATEA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  EFDATEI  PIC X(8).
           02  UIDL    COMP  PIC  S9(4).
           02  UIDF    PICTURE X.
           02  FILLER REDEFINES UIDF.
             03 UIDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  UIDI  PIC X(8).
           02  APPROVEDL    COMP  PIC  S9(4).
           02  APPROVEDF    PICTURE X.
           02  FILLER REDEFINES APPROVEDF.
             03 APPROVEDA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  APPROVEDI  PIC X(60).
           02  MSG1L    COMP  PIC  S9(4).
           02  MSG1F    PICTURE X.
           02  FILLER REDEFINES MSG1F.
             03 MSG1A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG1I  PIC X(60).
           02  MSG2L    COMP  PIC  S9(4).
           02  MSG2F    PICTURE X.
           02  FILLER REDEFINES MSG2F.
             03 MSG2A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG2I  PIC X(60).
           02  MSG3L    COMP  PIC  S9(4).
           02  MSG3F    PICTURE X.
           02  FILLER REDEFINES MSG3F.
             03 MSG3A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG3I  PIC X(60).
           02  MSG4L    COMP  PIC  S9(4).
           02  MSG4F    PICTURE X.
           02  FILLER REDEFINES MSG4F.
             03 MSG4A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG4I  PIC X(60).
           02  MSG5L    COMP  PIC  S9(4).
           02  MSG5F    PICTURE X.
           02  FILLER REDEFINES MSG5F.
             03 MSG5A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG5I  PIC X(60).
           02  MSG6L    COMP  PIC  S9(4).
           02  MSG6F    PICTURE X.
           02  FILLER REDEFINES MSG6F.
             03 MSG6A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG6I  PIC X(60).
           02  MSG7L    COMP  PIC  S9(4).
           02  MSG7F    PICTURE X.
           02  FILLER REDEFINES MSG7F.
             03 MSG7A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG7I  PIC X(60).
           02  MSG8L    COMP  PIC  S9(4).
           02  MSG8F    PICTURE X.
           02  FILLER REDEFINES MSG8F.
             03 MSG8A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG8I  PIC X(60).
           02  MSG9L    COMP  PIC  S9(4).
           02  MSG9F    PICTURE X.
           02  FILLER REDEFINES MSG9F.
             03 MSG9A    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSG9I  PIC X(60).
           02  MSGAL    COMP  PIC  S9(4).
           02  MSGAF    PICTURE X.
           02  FILLER REDEFINES MSGAF.
             03 MSGAA    PICTURE X.
           02  FILLER   PICTURE X(2).
           02  MSGAI  PIC X(60).
       01  MINIMAPO REDEFINES MINIMAPI.
           02  FILLER PIC X(12).
           02  FILLER PICTURE X(3).
           02  NameC    PICTURE X.
           02  NameH    PICTURE X.
           02  NameO  PIC X(20).
           02  FILLER PICTURE X(3).
           02  AMOUNTC    PICTURE X.
           02  AMOUNTH    PICTURE X.
           02  AMOUNTO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  AgeC    PICTURE X.
           02  AgeH    PICTURE X.
           02  AgeO  PIC X(10).
           02  FILLER PICTURE X(3).
           02  YRPAYMNTC    PICTURE X.
           02  YRPAYMNTH    PICTURE X.
           02  YRPAYMNTO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  CRDSCREC    PICTURE X.
           02  CRDSCREH    PICTURE X.
           02  CRDSCREO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  INTERESTC    PICTURE X.
           02  INTERESTH    PICTURE X.
           02  INTERESTO  PIC X(5).
           02  FILLER PICTURE X(3).
           02  INCOMEC    PICTURE X.
           02  INCOMEH    PICTURE X.
           02  INCOMEO  PIC X(18).
           02  FILLER PICTURE X(3).
           02  EFDATEC    PICTURE X.
           02  EFDATEH    PICTURE X.
           02  EFDATEO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  UIDC    PICTURE X.
           02  UIDH    PICTURE X.
           02  UIDO  PIC X(8).
           02  FILLER PICTURE X(3).
           02  APPROVEDC    PICTURE X.
           02  APPROVEDH    PICTURE X.
           02  APPROVEDO  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG1C    PICTURE X.
           02  MSG1H    PICTURE X.
           02  MSG1O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG2C    PICTURE X.
           02  MSG2H    PICTURE X.
           02  MSG2O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG3C    PICTURE X.
           02  MSG3H    PICTURE X.
           02  MSG3O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG4C    PICTURE X.
           02  MSG4H    PICTURE X.
           02  MSG4O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG5C    PICTURE X.
           02  MSG5H    PICTURE X.
           02  MSG5O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG6C    PICTURE X.
           02  MSG6H    PICTURE X.
           02  MSG6O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG7C    PICTURE X.
           02  MSG7H    PICTURE X.
           02  MSG7O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG8C    PICTURE X.
           02  MSG8H    PICTURE X.
           02  MSG8O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSG9C    PICTURE X.
           02  MSG9H    PICTURE X.
           02  MSG9O  PIC X(60).
           02  FILLER PICTURE X(3).
           02  MSGAC    PICTURE X.
           02  MSGAH    PICTURE X.
           02  MSGAO  PIC X(60).

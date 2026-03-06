//DB2RESP1 JOB (0),MSGCLASS=X,CLASS=A,NOTIFY=&SYSUID,REGION=0M
//******************************************************************
//* FREE Db2 REST services                                         *
//******************************************************************
//FREE EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
FREE SERVICE("SYSIBMSERVICE"."getEmployee")
FREE SERVICE("SYSIBMSERVICE"."getEmployees")
FREE SERVICE("SYSIBMSERVICE"."addEmployee")
FREE SERVICE("SYSIBMSERVICE"."updateEmployee")
FREE SERVICE("SYSIBMSERVICE"."deleteEmployee")
//******************************************************************
//* BIND Db2 getEmployee REST service                              *
//******************************************************************
//BIND EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//DSNSTMT  DD *
  SELECT EMPNO AS "employeeNumber", FIRSTNME AS "firstName",
         MIDINIT AS "middleInitial", LASTNAME as "lastName",
        WORKDEPT AS "department", PHONENO AS "phoneNumber",
        JOB AS "job", EDLEVEL AS "education level", SEX as "sex",
        BIRTHDATE as "birthDate", SALARY as "salary",
        BONUS as "bonus", COMM AS "commission"
  FROM USER1.EMPLOYEE WHERE EMPNO = :employeeNumber
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
BIND SERVICE("SYSIBMSERVICE") -
NAME("getEmployee") -
SQLENCODING(1047) -
DESCRIPTION('Get the details of all employees')
//******************************************************************
//* BIND Db2 getEmployees REST service                             *
//******************************************************************
//BIND EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//DSNSTMT  DD *
  SELECT EMPNO AS "employeeNumber", FIRSTNME AS "firstName",
         MIDINIT AS "middleInitial", LASTNAME as "lastName",
        WORKDEPT AS "department", PHONENO AS "phoneNumber",
        JOB AS "job", EDLEVEL AS "education level", SEX as "sex",
        BIRTHDATE as "birthDate", SALARY as "salary",
        BONUS as "bonus", COMM AS "commission"
  FROM USER1.EMPLOYEE WHERE WORKDEPT LIKE :department AND JOB LIKE :job
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
BIND SERVICE("SYSIBMSERVICE") -
NAME("getEmployees") -
SQLENCODING(1047) -
DESCRIPTION('Get the details of all employees')
//******************************************************************
//* BIND Db2 addEmployee REST service                              *
//******************************************************************
//BIND EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//DSNSTMT  DD *
  INSERT INTO USER1.EMPLOYEE
         (EMPNO,FIRSTNME,MIDINIT,LASTNAME,WORKDEPT,PHONENO,
          HIREDATE,JOB,EDLEVEL,SEX,BIRTHDATE,SALARY,BONUS,COMM)
     VALUES (:employeeNumber, :firstName, :middleInitial, :lastName,
             :department, :phoneNumber, :hireDate, :job,
             :educationLevel, :sex, :birthDate,
             :salary, :bonus, :commission)
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
BIND SERVICE("SYSIBMSERVICE") -
NAME("addEmployee") -
SQLENCODING(1047) -
DESCRIPTION('Add the details of an individual employee')
//******************************************************************
//* BIND Db2 updateEmployee REST service                           *
//******************************************************************
//BIND EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//DSNSTMT  DD *
  UPDATE USER1.EMPLOYEE
         SET WORKDEPT = :department, PHONENO = :phoneNumber,
             JOB = :job, EDLEVEL = :educationLevel,
             SALARY = :salary, BONUS  = :bonus, COMM = :commission
             WHERE EMPNO = :employeeNumber
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
BIND SERVICE("SYSIBMSERVICE") -
NAME("updateEmployee") SQLENCODING(1047) -
DESCRIPTION('Update the details of an individual employee')
//******************************************************************
//* BIND Db2 deleteEmployee REST service                           *
//******************************************************************
//BIND EXEC PGM=IKJEFT01,DYNAMNBR=20
//STEPLIB  DD DSN=DSN1210.DB2.SDSNEXIT,DISP=SHR
//         DD DSN=DSN1210.DB2.SDSNLOAD,DISP=SHR
//SYSTSPRT DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//DSNSTMT  DD *
  DELETE FROM USER1.EMPLOYEE
             WHERE EMPNO = :employeeNumber
//SYSTSIN  DD *
DSN SYSTEM(DSN2)
BIND SERVICE("SYSIBMSERVICE") -
NAME("deleteEmployee") SQLENCODING(1047) -
DESCRIPTION('Remove the details of an individual employee')

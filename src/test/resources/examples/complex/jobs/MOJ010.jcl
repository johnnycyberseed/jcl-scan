//* ----------------------------------------------------------------
//* JOB 10 — Distribute daily report to printer and archive copy
//* ----------------------------------------------------------------
//MOJ010  JOB (ACCT),'RPT DIST',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//PRINT   EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1  DD DSN=MO.REPORTS.DAILY,DISP=SHR
//SYSUT2  DD SYSOUT=A
//SYSIN   DD DUMMY
//ARCH    EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1  DD DSN=MO.REPORTS.DAILY,DISP=SHR
//SYSUT2  DD DSN=MO.ARCH.REPORTS.D&YYMMDD,DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(5,2),RLSE),DCB=(RECFM=VB,LRECL=300,BLKSIZE=0)
//SYSIN   DD DUMMY

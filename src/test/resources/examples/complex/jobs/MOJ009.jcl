//* ----------------------------------------------------------------
//* JOB 09 — Backup load library to a backup PDS (IEBCOPY)
//* ----------------------------------------------------------------
//MOJ009  JOB (ACCT),'BKUP LOAD',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//BK1     EXEC PGM=IEBCOPY
//SYSPRINT DD SYSOUT=*
//SYSUT1  DD DSN=MO.COBOL.LOAD,DISP=SHR
//SYSUT2  DD DSN=MO.COBOL.BKUP.LOAD,DISP=SHR
//SYSIN   DD *
  COPY OUTDD=SYSUT2,INDD=SYSUT1
/*

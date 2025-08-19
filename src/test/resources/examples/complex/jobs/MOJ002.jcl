//* ----------------------------------------------------------------
//* JOB 02 — Allocate control/report/log datasets (IEFBR14 via MOALLOC)
//* ----------------------------------------------------------------
//MOJ002  JOB (ACCT),'ALLOC FIXED',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//CTL1    EXEC MOALLOC,DSN=MO.DW.CNTRL.CARD,RECFM='FB',LRECL=80,SPACE='(TRK,(1,1),RLSE)'
//RPT1    EXEC MOALLOC,DSN=MO.REPORTS.DAILY,RECFM='VB',LRECL=300,SPACE='(CYL,(5,5),RLSE)'
//LOG1    EXEC MOALLOC,DSN=MO.LOGS.BATCH1,RECFM='FB',LRECL=133,SPACE='(CYL,(5,10),RLSE)'

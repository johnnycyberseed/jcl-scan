//* ----------------------------------------------------------------
//* JOB 12 — Housekeeping: delete WIP datasets, list members in LOAD
//* ----------------------------------------------------------------
//MOJ012  JOB (ACCT),'HKP',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//DELWIP  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN   DD *
  DELETE MO.WIP.** MASK
  SET MAXCC = 0
/*
//LISTLIB EXEC PGM=IEHLIST
//SYSPRINT DD SYSOUT=*
//SYSIN   DD *
  LISTPDS DSNAME=MO.COBOL.LOAD, MEMBERS
/*

//* ----------------------------------------------------------------
//* JOB 01 — Define GDG Bases for inbound/outbound/workflows (IDCAMS)
//* ----------------------------------------------------------------
//MOJ001  JOB (ACCT),'DEF GDG',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//DEFBASE EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
  DEFINE GDG (NAME(MO.DW.IN.GDG)  LIMIT(7)  SCRATCH EMPTY)
  DEFINE GDG (NAME(MO.DW.OUT.GDG) LIMIT(14) SCRATCH NOEMPTY)
  DEFINE GDG (NAME(MO.WIP.WORK.GDG) LIMIT(3) SCRATCH EMPTY)
/*

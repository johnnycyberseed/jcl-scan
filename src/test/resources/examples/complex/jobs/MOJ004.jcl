//* ----------------------------------------------------------------
//* JOB 04 — Sort inbound file by key to WIP (MOSORT, inline SYSIN override)
//* ----------------------------------------------------------------
//MOJ004  JOB (ACCT),'SORT IN',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//SRT     EXEC MOSORT,IN=MO.DW.IN.GDG(0),OUT=MO.WIP.WORK.GDG(+1),SYSINLIB=MO.DUMMY,SYSINMEM=DUMMY
//S1.SYSIN DD *
  SORT FIELDS=(1,10,CH,A)
/*

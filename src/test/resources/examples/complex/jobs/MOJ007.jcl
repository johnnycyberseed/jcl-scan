//* ----------------------------------------------------------------
//* JOB 07 — Easytrieve ad‑hoc report over latest outbound
//* ----------------------------------------------------------------
//MOJ007  JOB (ACCT),'EZT RPT',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//EZT     EXEC MOEASYGO,MEMBER=SALESUM,PARM=''
//EZTRUN.INPUT  DD DSN=MO.DW.OUT.GDG(0),DISP=SHR
//EZTRUN.REPORT DD DSN=MO.REPORTS.DAILY,DISP=MOD

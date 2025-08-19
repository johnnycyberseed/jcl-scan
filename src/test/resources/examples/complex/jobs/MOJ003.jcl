//* ----------------------------------------------------------------
//* JOB 03 — Ingest partner file to new inbound GDG gen (IEBGENER)
//* ----------------------------------------------------------------
//MOJ003  JOB (ACCT),'INGEST IN',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//SETVARS SET INFILE=MO.PARTNER.INBOUND.FILE
//COPYIN  EXEC PGM=IEBGENER
//SYSPRINT DD SYSOUT=*
//SYSUT1  DD DSN=&INFILE,DISP=SHR
//SYSUT2  DD DSN=MO.DW.IN.GDG(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(50,50),RLSE),
//            DCB=(RECFM=VB,LRECL=400,BLKSIZE=0)
//SYSIN   DD DUMMY

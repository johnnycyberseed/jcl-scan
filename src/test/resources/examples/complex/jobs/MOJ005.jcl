//* ----------------------------------------------------------------
//* JOB 05 — Validate records (COBOL compile/link/go via MOCBLCG)
//*           Input: WIP sorted; Output: validated GDG
//* ----------------------------------------------------------------
//MOJ005  JOB (ACCT),'CBL VALID',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//CBL1    EXEC MOCBLCG,MEMBER=VALIDATE,PARMCBL='RENT,LIST,APOST',PARMGO=''
//GO.INFILE  DD DSN=MO.WIP.WORK.GDG(0),DISP=SHR
//GO.OUTFILE DD DSN=MO.DW.VALID.GDG(+1),DISP=(NEW,CATLG,DELETE),
//              SPACE=(CYL,(20,10),RLSE),DCB=(RECFM=VB,LRECL=400,BLKSIZE=0)
//GO.SYSOUT  DD SYSOUT=*

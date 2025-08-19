//* ----------------------------------------------------------------
//* JOB 06 — Transform validated -> outbound (build only + separate run)
//*           Compile/Link: MOCBLCL (IGYWCL) ; Run: PGM=TRANSFORM
//* ----------------------------------------------------------------
//MOJ006  JOB (ACCT),'CBL XFORM',CLASS=A,MSGCLASS=H,MSGLEVEL=(1,1),NOTIFY=&SYSUID
//        JCLLIB ORDER=(MO.PROCLIB,SIGYPROC,SYS1.PROCLIB)
//BUILD   EXEC MOCBLCL,MEMBER=TRANSFORM,PARMCBL='RENT,LIST'
//RUN     EXEC PGM=TRANSFORM,REGION=0M
//STEPLIB DD  DSN=MO.COBOL.LOAD,DISP=SHR
//INFILE  DD  DSN=MO.DW.VALID.GDG(0),DISP=SHR
//OUTFILE DD  DSN=MO.DW.OUT.GDG(+1),DISP=(NEW,CATLG,DELETE),
//            SPACE=(CYL,(20,10),RLSE),DCB=(RECFM=VB,LRECL=400,BLKSIZE=0)
//SYSPRINT DD SYSOUT=*

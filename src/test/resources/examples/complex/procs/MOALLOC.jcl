//* ----------------------------------------------------------------
//* PROC #1: MOALLOC — Allocate a sequential dataset (IEFBR14 wrapper)
//* ----------------------------------------------------------------
//MOALLOC PROC DSN=,DISPNEW='(NEW,CATLG,DELETE)',SPACE='(TRK,(1,1),RLSE)',
//             RECFM='FB',LRECL=80,BLKSIZE=0,UNIT=SYSDA
//ALLOC   EXEC PGM=IEFBR14
//DD1     DD  DSN=&DSN,DISP=&DISPNEW,UNIT=&UNIT,
//             SPACE=&SPACE,DCB=(RECFM=&RECFM,LRECL=&LRECL,BLKSIZE=&BLKSIZE)
//* ---- End MOALLOC ------------------------------------------------
//* ----------------------------------------------------------------
//* PROC #2: MOSORT — General DFSORT sort/copy using control from a lib member
//*    Symbolics:
//*      IN=...    input DSN
//*      OUT=...   output DSN
//*      SYSINLIB= control statements PDS
//*      SYSINMEM= member name containing SORT control cards
//*      FILSZ=    rough size for work space
//* ----------------------------------------------------------------
//MOSORT  PROC IN=,OUT=,SYSINLIB=,SYSINMEM=,FILSZ=E32
//S1      EXEC PGM=SORT,REGION=0M
//SYSOUT  DD  SYSOUT=*
//SORTIN  DD  DSN=&IN,DISP=SHR
//SORTOUT DD  DSN=&OUT,DISP=SHR
//SYSIN   DD  DSN=&SYSINLIB(&SYSINMEM),DISP=SHR
//SORTWK01 DD UNIT=SYSDA,SPACE=(CYL,(&FILSZ)),DISP=(,DELETE)
//SORTWK02 DD UNIT=SYSDA,SPACE=(CYL,(&FILSZ)),DISP=(,DELETE)
//SORTWK03 DD UNIT=SYSDA,SPACE=(CYL,(&FILSZ)),DISP=(,DELETE)
//* (Override S1.SYSIN in the JOB to use inline control cards if preferred)
//* ---- End MOSORT -------------------------------------------------
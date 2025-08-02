//DLIBATCH JOB (ACCT),MSGCLASS=H,NOTIFY=&SYSUID
//STEP1      EXEC DLIBATCH,COND=(1,LT),
//           MBR=EZTRPT,
//           PSB=CBLWCPB
//*
//IMSDB1     DD DSN=?sitehlq?.IMSDB1,DISP=SHR           
//IMSDB2     DD DSN=?sitehlq?.IMSDB2,DISP=SHR           
//IMSDB3     DD DSN=?sitehlq?.IMSDB3,DISP=SHR           
//*
//SYSOUT   DD SYSOUT=*
//SYSPRINT DD SYSOUT=*
//
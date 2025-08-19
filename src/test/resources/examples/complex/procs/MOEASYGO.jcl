//* ----------------------------------------------------------------
//* PROC #5: MOEASYGO — Run an Easytrieve program
//*    Symbolics: MEMBER, SRCLIB, STEPLIB1, WORK, PARM
//* ----------------------------------------------------------------
//MOEASYGO PROC MEMBER=,SRCLIB=MO.EZT.SRC,PARM=' ',
//             STEPLIB1=MO.EZT.LOAD,WORK=MO.EZT.WORK
//EZTRUN  EXEC PGM=EZTPA00,PARM='&PARM'
//STEPLIB DD  DSN=&STEPLIB1,DISP=SHR
//SYSPRINT DD SYSOUT=*
//EZTVFM DD  DSN=&WORK,DISP=SHR
//SYSOUT  DD SYSOUT=*
//SYSIN   DD DSN=&SRCLIB(&MEMBER),DISP=SHR
//* (Override EZTRUN.ddnames in the JOB to pass input/output) 
//* ---- End MOEASYGO ----------------------------------------------

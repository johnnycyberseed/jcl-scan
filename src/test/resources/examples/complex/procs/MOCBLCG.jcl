//* ----------------------------------------------------------------
//* PROC #3: MOCBLCG — COBOL Compile, Link, and Go (wraps IGYWCLG)
//*    Requires IBM Enterprise COBOL cataloged procedures
//*    Symbolics: MEMBER, SRCLIB, LOADLIB, PARMCBL, PARMLKED, PARMGO
//* ----------------------------------------------------------------
//MOCBLCG PROC MEMBER=,SRCLIB=MO.COBOL.SRC,LOADLIB=MO.COBOL.LOAD,
//             PARMCBL=' ',PARMLKED=' ',PARMGO=' '
//C1      EXEC IGYWCLG,PARM.COBOL='&PARMCBL',PARM.LKED='&PARMLKED',PARM.GO='&PARMGO'
//COBOL.SYSIN DD DSN=&SRCLIB(&MEMBER),DISP=SHR
//LKED.SYSLMOD DD DSN=&LOADLIB(&MEMBER),DISP=SHR
//GO.SYSOUT DD SYSOUT=* 
//* (Override GO.ddnames in the JOB to feed input/output files to the run step)
//* ---- End MOCBLCG -----------------------------------------------

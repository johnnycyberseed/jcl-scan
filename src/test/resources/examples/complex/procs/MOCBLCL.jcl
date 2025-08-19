//* ----------------------------------------------------------------
//* PROC #4: MOCBLCL — COBOL Compile & Link only (wraps IGYWCL)
//*    Use for build‑only pipelines; run module in a separate step.
//* ----------------------------------------------------------------
//MOCBLCL PROC MEMBER=,SRCLIB=MO.COBOL.SRC,LOADLIB=MO.COBOL.LOAD,
//             PARMCBL=' ',PARMLKED=' '
//C1      EXEC IGYWCL,PARM.COBOL='&PARMCBL',PARM.LKED='&PARMLKED'
//COBOL.SYSIN DD DSN=&SRCLIB(&MEMBER),DISP=SHR
//LKED.SYSLMOD DD DSN=&LOADLIB(&MEMBER),DISP=SHR
//* ---- End MOCBLCL -----------------------------------------------

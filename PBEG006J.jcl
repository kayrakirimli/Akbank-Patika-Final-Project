//PBE06HWJ JOB 1,NOTIFY=&SYSUID
//***************************************************/
//* Copyright Contributors to the COBOL Programming Course
//* SPDX-License-Identifier: CC-BY-4.0
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBEGIDX),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBEGIDX),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//COBRUN  EXEC IGYWCL
//COBOL.SYSIN  DD DSN=&SYSUID..CBL(PBE006HW),DISP=SHR
//LKED.SYSLMOD DD DSN=&SYSUID..LOAD(PBE006HW),DISP=SHR
//LKED.SYSLIB  DD DSN=&SYSUID..LOAD(PBEGIDX),DISP=SHR
//***************************************************/
// IF RC < 5 THEN
//***************************************************/
//RUN       EXEC PGM=PBE006HW
//STEPLIB   DD DSN=&SYSUID..LOAD,DISP=SHR
//INPFILE   DD DSN=&SYSUID..QSAM.INP,DISP=SHR
//IDXFILE   DD DSN=&SYSUID..VSAM.DD,DISP=SHR
//OUTFILE   DD DSN=&SYSUID..QSAM.HW,DISP=(NEW,CATLG,DELETE),
//             SPACE=(TRK,(20,20),RLSE),
//             DCB=(RECFM=FB,LRECL=131)
//SYSOUT    DD SYSOUT=*,OUTLIM=15000
//CEEDUMP   DD DUMMY
//SYSUDUMP  DD DUMMY
//***************************************************/
// ENDIF
// ENDIF

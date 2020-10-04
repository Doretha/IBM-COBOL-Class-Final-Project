//AUTOPART JOB ,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
// SET COBPGM='AUTOPART'
//**** Compile JCL ******
//STP0000 EXEC PROC=ELAXFCOC,
// CICS=,
// DB2=,
// COMP=
//COBOL.SYSPRINT DD SYSOUT=*
//SYSLIN DD DISP=SHR,
//        DSN=&SYSUID..FINAL.COBOL.OBJ(&COBPGM.)
//COBOL.SYSLIB DD DISP=SHR,DSN=&SYSUID..COBOL.COPYLIB
//             DD DISP=SHR,DSN=&SYSUID..FINAL.COPYLIB
//COBOL.SYSXMLSD DD DUMMY
//COBOL.SYSIN DD DISP=SHR,DSN=&SYSUID..FINAL.COBOL(&COBPGM.)
//****Link/Edit Step ******
//LKED EXEC PROC=ELAXFLNK
//LINK.SYSLIB DD DSN=CEE.SCEELKED,
//        DISP=SHR
//        DD DSN=&SYSUID..FINAL.LOAD,
//        DISP=SHR
//LINK.OBJ0000 DD DISP=SHR,
//        DSN=&SYSUID..FINAL.COBOL.OBJ(&COBPGM.)
//LINK.SYSLIN DD *
     INCLUDE OBJ0000
/*
//LINK.SYSLMOD   DD  DISP=SHR,
//        DSN=&SYSUID..FINAL.LOAD(&COBPGM.)
//*
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PROC=ELAXFGO,GO=&COBPGM.,
//        LOADDSN=&SYSUID..FINAL.LOAD
//CEEOPTS DD *
  TEST(,,,DBMDT%RTPOT44:*)
/*
//******* ADDITIONAL RUNTIME JCL HERE ******
//**************************************************************
//*  PARTS INPUT FILE
//**************************************************************
//PARTSIN   DD DSN=RTPOT44.AUTOPART.INPUT,DISP=SHR
//***************************************************************
//*  STATE/ZIP CODE INPUT FILE
//***************************************************************
//STATEZIP  DD DSN=RTPOT44.AUTOPART.STATEZIP.FILE,DISP=SHR
//***************************************************************
//*  PARTS OUTPUT FILE WITH GOOD RECORDS
//***************************************************************
//PARTFILE   DD DSN=RTPOT44.AUTOPART.PARTFILE,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=92,BLKSIZE=0,RECFM=FB,DSORG=PS)
//***************************************************************
//*  SUPPLIER OUTPUT FILE WITH GOOD RECORDS
//***************************************************************
//SUPPLIER   DD DSN=RTPOT44.AUTOPART.SUPPLIER,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=40,BLKSIZE=0,RECFM=FB,DSORG=PS)
//***************************************************************
//*  ADDRESS OUTPUT FILE WITH GOOD RECORDS
//***************************************************************
//SUPPADDR   DD DSN=RTPOT44.AUTOPART.SUPPADDR,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=80,BLKSIZE=0,RECFM=FB,DSORG=PS)
//****************************************************************
//*  PURCHASE ORDER OUTPUT FILE WITH GOOD RECORDS
//****************************************************************
//PURCHORD   DD DSN=RTPOT44.AUTOPART.PURCHORD,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=45,BLKSIZE=0,RECFM=FB,DSORG=PS)
//****************************************************************
//*  PARTS OUTPUT FILE WITH GOOD RECORDS - PARTS FILE
//*  INPUT RECORDS
//***************************************************************
//PARTSOUT   DD DSN=RTPOT44.AUTOPART.PARTSOUT,DISP=(NEW,CATLG,DELETE),
//           SPACE=(TRK,(1,1),RLSE),
//           DCB=(LRECL=473,BLKSIZE=0,RECFM=FB,DSORG=PS)
//***************************************************************
//*  ERROR FILE
//***************************************************************
//ERRFILE  DD DSN=RTPOT44.AUTOPART.ERRFILE,DISP=(NEW,CATLG,DELETE),
//            SPACE=(TRK,(1,1),RLSE),
//            DCB=(LRECL=623,BLKSIZE=0,RECFM=FB,DSORG=PS)
//***************************************************************
//*  PARTS REPORT WITH GOOD RECORDS
//***************************************************************
//PARTSRPT  DD SYSOUT=*

# IBM-COBOL-Class-Final-Project
IBM Cobol Class Final Project Files

MAIN PROGRAM DESCRIPTION:
   
      *****************************************************************
      *  PROGRAM DESCRIPTION:
      *    THIS PROGRAM READS AN AUTOPARTS INPUT FILE AND PERFORMS
      *    VALIDATION CHECKS ON RECORD FIELDS. A STATE/ZIP FILE IS READ
      *    AND LOADED INTO A TABLE TO VALIDATE ADDRESS STATE/ZIP CODE
      *    COMBINATIONS. RECORDS THAT CONTAIN ERRORS ARE WRITTEN TO
      *    AN ERROR FILE. SPECIFIC FIELDS FROM INPUT RECORDS WITHOUT
      *    ERRORS ARE WRITTEN TO OUTPUT FILES AND A REPORT.
      *****************************************************************
      *
      *  PROGRAM MODULES CALLED:
      *    - PARTSUPP.CBL - PERFORMS EDITS ON THE PARTS GROUP SECTION
      *      OF THE AUTOPART INPUT RECORD.
      *    - SUPPLIER.CBL - PERFORMS EDITS ON THE SUPPLIER GROUP SECTION
      *      OF THE AUTOPART INPUT RECORD.
      *    - ADDRSUPP.CBL - PERFORMS EDITS ON THE ADDRESS GROUP SECTION
      *      OF THE AUTOPART INPUT RECORD.
      *    - PURCHORD.CBL - PERFORMS EDITS ON THE PURCHASE ORDER SECTION
      *      OF THE AUTOPART INPUT RECORD.
      *    - CEEDAYS - IBM DATE VALIDATION SUBPROGRAM CALLED BY SUPPLIER
      *      AND PURCHORD SUBPROGRAMS.
      *****************************************************************
      *
      *    INPUT FILES:
      *      RTPOT44.AUTOPART.INPUT - AUTOPART INPUT FILE)
      *      INTERNAL FILE NAME:      PARTSIN
      *      JCL DD NAME:             PARTSIN
      *
      *
      *      RTPOT44.AUTOPART.STATEZIP.FILE  - STATE/ZIPCODE FILE
      *      INTERNAL FILE NAME:               STATEZIP
      *      JCL DD NAME:                      STATEZIP
      *
      *
      *    OUTPUT FILES:
      *      RTPOT44.AUTOPART.PARTFILE - PARTS GROUP FIELDS
      *      INTERNAL FILE NAME:         PARTFILE
      *      JCL DD NAME:                PARTFILE
      *
      *
      *      RTPOT44.AUTOPART.SUPPLIER - SUPPLIER GROUP FIELDS
      *      INTERNAL FILE NAME:         SUPPLIER
      *      JCL DD NAME:                SUPPLIER
      *
      *
      *      RTPOT44.AUTOPART.ADDRSUPP - ADDRESSES GROUP FIELDS
      *      INTERNAL FILE NAME:         ADDRSUPP
      *      JCL DD NAME:                ADDRSUPP
      *
      *
      *      RTPOT44.AUTOPART.PURCHORD - PURCHASE ORDER GROUP FIELDS
      *      INTERNAL FILE NAME:         PURCHORD
      *      JCL DD NAME:                PURCHORD
      *
      *
      *      RTPOT44.AUTOPART.PARTSOUT - AUTOPARTS RECORDS WITHOUT
      *                                  ERRORS
      *      INTERNAL FILE NAME:         PARTSOUT
      *      JCL DD NAME:                PARTSOUT
      *
      *
      *      RTPOT44.AUTOPART.ERRFILE -  AUTOPART RECORDS WITH ERRORS
      *      INTERNAL FILE NAME:         ERRFILE
      *      JCL DD NAME:                ERRFILE
      *
      *
      *      DD SYSOUT=* (PARTS REPORT) - GOOD PARTS RECORDS
      *                                 - FIELDS FROM GOOD AUTOPART
      *                                   INPUT RECORDS DISPLAYED IN
      *                                   REPORT FORMAT
      *      INTERNAL FILE NAME:          PARTSRPT
      *      JCL DD NAME:                 PARTSRPT
      *
      *
      *    JCL JOB:
      *      RTPOT44.FINAL.JCL(AUTOPART)
      ****************************************************************
      *  CHANGE LOG: *
      ****************
      *      UPDATED BY:  DORETHA RILEY
      *            DATE:  09/16/2020
      *     DESCRIPTION:  UPDATED EXTERNAL FILE NAMES IN SELECT
      *                   STATEMENTS IN ENVIRONMENT DIVISION
      *
      *      CREATED BY:  DORETHA RILEY
      *     DESCRIPTION:  ORIGINAL CREATION OF PROGRAM
      *            DATE:  09/15/2020
      ****************************************************************

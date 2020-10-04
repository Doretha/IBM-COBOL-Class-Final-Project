       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUTOPART.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 09/15/20.
       DATE-COMPILED. 09/16/20.
       SECURITY. NON-CONFIDENTIAL.
      *
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

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARTSIN ASSIGN TO PARTSIN
              FILE STATUS IS PICODE.
      *
           SELECT STATEZIP ASSIGN TO STATEZIP
              FILE STATUS IS SZCODE.
      *
           SELECT PARTFILE ASSIGN TO PARTFILE
              FILE STATUS IS PACODE.
      *
           SELECT SUPPLIER ASSIGN TO SUPPLIER
              FILE STATUS IS SUCODE.
      *
           SELECT SUPPADDR ASSIGN TO SUPPADDR
              FILE STATUS IS SACODE.
      *
           SELECT PURCHORD ASSIGN TO PURCHORD
              FILE STATUS IS POCODE.
      *
           SELECT PARTSOUT ASSIGN TO PARTSOUT
               FILE STATUS IS PTCODE.
      *
           SELECT ERRFILE ASSIGN TO ERRFILE
               FILE STATUS IS ERCODE.
      *
           SELECT PARTSRPT ASSIGN TO PARTSRPT
               FILE STATUS IS PRCODE.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  PARTSIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTSIN-REC.
      *
       01 PARTSIN-REC PIC X(473).
      *
       FD  STATEZIP
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 34 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS STATEZIP-REC.
      *
       01 STATEZIP-REC PIC X(34).
      *
       FD  PARTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 92 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTS-REC.
      *
       01 PARTS-REC PIC X(92).
      *
       FD  SUPPLIER
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 40 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SUPPLIER-REC.
      *
       01 SUPPLIER-REC PIC X(40).
      *
       FD  SUPPADDR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS SUPP-ADDR-REC.
      *
       01 SUPP-ADDR-REC PIC X(80).
      *
       FD  PURCHORD
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 45 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PURCH-ORD-REC.
      *
       01 PURCH-ORD-REC PIC X(45).
      *
       FD  PARTSOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTSOUT-REC.
      *
       01 PARTSOUT-REC PIC X(473).
      *
       FD  PARTSRPT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTS-RPT-REC.
      *
       01 PARTS-RPT-REC PIC X(100).
      *
       FD  ERRFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 623 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERROR-REC.
      *
       01 ERROR-REC PIC X(623).
      *
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *  WORKING STORAGE COPYLIB MEMBER FOR PARTS INPUT FILE
      *****************************************************************
      *
           COPY PARTSREC. *> INCLUDE PARTSREC COPYLIB MEMBER
      *
       01 STATEZIP-REC-WS. *>USED TO HOLD READ STATE/ZIP RECORD
          05 STATE-LONG             PIC X(15) VALUE SPACES.
          05 FILLER                 PIC X(01) VALUE SPACES.
          05 STATE-ABBREV           PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 LOW-ZIP                PIC 9(05) VALUE 0.
          05 FILLER                 PIC X(03) VALUE SPACES.
          05 HIGH-ZIP               PIC 9(05) VALUE 0.
      *
       01  STATE-ADDRESS-TBL. *>TABLE TO HOLD STATE/ZIP FILE ENTRIES
           05 STATE-ADDRESS-ZIP-TBL
             OCCURS 72 TIMES INDEXED BY STATE-IDX.
             10 STATE-LONG-TBL      PIC X(15) VALUE SPACES.
             10 FILLER              PIC X(01) VALUE SPACES.
             10 STATE-ABBREV-TBL    PIC X(02) VALUE SPACES.
             10 FILLER              PIC X(02) VALUE SPACES.
             10 LOW-ZIP-TBL         PIC 9(10) VALUE 0.
             10 FILLER              PIC X(03) VALUE SPACES.
             10 HIGH-ZIP-TBL        PIC 9(10) VALUE 0.
      *
       01 WS-100-CHAR-BLANK-LINE    PIC X(100) VALUE SPACES.*>BLANK LINE
      *
       01 ERROR-MSG-AREA. *>PASSED IN LINKAGE TO STORE ERROR MESSAGES
          05 ERROR-COUNTER          PIC 99 VALUE 0.
             88 MAX-ERRORS-MET      VALUE 4.
          05 ERR-MSG-RETURN-CODE    PIC X(02).
             88 DATA-ERROR          VALUE '08'.
             88 VALID-DATA          VALUE '00'.
          05 ERROR-MSG-TABLE OCCURS 4 TIMES INDEXED BY ERROR-IDX.
             10 ERROR-MSG-TEXT      PIC X(50) VALUE SPACES.
      *
       01 WS-ERROR-REC. *>USED TO FORMAT ERROR MESSAGE RECORD
          05 WS-PART-DATA-ERR       PIC X(473).
          05 WS-ERROR-MESSAGES OCCURS 4 TIMES INDEXED BY PARTS-ERR-IDX.
             10 WS-ERROR-MSG-TEXT   PIC X(50) VALUE SPACES.
      *
      *****************************************************************
      *  OUTPUT WORKING STORAGE THAT INCLUDES PARTS FILEDS IN PARTS
      *  GROUP AREA OF AUTOPART INPUT FILE
      *****************************************************************
       01 WS-PARTS-REC              PIC X(78) VALUE SPACES. *>PARTS FILE

       01 PARTS-DATE-HEADER-1. *>HEADER FOR PARTS REPORT
          05 FILLER                  PIC X(05) VALUE SPACES.
          05 FILLER                  PIC X(07) VALUE 'DATE: '.
          05 PARTS-RPT-DATE.
             10 PARTS-RPT-YEAR       PIC X(04) VALUE SPACES.
             10 FILLER               PIC X(01) VALUE '/'.
             10 PARTS-RPT-MONTH      PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(01) VALUE '/'.
             10 PARTS-RPT-DAY        PIC X(02) VALUE SPACES.
          05 FILLER                  PIC X(52) VALUE SPACES.
          05 FILLER                  PIC X(06) VALUE 'PAGE: '.
          05 PARTS-RPT-PAGE-NUM      PIC 9(02) VALUE 0.

       01 PARTS-RPT-HEADER-1. *>HEADER FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(14) VALUE 'PART NAME'.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'WEEKS LEAD TIME'.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(12) VALUE 'VEHICLE MAKE'.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'SUPPLIER NAME'.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'SUPPLIER RATING'.
          05 FILLER                 PIC X(16) VALUE SPACES.
      *
       01 PARTS-RPT-HEADER-2. *>HEADER FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(14) VALUE  ALL '='.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE ALL '='.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(12) VALUE ALL '='.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE ALL '='.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE ALL '='.
          05 FILLER                 PIC X(16) VALUE SPACES.
      *
       01 PARTS-RPT-DETAIL. *>DETAIL LINE FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 PART-NAME-PO-RPT       PIC X(14) VALUE  SPACES.
          05 FILLER                 PIC X(07) VALUE SPACES.
          05 WEEKS-LEAD-TIME-PO     PIC 9(03).
          05 FILLER                 PIC X(09) VALUE SPACES.
          05 VEHICLE-MAKE-PO        PIC X(12) VALUE SPACES.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 SUPPLIER-NAME-PO       PIC X(15) VALUE SPACES.
          05 FILLER                 PIC X(02) VALUE SPACES.
          05 SUPPLIER-RATING-PO     PIC X(15) VALUE SPACES.
      *
       01 PARTS-RPT-ADDR-LINE-1. *>ORDER ADDRESS LINE FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'ORDER ADDRESS: '.
          05 ORDER-ADDRESS-PO       PIC X(76) VALUE SPACES.
          05 FILLER                 PIC X(04) VALUE SPACES.
      *
       01 PARTS-RPT-ADDR-LINE-2.  *>SCHED ADDRESS LINE FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'SCHED ADDRESS: '.
          05 SCHED-ADDRESS-PO       PIC X(76) VALUE SPACES.
          05 FILLER                 PIC X(04) VALUE SPACES.
      *
       01 PARTS-RPT-ADDR-LINE-3.  *>REMIT ADDRESS LINE FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(15) VALUE 'REMIT ADDRESS: '.
          05 REMIT-ADDRESS-PO       PIC X(76) VALUE SPACES.
          05 FILLER                 PIC X(04) VALUE SPACES.
      *
       01 PARTS-RPT-TOTAL-LINE-1. *>TOTAL LINE 1 FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(25) VALUE
                                    'TOTAL # PURCHASE ORDERS: '.
          05 TOTAL-PURCH-ORDS       PIC 9(02).
          05 FILLER                 PIC X(68) VALUE SPACES.
      *
       01 PARTS-RPT-TOTAL-LINE-2. *>TOTAL LINE 2 FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(30) VALUE
                                    'TOTAL PRICE PURCHASE ORDERS:  '.
          05 TOTAL-PURCH-ORDS-PRICE PIC $$,$$$,$$9.99.
          05 FILLER                 PIC X(52) VALUE SPACES.
      *
       01 PARTS-RPT-TOTAL-LINE-3. *>TOTAL LINE 3 FOR PARTS REPORT
          05 FILLER                 PIC X(05) VALUE SPACES.
          05 FILLER                 PIC X(36) VALUE
                                 'TOTAL QUANTITY IN PURCHASE ORDERS:  '.
          05 TOTAL-PURCH-ORDS-QUANT PIC ZZZZZZ9.
          05 FILLER                 PIC X(46) VALUE SPACES.
      *
       01 WS-PURCHASE-ORDER-OUT. *> PURCHASE ORDER OUTPUT FILE LAYOUT
          05 PO-NUMBER-O            PIC X(06) VALUE SPACES.
          05 BUYER-CODE-O           PIC X(03) VALUE SPACES.
          05 QUANTITY-O             PIC S9(7) VALUE +0.
          05 UNIT-PRICE-O           PIC S9(7)V99 VALUE +0.
          05 ORDER-DATE-O           PIC 9(08) VALUE 0.
          05 DELIVERY-DATE-O        PIC 9(08) VALUE 0.

       01 WS-ADDRESS-OUT. *>ADDRESS OUTPUT FILE LAYOUT
          05 ADDRESS-TYPE-O         PIC X(01) VALUE SPACES.
          05 ADDRESS-1-O            PIC X(15) VALUE SPACES.
          05 ADDRESS-2-O            PIC X(15) VALUE SPACES.
          05 ADDRESS-3-O            PIC X(15) VALUE SPACES.
          05 CITY-O                 PIC X(15) VALUE SPACES.
          05 ADDR-STATE-O           PIC X(02) VALUE SPACES.
          05 ZIP-CODE-O             PIC 9(10) VALUE 0.
      *
       01 FILE-STATUS-CODES. *>CODES TO CHECK FILE OPERATIONS
          05 PICODE                 PIC X(02) VALUE SPACES.
          05 SZCODE                 PIC X(02) VALUE SPACES.
          05 PACODE                 PIC X(02) VALUE SPACES.
          05 SUCODE                 PIC X(02) VALUE SPACES.
          05 SACODE                 PIC X(02) VALUE SPACES.
          05 POCODE                 PIC X(02) VALUE SPACES.
          05 PTCODE                 PIC X(02) VALUE SPACES.
          05 PRCODE                 PIC X(02) VALUE SPACES.
          05 ERCODE                 PIC X(02) VALUE SPACES.
      *
       01 SWITCHES-WS. *>SWITCHES TO DETECT END OF INPUT FILES
          05 PARTS-FILE-SW          PIC X(01) VALUE 'N'.
             88 END-OF-PARTS-FILE   VALUE 'Y'.
          05 STATE-ZIP-FILE-SW      PIC X(01) VALUE 'N'.
             88 END-OF-STATE-ZIP-FILE VALUE 'Y'.
      *
       01 WS-ACCUM-VARS. *> COUNTERS FOR TABLE INDEXES
          05 WS-MAX-STATE-IDX       PIC 9(02) VALUE 72.
          05 MAX-ERROR-CTR          PIC 9(02) VALUE 4.
      *
       01 WS-TEMP-VARIABLES. *>VARIABLES FOR PARTS RPT HEADER DATE
          05 WS-HOLD-DATE.
             10 WS-HOLD-YEAR     PIC X(04) VALUE SPACES.
             10 WS-HOLD-MONTH    PIC X(02) VALUE SPACES.
             10 WS-HOLD-DAY      PIC X(02) VALUE SPACES.

       01 WS-TALLY-LENGTH-VARIABLES. *>TALLY & LENGTH VARS FOR ADDRESS
          05 ADDRESS-1-TLY     PIC 9(02) VALUE 0.
          05 ADDRESS-1-LEN     PIC 9(02) VALUE 0.
          05 ADDRESS-2-TLY     PIC 9(02) VALUE 0.
          05 ADDRESS-2-LEN     PIC 9(02) VALUE 0.
          05 ADDRESS-3-TLY     PIC 9(02) VALUE 0.
          05 ADDRESS-3-LEN     PIC 9(02) VALUE 0.
          05 CITY-TLY          PIC 9(02) VALUE 0.
          05 CITY-LEN          PIC 9(02) VALUE 0.
      *
      *****************************************************************
      *  VARIABLE THAT HOLDS THE FORMATTED ADDRESS LINE FOR THE PARTS
      *  REPORT.
      *****************************************************************
      *
       01 WS-HOLD-ADDRESS-FORMAT  PIC X(76) VALUE SPACES.

      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE MAIN PROCEDURE SECTION CALLS PARAGRAPHS TO PERFORM
      *    HOUSEKEEPING, OPEN INPUT/OUTPUT FILES, READ INPUT FILES,
      *    LOAD THE STATE/ZIP TABLE, PROCESS INPUT RECORDS AND CLOSE
      *    FILES ROUTINES.
      *
      *  CALLED BY:
      *    - NONE
      *
      *  CALLS:
      *    - 0000-HOUSEKEEPING
      *    - 0100-OPEN-FILES
      *    - 0200-READ-PARTS-FILE
      *    - 0300-READ-STATE-ZIP-FILE
      *    - 0400-LOAD-STATE-ZIP-TABLE
      *    - 0500-MAIN-PROCESS
      *    - 2600-CLOSE-FILES
      ****************************************************************
      *
       PROCEDURE DIVISION.
      *    DISPLAY 'ENTERING PARA MAIN PROCEDURE AUTOPARTS PGM'.
           PERFORM 0000-HOUSEKEEPING.
           PERFORM 0100-OPEN-FILES.
           PERFORM 0200-READ-PARTS-FILE.
           PERFORM 0300-READ-STATE-ZIP-FILE.
           PERFORM 0400-LOAD-STATE-ZIP-TABLE
              UNTIL END-OF-STATE-ZIP-FILE.
           PERFORM 0500-MAIN-PROCESS
               UNTIL END-OF-PARTS-FILE.
           PERFORM 2600-CLOSE-FILES.
           GOBACK.
      *
      *****************************************************************
      *  DESCRIPTION:
      *   THE 0000-HOUSEKEEPING PARAGRAPH INITIALIZES VARIABLES
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       0000-HOUSEKEEPING.
      *    DISPLAY 'ENTERING PARA 0000-HOUSEKEEPING'.

           INITIALIZE PARTS-IN-REC-WS,
                      WS-ERROR-REC,
                      ERROR-MSG-AREA.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 0100-OPEN-FILES PARAGRAPH OPENS FILES FOR INPUT AND
      *    OUTPUT AND CHECKS THE FILE STATUS FOR A SUCCESSFUL OPEN
      *    OPERATION. IF THE OPEN OPERATION FAILS, AN ERROR MESSAGE
      *    IS DISPLAYED.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       0100-OPEN-FILES.
      *    DISPLAY 'ENTERING PARA 0100-OPEN-FILES'.

           OPEN INPUT PARTSIN.  *>AUTOPARTS INPUT FILE
           IF PICODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING PARTS INPUT FILE'
           END-IF.
      *
           OPEN INPUT STATEZIP. *>STATEZIP INPUT FILE
           IF SZCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING STATE ZIPCODE FILE'
           END-IF.
      *
           OPEN OUTPUT PARTFILE. *>PARTS GROUP AREA DATA OUTPUT FILE
           IF PACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING PARTS FILE'
           END-IF.
      *
           OPEN OUTPUT SUPPLIER. *>SUPPLIER GROUP AREA DATA OUTPUT FILE
           IF SUCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING SUPPLIER FILE'
           END-IF.
      *
           OPEN OUTPUT SUPPADDR. *>ADDRESS GROUP AREA DATA OUTPUT FILE
           IF SACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING SUPPLIER ADDRESS FILE'
           END-IF.
      *
           OPEN OUTPUT PURCHORD. *>PURCHASE ORDER GROUP DATA OUTPUT FILE
           IF POCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING PURCHASE ORDER FILE'
           END-IF.
      *
           OPEN OUTPUT PARTSOUT. *>GOOD PARTS INPUT RECORDS OUTPUT FILE
           IF PTCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING PARTSOUT OUTPUT FILE'
           END-IF.
      *
           OPEN OUTPUT PARTSRPT. *> PARTS DATA REPORT
           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING PARTS REPORT FILE'
           END-IF.
      *
           OPEN OUTPUT ERRFILE. *>ERROR FILE DATA
           IF ERCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR OPENING ERROR FILE'
           END-IF.
      *
      ******************************************************************
      *  DESCRIPTION:
      *    THE 0200-READ-PARTS-FILE PARAGRAPH READS THE AUTO PARTS INPUT
      *    FILE AND CHECKS THE FILE STATUS FOR A SUCCESSFUL READ
      *    OPERATION. IF THE READ OPERATION FAILS, AN ERROR MESSAGE
      *    IS DISPLAYED. WHEN THE END OF FILE IS REACHED, A FLAG IS SET
      *    TO INDICATE THAT STATUS.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  NONE
      ******************************************************************
      *
       0200-READ-PARTS-FILE.
      *    DISPLAY 'ENTERING PARA 0200-READ-PARTS-FILE'.

           READ PARTSIN INTO PARTS-IN-REC-WS
              AT END MOVE 'Y' TO PARTS-FILE-SW
           END-READ.
      *
           IF PICODE = '00' OR '10' *> IF GOOD READ OR END OF FILE
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR READING PARTS INPUT FILE.'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE  0300-READ-STATE-ZIP-FILE PARAGRAPH READS THE STATE/ZIP
      *    CODE FILE INTO WORKING STORAGE AND CHECKS THE FILE STATUS
      *    F0R A SUCCESSFUL READ OPERATION. IF THE READ OPERATION FAILS,
      *    AN ERROR MESSAGE IS DISPLAYED. WHEN THE END OF FILE IS
      *    REACHED, A FLAG IS SET TO INDICATE THAT STATUS.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       0300-READ-STATE-ZIP-FILE.
      *     DISPLAY 'ENTERING PARA 0300-READ-STATE-ZIP-FILE'.

            READ STATEZIP INTO STATEZIP-REC-WS
              AT END MOVE 'Y' TO STATE-ZIP-FILE-SW
            END-READ.

           IF SZCODE = '00' OR '10' *> IF GOOD READ OR END OF FILE
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR READING PARTS INPUT FILE.'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 0400-LOAD-STATE-ZIP-TABLE PARAGRAPH LOADS STATE/ZIP
      *    RECORDS READ FROM THE STATE/ZIP FILE INTO A TABLE (ARRAY)
      *    IN WORKING STORAGE.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *    -  0400-LOAD-STATE-ZIP-TABLE
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *

       0400-LOAD-STATE-ZIP-TABLE.
      *    DISPLAY 'ENTERING PARA 0400-LOAD-STATE-ZIP-TABLE'.
      *****************************************************************
      *  THE PERFORM LOOP MOVES STATE/ZIP CODE FILEDS FROM THE INPUT
      *  FILE TO WORKING STORAGE TABLE (ARRAY) ENTRIES AND GETS (READS)
      *  THE NEXT RECORD FROM THE STATE/ZIP FILE. THE LOOP IS PERFORMED
      *  UNTIL THE END OF THE STATE/ZIP FILE IS REACHED OR THE INDEX
      *  FOR THE TABLE (STATE-IDX) IS > WS-MAX-STATE-IDX (72)
      *****************************************************************
      *
           PERFORM VARYING STATE-IDX FROM 1 BY 1
             UNTIL END-OF-STATE-ZIP-FILE OR STATE-IDX > WS-MAX-STATE-IDX
               MOVE STATE-LONG   TO  STATE-LONG-TBL (STATE-IDX)
               MOVE STATE-ABBREV TO  STATE-ABBREV-TBL (STATE-IDX)
               MOVE LOW-ZIP      TO  LOW-ZIP-TBL(STATE-IDX)
               MOVE HIGH-ZIP     TO  HIGH-ZIP-TBL(STATE-IDX)
      *
               PERFORM 0300-READ-STATE-ZIP-FILE
           END-PERFORM.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 0500-MAIN-PROCESS PARAGRAPH PERFORMS THE MAIN LOGIC TO
      *    PROCESS THE AUTOPART INPUT FILE. THIS PARAGRPAH IS CALLED
      *    REPEATEDLY FROM THE MAIN PROCEDURE AREA UNTIL THE END OF
      *    THE AUTO PARTS INPUT FILE IS REACHED. AN AUTO PARTS RECORD
      *    IS PROCESSED BY CALLING SUBROUTINES TO EDIT SPECIFIED PARTS
      *    OF THE INPUT RECORD. EACH SUBROUTINE IS CALLED WITH ITS
      *    CORRESPONDING GROUP AREA OF THE INPUT RECORD TO BE VALIDATED
      *    AND THE ERROR-MSG-AREA. IF ERRORS ARE FOUND DURING EDIT
      *    VALIDATION IN A SUBROUTINE, THE ERROR IS LOADED INTO THE
      *    ERROR MESSAGE AREA AND THE ERROR MESSAGE COUNTER IS INCRE-
      *    MENTED. UPON RETURN FROM THE SUBROUTINE, A CHECK IS DONE
      *    TO DETERMINE IF THE MAXIMUM ALLOWABLE ERRORS FOR A RECORD
      *    (4 ERRORS) HAS BEEN MET. IF THE MAXIMUM NUMBER OF ERRORS HAS
      *    BEEN MET, NO ADDITIONAL SUBROUTINES ARE CALLED. WHEN
      *    THE RECORD CONTAINS ERRORS, LOGIC IS PERFORMED TO WRITE THE
      *    AUTO PART INPUT RECORD AND ITS CORRESPONDING ERRORS TO THE
      *    ERROR FILE. IF NO ERRORS ARE FOUND IN THE INPUT RECORD, LOGIC
      *    TO WRITE A NUMBER OF OUTPUT FILES IS CALLED.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  0200-READ-PARTS-FILE
      *    -  0550-INITIALIZE-ERROR-MSG-AREA
      *    -  0600-PARTS-DATA-EDITS
      *    -  0700-SUPPLIER-DATA-EDITS
      *    -  0800-SUPP-ADDRESS-EDITS
      *    -  0900-PURCHASE-ORDER-EDITS
      *    -  1000-PROCESS-OUTPUT-FILES
      *    -  1600-PROCESS-ERRORS
      *****************************************************************
      *
       0500-MAIN-PROCESS.
      *    DISPLAY 'ENTERING PARA 0500-MAIN-PROCESS'.

           PERFORM 0550-INITIALIZE-ERROR-MSG-AREA.

           INITIALIZE WS-PURCHASE-ORDER-OUT.
      *
           PERFORM 0600-PARTS-DATA-EDITS.
      *
           IF MAX-ERRORS-MET  *>VALUE OF 4 IN ERROR-COUNTER
              NEXT SENTENCE
           ELSE
              PERFORM 0700-SUPPLIER-DATA-EDITS
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
              PERFORM 0800-SUPP-ADDRESS-EDITS
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
              PERFORM 0900-PURCHASE-ORDER-EDITS
           END-IF.
      *
      *****************************************************************
      *  AFTER CALLING EDIT CHECK SUBROUTINES, CHECK TO DETERMINE IF
      *  ERRORS WERE ENCOUNTERED.  IF ERRORS WERE ENCOUNTERED, PROCESS
      *  THE ERROR FILE, OTHERWISE PROCESS THE OUTPUT FILES
      *****************************************************************
      *
           IF DATA-ERROR   *>RETURN CODE OF '08' IN ERROR-MSG-AREA
              PERFORM 1600-PROCESS-ERRORS
           ELSE
              PERFORM 1000-PROCESS-OUTPUT-FILES
           END-IF.
      *
      *****************************************************************
      *  AT THIS POINT, ALL EDIT CHECKS HAVE BEEN PERFORMED AND FILES
      *  WRITTEN.  READ THE NEXT AUTOPARTS INPUT RECORD FOR PROCESSING
      *****************************************************************
           PERFORM 0200-READ-PARTS-FILE.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 0550-INITIALIZE-ERROR-MSG-AREA PARAGRAPH CLEARS OUT THE
      *    ERROR MESSAGE AREA AFTER ALL EDITS FOR A RECORD HAVE BEEN
      *    PROCESSED. THIS PREPARES THE ERROR-MSG-AREA FOR THE NEXT
      *    INPUT RECORD.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       0550-INITIALIZE-ERROR-MSG-AREA.
      *    DISPLAY 'ENTERING PARA 0550-INITIALIZE-ERROR-MSG-AREA'.
           INITIALIZE WS-ERROR-REC.
      *
           MOVE 0 TO ERROR-COUNTER.
           MOVE SPACES TO ERR-MSG-RETURN-CODE.

           PERFORM VARYING PARTS-ERR-IDX FROM 1 BY 1
               UNTIL PARTS-ERR-IDX > 4 *> 4 IS NUMBER OF ENTRIES IN TBL
                  MOVE SPACES TO WS-ERROR-MSG-TEXT (PARTS-ERR-IDX)
           END-PERFORM.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 0600-PARTS-DATA-EDITS PARAGRAPH CALLS THE PARTSUPP
      *    SUBROUTINE (PROGRAM) WITH THE PARTS GROUP AREA AND THE
      *    ERROR MESSAGE AREA.  THE PARTSUPP SUBROUTINE PERFORMS EDIT
      *    CHECKS ON PARTS GROUP AREA FIELDS.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  PARTSUPP SUBPROGRAM
      *****************************************************************
      *
       0600-PARTS-DATA-EDITS.
      *    DISPLAY 'ENTERING 0600-PARTS-DATA-EDITS'.

           CALL 'PARTSUPP' USING PARTS, ERROR-MSG-AREA.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 0700-PARTS-DATA-EDITS PARAGRAPH CALLS THE SUPPLIER
      *    SUBROUTINE (PROGRAM) WITH THE SUPPLIER GROUP AREA AND THE
      *    ERROR MESSAGE AREA. THE SUPPLIER SUBROUTINE PERFORMS EDIT
      *    CHECKS ON SUPPLIER GROUP AREA FIELDS.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  SUPPLIER SUBPROGRAM
      *****************************************************************
        0700-SUPPLIER-DATA-EDITS.
      *    DISPLAY 'ENTERING 0700-SUPPLIER-DATA-EDITS'.

           CALL 'SUPPLIER' USING SUPPLIERS, ERROR-MSG-AREA.

      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 0800-SUPP-ADDRESS-EDITS PARAGRAPH CALLS THE SUPPADDR
      *    SUBROUTINE (PROGRAM) WITH THE SUPPLIER ADDRESS GROUP
      *    AREA AND THE ERROR MESSAGE AREA. THE SUPPADDR SUBROUTINE
      *    PERFORMS EDIT CHECKS ON SUPPLIER ADDRESS GROUP AREA FIELDS.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  SUPPADDR SUBPROGRAM
      *****************************************************************
      *
        0800-SUPP-ADDRESS-EDITS.
      *    DISPLAY 'ENTERING 0800-SUPP-ADDRESS-EDITS'.

           CALL 'ADDRSUPP' USING ADDRESSES, STATE-ADDRESS-TBL,
                                 ERROR-MSG-AREA.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 0900-PURCHASE-ORDER-EDITS PARAGRAPH CALLS THE PURCHORD
      *    SUBROUTINE (PROGRAM) WITH THE PURCHASE ORDER GROUP
      *    AREA AND THE ERROR MESSAGE AREA. THE PURCHORD SUBROUTINE
      *    PERFORMS EDIT CHECKS ON PURCHASE ORDER GROUP AREA FIELDS.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  PURCHORD SUBPROGRAM
      *****************************************************************
      *
        0900-PURCHASE-ORDER-EDITS.
      *
      *    DISPLAY 'ENTERING 0900-PURCHASE-ORDER-EDITS'.

           CALL 'PURCHORD' USING PURCHASE-ORDER, ERROR-MSG-AREA.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1000-PROCESS-OUTPUT-FILES PARAGRAPH CALLS PARAGRAPHS TO
      *    WRITE THE PARTS,SUPPLIER, SUPPLIER ADDRESS, PURCHASE ORDER
      *    AND AUTOPARTS OUTPUT FILES AND THE PARTS REPORT.
      *
      *  CALLED BY:
      *    -  0500-MAIN-PROCESS
      *
      *  CALLS:
      *    -  PURCHORD SUBPROGRAM
      *    -  1100-WRITE-PARTS-REC
      *    -  1200-WRITE-SUPPLIER-REC
      *    -  1300-WRITE-ADDRESS-REC
      *    -  1400-WRITE-PURCH-ORD-REC
      *    -  1500-WRITE-PARTS-OUT-REC
      *    -  1800-PROCESS-PARTS-REPORT.
      *****************************************************************
      *
       1000-PROCESS-OUTPUT-FILES.
      *    DISPLAY 'ENTERING PARA 1000-PROCESS-OUTPUT-FILES'.

           PERFORM 1100-WRITE-PARTS-REC.  *>PARTS GROUP FIELDS

           PERFORM 1200-WRITE-SUPPLIER-REC. *>SUPPLIER GROUP FIELDS
      *
      *
      *****************************************************************
      *  PERFORM LOOP TO MOVE ADDRESS TABLE FIELD ENTRIES TO OUTPUT
      *  WORKING STORAGE TO WRITE ADDRESS OUTPUT FILE
      *****************************************************************
      *
           PERFORM VARYING ADDR-IDX FROM 1 BY 1
              UNTIL ADDR-IDX > 3
                PERFORM 1250-MOVE-ADDRESS-FIELDS
                PERFORM 1300-WRITE-ADDRESS-REC
           END-PERFORM.
      *
      *
      *****************************************************************
      *  PERFORM LOOP TO MOVE PURCHASE ORDER TABLE FIELD ENTRIES TO
      *  OUTPUT WORKING STORAGE TO WRITE PURCHASE ORDER OUTPUT FILE
      *****************************************************************
      *
           PERFORM VARYING PO-IDX FROM 1 BY 1
              UNTIL PO-IDX > 3
                PERFORM 1350-MOVE-PURCH-ORD-FIELDS
                PERFORM 1400-WRITE-PURCH-ORD-REC
           END-PERFORM.
      *
      *
      *****************************************************************
      *  WRITE GOOD PARTS INPUT RECORDS TO OUTPUT FILE
      *****************************************************************
      *
           PERFORM 1500-WRITE-PARTS-OUT-REC.

           PERFORM 1800-PROCESS-PARTS-REPORT. *>PARTS REPORT
      *
      ****************************************************************
      *  DESCRIPTION:
      *   THE 1100-WRITE-PARTS-REC PARAGRAPH WRITES THE PARTS OUTPUT
      *   RECORD AND CHECKS FOR A SUCCESSFUL WRITE OPERATIOON. IF THE
      *   WRITE OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       1100-WRITE-PARTS-REC.
      *    DISPLAY 'ENTERING PARA 1100-WRITE-PARTS-REC'.

           MOVE PARTS TO WS-PARTS-REC. *> PARTS GROUP AREA WORK. STORAGE

           WRITE PARTS-REC FROM WS-PARTS-REC.
           IF PACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING PARTS FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1200-WRITE-SUPPLIER-REC PARAGRAPH WRITES THE SUPPLIER
      *    OUTPUT RECORD AND CHECKS FOR A SUCCESSFUL WRITE OPERATIOON.
      *    IF THE WRITE OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       1200-WRITE-SUPPLIER-REC.
      *    DISPLAY 'ENTERING PARA 1200-WRITE-SUPPLIER-REC'.

           WRITE SUPPLIER-REC FROM SUPPLIERS.
           IF SUCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING SUPPLIER FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1250-MOVE-ADDRESS-FIELDS PARAGRAPH MOVES ADDRESS FIELDS
      *    TO THE WORKING STORAGE GROUP AREA USED TO WRITE THE ADDRESS
      *    OUTPUT RECORD.
      *
      *  CALLED BY:
      *    -  1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
        1250-MOVE-ADDRESS-FIELDS.
      *    DISPLAY 'ENTERING PARA 1250-MOVE-ADDRESS-FIELDS'.

           MOVE ADDRESS-TYPE (ADDR-IDX) TO ADDRESS-TYPE-O.
           MOVE ADDRESS-1 (ADDR-IDX)    TO ADDRESS-1-O.
           MOVE ADDRESS-2 (ADDR-IDX)    TO ADDRESS-2-O.
           MOVE ADDRESS-3 (ADDR-IDX)    TO ADDRESS-3-O.
           MOVE CITY (ADDR-IDX)         TO CITY-O.
           MOVE ADDR-STATE (ADDR-IDX)   TO ADDR-STATE-O.
           MOVE ZIP-CODE (ADDR-IDX)     TO ZIP-CODE-O.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1300-WRITE-ADDRESS-REC PARAGRAPH WRITES THE ADDRESS
      *    OUTPUT RECORD AND CHECKS FOR A SUCCESSFUL WRITE OPERATIOON.
      *    IF THE WRITE OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       1300-WRITE-ADDRESS-REC.
      *    DISPLAY 'ENTERING PARA 1300-WRITE-ADDRESS-REC'.

           WRITE SUPP-ADDR-REC FROM WS-ADDRESS-OUT.
           IF SACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING SUPPLIER ADDRESS FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1350-MOVE-PURCH-ORD-FIELDS PARAGRAPH MOVES PURCHASE
      *    ORDER FILES TO THE WORKING STORAGE GROUP AREA USED TO
      *    WRITE THE PURCHASE ORDER RECORD.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       1350-MOVE-PURCH-ORD-FIELDS.
      *    DISPLAY 'ENTERING PARA 1350-MOVE-PURCH-ORD-FIELDS'.
           MOVE PO-NUMBER (PO-IDX)     TO PO-NUMBER-O.
           MOVE BUYER-CODE (PO-IDX)    TO BUYER-CODE-O.
           MOVE QUANTITY (PO-IDX)      TO QUANTITY-O.
           MOVE UNIT-PRICE (PO-IDX)    TO UNIT-PRICE-O.
           MOVE ORDER-DATE (PO-IDX)    TO ORDER-DATE-O.
           MOVE DELIVERY-DATE (PO-IDX) TO DELIVERY-DATE-O.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1400-WRITE-PURCH-ORD-REC PARAGRAPH WRITES THE PURCHASE
      *    ORDER RECORD AND CHECKS FOR A SUCCESSFUL WRITE OPERATION.
      *    IF THE WRITE OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       1400-WRITE-PURCH-ORD-REC.
      *    DISPLAY 'ENTERING 1400-WRITE-PURCH-ORD-REC'.

           WRITE PURCH-ORD-REC FROM WS-PURCHASE-ORDER-OUT.
           IF POCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING PURCHASE ORDER FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1500-WRITE-PARTS-OUT-REC PARAGRAPH WRITES GOOD AUTOPARTS
      *    INPUT RECORDS TO AN OUTPUT FILE AND CHECKS FOR A SUCCESSFUL
      *    WRITE OPERATION. IF THE WRITE OPERATION FAILS, AN ERROR
      *    MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       1500-WRITE-PARTS-OUT-REC.
      *    DISPLAY 'ENTERING 1500-WRITE-PARTS-OUT-REC'.

           WRITE PARTSOUT-REC FROM PARTSIN-REC.
           IF PTCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING PARTS OUT FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1600-PROCESS-ERRORS PARAGRAPH CHECKS THE ERROR MESSAGE
      *    AREA PASSED BACK FROM FIELD VALIDATION SUBROUTINES. IF THERE
      *    ARE MORE THAN 3 ERRORS, AN INVALID RECORD IS MOVED TO ENTRY
      *    1 OF THE ERROR FILE ERROR MESSAGE AREA AND THE REMAINING 3
      *    ENTRIES IN THE ERROR MESSAGE AREA OF THE ERROR RECORD ARE
      *    SET TO SPACES. IF THERE ARE 3 OR LESS ERRORS, A LOOP IS
      *    PERFORMED TO MOVE UP TO 3 ERRORS TO THE END OF THE ERROR
      *    RECORD. A PARAGRAPH IS THEN CALLED TO WRITE THE ERROR RECORD.
      *
      *  CALLED BY:
      *    -  1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -  1700-WRITE-ERROR-FILE
      *****************************************************************
      *
        1600-PROCESS-ERRORS.
      *    DISPLAY 'ENTERING 1600-PROCESS-ERRORS'.

      *****************************************************************
      *  MOVE THE AUTO PARTS INPUT RECORD WORKING STORAGE VARIABLE
      * (PARTS-IN-REC-WS) INTO THE WORKING STORAGE AREA
      *  FOR THE ERROR OUTPUT RECORD.
      *****************************************************************
           MOVE PARTS-IN-REC-WS TO WS-PART-DATA-ERR. *> AUTOPARTS REC.
      *
      *****************************************************************
      *  IF A RECORD HAS 4 ERRORS, MOVE AN INVALID RECORD MESSAGE
      *  TO THE END OF THE ERROR FILE RECORD ERROR MESSAGE TABLE
      *  (ENTRY 1) AND MOVE SPACES TO THE REMAINING 3 ENTRIES IN THE
      *  ERROR FILE ERROR MESSAGE TABLE.
      *****************************************************************
      *
           IF MAX-ERRORS-MET *> 4 ERRORS ENCOUNTERED
              MOVE 'INVALID RECORD. 4 OR MORE ERRORS EXIST.' TO
                   WS-ERROR-MSG-TEXT(1)
              MOVE SPACES TO  WS-ERROR-MSG-TEXT(2)
              MOVE SPACES TO  WS-ERROR-MSG-TEXT(3)
              MOVE SPACES TO  WS-ERROR-MSG-TEXT(4)
           ELSE
      *
      *****************************************************************
      *  IF AN AUTO PARTS INPUT RECORD HAS 3 OR LESS DATA FIELD ERRORS,
      *  PERFORM A LOOP BASED ON THE NUMBER OF ERRORS (ERROR-IDX)
      *  IN THE ERROR MESSAGE TABLE (ARRAY) PASSED BACK FROM THE EDIT
      *  SUBROUTINES AND MOVE THE ERRORS TO THE ERROR MESSAGE TABLE
      *  (ARRAY) AT THE END OF THE ERROR FILE RECORD.  PARTS-ERR-IDX
      *  IS THE INDEX FOR THE ERROR MESSAGE TABLE AT THE END OF THE
      *  ERROR FILE RECORD.  ERROR-IDX IS THE INDEX FOR THE ERROR
      *  MESSAGE TABLE PASSED BACK FROM THE FIELD EDIT SUBROUTINES.
      *****************************************************************
      *
              PERFORM VARYING ERROR-IDX FROM 1 BY 1
                 UNTIL ERROR-IDX > 4 OR
                       ERROR-MSG-TEXT (ERROR-IDX) = SPACES
                 SET PARTS-ERR-IDX TO ERROR-IDX
                 MOVE ERROR-MSG-TEXT (ERROR-IDX) TO
                      WS-ERROR-MSG-TEXT (PARTS-ERR-IDX)
              END-PERFORM
           END-IF.
      *
           PERFORM 1700-WRITE-ERROR-FILE.
      *
      ******************************************************************
      *  DESCRIPTION:
      *    THE 1700-WRITE-ERROR-FILE PARAGRAPH WRITES THE ERROR RECORD
      *    TO AN ERROR FILE AND CHECKS FOR A SUCCESSFUL WRITE OPERATION.
      *    IF THE WRITE OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *    AFTER THE ERROR RECORD IS WRITTEN TO THE ERROR FILE, THE
      *    ERROR MESSAGE TABLE THAT IS PASSED BETWEEN FIELD EDIT SUB-
      *    PROGRAMS IS INITIALIZED (CLEARED OUT) TO PREPARE IT FOR USE
      *    WITH THE NEXT AUTO PARTS INPUT FILE RECORD TO BE READ.
      *
      *  CALLED BY:
      *    -   1600-PROCESS-ERRORS
      *
      *  CALLS:
      *    -   NONE
      ******************************************************************
      *
        1700-WRITE-ERROR-FILE.
      *    DISPLAY 'ENTERING PARA 1700-WRITE-ERROR-FILE'.

           WRITE ERROR-REC FROM WS-ERROR-REC.

           INITIALIZE WS-ERROR-REC, ERROR-MSG-AREA.

           IF ERCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING TO ERROR FILE'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1800-PROCESS-PARTS-REPORT PARAGRAPH CALLS PARAGRAPHS TO
      *    CREATE THE AUTO PARTS REPORT. PARAGRAPHS ARE CALLED TO
      *    WRITE REPORT HEADERS, MOVE FIELDS TO AND WRITE THE DETAIL
      *    LINE, MOVE FIELDS TO AND WRITE TO THE ADDRESS LINES, AND
      *    TO CALCULATE AND MOVE FIELDS TO AND WRITE REPORT TOTALS.
      *
      *  CALLED BY:
      *    -   1000-PROCESS-OUTPUT-FILES
      *
      *  CALLS:
      *    -  1900-WRITE-PARTS-RPT-HDRS
      *    -  2000-MOVE-PARTS-DETAIL-FIELDS.
      *    -  2100-WRITE-PARTS-RPT-DETAIL
      *    -  2200-MOVE-PARTS-ADDRESS-FIELDS
      *    -  2300-WRITE-PARTS-RPT-ADDRESSES
      *    -  2400-CALC-PARTS-RPT-TOTALS
      *    -  2500-WRITE-PARTS-RPT-TOTALS.
      *****************************************************************
      *
       1800-PROCESS-PARTS-REPORT.
      *    DISPLAY 'ENTERING PARA 1800-PROCESS-PARTS-REPORT'.
           PERFORM 1900-WRITE-PARTS-RPT-HDRS.
           PERFORM 2000-MOVE-PARTS-DETAIL-FIELDS.
           PERFORM 2100-WRITE-PARTS-RPT-DETAIL.
           PERFORM 2200-MOVE-PARTS-ADDRESS-FIELDS.
           PERFORM 2300-WRITE-PARTS-RPT-ADDRESSES.
           PERFORM 2400-CALC-PARTS-RPT-TOTALS.
           PERFORM 2500-WRITE-PARTS-RPT-TOTALS.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 1900-WRITE-PARTS-RPT-HDRS PARAGRAPH MOVES THE DATE AND
      *    PAGE NUMBER TO A HEADER, WRITES HEADERS TO THE AUTO PARTS
      *    REPORT AND CHECKS THE FILE STATUS FOR A SUCCESSFUL WRITE
      *    OEPRATION. IF THE WRITE OPERATION FAILS, AN ERROR MESSAGE IS
      *    DISPLAYED.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       1900-WRITE-PARTS-RPT-HDRS.
      *    DISPLAY 'ENTERING PARA 1900-WRITE-PARTS-RPT-HDRS'.

      *
      *****************************************************************
      *  MOVE THE CURRENT DATE AND PAGE NUMBER TO THE PARTS RPT HEADER
      *****************************************************************
      *
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-HOLD-DATE.
           MOVE WS-HOLD-YEAR  TO PARTS-RPT-YEAR.
           MOVE WS-HOLD-MONTH TO PARTS-RPT-MONTH.
           MOVE WS-HOLD-DAY   TO PARTS-RPT-DAY.
           ADD 1              TO PARTS-RPT-PAGE-NUM. *>PAGE COUNTER
      *
           WRITE PARTS-RPT-REC FROM PARTS-DATE-HEADER-1.
           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE. *>BLANK LINE
           WRITE PARTS-RPT-REC FROM PARTS-RPT-HEADER-1.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-HEADER-2.

           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING TO PARTS REPORT'
           END-IF.
      *
      ****************************************************************
      *  DESCRIPTION:
      *    THE 2000-MOVE-PARTS-DETAIL-FIELDS PARAGRAPH MOVES FIELDS TO
      *    WORKING STORAGE VARIABLES USED TO WRITE THE DETAIL LINE FOR
      *    THE AUTO PARTS REPORT. EVALUATE CONSTRUCTS ARE USED TO
      *    CHECK 88 LEVELS TO MOVE EXPANDED VEHICLE-MAKE NAMES AND
      *    EXPANDED SUPPLIER RATINGS TO THE DETAIL LINE.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       2000-MOVE-PARTS-DETAIL-FIELDS.
      *    DISPLAY 'ENTERING PARA 2000-MOVE-PARTS-DETAIL-FIELDS'.

           MOVE PART-NAME          TO PART-NAME-PO-RPT.
           MOVE WEEKS-LEAD-TIME    TO WEEKS-LEAD-TIME-PO.
      *
      *****************************************************************
      *  CHECK VEHICLE MAKE 88 LEVELS TO MOVE EXPANDED VEHICLE MAKE
      *  NAMES TO THE AUTO PARTS REPORT DETAIL LINE
      *****************************************************************
      *
           EVALUATE TRUE
              WHEN CHRYSLER
                 MOVE 'CHRYSLER'   TO VEHICLE-MAKE-PO
              WHEN FORD
                 MOVE 'FORD'       TO VEHICLE-MAKE-PO
              WHEN GM
                 MOVE 'GM'         TO VEHICLE-MAKE-PO
              WHEN VOLKSWAGON
                 MOVE 'VOLKSWAGON' TO VEHICLE-MAKE-PO
              WHEN TOYOTA
                 MOVE 'TOYOTA'     TO VEHICLE-MAKE-PO
              WHEN JAGUAR
                 MOVE 'JAGUAR'     TO VEHICLE-MAKE-PO
              WHEN PEUGEOT
                 MOVE 'PEUGEOT'    TO VEHICLE-MAKE-PO
              WHEN BMW
                 MOVE 'BMW'        TO VEHICLE-MAKE-PO
           END-EVALUATE.

           MOVE SUPPLIER-NAME      TO SUPPLIER-NAME-PO.
      *
      *****************************************************************
      *  CHECK SUPPLIER RATING 88 LEVELS TO MOVE EXPANDED SUPPLIER
      *  RATING NAMES TO THE AUTO PARTS REPORT DETAIL LINE
      *****************************************************************
      *
           EVALUATE TRUE
              WHEN HIGHEST-QUALITY
                 MOVE 'HIGHEST QUALITY' TO SUPPLIER-RATING-PO
              WHEN AVERAGE-QUALITY
                 MOVE 'AVERAGE QUALITY' TO SUPPLIER-RATING-PO
              WHEN LOWEST-QUALITY
                 MOVE 'LOWEST QUALITY' TO SUPPLIER-RATING-PO
           END-EVALUATE.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2100-WRITE-PARTS-RPT-DETAIL PARAGRAPH WRITES THE DETAIL
      *    LINE TO THE AUTO PARTS REPORT AND CHECKS FOR A SUCCESSFUL
      *    WRITE OPERATION.  IF THE WRITE OPERATION FAILS, AN ERROR
      *    MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       2100-WRITE-PARTS-RPT-DETAIL.
      *    DISPLAY 'ENTERING PARA 2100-WRITE-PARTS-RPT-DETAIL'.

           WRITE PARTS-RPT-REC FROM PARTS-RPT-DETAIL.
           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING TO PARTS REPORT'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2200-MOVE-PARTS-ADDRESS-FIELDS PARAGRAPH MOVES FIELDS
      *    TO THE AUTO PARTS REPORT ADDRESS LINES. A LOOP IS PERFORMED
      *    TO MOVE ADDRESS, CITY, STATE AND ZIP CODE FOR THE 3 TYPES
      *    OF ADDRESSES IN THE ADDRESS TABLE (ORDER, SCHED, REMIT).
      *    A PARAGRAPH IS CALLED TO EXECUTE A ROUTINE TO REMOVE EXTRA
      *    SPACES FROM FIELDS IN THE ADDRESS LINE.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  2250-FORMAT-ADDRESS-LINE
      *****************************************************************
      *
       2200-MOVE-PARTS-ADDRESS-FIELDS.
      *    DISPLAY 'ENTERING PARA 2200-MOVE-PARTS-ADDRESS-FIELDS'.

           PERFORM VARYING ADDR-IDX FROM 1 BY 1 UNTIL ADDR-IDX > 3

              INITIALIZE WS-HOLD-ADDRESS-FORMAT *>HOLDS ADDRESS LINE
      *
      ***************************************************************
      *  THE ORDER-ADDRESS, SCHED-ADDRESS AND REMIT ADDRESS ARE
      *  PROCESSED TO REMOVE SPACES BETWEEN ADDRESS FIELDS BEFORE
      *  THE FORMATTED ADDRESS LINE IS MOVED TO THE ADDRESS OUTPUT
      *  WORKING STORAGE AREAS - ORDER-ADDRESS-PO, SCHED-ADDRESS-PO
      *  REMIT-ADDRESS-PO.
      ****************************************************************
      *
              IF ORDER-ADDRESS (ADDR-IDX)
                 PERFORM 2250-FORMAT-ADDRESS-LINE
                 MOVE WS-HOLD-ADDRESS-FORMAT TO ORDER-ADDRESS-PO
              ELSE
              IF SCHED-ADDRESS (ADDR-IDX)
                 PERFORM 2250-FORMAT-ADDRESS-LINE
                 MOVE WS-HOLD-ADDRESS-FORMAT TO SCHED-ADDRESS-PO
              ELSE
              IF REMIT-ADDRESS (ADDR-IDX)
                 PERFORM 2250-FORMAT-ADDRESS-LINE
                 MOVE WS-HOLD-ADDRESS-FORMAT TO REMIT-ADDRESS-PO
              END-IF
              END-IF
              END-IF
           END-PERFORM.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2250-FORMAT-ADDRESS-LINE PARAGRAPH REMOVES SPACES FROM
      *    ADDRESS FIELDS BY USING THE REVERSE FUNCTION AND COUNTING
      *    THE NUMBER OF LEADING SPACES FOR EACH FIELD, WHICH IS STORED
      *    IN A TALLY FIELD.  THE TALLY FIELD IS THEN SUBTRACTED FROM
      *    THE LENGTH OF THE FIELD TO DETERMINE THE NUMBER OF CHARACTERS
      *    IN THE FIELD. THE ACTUAL LENGTH OF THE FIELD IS USED AS
      *    A REFERENCE MOD TO CONCATENATE FIELDS USING THE STRING
      *    FUNCTION. THIS ALGORITHM IS ONLY PERFORMED ON FEILDS WHEN
      *    THE FIELD DOES NOT CONTAIN ALL SPACES.
      *
      *  CALLED BY:
      *    -  2200-MOVE-PARTS-ADDRESS-FIELDS
      *
      *  CALLS:
      *    -  2260-FORMAT-ADDRESS-1
      *    -  2270-FORMAT-ADDRESS-1-2
      *    -  2280-FORMAT-ADDRESS-1-2-3
      *    -  2290-FORMAT-ADDRESS-1-3
      *****************************************************************
      *
       2250-FORMAT-ADDRESS-LINE.
      *    DISPLAY 'ENTERING PARA 2250-FORMAT-ADDRESS-LINE'.

      *****************************************************************
      *  INITIALIZE THE FIELDS TO BE USED IN CALCULATING THE NUMBER OF
      *  SPACES AND NUMBER OF CHARACTERS IN ADDDRESS FIELDS
      *****************************************************************
      *
           INITIALIZE ADDRESS-1-TLY, ADDRESS-1-LEN, ADDRESS-2-TLY,
                      ADDRESS-2-LEN, ADDRESS-3-TLY, ADDRESS-3-LEN,
                      CITY-TLY, CITY-LEN.
      *
      ****************************************************************
      *  REVERSE THE ADDRESS-1 FIELD AND USE THE INSPECT TALLING
      *  FUNCTION TO COUNT LEADING SPACES
      *****************************************************************
      *
           INSPECT FUNCTION REVERSE (ADDRESS-1(ADDR-IDX)) TALLYING
                ADDRESS-1-TLY FOR LEADING SPACES.
      *
      ****************************************************************
      *  CALCULATE THE NUMBER OF CHARACTERS IN THE FIELD BY USING
      *  THE LENGTH FUNCTION TO OBTAIN THE SIZE OF THE FIELD AND
      *  SUBTRACTING THE NUMBER OF SPACES TALLIED FROM THE FIELD
      *****************************************************************
      *
           COMPUTE ADDRESS-1-LEN = FUNCTION
                LENGTH (ADDRESS-1(ADDR-IDX)) - ADDRESS-1-TLY.
      *
      ****************************************************************
      *  REPEAT THE CALCULATIONS TO DETERMINE THE NUMBER OF SPACES AND
      *  THE NUMBER OF CHARACTERS FOR ADDRESS-2 AND ADDRESS-3 ONLY
      *  IF THESE FIELDS DO NOT CONTAIN ALL SPACES.
      *****************************************************************
      *
           IF ADDRESS-2(ADDR-IDX) = SPACES
              NEXT SENTENCE
           ELSE
              INSPECT FUNCTION REVERSE (ADDRESS-2(ADDR-IDX)) TALLYING
                   ADDRESS-2-TLY FOR LEADING SPACES
              COMPUTE ADDRESS-2-LEN = FUNCTION
                   LENGTH (ADDRESS-2(ADDR-IDX)) - ADDRESS-2-TLY
           END-IF.

           IF ADDRESS-3(ADDR-IDX) = SPACES
              NEXT SENTENCE
           ELSE
              INSPECT FUNCTION REVERSE (ADDRESS-3(ADDR-IDX)) TALLYING
                   ADDRESS-3-TLY FOR LEADING SPACES
              COMPUTE ADDRESS-3-LEN = FUNCTION
                   LENGTH (ADDRESS-3(ADDR-IDX)) - ADDRESS-3-TLY
           END-IF.

      *
      ****************************************************************
      *  PERFORM THE ROUTINE TO REMOVE SPACES FROM THE CITY FIELD AND
      *  DETERMINE THE CHARACTERS IN THE FIELD.
      *****************************************************************
      *
           INSPECT FUNCTION REVERSE (CITY(ADDR-IDX)) TALLYING CITY-TLY
                   FOR LEADING SPACES.
           COMPUTE CITY-LEN = FUNCTION LENGTH (CITY(ADDR-IDX)) -
                   CITY-TLY.
      *
      ****************************************************************
      *  IF ADDRESS-2 AND ADDRESS-3 EQUAL SPACES, PERFORM THE ROUTINE
      *  TO FORMAT THE ADDRESS LINE WITH ADDRESS-1 ONLY
      *****************************************************************
      *
           IF ADDRESS-2 (ADDR-IDX) =  SPACES   AND
              ADDRESS-3 (ADDR-IDX)  = SPACES
                 PERFORM 2260-FORMAT-ADDRESS-1
           END-IF.
      *
      ****************************************************************
      *  IF ADDRESS-2 NOT EQUAL SPACES AND ADDRESS-3 EQUAL SPACES,
      *  PERFORM THE ROUTINE TO FORMAT THE ADDRESS LINE WITH ADDRESS-1
      *  AND ADDRESS-2.
      *****************************************************************
      *
           IF ADDRESS-2 (ADDR-IDX) NOT EQUAL SPACES AND
              ADDRESS-3 (ADDR-IDX) = SPACES
                 PERFORM 2270-FORMAT-ADDRESS-1-2
           END-IF.
      *
      ****************************************************************
      *  IF ADDRESS-2 AND ADDRESS-3 ARE NOT EQUAL SPACES, PERFORM THE
      *  ROUTINE TO FORMAT THE ADDRESS LINE WITH ADDRESS-1, ADDRESS-2
      *  AND ADDRESS-3.
      *****************************************************************
      *
           IF ADDRESS-2 (ADDR-IDX) NOT EQUAL SPACES AND
              ADDRESS-3 (ADDR-IDX) NOT EQUAL SPACES
                 PERFORM 2280-FORMAT-ADDRESS-1-2-3
           END-IF.

     *
      ****************************************************************
      *  IF ADDRESS-2 EQUALS SPACES AND ADDRESS-3 IS NOT EQUAL SPACES
      *  PERFORM THE ROUTINE TO FORMAT THE ADDRESS LINE WITH ADDRESS-1
      *  AND ADDRESS-3.
      *****************************************************************
      *
           IF ADDRESS-2 (ADDR-IDX) EQUAL SPACES AND
              ADDRESS-3 (ADDR-IDX) NOT EQUAL SPACES
                 PERFORM 2290-FORMAT-ADDRESS-1-3
           END-IF.

      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2260-FORMAT-ADDRESS-1 PARAGRAPH USES THE STRING FUNCTION
      *    TO CONCATENATE THE ADDRESS-1 FIELD WITH THE THE CITY, STATE,
      *    AND ZIP CODE FOR AN ADDRESS TABLE ENTRY
      *
      *  CALLED BY:
      *    -   2250-FORMAT-ADDRESS-LINE
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       2260-FORMAT-ADDRESS-1.
      *    DISPLAY 'ENTERING PARA PERFORM 2260-FORMAT-ADDRESS-1'.

           INITIALIZE WS-HOLD-ADDRESS-FORMAT. *>ADDRESS LINE
      *
      *****************************************************************
      *  STRING TOGETHER ADDRESS-1, CITY, STATE AND ZIP USING THE
      *  CALCULATED LENGTH FIELD FOR REFERENCE MODIFICATION WHEN
      *  REQUIRED. INSERT COMMAS AND SPACES BETWEEN FIELDS.
      *****************************************************************
      *
           STRING ADDRESS-1 (ADDR-IDX)(1:ADDRESS-1-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  CITY (ADDR-IDX)(1:CITY-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDR-STATE (ADDR-IDX) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  ZIP-CODE (ADDR-IDX) (6:5)
                                        DELIMITED BY SIZE
                     INTO WS-HOLD-ADDRESS-FORMAT
           END-STRING.

      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2270-FORMAT-ADDRESS-1-2 PARAGRAPH USES THE STRING
      *    FUNCTION TO CONCATENATE THE ADDRESS-1 AND ADDRESS-2
      *    FIELDS WITH THE THE CITY, STATE AND ZIP CODE FOR THE ADDRESS
      *    TABLE ENTRY.
      *
      *  CALLED BY:
      *    -   2250-FORMAT-ADDRESS-LINE
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       2270-FORMAT-ADDRESS-1-2.
      *    DISPLAY 'ENTERING PARA PERFORM 2270-FORMAT-ADDRESS-1-2'.

           INITIALIZE WS-HOLD-ADDRESS-FORMAT.
      *
      *****************************************************************
      *  STRING TOGETHER ADDRESS-1 AND ADDRESS-2, CITY, STATE AND ZIP
      *  USING THE CALCULATED LENGTH FIELD FOR REFERENCE MODIFICATION
      *  WHEN REQUIRED. INSERT COMMAS AND SPACES BETWEEN FIELDS.
      *****************************************************************
      *
           STRING ADDRESS-1 (ADDR-IDX)(1:ADDRESS-1-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDRESS-2 (ADDR-IDX)(1:ADDRESS-2-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  CITY (ADDR-IDX)(1:CITY-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDR-STATE (ADDR-IDX) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  ZIP-CODE (ADDR-IDX) (6:5)
                                        DELIMITED BY SIZE
                     INTO WS-HOLD-ADDRESS-FORMAT
           END-STRING.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2280-FORMAT-ADDRESS-1-2-3 PARAGRAPH USES THE STRING
      *    FUNCTION TO CONCATENATE THE ADDRESS-1, ADDRESS-2 AND
      *    ADDRESS-3 FIELDS WITH THE THE CITY, STATE, AND ZIP CODE FOR
      *    THE ADDRESS TABLE ENTRY.
      *
      *  CALLED BY:
      *    -   2250-FORMAT-ADDRESS-LINE
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       2280-FORMAT-ADDRESS-1-2-3.
      *    DISPLAY 'ENTERING PARA PERFORM 2280-FORMAT-ADDRESS-1-2-3'.

           INITIALIZE WS-HOLD-ADDRESS-FORMAT.
      *
      *****************************************************************
      *  STRING TOGETHER ADDRESS-1, ADDRESS-2 AND ADDRESS-3, AND CITY,
      *  STATE AND ZIP-CODE USING THE CALCULATED LENGTH FIELD FOR
      *  REFERENCE MODIFICATION  WHEN REQUIRED. INSERT COMMAS AND SPACES
      *  BETWEEN FIELDS.
      *****************************************************************
      *
           STRING ADDRESS-1 (ADDR-IDX)(1:ADDRESS-1-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDRESS-2 (ADDR-IDX)(1:ADDRESS-2-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDRESS-3 (ADDR-IDX)(1:ADDRESS-3-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  CITY (ADDR-IDX)(1:CITY-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDR-STATE (ADDR-IDX) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  ZIP-CODE (ADDR-IDX) (6:5)
                                       DELIMITED BY SIZE
                     INTO WS-HOLD-ADDRESS-FORMAT
           END-STRING.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2290-FORMAT-ADDRESS-1-3 PARAGRAPH USES THE STRING
      *    FUNCTION TO CONCATENATE THE ADDRESS-1 AND ADDRESS-3
      *    FIELDS WITH THE THE CITY, STATE, AND ZIP CODE FOR THE ADDRESS
      *    TABLE ENTRY.
      *
      *  CALLED BY:
      *    -   2250-FORMAT-ADDRESS-LINE
      *
      *  CALLS:
      *    -   NONE
      *****************************************************************
      *
       2290-FORMAT-ADDRESS-1-3.
      *    DISPLAY 'ENTERING PARA PERFORM 2290-FORMAT-ADDRESS-1-3'.

           INITIALIZE WS-HOLD-ADDRESS-FORMAT.
      *
      *****************************************************************
      *  STRING TOGETHER ADDRESS-1 AND ADDRESS-3, CITY, STATE AND
      *  ZIP-CODE USING THE CALCULATED LENGTH FIELD FOR REFERENCE
      *  MODIFICATION  WHEN REQUIRED. INSERT COMMAS AND SPACES BETWEEN
      *  FIELDS.
      *****************************************************************
      *
           STRING ADDRESS-1 (ADDR-IDX)(1:ADDRESS-1-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDRESS-3 (ADDR-IDX)(1:ADDRESS-3-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  CITY (ADDR-IDX)(1:CITY-LEN)
                                        DELIMITED BY SIZE
                  ', '                  DELIMITED BY SIZE
                  ADDR-STATE (ADDR-IDX) DELIMITED BY SIZE
                  ' '                   DELIMITED BY SIZE
                  ZIP-CODE (ADDR-IDX) (6:5)
                                        DELIMITED BY SIZE
                     INTO WS-HOLD-ADDRESS-FORMAT
           END-STRING.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2300-WRITE-PARTS-RPT-ADDRESSES PARAGRAPH WRITES THE
      *    ADDRESS LINES TO THE AUTO PARTS REPORT AND CHECKS FOR A
      *    SUCCESSFUL WRITE OPERATION. IF THE WRITE OPERATION FAILS,
      *    AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       2300-WRITE-PARTS-RPT-ADDRESSES.
      *    DISPLAY 'ENTERING PARA 2300-WRITE-PARTS-RPT-ADDRESSES'.

           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-ADDR-LINE-1.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-ADDR-LINE-2.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-ADDR-LINE-3.

           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING TO PARTS REPORT'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2400-CALC-PARTS-RPT-TOTALS PARAGRAPH CALCULATES THE
      *    THE TOTAL OF ALL QUANTITIES AND UNIT-PRICES ON MULTIPLE
      *    PURCHASE ORDERS FOR A PART NUMBER.  THE MAXIUM NUMBER OF
      *    PURCHASE ORDERS FOR A PART IS 3.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       2400-CALC-PARTS-RPT-TOTALS.
      *    DISPLAY 'ENTERING PARA 2400-CALC-PARTS-RPT-TOTALS'.

           MOVE 3 TO TOTAL-PURCH-ORDS. *>NUMBER OF PO'S ALWAYS 3
      *
      *****************************************************************
      *  USE THE SUM FUNCTION AND THE 'ALL' TABLE SUBSCRIPT PARAMETER
      *  ON THE PURCHASE ORDER TABLE TO SUM ALL OF THE UNIT PRICES AND
      *  QUANTITIES ON PURCHASE ORDERS FOR AN AUTO PART.
      *****************************************************************
      *
           COMPUTE TOTAL-PURCH-ORDS-PRICE =
                   FUNCTION SUM(UNIT-PRICE(ALL)).

           COMPUTE TOTAL-PURCH-ORDS-QUANT =
                   FUNCTION SUM(QUANTITY(ALL)).
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2500-WRITE-PARTS-RPT-TOTALS PARAGRAPH WRITES THE
      *    TOTALS LINES TO THE AUTO PARTS REPORT AND CHECKS FOR A
      *    SUCCESSFUL WRITE OPERATION. IF THE WRITE OPERATION FAILS,
      *    AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -   1800-PROCESS-PARTS-REPORT
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       2500-WRITE-PARTS-RPT-TOTALS.
      *    DISPLAY 'ENTERING PARA 2500-WRITE-PARTS-RPT-TOTALS'.

           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-TOTAL-LINE-1.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-TOTAL-LINE-2.
           WRITE PARTS-RPT-REC FROM PARTS-RPT-TOTAL-LINE-3.
           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE.
           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE.
           WRITE PARTS-RPT-REC FROM WS-100-CHAR-BLANK-LINE.

           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR WRITING TO PARTS FILE'
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE 2600-CLOSE-FILES PARAGRAPH CLOSES FILES AND CHECKS FILE
      *    STATUSES FOR SUCCESSFUL CLOSE OPERATIONS. IF THE CLOSE
      *    OPERATION FAILS, AN ERROR MESSAGE IS DISPLAYED.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  NONE
      *****************************************************************
      *
       2600-CLOSE-FILES.
      *    DISPLAY 'ENTERING PARA 2600-CLOSE-FILES'.

           CLOSE PARTSIN.
           IF PICODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING PARTS INPUT FILE'
           END-IF.
      *
           CLOSE STATEZIP.
           IF SZCODE = '00'
              NEXT SENTENCE
           ELSE
             DISPLAY 'ERROR CLOSING STATE ZIPCODE FILE'
           END-IF.
      *
           CLOSE PARTFILE.
           IF PACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING PARTS FILE'
           END-IF.
      *
           CLOSE SUPPLIER.
           IF SUCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING SUPPLIER FILE'
           END-IF.
      *
           CLOSE SUPPADDR.
           IF SACODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING SUPPLIER ADDRESS FILE'
           END-IF.
      *
           CLOSE PURCHORD.
           IF POCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING PURCHASE ORDER FILE'
           END-IF.
      *
           CLOSE PARTSOUT.
           IF PTCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING PARTSOUT OUTPUT FILE'
           END-IF.
      *
           CLOSE PARTSRPT.
           IF PRCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING PARTS REPORT FILE'
           END-IF.
      *
           CLOSE ERRFILE.
           IF ERCODE = '00'
              NEXT SENTENCE
           ELSE
              DISPLAY 'ERROR CLOSING ERROR FILE'
           END-IF.
      *

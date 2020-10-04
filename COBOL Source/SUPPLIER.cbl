       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPLIER.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 09/15/20.
       DATE-COMPILED. 09/15/20.
       SECURITY. NON-CONFIDENTIAL.
      *
      *****************************************************************
      *  PROGRAM DESCRIPTION:
      *    THIS SUBPROGRAM PERFORMS VALIDATION ON SELECTED FIELDS
      *    IN THE SUPPLIERS GROUP AREA OF THE AUTOPARTS INPUT FILE
      *    RECORD PASSED IN LINKAGE BY THE AUTOPART.CBL PROGRAM. ERROR
      *    MESSAGES ARE GENERATED FOR FAILED FIELD EDITS. THE ERROR
      *    COUNT, RETURN CODE AND UP TO 4 ERROR MESSAGES ARE PASSED
      *    BACK THROUGH LINKAGE TO THE CALLING PROGRAM IN THE ERROR
      *    MESSAGE AREA.
      *****************************************************************
      *
      *  CALLED BY PROGRAM:
      *    - AUTOPARTS.CBL
      *
      *  PROGRAM MODULES CALLED:
      *    - CEEDAYS - IBM DATE VALIDATION PROGRAM
      *
      *****************************************************************  P
      *
      *    INPUT FILES:
      *      -  NONE
      *
      *    OUTPUT FILES:
      *      -  NONE
      *
      *    VARIABLES PASSED IN LINKAGE:
      *      -  SUPPLIERS-LS - GROUP AREA OF AUTO PART INPUT FILE RECORD
      *         THAT CONTAINS SUPPLIER INFORMATION
      *      -  ERROR-MSG-LS - ERROR MESSAGE COUNTER, RETURN CODE AND
      *         ERROR MESSAGE TABLE USED TO TRACK THE NUMBER OF ERRORS
      *         AND ERROR MESSAGES GENERATED IN SUBPROGRAM FIELD
      *         VALIDATION CHECKS.
      *
      *    JCL JOB:
      *       RTPOT44.FINAL.JCL(AUTOPART)
      ****************************************************************
      *  CHANGE LOG: *
      ****************
      *      UPDATED BY:
      *            DATE:
      *     DESCRIPTION:
      *
      *      CREATED BY:  DORETHA RILEY
      *     DESCRIPTION:  ORIGINAL CREATION OF PROGRAM
      *            DATE:  09/15/2020
      ****************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR SUPPLIERS GROUP AREA FIELDS PASSED
      *  THROUGH LINKAGE.
      *****************************************************************
      *
       01 SUPPLIERS.
          05 SUPPLIER-CODE            PIC X(10) VALUE SPACES.
          05 SUPPLIER-TYPE            PIC X(01) VALUE SPACES.
             88 SUBCONTRACTOR         VALUE 'S'.
             88 DISTRIBUTOR           VALUE 'D'.
             88 MANUFACTURER          VALUE 'M'.
             88 IMPORTER              VALUE 'I'.
             88 VALID-SUPPLIER-TYPE   VALUE 'S', 'D', 'M', 'I'.
          05 SUPPLIER-NAME            PIC X(15) VALUE SPACES.
          05 SUPPLIER-PERF            PIC 9(03) VALUE ZERO.
          05 SUPPLIER-RATING          PIC X(01) VALUE SPACES.
             88 HIGHEST-QUALITY       VALUE '3'.
             88 AVERAGE-QUALITY       VALUE '2'.
             88 LOWEST-QUALITY        VALUE '1'.
             88 VALID-SUPPLIER-RATING VALUE '1', '2', '3'.
          05 SUPPLIER-STATUS          PIC X(01) VALUE SPACES.
             88 GOVT-COMM             VALUE '1'.
             88 GOVT-ONLY             VALUE '2'.
             88 COMMERCIAL-ONLY       VALUE '3'.
             88 VALID-SUPPLIER-STATUS VALUE '1', '2', '3'.
          05 SUPPLIER-ACT-DATE        PIC 9(08) VALUE ZERO.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR ERROR MESSAGE AREA PASSED
      *  THROUGH LINKAGE.
      *****************************************************************
      *
       01 ERROR-MSG-AREA.
          05 ERROR-COUNTER            PIC 99 VALUE 0.
             88 MAX-ERRORS-MET        VALUE 4.
          05 ERR-MSG-RETURN-CODE      PIC X(02).
             88 DATA-ERROR            VALUE '08'.
             88 VALID-DATA            VALUE '00'.
          05 ERROR-MSG-TABLE OCCURS 4 TIMES INDEXED BY ERROR-IDX.
             10 ERROR-MSG-TEXT        PIC X(50) VALUE SPACES.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR VARIABLES TO BE PASSED IN LINKAGE TO
      *  IBM DATE VALIDATION SUBPROGRAM CEEDAYS
      *****************************************************************
      *
       01 W-INPUT-DATE-INT        PIC 9(9) COMP.
       01 W-PICSTR-IN.
          10  W-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
          10  W-PICSTR-STR-IN     PIC X(8)  value 'YYYYMMDD'.
       01 W-DATE-IN-CEE.
          10  W-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
          10  W-DATE-IN-STR-CEE   PIC X(8).
       01 FC.
          10  FC-SEV              PIC S9(4) COMP.
          10  FC-MSG              PIC S9(4) COMP.
          10  FC-CTW              PIC X.
          10  FC-FAC              PIC X(3).
          10  FC-ISI              PIC S9(8) COMP.
      *
      *****************************************************************
      *  DECLARATION OF VARIABLE NAMES AND THEIR SIZES PASSED THROUGH
      *  LINKAGE AREA FROM AUTOPARTS.CBL PROGRAM
      *****************************************************************
      *
       LINKAGE SECTION.
       01  SUPPLIERS-LS          PIC X(39).
       01  ERROR-MSG-AREA-LS     PIC X(204).
      *
      *****************************************************************
      *  DESCRIPTION:
      *    THE MAIN PROCEDURE SECTION MOVES LINKAGE AREA VARIABLES TO
      *    LOCAL WORKING STORAGE VARIABLES, CALLS A PARAGRAPH TO PERFORM
      *    EDIT CHECKS ON FIELDS, MOVES ERROR MESSAGES TO THE LOCAL
      *    WORKING STORAGE ERROR MESSAGE AREA WHEN FIELD VALIDATION
      *    ERRORS OCCUR AND MOVES THE LOCAL ERROR MESSAGE AREA TO THE
      *    ERROR MESSAGE AREA IN LINKAGE BEFORE RETURNING CONTROL TO
      *    THE CALLED PROGRAM.
      *
      *  CALLED BY:
      *    -  AUTOPART.CBL PROGRAM
      *
      *  CALLS:
      *    -  0100-EDIT-CHECK
      ****************************************************************
      *
       PROCEDURE DIVISION USING SUPPLIERS-LS, ERROR-MSG-AREA-LS.
      *    DISPLAY 'ENTERING SUPPIER SUBPROGRAM - MAIN PROCEDURE AREA'

      *
      *****************************************************************
      *  MOVE VARIABLES PASSED IN LINKAGE TO LOCAL WORKING STORAGE
      *  VARIABLES
      *****************************************************************
      *
           MOVE SUPPLIERS-LS TO SUPPLIERS.
           MOVE ERROR-MSG-AREA-LS  TO ERROR-MSG-AREA.
      *
           PERFORM 0100-EDIT-CHECK. *>VALIDATE FIELDS
      *
      *****************************************************************
      *  MOVE LOCAL WORKING STORAGE VARIABLES BACK TO THE LINKAGE
      *  SECTION TO MAKE THE UPDATED ERROR MESSAGE AREA AVAILABE TO
      *  THE CALLING PROGRAM
      *****************************************************************
      *
           MOVE ERROR-MSG-AREA TO ERROR-MSG-AREA-LS.

           GOBACK. *> RETURN TO CALLING PROGRAM
      *
      *****************************************************************
      *  DESCRIPTION:
      *    PARAGRAPH 0100-EDIT-CHECK PERFORMS VALIDATION CHECKS ON
      *    SUPPLIERS GROUP AREA FIELDS PASSED BY THE CALLING PROGRAM.
      *    WHEN ERRORS ARE ENCOUNTERED, CALLS ARE MADE TO AN ERROR
      *    ROUTINE PARAGRAPH. THE "IF MAX-ERRORS-MET" STATEMENT BEFORE
      *    FIELD EDITS ENSURES THAT NO FURTHER PROCESSING OF ERRORS IS
      *    DONE IF THE MAXIMUM NUMBER OF ALLOWABLE ERRORS (4) FOR A
      *    RECORD HAS BEEN MET. ERROR MESSAGES GENERATED FOR THE FIELD
      *    VALIDATION CHECKS ARE INDICATIVE OF FIELD VALIDATION RULES.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  0200-ERROR-ROUTINE
      *    -  0300-VALIDATE-DATE
      ****************************************************************
      *
       0100-EDIT-CHECK.
      *    DISPLAY 'ENTERING SUPPLIER SUBPROGRAM - 0100-EDIT-CHECK'.

           IF SUPPLIER-CODE = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER CODE MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF.
      *
      *****************************************************************
      *  THE SUPPLIER-TYPE = SPACES AND VALID-SUPPLIER-TYPE EDIT CHECKS
      *  ARE INCLUDED IN A NESTED "IF" STATEMENT TO AVOID DUPLICATE
      *  ERRORS FOR THE SAME FIELD.  IF THE "IF" STATEMENTS WERE NOT
      *  NESTED, A SUPPLIER-TYPE FIELD = SPACES WOULD GENERATE TWO
      *  ERRORS--ONE FOR SUPPLIER-TYPE = SPACES AND ANOTHER FOR THE
      *  VALID-SUPPLIER-TYPE ERROR CHECK.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF SUPPLIER-TYPE = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER TYPE MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
           IF VALID-SUPPLIER-TYPE
               NEXT SENTENCE
           ELSE
                PERFORM 0200-ERROR-ROUTINE
                MOVE 'SUPPLIER TYPE VALUE IS INVALID.' TO
                      ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF SUPPLIER-NAME = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER NAME MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF SUPPLIER-PERF = ZERO
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER PERF FIELD MUST NOT BE ZERO.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.

           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VALID-SUPPLIER-RATING
              NEXT SENTENCE
           ELSE
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER RATING VALUE IS INVALID.'     TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF.

           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VALID-SUPPLIER-STATUS
              NEXT SENTENCE
           ELSE
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'SUPPLIER STATUS VALUE IS NOT VALID.'   TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE SUPPLIER-ACT-DATE MAY CONTAIN SPACES.  HOWEVER, IF THE
      *  FIELD IS POPULATED, IT MUST CONTAIN A VALID DATE.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF SUPPLIER-ACT-DATE = SPACES
               NEXT SENTENCE
           ELSE
               PERFORM 0300-VALIDATE-DATE
           END-IF
           END-IF.
      *
      *****************************************************************
      *  IF THE SUPPLIER-TYPE FIELD IS 'S' FOR SUBCONTRACTOR, THE
      *  SUPPLIER RATING MUST BE '3' FOR HIGHEST-QUALITY
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF SUBCONTRACTOR
              IF HIGHEST-QUALITY
                 NEXT SENTENCE
              ELSE
                 PERFORM 0200-ERROR-ROUTINE
                 MOVE 'SUPPLIER RATING VALUE MUST BE 3 FOR SUBCONTRACTOR
      -               '.'  TO ERROR-MSG-TEXT (ERROR-IDX)
              END-IF
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    PARAGRAPH 0200-ERROR-ROUTINE, SETS THE RETURN-CODE TO '08',
      *    WHICH SERVES AS A DATA-ERROR SWITCH, ADDS 1 TO THE ERROR-
      *    COUNT AND SETS THE INDEX FOR THE ERROR MESSAGE AREA TO THE
      *    SAME VALUE AS THE NUMBER OF ERRORS COUNTED. THE INDEX FOR THE
      *    ERROR MESSAGE AREA (ERROR-IDX) WILL BE USED IN THE CALLING
      *    PROGRAM TO DETERMINE HOW MANY TIMES TO LOOP THROUGH THE
      *    PASSED BACK LINKAGE ERROR MESSAGE AREA TO MOVE ERRORS
      *    MESSAGES TO THE END OF THE RECORD IN THE ERROR FILE.
      *
      *  CALLED BY:
      *    - 0100-EDIT-CHECK
      *
      *  CALLS:
      *    - NONE
      ****************************************************************
      *
       0200-ERROR-ROUTINE.
      *    DISPLAY 'ENTERING SUPPLIER SUBPROGRAM - 0200-ERROR-ROUTINE'.

           MOVE '08' TO ERR-MSG-RETURN-CODE.
           ADD 1 TO ERROR-COUNTER.
      *
      *****************************************************************
      *  SET THE INDEX IN THE ERROR MESSAGE AREA (ERROR-IDX) TO THE
      *  NUMBER OF ERRORS COUNTED.
      *****************************************************************
      *
           SET ERROR-IDX TO ERROR-COUNTER.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    PARAGRAPH 0300-VALIDATE-DATE CALLS THE IBM CEEDAYS DATE
      *    VALIDATION SUBPROGRAM. IF THE DATE IS VALID, THE FC-SEV
      *    FIELD (RETURN CODE) IS SET TO ZERO. UPON RETURN FROM CEEDAYS,
      *    THE FC-SEV RETURN CODE IS CHECKED AND THE ERROR ROUTINE
      *    PARAGRAPH IS CALLED IF THE DATE IS NOT VALID.
      *
      *  CALLED BY:
      *    - 0100-EDIT-CHECK
      *
      *  CALLS:
      *    - CEEDAYS SUBPROGRAM - IBM DATE VALIDATION SUBPROGRAM
      ****************************************************************
      *
       0300-VALIDATE-DATE.
      *    DISPLAY 'ENTERING SUPPLIER SUBPROGRAM - 0300-VALIDATE-DATE'.

      *
      *****************************************************************
      *  MOVE THE DATE TO BE VALIDATED TO THE LINKAGE VARIABLE
      *  TO BE USED IN THE CEEDAYS SUBPROGRAM.
      *****************************************************************
      *
           MOVE SUPPLIER-ACT-DATE TO W-DATE-IN-STR-CEE.

           CALL 'CEEDAYS' USING W-DATE-IN-CEE
                                W-PICSTR-IN, W-INPUT-DATE-INT, FC.

           IF FC-SEV = ZERO *> RETURN CODE FROM CEEDAYS DATE CHECK
              NEXT SENTENCE
           ELSE
              PERFORM 0200-ERROR-ROUTINE *> DATE NOT VALID. FLAG AS ERR.
              MOVE 'SUPPLIER ACT DATE VALUE IS INVALID.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           END-IF.

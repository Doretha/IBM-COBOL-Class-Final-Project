       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDRSUPP.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 09/15/20.
       DATE-COMPILED. 09/15/20.
       SECURITY. NON-CONFIDENTIAL.
      *
      *****************************************************************
      *  PROGRAM DESCRIPTION:
      *    THIS SUBPROGRAM PERFORMS VALIDATION ON SELECTED FIELDS
      *    IN THE ADDRESSES GROUP AREA OF THE AUTOPARTS INPUT FILE
      *    RECORD PASSED IN LINKAGE BY THE AUTOPART.CBL PROGRAM. ERROR
      *    MESSAGES ARE GENERATED FOR FAILED FIELD EDITS. THE ERROR
      *    COUNT, RETURN CODE AND UP TO 4 ERROR MESSAGES ARE PASSED
      *    BACK THROUGH LINKAGE TO THE CALLING PROGRAM IN THE ERROR
      *    MESSAGE AREA.
      *****************************************************************
      *
      *  CALLED BY PROGRAM:
      *    -  AUTOPARTS.CBL
      *
      *  PROGRAM MODULES CALLED:
      *    -  NONE
      *
      *****************************************************************
      *
      *    INPUT FILES:
      *      -  NONE
      *
      *    OUTPUT FILES:
      *      -  NONE
      *
      *    VARIABLES PASSED IN LINKAGE:
      *      -  ADDRESSES-LS - GROUP AREA OF AUTO PART INPUT FILE
      *         RECORD THAT CONTAINS SUPPLIER ADDRESS INFORMATION
      *      -  ERROR-MSG-LS - ERROR MESSAGE COUNTER, RETURN CODE AND
      *         ERROR MESSAGE TABLE USED TO TRACK THE NUMBER OF ERRORS
      *         AND ERROR MESSAGES GENERATED IN SUBPROGRAM FIELD
      *         VALIDATION CHECKS.
      *      -  STATE-ADDRESS-TBL-LS - STATE/ZIPCODE TABLE USED TO
      *         VALIDATE STATE/ZIPCODE COMBINATIONS ON ADDRESSES
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
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
      *
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR ADDRESSES GROUP AREA FIELDS PASSED
      *  THROUGH LINKAGE.
      *****************************************************************
      *
       01 ADDRESSES.
          05 SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY ADDR-IDX.
             10 ADDRESS-TYPE               PIC X(01) VALUE SPACES.
                88 ORDER-ADDRESS           VALUE '1'.
                88 SCHED-ADDRESS           VALUE '2'.
                88 REMIT-ADDRESS           VALUE '3'.
                88 VALID-ADDRESS-TYPE      VALUE '1', '2', '3'.
             10 ADDRESS-1                  PIC X(15) VALUE SPACES.
             10 ADDRESS-2                  PIC X(15) VALUE SPACES.
             10 ADDRESS-3                  PIC X(15) VALUE SPACES.
             10 CITY                       PIC X(15) VALUE SPACES.
             10 ADDR-STATE                 PIC X(02) VALUE SPACES.
             10 ZIP-CODE                   PIC 9(10) VALUE 0.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR ERROR MESSAGE AREA PASSED THROUGH
      *  LINKAGE.
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
      *  LOCAL WORKING STORAGE FOR STATE/ZIP TABLE PASSED THROUGH
      *  LINKAGE.
      *****************************************************************
      *
       01  STATE-ADDRESS-TBL.
           05 STATE-ADDRESS-ZIP-TBL OCCURS 72 TIMES INDEXED BY
              STATE-IDX.
             10 STATE-LONG-TBL        PIC X(15) VALUE SPACES.
             10 FILLER                PIC X(01) VALUE SPACES.
             10 STATE-ABBREV-TBL      PIC X(02) VALUE SPACES.
             10 FILLER                PIC X(02) VALUE SPACES.
             10 LOW-ZIP-TBL           PIC 9(10) VALUE 0.
             10 FILLER                PIC X(03) VALUE SPACES.
             10 HIGH-ZIP-TBL          PIC 9(10) VALUE 0.
      *
      *****************************************************************
      *  END OF TABLE DATA AND PROCESS SWITCHES
      *****************************************************************
      *
       01 WS-VARIABLE-SWITCHES.
          05 SEARCH-STATE-TABLE-SW    PIC X(01) VALUE 'N'.
             88 SEARCH-STATE-TABLE    VALUE 'Y'.
          05 STATE-ZIP-FOUND-SW       PIC X(01) VALUE 'N'.
             88 STATE-ZIP-FOUND       VALUE 'Y'.
             88 STATE-ZIP-NOT-FOUND   VALUE 'N'.
          05 STATE-ENTRY-SW           PIC X(01) VALUE 'N'.
             88 STATE-FOUND           VALUE 'Y'.
      *
       01 WS-TEMP-VARIABLES.
          05 ADDR-CTR                 PIC 9(3).
          05 WS-MAX-STATE-IDX         PIC 9(02) VALUE 72.
      *
      *****************************************************************
      *  DECLARATION OF VARIABLE NAMES AND THEIR SIZES PASSED THROUGH
      *  LINKAGE AREA FROM AUTOPARTS.CBL PROGRAM
      *****************************************************************
      *
       LINKAGE SECTION.
       01  ADDRESSES-LS          PIC X(219).
       01  STATE-ADDRESS-TBL-LS  PIC X(3096).
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
       PROCEDURE DIVISION USING ADDRESSES-LS, STATE-ADDRESS-TBL-LS,
                                ERROR-MSG-AREA-LS.
      *    DISPLAY 'ENTERING ADDRSUPP SUBPROGRAM - MAIN PROCEDURE AREA'.

      *
      *****************************************************************
      *  MOVE VARIABLES PASSED IN LINKAGE TO LOCAL WORKING STORAGE
      *  VARIABLES
      *****************************************************************
      *
           MOVE ADDRESSES-LS TO ADDRESSES.
           MOVE STATE-ADDRESS-TBL-LS TO STATE-ADDRESS-TBL.
           MOVE ERROR-MSG-AREA-LS  TO ERROR-MSG-AREA.
      *
      *****************************************************************
      *  PERFORM THE EDIT CHECK PARAGRAPH 3 TIMES (# OF OCCURENCES OF
      *  ADDRESSES IN ADDRESS TABLE) TO CONDUCT EDIT CHECKS ON ADDRESS
      *  FIELDS.
      *****************************************************************
      *
           PERFORM 0100-EDIT-CHECK VARYING ADDR-IDX FROM 1 BY 1
               UNTIL ADDR-IDX > 3.
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
      *    ADDRESSES GROUP AREA FIELDS PASSED BY THE CALLING PROGRAM.
      *    WHEN ERRORS ARE ENCOUNTERED, CALLS ARE MADE TO AN ERROR
      *    ROUTINE PARAGRAPH. THE "IF MAX-ERRORS-MET" STATEMENT BEFORE
      *    FIELD EDITS ENSURES THAT NO FURTHER PROCESSING OF ERRORS IS
      *    DONE IF THE MAXIMUM NUMBER OF ALLOWABLE ERRORS (4) FOR A
      *    RECORD HAS BEEN MET. ERROR MESSAGES GENERATED FOR THE FIELD
      *    VALIDATION CHECKS ARE INDICATIVE OF FIELD VALIDATION RULES.
      *    FOR ERRORS GENERATED BY FIELDS IN TABLE OCCURRENCES, THE
      *    STRING FUNCTION IS USED TO CONCATENATE THE INDEX OF THE
      *    ADDRESS FIELD TABLE ENTRY TO THE ERROR MESSAGE.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  0200-ERROR-ROUTINE
      *    -  0300-SEARCH-STATE-ZIP-TABLE
      ****************************************************************
      *
       0100-EDIT-CHECK.
      *    DISPLAY 'ENTERING ADDRSUPP SUBPROGRAM - 0100-EDIT-CHECK'.

      *
      *****************************************************************
      *  THE ADDR-CTR, WHICH IS SET TO THE VALUE OF THE ADDRESS TABLE
      *  INDEX, IS DISPLAYED IN THE ERROR MESSAGE TO INDICATE
      *  WHICH OCCURENCE OF A FIELD IN THE ADDRESS TABLE PRODUCED
      *  THE ERROR.
      *****************************************************************
      *
           SET ADDR-CTR TO ADDR-IDX.
      *
      *****************************************************************
      *  THE SEARCH-STATE-TABLE-SW IS INITIALIZED TO 'Y', MEANING THE
      *  STATE/ZIP TABLE SHOULD BE SEARCHED.  WHEN A VALIDATION ERROR
      *  OCCURS, THE SEARCH-TABLE-SW IS SET TO 'N' AND THE STATE/ZIP
      *  TABLE IS NOT SEARCHED.
      *****************************************************************
      *
           MOVE 'Y' TO SEARCH-STATE-TABLE-SW.
      *
      *****************************************************************
      *  INITIALIZE THE STATE ENTRY FOUND AND STATE ZIP CODE FOUND
      *  SWITCHES TO NO.
      ******************************************************************
      *
           MOVE 'N' TO STATE-ENTRY-SW, STATE-ZIP-FOUND-SW.
      *
      *****************************************************************
      *  THERE ARE 3 LINES FOR EACH ADDRESS (ADDRESS-1, ADDRESS-2,
      *  ADDRESS-3). ADDRESS-2 AND ADDRESS-3 PROVIDE ADDITIONAL SPACE
      *  FOR SUITE NUMBERS, FLOOR NUMBERS, ETC. ADDRESS-1 IS THE ONLY
      *  REQUIRED FIELD. THEREFORE, NO EDITS ARE PERFORMED ON THE
      *  ADDRESS-2 AND ADDRESS-3 FIELDS.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF ADDRESS-1 (ADDR-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'ADDRESS '  DELIMITED BY SIZE
                      ADDR-CTR   DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE SPACES.'
                                 DELIMITED BY SIZE
                          INTO ERROR-MSG-TEXT (ERROR-IDX)
              MOVE 'N' TO SEARCH-STATE-TABLE-SW *> NO STATE/ZIP SEARCH
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF CITY (ADDR-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'CITY '     DELIMITED BY SIZE
                      ADDR-CTR   DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE SPACES.'
                                 DELIMITED BY SIZE
                          INTO ERROR-MSG-TEXT (ERROR-IDX)
              MOVE 'N' TO SEARCH-STATE-TABLE-SW
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF ADDR-STATE (ADDR-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'STATE '     DELIMITED BY SIZE
                      ADDR-CTR    DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE SPACES.'
                                  DELIMITED BY SIZE
                          INTO ERROR-MSG-TEXT (ERROR-IDX)
              MOVE 'N' TO SEARCH-STATE-TABLE-SW
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF ZIP-CODE (ADDR-IDX) = 0
              PERFORM 0200-ERROR-ROUTINE
              STRING 'ZIP CODE ' DELIMITED BY SIZE
                      ADDR-CTR   DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE ZEROES.'
                                 DELIMITED BY SIZE
                        INTO ERROR-MSG-TEXT (ERROR-IDX)
              MOVE 'N' TO SEARCH-STATE-TABLE-SW
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VALID-ADDRESS-TYPE (ADDR-IDX)
              NEXT SENTENCE
           ELSE
              PERFORM 0200-ERROR-ROUTINE
              STRING 'ADDRESS TYPE '           DELIMITED BY SIZE
                      ADDR-CTR                 DELIMITED BY SIZE
                     ' OCCURRENCE IS INVALID.' DELIMITED BY SIZE
                         INTO ERROR-MSG-TEXT (ERROR-IDX)
              MOVE 'N' TO SEARCH-STATE-TABLE-SW
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
      *****************************************************************
      *  SEARCH THE STATE/ZIP TABLE WHEN THE MAXIMUM NUMBER OF ERRORS
      *  HAS NOT BEEN MET AND THERE ARE NO ERRORS FOUND IN THE ADDRESS
      *  FIELDS. THE STATE-ENTRY-SW IS SET TO 'Y' WHEN THE STATE CODE
      *  IN THE ADDRESS TABLE ENTRY IS EQUAL TO A STATE ABBREVIATION ON
      *  THE STATE/ZIP TABLE. THE STATE-ZIP-FOUND SWITCH IS SET TO 'Y'
      *  WHEN THE ZIP CODE IN THE ADDRESS TABLE ENTRY FALLS WITHIN THE
      *  RANGE OF THE LOWEST AND HIGHEST ZIP CODES ON THE STATE/ZIP
      *  TABLE. IF THE STATE OR ZIP CODE IN THE ADDRESS FIELDS ARE NOT
      *  FOUND, ERRORS ARE GENERATED.
      *****************************************************************
      *
           IF SEARCH-STATE-TABLE *>NO ERRORS IN ADDRESS FIELD ENTRIES
              PERFORM 0300-SEARCH-STATE-ZIP-TABLE
              IF STATE-FOUND *> ADDRESS STATE FOUND ON STATE/ZIP TABLE
                 IF STATE-ZIP-FOUND *>ADDR ZIP FOUND ON STATE/ZIP TABLE
                    NEXT SENTENCE
                 ELSE
                    PERFORM 0200-ERROR-ROUTINE *>STATE NOT FOUND
                    STRING 'STATE/ZIP ' DELIMITED BY SIZE
                            ADDR-CTR    DELIMITED BY SIZE
                           ' COMBO NOT FOUND ON STATE/ZIP TABLE.'
                                        DELIMITED BY SIZE
                        INTO ERROR-MSG-TEXT (ERROR-IDX)
                 END-IF
              ELSE
                 PERFORM 0200-ERROR-ROUTINE *>ZIP CODE NOT FOUND
                 STRING 'STATE '         DELIMITED BY SIZE
                         ADDR-CTR        DELIMITED BY SIZE
                        ' OCCURRENCE NOT FOUND IN STATE/ZIP TABLE.'
                                         DELIMITED BY SIZE
                              INTO ERROR-MSG-TEXT (ERROR-IDX)
              END-IF
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
      *    -  0100-EDIT-CHECK
      *
      *  CALLS:
      *    -  NONE
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
      *    PARAGRAPH 0300-SEARCH-STATE-ZIP-TABLE SEARCHES THE STATE/ZIP
      *    TABLE FOR THE STATE CODE IN THE ADDRESS TABLE ENTRY. THE
      *    STATE/ZIP TABLE IS SEARCHED UNTIL THE STATE-ZIP TABLE INDEX
      *    IS > 72 (WS-MAX-STATE-IDX) OR THE STATE AND ZIP CODE IN THE
      *    ADDRESS TABLE ENTRY ARE FOUND ON THE STATE/ZIP TABLE. WHEN
      *    THE STATE IN THE ADDRESS TABLE ENTRY IS FOUND ON THE
      *    STATE/ZIP TABLE, THE PARAGRAPH TO COMPARE ZIP CODE RANGES IS
      *    CALLED.
      *
      *  CALLED BY:
      *    -  0100-EDIT-CHECK
      *
      *  CALLS:
      *    -  0400-CHECK-VALID-STATE-ZIP
      ****************************************************************
      *
       0300-SEARCH-STATE-ZIP-TABLE.
      *    DISPLAY 'ENTERING ADDRSUP SUBPROGRAM 0300-SEARCH-STATE-ZIP-TA
      *-             'BLE'.

           PERFORM VARYING STATE-IDX FROM 1 BY 1
                UNTIL (STATE-IDX  >  WS-MAX-STATE-IDX) OR
                       STATE-ZIP-FOUND
      *****************************************************************
      *  STATE IN ADDRESS FIELD TABLE ENTRY IS FOUND IN STATE/ZIP TABLE
      *****************************************************************
                IF ADDR-STATE (ADDR-IDX) = STATE-ABBREV-TBL (STATE-IDX)
                   MOVE 'Y' TO STATE-ENTRY-SW
                   PERFORM 0400-CHECK-VALID-STATE-ZIP
                END-IF
              END-PERFORM.
      *
      *****************************************************************
      *  DESCRIPTION:
      *    PARAGRAPH 0400-CHECK-VALID-STATE-ZIP COMPARES THE ZIP CODE
      *    IN THE ADDRESS FIELD TABLE ENTRY TO DETERMINE IF THE VALUE
      *    FALLS BETWEEN THE LOW AND HIGH ZIP CODE VALUES ON THE
      *    STATE/ZIP TABLE. IF THE ZIP CODE FALLS WITHIN THE LOW/HIGH
      *    RANGES, THE STATE ZIP FOUND SWITCH IS SET TO 'Y'.
      *
      *  CALLED BY:
      *    -  0100-EDIT-CHECK
      *
      *  CALLS:
      *    -  0300-SEARCH-STATE-ZIP-TABLE
      *****************************************************************
      *
       0400-CHECK-VALID-STATE-ZIP.
      *    DISPLAY 'ENTERING ADDRSUP SUBPROGRAM 0400-CHECK-VALID-STATE-Z
      *-           'IP'.

           IF ZIP-CODE (ADDR-IDX) >= LOW-ZIP-TBL (STATE-IDX) AND
              ZIP-CODE (ADDR-IDX) <= HIGH-ZIP-TBL (STATE-IDX)
              MOVE 'Y' TO STATE-ZIP-FOUND-SW
           ELSE
              NEXT SENTENCE
           END-IF.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. PURCHORD.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 09/15/20.
       DATE-COMPILED. 09/15/20.
       SECURITY. NON-CONFIDENTIAL.
      *
      *****************************************************************
      *  PROGRAM DESCRIPTION:
      *    THIS SUBPROGRAM PERFORMS VALIDATION ON SELECTED FIELDS
      *    IN THE PURCHASE ORDER GROUP AREA OF THE AUTOPARTS
      *    INPUT FILE RECORD PASSED IN LINKAGE BY THE AUTOPART.CBL
      *    PROGRAM. ERROR MESSAGES ARE GENERATED FOR FAILED FIELD EDITS.
      *    THE ERROR COUNT, RETURN CODE AND UP TO 4 ERROR MESSAGES ARE
      *    PASSED BACK THROUGH LINKAGE TO THE CALLING PROGRAM IN THE
      *    ERROR MESSAGE AREA.
      *****************************************************************
      *
      *  CALLED BY PROGRAM:
      *    - AUTOPARTS.CBL
      *
      *  PROGRAM MODULES CALLED:
      *    - CEEDAYS - IBM DATE VALIDATION PROGRAM
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
      *      -  PURCHORD-ORDER-LS - GROUP AREA OF AUTO PART INPUT
      *         FILE RECORD THAT CONTAINS PURCHASE ORDER INFORMATION
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
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR PURCHASE ORDER GROUP AREA FIELDS
      *  PASSED THROUGH LINKAGE.
      *****************************************************************
      *
       01 PURCHASE-ORDER.
          05 PURCH-ORD OCCURS 3 TIMES INDEXED BY PO-IDX.
             10 PO-NUMBER               PIC X(06) VALUE SPACES.
             10 BUYER-CODE              PIC X(03) VALUE SPACES.
             10 QUANTITY                PIC S9(7)  VALUE ZERO.
                88 VALID-QUANTITY-RANGE VALUE 0 THROUGH +999,998.
             10 UNIT-PRICE              PIC S9(7)V99  VALUE ZERO.
                88 VALID-UNIT-PRICE-RANGE VALUE +1 THROUGH +1000000.
             10 ORDER-DATE              PIC X(08) VALUE SPACES.
             10 DELIVERY-DATE           PIC X(08) VALUE SPACES.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR ERROR MESSAGE AREA PASSED THROUGH
      *  LINKAGE.
      *****************************************************************
      *
       01 ERROR-MSG-AREA.
          05 ERROR-COUNTER              PIC 99 VALUE 0.
             88 MAX-ERRORS-MET          VALUE 4.
          05 ERR-MSG-RETURN-CODE        PIC X(02).
             88 DATA-ERROR              VALUE '08'.
             88 VALID-DATA              VALUE '00'.
          05 ERROR-MSG-TABLE OCCURS 4 TIMES INDEXED BY ERROR-IDX.
             10 ERROR-MSG-TEXT          PIC X(50) VALUE SPACES.
      *
      *****************************************************************
      *  LOCAL WORKING STORAGE FOR VARIABLES TO BE PASSED IN LINKAGE TO
      *  IBM DATE VALIDATION SUBPROGRAM CEEDAYS
      *****************************************************************
      *
       01 W-INPUT-DATE-INT              PIC 9(9) COMP.
       01 W-PICSTR-IN.
          10  W-PICSTR-LTH-IN           PIC S9(4) COMP VALUE 8.
          10  W-PICSTR-STR-IN           PIC X(8)  value 'YYYYMMDD'.
       01 W-DATE-IN-CEE.
          10  W-DATE-IN-LTH-CEE         PIC S9(4) COMP VALUE 8.
          10  W-DATE-IN-STR-CEE         PIC X(8).
       01 FC.
          10  FC-SEV                    PIC S9(4) COMP.
          10  FC-MSG                    PIC S9(4) COMP.
          10  FC-CTW                    PIC X.
          10  FC-FAC                    PIC X(3).
          10  FC-ISI                    PIC S9(8) COMP.
      *
      *****************************************************************
      *  VALID DATA SWITCHES
      *****************************************************************
      *
       01 VARIABLE-SWTICHES.
          05 VALID-DATE-SW              PIC X(01).
             88 VALID-DATE              VALUE 'Y'.
             88 INVALID-DATE            VALUE 'N'.
          05 VALID-UNIT-PRICE-SW        PIC X(01).
             88 VALID-UNIT-PRICE        VALUE 'Y'.
             88 INVALID-UNIT-PRICE      VALUE 'N'.
          05 VALID-QUANTITY-SW          PIC X(01).
             88 VALID-QUANTITY          VALUE 'Y'.
             88 INVALID-QUANTITY        VALUE 'N'.
      *
      *****************************************************************
      *  VARIABLES USED TO DISPLAY INDEX AND PACKED FIELDS
      *****************************************************************
      *
       01 WS-TEMP-VARIABLES.
          05 PO-CTR                     PIC 9(3).
          05 WS-HOLD-UNIT-PRICE         PIC $$,$$$,$$$,$$$.99.
          05 WS-HOLD-QUANTITY           PIC ZZZ,ZZZ,ZZ9.99.
      *
      *****************************************************************
      *  DECLARATION OF VARIABLE NAMES AND THEIR SIZES PASSED THROUGH
      *  LINKAGE AREA FROM AUTOPARTS.CBL PROGRAM
      *****************************************************************
      *
       LINKAGE SECTION.
       01  PURCHORD-ORDER-LS            PIC X(123).
       01  ERROR-MSG-AREA-LS            PIC X(204).
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
       PROCEDURE DIVISION USING PURCHORD-ORDER-LS, ERROR-MSG-AREA-LS.
      *    DISPLAY 'ENTERING PURCHORD SUBPROGRAM - MAIN PROCEDURE AREA'.

      *
      *****************************************************************
      *  MOVE VARIABLES PASSED IN LINKAGE TO LOCAL WORKING STORAGE
      *  VARIABLES
      *****************************************************************
      *
           MOVE PURCHORD-ORDER-LS TO PURCHASE-ORDER.
           MOVE ERROR-MSG-AREA-LS  TO ERROR-MSG-AREA.
      *
      *****************************************************************
      *  CALL THE EDIT CHECK ROUTINE 3 TIMES TO PROCESS ALL ENTRIES
      *  IN THE PURCHASE ORDER TABLE
      *****************************************************************
      *
           PERFORM 0100-EDIT-CHECK VARYING PO-IDX FROM 1 BY 1
               UNTIL PO-IDX > 3.

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
      *    PURCHASE ORDER GROUP AREA FIELDS PASSED BY THE CALLING
      *    PROGRAM. WHEN ERRORS ARE ENCOUNTERED, CALLS ARE MADE TO AN
      *    ERROR ROUTINE PARAGRAPH. THE "IF MAX-ERRORS-MET" STATEMENT
      *    BEFORE FIELD EDITS ENSURES THAT NO FURTHER PROCESSING OF
      *    ERRORS IS DONE IF THE MAXIMUM NUMBER OF ALLOWABLE ERRORS (4)
      *    FOR A  RECORD HAS BEEN MET. ERROR MESSAGES GENERATED FOR THE
      *    FIELD VALIDATION CHECKS ARE INDICATIVE OF FIELD VALIDATION
      *    RULES. FOR ERRORS GENERATED BY FIELDS IN TABLE OCCURRENCES,
      *    THE STRING FUNCTION IS USED TO CONCATENATE THE INDEX OF THE
      *    ENTRY IN THE TABLE TO THE ERROR MESSAGE.
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
      *    DISPLAY 'ENTERING PURCHORD SUBPROGRAM - 0100-EDIT-CHECK'.

      *
      *****************************************************************
      *  SET VALID DATA FIELD SWITCHES TO 'Y'.  WHEN ERRORS FOR
      *  THE ORDER DATE, UNIT PRICE OR QUANTITY FIELDS OCCUR, THE
      *  SWITCHES WILL BE SET TO 'N'.
      *****************************************************************
      *
           MOVE 'Y' TO VALID-DATE-SW.
           MOVE 'Y' TO VALID-UNIT-PRICE-SW.
           MOVE 'Y' TO VALID-QUANTITY-SW.
      *
      *****************************************************************
      *  THE PO-CTR, WHICH IS SET TO THE VALUE OF THE PURCHASE ORDER
      *  TABLE INDEX, IS DISPLAYED IN THE ERROR MESSAGE TO INDICATE
      *  WHICH OCCURENCE OF A FIELD IN THE PURCHASE TABLE PRODUCED
      *  THE ERROR.
      *****************************************************************
      *
           SET PO-CTR TO PO-IDX.
      *
           IF PO-NUMBER (PO-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'PO-NUMBER '     DELIMITED BY SIZE
                      PO-CTR          DELIMITED BY SIZE
                     ' OCCURENCE MUST NOT BE SPACES.'
                                      DELIMITED BY SIZE
                    INTO ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF BUYER-CODE (PO-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'BUYER-CODE '    DELIMITED BY SIZE
                      PO-CTR          DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE SPACES.'
                                      DELIMITED BY SIZE
                    INTO ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
      *****************************************************************
      *  ORDER DATE EDITS ARE INCLUDED IN A NESTED "IF" STATEMENT TO
      *  PREVENT AN UNNECESSARY CALL TO THE DATE VALIDATION ROUTINE IF
      *  THE ORDER DATE IS EQUAL SPACES. THIS ALSO PREVENTS GENERATION
      *  OF DUPLICATE ERRORS--ORDER DATE EQUAL SPACES AND INVALID DATE.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF ORDER-DATE (PO-IDX) = SPACES
              PERFORM 0200-ERROR-ROUTINE
              STRING 'ORDER-DATE '    DELIMITED BY SIZE
                      PO-CTR          DELIMITED BY SIZE
                     ' OCCURRENCE MUST NOT BE SPACES.'
                                      DELIMITED BY SIZE
                      INTO ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              MOVE ORDER-DATE (PO-IDX) TO W-DATE-IN-STR-CEE
              PERFORM 0300-VALIDATE-DATE
              IF FC-SEV = ZERO *> THE ORDER-DATE IS VALID
                 NEXT SENTENCE
              ELSE
                 PERFORM 0200-ERROR-ROUTINE
                 STRING 'ORDER-DATE ' DELIMITED BY SIZE
                         PO-CTR       DELIMITED BY SIZE
                        ' OCCURRENCE CONTAINS INVALID DATE.'
                                      DELIMITED BY SIZE
                     INTO ERROR-MSG-TEXT (ERROR-IDX)
              END-IF
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VALID-QUANTITY-RANGE (PO-IDX)
              NEXT SENTENCE
           ELSE
              MOVE 'N' TO VALID-QUANTITY-SW
              PERFORM 0200-ERROR-ROUTINE
              STRING 'QUANTITY '      DELIMITED BY SIZE
                      PO-CTR          DELIMITED BY SIZE
                     ' OCCURRENCE MUST BE 0 TO 999,998.'
                                      DELIMITED BY SIZE
                    INTO ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VALID-UNIT-PRICE-RANGE (PO-IDX)
              NEXT SENTENCE
           ELSE
              MOVE 'N' TO VALID-UNIT-PRICE-SW
              PERFORM 0200-ERROR-ROUTINE
              STRING 'UNIT PRICE '    DELIMITED BY SIZE
                      PO-CTR          DELIMITED BY SIZE
                     ' OCCURRENCE MUST BE $1 TO $1,000,000.'
                                      DELIMITED BY SIZE
                        INTO ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE NESTED "IF" STATEMENT CHECKS FOR INVALID QUANTIY AND
      *  INVALID UNIT PRICE VALUES BEFORE CHECKING FOR THE COMPOUND
      *  EDIT, QUANTITY > 0 AND UNIT PRICE > 0, TO AVOID GENERATING
      *  MULTIPLE ERRORS IF EITHER OF THESE FIELDS ARE FOUND INVALID IN
      *  PRIOR EDITS.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF INVALID-QUANTITY
              NEXT SENTENCE
           ELSE
           IF INVALID-UNIT-PRICE
              NEXT SENTENCE
           ELSE
           IF QUANTITY (PO-IDX) > 0
              IF UNIT-PRICE (PO-IDX) > 0
                 NEXT SENTENCE
               ELSE
                 PERFORM 0200-ERROR-ROUTINE
                 STRING 'UNIT PRICE ' DELIMITED BY SIZE
                         PO-CTR       DELIMITED BY SIZE
                        ' OCCURRENCE MUST BE > 0 IF QTY > 0.'
                                      DELIMITED BY SIZE
                           INTO ERROR-MSG-TEXT (ERROR-IDX)
               END-IF
              END-IF
           END-IF
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE DELIVERY-DATE IS NOT REQUIRED, BUT IF POPULATED, MUST BE
      *  VALID. DELIVERY-DATE EDITS ARE INCLUDED IN A NESTED "IF"
      *  STATEMENT TO PREVENT AN UNNECESSARY CALL TO THE DATE VALIDATION
      *  ROUTINE IF THE DELIVERY DATE IS EQUAL SPACES. THIS ALSO
      *  PREVENTS GENERATION OF DUPLICATE ERRORS--DELIVERY-DATE EQUAL
      *  SPACES, INVALID DATE AND DELIVERY DATE LESS THAN ORDER-DATE.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF DELIVERY-DATE (PO-IDX) = SPACES
              NEXT SENTENCE
           ELSE
              MOVE DELIVERY-DATE (PO-IDX) TO W-DATE-IN-STR-CEE
              PERFORM 0300-VALIDATE-DATE
              IF FC-SEV = ZERO *> VALID DATE
                 IF DELIVERY-DATE (PO-IDX) > ORDER-DATE (PO-IDX)
                    NEXT SENTENCE
                 ELSE
                    PERFORM 0200-ERROR-ROUTINE
                    STRING 'DELIVERY DATE ' DELIMITED BY SIZE
                            PO-CTR          DELIMITED BY SIZE
                           ' MUST BE GREATER THAN ORDER DATE.'
                                            DELIMITED BY SIZE
                             INTO ERROR-MSG-TEXT (ERROR-IDX)
                 END-IF
              ELSE
                 PERFORM 0200-ERROR-ROUTINE
                 STRING 'DELIVERY DATE '    DELIMITED BY SIZE
                            PO-CTR          DELIMITED BY SIZE
                           ' OCCURENCE CONTAINS INVALID DATE.'
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
      *    DISPLAY 'ENTERING PURCHORD SUBPROGRAM - 0200-ERROR-ROUTINE'.

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
      *    -  0100-EDIT-CHECK
      *
      *  CALLS:
      *    -  CEEDAYS SUBPROGRAM - IBM DATE VALIDATION SUBPROGRAM
      ****************************************************************
      *
       0300-VALIDATE-DATE.
           CALL 'CEEDAYS' USING W-DATE-IN-CEE
                                W-PICSTR-IN, W-INPUT-DATE-INT, FC.

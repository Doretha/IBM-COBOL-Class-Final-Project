       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTSUPP.
       AUTHOR. DORETHA RILEY.
       INSTALLATION. COBOL DEV CENTER.
       DATE-WRITTEN. 09/15/20.
       DATE-COMPILED. 09/15/20.
       SECURITY. NON-CONFIDENTIAL.
      *
      *****************************************************************
      *  PROGRAM DESCRIPTION:
      *    THIS SUBPROGRAM PERFORMS VALIDATION ON SELECTED FIELDS
      *    IN THE PARTS GROUP AREA OF THE AUTOPARTS INPUT FILE RECORD
      *    PASSED IN LINKAGE BY THE AUTOPART.CBL PROGRAM. ERROR MESSAGES
      *    ARE GENERATED FOR FAILED FIELD EDITS. THE ERROR COUNT, RETURN
      *    CODE AND UP TO 4 ERROR MESSAGES ARE PASSED BACK THROUGH
      *    LINKAGE TO THE CALLING PROGRAM IN THE ERROR MESSAGE AREA.
      *****************************************************************
      *
      *  CALLED BY PROGRAM:
      *    - AUTOPARTS.CBL
      *
      *  PROGRAM MODULES CALLED:
      *    - NONE
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
      *      -  PARTS-LS - GROUP AREA OF AUTO PART INPUT FILE RECORD
      *         THAT CONTAINS PARTS INFORMATION
      *      -  ERROR-MSG-LS - ERROR MESSAGE COUNTER, RETURN CODE AND
      *         ERROR MESSAGE TABLE USED TO TRACK THE NUMBER OF ERRORS
      *         AND ERROR MESSAGES GENERATED IN SUBPROGRAM FIELD
      *         VALIDATION CHECKS.
      *
      *    JCL JOB:
      *      RTPOT44.FINAL.JCL(AUTOPART)
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
      *  LOCAL WORKING STORAGE FOR PARTS GROUP AREA FIELDS PASSED
      *  THROUGH LINKAGE.
      *****************************************************************
      *
       01  PARTS.
           05 PART-NUMBER       PIC X(23) VALUE SPACES.
           05 PART-NAME         PIC X(14) VALUE SPACES.
           05 SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05 GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05 BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
           05 UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05 WEEKS-LEAD-TIME   PIC 9(03) VALUE 0.
              88 VALID-WEEKS-LEAD-TIME VALUE 1 THROUGH 4.
           05 VEHICLE-MAKE      PIC X(03) VALUE SPACES.
              88 CHRYSLER       VALUE 'CHR'.
              88 FORD           VALUE 'FOR'.
              88 GM             VALUE 'GM '.
              88 VOLKSWAGON     VALUE 'VW '.
              88 TOYOTA         VALUE 'TOY'.
              88 JAGUAR         VALUE 'JAG'.
              88 PEUGEOT        VALUE 'PEU'.
              88 BMW            VALUE 'BMW'.
              88 VALID-VEHICLE-MAKE VALUE 'CHR', 'FOR', 'GM ', 'VW ',
                                         'TOY', 'JAG', 'PEU', 'BMW'.
           05 VEHICLE-MODEL     PIC X(10) VALUE SPACES.
           05 VEHICLE-YEAR      PIC X(04) VALUE '0000'.
              88 VALID-VEHICLE-YEAR VALUE '1990' THROUGH '2019'.
           05 FILLER            PIC X(14) VALUE SPACES.
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
      *  DECLARATION OF VARIABLE NAMES AND THEIR SIZES PASSED THROUGH
      *  LINKAGE AREA FROM AUTOPARTS.CBL PROGRAM
      *****************************************************************
      *
       LINKAGE SECTION.
       01  PARTS-LS                   PIC X(92).
       01  ERROR-MSG-AREA-LS          PIC X(204).
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
       PROCEDURE DIVISION USING PARTS-LS, ERROR-MSG-AREA-LS.
      *    DISPLAY 'ENTERING PARTSUPP SUBPROGRAM - MAIN PROCEDURE AREA'.

      *
      *****************************************************************
      *  MOVE VARIABLES PASSED IN LINKAGE TO LOCAL WORKING STORAGE
      *  VARIABLES
      *****************************************************************
      *
           MOVE PARTS-LS TO PARTS.
           MOVE ERROR-MSG-AREA-LS  TO ERROR-MSG-AREA.

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
      *    PARTS GROUP AREA FIELDS PASSED BY THE CALLING PROGRAM. WHEN
      *    ERRORS ARE ENCOUNTERED, CALLS ARE MADE TO AN ERROR ROUTINE
      *    PARAGRAPH. THE "IF MAX-ERRORS-MET" STATEMENT BEFORE FIELD
      *    EDITS ENSURES THAT NO FURTHER PROCESSING OF ERRORS IS DONE
      *    IF THE MAXIMUM NUMBER OF ALLOWABLE ERRORS (4) FOR A RECORD
      *    HAS BEEN MET. ERROR MESSAGES GENERATED FOR THE FIELD VALI-
      *    DATION CHECKS ARE INDICATIVE OF FIELD VALIDATION RULES.
      *
      *  CALLED BY:
      *    -  MAIN PROCEDURE AREA
      *
      *  CALLS:
      *    -  0200-ERROR-ROUTINE
      ****************************************************************
      *
       0100-EDIT-CHECK.
      *    DISPLAY 'ENTERING PARTSUPP SUBPROGRAM - 0100-EDIT-CHECK'.

           IF PART-NUMBER = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'PART NUMBER MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF PART-NAME = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'PART NAME MUST NOT BE SPACES.'  TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE VEHICLE-MAKE = SPACES AND VALID-VEHICLE-MAKE EDIT CHECKS
      *  ARE INCLUDED IN A NESTED "IF" STATEMENT TO AVOID DUPLICATE
      *  ERRORS FOR THE SAME FIELD.  IF THE "IF" STATEMENTS WERE NOT
      *  NESTED, A VEHICLE-MAKE FIELD = SPACES WOULD GENERATE TWO
      *  ERRORS--ONE FOR VEHICLE-MAKE = SPACES AND ANOTHER FOR THE
      *  VALID-VEHICLE-MAKE ERROR CHECK.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VEHICLE-MAKE = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'VEHICLE MAKE MUST NOT BE SPACES.'  TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
           IF VALID-VEHICLE-MAKE
              NEXT SENTENCE
           ELSE
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'VEHICLE MAKE VALUE IS INVALID.'    TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF
           END-IF.
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VEHICLE-MODEL = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'VEHICLE MODEL MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
              NEXT SENTENCE
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE WEEKS-LEAD-TIME AND VALID-WEEKS-LEAD-TIME EDIT CHECKS
      *  ARE INCLUDED IN A NESTED "IF" STATEMENT TO AVOID DUPLICATE
      *  ERRORS FOR THE SAME FIELD.  IF THE "IF" STATEMENTS WERE NOT
      *  NESTED, IT IS POSSIBLE FOR THE WEEKS-LEAD-TIME TO CONTAIN
      *  CHARACTERS, RATHER THAN NUMERICS. THIS WOULD GENERATE 2
      *  SEPARATE ERRORS FOR THE SAME FIELD--NOT NUMERIC AND NOT IN THE
      *  VALID RANGE OF VALUES (1 TO 4).
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF WEEKS-LEAD-TIME NUMERIC
              IF VALID-WEEKS-LEAD-TIME
                 NEXT SENTENCE
              ELSE
                 PERFORM 0200-ERROR-ROUTINE
                 MOVE 'WEEKS LEAD TIME VALUE RANGE MUST BE 1 THROUGH 4.'
                    TO ERROR-MSG-TEXT (ERROR-IDX)
              END-IF
           ELSE
               PERFORM 0200-ERROR-ROUTINE
               MOVE 'WEEKS LEAD TIME FIELD MUST BE NUMERIC.' TO
                      ERROR-MSG-TEXT (ERROR-IDX)
           END-IF
           END-IF.
      *
      *****************************************************************
      *  THE VEHICLE-YEAR = SPACES AND VALID-VEHICLE-YEAR EDIT CHECKS
      *  ARE INCLUDED IN A NESTED "IF" STATEMENT TO AVOID DUPLICATE
      *  ERRORS FOR THE SAME FIELD.  IF THE "IF" STATEMENTS WERE NOT
      *  NESTED, A VEHICLE-YEAR FIELD = SPACES WOULD GENERATE TWO
      *  ERRORS--ONE FOR VEHICLE-YEAR = SPACES AND ANOTHER FOR THE
      *  VALID-VEHICLE-YEAR ERROR CHECK.
      *****************************************************************
      *
           IF MAX-ERRORS-MET
              NEXT SENTENCE
           ELSE
           IF VEHICLE-YEAR = SPACES
              PERFORM 0200-ERROR-ROUTINE
              MOVE 'VEHICLE-YEAR MUST NOT BE SPACES.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
           ELSE
           IF VALID-VEHICLE-YEAR
              NEXT SENTENCE
           ELSE
             PERFORM 0200-ERROR-ROUTINE
             MOVE 'VEHICLE-YEAR MUST BE BETWEEN 1990 AND 2019.' TO
                    ERROR-MSG-TEXT (ERROR-IDX)
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
      *    DISPLAY 'ENTERING PARTSUPP SUBPROGRAM - 0200-ERROR-ROUTINE'.

           MOVE '08' TO ERR-MSG-RETURN-CODE. *>DATA ERROR SWITCH
           ADD 1 TO ERROR-COUNTER. *>ADD 1 TO NUMBER OF ERRORS FOUND
      *
      *****************************************************************
      *  SET THE INDEX IN THE ERROR MESSAGE AREA (ERROR-IDX) TO THE
      *  NUMBER OF ERRORS COUNTED.
      *****************************************************************
      *
           SET ERROR-IDX TO ERROR-COUNTER.

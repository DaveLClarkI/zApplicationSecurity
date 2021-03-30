      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    IESCNTLP.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  OCTOBER 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      BATCH.
      *REMARKS.       AGE EXPIRED/REVOKED IESCNTL/BSTCNTL RECORDS
      *                               AND VUSERID/IESLDUM RECORDS.

      * CHANGE HISTORY ------------------------------------------------
      * 10/23/2019 DLC ORIGINAL PROGRAM.
      * END OF HISTORY ------------------------------------------------

      /*****************************************************************
      *                                                                *
      *    ENVIRONMENT DIVISION                                        *
      *                                                                *
      ******************************************************************
       ENVIRONMENT DIVISION.

      ******************************************************************
      *    CONFIGURATION SECTION                                       *
      ******************************************************************
       CONFIGURATION SECTION.

       SOURCE-COMPUTER. IBM-2086-A04-140.
       OBJECT-COMPUTER. IBM-2086-A04-140.

       SPECIAL-NAMES.
           SYSLST IS PRINTER,
           UPSI-7 ON STATUS IS WITH-UPDATES.

      ******************************************************************
      *    INPUT-OUTPUT SECTION                                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT IESCNTL
               ASSIGN TO IESCNTL
               ACCESS IS DYNAMIC  INDEXED
               RECORD KEY IS IUI-KEY
               FILE STATUS IS FILE1-STAT
                              FILE1-FDBK.

           SELECT BSTCNTL
               ASSIGN TO BSTCNTL
               ACCESS IS DYNAMIC  INDEXED
               RECORD KEY IS BSM-KEY
               FILE STATUS IS FILE2-STAT
                              FILE2-FDBK.

           SELECT IESLDUV
               ASSIGN TO IESLDUV
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS LDUM-MFUSRID
               FILE STATUS IS FILE3-STAT
                              FILE3-FDBK.

           SELECT VUSERID
               ASSIGN TO VUSERID
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS VUSER-KEY
               FILE STATUS IS FILE4-STAT
                              FILE4-FDBK.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    FILE SECTION                                                *
      ******************************************************************
       FILE SECTION.

       FD  IESCNTL.
       COPY IESCNTL.

       FD  BSTCNTL
           RECORD IS VARYING IN SIZE
             FROM 280 TO 65535 CHARACTERS
             DEPENDING ON BSM-RECL.
       COPY BSTCNTL.

       FD  IESLDUV.
       COPY IESLDUM.

       FD  VUSERID
           RECORD IS VARYING IN SIZE
             FROM 127 TO 174 CHARACTERS.
       COPY VUSERID.
       01  VUSERID-OLD-RECORD          PIC  X(127).

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'IESCNTLP'.

         03  IUI                       PIC S9(04)   BINARY VALUE 1.
         03  BSM                       PIC S9(04)   BINARY VALUE 2.
         03  LDUM                      PIC S9(04)   BINARY VALUE 3.
         03  VUSER                     PIC S9(04)   BINARY VALUE 4.

         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  NAME-LENGTH               PIC S9(04)   BINARY.
         03  BSM-RECL                  PIC  9(5)    VALUE ZEROES.
         03  BSM-SWITCH                PIC  X(01)   VALUE 'N'.
           88  BSM-WAS-CHANGED                      VALUE 'Y'.
           88  BSM-NOT-CHANGED                      VALUE 'N'.

         03  WORK-OPTION               PIC  X(08).
         03  WORK-INDENT               PIC  X(17)   VALUE SPACES.
         03  WORK-USRIDNT              PIC  X(08).
         03  WORK-USRNAME              PIC  X(20).
         03  WORK-USRTYPE              PIC  X(01).
           88  IS-ADMINISTRATOR                     VALUE '1'.
           88  IS-PROGRAMMER                        VALUE '2'.
           88  IS-GENERAL-USER                      VALUE '3'.
         03  WORK-ICCFUSR              PIC  X(01).
           88  IS-ICCF-USER                         VALUE 'Y'.
           88  NOT-ICCF-USER                        VALUE 'N'.
         03  WORK-AGED                 PIC  9(08).
         03  WORK-EXPIRES              PIC  9(08).
         03  WORK-REVOKED              PIC  9(08).
         03  WORK-SIGNON               PIC  9(08).

         03  NUM-JULN                  PIC  9(5).
         03  NUM-DATE                  PIC  9(8).
         03  WS-WRKDATE.
           05  WS-MODATE               PIC  9(02).
           05                          PIC  X.
           05  WS-DADATE               PIC  9(02).
           05                          PIC  X.
           05  WS-YYDATE               PIC  9(02).

         03  USER-COUNT                PIC  9(7)    VALUE ZEROES.
         03  IUI-COUNT                 PIC  9(7)    VALUE ZEROES.
         03  VUSER-COUNT               PIC  9(7)    VALUE ZEROES.
         03  LDUM-COUNT                PIC  9(7)    VALUE ZEROES.
         03  BSM-SEC-COUNT             PIC  9(7)    VALUE ZEROES.
         03  BSM-GRP-COUNT             PIC  9(7)    VALUE ZEROES.

         03  VAR-EDIT                  PIC  Z(6)9-.
         03  VAR-TEXTL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  VAR-TEXT                  PIC  X(120)  VALUE SPACES.

       COPY BITMAN.

       COPY DTEMAN.

       COPY RTCMAN.

       COPY VSMSTATW.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    MAINLINE ROUTINE                                            *
      ******************************************************************
       A00-MAINLINE-ROUTINE.

           PERFORM B10-INITIALIZATION THRU B15-EXIT.

           PERFORM C00-PROCESS-IESCNTL THRU C99-EXIT-PROCESS
             UNTIL STAT-EOFILE(IUI)
                OR RTC-CODE NOT = ZERO.

           PERFORM B20-TERMINATION THRU B25-EXIT.

           DISPLAY ' '               UPON PRINTER.
           DISPLAY ' '               UPON PRINTER.
           MOVE USER-COUNT             TO VAR-EDIT.
           DISPLAY 'Total users found ........ ' VAR-EDIT
                                     UPON PRINTER.
           MOVE IUI-COUNT              TO VAR-EDIT.
           DISPLAY 'IBM Security purged ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE VUSER-COUNT            TO VAR-EDIT.
           DISPLAY 'WIN Security purged ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE LDUM-COUNT             TO VAR-EDIT.
           DISPLAY 'LDAP Mappings removed .... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE BSM-GRP-COUNT          TO VAR-EDIT.
           DISPLAY 'Memberships revoked ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE BSM-SEC-COUNT          TO VAR-EDIT.
           DISPLAY 'Permissions revoked ...... ' VAR-EDIT
                                     UPON PRINTER.

           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           COPY BATCHINI.

           MOVE 'IESCNTL'              TO VSAM-FILE(IUI).
           MOVE 'BSTCNTL'              TO VSAM-FILE(BSM).
           MOVE 'IESLDUV'              TO VSAM-FILE(LDUM).
           MOVE 'VUSERID'              TO VSAM-FILE(VUSER).

           MOVE LENGTH OF IUI-KEY      TO VSAM-KEYL(IUI).
           MOVE LENGTH OF BSM-KEY      TO VSAM-KEYL(BSM).
           MOVE LENGTH OF LDUM-MFUSRID TO VSAM-KEYL(LDUM).
           MOVE LENGTH OF VUSER-KEY    TO VSAM-KEYL(VUSER).

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET FUNC-OPEN(VSUB)     TO TRUE
           END-PERFORM

           IF  WITH-UPDATES
               OPEN I-O   IESCNTL
                          BSTCNTL
                          IESLDUV
                          VUSERID
           ELSE
               OPEN INPUT IESCNTL
                          BSTCNTL
                          IESLDUV
                          VUSERID
           END-IF.

           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.

           IF  RTC-CODE = ZERO
               PERFORM WITH TEST BEFORE
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
                   SET FUNC-READ(VSUB) TO TRUE
               END-PERFORM
           END-IF

           IF  RTC-CODE = ZERO
      * start at user profiles
               SET  IUI-USER-PROFILE   TO TRUE
               MOVE LOW-VALUES         TO IUI-KEY-VALUE
               MOVE IUI                TO VSUB
               SET  FUNC-START(VSUB)   TO TRUE
               START IESCNTL KEY >= IUI-KEY END-START
               IF  NOT STAT-NORMAL(VSUB)
               AND NOT STAT-NOTFND(VSUB)
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               IF  STAT-NOTFND(VSUB)
                   SET STAT-EOFILE(VSUB) TO TRUE
               END-IF

      * get aged date
               SET  DTE-REQUEST-CURRENT-DATE
                                       TO TRUE
               CALL DTEMAN          USING DTEMAN-PARMS
               DISPLAY 'CURRENT=' DTE-CCYYMMDD
                                     UPON PRINTER
               SET  DTE-REQUEST-TOTL-DAYS
                                       TO TRUE
               IF  DTE-LEAP-YEAR
               AND DTE-JUDA > 59
                   SUBTRACT 366      FROM DTE-TOTD
               ELSE
                   SUBTRACT 365      FROM DTE-TOTD
               END-IF
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-GNUM           TO WORK-AGED
               DISPLAY '   AGED=' WORK-AGED
                                     UPON PRINTER
               DISPLAY SPACES        UPON PRINTER
               IF  WITH-UPDATES
                   MOVE 'PURGED:'      TO WORK-OPTION
               ELSE
                   MOVE 'REPORT:'      TO WORK-OPTION
               END-IF
           END-IF.

       B15-EXIT.
           EXIT.

      /*****************************************************************
      *    PROGRAM TERMINATION ROUTINE                                 *
      ******************************************************************
       B20-TERMINATION.

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET  FUNC-CLOSE(VSUB)   TO TRUE
           END-PERFORM.

           CLOSE IESCNTL
                 BSTCNTL
                 IESLDUV
                 VUSERID.

           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.

           IF  RTC-CODE = ZERO
           AND BSM-WAS-CHANGED
               SET RTC-REQUEST-FINISH  TO TRUE
               MOVE +2                 TO RTC-CODE
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT
               MOVE RTC-CODE           TO RETURN-CODE
               GO TO B25-EXIT
           END-IF.

           COPY BATCHRTN.

       B25-EXIT.
           EXIT.

      /*****************************************************************
      *    CHECK A VSAM OR WORK FILE'S STATUS                          *
      ******************************************************************
       COPY VSMSTATP.

      /*****************************************************************
      *    PROGRAM PROCESSING ROUTINES                                 *
      ******************************************************************

       C00-PROCESS-IESCNTL.

           MOVE IUI                    TO VSUB
           SET  FUNC-READNEXT(VSUB)    TO TRUE.
           READ IESCNTL NEXT RECORD END-READ.
           IF  NOT STAT-NORMAL(VSUB)
               IF  NOT STAT-EOFILE(VSUB)
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               GO TO C99-EXIT-PROCESS
           END-IF.

           IF  NOT IUI-USER-PROFILE
               GO TO C00-PROCESS-IESCNTL
           END-IF.

           ADD  1                      TO USER-COUNT.

           PERFORM P10-IMPORT-CONTROL-DATA THRU P15-EXIT.

           IF  IS-ICCF-USER
               GO TO C00-PROCESS-IESCNTL
           END-IF.

           IF  WORK-EXPIRES < WORK-AGED
           OR  WORK-REVOKED < WORK-AGED
               DISPLAY WORK-OPTION WORK-USRIDNT
                             ' - ' WORK-USRNAME
                      ', EXPIRES=' WORK-EXPIRES
                      ', REVOKED=' WORK-REVOKED
                      ', LAST ON=' WORK-SIGNON
                                     UPON PRINTER
               PERFORM D00-PURGE-USER-PROFILE THRU D99-EXIT
           END-IF.

       C99-EXIT-PROCESS.
           EXIT.

       D00-PURGE-USER-PROFILE.
      * purge vse ldap user mapping record
           SET  FUNC-READUPD(LDUM)     TO TRUE.
           MOVE WORK-USRIDNT           TO LDUM-MFUSRID.
           READ IESLDUV RECORD END-READ.
           IF  STAT-NORMAL(LDUM)
               ADD  1                  TO LDUM-COUNT
               DISPLAY WORK-INDENT 'IESLDUM: ' LDUM-KEY
                                     UPON PRINTER
               IF  WITH-UPDATES
                 SET FUNC-DELETE(LDUM) TO TRUE
                 DELETE IESLDUV RECORD END-DELETE
               END-IF
           END-IF.
           IF  NOT STAT-NORMAL(LDUM)
               IF  NOT STAT-NOTFND(LDUM)
                   MOVE LDUM           TO VSUB
                   MOVE LDUM-MFUSRID   TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D99-EXIT
               END-IF
           END-IF.

      * purge winsupply user id extension record
           SET  FUNC-READUPD(VUSER)    TO TRUE.
           MOVE WORK-USRIDNT           TO VUSER-KEY.
           READ VUSERID RECORD END-READ.
           IF  STAT-NORMAL(VUSER)
               ADD  1                  TO VUSER-COUNT
               DISPLAY WORK-INDENT 'VUSERID: ' VUSER-KEY
                                     UPON PRINTER
               IF  WITH-UPDATES
                 SET FUNC-DELETE(VUSER) TO TRUE
                 DELETE VUSERID RECORD END-DELETE
               END-IF
           END-IF.
           IF  NOT STAT-NORMAL(VUSER)
               IF  NOT STAT-NOTFND(VUSER)
                   MOVE VUSER          TO VSUB
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D99-EXIT
               END-IF
           END-IF.

      * purge vse basic security manager permission records
           PERFORM F00-PURGE-FACILITY-PERMISSIONS THRU F99-EXIT.
           IF  RTC-CODE > ZERO
               GO TO D99-EXIT
           END-IF.

           PERFORM G00-PURGE-GROUP-MEMBERSHIP THRU G99-EXIT.
           IF  RTC-CODE > ZERO
               GO TO D99-EXIT
           END-IF.

      * purge vse interactive user interface profile record
           ADD  1                      TO IUI-COUNT.
           IF  WITH-UPDATES
               SET  FUNC-DELETE(IUI)   TO TRUE
               DELETE IESCNTL RECORD END-DELETE
               IF  NOT STAT-NORMAL(IUI)
                   MOVE IUI            TO VSUB
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D99-EXIT
               END-IF
           END-IF.
       D99-EXIT.
           EXIT.

       F00-PURGE-FACILITY-PERMISSIONS.
           MOVE LOW-VALUES             TO BSM-KEY.
           SET  BSM-FACILITY           TO TRUE.

           SET  FUNC-START(BSM)        TO TRUE.
           START BSTCNTL KEY >= BSM-KEY END-START.
           IF  NOT STAT-NORMAL(BSM)
               IF  STAT-NOTFND(BSM)
                   SET STAT-EOFILE(BSM) TO TRUE
               ELSE
                   MOVE BSM            TO VSUB
                   MOVE BSM-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
           END-IF.

           PERFORM WITH TEST BEFORE
             UNTIL STAT-EOFILE(BSM) OR RTC-CODE > ZERO
               SET  FUNC-READNEXT(BSM) TO TRUE
               READ BSTCNTL NEXT RECORD END-READ

               EVALUATE TRUE
               WHEN NOT STAT-NORMAL(VSUB)
                 IF  NOT STAT-EOFILE(VSUB)
                   MOVE BSM            TO VSUB
                   MOVE BSM-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 END-IF

               WHEN BSM-FACILITY
                 PERFORM WITH TEST BEFORE
                   VARYING SUB FROM 1 BY 1
                     UNTIL SUB > BSM-SEC-PERMITS
                        OR BSM-SEC-USERID(SUB) = WORK-USRIDNT
                 END-PERFORM
                 IF  SUB <= BSM-SEC-PERMITS
                 AND BSM-SEC-USERID(SUB) = WORK-USRIDNT
                   ADD  1              TO BSM-SEC-COUNT
                   DISPLAY WORK-INDENT 'BSTCNTL: ' BSM-KEY(1:50)
                                     UPON PRINTER
                   IF  WITH-UPDATES
                     SET BSM-WAS-CHANGED TO TRUE
                     PERFORM WITH TEST BEFORE
                       VARYING SUB FROM SUB BY 1
                         UNTIL SUB >= BSM-SEC-PERMITS
                       MOVE BSM-SEC-PERMIT(SUB + 1)
                                       TO BSM-SEC-PERMIT(SUB)
                     END-PERFORM
                     SUBTRACT 1      FROM BSM-SEC-PERMITS
                     SUBTRACT LENGTH OF BSM-SEC-PERMIT
                                     FROM BSM-RECL
                                          BSM-RECLENG
                     SET FUNC-REWRITE(BSM) TO TRUE
                     REWRITE BSTCNTL-RECORD END-REWRITE
                     IF  NOT STAT-NORMAL(VSUB)
                       MOVE BSM        TO VSUB
                       MOVE BSM-KEY    TO VSAM-KEYD(VSUB)
                       PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                     END-IF
                   END-IF
                 END-IF

               WHEN OTHER
                 SET  STAT-EOFILE(BSM) TO TRUE
               END-EVALUATE
           END-PERFORM.
       F99-EXIT.
           EXIT.

       G00-PURGE-GROUP-MEMBERSHIP.
           MOVE LOW-VALUES             TO BSM-KEY.
           SET  BSM-GROUP              TO TRUE.

           SET  FUNC-START(BSM)        TO TRUE.
           START BSTCNTL KEY >= BSM-KEY END-START.
           IF  NOT STAT-NORMAL(BSM)
               IF  STAT-NOTFND(BSM)
                   SET STAT-EOFILE(BSM) TO TRUE
               ELSE
                   MOVE BSM            TO VSUB
                   MOVE BSM-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
           END-IF.

           PERFORM WITH TEST BEFORE
             UNTIL STAT-EOFILE(BSM) OR RTC-CODE > ZERO
               SET  FUNC-READNEXT(BSM) TO TRUE
               READ BSTCNTL NEXT RECORD END-READ

               EVALUATE TRUE
               WHEN NOT STAT-NORMAL(VSUB)
                 IF  NOT STAT-EOFILE(VSUB)
                   MOVE BSM            TO VSUB
                   MOVE BSM-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                 END-IF

               WHEN BSM-GROUP
                 IF  BSM-GRP-MEMBER = WORK-USRIDNT
                   ADD  1              TO BSM-GRP-COUNT
                   DISPLAY WORK-INDENT 'BSTCNTL: ' BSM-KEY(1:50)
                                     UPON PRINTER
                   IF  WITH-UPDATES
                     SET BSM-WAS-CHANGED TO TRUE
                     SET FUNC-DELETE(BSM) TO TRUE
                     DELETE BSTCNTL RECORD END-DELETE
                     IF  NOT STAT-NORMAL(VSUB)
                       MOVE BSM        TO VSUB
                       MOVE BSM-KEY    TO VSAM-KEYD(VSUB)
                       PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                     END-IF
                   END-IF
                 END-IF

               WHEN OTHER
                 SET  STAT-EOFILE(BSM) TO TRUE
               END-EVALUATE
           END-PERFORM.
       G99-EXIT.
           EXIT.

      /*****************************************************************
      *    OTHER PERFORMED ROUTINES                                    *
      ******************************************************************

       P10-IMPORT-CONTROL-DATA.
           MOVE SPACES                 TO WORK-USRIDNT.
           MOVE 1                      TO POS.
           STRING IUI-US-USRIDNT          DELIMITED BY LOW-VALUE
                                     INTO WORK-USRIDNT.
           SUBTRACT 1 FROM POS     GIVING NAME-LENGTH.

           MOVE IUI-US-USRNAME         TO WORK-USRNAME.

      * extract settings for IUI information
           SET  BIT-DECODE-FROM-BYTE   TO TRUE.
           MOVE IUI-US-IESINFO         TO BIT-BYTE.
           CALL BITMAN              USING BITMAN-PARMS.
           EVALUATE TRUE
             WHEN BIT-0-IS-ON  AND BIT-1-IS-ON
               SET  IS-ADMINISTRATOR   TO TRUE
             WHEN BIT-0-IS-OFF AND BIT-1-IS-ON
               SET  IS-PROGRAMMER      TO TRUE
             WHEN OTHER
               SET  IS-GENERAL-USER    TO TRUE
           END-EVALUATE.

      * extract settings for VSAM security
           SET  BIT-DECODE-FROM-BYTE   TO TRUE.
           MOVE IUI-US-IESVSAM         TO BIT-BYTE.
           CALL BITMAN              USING BITMAN-PARMS.
           IF  BIT-4-IS-ON
               SET  NOT-ICCF-USER      TO TRUE
           ELSE
               SET  IS-ICCF-USER       TO TRUE
           END-IF.

      * extract settings for expiration date
           IF  IUI-US-EXPJULN > ZEROES
           OR  IUI-US-EXPDATE > SPACES
               IF  IUI-US-EXPJULN > ZEROES
                   SET  DTE-REQUEST-JULN-EDIT
                                       TO TRUE
                   MOVE IUI-US-EXPJULN TO NUM-JULN
                   MOVE NUM-JULN       TO DTE-JULN
                   CALL DTEMAN      USING DTEMAN-PARMS
                   MOVE DTE-GNUM       TO WORK-EXPIRES
               ELSE
                   MOVE IUI-US-EXPDATE TO WS-WRKDATE
                   SET  DTE-REQUEST-GREG-EDIT
                                       TO TRUE
                   MOVE SPACES         TO DTE-GREG
                   STRING WS-YYDATE WS-MODATE WS-DADATE
                     DELIMITED BY SIZE INTO DTE-GREG
                   CALL DTEMAN      USING DTEMAN-PARMS
                   MOVE DTE-GNUM       TO WORK-EXPIRES
               END-IF
           ELSE
               MOVE 99999999           TO WORK-EXPIRES
           END-IF.

      * extract settings for revokation date
           IF  IUI-US-REVOKEJ > ZEROES
               SET  DTE-REQUEST-JULN-EDIT
                                       TO TRUE
               MOVE IUI-US-REVOKEJ     TO NUM-JULN
               MOVE NUM-JULN           TO DTE-JULN
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-GNUM           TO WORK-REVOKED
           ELSE
               MOVE 99999999           TO WORK-REVOKED
           END-IF.

      * extract settings for last signon date
           IF  IUI-US-LASTLGN > ZEROES
               SET  DTE-REQUEST-JULN-EDIT
                                       TO TRUE
               MOVE IUI-US-LASTLGN     TO NUM-JULN
               MOVE NUM-JULN           TO DTE-JULN
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-GNUM           TO WORK-SIGNON
           ELSE
               MOVE 99999999           TO WORK-SIGNON
           END-IF.
       P15-EXIT.
           EXIT.


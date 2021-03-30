      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    IESCNTLR.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  MARCH 2020.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      BATCH.
      *REMARKS.       REVOKE IESCNTL USERS DISABLED IN ACTIVE DIRECTORY.
      *               CAN ALSO REVOKE USERS MISSING IN ACTIVE DIRECTORY.
      *               PRODUCES SYSLST REPORT.  OPTIONALLY, SEND EMAIL.

      * CHANGE HISTORY ------------------------------------------------
      * 03/06/2020 DLC ORIGINAL PROGRAM.
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
           UPSI-0 ON STATUS IS SEND-EMAIL,
           UPSI-6 ON STATUS IS REVOKE-MISSING,
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

           SELECT IESLDUV
               ASSIGN TO IESLDUV
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS LDUM-MFUSRID
               FILE STATUS IS FILE2-STAT
                              FILE2-FDBK.

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

       FD  IESLDUV.
       COPY IESLDUM.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(08)   VALUE 'IESCNTLR'.

         03  IESLDGAB                  PIC  X(08)   VALUE 'IESLDGAB'.
         03  MAILWRTR                  PIC  X(08)   VALUE 'MAILWRTB'.
         03  JOBDATA                   PIC  X(08)   VALUE 'JOBDATA'.

         03  IUI                       PIC S9(04)   BINARY VALUE 1.
         03  LDUM                      PIC S9(04)   BINARY VALUE 2.

         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  NAME-LENGTH               PIC S9(04)   BINARY.
         03  DISABLED-SWITCH           PIC  X(01).
           88  USER-DISABLED                        VALUE 'Y'.
           88  USER-ENABLED                         VALUE 'N'.
         03  EMAIL-SWITCH              PIC  X(1)    VALUE 'C'.
           88  EMAIL-CLOSED                         VALUE 'C'.
           88  EMAIL-OPEN                           VALUE 'O'.

         03  WORK-OPTION               PIC  X(08).
         03  WORK-INDENT               PIC  X(19)   VALUE SPACES.
         03  WORK-USRIDNT              PIC  X(08).
         03  WORK-USRNAME              PIC  X(20).
         03  WORK-USRTYPE              PIC  X(01).
           88  IS-ADMINISTRATOR                     VALUE '1'.
           88  IS-PROGRAMMER                        VALUE '2'.
           88  IS-GENERAL-USER                      VALUE '3'.
         03  WORK-ICCFUSR              PIC  X(01).
           88  IS-ICCF-USER                         VALUE 'Y'.
           88  NOT-ICCF-USER                        VALUE 'N'.
         03  WORK-CURRJUL              PIC  9(05).
         03  WORK-CURRENT              PIC  9(08).
         03  WORK-REVOKED              PIC  9(08).
         03  WORK-SIGNON               PIC  9(08).

         03  USER-COUNT                PIC  9(7)    VALUE ZEROES.
         03  REVOKED-COUNT             PIC  9(7)    VALUE ZEROES.
         03  ICCF-USERS                PIC  9(7)    VALUE ZEROES.
         03  VSE-USERS                 PIC  9(7)    VALUE ZEROES.
         03  REPORT-USERS              PIC  9(7)    VALUE ZEROES.
         03  LDAP-ERRORS               PIC  9(7)    VALUE ZEROES.
         03  MISSING-USERS             PIC  9(7)    VALUE ZEROES.
         03  REVOKED-USERS             PIC  9(7)    VALUE ZEROES.

         03  VAR-EDIT                  PIC  Z(6)9-.
         03  VAR-TEXTL                 PIC S9(04)   BINARY VALUE ZEROES.
         03  VAR-TEXT                  PIC  X(120)  VALUE SPACES.

       COPY BINARIES.
       COPY DATETIME.

      * IBM GET LDAP ATTRIBUTE INTERFACE
       01  IESLDGA-COMMAREA.
         03  LDGA-AREA-LENGTH          PIC S9(09)   BINARY.
         03  LDGA-USER-ID              PIC  X(64).
         03  LDGA-SEARCH-FILTER        PIC  X(128).
         03  LDGA-RET-CODE             PIC S9(09)   BINARY.
         03  LDGA-LDAP-CODE            PIC S9(09)   BINARY.
         03  LDGA-ATTR-COUNT           PIC S9(04)   BINARY.
         03  LDGA-ATTR-ENTRY                        OCCURS 2.
           05  LDGA-ATTR-NAME          PIC  X(64).
           05  LDGA-VALUE-LENGTH       PIC S9(04)   BINARY.
           05  LDGA-VALUE-COUNT        PIC S9(04)   BINARY.
           05  LDGA-VALUE-ENTRY                     OCCURS 1.
             07  LDGA-ATTR-VALUE       PIC  X(128).

      * MAINFRAME MAIL INTERFACE
       01  MAILWRTR-COMMAREA.
         COPY MAILWRTR.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       COPY BITMAN.

       COPY DTEMAN.

       COPY JOBDATA.

       COPY NUMMAN.

       COPY RTCMAN.

       COPY TXTMAN.

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
           MOVE ICCF-USERS             TO VAR-EDIT.
           DISPLAY 'ICCF users skipped ....... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE REVOKED-COUNT          TO VAR-EDIT.
           DISPLAY 'Revoked users skipped .... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE VSE-USERS              TO VAR-EDIT.
           DISPLAY 'Unmapped or VSE-only users ' VAR-EDIT
                                     UPON PRINTER.
           MOVE REPORT-USERS           TO VAR-EDIT.
           DISPLAY 'Users checked in AD ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE LDAP-ERRORS            TO VAR-EDIT.
           DISPLAY 'Mapping errors in AD ..... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE MISSING-USERS          TO VAR-EDIT.
           DISPLAY 'Users not found on AD .... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE REVOKED-USERS          TO VAR-EDIT.
           DISPLAY 'Users newly revoked ...... ' VAR-EDIT
                                     UPON PRINTER.

           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           COPY BATCHINI.

           MOVE 'IESCNTL'              TO VSAM-FILE(IUI).
           MOVE 'IESLDUV'              TO VSAM-FILE(LDUM).

           MOVE LENGTH OF IUI-KEY      TO VSAM-KEYL(IUI).
           MOVE LENGTH OF LDUM-MFUSRID TO VSAM-KEYL(LDUM).

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET FUNC-OPEN(VSUB)     TO TRUE
           END-PERFORM

           IF  WITH-UPDATES
               OPEN I-O   IESCNTL
           ELSE
               OPEN INPUT IESCNTL
           END-IF.
           OPEN INPUT IESLDUV.

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

      * get current date for revoke
               SET  DTE-REQUEST-CURRENT-DATE
                                       TO TRUE
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-CCYYMMDD       TO WORK-CURRENT
               MOVE DTE-YYDDD          TO WORK-CURRJUL
               DISPLAY 'CURRENT=' WORK-CURRENT
                                     UPON PRINTER
               DISPLAY SPACES        UPON PRINTER
               IF  WITH-UPDATES
                   MOVE 'UPDATE:'      TO WORK-OPTION
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

           IF  EMAIL-OPEN
               PERFORM R20-CLOSE-EMAIL THRU R25-EXIT
           END-IF.

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET  FUNC-CLOSE(VSUB)   TO TRUE
           END-PERFORM.

           CLOSE IESCNTL
                 IESLDUV.

           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.

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
           AND NAME-LENGTH <= 4
               ADD  1                  TO ICCF-USERS
               GO TO C00-PROCESS-IESCNTL
           END-IF.

           IF  WORK-REVOKED NOT = ZERO
               ADD  1                  TO REVOKED-COUNT
           ELSE
               PERFORM D00-CHECK-ACTIVE-DIRECTORY THRU D99-EXIT
               IF  USER-DISABLED
                   ADD  1              TO REVOKED-USERS
                   DISPLAY WORK-INDENT 'USER=' WORK-USRIDNT
                                       ', REVOKED=' WORK-REVOKED
                                     UPON PRINTER
                   IF  SEND-EMAIL
                     PERFORM R30-SEND-EMAIL THRU R35-EXIT
                     IF  LDGA-RET-CODE NOT = ZERO
                       STRING VAR-TEXT(1:VAR-TEXTL)
                              '<br>'      DELIMITED BY SIZE
                                     INTO MAIL-BODY-TEXT
                       PERFORM R00-CALL-MAILWRTR THRU R05-EXIT
                     END-IF
                   END-IF
               END-IF
           END-IF.

       C99-EXIT-PROCESS.
           EXIT.

       D00-CHECK-ACTIVE-DIRECTORY.
           SET  USER-ENABLED           TO TRUE.

      * get vse ldap user mapping record
           SET  FUNC-READUPD(LDUM)     TO TRUE.
           MOVE WORK-USRIDNT           TO LDUM-MFUSRID.
           READ IESLDUV RECORD END-READ.
           IF  STAT-NOTFND(LDUM)
               ADD  1                  TO VSE-USERS
               GO TO D99-EXIT
           END-IF.
           IF  NOT STAT-NORMAL(LDUM)
               MOVE LDUM               TO VSUB
               MOVE LDUM-MFUSRID       TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               GO TO D99-EXIT
           END-IF.

           ADD  1                      TO REPORT-USERS.

           DISPLAY WORK-OPTION WORK-USRIDNT ' - ' WORK-USRNAME
                                     ', LAST ON=' WORK-SIGNON
                                     UPON PRINTER.

      * check if user disabled in active directory
           PERFORM P20-GET-SECURITY-ATTRIBUTES THRU P25-EXIT.
           IF  USER-ENABLED
               GO TO D99-EXIT
           END-IF.
           MOVE WORK-CURRENT           TO WORK-REVOKED.

      * purge vse interactive user interface profile record
           IF  WITH-UPDATES
               MOVE WORK-CURRJUL       TO IUI-US-REVOKEJ
               SET  FUNC-REWRITE(IUI)  TO TRUE
               REWRITE IESCNTL-RECORD END-REWRITE
               IF  NOT STAT-NORMAL(IUI)
                   MOVE IUI            TO VSUB
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
                   GO TO D99-EXIT
               END-IF
           END-IF.
       D99-EXIT.
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

      * extract settings for revokation date
           IF  IUI-US-REVOKEJ > ZEROES
               SET  DTE-REQUEST-JULN-EDIT
                                       TO TRUE
               MOVE IUI-US-REVOKEJ     TO JUL-DATE
               MOVE JUL-DATE           TO DTE-JULN
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-GNUM           TO WORK-REVOKED
           ELSE
               MOVE ZEROES             TO WORK-REVOKED
           END-IF.

      * extract settings for last signon date
           IF  IUI-US-LASTLGN > ZEROES
               SET  DTE-REQUEST-JULN-EDIT
                                       TO TRUE
               MOVE IUI-US-LASTLGN     TO JUL-DATE
               MOVE JUL-DATE           TO DTE-JULN
               CALL DTEMAN          USING DTEMAN-PARMS
               MOVE DTE-GNUM           TO WORK-SIGNON
           ELSE
               MOVE ZEROES             TO WORK-SIGNON
           END-IF.
       P15-EXIT.
           EXIT.

       P20-GET-SECURITY-ATTRIBUTES.
           INITIALIZE IESLDGA-COMMAREA.
           MOVE LENGTH OF IESLDGA-COMMAREA
                                       TO LDGA-AREA-LENGTH.
           MOVE LDUM-NETUSRID          TO LDGA-USER-ID.
           MOVE '&(objectClass=person)(objectClass=user)'
                                       TO LDGA-SEARCH-FILTER.
           MOVE 1                      TO LDGA-ATTR-COUNT.

           MOVE 'userAccountControl'   TO LDGA-ATTR-NAME(1).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(1).
           MOVE 1                      TO LDGA-VALUE-COUNT(1).

           CALL IESLDGAB            USING IESLDGA-COMMAREA.

           IF  LDGA-RET-CODE NOT = ZERO
               IF  LDGA-RET-CODE = 9
               AND LDGA-LDAP-CODE = ZERO
                   ADD  1              TO MISSING-USERS
                   IF  REVOKE-MISSING
                     SET USER-DISABLED TO TRUE
                   END-IF
               ELSE
                   ADD  1              TO LDAP-ERRORS
               END-IF

               MOVE 1                  TO VAR-TEXTL
               MOVE SPACES             TO VAR-TEXT
               STRING WORK-INDENT 'IESLDGA: ' DELIMITED BY SIZE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
               MOVE LDGA-RET-CODE      TO VAR-EDIT
               PERFORM P30-JUSTIFY-LEFT THRU P35-EXIT
               STRING 'LDAP error rc='    DELIMITED BY SIZE
                      VAR-EDIT            DELIMITED BY SPACE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
               MOVE LDGA-LDAP-CODE     TO VAR-EDIT
               PERFORM P30-JUSTIFY-LEFT THRU P35-EXIT
               STRING ',rs='              DELIMITED BY SIZE
                      VAR-EDIT            DELIMITED BY SPACE
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL
               SUBTRACT 1            FROM VAR-TEXTL
               DISPLAY VAR-TEXT(1:VAR-TEXTL)
                                     UPON PRINTER
               GO TO P25-EXIT
           END-IF.

           SET NUM-REQUEST-IGNORE-FORMAT TO TRUE.
           MOVE 10                     TO NUM-TOTL.
           MOVE +0                     TO NUM-DECM.
           MOVE LDGA-ATTR-VALUE(1 1)   TO NUM-DATA.
           CALL NUMMAN              USING NUMMAN-PARMS.
           IF  NOT NUM-REQUEST-COMPLETED
               DISPLAY WORK-INDENT '  NUMMAN: ERROR = ' NUM-RETN
                                   ', DATA = ' LDGA-ATTR-VALUE(1 1)
                                     UPON PRINTER
               GO TO P25-EXIT
           END-IF.

           MOVE NUM-T10-D0             TO FULL-WORD.

           SET  BIT-DECODE-FROM-BYTE   TO TRUE.
           MOVE ONE-BYTE               TO BIT-BYTE.
           CALL BITMAN              USING BITMAN-PARMS.
           IF  NOT BIT-REQUEST-COMPLETED
               DISPLAY WORK-INDENT '  BITMAN: ERROR = ' BIT-RETN
                                   ', BITS = ' BIT-DATA
                                     UPON PRINTER
               GO TO P25-EXIT
           END-IF.

           IF  BIT-6-IS-ON
               SET  USER-DISABLED      TO TRUE
           END-IF.
       P25-EXIT.
           EXIT.

       P30-JUSTIFY-LEFT.
           SET TXT-REQUEST-JUSTIFY-L TO TRUE
           MOVE +1                     TO TXT-PNTR
           MOVE LOW-VALUES             TO TXT-DLMS
           MOVE LENGTH OF VAR-EDIT     TO TXT-PARM-STRLEN
           MOVE VAR-EDIT               TO TXT-PARM-STRING
           CALL TXTMAN              USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING.
           MOVE TXT-PARM-STRING        TO VAR-EDIT.
       P35-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       R00-CALL-MAILWRTR.
           CALL MAILWRTR            USING MAILWRTR-COMMAREA.
           MOVE SPACES                 TO MAIL-PARMS.
       R05-EXIT.
           EXIT.

       R10-OPEN-EMAIL.
           INITIALIZE MAILWRTR-COMMAREA.
           SET  MAIL-HEAD-PARMS        TO TRUE.
           MOVE 'INFORMATION_SECURITY' TO MAIL-HEAD-TO-GRP.
           MOVE '(IESCNTLR) REVOKED USERS DISABLED IN AD'
                                       TO MAIL-HEAD-SUBJECT.
           MOVE 'SY'                   TO MAIL-HEAD-SYSTEM.
           SET  MAIL-DELIVERY-ON       TO TRUE.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           SET  MAIL-DIST-PARMS        TO TRUE.
           MOVE 'MAINFRAME_PROGRAMMING'
                                       TO MAIL-DIST-TO-GRP.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           SET  MAIL-BODY-PARMS        TO TRUE.
           MOVE '<pre><br>'            TO MAIL-BODY-TEXT.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           MOVE FUNCTION CURRENT-DATE  TO WRK-DATETIME.
           MOVE CORR ISO-DATE          TO USA-DATE.
           MOVE      NUM-DATE          TO USA-DSP-DATE.
           MOVE      NUM-TIME          TO ISO-DSP-TIME.
           INSPECT ISO-DSP-TIME REPLACING ALL SPACES BY ':'.

           CALL JOBDATA             USING JOBDATA-PARMS.
           STRING 'JOB: '                 DELIMITED BY SIZE
                  JOBD-PWR-NAME           DELIMITED BY SPACE
                  ' #'                    DELIMITED BY SIZE
                  JOBD-PWR-NUMB           DELIMITED BY SPACE
                  ' PGM: '                DELIMITED BY SIZE
                  JOBD-EXEC-PGM           DELIMITED BY SPACE
                  ' DATE: '               DELIMITED BY SIZE
                  USA-DSP-DATE            DELIMITED BY SPACE
                  ' TIME: '               DELIMITED BY SIZE
                  ISO-DSP-TIME            DELIMITED BY SPACE
                  '<br>'
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           MOVE '<br>'                 TO MAIL-BODY-TEXT.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           SET  EMAIL-OPEN             TO TRUE.
       R15-EXIT.
           EXIT.

       R20-CLOSE-EMAIL.
           MOVE '</pre>'               TO MAIL-BODY-TEXT.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           SET  MAIL-MESG-COMPLETE     TO TRUE.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.

           SET  EMAIL-CLOSED           TO TRUE.
       R25-EXIT.
           EXIT.

       R30-SEND-EMAIL.
           IF  NOT EMAIL-OPEN
               PERFORM R10-OPEN-EMAIL THRU R15-EXIT
           END-IF.

           STRING WORK-OPTION  WORK-USRIDNT
                  ' - '        WORK-USRNAME
                  ', LAST ON=' WORK-SIGNON
                  ', REVOKED=' WORK-REVOKED
                  '<br>'
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.
           PERFORM R00-CALL-MAILWRTR THRU R05-EXIT.
       R35-EXIT.
           EXIT.


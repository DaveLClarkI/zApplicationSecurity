      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    IESCNTLO.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  MARCH 2021.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      BATCH.
      *REMARKS.       IDENTIFY AND PURGE BSTCNTL ORPHANS.

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

       SOURCE-COMPUTER. IBM-2818-Y02.
       OBJECT-COMPUTER. IBM-2086-A04-140.

       SPECIAL-NAMES.
           SYSLST IS PRINTER,
           UPSI-7 ON STATUS IS WITH-UPDATES.

      ******************************************************************
      *    INPUT-OUTPUT SECTION                                        *
      ******************************************************************
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT BSTCNTL-DRIVER
               ASSIGN TO BSTCNTL
               ACCESS IS DYNAMIC  INDEXED
               RECORD KEY IS DRV-KEY
               FILE STATUS IS FILE1-STAT
                              FILE1-FDBK.

           SELECT BSTCNTL
               ASSIGN TO BSTCNTL
               ACCESS IS DYNAMIC  INDEXED
               RECORD KEY IS BSM-KEY
               FILE STATUS IS FILE2-STAT
                              FILE2-FDBK.

           SELECT IESCNTL
               ASSIGN TO IESCNTL
               ACCESS IS RANDOM   INDEXED
               RECORD KEY IS IUI-KEY
               FILE STATUS IS FILE3-STAT
                              FILE3-FDBK.

           SELECT IESLDUV
               ASSIGN TO IESLDUV
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS LDUM-MFUSRID
               FILE STATUS IS FILE4-STAT
                              FILE4-FDBK.

           SELECT VUSERID
               ASSIGN TO VUSERID
               ACCESS IS RANDOM  INDEXED
               RECORD KEY IS VUSER-KEY
               FILE STATUS IS FILE5-STAT
                              FILE5-FDBK.

           SELECT SORT-WKFILE
               ASSIGN TO SYS001-UT-SORTWK1.

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

       FD  BSTCNTL-DRIVER
           RECORD IS VARYING IN SIZE
             FROM 280 TO 65535 CHARACTERS
             DEPENDING ON DRVR-RECL.
       COPY BSTCNTL REPLACING BSTCNTL-RECORD      BY DRIVER-RECORD
                              BSM-PFX             BY DRV-PFX
                              BSM-RECLENG         BY DRV-RECLENG
                              BSM-VERSION         BY DRV-VERSION
                              BSM-MODLEVL         BY DRV-MODLEVL
                              BSM-KEY             BY DRV-KEY
                              BSM-CLASS           BY DRV-CLASS
                              BSM-APPL            BY DRV-APPL
                              BSM-CICS-PCT        BY DRV-CICS-PCT
                              BSM-CICS-DCT        BY DRV-CICS-DCT
                              BSM-CICS-FCT        BY DRV-CICS-FCT
                              BSM-CICS-JCT        BY DRV-CICS-JCT
                              BSM-CICS-PPT        BY DRV-CICS-PPT
                              BSM-CICS-TST        BY DRV-CICS-TST
                              BSM-TRANS           BY DRV-TRANS
                              BSM-FACILITY        BY DRV-FACILITY
                              BSM-GROUP           BY DRV-GROUP
                              BSM-RESOURCE        BY DRV-RESOURCE
                              BSM-SEQUENCE        BY DRV-SEQUENCE
                              BSM-KEYVALEN        BY DRV-KEYVALEN
                              BSM-RSRCDESC        BY DRV-RSRCDESC
                              BSM-DATA            BY DRV-DATA
                              BSM-SECURITY-RECORD BY DRV-SECURITY-RECORD
                              BSM-SEC-GENERIC     BY DRV-SEC-GENERIC
                              BSM-SEC-UA-FLAG     BY DRV-SEC-UA-FLAG
                              BSM-SEC-UA-DENIED   BY DRV-SEC-UA-DENIED
                              BSM-SEC-UA-READ     BY DRV-SEC-UA-READ
                              BSM-SEC-UA-UPDATE   BY DRV-SEC-UA-UPDATE
                              BSM-SEC-UA-ALTER    BY DRV-SEC-UA-ALTER
                              BSM-SEC-LOGGING1    BY DRV-SEC-LOGGING1
                              BSM-SEC-LOGGING2    BY DRV-SEC-LOGGING2
                              BSM-SEC-FILLER      BY DRV-SEC-FILLER
                              BSM-SEC-PERMITS     BY DRV-SEC-PERMITS
                              BSM-SEC-PERMIT      BY DRV-SEC-PERMIT
                              BSM-SEC-USERID      BY DRV-SEC-USERID
                              BSM-SEC-AC-FLAG     BY DRV-SEC-AC-FLAG
                              BSM-SEC-AC-DENIED   BY DRV-SEC-AC-DENIED
                              BSM-SEC-AC-READ     BY DRV-SEC-AC-READ
                              BSM-SEC-AC-UPDATE   BY DRV-SEC-AC-UPDATE
                              BSM-SEC-AC-ALTER    BY DRV-SEC-AC-ALTER
                              BSM-GROUP-RECORD    BY DRV-GROUP-RECORD
                              BSM-GRP-NAME        BY DRV-GRP-NAME
                              BSM-GRP-MEMBER      BY DRV-GRP-MEMBER.

       FD  BSTCNTL
           RECORD IS VARYING IN SIZE
             FROM 280 TO 65535 CHARACTERS
             DEPENDING ON BSM-RECL.
       COPY BSTCNTL.

       FD  IESCNTL.
       COPY IESCNTL.

       FD  IESLDUV.
       COPY IESLDUM.

       FD  VUSERID
           RECORD IS VARYING IN SIZE
             FROM 127 TO 174 CHARACTERS.
       COPY VUSERID.
       01  VUSERID-OLD-RECORD          PIC  X(127).

       SD  SORT-WKFILE.
       01  SORT-RECORD.
         03  SORT-KEY                  PIC  X(8).

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  WS-FIELDS.
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.
         03  THIS-PGM                  PIC  X(8)    VALUE 'IESCNTLO'.

         03  DRVR                      PIC S9(04)   BINARY VALUE 1.
         03  BSM                       PIC S9(04)   BINARY VALUE 2.
         03  IUI                       PIC S9(04)   BINARY VALUE 3.
         03  LDUM                      PIC S9(04)   BINARY VALUE 4.
         03  VUSER                     PIC S9(04)   BINARY VALUE 5.

         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  NAME-LENGTH               PIC S9(04)   BINARY.
         03  DRVR-RECL                 PIC  9(5)    VALUE ZEROES.
         03  BSM-RECL                  PIC  9(5)    VALUE ZEROES.
         03  BSM-SWITCH                PIC  X(01)   VALUE 'N'.
           88  BSM-WAS-CHANGED                      VALUE 'Y'.
           88  BSM-NOT-CHANGED                      VALUE 'N'.
         03  SORT-SWITCH               PIC  A(1)    VALUE 'M'.
           88  MORE-SORT-RECORDS                    VALUE 'M'.
           88  SORT-AT-END                          VALUE 'E'.
         03  RELEASE-SWITCH            PIC  A(1)    VALUE 'Y'.
           88  RELEASE-TO-SORT                      VALUE 'Y'.
           88  DO-NOT-RELEASE                       VALUE 'N'.

         03  SORT-OPTIONS              PIC  X(8)    VALUE 'SORTOPTD'.
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
         03  GROUP-HEADER.
           05                          PIC  X(2)    VALUE X'0000'.
           05                          PIC  X(6)    VALUE 'GrpUID'.

         03  USER-COUNT                PIC  9(7)    VALUE ZEROES.
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

           SORT SORT-WKFILE
                       ON ASCENDING KEY SORT-KEY
                INPUT  PROCEDURE IS
                       C00-PROCESS-GROUPS THRU C99-EXIT-PROCESS
                OUTPUT PROCEDURE IS
                       E00-PURGE-USER-PROFILE THRU E99-EXIT.

           PERFORM B20-TERMINATION THRU B25-EXIT.

           DISPLAY ' '               UPON PRINTER.
           DISPLAY ' '               UPON PRINTER.
           MOVE USER-COUNT             TO VAR-EDIT.
           DISPLAY 'Total orphans found ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE BSM-GRP-COUNT          TO VAR-EDIT.
           DISPLAY 'Memberships revoked ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE BSM-SEC-COUNT          TO VAR-EDIT.
           DISPLAY 'Permissions revoked ...... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE LDUM-COUNT             TO VAR-EDIT.
           DISPLAY 'LDAP Mappings removed .... ' VAR-EDIT
                                     UPON PRINTER.
           MOVE VUSER-COUNT            TO VAR-EDIT.
           DISPLAY 'WIN Security purged ...... ' VAR-EDIT
                                     UPON PRINTER.

           GOBACK.

      /*****************************************************************
      *    PROGRAM INITIALIZATION ROUTINE                              *
      ******************************************************************
       B10-INITIALIZATION.

           COPY BATCHINI.

           MOVE 'BSTCNTL'              TO VSAM-FILE(DRVR)
                                          VSAM-FILE(BSM).
           MOVE 'IESCNTL'              TO VSAM-FILE(IUI).
           MOVE 'IESLDUV'              TO VSAM-FILE(LDUM).
           MOVE 'VUSERID'              TO VSAM-FILE(VUSER).

           MOVE LENGTH OF BSM-KEY      TO VSAM-KEYL(DRVR)
                                          VSAM-KEYL(BSM).
           MOVE LENGTH OF IUI-KEY      TO VSAM-KEYL(IUI).
           MOVE LENGTH OF LDUM-MFUSRID TO VSAM-KEYL(LDUM).
           MOVE LENGTH OF VUSER-KEY    TO VSAM-KEYL(VUSER).

           PERFORM WITH TEST BEFORE
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL
               SET FUNC-OPEN(VSUB)     TO TRUE
           END-PERFORM

           OPEN INPUT BSTCNTL-DRIVER
                      IESCNTL.
           IF  WITH-UPDATES
               OPEN I-O   BSTCNTL
                          IESLDUV
                          VUSERID
           ELSE
               OPEN INPUT BSTCNTL
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
      * start at bsm group definitons
               MOVE LOW-VALUES         TO DRV-KEY
               SET  DRV-GROUP          TO TRUE
               SET  FUNC-START(DRVR)   TO TRUE
               START BSTCNTL-DRIVER KEY >= DRV-KEY END-START
               IF  NOT STAT-NORMAL(DRVR)
               AND NOT STAT-NOTFND(DRVR)
                   MOVE DRVR           TO VSUB
                   MOVE DRV-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               IF  STAT-NOTFND(DRVR)
                   SET STAT-EOFILE(DRVR) TO TRUE
               END-IF

               DISPLAY SPACES        UPON PRINTER
               IF  WITH-UPDATES
                   MOVE 'PURGED:'      TO WORK-OPTION
               ELSE
                   MOVE 'REPORT:'      TO WORK-OPTION
               END-IF
               MOVE SORT-OPTIONS       TO SORT-CONTROL
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

           CLOSE BSTCNTL-DRIVER
                 IESCNTL
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

       C00-PROCESS-GROUPS.

           SET  FUNC-READNEXT(DRVR)    TO TRUE.
           READ BSTCNTL-DRIVER NEXT RECORD END-READ.
           IF  NOT STAT-NORMAL(DRVR)
               IF  NOT STAT-EOFILE(DRVR)
                   MOVE DRVR           TO VSUB
                   MOVE DRV-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               GO TO C99-EXIT-PROCESS
           END-IF.

           IF  NOT DRV-GROUP
      * now start at bsm facility definitons
               MOVE LOW-VALUES         TO DRV-KEY
               SET  DRV-FACILITY       TO TRUE
               SET  FUNC-START(DRVR)   TO TRUE
               START BSTCNTL-DRIVER KEY >= DRV-KEY END-START
               IF  NOT STAT-NORMAL(DRVR)
               AND NOT STAT-NOTFND(DRVR)
                   MOVE DRVR           TO VSUB
                   MOVE DRV-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               IF  STAT-NOTFND(DRVR)
                   SET STAT-EOFILE(DRVR) TO TRUE
                   GO TO C99-EXIT-PROCESS
               END-IF
               GO TO C50-PROCESS-SECURITY
           END-IF.

           IF  DRV-GRP-MEMBER = GROUP-HEADER
               GO TO C00-PROCESS-GROUPS
           END-IF.

           MOVE DRV-GRP-MEMBER         TO WORK-USRIDNT
                                          SORT-KEY.

           PERFORM D20-CHECK-USER-PROFILE THRU D25-EXIT.
           IF  STAT-NOTFND(IUI)
               RELEASE SORT-RECORD
           END-IF.

           IF  RTC-CODE NOT = ZERO
               GO TO C99-EXIT-PROCESS
           END-IF.
           GO TO C00-PROCESS-GROUPS.

       C50-PROCESS-SECURITY.

           SET  FUNC-READNEXT(DRVR)    TO TRUE.
           READ BSTCNTL-DRIVER NEXT RECORD END-READ.
           IF  NOT STAT-NORMAL(DRVR)
               IF  NOT STAT-EOFILE(DRVR)
                   MOVE DRVR           TO VSUB
                   MOVE DRV-KEY        TO VSAM-KEYD(VSUB)
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
               END-IF
               GO TO C99-EXIT-PROCESS
           END-IF.

           IF  NOT DRV-FACILITY
      * processing completed
               SET STAT-EOFILE(DRVR)   TO TRUE
               MOVE SPACES             TO WORK-USRIDNT
               GO TO C99-EXIT-PROCESS
           END-IF.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > DRV-SEC-PERMITS
                  OR RTC-CODE NOT = ZERO
               MOVE DRV-SEC-USERID(IDX) TO WORK-USRIDNT
                                           SORT-KEY
               PERFORM D10-CHECK-BSM-GROUP THRU D15-EXIT
               IF  STAT-NOTFND(BSM)
                   PERFORM D20-CHECK-USER-PROFILE THRU D25-EXIT
                   IF  STAT-NOTFND(IUI)
                       RELEASE SORT-RECORD
                   END-IF
               END-IF
           END-PERFORM.

           IF  RTC-CODE NOT = ZERO
               GO TO C99-EXIT-PROCESS
           END-IF.
           GO TO C50-PROCESS-SECURITY.

       C99-EXIT-PROCESS.
           EXIT.

       D10-CHECK-BSM-GROUP.
           SET  FUNC-READ(BSM)         TO TRUE.
           MOVE LOW-VALUES             TO BSM-KEY.
           SET  BSM-GROUP              TO TRUE.
           MOVE SPACES                 TO BSM-RESOURCE.
           MOVE WORK-USRIDNT           TO BSM-GRP-NAME.
           MOVE GROUP-HEADER           TO BSM-GRP-MEMBER.
           READ BSTCNTL RECORD END-READ.
           IF  NOT STAT-NORMAL(BSM)
           AND NOT STAT-NOTFND(BSM)
               MOVE BSM                TO VSUB
               MOVE BSM-KEY            TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.
       D15-EXIT.
           EXIT.

       D20-CHECK-USER-PROFILE.
           SET  FUNC-READ(IUI)         TO TRUE.
           SET  IUI-USER-PROFILE       TO TRUE.
           MOVE LOW-VALUES             TO IUI-KEY-VALUE.
           STRING WORK-USRIDNT            DELIMITED BY SPACE
                                     INTO IUI-US-USRIDNT.
           READ IESCNTL RECORD END-READ.
           IF  NOT STAT-NORMAL(IUI)
           AND NOT STAT-NOTFND(IUI)
               MOVE IUI                TO VSUB
               MOVE IUI-KEY            TO VSAM-KEYD(VSUB)
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK
           END-IF.
       D25-EXIT.
           EXIT.

       E00-PURGE-USER-PROFILE.
           IF  RTC-CODE NOT = ZERO
               GO TO E99-EXIT
           END-IF.

           RETURN SORT-WKFILE RECORD
               AT END GO TO E99-EXIT
           END-RETURN.
           IF  SORT-KEY = WORK-USRIDNT
               GO TO E00-PURGE-USER-PROFILE
           END-IF.
           MOVE SORT-KEY               TO WORK-USRIDNT.

           ADD  1                      TO USER-COUNT.
           DISPLAY WORK-OPTION WORK-USRIDNT
                                     UPON PRINTER.

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
                   GO TO E99-EXIT
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
                   GO TO E99-EXIT
               END-IF
           END-IF.

      * purge vse basic security manager permission records
           PERFORM F00-PURGE-FACILITY-PERMISSIONS THRU F99-EXIT.
           IF  RTC-CODE > ZERO
               GO TO E99-EXIT
           END-IF.

           PERFORM G00-PURGE-GROUP-MEMBERSHIP THRU G99-EXIT.
           IF  RTC-CODE > ZERO
               GO TO E99-EXIT
           END-IF.

      * more sort records?
           IF  MORE-SORT-RECORDS
           AND RTC-CODE = ZERO
               GO TO E00-PURGE-USER-PROFILE
           END-IF.
       E99-EXIT.
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


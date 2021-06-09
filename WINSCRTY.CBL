      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    WINSCRTY.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  DECEMBER 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       WINSUPPLY CICS SECURITY CHECK PROGRAM.

      * CHANGE HISTORY ------------------------------------------------
      * 12/26/2019 DLC COMPLETE REWRITE FROM ORIGINAL DAPSCRTY PROGRAM.
/DLC0/* 06/05/2020 DLC ALLOW AL'S ACCESS TO THEIR OWN HOME COMPANY.
/DLC1/* 12/15/2020 DLC CONVERT GETMAIN TO WORKING STORAGE TO PREVENT
/DLC1/*                CICS SHORT-ON-STORAGE CONDITIONS DUE TO LINK.
/DLC2/* 03/05/2021 DLC REPORT REMOTE CARRIED IN GROUPS FILE BY REGION.
/DLC3/* 04/16/2021 DLC FIX LOGIC ERROR ON LENGTH ERROR.
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
       OBJECT-COMPUTER. IBM-2096-N03.

      /*****************************************************************
      *                                                                *
      *    DATA DIVISION                                               *
      *                                                                *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *    WORKING-STORAGE SECTION                                     *
      ******************************************************************
       WORKING-STORAGE SECTION.

       01  CONTROL-FIELDS.
      *
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES
         COPY COMMWORK       REPLACING =='PROGRAM'== BY =='WINSCRTY'==.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.
         03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.
         03  COMASTR                   PIC  X(08)   VALUE 'COMASTR '.
         03  COMASTA                   PIC  X(08)   VALUE 'COMASTA '.
         03  COMASTW                   PIC  X(08)   VALUE 'COMASTW '.
         03  GROUPS                    PIC  X(08)   VALUE 'GROUPS  '.
         03  BSTCNTL                   PIC  X(08)   VALUE 'BSTCNTL '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  VSECSERV                  PIC  X(08)   VALUE 'VSECSERV'.
      *
         03  COMM-ADDR                              POINTER.
         03  COMM-OFFS       REDEFINES COMM-ADDR
                                       PIC S9(08)   BINARY.
         03  FLD-ADDR                               POINTER.
         03  FLD-OFFS        REDEFINES FLD-ADDR
                                       PIC S9(08)   BINARY.
         03  BSM-RECL                  PIC S9(04)   BINARY.
/DLC3/   03  IUI-RECL                  PIC S9(04)   BINARY.

         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.

         03  USER-ID                   PIC  X(08)   VALUE SPACES.
         03  CNTL-OPRIDNT              PIC  X(03).
         03  CNTL-USRNAME              PIC  X(20).
         03  CNTL-EXPIRED              PIC  X(08).
         03  CNTL-REVOKED              PIC  X(08).

         03  DFT-GROUP                 PIC  X(02)   VALUE 'PC'.
         03  DFT-SVCRGN                PIC  X(02)   VALUE '01'.
         03  DFT-REMOTE                PIC  X(03)   VALUE '088'.
         03  DFT-LCLPTR                PIC  X(04)   VALUE 'DI9P'.

         03  HOME-SVCRGN               PIC  X(02)   VALUE SPACES.
         03  HOME-AREA                 PIC  X(03)   VALUE SPACES.
         03  HOME-CONUM                PIC  X(05)   VALUE SPACES.

         03  SAVE-CONUM                PIC  X(05)   VALUE SPACES.
         03  SAVE-LEVEL                PIC  X(01)   VALUE SPACES.
         03  SAVE-GROUP                PIC  X(02)   VALUE SPACES.
         03  SAVE-SVCRGN               PIC  X(02)   VALUE SPACES.
         03  SAVE-ALTRGN               PIC  X(02)   VALUE SPACES.
         03  SAVE-RPTRMT               PIC  X(03)   VALUE SPACES.
         03  SAVE-ALTRMT               PIC  X(03)   VALUE SPACES.
         03  SAVE-DFTPTR               PIC  X(04)   VALUE SPACES.

         03  LOOKUP-SWITCH             PIC  X(01)   VALUE 'N'.
           88  LOOKUP-FAILED                        VALUE 'N'.
           88  LOOKUP-SUCCEEDED                     VALUE 'Y'.
         03  COMPANY-SWITCH            PIC  X(01)   VALUE 'N'.
           88  COMPANY-NOT-FOUND                    VALUE 'N'.
           88  COMPANY-WAS-FOUND                    VALUE 'Y'.
         03  AREA-SWITCH               PIC  X(01)   VALUE 'N'.
           88  CHECK-COMP-ACCESS                    VALUE 'N'.
           88  CHECK-AREA-ACCESS                    VALUE 'Y'.
         03  VSEC-RESOURCE-ID          PIC  X(39).
      *
      * TO CONVERT DATES AND TIMES
         03  NUM-DATE                  PIC  9(8).
         03  WRK-DATE                  REDEFINES    NUM-DATE.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYR                PIC  9999.
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.
         03  WS-WRKDATE.
           05  WS-MODATE               PIC  9(02).
           05                          PIC  X.
           05  WS-DADATE               PIC  9(02).
           05                          PIC  X.
           05  WS-CYDATE               PIC  9(04).
         03  NUM-DATE6                 PIC  9(6).
         03  WRK-DATE6                 REDEFINES    NUM-DATE6.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYY                PIC  99.
         03  EDT-DATE                  PIC  99/99/99.
         03  EDT-DATE-X                REDEFINES    EDT-DATE.
           05  EDT-YY                  PIC  9(02).
           05                          PIC  X(01).
           05  EDT-MM                  PIC  9(02).
           05                          PIC  X(01).
           05  EDT-DD                  PIC  9(02).
         03  WS-WRKTIME.
           05  WS-HRSTIME              PIC  9(02).
           05                          PIC  X(03).
           05  WS-XXXTIME              PIC  X(03).

       01  DSYS-RECORD.                                                           <CPY
         03  DSYS-KEY.                                                            <CPY
           05  DSYS-KEY-SYSTEM         PIC  X(3).                                 <CPY
           05  DSYS-KEY-QUAL           PIC  X(17).                                <CPY
         03  DSYS-DATA                 PIC  X(230).                               <CPY

       01  TRNSMITC-RECORD.                                                       <CPY
         COPY TRNSMITC.                                                           <CPY

/DLC1/* IBM VSE Control file record layout
/DLC1/ COPY IESCNTL.

/DLC1/* WIN User Id file record layout
/DLC1/ COPY VUSERID.

/DLC1/* WIN Company master file record layout
/DLC1/ COPY COMASTR.

/DLC1/* WIN Company Groups file record layout
/DLC1/ COPY GROUPS.

/DLC1/* IBM BSM CONTROL FILE RECORD LAYOUT
/DLC1/ COPY BSTCNTL.

      * APPLICATION SECURITY CONTROL AREAS
       01  VSECSERV-COMMAREA.
         COPY VSECSERV.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-CONTROL.
        02  CICSINFO                   PIC  X(08)   VALUE 'CICSINFO'.
        02  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY LOGGING.

       COPY UNEXERRW.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
         COPY WINSCRTY.

/DLC1/* IBM VSE Control file record layout
/DLC1/*COPY IESCNTL.

/DLC1/* WIN User Id file record layout
/DLC1/*COPY VUSERID.

/DLC1/* WIN Company master file record layout
/DLC1/*COPY COMASTR.

/DLC1/* WIN Company Groups file record layout
/DLC1/*COPY GROUPS.

/DLC1/* IBM BSM CONTROL FILE RECORD LAYOUT
/DLC1/*COPY BSTCNTL.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    MAINLINE CODE                                               *
      ******************************************************************

       A00-SCRTY-PROGRAM-ENTRY.

           SET  DAPL-LOGGING           TO TRUE.

      * communication area large enough?
           IF  EIBCALEN < (LENGTH OF SCRTY-REQUEST
                         + LENGTH OF SCRTY-ACCESS)
               EXEC CICS ABEND ABCODE('CALN') END-EXEC
           END-IF.

      * initialize communication response area (except for oldlvl '$')
           SET  COMM-ADDR              TO ADDRESS OF DFHCOMMAREA.
           SET  FLD-ADDR               TO ADDRESS OF SCRTY-OLDLVL.
           COMPUTE LEN = FLD-OFFS - COMM-OFFS.
           IF  EIBCALEN > LEN
               COMPUTE LEN = LEN - LENGTH OF SCRTY-REQUEST
           ELSE
               COMPUTE LEN = EIBCALEN - LENGTH OF SCRTY-REQUEST
           END-IF.
           MOVE SPACES                 TO SCRTY-RESPONSE(1:LEN).

      * retrieve general CICS information
           EXEC CICS LINK
                     PROGRAM  (CICSINFO)
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.
           MOVE CICS-FULLDATE          TO WS-WRKDATE.
           MOVE CICS-FULLTIME          TO WS-WRKTIME.
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.

      * validate user id security information
           IF  SCRTY-USERID > SPACES
               MOVE SCRTY-USERID       TO USER-ID
           ELSE
               MOVE CICS-USERID        TO USER-ID
           END-IF.

           PERFORM Q10-INITKEY-IESCNTL.
           STRING USER-ID
               DELIMITED BY SPACE    INTO IUI-US-USRIDNT.
           PERFORM Q16-READEQ-IESCNTL.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               SET SCRTY-USERID-UNKNOWN TO TRUE
               STRING '>' USER-ID ' using ' EIBTRNID ' on ' EIBTRMID
                      ' unknown to IBM.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  SCRTY-ACCESS-DENY  TO TRUE
               GO TO A95-SCRTY-PROGRAM-EXIT
           END-IF.

           PERFORM P30-IMPORT-CONTROL-DATA THRU P35-EXIT.

           IF  CNTL-EXPIRED > SPACES
               SET SCRTY-USERID-EXPIRED TO TRUE
               STRING '>' USER-ID ' using ' EIBTRNID ' on ' EIBTRMID
                      ' expired ' CNTL-EXPIRED '.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  SCRTY-ACCESS-DENY  TO TRUE
               GO TO A95-SCRTY-PROGRAM-EXIT
           END-IF.

           IF  CNTL-REVOKED > SPACES
               SET SCRTY-USERID-REVOKED TO TRUE
               STRING '>' USER-ID ' using ' EIBTRNID ' on ' EIBTRMID
                      ' revoked ' CNTL-REVOKED '.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  SCRTY-ACCESS-DENY  TO TRUE
               GO TO A95-SCRTY-PROGRAM-EXIT
           END-IF.

      * get additional user-specific security information
           PERFORM Q20-INITKEY-VUSERID.
           MOVE USER-ID                TO VUSER-KEY.
           PERFORM Q26-READEQ-VUSERID.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               SET SCRTY-USERID-UNKNOWN TO TRUE
               STRING '>' USER-ID ' using ' EIBTRNID ' on ' EIBTRMID
                      ' unknown to WIN.'
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               SET  SCRTY-ACCESS-DENY  TO TRUE
               GO TO A95-SCRTY-PROGRAM-EXIT
           END-IF.

      * save starting values
           PERFORM Q30-INITKEY-COMASTR.
           SET  CHECK-COMP-ACCESS      TO TRUE.
           IF  SCRTY-CONUM > SPACES
               MOVE 1                  TO LEN
               STRING SCRTY-CONUM         DELIMITED BY SPACE
                   INTO SAVE-CONUM   WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               IF  LEN = 3
               AND SAVE-CONUM(1:1) IS ALPHABETIC
               AND SAVE-CONUM(2:2) IS NUMERIC
               AND SAVE-CONUM(4:2) = SPACES
                 SET CHECK-AREA-ACCESS TO TRUE
               END-IF
           ELSE
               MOVE SPACES             TO SAVE-CONUM
           END-IF.

           SET  FLD-ADDR               TO ADDRESS OF SCRTY-OLDLVL.
           COMPUTE LEN = FLD-OFFS - COMM-OFFS.
           IF  EIBCALEN > LEN
           AND SCRTY-OLDLVL = '$'
               MOVE SCRTY-OLDLVL       TO SAVE-LEVEL
           ELSE
               MOVE SPACES             TO SAVE-LEVEL
           END-IF.

           IF  VUSER-WIN-FUNCTION = 'AC'
           OR  VUSER-WIN-FUNCTION = 'VP'
           AND VUSER-WIN-UNIT = 'AL'
               MOVE VUSER-WIN-ORG      TO HOME-AREA
           ELSE
               MOVE SPACES             TO HOME-AREA
           END-IF.
           MOVE VUSER-PRIMARY-CO       TO HOME-CONUM.

           MOVE VUSER-SVC-REGION       TO SAVE-SVCRGN.
           MOVE VUSER-ALT-REGION       TO SAVE-ALTRGN.
           PERFORM B20-MAP-SERVICE-REGION THRU B29-EXIT.

           MOVE VUSER-RPT-REMOTE       TO SAVE-RPTRMT.
           MOVE VUSER-ALT-REMOTE       TO SAVE-ALTRMT.
           PERFORM B40-MAP-REPORT-REMOTE  THRU B49-EXIT.

           MOVE VUSER-DFT-PRINTER      TO SAVE-DFTPTR.
           IF  SAVE-DFTPTR <= SPACES
               EXEC CICS INQUIRE
                         TERMINAL (EIBTRMID)
                         PRINTER  (SAVE-DFTPTR)
                         NOHANDLE
               END-EXEC
               IF  SAVE-DFTPTR <= SPACES
                   MOVE DFT-LCLPTR     TO SAVE-DFTPTR
               END-IF
           END-IF.

           PERFORM B60-MAP-TERMINAL-GROUP THRU B69-EXIT.

      * log the security request
           STRING '<' SAVE-CONUM ' ' SAVE-LEVEL ' requested by '
                      USER-ID ' using ' EIBTRNID ' on ' EIBTRMID ' '
                      SAVE-GROUP ' ' HOME-CONUM ' ' HOME-AREA
               DELIMITED BY SIZE     INTO LOGF-MESG
           PERFORM Q100-LOGIT THRU Q199-EXIT.

           IF  SAVE-LEVEL NOT = '$'
               MOVE '0'                TO SAVE-LEVEL
           END-IF.

      * validate company number
           IF  CHECK-COMP-ACCESS
             IF  SAVE-CONUM > SPACES
             AND NOT = '*****' AND '?????' AND '00000'
             OR  HOME-CONUM IS NUMERIC
               PERFORM Q30-INITKEY-COMASTR
               IF  SAVE-CONUM > SPACES
               AND NOT = '*****' AND '?????' AND '00000'
                   MOVE SAVE-CONUM     TO COMAST-KEY
               ELSE
                   MOVE HOME-CONUM     TO COMAST-KEY
               END-IF
               PERFORM Q36-READEQ-COMASTR
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO A90-SCRTY-PROGRAM-RETURN
               END-IF
               IF  SAVE-SVCRGN <= SPACES
               AND COMAST-DKEY-DAP NOT = '09'
               AND (VUSER-DAPSCO >= '0' AND < '9')
               AND VUSER-WIN-FUNCTION NOT = 'AC'
                  MOVE COMAST-DKEY-DAP TO SAVE-SVCRGN
               END-IF
             END-IF
           ELSE
      * validate area number
               PERFORM Q30-INITKEY-COMASTR
               MOVE SAVE-CONUM         TO COMAST-AKEY
               PERFORM Q36-READGE-COMASTA
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO A90-SCRTY-PROGRAM-RETURN
               END-IF
           END-IF.                                                      ON  ON  DLC      DLC      Dapsco Cis Security Check            /CTL/M-FULL-1/     /UTL.CICS        /CTL/

      * return any optional response fields requested

      * ... set offset to service region response field
           SET  FLD-ADDR               TO ADDRESS OF SCRTY-SVCRGN
           COMPUTE LEN = FLD-OFFS - COMM-OFFS
                       + LENGTH OF SCRTY-SVCRGN
           IF  LEN <= EIBCALEN
             MOVE SAVE-SVCRGN          TO SCRTY-SVCRGN
      * ... set offset to alternate region response field
             SET  FLD-ADDR             TO ADDRESS OF SCRTY-ALTRGN
             COMPUTE LEN = FLD-OFFS - COMM-OFFS
                         + LENGTH OF SCRTY-ALTRGN
             IF  LEN <= EIBCALEN
               MOVE SAVE-ALTRGN        TO SCRTY-ALTRGN
      * ... set offset to report remote response field
               SET  FLD-ADDR           TO ADDRESS OF SCRTY-RPTRMT
               COMPUTE LEN = FLD-OFFS - COMM-OFFS
                           + LENGTH OF SCRTY-RPTRMT
               IF  LEN <= EIBCALEN
                 MOVE SAVE-RPTRMT      TO SCRTY-RPTRMT
      * ... set offset to alternate remote response field
                 SET  FLD-ADDR         TO ADDRESS OF SCRTY-ALTRMT
                 COMPUTE LEN = FLD-OFFS - COMM-OFFS
                             + LENGTH OF SCRTY-ALTRMT
                 IF  LEN <= EIBCALEN
                   MOVE SAVE-ALTRMT    TO SCRTY-ALTRMT
      * ... set offset to default printer response field
                   SET  FLD-ADDR       TO ADDRESS OF SCRTY-DFTPTR
                   COMPUTE LEN = FLD-OFFS - COMM-OFFS
                               + LENGTH OF SCRTY-DFTPTR
                   IF  LEN <= EIBCALEN
                     MOVE SAVE-DFTPTR  TO SCRTY-DFTPTR
      * ... set offset to win function response field
                     SET  FLD-ADDR     TO ADDRESS OF SCRTY-WINFUNC
                     COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                 + LENGTH OF SCRTY-WINFUNC
                     IF  LEN <= EIBCALEN
                       MOVE VUSER-WIN-FUNCTION
                                       TO SCRTY-WINFUNC
      * ... set offset to win unit response field
                       SET  FLD-ADDR   TO ADDRESS OF SCRTY-WINUNIT
                       COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                   + LENGTH OF SCRTY-WINUNIT
                       IF  LEN <= EIBCALEN
                         MOVE VUSER-WIN-UNIT
                                       TO SCRTY-WINUNIT
      * ... set offset to win org response field
                         SET  FLD-ADDR TO ADDRESS OF SCRTY-WINORG
                         COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                     + LENGTH OF SCRTY-WINORG
                         IF  LEN <= EIBCALEN
                           MOVE VUSER-WIN-ORG
                                       TO SCRTY-WINORG
      * ... set offset to win position response field
                           SET  FLD-ADDR TO ADDRESS OF SCRTY-WINPOS
                           COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                       + LENGTH OF SCRTY-WINPOS
                           IF  LEN <= EIBCALEN
                             MOVE VUSER-WIN-POSITION
                                       TO SCRTY-WINPOS
      * ... set offset to win director response field
                             SET FLD-ADDR TO ADDRESS OF SCRTY-WINDIR
                             COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                         + LENGTH OF SCRTY-WINDIR
                             IF  LEN <= EIBCALEN
                               MOVE VUSER-WIN-DIRECTOR
                                       TO SCRTY-WINDIR
      * ... set offset to old dapsco code response field
                               SET FLD-ADDR TO ADDRESS OF SCRTY-OLDDAP
                               COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                           + LENGTH OF SCRTY-OLDDAP
                               IF  LEN <= EIBCALEN
                                MOVE VUSER-DAPSCO
                                       TO SCRTY-OLDDAP
      * ... set offset to old classification (00-99) response field
                                SET FLD-ADDR TO ADDRESS OF SCRTY-OLDCLS
                                COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                            + LENGTH OF SCRTY-OLDCLS
                                IF  LEN <= EIBCALEN
                                 MOVE VUSER-CLASS
                                       TO SCRTY-OLDCLS
      * ... set offset to old terminal group response field
                                 SET FLD-ADDR TO ADDRESS OF SCRTY-OLDGRP
                                 COMPUTE LEN = FLD-OFFS - COMM-OFFS
                                             + LENGTH OF SCRTY-OLDGRP
                                 IF  LEN <= EIBCALEN
                                   MOVE SAVE-GROUP
                                       TO SCRTY-OLDGRP
                                 END-IF.

      * begin security access evaluation by Active Directory attributes
           EVALUATE TRUE

           WHEN SAVE-GROUP = 'AC'
           WHEN SAVE-GROUP = 'PC'
               PERFORM C00-INDIVCO-SECURITY THRU C99-EXIT

           WHEN SAVE-GROUP = 'AL'
               PERFORM D00-AREA-LEADER-SECURITY THRU D99-EXIT

           WHEN SAVE-GROUP = 'XT'
               PERFORM E00-EXTERNAL-SECURITY THRU E99-EXIT

           WHEN SAVE-GROUP = 'DI'
               IF  CHECK-COMP-ACCESS
               AND SAVE-CONUM NOT = COMAST-KEY
               OR  CHECK-AREA-ACCESS
               AND SAVE-CONUM NOT = COMAST-AKEY-AREA
                   MOVE '*****'        TO SAVE-CONUM
               END-IF
               MOVE '9'                TO SAVE-LEVEL

           WHEN SAVE-GROUP = 'DS'
               PERFORM F00-DISTROINC-SECURITY THRU F99-EXIT

           WHEN SAVE-GROUP = 'IP'
           WHEN SAVE-GROUP = 'NC'
           WHEN SAVE-GROUP = 'PR'
               PERFORM I00-INDPRES-SECURITY THRU I99-EXIT

           WHEN OTHER
               PERFORM G00-WGSRGN-SECURITY THRU G99-EXIT

           END-EVALUATE.

      * if not authorized, check VSEC company-level security
           IF  SAVE-LEVEL = '0'
               MOVE SPACES             TO VSEC-RESOURCE-ID
               STRING 'WINCORP.CO' SAVE-CONUM
                  DELIMITED BY SIZE  INTO VSEC-RESOURCE-ID
               IF  USER-ID = CICS-USERID
                   PERFORM P40-CHECK-CUR-USER-ACCESS THRU P45-EXIT
               ELSE
                   PERFORM P50-CHECK-ALT-USER-ACCESS THRU P55-EXIT
               END-IF
           END-IF.

       A90-SCRTY-PROGRAM-RETURN.

      * return the resulting user and company
           MOVE USER-ID                TO SCRTY-USERID.
           MOVE SAVE-CONUM             TO SCRTY-CONUM.

       A95-SCRTY-PROGRAM-EXIT.

      * translate old level (0-9) to new security access (0-4) field
           EVALUATE TRUE
           WHEN SAVE-LEVEL > '0'
           AND  SCRTY-ACCESS = SPACE
             EVALUATE TRUE
             WHEN SAVE-LEVEL > '5'
               SET  SCRTY-ACCESS-ALTR  TO TRUE
             WHEN SAVE-LEVEL > '2'
               SET  SCRTY-ACCESS-UPDT  TO TRUE
             WHEN OTHER
               SET  SCRTY-ACCESS-READ  TO TRUE
             END-EVALUATE
           WHEN SAVE-LEVEL <= '0'
             SET  SCRTY-ACCESS-DENY    TO TRUE
             MOVE '0'                  TO SAVE-LEVEL
           END-EVALUATE.

      * ... set offset to old access level (0-9) response field
           SET  FLD-ADDR               TO ADDRESS OF SCRTY-OLDLVL.
           COMPUTE LEN = FLD-OFFS - COMM-OFFS.
           IF  EIBCALEN > LEN
               MOVE SAVE-LEVEL         TO SCRTY-OLDLVL
           END-IF.

      * log the security response
           IF  SAVE-LEVEL > '0'
               STRING '>' SAVE-CONUM ' ' SAVE-LEVEL ' access granted '
                          'for ' USER-ID
                          ', rmt=' SAVE-RPTRMT ' dft=' SAVE-DFTPTR
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
           ELSE
             IF  SAVE-GROUP = 'PC'
             OR  SAVE-CONUM > SPACES
               STRING '>' SAVE-CONUM ' ' SAVE-LEVEL ' access denied! '
                          'for ' USER-ID
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
             END-IF
           END-IF.

           EXEC CICS RETURN END-EXEC.
           GOBACK.

      ******************************************************************
      * END OF MAINLINE CODE                                           *
      ******************************************************************

      /*****************************************************************
      *    MAP OLD ATTRIBUTES                                          *
      ******************************************************************

       B20-MAP-SERVICE-REGION.
           IF  COMAST-KEY NOT = HOME-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
           END-IF.
           IF  COMAST-KEY = HOME-CONUM
               IF  SAVE-SVCRGN <= SPACES
               AND COMAST-DKEY-DAP NOT = '09'
               AND (VUSER-WIN-FUNCTION NOT = 'AC' OR 'VP')
                 MOVE COMAST-DKEY-DAP  TO SAVE-SVCRGN
               END-IF
           ELSE
               IF  SAVE-SVCRGN <= SPACES
               AND (VUSER-DAPSCO >= '0' AND < '9')
                 STRING '0' VUSER-DAPSCO
                   DELIMITED BY SIZE INTO SAVE-SVCRGN
               END-IF
           END-IF.
           IF  SAVE-ALTRGN <= SPACES
               EVALUATE SAVE-SVCRGN
                 WHEN '01'  MOVE '05'  TO SAVE-ALTRGN
                 WHEN '03'  MOVE '07'  TO SAVE-ALTRGN
                 WHEN '05'  MOVE '01'  TO SAVE-ALTRGN
                 WHEN '08'  MOVE '05'  TO SAVE-ALTRGN
               END-EVALUATE
           END-IF.
       B29-EXIT.
           EXIT.

       B40-MAP-REPORT-REMOTE.
           IF  COMAST-KEY NOT = HOME-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
           END-IF.
           IF  COMAST-KEY = HOME-CONUM
               MOVE COMAST-DKEY-DAP    TO HOME-SVCRGN
           ELSE
               MOVE SAVE-SVCRGN        TO HOME-SVCRGN
           END-IF.
/DLC2/     IF  SAVE-RPTRMT <= SPACES
/DLC2/         PERFORM Q40-INITKEY-GROUPS
/DLC2/         SET  GRP-DAP-TYPE       TO TRUE
/DLC2/         MOVE HOME-SVCRGN        TO GRP-KEY-ID
/DLC2/         PERFORM Q46-READEQ-GROUPS
/DLC2/         IF  LOOKUP-SUCCEEDED
/DLC2/           MOVE GRP-REMOTE       TO SAVE-RPTRMT
/DLC2/         END-IF
/DLC2/     END-IF.
           IF  SAVE-RPTRMT <= SPACES
               EVALUATE HOME-SVCRGN
                 WHEN '00'  MOVE '004' TO SAVE-RPTRMT
                 WHEN '01'  MOVE '006' TO SAVE-RPTRMT
                 WHEN '02'  MOVE '013' TO SAVE-RPTRMT
                 WHEN '03'  MOVE '012' TO SAVE-RPTRMT
                 WHEN '04'  MOVE '007' TO SAVE-RPTRMT
                 WHEN '05'  MOVE '005' TO SAVE-RPTRMT
                 WHEN '06'  MOVE '008' TO SAVE-RPTRMT
                 WHEN '07'  MOVE '027' TO SAVE-RPTRMT
                 WHEN '08'  MOVE '028' TO SAVE-RPTRMT
                 WHEN OTHER MOVE DFT-REMOTE
                                       TO SAVE-RPTRMT
               END-EVALUATE
           END-IF.
       B49-EXIT.
           EXIT.

       B60-MAP-TERMINAL-GROUP.
           EVALUATE TRUE
             WHEN VUSER-WIN-FUNCTION = 'XT'
               MOVE 'XT'               TO SAVE-GROUP
             WHEN VUSER-PRIMARY-IND  = 'B'
               MOVE 'PR'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'AC'
               MOVE 'AC'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'VP'
             AND  VUSER-WIN-UNIT = 'AL'
               MOVE 'AL'               TO SAVE-GROUP
             WHEN (VUSER-WIN-FUNCTION = 'WW' OR 'WS')
             AND  VUSER-PRIMARY-IND = 'D'
             AND  VUSER-WIN-UNIT = 'EXEC'
               MOVE 'DS'               TO SAVE-GROUP
             WHEN VUSER-PRIMARY-IND  = 'N'
             AND (VUSER-WIN-UNIT = 'MKT' OR 'FIN')
               MOVE 'NC'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'WW'
             AND  VUSER-PRIMARY-IND = 'N'
             AND  VUSER-WIN-UNIT NOT = 'IT'
               MOVE 'NO'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'VP' OR 'CO'
               MOVE 'IP'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'WW' OR 'WG'
               MOVE 'DI'               TO SAVE-GROUP
             WHEN VUSER-WIN-FUNCTION = 'WS'
             OR   VUSER-PRIMARY-IND = 'D'
               PERFORM B80-MAP-REGIONAL-USER THRU B89-EXIT
             WHEN OTHER
               MOVE DFT-GROUP          TO SAVE-GROUP
           END-EVALUATE.
       B69-EXIT.
           EXIT.

       B80-MAP-REGIONAL-USER.
           IF  COMAST-KEY NOT = HOME-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
           END-IF.
           IF  COMAST-KEY = HOME-CONUM
               IF  SAVE-SVCRGN <= SPACES
                 MOVE COMAST-DKEY-DAP  TO SAVE-SVCRGN
               END-IF
           ELSE
               IF  SAVE-SVCRGN <= SPACES
                 MOVE DFT-SVCRGN       TO SAVE-SVCRGN
               END-IF
           END-IF.
           STRING SAVE-SVCRGN '***'
               DELIMITED BY SIZE     INTO HOME-CONUM.
           EVALUATE SAVE-SVCRGN
               WHEN '00'   MOVE 'NE'   TO SAVE-GROUP
               WHEN '01'   MOVE 'DA'   TO SAVE-GROUP
               WHEN '02'   MOVE 'MW'   TO SAVE-GROUP
               WHEN '03'   MOVE 'SW'   TO SAVE-GROUP
               WHEN '04'   MOVE 'DP'   TO SAVE-GROUP
               WHEN '05'   MOVE 'NO'   TO SAVE-GROUP
               WHEN '06'   MOVE 'AT'   TO SAVE-GROUP
               WHEN '07'   MOVE 'OM'   TO SAVE-GROUP
               WHEN '08'   MOVE 'NC'   TO SAVE-GROUP
               WHEN '09'   MOVE 'DI'   TO SAVE-GROUP
           END-EVALUATE.
       B89-EXIT.
           EXIT.

      /*****************************************************************
      *    SECURITY ROUTINES                                           *
      ******************************************************************

       C00-INDIVCO-SECURITY.
           IF  HOME-CONUM NOT NUMERIC
           AND (SAVE-CONUM <= SPACES OR = '00000'
                                     OR = '*****'
                                     OR = '?????')
               MOVE HOME-CONUM         TO SAVE-CONUM
               MOVE '0'                TO SAVE-LEVEL
               GO TO C99-EXIT
           END-IF

           IF  SAVE-GROUP = 'AC'
           AND HOME-CONUM > SPACES
           AND HOME-CONUM NOT NUMERIC
               IF  HOME-CONUM(4:2) >= '00'
               AND HOME-CONUM(3:3) NOT NUMERIC
                 MOVE HOME-CONUM(3:3)  TO HOME-AREA
               ELSE
                 MOVE HOME-CONUM(1:3)  TO HOME-AREA
               END-IF
           ELSE
               IF  COMAST-KEY NOT = HOME-CONUM
                 PERFORM Q30-INITKEY-COMASTR
                 MOVE HOME-CONUM       TO COMAST-KEY
                 PERFORM Q36-READEQ-COMASTR
               END-IF
               IF  COMAST-KEY NOT NUMERIC
               OR  COMAST-AKEY-AREA NOT > SPACES
                 PERFORM P20-TEST-COMPANY THRU P25-EXIT
                 GO TO C99-EXIT
               END-IF
               IF  HOME-AREA <= SPACES
                 MOVE COMAST-AKEY-AREA TO HOME-AREA
               END-IF
           END-IF.

           MOVE '1'                    TO SAVE-LEVEL

           PERFORM Q40-INITKEY-GROUPS.
           SET  GRP-AREA-TYPE          TO TRUE.
           MOVE HOME-AREA              TO GRP-KEY-ID.
           PERFORM Q46-READEQ-GROUPS.
           IF  LOOKUP-FAILED
           OR  SAVE-GROUP     = 'PC'
           OR  SAVE-GROUP NOT = 'AC'
           AND HOME-CONUM NOT = GRP-HOME
               IF  SAVE-CONUM IS NUMERIC
               AND SAVE-CONUM NOT = HOME-CONUM
               OR  CHECK-AREA-ACCESS
                   MOVE '0'            TO SAVE-LEVEL
               ELSE
                   MOVE HOME-CONUM     TO SAVE-CONUM
               END-IF
               GO TO C99-EXIT
           END-IF.

      * DENY ACCESS TO THESE INDUSTRY GROUPS.
           IF  COMAST-IND-PRIMUS
           OR  COMAST-IND-DISTRO
           OR  COMAST-IND-WINHOLESALE
           OR  COMAST-IND-DAPSCO
           OR  COMAST-IND-VALUATION
           OR  COMAST-IND-OTHER-NO-ACCESS
               MOVE '0'                TO SAVE-LEVEL
               GO TO C99-EXIT
           END-IF.

           IF  SAVE-GROUP NOT = 'AC'
               GO TO C99-EXIT
           END-IF.

           MOVE '3'                    TO SAVE-LEVEL.

           IF  SAVE-CONUM NOT NUMERIC
               IF  CHECK-COMP-ACCESS
               AND SAVE-CONUM NOT = GRP-CONS
               OR  CHECK-AREA-ACCESS
               AND SAVE-CONUM NOT = HOME-AREA
                   MOVE HOME-CONUM     TO SAVE-CONUM
               END-IF
               GO TO C99-EXIT
           END-IF.

           PERFORM Q30-INITKEY-COMASTR.
           MOVE  HOME-AREA             TO COMAST-AKEY-AREA.
           MOVE  SAVE-CONUM            TO COMAST-AKEY-CO.
           PERFORM Q36-READEQ-COMASTA.
           IF  COMPANY-NOT-FOUND
               MOVE '0'                TO SAVE-LEVEL
               GO TO C99-EXIT
           END-IF.
       C99-EXIT.
           EXIT.

       D00-AREA-LEADER-SECURITY.
           MOVE '4'                    TO SAVE-LEVEL.

           IF  SAVE-CONUM <= SPACES OR = '00000'
                                    OR = '?????'
               MOVE '*****'            TO SAVE-CONUM
               GO TO D99-EXIT
           END-IF.

/DLC0/     IF  HOME-CONUM <= SPACES
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-AREA          TO COMAST-WKEY-WIN
               PERFORM Q36-READGE-COMASTW
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO D99-EXIT
               END-IF
               MOVE COMAST-WKEY-CO     TO HOME-CONUM
/DLC0/     END-IF.

           IF  HOME-CONUM IS NUMERIC
/DLC0/*    AND (SAVE-CONUM <= SPACES OR = '00000'
/DLC0/*                              OR = '?????')
/DLC0/     AND (SAVE-CONUM = HOME-CONUM OR '*****')
               MOVE HOME-CONUM         TO SAVE-CONUM
/DLC0/         GO TO D99-EXIT
           END-IF

/DLC0/*    MOVE SAVE-CONUM             TO HOME-CONUM.
/DLC0/*    IF  COMAST-KEY NOT = HOME-CONUM
/DLC0/     IF  COMAST-KEY NOT = SAVE-CONUM
               PERFORM Q30-INITKEY-COMASTR
/DLC0/*        MOVE HOME-CONUM         TO COMAST-KEY
/DLC0/         MOVE SAVE-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO D99-EXIT
               END-IF
           END-IF.

      * DENY ACCESS TO THESE INDUSTRY GROUPS.
           IF  COMAST-IND-PRIMUS
           OR  COMAST-IND-DISTRO
           OR  COMAST-IND-WINHOLESALE
           OR  COMAST-IND-DAPSCO
           OR  COMAST-IND-VALUATION
           OR  COMAST-IND-OTHER-NO-ACCESS
               MOVE '0'                TO SAVE-LEVEL
           END-IF.
       D99-EXIT.
           EXIT.

       E00-EXTERNAL-SECURITY.
           MOVE '0'                    TO SAVE-LEVEL.
           MOVE SPACES                 TO VSEC-RESOURCE-ID.
           STRING 'WINCORP.CO' SAVE-CONUM
              DELIMITED BY SIZE      INTO VSEC-RESOURCE-ID.
           IF  USER-ID = CICS-USERID
               PERFORM P40-CHECK-CUR-USER-ACCESS THRU P45-EXIT
           ELSE
               PERFORM P50-CHECK-ALT-USER-ACCESS THRU P55-EXIT
           END-IF.
       E99-EXIT.
           EXIT.

       F00-DISTROINC-SECURITY.
           IF  HOME-CONUM IS NUMERIC
           AND (SAVE-CONUM <= SPACES OR = '00000'
                                     OR = '*****'
                                     OR = '?????')
               MOVE HOME-CONUM         TO SAVE-CONUM
           END-IF

           IF  SAVE-CONUM NOT NUMERIC
               IF (USER-ID = 'DLHEWITT' OR 'DCAMFIEL')
               AND SAVE-CONUM = '00LDS'
                   MOVE '9****'        TO SAVE-CONUM
               END-IF
               MOVE '4'                TO SAVE-LEVEL
               GO TO F99-EXIT
           END-IF.

      *    MOVE 'WINAPPS.SY.WINSCRTY.WSS'
      *                                TO VSEC-RESOURCE-ID.
      *    PERFORM P10-GET-VSEC-SECURITY THRU P15-EXIT.
      *    IF  VSECSERV-SEC-UPDATABLE
      *        MOVE SPACES             TO VSEC-RESOURCE-ID
      *        STRING 'WINCORP.CO'        SAVE-CONUM
      *           DELIMITED BY SIZE  INTO VSEC-RESOURCE-ID
      *        PERFORM P10-GET-VSEC-SECURITY THRU P15-EXIT
      *        IF  VSECSERV-SEC-UPDATABLE
      *            MOVE '4'            TO SAVE-LEVEL
      *            GO TO F99-EXIT
      *        END-IF
      *    END-IF.

           IF (COMAST-FUNC-SOURCING
           AND COMAST-STATUS-ACTIVE
           AND COMAST-TIER-FIELD)
               MOVE '4'                TO SAVE-LEVEL
               GO TO F99-EXIT
           END-IF.

           MOVE '4'                    TO SAVE-LEVEL.

      * DENY ACCESS TO THESE INDUSTRY GROUPS.
           IF  COMAST-IND-DAPSCO
           OR  COMAST-IND-VALUATION
           OR  COMAST-IND-OTHER-NO-ACCESS
               MOVE '0'                TO SAVE-LEVEL
           END-IF.
       F99-EXIT.
           EXIT.

       G00-WGSRGN-SECURITY.
           IF  SAVE-CONUM(1:2) IS NUMERIC
           AND SAVE-CONUM(3:3) = '***'
               MOVE '5'                TO SAVE-LEVEL
               GO TO G99-EXIT
           END-IF.

           IF  HOME-CONUM IS NUMERIC
           AND (SAVE-CONUM <= SPACES OR = '00000'
                                     OR = '*****'
                                     OR = '?????')
               MOVE HOME-CONUM         TO SAVE-CONUM
           END-IF

           IF  COMAST-KEY NOT = SAVE-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE SAVE-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO G99-EXIT
               END-IF
           END-IF.

      *    MOVE 'WINAPPS.SY.WINSCRTY.WGS'
      *                                TO VSEC-RESOURCE-ID.
      *    PERFORM P10-GET-VSEC-SECURITY THRU P15-EXIT.
      *    IF  VSECSERV-SEC-UPDATABLE
      *        MOVE SPACES             TO VSEC-RESOURCE-ID
      *        STRING 'WINCORP.CO'        SAVE-CONUM
      *           DELIMITED BY SIZE  INTO VSEC-RESOURCE-ID
      *        PERFORM P10-GET-VSEC-SECURITY THRU P15-EXIT
      *        IF  VSECSERV-SEC-UPDATABLE
      *            MOVE '5'            TO SAVE-LEVEL
      *            GO TO G99-EXIT
      *        END-IF
      *    END-IF.

           IF (COMAST-DKEY-DAP >= '00' AND <= '08')
           OR  COMAST-KEY-CO NOT NUMERIC
           OR (COMAST-FUNC-SERVICE
           AND COMAST-STATUS-ACTIVE
           AND COMAST-TIER-FIELD)
               MOVE '5'                TO SAVE-LEVEL
           END-IF.
       G99-EXIT.
           EXIT.

       I00-INDPRES-SECURITY.
           MOVE '4'                    TO SAVE-LEVEL.

           IF  SAVE-CONUM <= SPACES OR = '00000'
                                    OR = '?????'
               MOVE '*****'            TO SAVE-CONUM
               GO TO I99-EXIT
           END-IF.

           IF  SAVE-CONUM EQUAL '*****'
               IF  SAVE-GROUP = 'NC'
                   MOVE SAVE-SVCRGN    TO SAVE-CONUM(1:2)
               END-IF
               GO TO I99-EXIT
           END-IF.

           MOVE '0'                    TO SAVE-LEVEL.

           IF  HOME-CONUM IS NUMERIC
           AND (SAVE-CONUM <= SPACES OR = '00000'
                                     OR = '*****'
                                     OR = '?????')
               MOVE HOME-CONUM         TO SAVE-CONUM
           END-IF

           MOVE SAVE-CONUM             TO HOME-CONUM.
           IF  COMAST-KEY NOT = HOME-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-CONUM         TO COMAST-KEY
               PERFORM Q36-READEQ-COMASTR
               IF  COMPANY-NOT-FOUND
                   GO TO I99-EXIT
               END-IF
           END-IF.

           IF  SAVE-GROUP = 'NC'
             IF  COMAST-TYPE-NOL
               IF  VUSER-DAPSCO = '8'
                 MOVE '4'              TO SAVE-LEVEL
               ELSE
                 IF  COMAST-FUNC-LOCAL-OPER
                 AND COMAST-WKEY-WIN = VUSER-IND-LIST(2:1)
                                    OR VUSER-IND-LIST(4:1)
                     MOVE '4'          TO SAVE-LEVEL
                 END-IF
               END-IF
             END-IF
           ELSE
             IF  COMAST-TYPE-WIN
               IF  VUSER-DAPSCO = '9'
                 MOVE '4'              TO SAVE-LEVEL
               ELSE
                 IF  COMAST-FUNC-LOCAL-OPER
                 AND COMAST-WKEY-WIN = VUSER-IND-LIST(2:1)
                                    OR VUSER-IND-LIST(4:1)
                     MOVE '4'          TO SAVE-LEVEL
                 END-IF
               END-IF
             END-IF
           END-IF.

      * DENY ACCESS TO THESE INDUSTRY GROUPS.
           IF  COMAST-IND-PRIMUS
           OR  COMAST-IND-DISTRO
           OR  COMAST-IND-WINHOLESALE
           OR  COMAST-IND-DAPSCO
           OR  COMAST-IND-VALUATION
           OR  COMAST-IND-OTHER-NO-ACCESS
               MOVE '0'                TO SAVE-LEVEL
           END-IF.

      * SPECIAL ACCESS TO BOTH WIN LOCALS AND ALL NOLAND
           IF  USER-ID = 'MGSOUDER' OR 'MLSALSMA' OR 'RWFERGUS'
             IF  COMAST-WKEY-WIN = 'C'
             OR  COMAST-WKEY-WIN = 'E'
             OR  COMAST-WKEY-WIN = 'W'
             OR  COMAST-TYPE-NOL
               MOVE '4'                TO SAVE-LEVEL
             END-IF
           END-IF.

      * SPECIAL ACCESS TO 944 PLUS 902, 912, AND WIN ALPHA COMPANIES
           IF  USER-ID = 'MLSALSMA' OR 'RWFERGUS'
           OR  SAVE-GROUP = 'PR'
               MOVE '4'                TO SAVE-LEVEL
           END-IF.
       I99-EXIT.
           EXIT.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P10-GET-VSEC-SECURITY.
           MOVE LOW-VALUES             TO VSECSERV-HEADER.
           SET  VSECSERV-TRAN          TO TRUE.
           SET  VSECSERV-CHK-SECURITY  TO TRUE.
           MOVE VSEC-RESOURCE-ID       TO VSECSERV-CGI-REQU.
           EXEC CICS LINK
                     PROGRAM (VSECSERV)
                     COMMAREA(VSECSERV-HEADER)
                     NOHANDLE
           END-EXEC.
       P15-EXIT.
           EXIT.

       P20-TEST-COMPANY.
           MOVE SPACES                 TO TRMSC-KEY.
           MOVE 'P36'                  TO TRMSC-SYS.
           MOVE 'TRNSMITC'             TO TRMSC-QUAL.
           MOVE HOME-CONUM(3:3)        TO TRMSC-CONUM.
           MOVE TRMSC-KEY              TO DSYS-KEY.
           PERFORM Q56-READEQ-DAPSYSF
           IF  LOOKUP-SUCCEEDED
               MOVE DSYS-RECORD        TO TRNSMITC-RECORD
           END-IF.
           IF  LOOKUP-FAILED
           OR  TRMSC-AREA-CHAIRMAN = SPACES
               MOVE '1'                TO SAVE-LEVEL
               MOVE HOME-CONUM         TO SAVE-CONUM
               GO TO P25-EXIT
           END-IF.

           IF  TRMSC-AREA-CHAIRMAN IS NUMERIC
               IF  SAVE-LEVEL = '$'
                   MOVE '1'            TO SAVE-LEVEL
                   MOVE TRMSC-AREA-CHAIRMAN
                                       TO SAVE-CONUM
                   GO TO P25-EXIT
               END-IF
               MOVE '2'                TO SAVE-LEVEL
               IF  SAVE-CONUM = TRMSC-AREA-CHAIRMAN
                   GO TO P25-EXIT
               END-IF
               IF  SAVE-CONUM = '     ' OR '00000'
                   MOVE TRMSC-AREA-CHAIRMAN
                                       TO SAVE-CONUM
                   GO TO P25-EXIT
               END-IF
               MOVE '0'                TO SAVE-LEVEL
               GO TO P25-EXIT
           END-IF.

           MOVE '1'                    TO SAVE-LEVEL.

           MOVE TRMSC-AREA-CHAIRMAN    TO HOME-AREA.
           PERFORM Q40-INITKEY-GROUPS.
           SET  GRP-AREA-TYPE          TO TRUE.
           MOVE HOME-AREA              TO GRP-KEY-ID.
           PERFORM Q46-READEQ-GROUPS.
           IF  LOOKUP-FAILED
               MOVE HOME-CONUM         TO SAVE-CONUM
               GO TO P25-EXIT
           END-IF.

           MOVE '3'                    TO SAVE-LEVEL.

           IF  SAVE-CONUM NOT = HOME-CONUM
               PERFORM Q30-INITKEY-COMASTR
               MOVE HOME-AREA          TO COMAST-AKEY-AREA
               MOVE SAVE-CONUM         TO COMAST-AKEY-CO
               PERFORM Q36-READEQ-COMASTA
               IF  COMPANY-NOT-FOUND
                   MOVE '0'            TO SAVE-LEVEL
                   GO TO P25-EXIT
               END-IF
           END-IF.
       P25-EXIT.
           EXIT.

       P30-IMPORT-CONTROL-DATA.
           MOVE IUI-US-USRNAME         TO CNTL-USRNAME.
           MOVE IUI-US-OPRIDNT         TO CNTL-OPRIDNT.

      * extract settings for expiration date
           IF  IUI-US-EXPJULN > ZEROES
           AND (IUI-US-EXPJULN + 100000) < EIBDATE
               MOVE 'J'                TO DTE-REQU
               MOVE IUI-US-EXPJULN     TO DTE-JNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE ALL '/'            TO EDT-DATE-X
               MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
               MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
               MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
               MOVE EDT-DATE-X         TO CNTL-EXPIRED
           ELSE
               MOVE SPACES             TO CNTL-EXPIRED
           END-IF.

      * extract settings for revokation date
           IF  IUI-US-REVOKEJ > ZEROES
           AND (IUI-US-REVOKEJ + 100000) < EIBDATE
               MOVE   'J'              TO DTE-REQU
               MOVE IUI-US-REVOKEJ     TO DTE-JNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE ALL '/'            TO EDT-DATE-X
               MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
               MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
               MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
               MOVE EDT-DATE-X         TO CNTL-REVOKED
           ELSE
               MOVE SPACES             TO CNTL-REVOKED
           END-IF.
       P35-EXIT.
           EXIT.

       P40-CHECK-CUR-USER-ACCESS.
           PERFORM P10-GET-VSEC-SECURITY THRU P15-EXIT.
           EVALUATE TRUE
           WHEN VSECSERV-SEC-ALTERABLE
               MOVE '1'                TO SAVE-LEVEL
               SET  SCRTY-ACCESS-ALTR
                                       TO TRUE
           WHEN VSECSERV-SEC-UPDATABLE
               MOVE '1'                TO SAVE-LEVEL
               SET  SCRTY-ACCESS-UPDT
                                       TO TRUE
           WHEN VSECSERV-SEC-READABLE
               MOVE '1'                TO SAVE-LEVEL
               SET  SCRTY-ACCESS-READ
                                       TO TRUE
           WHEN OTHER
               MOVE '0'                TO SAVE-LEVEL
               SET  SCRTY-ACCESS-DENY
                                       TO TRUE
           END-EVALUATE.
       P45-EXIT.
           EXIT.

       P50-CHECK-ALT-USER-ACCESS.
           PERFORM Q60-INITKEY-BSTCNTL.
           MOVE VSEC-RESOURCE-ID       TO BSM-RESOURCE.
           PERFORM Q66-READEQ-BSTCNTL.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE '0'                TO SAVE-LEVEL
               SET  SCRTY-ACCESS-DENY  TO TRUE
           ELSE
               PERFORM WITH TEST BEFORE
                 VARYING SUB FROM 1 BY 1
                   UNTIL SUB > BSM-SEC-PERMITS
                      OR BSM-SEC-USERID(SUB) = USER-ID
               END-PERFORM
               IF  SUB > BSM-SEC-PERMITS
                   MOVE '0'            TO SAVE-LEVEL
                   SET  SCRTY-ACCESS-DENY
                                       TO TRUE
               ELSE
                   EVALUATE TRUE
                   WHEN BSM-SEC-AC-ALTER(SUB)
                     MOVE '1'          TO SAVE-LEVEL
                     SET  SCRTY-ACCESS-ALTR
                                       TO TRUE
                   WHEN BSM-SEC-AC-UPDATE(SUB)
                     MOVE '1'          TO SAVE-LEVEL
                     SET  SCRTY-ACCESS-UPDT
                                       TO TRUE
                   WHEN BSM-SEC-AC-READ(SUB)
                     MOVE '1'          TO SAVE-LEVEL
                     SET  SCRTY-ACCESS-READ
                                       TO TRUE
                   WHEN OTHER
                     MOVE '0'          TO SAVE-LEVEL
                     SET  SCRTY-ACCESS-DENY
                                       TO TRUE
                   END-EVALUATE
               END-IF
           END-IF.
       P55-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-FORMAT-DATE-AND-TIME.
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     FULLDATE (WS-WRKDATE)          DATESEP
                     TIME     (WS-WRKTIME)          TIMESEP
           END-EXEC.
       Q02-REFORMAT-TIME.
           IF  WS-HRSTIME < 12
               MOVE ' AM'              TO WS-XXXTIME
               IF  WS-HRSTIME = 00
                   MOVE 12             TO WS-HRSTIME
               END-IF
           ELSE
               MOVE ' PM'              TO WS-XXXTIME
               IF  WS-HRSTIME > 12
                   SUBTRACT 12       FROM WS-HRSTIME
               END-IF
           END-IF.
       Q05-EXIT.
           EXIT.

       Q10-INITKEY-IESCNTL.
/DLC1/*    IF  ADDRESS OF IESCNTL-RECORD = NULL
/DLC1/*        EXEC CICS GETMAIN
/DLC1/*                  SET      (ADDRESS OF IESCNTL-RECORD)
/DLC1/*                  LENGTH   (LENGTH OF IESCNTL-RECORD)
/DLC1/*        END-EXEC
/DLC1/*        SET ADDRESS OF IUI-US-RECORD
/DLC1/*                                TO ADDRESS OF IESCNTL-RECORD
/DLC1/*    END-IF.
           MOVE LOW-VALUES             TO IESCNTL-RECORD.
           SET  IUI-USER-PROFILE       TO TRUE.
/DLC1/     MOVE IESCNTL-RECORD         TO IUI-US-RECORD.
       Q16-READEQ-IESCNTL.
           MOVE LOW-VALUES             TO IUI-US-RECORD(13:).
/DLC3/     MOVE LENGTH OF IESCNTL-RECORD
/DLC3/                                 TO IUI-RECL.
           EXEC CICS READ
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
/DLC3/               LENGTH   (IUI-RECL)
                     RIDFLD   (IUI-US-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q19-EXIT.
           EXIT.

       Q20-INITKEY-VUSERID.
/DLC1/*    IF  ADDRESS OF VUSERID-RECORD = NULL
/DLC1/*        EXEC CICS GETMAIN
/DLC1/*                  SET      (ADDRESS OF VUSERID-RECORD)
/DLC1/*                  LENGTH   (LENGTH OF VUSERID-RECORD)
/DLC1/*        END-EXEC
/DLC1/*    END-IF.
           MOVE SPACES                 TO VUSERID-RECORD.
       Q26-READEQ-VUSERID.
           MOVE SPACES                 TO VUSERID-RECORD(13:).
           EXEC CICS READ
                     DATASET  (VUSERID)
                     INTO     (VUSERID-RECORD)
                     RIDFLD   (VUSER-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q30-INITKEY-COMASTR.
/DLC1/*    IF  ADDRESS OF COMASTR-RECORD = NULL
/DLC1/*        EXEC CICS GETMAIN
/DLC1/*                  SET      (ADDRESS OF COMASTR-RECORD)
/DLC1/*                  LENGTH   (LENGTH OF COMASTR-RECORD)
/DLC1/*        END-EXEC
/DLC1/*    END-IF.
           MOVE SPACES                 TO COMASTR-RECORD.
       Q36-READEQ-COMASTR.
           SET  COMPANY-WAS-FOUND      TO TRUE.
           EXEC CICS READ
                     DATASET  (COMASTR)
                     INTO     (COMASTR-RECORD)
                     RIDFLD   (COMAST-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO COMASTR-RECORD
               SET  COMPANY-NOT-FOUND  TO TRUE
           END-IF.
       Q36-READEQ-COMASTA.
           SET  COMPANY-WAS-FOUND      TO TRUE.
           EXEC CICS READ
                     DATASET  (COMASTA)
                     INTO     (COMASTR-RECORD)
                     RIDFLD   (COMAST-AKEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO COMASTR-RECORD
               SET  COMPANY-NOT-FOUND  TO TRUE
           END-IF.
       Q36-READGE-COMASTA.
           SET  COMPANY-WAS-FOUND      TO TRUE.
           EXEC CICS READ
                     DATASET  (COMASTA)
                     INTO     (COMASTR-RECORD)
                     RIDFLD   (COMAST-AKEY)
                     KEYLENGTH(3)
                     GENERIC
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO COMASTR-RECORD
               SET  COMPANY-NOT-FOUND  TO TRUE
           END-IF.
       Q36-READGE-COMASTW.
           SET  COMPANY-WAS-FOUND      TO TRUE.
           EXEC CICS READ
                     DATASET  (COMASTW)
                     INTO     (COMASTR-RECORD)
                     RIDFLD   (COMAST-WKEY)
                     KEYLENGTH(1)
                     GENERIC
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO COMASTR-RECORD
               SET  COMPANY-NOT-FOUND  TO TRUE
           END-IF.
       Q39-EXIT.
           EXIT.

       Q40-INITKEY-GROUPS.
/DLC1/*    IF  ADDRESS OF GROUPS-RECORD = NULL
/DLC1/*        EXEC CICS GETMAIN
/DLC1/*                  SET      (ADDRESS OF GROUPS-RECORD)
/DLC1/*                  LENGTH   (LENGTH OF GROUPS-RECORD)
/DLC1/*        END-EXEC
/DLC1/*    END-IF.
           MOVE SPACES                 TO GROUPS-RECORD.
       Q46-READEQ-GROUPS.
           SET  LOOKUP-SUCCEEDED       TO TRUE.
           EXEC CICS READ
                     DATASET  (GROUPS)
                     INTO     (GROUPS-RECORD)
                     RIDFLD   (GRP-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO GROUPS-RECORD
               SET  LOOKUP-FAILED      TO TRUE
           END-IF.
       Q49-EXIT.
           EXIT.

       Q56-READEQ-DAPSYSF.
           SET  LOOKUP-SUCCEEDED       TO TRUE.
           EXEC CICS READ
                     DATASET  (DAPSYSF)
                     INTO     (DSYS-RECORD)
                     RIDFLD   (DSYS-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
               MOVE SPACES             TO DSYS-RECORD
               SET  LOOKUP-FAILED      TO TRUE
           END-IF.
       Q59-EXIT.
           EXIT.

       Q60-INITKEY-BSTCNTL.
/DLC1/*    IF  ADDRESS OF BSTCNTL-RECORD = NULL
/DLC1/*        EXEC CICS GETMAIN
/DLC1/*                  SET      (ADDRESS OF BSTCNTL-RECORD)
/DLC1/*                  LENGTH   (LENGTH OF BSTCNTL-RECORD)
/DLC1/*        END-EXEC
/DLC1/*        SET  ADDRESS OF BSM-SECURITY-RECORD
/DLC1/*                                TO ADDRESS OF BSTCNTL-RECORD
/DLC1/*        SET  ADDRESS OF BSM-GROUP-RECORD
/DLC1/*                                TO ADDRESS OF BSTCNTL-RECORD
/DLC1/*    END-IF.
           MOVE LOW-VALUES             TO BSTCNTL-RECORD.
           SET  BSM-FACILITY           TO TRUE.
       Q66-READEQ-BSTCNTL.
           MOVE +32767                 TO BSM-RECL.
           EXEC CICS READ
                     DATASET  (BSTCNTL)
                     INTO     (BSTCNTL-RECORD)
                     LENGTH   (BSM-RECL)
                     RIDFLD   (BSM-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               IF  EIBRESP NOT = DFHRESP(NOTFND)
                   PERFORM Q200-UNEX-LOG
                   EXEC CICS ABEND ABCODE('UNEX') END-EXEC
               END-IF
/DLC1/     ELSE
/DLC1/         MOVE BSTCNTL-RECORD     TO BSM-SECURITY-RECORD
/DLC1/                                    BSM-GROUP-RECORD
           END-IF.
       Q69-EXIT.
           EXIT.

      /*****************************************************************
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *
      ******************************************************************
       COPY LOGGINGP.

      /*****************************************************************
      *    PROGRAM ERRORS (UNEXPECTED)                                 *
      ******************************************************************
       COPY UNEXERRP.


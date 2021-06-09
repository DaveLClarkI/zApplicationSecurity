      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    GETMAILA.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  JULY 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      NONE.
      *REMARKS.       GET AD MAIL ATTRIBUTE FOR CURRENT USER.

      * CHANGE HISTORY ------------------------------------------------
      * 01/05/2020 DLC ORIGINAL PROGRAM.
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
         03  IESLDUM                   PIC  X(08)   VALUE 'IESLDUM'.
         03  IESLDUV                   PIC  X(08)   VALUE 'IESLDUV'.
         03  IESLDGAC                  PIC  X(08)   VALUE 'IESLDGAC'.
         03  CICS-USERID               PIC  X(8).

       COPY IESLDGAC.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

       01  DFHCOMMAREA.
         03  REQ-USERID                PIC  X(08).
         03  AD-MAIL                   PIC  X(40).

      * IESLDUM IBM LDAP MAPPING FILE LAYOUT
       COPY IESLDUM.

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

           IF  EIBCALEN < LENGTH OF DFHCOMMAREA
               EXEC CICS ABEND
                         ABCODE('CALN')
                         CANCEL
                         NODUMP
               END-EXEC
           END-IF.

           PERFORM Q20-INITKEY-IESLDUM.
           IF  REQ-USERID > SPACES
               MOVE REQ-USERID         TO LDUM-MFUSRID
           ELSE
               EXEC CICS ASSIGN
                         USERID    (CICS-USERID)
                         NOHANDLE
               END-EXEC
               MOVE CICS-USERID        TO LDUM-MFUSRID
           END-IF.
           PERFORM Q26-READEQ-IESLDUV.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM P20-GET-MAIL-ATTRIBUTE THRU P25-EXIT
           ELSE
               MOVE '*unknown'         TO AD-MAIL
           END-IF.

           EXEC CICS RETURN END-EXEC.

           GOBACK.

       P20-GET-MAIL-ATTRIBUTE.
           INITIALIZE IESLDGA-COMMAREA.
           MOVE LENGTH OF IESLDGA-COMMAREA
                                       TO LDGA-AREA-LENGTH.
           MOVE LDUM-NETUSRID          TO LDGA-USER-ID.
           MOVE '&(objectClass=person)(objectClass=user)'
                                       TO LDGA-SEARCH-FILTER.
           MOVE 1                      TO LDGA-ATTR-COUNT.

           MOVE 'mail'                 TO LDGA-ATTR-NAME(1).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(1).
           MOVE 8                      TO LDGA-VALUE-COUNT(1).

           EXEC CICS LINK
                     PROGRAM  (IESLDGAC)
                     COMMAREA (IESLDGA-COMMAREA)
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND LDGA-RET-CODE = ZEROES
           AND LDGA-ATTR-VALUE(1 1) > SPACES
               MOVE LDGA-ATTR-VALUE(1 1)
                                       TO AD-MAIL
           ELSE
               MOVE '*notfound'        TO AD-MAIL
           END-IF.
       P25-EXIT.
           EXIT.

       Q20-INITKEY-IESLDUM.
           IF  ADDRESS OF IESLDUM-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESLDUM-RECORD)
                         LENGTH   (LENGTH OF IESLDUM-RECORD)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO LDUM-KEY.
           SET  LDUM-USRMAP-RECORD     TO TRUE.
       Q26-READEQ-IESLDUV.
           EXEC CICS READ
                     DATASET  (IESLDUV)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-MFUSRID)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.


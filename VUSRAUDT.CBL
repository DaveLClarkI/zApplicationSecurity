      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VUSRAUDT.                                                 
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  JANUARY 2020.                                             
       DATE-COMPILED.                                                           
       INSTALLATION.  WINSUPPLY GROUP SERVICES.                                 
       SECURITY.      BATCH.                                                    
      *REMARKS.       GET ACTIVE DIRECTORY ATTRIBUTES FOR AUDIT.                
      *                                                                         
      * this program compares vuserid attributes with active directory          
      * and lists any conflicting or mismatching attributes as an audit.        
      * any changes here may also require changes in program WLSNPGM            
      *                                       and in program VUPMPGM            
      *                                       and in program VUSRPGM            
      *                                       and in program VUSRATTR.          
      *                                                                         
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 01/29/2020 DLC ORIGINAL PROGRAM.                                        
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
           SYSLST IS PRINTER.                                                   
                                                                                
      ******************************************************************        
      *    INPUT-OUTPUT SECTION                                        *        
      ******************************************************************        
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
           SELECT VUSERID                                                       
               ASSIGN TO VUSERID                                                
               ACCESS IS SEQUENTIAL  INDEXED                                    
               RECORD KEY IS VUSER-KEY                                          
               FILE STATUS IS FILE1-STAT                                        
                              FILE1-FDBK.                                       
                                                                                
           SELECT IESCNTL                                                       
               ASSIGN TO IESCNTL                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS IUI-KEY                                            
               FILE STATUS IS FILE2-STAT                                        
                              FILE2-FDBK.                                       
                                                                                
           SELECT IESLDUM                                                       
               ASSIGN TO IESLDUM                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS LDUM-KEY                                           
               FILE STATUS IS FILE3-STAT                                        
                              FILE3-FDBK.                                       
                                                                                
           SELECT IESLDUV                                                       
               ASSIGN TO IESLDUV                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS LDUV-MFUSRID                                       
               FILE STATUS IS FILE4-STAT                                        
                              FILE4-FDBK.                                       
                                                                                
           SELECT COMASTR                                                       
               ASSIGN TO COMASTR                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS COMSTR-KEY                                         
               FILE STATUS IS FILE5-STAT                                        
                              FILE5-FDBK.                                       
                                                                                
           SELECT COMASTI                                                       
               ASSIGN TO COMASTI                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS COMSTI-KEY                                         
               FILE STATUS IS FILE6-STAT                                        
                              FILE6-FDBK.                                       
                                                                                
           SELECT COMASTA                                                       
               ASSIGN TO COMASTA                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS COMSTA-KEY                                         
               FILE STATUS IS FILE7-STAT                                        
                              FILE7-FDBK.                                       
                                                                                
           SELECT COMASTW                                                       
               ASSIGN TO COMASTW                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS COMSTW-KEY                                         
               FILE STATUS IS FILE8-STAT                                        
                              FILE8-FDBK.                                       
                                                                                
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
                                                                                
       FD  VUSERID                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 127 TO 174 CHARACTERS                                         
             DEPENDING ON VUSERID-RECL.                                         
       COPY VUSERID.                                                            
                                                                                
       FD  IESCNTL                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 100 TO 1000 CHARACTERS                                        
             DEPENDING ON IESCNTL-RECL.                                         
       COPY IESCNTL.                                                            
                                                                                
       FD  IESLDUM.                                                             
       COPY IESLDUM.                                                            
                                                                                
       FD  IESLDUV.                                                             
       01  IESLDUV-RECORD.                                                      
         03                            PIC  X(70).                              
         03  LDUV-MFUSRID              PIC  X(8).                               
         03                            PIC  X(172).                             
                                                                                
       FD  COMASTR.                                                             
       01  COMSTR-RECORD.                                                       
         03  COMSTR-KEY                PIC  X(5).                               
         03                            PIC  X(199).                             
                                                                                
       FD  COMASTI.                                                             
       01  COMSTI-RECORD.                                                       
         03                            PIC  X(26).                              
         03  COMSTI-KEY                PIC  X(6).                               
         03                            PIC  X(172).                             
                                                                                
       FD  COMASTA.                                                             
       01  COMSTA-RECORD.                                                       
         03                            PIC  X(18).                              
         03  COMSTA-KEY                PIC  X(8).                               
         03                            PIC  X(178).                             
                                                                                
       FD  COMASTW.                                                             
       01  COMSTW-RECORD.                                                       
         03                            PIC  X(12).                              
         03  COMSTW-KEY                PIC  X(6).                               
         03                            PIC  X(186).                             
                                                                                
      ******************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-FIELDS.                                                           
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.        
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.            
         03  THIS-PGM                  PIC  X(8)    VALUE 'VUSRAUDT'.           
         03  IESLDGAB                  PIC  X(08)   VALUE 'IESLDGAB'.           
                                                                                
         03  VUSER                     PIC S9(04)   BINARY VALUE 1.             
         03  IUI                       PIC S9(04)   BINARY VALUE 2.             
         03  LDUM                      PIC S9(04)   BINARY VALUE 3.             
         03  LDUV                      PIC S9(04)   BINARY VALUE 4.             
         03  COMSTR                    PIC S9(04)   BINARY VALUE 5.             
         03  COMSTI                    PIC S9(04)   BINARY VALUE 6.             
         03  COMSTA                    PIC S9(04)   BINARY VALUE 7.             
         03  COMSTW                    PIC S9(04)   BINARY VALUE 8.             
                                                                                
         03  USER-COUNT                PIC  9(7)    VALUE ZEROES.               
         03  IBM-MISSING               PIC  9(7)    VALUE ZEROES.               
         03  ICCF-USERS                PIC  9(7)    VALUE ZEROES.               
         03  REVOKED-USERS             PIC  9(7)    VALUE ZEROES.               
         03  EXPIRED-USERS             PIC  9(7)    VALUE ZEROES.               
         03  VSE-USERS                 PIC  9(7)    VALUE ZEROES.               
         03  MAIL-ERRORS               PIC  9(7)    VALUE ZEROES.               
         03  MFID-ERRORS               PIC  9(7)    VALUE ZEROES.               
         03  ATTRS-DIFFER              PIC  9(7)    VALUE ZEROES.               
         03  FUNC-UNKNOWN              PIC  9(7)    VALUE ZEROES.               
         03  UNIT-DISAGREE             PIC  9(7)    VALUE ZEROES.               
         03  POS-UNKNOWN               PIC  9(7)    VALUE ZEROES.               
         03  PRES-DISAGREE             PIC  9(7)    VALUE ZEROES.               
         03  SIC-DISAGREE              PIC  9(7)    VALUE ZEROES.               
         03  VP-DISAGREE               PIC  9(7)    VALUE ZEROES.               
         03  EXEC-DISAGREE             PIC  9(7)    VALUE ZEROES.               
         03  AL-DISAGREE               PIC  9(7)    VALUE ZEROES.               
         03  COMP-ERRORS               PIC  9(7)    VALUE ZEROES.               
         03  WRONG-INDUSTRY            PIC  9(7)    VALUE ZEROES.               
         03  WRONG-WINAREA             PIC  9(7)    VALUE ZEROES.               
         03  INDUSTRY-ERROR            PIC  9(7)    VALUE ZEROES.               
         03  WINAREA-ERROR             PIC  9(7)    VALUE ZEROES.               
         03  WINRGN-ERROR              PIC  9(7)    VALUE ZEROES.               
         03  USER-ERRORS               PIC  9(7)    VALUE ZEROES.               
                                                                                
         03  VUSERID-RECL              PIC  9(05)   VALUE ZEROES.               
         03  IESCNTL-RECL              PIC  9(05)   VALUE ZEROES.               
         03  VAR-EDIT                  PIC  Z(6)9-.                             
         03  VAR-TEXTL                 PIC S9(04)   BINARY VALUE ZEROES.        
         03  VAR-TEXT                  PIC  X(120)  VALUE SPACES.               
                                                                                
         03  ERROR-SWITCH              PIC  X(01).                              
           88  USER-IS-GOOD                         VALUE 'N'.                  
           88  USER-IN-ERROR                        VALUE 'Y'.                  
         03  WORK-ICCFUSR              PIC  X(01).                              
           88  IS-ICCF-USER                         VALUE 'Y'.                  
           88  NOT-ICCF-USER                        VALUE 'N'.                  
         03  NAME-LENGTH               PIC S9(04)   BINARY.                     
         03  WORK-EXPIRED              PIC  X(1).                               
         03  WORK-REVOKED              PIC  X(1).                               
                                                                                
         03  NUM-JULN                  PIC  9(5).                               
         03  NUM-DATE                  PIC  9(8).                               
         03  WS-WRKDATE.                                                        
           05  WS-MODATE               PIC  9(02).                              
           05                          PIC  X.                                  
           05  WS-DADATE               PIC  9(02).                              
           05                          PIC  X.                                  
           05  WS-YYDATE               PIC  9(02).                              
                                                                                
         03  WS-ATTRIBUTES.                                                     
           05  WS-PRIMARY-CO           PIC  X(5)    VALUE SPACES.               
           05  WS-PRIMARY-IND          PIC  X(1)    VALUE SPACES.               
           05  WS-WIN-FUNCTION         PIC  X(3)    VALUE SPACES.               
           05  WS-WIN-UNIT             PIC  X(10)   VALUE SPACES.               
           05  WS-WIN-ORG              PIC  X(5)    VALUE SPACES.               
           05  WS-WIN-POSITION         PIC  X(5)    VALUE SPACES.               
           05  WS-WIN-DIRECTOR         PIC  X(1)    VALUE SPACES.               
                                                                                
         03  WORD1                     PIC  X(20)   VALUE SPACES.               
         03  WORD2                     PIC  X(20)   VALUE SPACES.               
         03  WORD3                     PIC  X(20)   VALUE SPACES.               
         03  WORD4                     PIC  X(20)   VALUE SPACES.               
         03  WORD5                     PIC  X(20)   VALUE SPACES.               
                                                                                
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.        
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.        
                                                                                
      * IBM GET LDAP ATTRIBUTE INTERFACE                                        
       01  IESLDGA-COMMAREA.                                                    
         03  LDGA-AREA-LENGTH          PIC S9(09)   BINARY.                     
         03  LDGA-USER-ID              PIC  X(64).                              
         03  LDGA-SEARCH-FILTER        PIC  X(128).                             
         03  LDGA-RET-CODE             PIC S9(09)   BINARY.                     
         03  LDGA-LDAP-CODE            PIC S9(09)   BINARY.                     
         03  LDGA-ATTR-COUNT           PIC S9(04)   BINARY.                     
         03  LDGA-ATTR-ENTRY                        OCCURS 16.                  
           05  LDGA-ATTR-NAME          PIC  X(64).                              
           05  LDGA-VALUE-LENGTH       PIC S9(04)   BINARY.                     
           05  LDGA-VALUE-COUNT        PIC S9(04)   BINARY.                     
           05  LDGA-VALUE-ENTRY                     OCCURS 1.                   
             07  LDGA-ATTR-VALUE       PIC  X(128).                             
                                                                                
       COPY COMASTR.                                                            
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       COPY BITMAN.                                                             
                                                                                
       COPY DTEMAN.                                                             
                                                                                
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
                                                                                
           PERFORM C00-AUDIT-USER-ATTRIBUTES THRU C99-EXIT                      
             UNTIL STAT-EOFILE(VUSER)                                           
                OR RTC-CODE NOT = ZERO.                                         
                                                                                
           PERFORM B20-TERMINATION THRU B25-EXIT.                               
                                                                                
           DISPLAY ' '               UPON PRINTER.                              
           DISPLAY ' '               UPON PRINTER.                              
           MOVE USER-COUNT             TO VAR-EDIT.                             
           DISPLAY 'Total users found ........ ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE IBM-MISSING            TO VAR-EDIT.                             
           DISPLAY 'IBM security missing ..... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE ICCF-USERS             TO VAR-EDIT.                             
           DISPLAY 'ICCF users skipped ....... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE EXPIRED-USERS          TO VAR-EDIT.                             
           DISPLAY 'Expired users skipped .... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE REVOKED-USERS          TO VAR-EDIT.                             
           DISPLAY 'Revoked users skipped .... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE VSE-USERS              TO VAR-EDIT.                             
           DISPLAY 'VSE-Only users skipped ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
                                                                                
           DISPLAY ' '               UPON PRINTER.                              
           MOVE MAIL-ERRORS            TO VAR-EDIT.                             
           DISPLAY 'AD MAIL missing .......... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE MFID-ERRORS            TO VAR-EDIT.                             
           DISPLAY 'AD MFID errors ........... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE ATTRS-DIFFER           TO VAR-EDIT.                             
           DISPLAY 'Attribute mismatches ..... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE FUNC-UNKNOWN           TO VAR-EDIT.                             
           DISPLAY 'AD WINFunction unknown ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE UNIT-DISAGREE          TO VAR-EDIT.                             
           DISPLAY 'AD OU disagrees w/Func ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE POS-UNKNOWN            TO VAR-EDIT.                             
           DISPLAY 'AD WINPosition unknown ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE PRES-DISAGREE          TO VAR-EDIT.                             
           DISPLAY '"PRES" disagrees w/Pos ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE SIC-DISAGREE           TO VAR-EDIT.                             
           DISPLAY '"SIC" disagrees W/Func ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE VP-DISAGREE            TO VAR-EDIT.                             
           DISPLAY '"VP" disagrees W/Unit .... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE EXEC-DISAGREE          TO VAR-EDIT.                             
           DISPLAY '"EXEC" disagrees W/Fnc ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE AL-DISAGREE            TO VAR-EDIT.                             
           DISPLAY '"AL" disagrees W/Func .... ' VAR-EDIT                       
                                     UPON PRINTER.                              
                                                                                
           DISPLAY ' '               UPON PRINTER.                              
           MOVE COMP-ERRORS            TO VAR-EDIT.                             
           DISPLAY 'Primary company errors ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE WRONG-INDUSTRY         TO VAR-EDIT.                             
           DISPLAY 'Wrong primary industry ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE WRONG-WINAREA          TO VAR-EDIT.                             
           DISPLAY 'Wrong WIN Area ........... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE INDUSTRY-ERROR         TO VAR-EDIT.                             
           DISPLAY 'Industry not on file ..... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE WINAREA-ERROR          TO VAR-EDIT.                             
           DISPLAY 'WIN Area not on file ..... ' VAR-EDIT                       
                                     UPON PRINTER.                              
           MOVE WINRGN-ERROR           TO VAR-EDIT.                             
           DISPLAY 'WIN Region not on file ... ' VAR-EDIT                       
                                     UPON PRINTER.                              
                                                                                
           DISPLAY ' '               UPON PRINTER.                              
           MOVE USER-ERRORS            TO VAR-EDIT.                             
           DISPLAY 'Total users in error ..... ' VAR-EDIT                       
                                     UPON PRINTER.                              
                                                                                
           GOBACK.                                                              
                                                                                
      /*****************************************************************        
      *    PROGRAM INITIALIZATION ROUTINE                              *        
      ******************************************************************        
       B10-INITIALIZATION.                                                      
                                                                                
           COPY BATCHINI.                                                       
                                                                                
           MOVE ZEROES                 TO RTC-CODE.                             
                                                                                
           MOVE 'VUSERID'              TO VSAM-FILE(VUSER).                     
           MOVE LENGTH OF VUSER-KEY    TO VSAM-KEYL(VUSER).                     
                                                                                
           MOVE 'IESCNTL'              TO VSAM-FILE(IUI).                       
           MOVE LENGTH OF IUI-KEY      TO VSAM-KEYL(IUI).                       
                                                                                
           MOVE 'IESLDUM'              TO VSAM-FILE(LDUM).                      
           MOVE LENGTH OF LDUM-KEY     TO VSAM-KEYL(LDUM).                      
                                                                                
           MOVE 'IESLDUV'              TO VSAM-FILE(LDUV).                      
           MOVE LENGTH OF LDUM-MFUSRID TO VSAM-KEYL(LDUV).                      
                                                                                
           MOVE 'COMASTR'              TO VSAM-FILE(COMSTR).                    
           MOVE LENGTH OF COMAST-KEY   TO VSAM-KEYL(COMSTR).                    
                                                                                
           MOVE 'COMASTI'              TO VSAM-FILE(COMSTI).                    
           MOVE LENGTH OF COMAST-IKEY  TO VSAM-KEYL(COMSTI).                    
                                                                                
           MOVE 'COMASTA'              TO VSAM-FILE(COMSTA).                    
           MOVE LENGTH OF COMAST-AKEY  TO VSAM-KEYL(COMSTA).                    
                                                                                
           MOVE 'COMASTW'              TO VSAM-FILE(COMSTW).                    
           MOVE LENGTH OF COMAST-WKEY  TO VSAM-KEYL(COMSTW).                    
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                    
               SET  FUNC-OPEN(VSUB)    TO TRUE                                  
           END-PERFORM.                                                         
                                                                                
           OPEN INPUT VUSERID                                                   
                      IESCNTL                                                   
                      IESLDUM                                                   
                      IESLDUV                                                   
                      COMASTR                                                   
                      COMASTI                                                   
                      COMASTA                                                   
                      COMASTW.                                                  
                                                                                
           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                         
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.                   
                                                                                
           IF  RTC-CODE = ZERO                                                  
               PERFORM WITH TEST BEFORE                                         
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                
                   SET FUNC-READ(VSUB) TO TRUE                                  
               END-PERFORM                                                      
               MOVE SPACES             TO VUSER-KEY                             
               SET  FUNC-START(VUSER)  TO TRUE                                  
               START VUSERID KEY >= VUSER-KEY END-START                         
               IF  NOT STAT-NORMAL(VUSER)                                       
                   MOVE VUSER          TO VSUB                                  
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
                                                                                
               IF  RTC-CODE = ZERO                                              
                   DISPLAY '                                        '           
                           '-Primary- ----------- W I N ------------'           
                                     UPON PRINTER                               
                   DISPLAY ' UserId            User Name            '           
                           'Comp# Ind Fnc    Unit     Org   Pos  Dir'           
                           '     Audit Reason'                                  
                                     UPON PRINTER                               
                   DISPLAY '-------- ------------------------------ '           
                           '----- --- --- ---------- ----- ----- ---'           
                           ' --------------------'                              
                                     UPON PRINTER                               
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
                                                                                
           CLOSE VUSERID                                                        
                 IESCNTL                                                        
                 IESLDUM                                                        
                 IESLDUV                                                        
                 COMASTR                                                        
                 COMASTI                                                        
                 COMASTA                                                        
                 COMASTW.                                                       
                                                                                
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
                                                                                
       C00-AUDIT-USER-ATTRIBUTES.                                               
                                                                                
           SET  FUNC-READNEXT(VUSER)   TO TRUE.                                 
           READ VUSERID NEXT RECORD INTO COMASTR-RECORD END-READ.               
           IF  NOT STAT-NORMAL(VUSER)                                           
               IF  NOT STAT-EOFILE(VUSER)                                       
                   MOVE VUSER          TO VSUB                                  
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
           SET  USER-IS-GOOD           TO TRUE.                                 
           ADD  1                      TO USER-COUNT.                           
                                                                                
      * get user's security attributes                                          
           MOVE LOW-VALUES             TO IUI-KEY.                              
           SET  IUI-USER-PROFILE       TO TRUE.                                 
           MOVE 1                      TO NAME-LENGTH.                          
           STRING VUSER-KEY               DELIMITED BY SPACE                    
               INTO IUI-US-USRIDNT   WITH POINTER NAME-LENGTH.                  
           SUBTRACT 1                FROM NAME-LENGTH.                          
                                                                                
           READ IESCNTL RECORD KEY IS IUI-KEY END-READ.                         
           IF  NOT STAT-NORMAL(IUI)                                             
               IF  NOT STAT-NOTFND(IUI)                                         
                   MOVE IUI            TO VSUB                                  
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT                                               
               END-IF                                                           
               ADD  1                  TO IBM-MISSING                           
               GO TO C10-CHECK-USER-MAPPING                                     
           END-IF.                                                              
                                                                                
      * extract settings from vsam security byte                                
           SET  BIT-DECODE-FROM-BYTE   TO TRUE.                                 
           MOVE IUI-US-IESVSAM         TO BIT-BYTE.                             
           CALL BITMAN              USING BITMAN-PARMS.                         
           IF  BIT-4-IS-ON                                                      
               SET  NOT-ICCF-USER      TO TRUE                                  
           ELSE                                                                 
               SET  IS-ICCF-USER       TO TRUE                                  
               IF  NAME-LENGTH = 4                                              
                   ADD  1              TO ICCF-USERS                            
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * extract settings for expiration date                                    
           IF  IUI-US-EXPJULN > ZEROES                                          
               MOVE   'C'              TO DTE-REQU                              
               CALL 'DTEMAN'        USING DTEMAN-PARMS                          
               IF  DTE-YYDDD > IUI-US-EXPJULN                                   
                   MOVE '*'            TO WORK-EXPIRED                          
                   ADD  1              TO EXPIRED-USERS                         
               ELSE                                                             
                   MOVE SPACE          TO WORK-EXPIRED                          
               END-IF                                                           
           ELSE                                                                 
               MOVE SPACE              TO WORK-EXPIRED                          
           END-IF.                                                              
                                                                                
      * extract settings for revokation date                                    
           IF  IUI-US-REVOKEJ > ZEROES                                          
               MOVE   'C'              TO DTE-REQU                              
               CALL 'DTEMAN'        USING DTEMAN-PARMS                          
               IF  DTE-YYDDD > IUI-US-REVOKEJ                                   
                   ADD  1              TO REVOKED-USERS                         
                   MOVE '*'            TO WORK-REVOKED                          
               ELSE                                                             
                   MOVE SPACE          TO WORK-REVOKED                          
               END-IF                                                           
           ELSE                                                                 
               MOVE SPACE              TO WORK-REVOKED                          
           END-IF.                                                              
                                                                                
      * don't bother auditing iccf or expired/revoked users                     
           IF (IS-ICCF-USER AND NAME-LENGTH = 4)                                
           OR  WORK-EXPIRED = '*'                                               
           OR  WORK-REVOKED = '*'                                               
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
       C10-CHECK-USER-MAPPING.                                                  
                                                                                
      * check user ldap mappings                                                
           MOVE VUSER-KEY              TO LDUV-MFUSRID.                         
           READ IESLDUV RECORD       INTO IESLDUM-RECORD                        
                                   KEY IS LDUV-MFUSRID END-READ.                
           IF  NOT STAT-NORMAL(LDUV)                                            
               IF  NOT STAT-NOTFND(LDUV)                                        
                   MOVE LDUV           TO VSUB                                  
                   MOVE LDUV-MFUSRID   TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT                                               
               END-IF                                                           
               SET  LDUM-USRMAP-RECORD TO TRUE                                  
               MOVE VUSER-KEY          TO LDUM-NETUSRID                         
               READ IESLDUM RECORD KEY IS LDUM-KEY END-READ                     
               IF  NOT STAT-NORMAL(LDUM)                                        
               AND NOT STAT-NOTFND(LDUM)                                        
                   MOVE LDUM           TO VSUB                                  
                   MOVE LDUM-NETUSRID  TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT                                               
               END-IF                                                           
      * don't bother auditing vse-only users                                    
               IF  STAT-NORMAL(LDUM)                                            
               AND LDUM-MFUSRID <= SPACES                                       
                   ADD  1              TO VSE-USERS                             
                   GO TO C99-EXIT                                               
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * begin auditing                                                          
                                                                                
           PERFORM P10-GET-SECURITY-ATTRIBUTES                                  
                                     THRU P15-EXIT.                             
                                                                                
           IF  LDGA-RET-CODE = ZERO                                             
                                                                                
               IF  LDGA-ATTR-VALUE(11 1) <= SPACES                              
                 SET  USER-IN-ERROR    TO TRUE                                  
                 ADD  1                TO MAIL-ERRORS                           
                 MOVE SPACES           TO VAR-TEXT                              
                 STRING VUSER-KEY ' '                                           
                        VUSER-NAME                                              
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                 MOVE 82               TO VAR-TEXTL                             
                 STRING 'AD MAIL missing'                                       
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
               END-IF                                                           
                                                                                
               INSPECT LDGA-ATTR-VALUE(1 1) CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
               IF  LDUM-MFUSRID NOT = LDGA-ATTR-VALUE(1 1)                      
                 SET  USER-IN-ERROR    TO TRUE                                  
                 ADD  1                TO MFID-ERRORS                           
                 MOVE SPACES           TO VAR-TEXT                              
                 STRING VUSER-KEY ' '                                           
                        VUSER-NAME                                              
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                 MOVE 82               TO VAR-TEXTL                             
                 IF  LDGA-ATTR-VALUE(1 1) <= SPACES                             
                   STRING 'AD MFID missing'                                     
                     DELIMITED BY SIZE INTO VAR-TEXT                            
                               WITH POINTER VAR-TEXTL                           
                 ELSE                                                           
                   STRING 'AD MFID mismatch='                                   
                          LDGA-ATTR-VALUE(1 1)(1:12)                            
                     DELIMITED BY SIZE INTO VAR-TEXT                            
                               WITH POINTER VAR-TEXTL                           
                 END-IF                                                         
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
               END-IF                                                           
                                                                                
               MOVE SPACES             TO WS-ATTRIBUTES                         
               PERFORM P30-PARSE-OUT-UNIT-VALUE THRU P35-EXIT                   
                                                                                
               MOVE LDGA-ATTR-VALUE(4 1)                                        
                                       TO WS-PRIMARY-CO                         
               MOVE LDGA-ATTR-VALUE(5 1)                                        
                                       TO WS-PRIMARY-IND                        
               INSPECT VUSER-PRIMARY-IND  CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(6 1)                                        
                                       TO WS-WIN-ORG                            
               INSPECT VUSER-WIN-ORG      CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(7 1)                                        
                                       TO WS-WIN-FUNCTION                       
               INSPECT VUSER-WIN-FUNCTION CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(8 1)                                        
                                       TO WS-WIN-DIRECTOR                       
               INSPECT VUSER-WIN-DIRECTOR CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(9 1)                                        
                                       TO WS-WIN-POSITION                       
               INSPECT VUSER-WIN-POSITION CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
                                                                                
               IF  VUSER-PRIMARY-CO   NOT = WS-PRIMARY-CO                       
               OR  VUSER-PRIMARY-IND  NOT = WS-PRIMARY-IND                      
               OR  VUSER-WIN-FUNCTION NOT = WS-WIN-FUNCTION                     
               OR  VUSER-WIN-UNIT     NOT = WS-WIN-UNIT                         
               OR  VUSER-WIN-ORG      NOT = WS-WIN-ORG                          
               OR  VUSER-WIN-POSITION NOT = WS-WIN-POSITION                     
               OR  VUSER-WIN-DIRECTOR NOT = WS-WIN-DIRECTOR                     
                 SET  USER-IN-ERROR    TO TRUE                                  
                 ADD  1                TO ATTRS-DIFFER                          
                 MOVE SPACES           TO VAR-TEXT                              
                 MOVE 1                TO VAR-TEXTL                             
                 STRING VUSER-KEY ' '                                           
                        VUSER-NAME ' '                                          
                        VUSER-PRIMARY-CO '  '                                   
                        VUSER-PRIMARY-IND '  '                                  
                        VUSER-WIN-FUNCTION ' '                                  
                        VUSER-WIN-UNIT ' '                                      
                        VUSER-WIN-ORG ' '                                       
                        VUSER-WIN-POSITION '  '                                 
                        VUSER-WIN-DIRECTOR '  '                                 
                        'User settings differ'                                  
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
                 MOVE SPACES           TO VAR-TEXT                              
                 MOVE 1                TO VAR-TEXTL                             
                 STRING VUSER-KEY ' '                                           
                        LDGA-ATTR-VALUE(12 1)(1:30) ' '                         
                        WS-PRIMARY-CO '  '                                      
                        WS-PRIMARY-IND '  '                                     
                        WS-WIN-FUNCTION ' '                                     
                        WS-WIN-UNIT ' '                                         
                        WS-WIN-ORG ' '                                          
                        WS-WIN-POSITION '  '                                    
                        WS-WIN-DIRECTOR '  '                                    
                        'from AD attributes'                                    
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
               END-IF                                                           
                                                                                
               PERFORM P40-CHECK-AD-ATTRIBUTES THRU P45-EXIT                    
           END-IF.                                                              
                                                                                
           IF  USER-IN-ERROR                                                    
               ADD  1                  TO USER-ERRORS                           
           END-IF.                                                              
                                                                                
       C99-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    OTHER PERFORMED ROUTINES                                    *        
      ******************************************************************        
                                                                                
       P10-GET-SECURITY-ATTRIBUTES.                                             
           INITIALIZE IESLDGA-COMMAREA.                                         
           MOVE LENGTH OF IESLDGA-COMMAREA                                      
                                       TO LDGA-AREA-LENGTH.                     
           MOVE LDUM-NETUSRID          TO LDGA-USER-ID.                         
           MOVE '&(objectClass=person)(objectClass=user)'                       
                                       TO LDGA-SEARCH-FILTER.                   
           MOVE 12                     TO LDGA-ATTR-COUNT.                      
                                                                                
           MOVE 'mfid'                 TO LDGA-ATTR-NAME(1).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(1).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(1).                  
                                                                                
           MOVE 'ou'                   TO LDGA-ATTR-NAME(2).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(2).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(2).                  
                                                                                
           MOVE 'department'           TO LDGA-ATTR-NAME(3).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(3).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(3).                  
                                                                                
           MOVE 'company'              TO LDGA-ATTR-NAME(4).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(4).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(4).                  
                                                                                
           MOVE 'industryLetter'       TO LDGA-ATTR-NAME(5).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(5).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(5).                  
                                                                                
           MOVE 'WinOrg'               TO LDGA-ATTR-NAME(6).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(6).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(6).                  
                                                                                
           MOVE 'WinFunction'          TO LDGA-ATTR-NAME(7).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(7).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(7).                  
                                                                                
           MOVE 'WinDirector'          TO LDGA-ATTR-NAME(8).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(8).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(8).                  
                                                                                
           MOVE 'WinPosition'          TO LDGA-ATTR-NAME(9).                    
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(9).                 
           MOVE 1                      TO LDGA-VALUE-COUNT(9).                  
                                                                                
           MOVE 'distinguishedName'    TO LDGA-ATTR-NAME(10).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(10).                
           MOVE 1                      TO LDGA-VALUE-COUNT(10).                 
                                                                                
           MOVE 'mail'                 TO LDGA-ATTR-NAME(11).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(11).                
           MOVE 1                      TO LDGA-VALUE-COUNT(11).                 
                                                                                
           MOVE 'displayName'          TO LDGA-ATTR-NAME(12).                   
           MOVE LENGTH OF LDGA-ATTR-VALUE                                       
                                       TO LDGA-VALUE-LENGTH(12).                
           MOVE 1                      TO LDGA-VALUE-COUNT(12).                 
                                                                                
           CALL IESLDGAB            USING IESLDGA-COMMAREA.                     
                                                                                
           IF  LDGA-RET-CODE NOT = ZERO                                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 82                 TO VAR-TEXTL                             
               MOVE LDGA-RET-CODE      TO VAR-EDIT                              
               PERFORM P20-JUSTIFY-LEFT THRU P25-EXIT                           
               STRING 'LDAP error rc='    DELIMITED BY SIZE                     
                      VAR-EDIT            DELIMITED BY SPACE                    
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL                     
               MOVE LDGA-LDAP-CODE     TO VAR-EDIT                              
               PERFORM P20-JUSTIFY-LEFT THRU P25-EXIT                           
               STRING ',rs='              DELIMITED BY SIZE                     
                      VAR-EDIT            DELIMITED BY SPACE                    
                       INTO VAR-TEXT WITH POINTER VAR-TEXTL                     
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
               GO TO P15-EXIT                                                   
           END-IF.                                                              
       P15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P20-JUSTIFY-LEFT.                                                        
           SET TXT-REQUEST-JUSTIFY-L TO TRUE                                    
           MOVE +1                     TO TXT-PNTR                              
           MOVE LOW-VALUES             TO TXT-DLMS                              
           MOVE LENGTH OF VAR-EDIT     TO TXT-PARM-STRLEN                       
           MOVE VAR-EDIT               TO TXT-PARM-STRING                       
           CALL 'TXTMAN'            USING TXTMAN-PARMS                          
                                          TXTMAN-BUFFER                         
                                          TXTMAN-STRING.                        
           MOVE TXT-PARM-STRING        to VAR-EDIT.                             
       P25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P30-PARSE-OUT-UNIT-VALUE.                                                
           MOVE LDGA-ATTR-VALUE(2 1)   TO WS-WIN-UNIT.                          
           INSPECT WS-WIN-UNIT            CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE.                
           IF  WS-WIN-UNIT <= SPACES                                            
               MOVE LDGA-ATTR-VALUE(10 1)                                       
                                       TO VAR-TEXT                              
               UNSTRING VAR-TEXT DELIMITED BY ','                               
                   INTO WORD1 WORD2 WORD3 WORD4 WORD5                           
               IF  'OU=' = WORD2(1:3) AND WORD3(1:3)                            
                   MOVE WORD2(4:)      TO WS-WIN-UNIT                           
                   INSPECT WS-WIN-UNIT    CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               END-IF                                                           
           END-IF.                                                              
       P35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P40-CHECK-AD-ATTRIBUTES.                                                 
           IF  WS-ATTRIBUTES <= SPACES                                          
               GO TO P45-EXIT                                                   
           END-IF.                                                              
                                                                                
           IF  WS-WIN-FUNCTION NOT = 'WW' AND 'WG' AND 'WS'                     
                                 AND 'CO' AND 'AC' AND 'LC'                     
                                 AND 'VP' AND 'XT'                              
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO FUNC-UNKNOWN                          
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION                                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Not a known Function'                                    
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF (WS-WIN-FUNCTION = 'WW' OR 'WG' OR 'WS'                           
                              OR 'CO' OR 'VP')                                  
           AND WS-WIN-UNIT NOT = 'ACCT'       AND 'AL'                          
                             AND 'AP'         AND 'COMM'                        
                             AND 'CONTRACTOR' AND 'CUSTSVC'                     
                             AND 'DTAMGMT'    AND 'EXEC'                        
                             AND 'FIN'        AND 'HR'                          
                             AND 'INVSTR'     AND 'IT'                          
                             AND 'MKT'        AND 'PRICING'                     
                             AND 'PURCHS'     AND 'ROMS'                        
                             AND 'SPS'        AND 'TRAINING'                    
                             AND 'WCMS'       AND 'WINSUP'                      
                             AND 'WSS'        AND 'DAYTON'                      
                             AND 'DENVER'     AND 'JACKSONVLE'                  
                             AND 'NHAVEN'     AND 'RCHMND'                      
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO UNIT-DISAGREE                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION ' ' WS-WIN-UNIT                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Ou disagrees w/ Fnc'                                     
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF (WS-WIN-FUNCTION = 'AC' OR 'LC' OR 'XT')                          
           AND (WS-WIN-UNIT    = 'ACCT'       OR 'AL'                           
                              OR 'AP'         OR 'COMM'                         
                              OR 'CONTRACTOR' OR 'CUSTSVC'                      
                              OR 'DTAMGMT'    OR 'EXEC'                         
                              OR 'FIN'        OR 'HR'                           
                              OR 'INVSTR'     OR 'IT'                           
                              OR 'MKT'        OR 'PRICING'                      
                              OR 'PURCHS'     OR 'ROMS'                         
                              OR 'SPS'        OR 'TRAINING'                     
                              OR 'WCMS'       OR 'WINSUP'                       
                              OR 'WSS'        OR 'DAYTON'                       
                              OR 'DENVER'     OR 'JACKSONVLE'                   
                              OR 'NHAVEN'     OR 'RCHMND')                      
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO UNIT-DISAGREE                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION ' ' WS-WIN-UNIT                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Ou disagrees w/ Fnc'                                     
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-POSITION NOT = 'EMPL' AND 'PRES' AND 'SIC'                
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO POS-UNKNOWN                           
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 72                 TO VAR-TEXTL                             
               STRING WS-WIN-POSITION                                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Not a known Position'                                    
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-POSITION     = 'PRES'                                     
           AND WS-WIN-DIRECTOR NOT = 'Y'                                        
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO PRES-DISAGREE                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 72                 TO VAR-TEXTL                             
               STRING WS-WIN-POSITION '  ' WS-WIN-DIRECTOR                      
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'PRES s/b Dir=Y'                                          
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-POSITION     = 'SIC'                                      
           AND WS-WIN-FUNCTION NOT = 'LC'                                       
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO SIC-DISAGREE                          
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION                                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 72                 TO VAR-TEXTL                             
               STRING WS-WIN-POSITION                                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'SIC s/b Fnc=LC'                                          
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-FUNCTION     = 'VP'                                       
           AND WS-WIN-UNIT     NOT = 'AL' AND 'EXEC' AND 'MKT'                  
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO VP-DISAGREE                           
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION ' ' WS-WIN-UNIT                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Disagree w/ Fnc=VP'                                      
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-UNIT         = 'EXEC'                                     
           AND WS-WIN-FUNCTION NOT = 'WW' AND 'WS' AND 'WG'                     
                                 AND 'VP' AND 'CO'                              
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO EXEC-DISAGREE                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION ' ' WS-WIN-UNIT                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Disagree w/ ou=EXEC'                                     
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-UNIT         = 'AL'                                       
           AND WS-WIN-FUNCTION NOT = 'VP'                                       
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO AL-DISAGREE                           
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION ' ' WS-WIN-UNIT                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Disagree w/ ou=AL'                                       
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           MOVE SPACES                 TO COMASTR-RECORD.                       
           MOVE WS-PRIMARY-CO          TO COMSTR-KEY.                           
           READ COMASTR RECORD       INTO COMASTR-RECORD                        
                                   KEY IS COMSTR-KEY END-READ.                  
           IF  NOT STAT-NORMAL(COMSTR)                                          
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO COMP-ERRORS                           
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 41                 TO VAR-TEXTL                             
               STRING WS-PRIMARY-CO                                             
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               IF  WS-PRIMARY-CO <= SPACES                                      
                 STRING 'Company is missing'                                    
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               ELSE                                                             
                 STRING 'Company not on file'                                   
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               END-IF                                                           
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-PRIMARY-IND NOT = COMAST-IKEY-IND                             
           AND WS-PRIMARY-IND NOT = 'B'                                         
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO WRONG-INDUSTRY                        
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 41                 TO VAR-TEXTL                             
               STRING WS-PRIMARY-CO '  ' WS-PRIMARY-IND                         
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'Industry s/b ' COMAST-IKEY-IND                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-FUNCTION = 'AC'                                           
           AND WS-WIN-ORG NOT = COMAST-AKEY-AREA                                
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO WRONG-WINAREA                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 41                 TO VAR-TEXTL                             
               STRING WS-PRIMARY-CO                                             
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 51                 TO VAR-TEXTL                             
               STRING WS-WIN-FUNCTION                                           
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 66                 TO VAR-TEXTL                             
               STRING WS-WIN-ORG                                                
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               STRING 'WinOrg s/b ' COMAST-AKEY-AREA                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           MOVE SPACES                 TO COMASTR-RECORD.                       
           MOVE WS-PRIMARY-IND         TO COMSTI-KEY.                           
           START COMASTI KEY >= COMSTI-KEY END-START.                           
           READ COMASTI NEXT RECORD INTO COMASTR-RECORD END-READ.               
           IF  NOT STAT-NORMAL(COMSTI)                                          
           OR  COMAST-IKEY-IND NOT = WS-PRIMARY-IND                             
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO INDUSTRY-ERROR                        
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 47                 TO VAR-TEXTL                             
               STRING WS-PRIMARY-IND                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               IF  WS-PRIMARY-IND <= SPACES                                     
                 STRING 'Industry is missing'                                   
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               ELSE                                                             
                 STRING 'Industry not on file'                                  
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               END-IF                                                           
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WS-WIN-FUNCTION = 'AC'                                           
             MOVE SPACES               TO COMASTR-RECORD                        
             MOVE WS-WIN-ORG(1:3)      TO COMSTA-KEY                            
             START COMASTA KEY >= COMSTA-KEY END-START                          
             READ COMASTA NEXT RECORD INTO COMASTR-RECORD END-READ              
             IF  NOT STAT-NORMAL(COMSTI)                                        
             OR  COMAST-AKEY-AREA NOT = WS-WIN-ORG(1:3)                         
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO WINAREA-ERROR                         
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 66                 TO VAR-TEXTL                             
               STRING WS-WIN-ORG                                                
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               IF  WS-WIN-ORG <= SPACES                                         
                 STRING 'Area is missing'                                       
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               ELSE                                                             
                 STRING 'Area not on file'                                      
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               END-IF                                                           
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
             END-IF                                                             
           END-IF.                                                              
                                                                                
           IF  WS-WIN-UNIT = 'AL'                                               
             MOVE SPACES               TO COMASTR-RECORD                        
             MOVE WS-WIN-ORG(1:1)      TO COMSTW-KEY                            
             START COMASTW KEY >= COMSTW-KEY END-START                          
             READ COMASTW NEXT RECORD INTO COMASTR-RECORD END-READ              
             IF  NOT STAT-NORMAL(COMSTI)                                        
             OR  COMAST-WKEY-WIN NOT = WS-WIN-ORG(1:1)                          
               SET  USER-IN-ERROR      TO TRUE                                  
               ADD  1                  TO WINRGN-ERROR                          
               MOVE SPACES             TO VAR-TEXT                              
               STRING VUSER-KEY ' '                                             
                      VUSER-NAME ' '                                            
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
               MOVE 66                 TO VAR-TEXTL                             
               STRING WS-WIN-ORG                                                
                 DELIMITED BY SIZE   INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               MOVE 82                 TO VAR-TEXTL                             
               IF  WS-WIN-ORG <= SPACES                                         
                 STRING 'WinRgn is missing'                                     
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               ELSE                                                             
                 STRING 'WinRgn not on file'                                    
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                             WITH POINTER VAR-TEXTL                             
               END-IF                                                           
               SUBTRACT 1            FROM VAR-TEXTL                             
               DISPLAY VAR-TEXT(1:VAR-TEXTL)                                    
                                     UPON PRINTER                               
             END-IF                                                             
           END-IF.                                                              
       P45-EXIT.                                                                
           EXIT.                                                                
                                                                                

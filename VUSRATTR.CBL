      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VUSRATTR.                                                 
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  SEPTEMBER 2019.                                           
       DATE-COMPILED.                                                           
       INSTALLATION.  WINSUPPLY GROUP SERVICES.                                 
       SECURITY.      BATCH.                                                    
      *REMARKS.       GET ACTIVE DIRECTORY ATTRIBUTES FOR USER(S).              
      *                                                                         
      * note that this program sets vuserid attributes upon mapping.            
      * any changes here may also require changes in program WLSNPGM            
      *                                       and in program VUPMPGM            
      *                                       and in program VUSRPGM            
      *                                       and in program VUSRAUDT.          
      *                                                                         
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 09/09/2019 DLC ORIGINAL PROGRAM.                                        
/DLC0/* 01/20/2020 DLC HONOR VUSERID NO REFRESH FLAG.                           
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
/DLC0/     UPSI-0 ON STATUS IS FORCED-REFRESH.                                  
                                                                                
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
                                                                                
       FD  VUSERID                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 127 TO 174 CHARACTERS                                         
             DEPENDING ON VUSERID-RECL.                                         
       COPY VUSERID.                                                            
                                                                                
       FD  IESLDUV.                                                             
       COPY IESLDUM.                                                            
                                                                                
      ******************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-FIELDS.                                                           
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.        
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.            
         03  THIS-PGM                  PIC  X(8)    VALUE 'VUSRATTR'.           
         03  IESLDGAB                  PIC  X(08)   VALUE 'IESLDGAB'.           
                                                                                
         03  VUSER                     PIC S9(04)   BINARY VALUE 1.             
         03  LDUM                      PIC S9(04)   BINARY VALUE 2.             
                                                                                
         03  VUSERID-RECL              PIC  9(05)   VALUE ZEROES.               
         03  VAR-EDIT                  PIC  Z(6)9-.                             
         03  VAR-TEXTL                 PIC S9(04)   BINARY VALUE ZEROES.        
         03  VAR-TEXT                  PIC  X(120)  VALUE SPACES.               
                                                                                
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
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       COPY RTCMAN.                                                             
                                                                                
       COPY TXTMAN.                                                             
                                                                                
       COPY VSMSTATW.                                                           
                                                                                
      ******************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
       COPY EXECPARM.                                                           
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION USING EXECPARM-PARMS.                                 
                                                                                
      ******************************************************************        
      *    MAINLINE ROUTINE                                            *        
      ******************************************************************        
       A00-MAINLINE-ROUTINE.                                                    
                                                                                
           PERFORM B10-INITIALIZATION THRU B15-EXIT.                            
                                                                                
           PERFORM C00-PROCESS-VUSERID THRU C99-EXIT-PROCESS                    
             UNTIL STAT-EOFILE(VUSER)                                           
                OR RTC-CODE NOT = ZERO.                                         
                                                                                
           PERFORM B20-TERMINATION THRU B25-EXIT.                               
                                                                                
           GOBACK.                                                              
                                                                                
      /*****************************************************************        
      *    PROGRAM INITIALIZATION ROUTINE                              *        
      ******************************************************************        
       B10-INITIALIZATION.                                                      
                                                                                
           COPY BATCHINI.                                                       
                                                                                
           MOVE ZEROES                 TO RTC-CODE.                             
                                                                                
           MOVE 'VUSERID'              TO VSAM-FILE(VUSER).                     
           MOVE LENGTH OF VUSER-KEY    TO VSAM-KEYL(VUSER).                     
                                                                                
           MOVE 'IESLDUV'              TO VSAM-FILE(LDUM).                      
           MOVE LENGTH OF LDUM-MFUSRID TO VSAM-KEYL(LDUM).                      
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                    
               SET  FUNC-OPEN(VSUB)    TO TRUE                                  
           END-PERFORM.                                                         
                                                                                
           OPEN I-O   VUSERID                                                   
                INPUT IESLDUV.                                                  
                                                                                
           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                         
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.                   
                                                                                
           IF  RTC-CODE = ZERO                                                  
               PERFORM WITH TEST BEFORE                                         
                 VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                
                   SET FUNC-READ(VSUB) TO TRUE                                  
               END-PERFORM                                                      
                                                                                
               IF  EXEC-PLEN > ZERO AND <= 8                                    
                 MOVE EXEC-PARM(1:EXEC-PLEN)                                    
                                       TO VUSER-KEY                             
                 MOVE VUSER            TO VSUB                                  
                 SET  FUNC-START(VSUB) TO TRUE                                  
                 START VUSERID KEY = VUSER-KEY END-START                        
                 IF  NOT STAT-NORMAL(VSUB)                                      
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                 END-IF                                                         
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
                                                                                
           CLOSE VUSERID.                                                       
                                                                                
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
                                                                                
       C00-PROCESS-VUSERID.                                                     
                                                                                
           MOVE VUSER                  TO VSUB                                  
           SET  FUNC-READNEXT(VSUB)    TO TRUE.                                 
           READ VUSERID NEXT RECORD END-READ.                                   
           IF  NOT STAT-NORMAL(VSUB)                                            
               IF  NOT STAT-EOFILE(VSUB)                                        
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
               GO TO C99-EXIT-PROCESS                                           
           END-IF.                                                              
                                                                                
/DLC0/     IF  NOT VUSER-DO-REFRESH                                             
/DLC0/     AND NOT FORCED-REFRESH                                               
/DLC0/         GO TO C90-SKIP-REFRESH                                           
/DLC0/     END-IF.                                                              
                                                                                
           MOVE VUSER-KEY              TO LDUM-MFUSRID.                         
                                                                                
           READ IESLDUV KEY IS LDUM-MFUSRID END-READ.                           
           IF  NOT STAT-NORMAL(LDUM)                                            
               IF  NOT STAT-NOTFND(LDUM)                                        
                   MOVE LDUM           TO VSUB                                  
                   MOVE LDUM-MFUSRID   TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT-PROCESS                                       
               END-IF                                                           
               SET  STAT-NORMAL(LDUM)  TO TRUE                                  
               MOVE LDUM-MFUSRID       TO LDUM-NETUSRID                         
               INSPECT LDUM-NETUSRID      CONVERTING UPPER-CASE                 
                                                  TO LOWER-CASE                 
           END-IF.                                                              
                                                                                
           PERFORM P10-GET-SECURITY-ATTRIBUTES                                  
                                     THRU P15-EXIT.                             
           IF  LDGA-RET-CODE = ZERO                                             
               MOVE LENGTH OF VUSERID-RECORD                                    
                                       TO VUSERID-RECL                          
               INSPECT LDGA-ATTR-VALUE(1 1) CONVERTING LOWER-CASE               
                                                    TO UPPER-CASE               
               IF  LDUM-MFUSRID NOT = LDGA-ATTR-VALUE(1 1)                      
                 MOVE SPACES           TO VAR-TEXT                              
                 MOVE 1                TO VAR-TEXTL                             
                 STRING LDUM-MFUSRID      DELIMITED BY SPACE                    
                        ' does not match Active'                                
                        ' Directory MFID ' DELIMITED BY SIZE                    
                        '(' LDGA-ATTR-VALUE(1 1) ')'                            
                  DELIMITED BY SPACE INTO VAR-TEXT                              
                                  POINTER VAR-TEXTL                             
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
               END-IF                                                           
               IF  LDGA-ATTR-VALUE(11 1) <= SPACES                              
                 MOVE SPACES           TO VAR-TEXT                              
                 MOVE 1                TO VAR-TEXTL                             
                 STRING LDUM-MFUSRID      DELIMITED BY SPACE                    
                        ' does not have an AD mail attribute value'             
                  DELIMITED BY SIZE  INTO VAR-TEXT                              
                                  POINTER VAR-TEXTL                             
                 SUBTRACT 1          FROM VAR-TEXTL                             
                 DISPLAY VAR-TEXT(1:VAR-TEXTL)                                  
                                     UPON PRINTER                               
               END-IF                                                           
               PERFORM P30-PARSE-OUT-UNIT-VALUE THRU P35-EXIT                   
               MOVE LDGA-ATTR-VALUE(3 1)                                        
                                       TO VUSER-DEPT-NAME                       
               MOVE LDGA-ATTR-VALUE(4 1)                                        
                                       TO VUSER-PRIMARY-CO                      
               MOVE LDGA-ATTR-VALUE(5 1)                                        
                                       TO VUSER-PRIMARY-IND                     
               INSPECT VUSER-PRIMARY-IND  CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(6 1)                                        
                                       TO VUSER-WIN-ORG                         
               INSPECT VUSER-WIN-ORG      CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(7 1)                                        
                                       TO VUSER-WIN-FUNCTION                    
               INSPECT VUSER-WIN-FUNCTION CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(8 1)                                        
                                       TO VUSER-WIN-DIRECTOR                    
               INSPECT VUSER-WIN-DIRECTOR CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               MOVE LDGA-ATTR-VALUE(9 1)                                        
                                       TO VUSER-WIN-POSITION                    
               INSPECT VUSER-WIN-POSITION CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
      * overrides for test users, only                                          
               IF  VUSER-WIN-UNIT = 'TEST'                                      
                   MOVE VUSER-WIN-UNIT TO VUSER-DEPT-NAME                       
                   EVALUATE TRUE                                                
                   WHEN LDUM-MFUSRID = 'WZTESTAC'                               
                     MOVE 'LINCOLWWA'  TO VUSER-WIN-UNIT                        
                     MOVE 'M01'        TO VUSER-WIN-ORG                         
                     MOVE 'AC'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00783'      TO VUSER-PRIMARY-CO                      
                     MOVE 'W'          TO VUSER-PRIMARY-IND                     
                   WHEN LDUM-MFUSRID = 'WZTESTAL'                               
                     MOVE 'AL'         TO VUSER-WIN-UNIT                        
                     MOVE 'A00'        TO VUSER-WIN-ORG                         
                     MOVE 'VP'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00961'      TO VUSER-PRIMARY-CO                      
                     MOVE 'M'          TO VUSER-PRIMARY-IND                     
                   WHEN LDUM-MFUSRID = 'WZTSTEXC'                               
                     MOVE 'EXEC'       TO VUSER-WIN-UNIT                        
                     MOVE '00924'      TO VUSER-WIN-ORG                         
                     MOVE 'WW'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00924'      TO VUSER-PRIMARY-CO                      
                     MOVE 'O'          TO VUSER-PRIMARY-IND                     
                   WHEN LDUM-MFUSRID = 'WZTSTLCP'                               
                     MOVE 'DAYTONWNE'  TO VUSER-WIN-UNIT                        
                     MOVE '00108'      TO VUSER-WIN-ORG                         
                     MOVE 'LC'         TO VUSER-WIN-FUNCTION                    
                     MOVE '00108'      TO VUSER-PRIMARY-CO                      
                     MOVE 'H'          TO VUSER-PRIMARY-IND                     
                   END-EVALUATE                                                 
                   MOVE 'Y'            TO VUSER-WIN-DIRECTOR                    
               END-IF                                                           
      *                                                                         
               EVALUATE TRUE                                                    
               WHEN VUSER-WIN-UNIT = 'EXEC' OR 'IT' OR 'MKT'                    
                   MOVE '9'            TO VUSER-DAPSCO                          
                   IF  VUSER-WIN-FUNCTION = 'WW' OR 'CO' OR 'VP'                
                       MOVE '99'       TO VUSER-CLASS                           
                   ELSE                                                         
                     IF  VUSER-WIN-DIRECTOR = 'Y'                               
                       MOVE '95'       TO VUSER-CLASS                           
                     ELSE                                                       
                       MOVE '91'       TO VUSER-CLASS                           
                     END-IF                                                     
                   END-IF                                                       
               WHEN VUSER-WIN-UNIT = 'AL'                                       
               OR   VUSER-WIN-FUNCTION = 'VP'                                   
                   SET VUSER-AREA-LEADER TO TRUE                                
                   MOVE '99'           TO VUSER-CLASS                           
                   MOVE '*'            TO VUSER-IND-LIST(1:1)                   
                   MOVE VUSER-WIN-ORG(1:1)                                      
                                       TO VUSER-IND-LIST(2:)                    
                   MOVE VUSER-IND-LIST                                          
                                       TO VUSER-DFQ-SELECT                      
               WHEN VUSER-WIN-FUNCTION = 'LC' OR 'AC'                           
                   SET VUSER-WISE-USER-OR-AC TO TRUE                            
                   IF  VUSER-WIN-DIRECTOR = 'Y'                                 
                       MOVE '21'       TO VUSER-CLASS                           
                   ELSE                                                         
                       MOVE '20'       TO VUSER-CLASS                           
                   END-IF                                                       
                   MOVE VUSER-PRIMARY-CO(3:3)                                   
                                       TO VUSER-IND-LIST                        
                   IF  VUSER-WIN-FUNCTION = 'AC'                                
                       MOVE '*'        TO VUSER-DFQ-SELECT(1:1)                 
                       MOVE VUSER-WIN-ORG                                       
                                       TO VUSER-DFQ-SELECT(2:)                  
                   END-IF                                                       
               END-EVALUATE                                                     
                                                                                
               MOVE VUSER              TO VSUB                                  
               SET  FUNC-REWRITE(VSUB) TO TRUE                                  
               REWRITE VUSERID-RECORD END-REWRITE                               
               IF  NOT STAT-NORMAL(VSUB)                                        
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT-PROCESS                                       
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC0/ C90-SKIP-REFRESH.                                                        
                                                                                
           IF  EXEC-PLEN > ZERO AND <= 8                                        
               SET  STAT-EOFILE(VUSER) TO TRUE                                  
           END-IF.                                                              
                                                                                
       C99-EXIT-PROCESS.                                                        
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
           MOVE 11                     TO LDGA-ATTR-COUNT.                      
                                                                                
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
                                                                                
           CALL IESLDGAB            USING IESLDGA-COMMAREA.                     
                                                                                
           IF  LDGA-RET-CODE NOT = ZERO                                         
               MOVE SPACES             TO VAR-TEXT                              
               MOVE 1                  TO VAR-TEXTL                             
               STRING LDUM-NETUSRID       DELIMITED BY SPACE                    
                      ' unable to retrieve attrs - rc='                         
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                                  POINTER VAR-TEXTL                             
               MOVE LDGA-RET-CODE      TO VAR-EDIT                              
               PERFORM P20-JUSTIFY-LEFT THRU P25-EXIT                           
               STRING VAR-EDIT            DELIMITED BY SPACE                    
                      ', rsn='                                                  
                   DELIMITED BY SIZE INTO VAR-TEXT                              
                                  POINTER VAR-TEXTL                             
               MOVE LDGA-LDAP-CODE     TO VAR-EDIT                              
               PERFORM P20-JUSTIFY-LEFT THRU P25-EXIT                           
               STRING VAR-EDIT            DELIMITED BY SPACE                    
                                     INTO VAR-TEXT                              
                                  POINTER VAR-TEXTL                             
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
           MOVE LDGA-ATTR-VALUE(2 1)   TO VUSER-WIN-UNIT.                       
           INSPECT VUSER-WIN-UNIT         CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE.                
           IF VUSER-WIN-UNIT <= SPACES                                          
               MOVE LDGA-ATTR-VALUE(10 1)                                       
                                       TO VAR-TEXT                              
               UNSTRING VAR-TEXT DELIMITED BY ','                               
                   INTO WORD1 WORD2 WORD3 WORD4 WORD5                           
               IF  'OU=' = WORD2(1:3) AND WORD3(1:3)                            
                   MOVE WORD2(4:)      TO VUSER-WIN-UNIT                        
                   INSPECT VUSER-WIN-UNIT CONVERTING LOWER-CASE                 
                                                  TO UPPER-CASE                 
               END-IF                                                           
           END-IF.                                                              
       P35-EXIT.                                                                
           EXIT.                                                                
                                                                                

      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECUSER.                                                 
       AUTHOR.        DAVE L CLARK.                                             
       DATE-WRITTEN.  JAN 2008.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.                              
       SECURITY.      BATCH.                                                    
      *REMARKS.       CICS RESOURCE SECURITY USERNAME INIT.                     
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 01/14/2008 DLC ORIGINAL PROGRAM.                                        
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
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS IUI-KEY                                            
               FILE STATUS IS FILE1-STAT                                        
                              FILE1-FDBK.                                       
                                                                                
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
                                                                                
       FD  IESCNTL                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 100 TO 1000 DEPENDING ON IUI-LRECL.                           
       COPY IESCNTL.                                                            
                                                                                
      ******************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-FIELDS.                                                           
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.        
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.            
         03  THIS-PGM                  PIC  X(8)    VALUE 'VSECUSER'.           
         03  SBFTLOAD                  PIC  X(8)    VALUE 'SBFTLOAD'.           
         03  PGM-DFHSNT                PIC  X(8)    VALUE 'DFHSNT  '.           
                                                                                
         03  IUI                       PIC S9(04)   BINARY VALUE 1.             
                                                                                
         03  LEN                       PIC S9(04)   BINARY.                     
         03  SUB                       PIC S9(04)   BINARY.                     
         03  IUI-LRECL                 PIC  9(05)   VALUE 65535.                
                                                                                
         03  IUI-USERID                PIC  X(8).                               
         03  SNT-USERID                PIC  X(8).                               
                                                                                
         03  DFHSNT-SAVE               PIC S9(9)    BINARY.                     
         03  DFHSNT-PTR                             POINTER.                    
         03  DFHSNT-OFFSET   REDEFINES DFHSNT-PTR   PIC S9(9) BINARY.           
                                                                                
         03  LOWER-CASE   PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.        
         03  UPPER-CASE   PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.        
                                                                                
      *  WORK AREA FOR BYTE-ENCODED NUMBERS                                     
       01  DOUBLE-WORD                 PIC S9(18)   BINARY.                     
       01  EIGHT-BYTES       REDEFINES DOUBLE-WORD.                             
         03  FILLER                    PIC  X(4).                               
         03  FULL-WORD                 PIC S9(9)    BINARY.                     
         03  FOUR-BYTES      REDEFINES FULL-WORD.                               
           05  HI-MSB                  PIC  X.                                  
           05  THREE-BYTES.                                                     
             07  HI-LSB                PIC  X.                                  
             07  HALF-WORD             PIC S9(4)    BINARY.                     
             07  TWO-BYTES   REDEFINES HALF-WORD.                               
               09  LO-MSB              PIC  X.                                  
               09  LO-LSB              PIC  X.                                  
                                                                                
       COPY RTCMAN.                                                             
                                                                                
       COPY VSMSTATW.                                                           
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
      * SIGN-ON TABLE                                                           
       01  DFHSNT.                                                              
         03  SNT-ENTRY-LENGTH          PIC S9(4)    BINARY.                     
         03  SNT-FILLER-BYTE1          PIC  X.                                  
         03  SNT-TIMEOUT-BYTE          PIC  X.                                  
         03  SNT-PASSWORD              PIC  X(8).                               
         03  SNT-USER-BLK-PTR                       POINTER.                    
         03  SNT-SEC-CLASSES           PIC  X(8).                               
         03  SNT-RSC-CLASSES           PIC  X(3).                               
         03  SNT-OPR-PRIORITY          PIC  X.                                  
         03  SNT-OPR-CLASSES           PIC  X(3).                               
         03  SNT-OPR-ID                PIC  X(3).                               
         03  SNT-USER-IDL              PIC S9(4)    BINARY.                     
         03  SNT-USER-ID               PIC  X(8).                               
         03  SNT-USE-COUNT             PIC S9(4)    BINARY.                     
         03  SNT-NAT-LANGUAGE          PIC  X.                                  
         03  SNT-USER-NAMEL            PIC S9(4)    BINARY.                     
         03  SNT-USER-NAME             PIC  X(20).                              
                                                                                
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
                                                                                
           PERFORM C00-INIT-USERNAME THRU C99-EXIT-INIT                         
             UNTIL STAT-EOFILE(IUI)                                             
                OR RTC-CODE NOT = ZERO                                          
                                                                                
           PERFORM B20-TERMINATION THRU B25-EXIT.                               
                                                                                
           GOBACK.                                                              
                                                                                
      /*****************************************************************        
      *    PROGRAM INITIALIZATION ROUTINE                              *        
      ******************************************************************        
       B10-INITIALIZATION.                                                      
                                                                                
           COPY BATCHINI.                                                       
                                                                                
           IF  RTC-CODE = ZERO                                                  
                                                                                
             MOVE 'IESCNTL'            TO VSAM-FILE(IUI)                        
                                                                                
             MOVE LENGTH OF IUI-KEY    TO VSAM-KEYL(IUI)                        
                                                                                
             PERFORM WITH TEST BEFORE                                           
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                 SET FUNC-OPEN(VSUB)   TO TRUE                                  
             END-PERFORM                                                        
                                                                                
             IF  WITH-UPDATES                                                   
                 OPEN I-O   IESCNTL                                             
             ELSE                                                               
                 OPEN INPUT IESCNTL                                             
             END-IF                                                             
                                                                                
             PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                       
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                                                                                
             IF  RTC-CODE = ZERO                                                
                 MOVE LOW-VALUES       TO IUI-KEY                               
                 SET  IUI-USER-RECORD  TO TRUE                                  
                 SET  FUNC-START(IUI)  TO TRUE                                  
                 START IESCNTL KEY >= IUI-KEY END-START                         
             END-IF                                                             
                                                                                
             IF  RTC-CODE = ZERO                                                
                 PERFORM WITH TEST BEFORE                                       
                   VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL              
                     SET FUNC-READ(VSUB) TO TRUE                                
                 END-PERFORM                                                    
                 IF  RTC-CODE = ZERO                                            
                   IF  WITH-UPDATES                                             
                     DISPLAY THIS-PGM ': PROCESSING WITH UPDATES'               
                                     UPON CONSOLE                               
                   ELSE                                                         
                     DISPLAY THIS-PGM ': PROCESSING WITHOUT UPDATES'            
                                     UPON CONSOLE                               
                   END-IF                                                       
                 END-IF                                                         
                 CALL SBFTLOAD      USING PGM-DFHSNT DFHSNT-SAVE                
                 ADD  32               TO DFHSNT-SAVE                           
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
                                                                                
           CLOSE IESCNTL.                                                       
                                                                                
           PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                         
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL.                   
                                                                                
           COPY BATCHRTN.                                                       
                                                                                
       B25-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    CHECK A VSAM OR WORK FILE'S STATUS                          *        
      ******************************************************************        
       B90-CHECK-STATUS.                                                        
                                                                                
           COPY VSMSTATP.                                                       
                                                                                
       B95-EXIT-CHECK.                                                          
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM PROCESSING ROUTINES                                 *        
      ******************************************************************        
                                                                                
       C00-INIT-USERNAME.                                                       
                                                                                
           READ IESCNTL NEXT RECORD END-READ.                                   
           IF  NOT STAT-NORMAL(IUI)                                             
           OR  NOT IUI-USER-RECORD                                              
               IF  NOT STAT-NORMAL(IUI)                                         
               AND NOT STAT-EOFILE(IUI)                                         
                   MOVE IUI            TO VSUB                                  
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
               SET  STAT-EOFILE(IUI)   TO TRUE                                  
               GO TO C99-EXIT-INIT                                              
           END-IF.                                                              
                                                                                
           MOVE IUI-US-USRIDNT         TO IUI-USERID.                           
           INSPECT IUI-USERID REPLACING ALL LOW-VALUE BY SPACE.                 
                                                                                
           MOVE DFHSNT-SAVE            TO DFHSNT-OFFSET.                        
           SET  ADDRESS OF DFHSNT      TO DFHSNT-PTR.                           
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL SNT-ENTRY-LENGTH NOT > ZERO                                  
                OR SNT-USER-ID(1:SNT-USER-IDL) = IUI-USERID                     
               ADD  SNT-ENTRY-LENGTH   TO DFHSNT-OFFSET                         
               SET  ADDRESS OF DFHSNT  TO DFHSNT-PTR                            
           END-PERFORM.                                                         
                                                                                
           IF  SNT-ENTRY-LENGTH NOT > ZERO                                      
               DISPLAY 'FAILED: ' IUI-USERID ' NOT FOUND IN SNT'                
                                     UPON PRINTER                               
           END-IF.                                                              
                                                                                
           IF  WITH-UPDATES                                                     
           AND IUI-US-VERSION-64                                                
               IF  SNT-ENTRY-LENGTH > ZERO                                      
                   ADD  1              TO SNT-USER-NAMEL                        
                                   GIVING LEN                                   
                   MOVE SNT-USER-NAME(1:LEN)                                    
                                       TO IUI-US-USRNAME                        
      * trnaslate to word-caps                                                  
                   INSPECT IUI-US-USRNAME                                       
                     CONVERTING UPPER-CASE TO LOWER-CASE                        
                   INSPECT IUI-US-USRNAME(1:1)                                  
                     CONVERTING LOWER-CASE TO UPPER-CASE                        
                   PERFORM WITH TEST BEFORE                                     
                     VARYING SUB FROM 1 BY 1 UNTIL SUB > LEN                    
                       IF  IUI-US-USRNAME(SUB:1) = SPACE                        
                       AND IUI-US-USRNAME(SUB + 1:1) > SPACE                    
                           INSPECT IUI-US-USRNAME(SUB + 1:1)                    
                             CONVERTING LOWER-CASE TO UPPER-CASE                
                       END-IF                                                   
                   END-PERFORM                                                  
      *                                                                         
               ELSE                                                             
                   IF  IUI-US-USRNAME NOT > SPACES                              
                       MOVE '*NOT FOUND IN DFHSNT'                              
                                       TO IUI-US-USRNAME                        
                   END-IF                                                       
               END-IF                                                           
               SET  FUNC-REWRITE(IUI)  TO TRUE                                  
               REWRITE IUI-US-RECORD END-REWRITE                                
               IF  NOT STAT-NORMAL(IUI)                                         
                   MOVE IUI            TO VSUB                                  
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT-INIT                                          
               END-IF                                                           
           END-IF.                                                              
                                                                                
       C99-EXIT-INIT.                                                           
           EXIT.                                                                
                                                                                

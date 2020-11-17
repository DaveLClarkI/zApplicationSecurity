      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECAUDT.                                                 
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  JAN 2020.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  WINSUPPLY GROUP SERVICES.                                 
       SECURITY.      BATCH.                                                    
      *REMARKS.       CICS RESOURCE SECURITY AUDIT.                             
      *                                                                         
      * THIS PROGRAM EMAILS A LIST OF USERS THAT HAVE DIRECT ACCESS             
      * TO A BSM SECURITY RESOURCE AND ALSO HAVE ACCESS TO THE SAME             
      * BSM SECURITY RESOURCE VIA A USER GROUP.                                 
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 01/28/2020 DLC ORIGINAL PROGRAM.                                        
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
                                                                                
       SOURCE-COMPUTER. IBM-2096-N03-02.                                        
       OBJECT-COMPUTER. IBM-2096-N03-02.                                        
                                                                                
       SPECIAL-NAMES.                                                           
           SYSLST IS PRINTER.                                                   
                                                                                
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
                                                                                
           SELECT BSM-FAC-CNTL                                                  
               ASSIGN TO BSTCNTL                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS BSM-FAC-KEY                                        
               FILE STATUS IS FILE2-STAT                                        
                              FILE2-FDBK.                                       
                                                                                
           SELECT BSM-GRP-CNTL                                                  
               ASSIGN TO BSTCNTL                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS BSM-GRP-KEY                                        
               FILE STATUS IS FILE3-STAT                                        
                              FILE3-FDBK.                                       
                                                                                
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
             FROM 100 TO 1000 CHARACTERS                                        
             DEPENDING ON IUI-RECL.                                             
       COPY IESCNTL.                                                            
                                                                                
       FD  BSM-FAC-CNTL                                                         
           RECORD IS VARYING IN SIZE                                            
             FROM 280 TO 65535 CHARACTERS                                       
             DEPENDING ON BSM-RECL.                                             
       01  BSM-FAC-RECORD.                                               162    
         03                            PIC  X(4).                               
         03  BSM-FAC-KEY.                                                       
           05  BSM-FAC-CLASS           PIC  X(8).                               
             88  BSM-FACILITY                       VALUE 'FACILITY'.           
           05  BSM-FAC-RESOURCE        PIC  X(246).                             
           05  BSM-FAC-SEQUENCE        PIC  X(1).                               
             88  BSM-FAC-GENERIC                    VALUE X'80'.                
         03  BSM-FAC-RSCLEN            PIC  X(1).                               
         03  BSM-FAC-DESC              PIC  X(20).                              
         03  BSM-FAC-UA-FLAG           PIC  X(01).                              
           88  BSM-FAC-UA-DENIED                    VALUE X'00'.                
           88  BSM-FAC-UA-READ                      VALUE X'20'.                
           88  BSM-FAC-UA-UPDATE                    VALUE X'40'.                
           88  BSM-FAC-UA-ALTER                     VALUE X'80'.                
         03  BSM-FAC-FILLER            PIC  X(05).                              
         03  BSM-FAC-PERMITS           PIC S9(04)   BINARY.                     
         03  BSM-FAC-PERMIT                         OCCURS 5437                 
                                                    DEPENDING ON                
                                                    BSM-FAC-PERMITS.            
           05  BSM-FAC-USERID          PIC  X(08).                              
           05  BSM-FAC-AC-FLAG         PIC  X(01).                              
             88  BSM-FAC-AC-DENIED                  VALUE X'00'.                
             88  BSM-FAC-AC-READ                    VALUE X'20'.                
             88  BSM-FAC-AC-UPDATE                  VALUE X'40'.                
             88  BSM-FAC-AC-ALTER                   VALUE X'80'.                
           05                          PIC  X(03).                              
                                                                                
       FD  BSM-GRP-CNTL.                                                        
       01  BSM-GRP-RECORD.                                               162    
         03                            PIC  X(4).                               
         03  BSM-GRP-KEY.                                                       
           05  BSM-GRP-CLASS           PIC  X(8).                               
               88  BSM-GROUP                        VALUE 'GROUP'.              
           05  BSM-GRP-RESOURCE.                                                
             07  BSM-GRP-NAME          PIC  X(8).                               
             07                        PIC  X(8).                               
             07  BSM-GRP-MEMBER        PIC  X(8).                               
             07                        PIC  X(222).                             
             07  BSM-GRP-SEQUENCE      PIC  X(1).                               
         03  BSM-GRP-RSCLEN            PIC  X(1).                               
         03  BSM-GRP-DESC              PIC  X(20).                              
                                                                                
      ******************************************************************        
      *    WORKING-STORAGE SECTION                                     *        
      ******************************************************************        
       WORKING-STORAGE SECTION.                                                 
                                                                                
       01  WS-FIELDS.                                                           
         03  FILLER                    PIC  X(11)   VALUE '**STORAGE**'.        
         03  LOWVALUE                  PIC  X(01)   VALUE LOW-VALUE.            
         03  THIS-PGM                  PIC  X(8)    VALUE 'VSECAUDT'.           
                                                                                
         03  MAILWRTR                  PIC  X(8)    VALUE 'MAILWRTB'.           
         03  JOBDATA                   PIC  X(8)    VALUE 'JOBDATA'.            
                                                                                
         03  IUI                       PIC S9(04)   BINARY VALUE 1.             
         03  FAC                       PIC S9(04)   BINARY VALUE 2.             
         03  GRP                       PIC S9(04)   BINARY VALUE 3.             
                                                                                
         03  SUB                       PIC S9(04)   BINARY.                     
         03  IDX                       PIC S9(04)   BINARY.                     
         03  EMAIL-SWITCH              PIC  X(1)    VALUE 'C'.                  
           88  EMAIL-CLOSED                         VALUE 'C'.                  
           88  EMAIL-OPEN                           VALUE 'O'.                  
         03  IUI-RECL                  PIC  9(05)   VALUE 1000.                 
         03  BSM-RECL                  PIC  9(05)   VALUE 65535.                
                                                                                
         03  FAC-ACCESS                PIC  X(01)   VALUE SPACES.               
         03  GRP-ACCESS                PIC  X(01)   VALUE SPACES.               
         03  CURRENT-USER              PIC  X(08)   VALUE SPACES.               
                                                                                
      *  WORK AREA FOR DATE/TIME CONVERSIONS                                    
         03  NUM-DATE                  PIC  9(8).                               
         03  WRK-DATE        REDEFINES NUM-DATE.                                
           05  WRK-MM                  PIC  99.                                 
           05  WRK-DD                  PIC  99.                                 
           05  WRK-YYYY                PIC  9999.                               
         03  WRK-DATETIME.                                                      
           05  SRT-DATE.                                                        
             07  WRK-YYYY              PIC  9999.                               
             07  WRK-MM                PIC  99.                                 
             07  WRK-DD                PIC  99.                                 
           05  NUM-TIME                PIC  9(6).                               
         03  DSP-DATE                  PIC  99/99/9999.                         
         03  DSP-TIME                  PIC  99B99B99.                           
                                                                                
      *  WORK AREA FOR BYTE-ENCODED NUMBERS                                     
         03  DOUBLE-WORD               PIC S9(18)   BINARY.                     
         03  EIGHT-BYTES     REDEFINES DOUBLE-WORD.                             
           05  FILLER                  PIC  X(4).                               
           05  FULL-WORD               PIC S9(9)    BINARY.                     
           05  FOUR-BYTES    REDEFINES FULL-WORD.                               
             07  HI-MSB                PIC  X.                                  
             07  THREE-BYTES.                                                   
               09  HI-LSB              PIC  X.                                  
               09  HALF-WORD           PIC S9(4)    BINARY.                     
               09  TWO-BYTES REDEFINES HALF-WORD.                               
                 11  LO-MSB            PIC  X.                                  
                 11  LO-LSB            PIC  X.                                  
                                                                                
       01  MAILWRTR-COMMAREA.                                                   
         COPY MAILWRTR.                                                         
                                                                                
       COPY JOBDATA.                                                            
                                                                                
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
                                                                                
           PERFORM C00-AUDIT-SECURITY THRU C99-EXIT                             
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
             MOVE 'BSTCNTL'            TO VSAM-FILE(FAC)                        
             MOVE 'BSTCNTL'            TO VSAM-FILE(GRP)                        
                                                                                
             MOVE LENGTH OF IUI-KEY    TO VSAM-KEYL(IUI)                        
             MOVE 48                   TO VSAM-KEYL(FAC)                        
             MOVE 32                   TO VSAM-KEYL(GRP)                        
                                                                                
             PERFORM WITH TEST BEFORE                                           
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                 SET FUNC-OPEN(VSUB)   TO TRUE                                  
             END-PERFORM                                                        
                                                                                
             OPEN INPUT  IESCNTL                                                
                         BSM-FAC-CNTL                                           
                         BSM-GRP-CNTL                                           
                                                                                
             PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                       
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                                                                                
             IF  RTC-CODE = ZERO                                                
                 PERFORM WITH TEST BEFORE                                       
                   VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL              
                     SET FUNC-READ(VSUB) TO TRUE                                
                 END-PERFORM                                                    
                 MOVE LOW-VALUES       TO IESCNTL-RECORD                        
                 SET  IUI-USER-PROFILE TO TRUE                                  
                 SET  FUNC-START(IUI)  TO TRUE                                  
                 START IESCNTL KEY >= IUI-KEY END-START                         
                 IF  NOT STAT-NORMAL(IUI)                                       
                   MOVE IUI            TO VSUB                                  
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                 ELSE                                                           
                   SET  FUNC-READNEXT(IUI)                                      
                                       TO TRUE                                  
                 END-IF                                                         
             END-IF                                                             
                                                                                
           END-IF.                                                              
                                                                                
       B15-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM TERMINATION ROUTINE                                 *        
      ******************************************************************        
       B20-TERMINATION.                                                         
                                                                                
           IF  EMAIL-OPEN                                                       
               PERFORM P20-close-EMAIL THRU P29-EXIT                            
           END-IF.                                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                    
               SET  FUNC-CLOSE(VSUB)   TO TRUE                                  
           END-PERFORM.                                                         
                                                                                
           CLOSE IESCNTL                                                        
                 BSM-FAC-CNTL                                                   
                 BSM-GRP-CNTL.                                                  
                                                                                
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
                                                                                
       C00-AUDIT-SECURITY.                                                      
                                                                                
      * audit all users ...                                                     
           READ IESCNTL NEXT RECORD END-READ.                                   
           IF  NOT STAT-NORMAL(IUI)                                             
               IF  NOT STAT-EOFILE(IUI)                                         
                   MOVE IUI            TO VSUB                                  
                   MOVE IUI-KEY        TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
               GO TO C99-EXIT                                                   
           END-IF.                                                              
           IF  NOT IUI-USER-PROFILE                                             
               SET  STAT-EOFILE(IUI)   TO TRUE                                  
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE SPACES                 TO CURRENT-USER.                         
           STRING IUI-US-USRIDNT          DELIMITED BY LOW-VALUE                
                                     INTO CURRENT-USER.                         
           DISPLAY 'Auditing ' CURRENT-USER ' ...'                              
                                     UPON PRINTER.                              
                                                                                
      * ... and all security resources...                                       
           MOVE LOW-VALUES             TO BSM-FAC-RECORD.                       
           SET  BSM-FACILITY           TO TRUE.                                 
           SET  FUNC-START(FAC)        TO TRUE.                                 
           START BSM-FAC-CNTL KEY >= BSM-FAC-KEY END-START.                     
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT STAT-NORMAL(FAC)                                         
                OR NOT BSM-FACILITY                                             
                OR RTC-CODE > ZERO                                              
                                                                                
               SET  FUNC-READNEXT(FAC) TO TRUE                                  
               READ BSM-FAC-CNTL NEXT RECORD END-READ                           
               IF  STAT-NORMAL(FAC)                                             
               AND BSM-FACILITY                                                 
                                                                                
                 PERFORM WITH TEST BEFORE                                       
                   VARYING SUB FROM 1 BY 1                                      
                     UNTIL SUB > BSM-FAC-PERMITS                                
                        OR BSM-FAC-USERID(SUB) = CURRENT-USER                   
                 END-PERFORM                                                    
                                                                                
      * ... where the user has direct access to a security resource...          
                 IF  SUB <= BSM-FAC-PERMITS                                     
                 AND BSM-FAC-USERID(SUB) = CURRENT-USER                         
                   EVALUATE TRUE                                                
                   WHEN BSM-FAC-AC-ALTER(SUB)  MOVE '4' TO FAC-ACCESS           
                   WHEN BSM-FAC-AC-UPDATE(SUB) MOVE '3' TO FAC-ACCESS           
                   WHEN BSM-FAC-AC-READ(SUB)   MOVE '2' TO FAC-ACCESS           
                   WHEN OTHER                  MOVE '0' TO FAC-ACCESS           
                   END-EVALUATE                                                 
                   DISPLAY '     Found: ' BSM-FAC-RESOURCE(1:40)                
                               ' Access=' FAC-ACCESS                            
                                     UPON PRINTER                               
                                                                                
      * ... and also has access to the same resource via user group ...         
                   PERFORM WITH TEST BEFORE                                     
                     VARYING IDX FROM 1 BY 1                                    
                       UNTIL IDX > BSM-FAC-PERMITS                              
                          OR RTC-CODE > ZERO                                    
                     MOVE SPACES       TO BSM-GRP-KEY                           
                     SET  BSM-GROUP    TO TRUE                                  
                     MOVE BSM-FAC-USERID(IDX)                                   
                                       TO BSM-GRP-NAME                          
                     MOVE CURRENT-USER TO BSM-GRP-MEMBER                        
                     MOVE LOW-VALUE    TO BSM-GRP-SEQUENCE                      
                     READ BSM-GRP-CNTL RECORD KEY IS BSM-GRP-KEY                
                                          END-READ                              
                                                                                
                     EVALUATE TRUE                                              
                     WHEN STAT-NORMAL(GRP)                                      
      * ... then, if so found, send a notice.                                   
                       EVALUATE TRUE                                            
                       WHEN BSM-FAC-AC-ALTER(IDX)                               
                           MOVE '4' TO GRP-ACCESS                               
                       WHEN BSM-FAC-AC-UPDATE(IDX)                              
                           MOVE '3' TO GRP-ACCESS                               
                       WHEN BSM-FAC-AC-READ(IDX)                                
                           MOVE '2' TO GRP-ACCESS                               
                       WHEN OTHER                                               
                           MOVE '0' TO GRP-ACCESS                               
                       END-EVALUATE                                             
                       DISPLAY '                                    '           
                               ' Group: ' BSM-GRP-NAME                          
                               ' Access=' GRP-ACCESS                            
                                     UPON PRINTER                               
                       PERFORM P30-SEND-EMAIL THRU P39-EXIT                     
                     WHEN NOT STAT-NORMAL(GRP)                                  
                     AND  NOT STAT-NOTFND(GRP)                                  
                       MOVE GRP              TO VSUB                            
                       MOVE BSM-GRP-KEY      TO VSAM-KEYD(VSUB)                 
                       PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK             
                     END-EVALUATE                                               
                   END-PERFORM                                                  
                                                                                
                 END-IF                                                         
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  NOT STAT-NORMAL(FAC)                                             
               IF  NOT STAT-EOFILE(FAC)                                         
                   MOVE FAC            TO VSUB                                  
                   MOVE BSM-FAC-KEY    TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
               END-IF                                                           
               GO TO C99-EXIT                                                   
           END-IF.                                                              
           IF  NOT BSM-FACILITY                                                 
               SET  STAT-EOFILE(FAC)   TO TRUE                                  
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
       C99-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
       P00-CALL-MAILWRTR.                                                       
           CALL MAILWRTR            USING MAILWRTR-COMMAREA.                    
           MOVE SPACES                 TO MAIL-PARMS.                           
       P99-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P10-OPEN-EMAIL.                                                          
           INITIALIZE MAILWRTR-COMMAREA.                                        
           SET  MAIL-HEAD-PARMS        TO TRUE.                                 
           MOVE 'MAINFRAME_PROGRAMMING'                                         
                                       TO MAIL-HEAD-TO-GRP.                     
           MOVE '(VSECAUDT) CICS RESOURCE SECURITY AUDIT'                       
                                       TO MAIL-HEAD-SUBJECT.                    
           MOVE 'SY'                   TO MAIL-HEAD-SYSTEM.                     
           SET  MAIL-DELIVERY-ON       TO TRUE.                                 
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           SET  MAIL-BODY-PARMS        TO TRUE.                                 
           MOVE '<pre><br>'            TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE FUNCTION CURRENT-DATE  TO WRK-DATETIME.                         
           MOVE CORR SRT-DATE          TO WRK-DATE.                             
           MOVE      NUM-DATE          TO DSP-DATE.                             
           MOVE      NUM-TIME          TO DSP-TIME.                             
           INSPECT DSP-TIME     REPLACING ALL SPACES BY ':'.                    
                                                                                
           CALL JOBDATA             USING JOBDATA-PARMS.                        
           STRING 'JOB: '                 DELIMITED BY SIZE                     
                  JOBD-PWR-NAME           DELIMITED BY SPACE                    
                  ' #'                    DELIMITED BY SIZE                     
                  JOBD-PWR-NUMB           DELIMITED BY SPACE                    
                  ' PGM: '                DELIMITED BY SIZE                     
                  JOBD-EXEC-PGM           DELIMITED BY SPACE                    
                  ' DATE: '               DELIMITED BY SIZE                     
                  DSP-DATE                DELIMITED BY SPACE                    
                  ' TIME: '               DELIMITED BY SIZE                     
                  DSP-TIME                DELIMITED BY SPACE                    
                  '<br></pre>'                                                  
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE '<p>This is a list of those users having more than'             
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE 'singular access to a particular security resource --'          
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE 'i.e., direct vs. group access.  This may or may not'           
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE 'indicate a problem.  However, at the least where'              
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE 'access is equal, the direct access should be revoked'          
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE 'in favor of the group definition.</p>'                         
                                       TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           MOVE '<pre><br>'            TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           STRING ' UserId    '                                                 
                  '             Resource Name               '                   
                  'Access   '                                                   
                  ' Group   '                                                   
                  'Access'                                                      
                  '<br>'                                                        
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           STRING '--------   '                                                 
                  '---------------------------------------- '                   
                  '------   '                                                   
                  '-------- '                                                   
                  '------'                                                      
                  '<br>'                                                        
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           SET  EMAIL-OPEN             TO TRUE.                                 
       P19-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P20-CLOSE-EMAIL.                                                         
           MOVE '</pre>'               TO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           SET  MAIL-MESG-COMPLETE     TO TRUE.                                 
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
                                                                                
           SET  EMAIL-CLOSED           TO TRUE.                                 
       P29-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P30-SEND-EMAIL.                                                          
           IF  NOT EMAIL-OPEN                                                   
               PERFORM P10-OPEN-EMAIL THRU P19-EXIT                             
           END-IF.                                                              
                                                                                
           STRING CURRENT-USER '   '                                            
                  BSM-FAC-RESOURCE(1:40) ' '                                    
                  '  ' FAC-ACCESS '      '                                      
                  BSM-GRP-NAME ' '                                              
                  '  ' GRP-ACCESS '   '                                         
                  '<br>'                                                        
               DELIMITED BY SIZE     INTO MAIL-BODY-TEXT.                       
           PERFORM P00-CALL-MAILWRTR THRU P99-EXIT.                             
       P39-EXIT.                                                                
           EXIT.                                                                
                                                                                

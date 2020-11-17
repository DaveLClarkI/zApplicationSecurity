      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECSERV.                                                 
       AUTHOR.        DAVE L CLARK.                                             
       DATE-WRITTEN.  JAN 2008.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.                              
       SECURITY.      INTERNAL, VIA "QUERY SECURITY".                           
      *REMARKS.       CICS RESOURCE SECURITY UPDATE INTERFACE.                  
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 01/10/2008 DLC ORIGINAL PROGRAM.                                        
/DLC0/* 07/08/2008 DLC ALLOW NON-ICCF USERS TO BE DELETED.                      
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
           CLASS HEXADECIMAL IS 'A' THRU 'F', '0' THRU '9',                     
           CLASS ALPHAMERIC  IS 'A' THRU 'I', 'J' THRU 'R',                     
                                'S' THRU 'Z', '0' THRU '9',                     
           CLASS NATIONAL-ALPHAMERIC                                            
                             IS '$', '#', '@',                                  
                                'A' THRU 'I', 'J' THRU 'R',                     
                                'S' THRU 'Z', '0' THRU '9',                     
           CLASS MIXED-NATIONAL-ALPHAMERIC                                      
                             IS '$', '#', '@', 'a' THRU 'i',                    
                                'j' THRU 'r', 's' THRU 'z',                     
                                'A' THRU 'I', 'J' THRU 'R',                     
                                'S' THRU 'Z', '0' THRU '9',                     
           CLASS NATIONAL-ALPHAMERIC-OR-PERIOD                                  
                             IS '.', '$', '#', '@',                             
                                'A' THRU 'I', 'J' THRU 'R',                     
                                'S' THRU 'Z', '0' THRU '9'.                     
                                                                                
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
         03  THIS-PGM                  PIC  X(08)   VALUE 'VSECSERV'.           
         03  BSTCNTL                   PIC  X(08)   VALUE 'BSTCNTL '.           
         03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.           
         03  DTLG                      PIC  X(04)   VALUE 'DTLG'.               
         03  BSM-RECL                  PIC S9(04)   BINARY.                     
         03  IUI-RECL                  PIC S9(04)   BINARY.                     
         03  MAX-ENTRIES               PIC S9(04).                              
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  LOVALUE                   PIC  X(01)   VALUE LOW-VALUE.            
         03  GROUP-HEADER.                                                      
           05                          PIC  X(2)    VALUE X'0000'.              
           05                          PIC  X(6)    VALUE 'GrpUID'.             
         03  LOWER-CASE                PIC  X(26)   VALUE                       
                                           'abcdefghijklmnopqrstuvwxyz'.        
         03  UPPER-CASE                PIC  X(26)   VALUE                       
                                           'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.        
                                                                                
      *  WORK AREA TO CONVERT BYTE DATA TO A BINARY NUMBER                      
       01  DOUBLE-WORD                 PIC S9(18)   BINARY.                     
       01  EIGHT-BYTES                 REDEFINES    DOUBLE-WORD.                
         03                            PIC  X(2).                               
         03  SIX-BYTES.                                                         
           05                          PIC  X(2).                               
           05  FULL-WORD               PIC S9(9)    BINARY.                     
           05  FOUR-BYTES              REDEFINES    FULL-WORD.                  
             07  HI-MSB                PIC  X.                                  
             07  THREE-BYTES.                                                   
               09  HI-LSB              PIC  X.                                  
               09  HALF-WORD           PIC S9(4)    BINARY.                     
               09  TWO-BYTES           REDEFINES    HALF-WORD.                  
                 11  LO-MSB            PIC  X.                                  
                 11  LO-LSB            PIC  X.                                  
                                                                                
      * APPLICATION SECURITY CONTROL AREAS                                      
       01  WINVSEC-CONTROL.                                                     
         03  WINVSEC-CLASS             PIC  X(8)    VALUE SPACES.               
         03  WINVSEC-FACILITY          PIC  X(8)    VALUE 'FACILITY'.           
         03  WINVSEC-GROUP             PIC  X(8)    VALUE 'GROUP   '.           
         03  WINVSEC-TRANS             PIC  X(8)    VALUE 'TCICSTRN'.           
         03  WINVSEC-PREFIX            PIC  X(8)    VALUE SPACES.               
         03  WINVSEC-RESID             PIC  X(39).                              
         03  WINVSEC-RESIDLEN          PIC S9(9)    BINARY.                     
         03  QRY-READ                  PIC S9(9)    BINARY.                     
         03  QRY-UPDATE                PIC S9(9)    BINARY.                     
         03  QRY-ALTER                 PIC S9(9)    BINARY.                     
         03  MSG-NOLOG                 PIC S9(9)    BINARY VALUE +55.           
                                                                                
      * RESOURCE LIST TEMP STORAGE CONTROL AREAS                                
       01  TS-CTRL.                                                             
         03  TS-QUEUE.                                                          
           05  TS-TRAN                 PIC  X(4).                               
           05  TS-TASK                 PIC S9(7)    PACKED-DECIMAL.             
         03  TS-TOTL                   PIC S9(4)    BINARY.                     
         03  TS-ITEM                   PIC S9(4)    BINARY.                     
       01  TS-RECD.                                                             
         03  TS-RSC-TYPE               PIC  X(1).                               
           88  TS-RSC-FACILITY                      VALUE 'F'.                  
           88  TS-RSC-GROUP                         VALUE 'G'.                  
           88  TS-RSC-SUBGROUP                      VALUE 'H'.                  
           88  TS-RSC-TRANS                         VALUE 'T'.                  
           88  TS-RSC-USERS                         VALUE 'U'.                  
         03  TS-FAC-CODE               PIC  X(4).                               
           88  TS-FAC-APPS                          VALUE 'APPS'.               
           88  TS-FAC-CORP                          VALUE 'CORP'.               
           88  TS-FAC-ROLE                          VALUE 'ROLE'.               
           88  TS-FAC-VSEC                          VALUE 'VSEC'.               
         03  TS-RESOURCE               PIC  X(39).                              
         03  TS-RSC-DESC               PIC  X(20).                              
         03  TS-ACCESS                 PIC  X(1).                               
           88  TS-ACC-DENIED                        VALUE '0', ' '.             
           88  TS-ACC-READ                          VALUE '2'.                  
           88  TS-ACC-UPDATE                        VALUE '3'.                  
           88  TS-ACC-ALTER                         VALUE '4'.                  
           88  TS-ACC-USER                          VALUE '2'.                  
           88  TS-ACC-PROG                          VALUE '3'.                  
           88  TS-ACC-ADMIN                         VALUE '4'.                  
         03  TS-SELECT                 PIC  X(1).                               
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       01  LOGF-RECORD.                                                         
         03  LOGF-DEST                 PIC  X(1)    VALUE '*'.                  
         03  LOGF-PROG                 PIC  X(9)    VALUE SPACES.               
         03  LOGF-MESG.                                                         
           05  LOGF-NUMB               PIC  ZZZZZZ9.                            
           05  LOGF-TEXT               PIC  X(63).                              
                                                                                
       COPY BITMAN.                                                             
                                                                                
       COPY HEXMAN.                                                             
                                                                                
       COPY UNEXERRW.                                                           
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).                 
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
       01  DFHCOMMAREA.                                                         
         COPY VSECSERV.                                                         
                                                                                
      * BSTCNTL RECORD I/O AREA                                                 
       COPY BSTCNTL.                                                            
                                                                                
      * IESCNTL RECORD I/O AREA                                                 
       COPY IESCNTL.                                                            
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION.                                                      
                                                                                
      ******************************************************************        
      *    MAINLINE ROUTINE                                            *        
      ******************************************************************        
       A00-VSEC-MAINLINE.                                                       
                                                                                
           MOVE 'L'                    TO LOGF-DEST.                            
           MOVE THIS-PGM               TO LOGF-PROG.                            
           MOVE SPACES                 TO LOGF-MESG.                            
                                                                                
           IF  EIBCALEN < LENGTH OF VSECSERV-HEADER                             
           OR  VSECSERV-REQ-TRAN NOT = THIS-PGM(1:4)                            
             IF  EIBCALEN >= (LENGTH OF VSECSERV-HEADER -                       
                              LENGTH OF VSECSERV-RTN-MESG)                      
               MOVE 'COMM'             TO VSECSERV-RTN-CODE                     
               GO TO A90-EXIT                                                   
             END-IF                                                             
             IF  EIBCALEN >= LENGTH OF VSECSERV-API-RETC                        
               MOVE 'COMM'             TO VSECSERV-API-RETC                     
               GO TO A99-RETURN                                                 
             END-IF                                                             
             EXEC CICS ABEND                                                    
                       ABCODE('COMM')                                           
                       CANCEL                                                   
             END-EXEC                                                           
           END-IF.                                                              
                                                                                
       A10-VSEC-GETMAIN.                                                        
                                                                                
           IF  ADDRESS OF BSTCNTL-RECORD = NULL                                 
               MOVE +32767             TO BSM-RECL                              
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF BSTCNTL-RECORD)                         
                         LENGTH(BSM-RECL)                                       
               END-EXEC                                                         
               SET ADDRESS OF BSM-FACILITY-RECORD                               
                                       TO ADDRESS OF BSTCNTL-RECORD             
               SET ADDRESS OF BSM-GROUP-RECORD                                  
                                       TO ADDRESS OF BSTCNTL-RECORD             
           END-IF.                                                              
           IF  ADDRESS OF IESCNTL-RECORD = NULL                                 
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF IESCNTL-RECORD)                         
                         LENGTH(LENGTH OF IESCNTL-RECORD)                       
               END-EXEC                                                         
               SET ADDRESS OF IUI-US-RECORD                                     
                                       TO ADDRESS OF IESCNTL-RECORD             
           END-IF.                                                              
                                                                                
       A20-VSEC-VALIDATE.                                                       
                                                                                
           IF  VSECSERV-CGI-REQU > SPACES AND NOT = '*'                         
               MOVE VSECSERV-CGI-REQU  TO WINVSEC-RESID                         
           ELSE                                                                 
               MOVE SPACES             TO WINVSEC-RESID                         
           END-IF.                                                              
                                                                                
           IF  VSECSERV-CHK-SECURITY                                            
               PERFORM M00-CHECK-SECURITY THRU M90-EXIT                         
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
               OR (NOT VSECSERV-SEC-DENIED                                      
               AND NOT VSECSERV-SEC-READABLE                                    
               AND NOT VSECSERV-SEC-UPDATABLE                                   
               AND NOT VSECSERV-SEC-ALTERABLE)                                  
                   GO TO A70-VSEC-FREEMAIN                                      
               END-IF                                                           
               GO TO A99-RETURN                                                 
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO VSECSERV-RTN-QUEUE.                   
           SET  VSECSERV-RTN-OKAY      TO TRUE.                                 
           MOVE 'Transaction function completed normally.'                      
                                       TO VSECSERV-RTN-MESG.                    
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECSERV-GET-RESOURCE                                           
           WHEN VSECSERV-UPD-RESOURCE                                           
           WHEN VSECSERV-DEL-RESOURCE                                           
             IF  EIBCALEN < (LENGTH OF VSECSERV-HEADER +                        
                             LENGTH OF VSECUPD-RESOURCE)                        
               MOVE 'CALN'             TO VSECSERV-RTN-CODE                     
               MOVE 'Commarea length too short for function.'                   
                                       TO VSECSERV-RTN-MESG                     
             ELSE                                                               
               EVALUATE TRUE                                                    
               WHEN VSECSERV-GET-RESOURCE                                       
                 PERFORM B00-GET-RESOURCE THRU B90-EXIT                         
               WHEN VSECSERV-UPD-RESOURCE                                       
                 PERFORM C00-UPD-RESOURCE THRU C90-EXIT                         
               WHEN VSECSERV-DEL-RESOURCE                                       
                 PERFORM D00-DEL-RESOURCE THRU D90-EXIT                         
               END-EVALUATE                                                     
             END-IF                                                             
                                                                                
           WHEN VSECSERV-GET-PERMISSION                                         
           WHEN VSECSERV-UPD-PERMISSION                                         
           WHEN VSECSERV-DEL-PERMISSION                                         
             IF  EIBCALEN < (LENGTH OF VSECSERV-HEADER +                        
                             LENGTH OF VSECUPD-DETAIL)                          
               MOVE 'CALN'             TO VSECSERV-RTN-CODE                     
               MOVE 'Commarea length too short for function.'                   
                                       TO VSECSERV-RTN-MESG                     
             ELSE                                                               
               EVALUATE TRUE                                                    
               WHEN VSECSERV-GET-PERMISSION                                     
                 PERFORM E00-GET-PERMISSION THRU E90-EXIT                       
               WHEN VSECSERV-UPD-PERMISSION                                     
                 PERFORM F00-UPD-PERMISSION THRU F90-EXIT                       
               WHEN VSECSERV-DEL-PERMISSION                                     
                 PERFORM G00-DEL-PERMISSION THRU G90-EXIT                       
               END-EVALUATE                                                     
             END-IF                                                             
                                                                                
           WHEN VSECSERV-LST-SECURITY                                           
             IF  EIBCALEN < (LENGTH OF VSECSERV-HEADER +                        
                             LENGTH OF VSECLST-DETAIL)                          
               MOVE 'CALN'             TO VSECSERV-RTN-CODE                     
               MOVE 'Commarea length too short for function.'                   
                                       TO VSECSERV-RTN-MESG                     
             ELSE                                                               
               PERFORM H00-LIST-SECURITY THRU H90-EXIT                          
             END-IF                                                             
                                                                                
           WHEN VSECSERV-LST-APPS                                               
           WHEN VSECSERV-LST-CORP                                               
           WHEN VSECSERV-LST-ROLE                                               
           WHEN VSECSERV-LST-VSEC                                               
           WHEN VSECSERV-LST-GROUP                                              
           WHEN VSECSERV-LST-TRANS                                              
           WHEN VSECSERV-LST-USERS                                              
               MOVE EIBTRNID           TO TS-TRAN                               
               MOVE EIBTASKN           TO TS-TASK                               
               PERFORM Q30-TS-DELETEQ                                           
               EVALUATE TRUE                                                    
               WHEN VSECSERV-LST-APPS                                           
               WHEN VSECSERV-LST-CORP                                           
               WHEN VSECSERV-LST-ROLE                                           
               WHEN VSECSERV-LST-VSEC                                           
                 PERFORM I00-LST-FACILITY THRU I90-EXIT                         
               WHEN VSECSERV-LST-GROUP                                          
                 PERFORM J00-LST-GROUP THRU J90-EXIT                            
               WHEN VSECSERV-LST-TRANS                                          
                 PERFORM K00-LST-TRANS THRU K90-EXIT                            
               WHEN VSECSERV-LST-USERS                                          
                 PERFORM L00-LST-USERS THRU L90-EXIT                            
               END-EVALUATE                                                     
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO A80-VSEC-ERRCHECK                                      
               END-IF                                                           
               MOVE TS-QUEUE           TO VSECSERV-RTN-QUEUE                    
                                                                                
           WHEN OTHER                                                           
               MOVE 'RQER'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested function is undefined.'                      
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
                                                                                
       A70-VSEC-FREEMAIN.                                                       
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
       A80-VSEC-ERRCHECK.                                                       
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE 'UNEX'             TO VSECSERV-RTN-CODE                     
               MOVE 'See DTLG for unexpected error information.'                
                                       TO VSECSERV-RTN-MESG                     
               PERFORM X00-UNEX-ERR                                             
               IF  LOGF-MESG > SPACES                                           
                   PERFORM Q00-LOGIT THRU Q90-EXIT                              
               END-IF                                                           
               STRING VSECSERV-REQ-FUNC                                         
                      ': ' UNEX-MSG(24:55)                                      
                   DELIMITED BY SIZE INTO LOGF-MESG                             
               PERFORM Q00-LOGIT THRU Q90-EXIT                                  
               STRING VSECSERV-RTN-CODE                                         
                      ': RS=' UNEX-RESP(5:)                                     
                      ', R2=' UNEX-RESP2(5:)                                    
                      ', DS=' UNEX-DS                                           
                   DELIMITED BY SIZE INTO LOGF-MESG                             
               PERFORM Q00-LOGIT THRU Q90-EXIT                                  
           END-IF.                                                              
                                                                                
       A90-EXIT.                                                                
                                                                                
           MOVE VSECSERV-RTN-CODE      TO VSECSERV-API-RETC.                    
                                                                                
       A99-RETURN.                                                              
                                                                                
           EXEC CICS RETURN END-EXEC.                                           
           GOBACK.                                                              
                                                                                
      /*****************************************************************        
      *    GET RESOURCE ENTRY                                          *        
      ******************************************************************        
       B00-GET-RESOURCE.                                                        
                                                                                
           MOVE 1                      TO pos.                                  
           MOVE SPACES                 TO WINVSEC-RESID.                        
           STRING VSECUPD-RES-NAME        DELIMITED BY SPACE                    
               INTO WINVSEC-RESID    WITH POINTER POS.                          
           IF  VSECUPD-RES-NAME(POS:) > SPACES                                  
               MOVE 'RINV'             TO VSECSERV-RTN-CODE                     
               MOVE 'Embedded space not allowed in resource name.'              
                                       TO VSECSERV-RTN-MESG                     
               GO TO B90-EXIT                                                   
           END-IF.                                                              
           SUBTRACT 1                FROM POS.                                  
                                                                                
       B10-GET-TRANS.                                                           
           IF  POS > 4                                                          
               INSPECT WINVSEC-RESID                                            
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               GO TO B20-GET-GROUP                                              
           END-IF.                                                              
                                                                                
           MOVE WINVSEC-TRANS          TO WINVSEC-CLASS.                        
                                                                                
      * Is it a lower-case transaction resource?                                
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Is it an upper-case transaction resource?                               
           INSPECT WINVSEC-RESID                                                
               CONVERTING LOWER-CASE   TO UPPER-CASE.                           
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
       B20-GET-GROUP.                                                           
           IF  POS > 8                                                          
               GO TO B30-GET-FACILITY                                           
           END-IF.                                                              
                                                                                
           MOVE WINVSEC-GROUP          TO WINVSEC-CLASS.                        
                                                                                
      * Is it a group resource?                                                 
           PERFORM Q20-INITKEY-SELECTED.                                        
           MOVE WINVSEC-RESID          TO BSM-RESOURCE.                         
           MOVE GROUP-HEADER           TO BSM-RESOURCE(17:8).                   
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
       B30-GET-FACILITY.                                                        
                                                                                
           MOVE WINVSEC-FACILITY       TO WINVSEC-CLASS.                        
                                                                                
      * Is it a WINCORP facility resource?                                      
           SET  VSECUPD-FAC-CORP       TO TRUE.                                 
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Is it a WINAPPS facility resource?                                      
           SET  VSECUPD-FAC-APPS       TO TRUE.                                 
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Is it a WINROLE facility resource?                                      
           SET  VSECUPD-FAC-ROLE       TO TRUE.                                 
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Is it a WINVSEC facility resource?                                      
           SET  VSECUPD-FAC-VSEC       TO TRUE.                                 
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
       B40-GET-USER.                                                            
           IF  POS > 8                                                          
               GO TO B50-GET-ERROR                                              
           END-IF.                                                              
                                                                                
      * Is it a user resource?                                                  
           SET  IUI-USER-RECORD        TO TRUE.                                 
           MOVE LOW-VALUE              TO IUI-KEYD.                             
           STRING WINVSEC-RESID(1:8)      DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT.                       
                                                                                
           PERFORM Q46-READEQ-IESCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-USER   TO TRUE                                  
               GO TO B60-GOT-RESRC                                              
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO B90-EXIT                                                   
           END-IF.                                                              
                                                                                
       B50-GET-ERROR.                                                           
                                                                                
      * Failed to identify resource.                                            
           MOVE '* UNKNOWN RESOURCE *' TO VSECUPD-RES-DESC.                     
           MOVE LOW-VALUE              TO VSECUPD-RES-UACC.                     
                                                                                
           MOVE 'RUNK'                 TO VSECSERV-RTN-CODE.                    
           MOVE 'Unable to determine resource type.'                            
                                       TO VSECSERV-RTN-MESG.                    
           GO TO B90-EXIT.                                                      
                                                                                
       B60-GOT-RESRC.                                                           
                                                                                
      * Resource identified.                                                    
           IF  VSECUPD-RES-USER                                                 
               MOVE WINVSEC-RESID(1:8) TO VSECUPD-RES-NAME                      
               MOVE IUI-US-USRNAME     TO VSECUPD-RES-DESC                      
           ELSE                                                                 
               PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT                     
               MOVE WINVSEC-RESID(CNT + 1:LEN) TO VSECUPD-RES-NAME              
               MOVE BSM-RSRCDESC       TO VSECUPD-RES-DESC                      
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-GROUP                                               
               MOVE LOW-VALUE          TO VSECUPD-RES-UACC                      
                                                                                
           WHEN VSECUPD-RES-USER                                                
               SET  BIT-DECODE-FROM-BYTE TO TRUE                                
               MOVE IUI-US-USRINFO     TO BIT-BYTE                              
               CALL 'BITMAN'        USING BITMAN-PARMS                          
               EVALUATE TRUE                                                    
               WHEN BIT-0-IS-ON                                                 
                AND BIT-1-IS-ON                                                 
                   MOVE '4'            TO VSECUPD-RES-UACC                      
               WHEN BIT-0-IS-OFF                                                
                AND BIT-1-IS-ON                                                 
                   MOVE '3'            TO VSECUPD-RES-UACC                      
               WHEN OTHER                                                       
                   MOVE '2'            TO VSECUPD-RES-UACC                      
               END-EVALUATE                                                     
                                                                                
           WHEN OTHER                                                           
               EVALUATE TRUE                                                    
               WHEN BSM-FAC-UA-ALTER  SET VSECUPD-RES-UA-ALTER  TO TRUE         
               WHEN BSM-FAC-UA-UPDATE SET VSECUPD-RES-UA-UPDATE TO TRUE         
               WHEN BSM-FAC-UA-READ   SET VSECUPD-RES-UA-READ   TO TRUE         
               WHEN OTHER             SET VSECUPD-RES-UA-DENIED TO TRUE         
               END-EVALUATE                                                     
                                                                                
           END-EVALUATE.                                                        
                                                                                
       B90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    ADD/ALTER RESOURCE ENTRY                                    *        
      ******************************************************************        
       C00-UPD-RESOURCE.                                                        
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
             EVALUATE TRUE                                                      
             WHEN VSECUPD-FAC-APPS                                              
             WHEN VSECUPD-FAC-CORP                                              
             WHEN VSECUPD-FAC-ROLE                                              
             WHEN VSECUPD-FAC-VSEC                                              
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               MOVE WINVSEC-FACILITY   TO WINVSEC-CLASS                         
               PERFORM C50-UPD-RESRC THRU C55-EXIT                              
             WHEN OTHER                                                         
               MOVE 'RTYP'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource type is undefined.'                 
                                       TO VSECSERV-RTN-MESG                     
             END-EVALUATE                                                       
                                                                                
           WHEN VSECUPD-RES-GROUP                                               
           WHEN VSECUPD-RES-USER                                                
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               MOVE WINVSEC-GROUP      TO WINVSEC-CLASS                         
               PERFORM C50-UPD-RESRC THRU C55-EXIT                              
                                                                                
           WHEN VSECUPD-RES-TRANS                                               
               MOVE WINVSEC-TRANS      TO WINVSEC-CLASS                         
               PERFORM C50-UPD-RESRC THRU C55-EXIT                              
                                                                                
           WHEN OTHER                                                           
               MOVE 'RCLS'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource class is undefined.'                
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
                                                                                
           GO TO C90-EXIT.                                                      
                                                                                
       C50-UPD-RESRC.                                                           
                                                                                
           PERFORM P00-VALIDATE-REQU THRU P06-EXIT.                             
           IF  VSECSERV-RTN-OKAY                                                
             PERFORM P12-GET-FOR-UPDATE-RESID                                   
             EVALUATE EIBRESP                                                   
             WHEN DFHRESP(NOTFND)                                               
               IF  VSECUPD-RES-USER                                             
                 MOVE 'RUSR'           TO VSECSERV-RTN-CODE                     
                 MOVE 'Cannot add user resource via this method.'               
                                       TO VSECSERV-RTN-MESG                     
                 GO TO C55-EXIT                                                 
               END-IF                                                           
               IF  NOT VSECUPD-RES-GROUP                                        
                 PERFORM P21-INIT-RESOURCE                                      
               ELSE                                                             
                 PERFORM P22-INIT-GROUP                                         
               END-IF                                                           
               IF  VSECUPD-RES-DESC NOT = LOW-VALUES                            
                 MOVE VSECUPD-RES-DESC TO BSM-RSRCDESC                          
               ELSE                                                             
                 MOVE SPACES           TO BSM-RSRCDESC                          
               END-IF                                                           
               IF  NOT VSECUPD-RES-GROUP                                        
                 PERFORM P23-UPDATE-UACC                                        
               END-IF                                                           
               PERFORM Q27-WRITE-BSTCNTL                                        
             WHEN DFHRESP(NORMAL)                                               
               IF  VSECUPD-RES-DESC NOT = LOW-VALUES                            
                 IF  VSECUPD-RES-USER                                           
                   MOVE VSECUPD-RES-DESC TO IUI-US-USRNAME                      
                 ELSE                                                           
                   MOVE VSECUPD-RES-DESC TO BSM-RSRCDESC                        
                 END-IF                                                         
               END-IF                                                           
               IF  NOT VSECUPD-RES-GROUP                                        
               AND NOT VSECUPD-RES-USER                                         
               AND VSECUPD-RES-UACC NOT = LOW-VALUES                            
                 PERFORM P23-UPDATE-UACC                                        
               END-IF                                                           
               IF  VSECUPD-RES-USER                                             
                 PERFORM Q47-REWRITE-IESCNTL                                    
               ELSE                                                             
                 PERFORM Q27-REWRITE-BSTCNTL                                    
               END-IF                                                           
             END-EVALUATE                                                       
           END-IF.                                                              
                                                                                
       C55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       C90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    DEL RESOURCE ENTRY                                          *        
      ******************************************************************        
       D00-DEL-RESOURCE.                                                        
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
             EVALUATE TRUE                                                      
             WHEN VSECUPD-FAC-APPS                                              
             WHEN VSECUPD-FAC-CORP                                              
             WHEN VSECUPD-FAC-ROLE                                              
             WHEN VSECUPD-FAC-VSEC                                              
               MOVE WINVSEC-FACILITY   TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM D50-DEL-RESRC THRU D55-EXIT                              
             WHEN OTHER                                                         
               MOVE 'RTYP'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource type is undefined.'                 
                                       TO VSECSERV-RTN-MESG                     
             END-EVALUATE                                                       
                                                                                
           WHEN VSECUPD-RES-GROUP                                               
               MOVE WINVSEC-GROUP      TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM D50-DEL-RESRC THRU D55-EXIT                              
                                                                                
           WHEN VSECUPD-RES-TRANS                                               
               MOVE WINVSEC-TRANS      TO WINVSEC-CLASS                         
               PERFORM D50-DEL-RESRC THRU D55-EXIT                              
                                                                                
/DLC0/     WHEN VSECUPD-RES-USER                                                
/DLC0/         PERFORM D50-DEL-RESRC THRU D55-EXIT                              
                                                                                
           WHEN OTHER                                                           
               MOVE 'RCLS'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource class is undefined.'                
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
                                                                                
           GO TO D90-EXIT.                                                      
                                                                                
       D50-DEL-RESRC.                                                           
                                                                                
           PERFORM P00-VALIDATE-REQU THRU P06-EXIT.                             
           IF  VSECSERV-RTN-OKAY                                                
             PERFORM P12-GET-FOR-UPDATE-RESID                                   
             IF  EIBRESP = DFHRESP(NORMAL)                                      
/DLC0/         IF  NOT VSECUPD-RES-USER                                         
                 PERFORM Q28-DELETE-SELECTED                                    
/DLC0/         ELSE                                                             
/DLC0/           SET  BIT-DECODE-FROM-BYTE TO TRUE                              
/DLC0/           MOVE IUI-US-USRINFO   TO BIT-BYTE                              
/DLC0/           CALL 'BITMAN'      USING BITMAN-PARMS                          
/DLC0/           IF  BIT-0-IS-OFF                                               
/DLC0/           AND BIT-1-IS-OFF                                               
/DLC0/             PERFORM Q48-DELETE-IESCNTL                                   
/DLC0/           ELSE                                                           
/DLC0/             PERFORM Q48-UNLOCK-IESCNTL                                   
/DLC0/             MOVE 'RUSR'         TO VSECSERV-RTN-CODE                     
/DLC0/             MOVE 'Cannot delete an ICCF user resource via this me        
/DLC0/-                 'thod.'        TO VSECSERV-RTN-MESG                     
/DLC0/           END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-IF.                                                              
                                                                                
       D55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       D90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    GET PERMIT ENTRY                                            *        
      ******************************************************************        
       E00-GET-PERMISSION.                                                      
                                                                                
           PERFORM B00-GET-RESOURCE THRU B90-EXIT.                              
           IF  NOT VSECSERV-RTN-OKAY                                            
           OR  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO E90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE VSECUPD-PER-NAME       TO WINVSEC-RESID.                        
           INSPECT WINVSEC-RESID                                                
               CONVERTING LOWER-CASE   TO UPPER-CASE.                           
                                                                                
      * Is it a group permission?                                               
           MOVE WINVSEC-GROUP          TO WINVSEC-CLASS.                        
           PERFORM Q20-INITKEY-SELECTED.                                        
           MOVE WINVSEC-RESID          TO BSM-RESOURCE.                         
           MOVE GROUP-HEADER           TO BSM-RESOURCE(17:8).                   
                                                                                
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               GO TO E50-GOT-PERMIT                                             
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO E90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Is it a user permission?                                                
           SET  IUI-USER-RECORD        TO TRUE.                                 
           MOVE LOW-VALUE              TO IUI-KEYD.                             
           STRING WINVSEC-RESID(1:8)      DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT.                       
           PERFORM Q46-READEQ-IESCNTL.                                          
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               SET  VSECUPD-PER-USER   TO TRUE                                  
               GO TO E50-GOT-PERMIT                                             
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NOTFND)                                    
               GO TO E90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * Failed to identify permission.                                          
           MOVE '** UNKNOWN PERMIT **' TO VSECUPD-PER-DESC.                     
           MOVE LOW-VALUE              TO VSECUPD-PER-ACCS.                     
                                                                                
           MOVE 'PUNK'                 TO VSECSERV-RTN-CODE.                    
           MOVE 'Unable to determine permission type.'                          
                                       TO VSECSERV-RTN-MESG.                    
           GO TO E90-EXIT.                                                      
                                                                                
       E50-GOT-PERMIT.                                                          
                                                                                
           MOVE WINVSEC-RESID          TO VSECUPD-PER-NAME.                     
           IF  VSECUPD-PER-GROUP                                                
               MOVE BSM-RSRCDESC       TO VSECUPD-PER-DESC                      
           ELSE                                                                 
               MOVE IUI-US-USRNAME     TO VSECUPD-PER-DESC                      
           END-IF.                                                              
                                                                                
           IF  VSECUPD-RES-GROUP                                                
             IF  VSECUPD-PER-GROUP                                              
               MOVE LOW-VALUE          TO VSECUPD-PER-ACCS                      
             ELSE                                                               
               SET  BIT-DECODE-FROM-BYTE TO TRUE                                
               MOVE IUI-US-USRINFO     TO BIT-BYTE                              
               CALL 'BITMAN'        USING BITMAN-PARMS                          
               EVALUATE TRUE                                                    
               WHEN BIT-0-IS-ON                                                 
                AND BIT-1-IS-ON                                                 
                   MOVE '4'            TO VSECUPD-PER-ACCS                      
               WHEN BIT-0-IS-OFF                                                
                AND BIT-1-IS-ON                                                 
                   MOVE '3'            TO VSECUPD-PER-ACCS                      
               WHEN OTHER                                                       
                   MOVE '2'            TO VSECUPD-PER-ACCS                      
               END-EVALUATE                                                     
             END-IF                                                             
           ELSE                                                                 
             PERFORM WITH TEST BEFORE                                           
               VARYING SUB FROM 1 BY 1                                          
                 UNTIL SUB > BSM-FAC-PERMITS                                    
                    OR BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                   
             END-PERFORM                                                        
             IF  SUB > BSM-FAC-PERMITS                                          
             OR  BSM-FAC-USERID(SUB) NOT = VSECUPD-PER-NAME                     
               MOVE LOW-VALUE          TO VSECUPD-PER-ACCS                      
             ELSE                                                               
               EVALUATE TRUE                                                    
               WHEN BSM-FAC-AC-ALTER(SUB)                                       
                                      SET VSECUPD-PER-AC-ALTER  TO TRUE         
               WHEN BSM-FAC-AC-UPDATE(SUB)                                      
                                      SET VSECUPD-PER-AC-UPDATE TO TRUE         
               WHEN BSM-FAC-AC-READ(SUB)                                        
                                      SET VSECUPD-PER-AC-READ   TO TRUE         
               WHEN OTHER                                                       
                                      SET VSECUPD-PER-AC-DENIED TO TRUE         
               END-EVALUATE                                                     
             END-IF                                                             
           END-IF.                                                              
                                                                                
       E90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    CHG PERMIT ENTRY                                            *        
      ******************************************************************        
       F00-UPD-PERMISSION.                                                      
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
             EVALUATE TRUE                                                      
             WHEN VSECUPD-FAC-APPS                                              
             WHEN VSECUPD-FAC-CORP                                              
             WHEN VSECUPD-FAC-ROLE                                              
             WHEN VSECUPD-FAC-VSEC                                              
               MOVE WINVSEC-FACILITY   TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM F50-UPD-PERMIT THRU F55-EXIT                             
             WHEN OTHER                                                         
               MOVE 'RTYP'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource type is undefined.'                 
                                       TO VSECSERV-RTN-MESG                     
             END-EVALUATE                                                       
                                                                                
           WHEN VSECUPD-RES-GROUP                                               
               MOVE WINVSEC-GROUP      TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM F50-UPD-PERMIT THRU F55-EXIT                             
                                                                                
           WHEN VSECUPD-RES-TRANS                                               
               MOVE WINVSEC-TRANS      TO WINVSEC-CLASS                         
               PERFORM F50-UPD-PERMIT THRU F55-EXIT                             
                                                                                
           WHEN OTHER                                                           
               MOVE 'RCLS'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource class is undefined.'                
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
                                                                                
           GO TO F90-EXIT.                                                      
                                                                                
       F50-UPD-PERMIT.                                                          
                                                                                
           PERFORM P00-VALIDATE-REQU THRU P06-EXIT.                             
           IF  VSECSERV-RTN-OKAY                                                
             PERFORM P13-GET-EQUAL-RESID                                        
             IF  VSECSERV-RTN-OKAY                                              
             AND EIBRESP = DFHRESP(NORMAL)                                      
               PERFORM P60-GET-PERMIT-DESC THRU P65-EXIT                        
               IF  VSECSERV-RTN-OKAY                                            
               AND EIBRESP = DFHRESP(NORMAL)                                    
                 IF  NOT VSECUPD-RES-GROUP                                      
                   PERFORM P12-GET-FOR-UPDATE-RESID                             
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                     PERFORM P33-GET-RSC-PERMIT                                 
                     IF  SUB > BSM-FAC-PERMITS                                  
                     OR  BSM-FAC-USERID(SUB) NOT = VSECUPD-PER-NAME             
                       PERFORM P70-INSERT-PERMIT THRU P75-EXIT                  
                     ELSE                                                       
                       PERFORM P80-UPDATE-PERMIT THRU P85-EXIT                  
                     END-IF                                                     
                   END-IF                                                       
                 ELSE                                                           
                   PERFORM P34-GET-GRP-PERMIT                                   
                   EVALUATE EIBRESP                                             
                   WHEN DFHRESP(NOTFND)                                         
                     PERFORM P22-INIT-GROUP                                     
                     MOVE VSECUPD-PER-NAME TO BSM-RESOURCE(17:8)                
                     MOVE VSECUPD-PER-DESC TO BSM-RSRCDESC                      
                     PERFORM Q27-WRITE-BSTCNTL                                  
                   WHEN DFHRESP(NORMAL)                                         
                     MOVE VSECUPD-PER-DESC TO BSM-RSRCDESC                      
                     PERFORM Q27-REWRITE-BSTCNTL                                
                   END-EVALUATE                                                 
                 END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-IF.                                                              
                                                                                
       F55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       F90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    DEL PERMIT ENTRY                                            *        
      ******************************************************************        
       G00-DEL-PERMISSION.                                                      
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
             EVALUATE TRUE                                                      
             WHEN VSECUPD-FAC-APPS                                              
             WHEN VSECUPD-FAC-CORP                                              
             WHEN VSECUPD-FAC-ROLE                                              
             WHEN VSECUPD-FAC-VSEC                                              
               MOVE WINVSEC-FACILITY   TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM G50-DEL-PERMIT THRU G55-EXIT                             
             WHEN OTHER                                                         
               MOVE 'RTYP'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource type is undefined.'                 
                                       TO VSECSERV-RTN-MESG                     
             END-EVALUATE                                                       
                                                                                
           WHEN VSECUPD-RES-GROUP                                               
               MOVE WINVSEC-GROUP      TO WINVSEC-CLASS                         
               INSPECT VSECUPD-RES-NAME                                         
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               PERFORM G50-DEL-PERMIT THRU G55-EXIT                             
                                                                                
           WHEN VSECUPD-RES-TRANS                                               
               MOVE WINVSEC-TRANS      TO WINVSEC-CLASS                         
               PERFORM G50-DEL-PERMIT THRU G55-EXIT                             
                                                                                
           WHEN OTHER                                                           
               MOVE 'RCLS'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested resource class is undefined.'                
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
                                                                                
           GO TO G90-EXIT.                                                      
                                                                                
       G50-DEL-PERMIT.                                                          
                                                                                
           PERFORM P00-VALIDATE-REQU THRU P06-EXIT.                             
           IF  VSECSERV-RTN-OKAY                                                
             PERFORM P13-GET-EQUAL-RESID                                        
             IF  EIBRESP = DFHRESP(NORMAL)                                      
               IF  NOT VSECUPD-RES-GROUP                                        
                 PERFORM P33-GET-RSC-PERMIT                                     
                 IF  SUB > BSM-FAC-PERMITS                                      
                 OR  BSM-FAC-USERID(SUB) NOT = VSECUPD-PER-NAME                 
                   MOVE 'PNTD'         TO VSECSERV-RTN-CODE                     
                   MOVE 'Permission entry was not defined.'                     
                                       TO VSECSERV-RTN-MESG                     
                 ELSE                                                           
                   PERFORM P12-GET-FOR-UPDATE-RESID                             
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                     PERFORM P90-DELETE-PERMIT THRU P95-EXIT                    
                   END-IF                                                       
                 END-IF                                                         
               ELSE                                                             
                 PERFORM P34-GET-GRP-PERMIT                                     
                 IF  EIBRESP = DFHRESP(NORMAL)                                  
                   PERFORM Q28-DELETE-BSTCNTL                                   
                 END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-IF.                                                              
                                                                                
       G55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       G90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
       H00-LIST-SECURITY.                                                       
                                                                                
           EXEC CICS ASSIGN USERID(VSEC-USER-ID) END-EXEC.                      
                                                                                
           MOVE HIGH-VALUES            TO VSEC-TABLE.                           
           MOVE WINVSEC-FACILITY       TO WINVSEC-CLASS.                        
                                                                                
      * set default role level                                                  
           SET  VSEC-RA-DENIED         TO TRUE.                                 
           MOVE 950                    TO VSEC-ROLE-NUMB.                       
                                                                                
      * get user's role level                                                   
           MOVE 'WINROLE.'             TO WINVSEC-PREFIX.                       
                                                                                
           MOVE WINVSEC-CLASS          TO BSM-CLASS.                            
           MOVE WINVSEC-PREFIX         TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO H90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE ZEROES                 TO HALF-WORD.                            
           MOVE LENGTH OF WINVSEC-PREFIX TO CNT.                                
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL QRY-READ = DFHVALUE(READABLE)                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
                OR BSM-CLASS NOT = WINVSEC-CLASS                                
                OR BSM-RESOURCE(1:CNT) NOT = WINVSEC-PREFIX                     
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-CLASS = WINVSEC-CLASS                                      
             AND BSM-RESOURCE(1:CNT) = WINVSEC-PREFIX                           
               MOVE BSM-KEYVALEN       TO LO-LSB                                
               MOVE HALF-WORD          TO WINVSEC-RESIDLEN                      
               PERFORM Q12-CHECK-SECURITY THRU Q15-EXIT                         
               IF  QRY-READ = DFHVALUE(READABLE)                                
                   EVALUATE TRUE                                                
                   WHEN QRY-ALTER  = DFHVALUE(ALTERABLE)                        
                       SET VSEC-RA-ALTER  TO TRUE                               
                   WHEN QRY-UPDATE = DFHVALUE(UPDATABLE)                        
                       SET VSEC-RA-UPDATE TO TRUE                               
                   WHEN QRY-READ   = DFHVALUE(READABLE)                         
                       SET VSEC-RA-READ   TO TRUE                               
                   END-EVALUATE                                                 
                   MOVE BSM-RESOURCE(CNT + 1:3) TO VSEC-ROLE-NUMB               
               END-IF                                                           
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO H90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * get user's authorized companies list                                    
           MOVE 'WINCORP.'             TO WINVSEC-PREFIX.                       
                                                                                
           COMPUTE MAX-ENTRIES = LENGTH OF VSEC-TABLE                           
                               / LENGTH OF VSEC-ENTRY.                          
           MOVE ZEROES                 TO VSEC-ENTRIES.                         
                                                                                
           MOVE WINVSEC-CLASS          TO BSM-CLASS.                            
           MOVE WINVSEC-PREFIX         TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO H90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE ZEROES                 TO HALF-WORD.                            
           MOVE LENGTH OF WINVSEC-PREFIX TO CNT.                                
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL MAX-ENTRIES <= VSEC-ENTRIES                                  
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
                OR BSM-CLASS NOT = WINVSEC-CLASS                                
                OR BSM-RESOURCE(1:CNT) NOT = WINVSEC-PREFIX                     
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-CLASS = WINVSEC-CLASS                                      
             AND BSM-RESOURCE(1:CNT) = WINVSEC-PREFIX                           
               MOVE BSM-KEYVALEN       TO LO-LSB                                
               MOVE HALF-WORD          TO WINVSEC-RESIDLEN                      
               PERFORM Q12-CHECK-SECURITY THRU Q15-EXIT                         
               IF  QRY-READ = DFHVALUE(READABLE)                                
                   ADD  1              TO VSEC-ENTRIES                          
                   EVALUATE TRUE                                                
                   WHEN QRY-ALTER  = DFHVALUE(ALTERABLE)                        
                       SET VSEC-CA-ALTER(VSEC-ENTRIES)  TO TRUE                 
                   WHEN QRY-UPDATE = DFHVALUE(UPDATABLE)                        
                       SET VSEC-CA-UPDATE(VSEC-ENTRIES) TO TRUE                 
                   WHEN QRY-READ   = DFHVALUE(READABLE)                         
                       SET VSEC-CA-READ(VSEC-ENTRIES)   TO TRUE                 
                   END-EVALUATE                                                 
                   MOVE BSM-RESOURCE(CNT + 3:5)                                 
                                       TO VSEC-CORP-NUMB(VSEC-ENTRIES)          
               END-IF                                                           
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO H90-EXIT                                                   
           END-IF.                                                              
                                                                                
      * did the table fill up?                                                  
           IF  MAX-ENTRIES <= VSEC-ENTRIES                                      
               SET  VSECSERV-RTN-FULL  TO TRUE                                  
           END-IF.                                                              
                                                                                
       H90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    LIST FACILITY RESOURCES                                     *        
      ******************************************************************        
       I00-LST-FACILITY.                                                        
           STRING 'WIN' VSECSERV-REQ-FUNC '.'                                   
               DELIMITED BY SIZE     INTO WINVSEC-PREFIX.                       
           MOVE LENGTH OF WINVSEC-PREFIX TO CNT.                                
                                                                                
           SET  BSM-FACILITY           TO TRUE.                                 
           MOVE WINVSEC-PREFIX         TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO I90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO TS-RECD                               
                                          TWO-BYTES.                            
           SET  TS-RSC-FACILITY        TO TRUE.                                 
           MOVE VSECSERV-REQ-FUNC      TO TS-FAC-CODE.                          
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)                                
                OR NOT BSM-FACILITY                                             
                OR BSM-RESOURCE(1:CNT) NOT = WINVSEC-PREFIX                     
               PERFORM Q23-READNEXT-BSTCNTL                                     
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-FACILITY                                                 
               AND BSM-RESOURCE(1:CNT) = WINVSEC-PREFIX                         
                 MOVE BSM-KEYVALEN     TO LO-LSB                                
                 MOVE HALF-WORD        TO LEN                                   
                 MOVE BSM-RESOURCE(1:LEN)                                       
                                       TO TS-RESOURCE                           
                 IF  BSM-FAC-GENERIC                                            
                   MOVE '*'            TO TS-RESOURCE(LEN + 1:1)                
                 END-IF                                                         
                 MOVE BSM-RSRCDESC     TO TS-RSC-DESC                           
                 IF  BSM-FAC-UA-DENIED                                          
                   MOVE SPACE          TO TS-ACCESS                             
                 ELSE                                                           
                   EVALUATE TRUE                                                
                   WHEN BSM-FAC-UA-READ   SET TS-ACC-READ   TO TRUE             
                   WHEN BSM-FAC-UA-UPDATE SET TS-ACC-UPDATE TO TRUE             
                   WHEN BSM-FAC-UA-ALTER  SET TS-ACC-ALTER  TO TRUE             
                   END-EVALUATE                                                 
                 END-IF                                                         
                 PERFORM Q33-TS-WRITEQ                                          
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       I90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    LIST GROUP RESOURCES                                        *        
      ******************************************************************        
       J00-LST-GROUP.                                                           
           SET  BSM-GROUP              TO TRUE.                                 
           IF  WINVSEC-RESID(1:8) > SPACES                                      
               MOVE WINVSEC-RESID(1:8) TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
           ELSE                                                                 
               MOVE SPACES             TO BSM-RESOURCE                          
           END-IF.                                                              
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO J90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO TS-RECD.                              
           SET  TS-RSC-GROUP           TO TRUE.                                 
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)                                
                OR NOT BSM-GROUP                                                
                OR (WINVSEC-RESID(1:8) > SPACES AND                             
                    BSM-RESOURCE(1:8) NOT = WINVSEC-RESID(1:8))                 
               PERFORM Q23-READNEXT-BSTCNTL                                     
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-GROUP                                                    
               AND ((WINVSEC-RESID(1:8) > SPACES AND                            
                     BSM-RESOURCE(1:8)  = WINVSEC-RESID(1:8) AND                
                     BSM-RESOURCE(17:8) NOT = GROUP-HEADER )                    
                   OR                                                           
                    (WINVSEC-RESID(1:8) NOT > SPACES AND                        
                     BSM-RESOURCE(17:8) = GROUP-HEADER )                        
                   )                                                            
               THEN                                                             
                 MOVE BSM-KEYVALEN     TO LO-LSB                                
                 MOVE HALF-WORD        TO LEN                                   
                 IF  BSM-RESOURCE(17:8) = GROUP-HEADER                          
                   MOVE BSM-RESOURCE(1:8)                                       
                                       TO TS-RESOURCE                           
                 ELSE                                                           
                   MOVE BSM-RESOURCE(17:8)                                      
                                       TO TS-RESOURCE                           
                 END-IF                                                         
                 MOVE BSM-RSRCDESC     TO TS-RSC-DESC                           
                 MOVE SPACE            TO TS-ACCESS                             
                 PERFORM Q33-TS-WRITEQ                                          
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       J90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    LIST TRANS RESOURCES                                        *        
      ******************************************************************        
       K00-LST-TRANS.                                                           
           SET  BSM-TRANS              TO TRUE.                                 
           MOVE SPACES                 TO BSM-RESOURCE                          
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO K90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO TS-RECD                               
                                          TWO-BYTES.                            
           SET  TS-RSC-TRANS           TO TRUE.                                 
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)                                
                OR NOT BSM-TRANS                                                
               PERFORM Q23-READNEXT-BSTCNTL                                     
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-TRANS                                                    
                 MOVE BSM-KEYVALEN     TO LO-LSB                                
                 MOVE HALF-WORD        TO LEN                                   
                 MOVE BSM-RESOURCE(1:LEN)                                       
                                       TO TS-RESOURCE                           
                 IF  BSM-FAC-GENERIC                                            
                   MOVE '*'            TO TS-RESOURCE(LEN + 1:1)                
                 END-IF                                                         
                 MOVE BSM-RSRCDESC     TO TS-RSC-DESC                           
                 IF  BSM-FAC-UA-DENIED                                          
                   MOVE SPACE          TO TS-ACCESS                             
                 ELSE                                                           
                   EVALUATE TRUE                                                
                   WHEN BSM-FAC-UA-READ   SET TS-ACC-READ   TO TRUE             
                   WHEN BSM-FAC-UA-UPDATE SET TS-ACC-UPDATE TO TRUE             
                   WHEN BSM-FAC-UA-ALTER  SET TS-ACC-ALTER  TO TRUE             
                   END-EVALUATE                                                 
                 END-IF                                                         
                 PERFORM Q33-TS-WRITEQ                                          
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       K90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    LIST USER RESOURCES                                         *        
      ******************************************************************        
       L00-LST-USERS.                                                           
           SET  IUI-USER-RECORD        TO TRUE.                                 
           MOVE LOW-VALUES             TO IUI-KEYD.                             
                                                                                
           PERFORM Q41-STARTBR-IESCNTL.                                         
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO L90-EXIT                                                   
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO TS-RECD.                              
           SET  TS-RSC-USERS           TO TRUE.                                 
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL EIBRESP NOT = DFHRESP(NORMAL)                                
                OR NOT IUI-USER-RECORD                                          
               PERFORM Q43-READNEXT-IESCNTL                                     
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND IUI-USER-RECORD                                              
                 MOVE SPACES           TO TS-RESOURCE                           
                 STRING IUI-US-USRIDNT    DELIMITED BY LOW-VALUE                
                                     INTO TS-RESOURCE                           
                 MOVE IUI-US-USRNAME   TO TS-RSC-DESC                           
                 SET  BIT-DECODE-FROM-BYTE TO TRUE                              
                 MOVE IUI-US-USRINFO   TO BIT-BYTE                              
                 CALL 'BITMAN'      USING BITMAN-PARMS                          
                 EVALUATE TRUE                                                  
                 WHEN BIT-0-IS-ON     SET TS-ACC-ADMIN TO TRUE                  
                 WHEN BIT-1-IS-ON     SET TS-ACC-PROG  TO TRUE                  
                 WHEN OTHER           SET TS-ACC-USER  TO TRUE                  
                 END-EVALUATE                                                   
                 PERFORM Q33-TS-WRITEQ                                          
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q45-ENDBR-IESCNTL                                        
           END-IF.                                                              
       L90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    CHECK USER SECURITY ACCESS                                  *        
      ******************************************************************        
       M00-CHECK-SECURITY.                                                      
           PERFORM P01-SET-RESID-LEN.                                           
           IF  WINVSEC-RESID NOT > SPACES                                       
           OR  LEN > 0 AND                                                      
               WINVSEC-RESID(1:LEN) NOT NATIONAL-ALPHAMERIC-OR-PERIOD           
           OR  LEN > 0 AND                                                      
               WINVSEC-RESID(POS + 1:) NOT = SPACES                             
           THEN                                                                 
               MOVE 'RNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'Resource Identifier missing or invalid.'                   
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
           MOVE WINVSEC-FACILITY       TO WINVSEC-CLASS.                        
           MOVE WINVSEC-RESID(1:LEN)   TO BSM-RESOURCE.                         
           MOVE LEN                    TO WINVSEC-RESIDLEN.                     
           PERFORM Q12-CHECK-SECURITY.                                          
           EVALUATE TRUE                                                        
           WHEN QRY-ALTER  = DFHVALUE(ALTERABLE)                                
               SET VSECSERV-SEC-ALTERABLE TO TRUE                               
           WHEN QRY-UPDATE = DFHVALUE(UPDATABLE)                                
               SET VSECSERV-SEC-UPDATABLE TO TRUE                               
           WHEN QRY-READ   = DFHVALUE(READABLE)                                 
               SET VSECSERV-SEC-READABLE  TO TRUE                               
           WHEN OTHER                                                           
               SET VSECSERV-SEC-DENIED    TO TRUE                               
           END-EVALUATE.                                                        
       M90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
       P00-VALIDATE-REQU.                                                       
           MOVE VSECUPD-RES-NAME       TO WINVSEC-RESID                         
           IF  VSECUPD-RES-FACILITY                                             
               STRING 'WIN' VSECUPD-FAC-TYPE '.'                                
                   DELIMITED BY SIZE INTO WINVSEC-PREFIX                        
               IF  WINVSEC-RESID(1:8) NOT = WINVSEC-PREFIX                      
                 MOVE WINVSEC-PREFIX   TO WINVSEC-RESID                         
                 MOVE VSECUPD-RES-NAME TO WINVSEC-RESID(9:)                     
               END-IF                                                           
           END-IF.                                                              
                                                                                
       P01-SET-RESID-LEN.                                                       
           MOVE ZEROES                 TO LEN POS.                              
           INSPECT WINVSEC-RESID          TALLYING LEN                          
               FOR CHARACTERS      BEFORE INITIAL ' '.                          
           INSPECT WINVSEC-RESID          TALLYING POS                          
               FOR CHARACTERS      BEFORE INITIAL '*'.                          
                                                                                
           IF  EIBCALEN > LENGTH OF VSECSERV-HEADER                             
               EVALUATE TRUE                                                    
               WHEN VSECUPD-RES-FACILITY  MOVE 39 TO CNT                        
               WHEN VSECUPD-RES-GROUP     MOVE  8 TO CNT                        
                                          GO TO P02-CONTINUE                    
               WHEN VSECUPD-RES-TRANS     MOVE  4 TO CNT                        
               WHEN VSECUPD-RES-USER      MOVE  8 TO CNT                        
                                          GO TO P02-CONTINUE                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
           IF  LEN < POS                                                        
               MOVE LEN                TO POS                                   
           ELSE                                                                 
             IF  POS < CNT                                                      
               MOVE POS                TO LEN                                   
               ADD  1                  TO POS                                   
             END-IF                                                             
           END-IF.                                                              
                                                                                
       P02-CONTINUE.                                                            
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
             IF  WINVSEC-RESID NOT > SPACES                                     
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(1:LEN) NOT NATIONAL-ALPHAMERIC-OR-PERIOD         
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(POS + 1:) NOT = SPACES                           
             THEN                                                               
               MOVE 'RNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'Resource Identifier missing or invalid.'                   
                                       TO VSECSERV-RTN-MESG                     
             END-IF                                                             
           WHEN VSECUPD-RES-GROUP                                               
             IF  WINVSEC-RESID NOT > SPACES                                     
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(1:LEN) NOT NATIONAL-ALPHAMERIC                   
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(LEN + 1:) NOT = SPACES                           
             OR  LEN > CNT                                                      
               MOVE 'RNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'Group Identifier missing or invalid.'                      
                                       TO VSECSERV-RTN-MESG                     
             END-IF                                                             
           WHEN VSECUPD-RES-TRANS                                               
             IF  WINVSEC-RESID NOT > SPACES                                     
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(1:LEN) NOT MIXED-NATIONAL-ALPHAMERIC             
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(POS + 1:) NOT = SPACES                           
             OR  LEN > CNT                                                      
               MOVE 'RNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'Transaction Identifier missing or invalid.'                
                                       TO VSECSERV-RTN-MESG                     
             END-IF                                                             
           WHEN VSECUPD-RES-USER                                                
             IF  WINVSEC-RESID NOT > SPACES                                     
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(1:LEN) NOT NATIONAL-ALPHAMERIC                   
             OR  LEN > 0 AND                                                    
                 WINVSEC-RESID(LEN + 1:) NOT = SPACES                           
             OR  LEN > CNT                                                      
               MOVE 'RNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'User Identifier missing or invalid.'                       
                                       TO VSECSERV-RTN-MESG                     
             END-IF                                                             
           END-EVALUATE.                                                        
           IF  NOT VSECSERV-RTN-OKAY                                            
               GO TO P06-EXIT                                                   
           END-IF.                                                              
                                                                                
       P03-UACCESS.                                                             
           IF  VSECSERV-UPD-RESOURCE                                            
           AND NOT VSECUPD-RES-GROUP                                            
           AND NOT VSECUPD-RES-USER                                             
           AND VSECUPD-RES-UACC > SPACES                                        
           AND VSECUPD-RES-UACC NOT = '2' AND '3' AND '4'                       
               MOVE 'RUAC'             TO VSECSERV-RTN-CODE                     
               MOVE 'Universal Access must be blank, 2, 3, or 4.'               
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
                                                                                
       P04-PERMITS.                                                             
           IF  NOT VSECSERV-RTN-OKAY                                            
           OR  NOT VSECSERV-UPD-PERMISSION                                      
           AND NOT VSECSERV-DEL-PERMISSION                                      
               GO TO P06-EXIT                                                   
           END-IF.                                                              
                                                                                
           INSPECT VSECUPD-PER-NAME                                             
               CONVERTING LOWER-CASE   TO UPPER-CASE.                           
           MOVE ZEROES                 TO LEN.                                  
           INSPECT VSECUPD-PER-NAME       TALLYING                              
               LEN FOR CHARACTERS  BEFORE INITIAL ' '.                          
           MOVE 8                      TO CNT.                                  
                                                                                
           IF  VSECUPD-PER-NAME NOT > SPACES                                    
           OR  LEN > 0 AND                                                      
               VSECUPD-PER-NAME(1:LEN) NOT NATIONAL-ALPHAMERIC                  
           OR  LEN > 0 AND                                                      
               VSECUPD-PER-NAME(LEN + 1:) NOT = SPACES                          
           OR  LEN > CNT                                                        
               MOVE 'PNAM'             TO VSECSERV-RTN-CODE                     
               MOVE 'Permission Identifier missing or invalid.'                 
                                       TO VSECSERV-RTN-MESG                     
               GO TO P06-EXIT                                                   
           END-IF.                                                              
                                                                                
       P05-ACCESS.                                                              
           IF  VSECSERV-UPD-PERMISSION                                          
           AND NOT VSECUPD-RES-GROUP                                            
           AND VSECUPD-PER-ACCS > SPACES                                        
           AND VSECUPD-PER-ACCS NOT = '2' AND '3' AND '4'                       
               MOVE 'PACC'             TO VSECSERV-RTN-CODE                     
               MOVE 'Permission Access must be blank, 2, 3, or 4.'              
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
                                                                                
       P06-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P10-SET-RESID-KEY.                                                       
           PERFORM Q20-INITKEY-SELECTED.                                        
           IF  VSECUPD-RES-GROUP                                                
               MOVE WINVSEC-RESID      TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
           ELSE                                                                 
               STRING WINVSEC-RESID       DELIMITED BY SPACE                    
                 INTO BSM-RESOURCE   WITH POINTER PTR                           
               PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                      
           END-IF.                                                              
       P12-GET-FOR-UPDATE-RESID.                                                
           IF  VSECUPD-RES-USER                                                 
               SET  IUI-USER-RECORD    TO TRUE                                  
               MOVE LOW-VALUE          TO IUI-KEYD                              
               STRING WINVSEC-RESID(1:8)  DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
               PERFORM Q46-READUP-IESCNTL                                       
           ELSE                                                                 
               PERFORM P10-SET-RESID-KEY                                        
               PERFORM Q26-READUP-BSTCNTL                                       
           END-IF.                                                              
       P13-GET-EQUAL-RESID.                                                     
           PERFORM P10-SET-RESID-KEY.                                           
           PERFORM Q26-READEQ-BSTCNTL.                                          
           IF  EIBRESP = DFHRESP(NOTFND)                                        
               MOVE 'RNTF'             TO VSECSERV-RTN-CODE                     
               MOVE 'Resource Identifier was not found.'                        
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
       P15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P21-INIT-RESOURCE.                                                       
           MOVE ZEROES                 TO BSM-FAC-PERMITS.                      
           MOVE LENGTH OF BSM-FACILITY-RECORD                                   
                                       TO BSM-RECL.                             
           MOVE LOW-VALUES           TO BSM-FACILITY-RECORD(1:BSM-RECL).        
           MOVE BSM-RECL               TO BSM-RECLENG.                          
           MOVE X'01'                  TO BSM-VERSION                           
                                          BSM-MODLEVL.                          
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING WINVSEC-RESID           DELIMITED BY SPACE                    
             INTO BSM-RESOURCE       WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
           MOVE PTR                    TO HALF-WORD.                            
           MOVE LO-LSB                 TO BSM-KEYVALEN.                         
           MOVE X'2000002000'          TO BSM-FAC-FILLER.                       
       P22-INIT-GROUP.                                                          
           MOVE LOW-VALUES             TO BSM-GROUP-RECORD.                     
           MOVE LENGTH OF BSM-GROUP-RECORD                                      
                                       TO BSM-RECL                              
                                          BSM-RECLENG.                          
           MOVE X'01'                  TO BSM-VERSION                           
                                          BSM-MODLEVL.                          
           PERFORM Q20-INITKEY-SELECTED                                         
           MOVE WINVSEC-RESID          TO BSM-RESOURCE.                         
           MOVE GROUP-HEADER           TO BSM-RESOURCE(17:8).                   
           MOVE +24                    TO HALF-WORD.                            
           MOVE LO-LSB                 TO BSM-KEYVALEN.                         
       P23-UPDATE-UACC.                                                         
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-UA-ALTER   SET BSM-FAC-UA-ALTER  TO TRUE            
           WHEN VSECUPD-RES-UA-UPDATE  SET BSM-FAC-UA-UPDATE TO TRUE            
           WHEN VSECUPD-RES-UA-READ    SET BSM-FAC-UA-READ   TO TRUE            
           WHEN OTHER                  SET BSM-FAC-UA-DENIED TO TRUE            
           END-EVALUATE.                                                        
       P24-UPDATE-ACCS.                                                         
           EVALUATE TRUE                                                        
           WHEN VSECUPD-PER-AC-ALTER  SET BSM-FAC-AC-ALTER(SUB)  TO TRUE        
           WHEN VSECUPD-PER-AC-UPDATE SET BSM-FAC-AC-UPDATE(SUB) TO TRUE        
           WHEN VSECUPD-PER-AC-READ   SET BSM-FAC-AC-READ(SUB)   TO TRUE        
           WHEN OTHER                 SET BSM-FAC-AC-DENIED(SUB) TO TRUE        
           END-EVALUATE.                                                        
       P25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P33-GET-RSC-PERMIT.                                                      
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > BSM-FAC-PERMITS                                      
                  OR BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                     
           END-PERFORM.                                                         
       P34-GET-GRP-PERMIT.                                                      
           PERFORM Q20-INITKEY-SELECTED.                                        
           MOVE WINVSEC-RESID          TO BSM-RESOURCE.                         
           MOVE VSECUPD-PER-NAME       TO BSM-RESOURCE(17:8).                   
           PERFORM Q26-READUP-BSTCNTL.                                          
       P35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P60-GET-PERMIT-DESC.                                                     
           EVALUATE TRUE                                                        
           WHEN VSECUPD-PER-GROUP                                               
               MOVE WINVSEC-GROUP      TO BSM-CLASS                             
               MOVE VSECUPD-PER-NAME   TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
               MOVE LOW-VALUE          TO BSM-SEQUENCE                          
               PERFORM Q26-READEQ-BSTCNTL                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                 IF  EIBRESP   = DFHRESP(NOTFND)                                
                   MOVE 'PNTF'         TO VSECSERV-RTN-CODE                     
                   MOVE 'Group Permission Identifier was not found.'            
                                       TO VSECSERV-RTN-MESG                     
                 END-IF                                                         
               ELSE                                                             
                 IF  VSECUPD-PER-DESC NOT > SPACES                              
                   MOVE BSM-RSRCDESC   TO VSECUPD-PER-DESC                      
                 END-IF                                                         
               END-IF                                                           
                                                                                
           WHEN VSECUPD-PER-USER                                                
               SET  IUI-USER-RECORD    TO TRUE                                  
               MOVE LOW-VALUE          TO IUI-KEYD                              
               STRING VSECUPD-PER-NAME    DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
               PERFORM Q46-READEQ-IESCNTL                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                 IF  EIBRESP   = DFHRESP(NOTFND)                                
                   MOVE 'PNTF'         TO VSECSERV-RTN-CODE                     
                   MOVE 'User Permission Identifier was not found.'             
                                       TO VSECSERV-RTN-MESG                     
                 END-IF                                                         
               ELSE                                                             
                 IF  VSECUPD-PER-DESC NOT > SPACES                              
                   MOVE IUI-US-USRNAME TO VSECUPD-PER-DESC                      
                 END-IF                                                         
               END-IF                                                           
                                                                                
           WHEN OTHER                                                           
               MOVE 'PTYP'             TO VSECSERV-RTN-CODE                     
               MOVE 'The requested permission type is undefined.'               
                                       TO VSECSERV-RTN-MESG                     
           END-EVALUATE.                                                        
       P65-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P70-INSERT-PERMIT.                                                       
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > BSM-FAC-PERMITS                                      
                  OR BSM-FAC-USERID(SUB) >= VSECUPD-PER-NAME                    
           END-PERFORM.                                                         
           IF  SUB <= BSM-FAC-PERMITS                                           
           AND BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                           
               PERFORM Q28-UNLOCK-BSTCNTL                                       
               MOVE 'PDUP'             TO VSECSERV-RTN-CODE                     
               MOVE 'Duplicate permission entry encountered.'                   
                                       TO VSECSERV-RTN-MESG                     
           ELSE                                                                 
               MOVE BSM-FAC-PERMITS    TO POS                                   
               ADD  LENGTH OF BSM-FAC-PERMIT                                    
                                       TO BSM-RECL                              
                                          BSM-RECLENG                           
               ADD  1                  TO BSM-FAC-PERMITS                       
               PERFORM WITH TEST BEFORE                                         
                 VARYING POS FROM POS BY -1                                     
                   UNTIL POS < SUB                                              
                 MOVE BSM-FAC-PERMIT(POS)                                       
                                       TO BSM-FAC-PERMIT(POS + 1)               
               END-PERFORM                                                      
               MOVE LOW-VALUES         TO BSM-FAC-PERMIT(SUB)                   
               MOVE VSECUPD-PER-NAME   TO BSM-FAC-USERID(SUB)                   
               PERFORM P24-UPDATE-ACCS                                          
               PERFORM Q27-REWRITE-BSTCNTL                                      
           END-IF.                                                              
       P75-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P80-UPDATE-PERMIT.                                                       
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > BSM-FAC-PERMITS                                      
                  OR BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                     
           END-PERFORM.                                                         
           IF  SUB <= BSM-FAC-PERMITS                                           
           AND BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                           
               IF  VSECUPD-PER-ACCS NOT = LOW-VALUE                             
                   PERFORM P24-UPDATE-ACCS                                      
               END-IF                                                           
               PERFORM Q27-REWRITE-BSTCNTL                                      
           ELSE                                                                 
               PERFORM Q28-UNLOCK-BSTCNTL                                       
               MOVE 'PUND'             TO VSECSERV-RTN-CODE                     
               MOVE 'Undefined permission entry encountered.'                   
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
       P85-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P90-DELETE-PERMIT.                                                       
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > BSM-FAC-PERMITS                                      
                  OR BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                     
           END-PERFORM.                                                         
           IF  SUB <= BSM-FAC-PERMITS                                           
           AND BSM-FAC-USERID(SUB) = VSECUPD-PER-NAME                           
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM SUB BY 1                                      
                   UNTIL SUB >= BSM-FAC-PERMITS                                 
                 MOVE BSM-FAC-PERMIT(SUB + 1)                                   
                                       TO BSM-FAC-PERMIT(SUB)                   
               END-PERFORM                                                      
               SUBTRACT 1            FROM BSM-FAC-PERMITS                       
               SUBTRACT LENGTH OF BSM-FAC-PERMIT                                
                                     FROM BSM-RECL                              
                                          BSM-RECLENG                           
               PERFORM Q27-REWRITE-BSTCNTL                                      
           ELSE                                                                 
               PERFORM Q28-UNLOCK-BSTCNTL                                       
               MOVE 'PUND'             TO VSECSERV-RTN-CODE                     
               MOVE 'Undefined permission entry encountered.'                   
                                       TO VSECSERV-RTN-MESG                     
           END-IF.                                                              
       P95-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MORE PERFORMED ROUTINES                                     *        
      ******************************************************************        
                                                                                
       Q10-CHECK-AUTHORIZATION.                                                 
           IF  WINVSEC-RESIDLEN <= LENGTH OF WINVSEC-PREFIX                     
               PERFORM WITH TEST BEFORE                                         
                 VARYING WINVSEC-RESIDLEN                                       
                    FROM LENGTH OF WINVSEC-RESID BY -1                          
                   UNTIL WINVSEC-RESIDLEN <= LENGTH OF WINVSEC-PREFIX           
                      OR BSM-RESOURCE(WINVSEC-RESIDLEN:1) > SPACE               
               END-PERFORM                                                      
               IF  BSM-RESOURCE(WINVSEC-RESIDLEN:1) = HIGH-VALUE                
                   SUBTRACT 1        FROM WINVSEC-RESIDLEN                      
               END-IF                                                           
           END-IF.                                                              
       Q12-CHECK-SECURITY.                                                      
           EXEC CICS QUERY SECURITY                                             
                     RESCLASS(WINVSEC-CLASS)                                    
                     RESID(BSM-RESOURCE)                                        
                     RESIDLENGTH(WINVSEC-RESIDLEN)                              
                     READ(QRY-READ)                                             
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q20-INITKEY-SELECTED.                                                    
           MOVE WINVSEC-CLASS          TO BSM-CLASS                             
           MOVE SPACES                 TO BSM-RESOURCE                          
           MOVE 1                      TO PTR.                                  
           IF  BSM-CLASS = WINVSEC-FACILITY                                     
               STRING 'WIN' VSECUPD-FAC-TYPE '.' DELIMITED BY SIZE              
                 INTO WINVSEC-PREFIX WITH POINTER PTR                           
               IF  WINVSEC-RESID(1:8) NOT = WINVSEC-PREFIX                      
                   MOVE WINVSEC-PREFIX TO BSM-RESOURCE                          
               ELSE                                                             
                   MOVE 1              TO PTR                                   
               END-IF                                                           
           END-IF.                                                              
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
       Q21-STARTBR-BSTCNTL.                                                     
           EXEC CICS STARTBR                                                    
                     DATASET (BSTCNTL)                                          
                     RIDFLD  (BSM-KEY)                                          
                     GTEQ                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q22-RESETBR-BSTCNTL.                                                     
           EXEC CICS RESETBR                                                    
                     DATASET (BSTCNTL)                                          
                     RIDFLD  (BSM-KEY)                                          
                     GTEQ                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q23-READNEXT-BSTCNTL.                                                    
           MOVE +32767                 TO BSM-RECL.                             
           EXEC CICS READNEXT                                                   
                     DATASET (BSTCNTL)                                          
                     INTO    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q23-IS-AUTHORIZED.                                                       
           EVALUATE WINVSEC-CLASS                                               
           WHEN WINVSEC-GROUP                                                   
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(17:8) NOT = GROUP-HEADER                        
                   GO TO Q23-READNEXT-BSTCNTL                                   
               END-IF                                                           
           WHEN WINVSEC-TRANS                                                   
               CONTINUE                                                         
           WHEN OTHER                                                           
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(1:8) = WINVSEC-PREFIX                           
                   MOVE ZEROES         TO WINVSEC-RESIDLEN                      
                   PERFORM Q10-CHECK-AUTHORIZATION THRU Q15-EXIT                
                   IF  EIBRESP = DFHRESP(NOTFND)                                
                   AND EIBRESP2 = +8                                            
                       MOVE DFHRESP(NORMAL) TO EIBRESP                          
                       MOVE DFHVALUE(READABLE) TO QRY-READ                      
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND QRY-READ NOT = DFHVALUE(READABLE)                        
                       GO TO Q23-READNEXT-BSTCNTL                               
                   END-IF                                                       
               END-IF                                                           
           END-EVALUATE.                                                        
       Q23-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND (BSM-CLASS NOT = WINVSEC-CLASS OR                                
                (WINVSEC-CLASS = WINVSEC-FACILITY AND                           
                  BSM-RESOURCE(1:8) NOT = WINVSEC-PREFIX))                      
           THEN                                                                 
             PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-AUTHORIZED                
             MOVE DFHRESP(ENDFILE)     TO EIBRESP                               
           END-IF.                                                              
       Q24-READPREV-BSTCNTL.                                                    
           MOVE +32767                 TO BSM-RECL.                             
           EXEC CICS READPREV                                                   
                     DATASET (BSTCNTL)                                          
                     INTO    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q24-IS-AUTHORIZED.                                                       
           EVALUATE WINVSEC-CLASS                                               
           WHEN WINVSEC-GROUP                                                   
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(17:8) NOT = GROUP-HEADER                        
                   GO TO Q24-READPREV-BSTCNTL                                   
               END-IF                                                           
           WHEN WINVSEC-TRANS                                                   
               CONTINUE                                                         
           WHEN OTHER                                                           
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(1:8) = WINVSEC-PREFIX                           
                   MOVE ZEROES             TO WINVSEC-RESIDLEN                  
                   PERFORM Q10-CHECK-AUTHORIZATION THRU Q15-EXIT                
                   IF  EIBRESP = DFHRESP(NOTFND)                                
                   AND EIBRESP2 = +8                                            
                       MOVE DFHRESP(NORMAL) TO EIBRESP                          
                       MOVE DFHVALUE(READABLE) TO QRY-READ                      
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND QRY-READ NOT = DFHVALUE(READABLE)                        
                       GO TO Q24-READPREV-BSTCNTL                               
                   END-IF                                                       
               END-IF                                                           
           END-EVALUATE.                                                        
       Q24-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND (BSM-CLASS NOT = WINVSEC-CLASS OR                                
                (WINVSEC-CLASS = WINVSEC-FACILITY AND                           
                  BSM-RESOURCE(1:8) NOT = WINVSEC-PREFIX))                      
           THEN                                                                 
             PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-AUTHORIZED                
             MOVE DFHRESP(ENDFILE)     TO EIBRESP                               
           END-IF.                                                              
       Q25-ENDBR-BSTCNTL.                                                       
           EXEC CICS ENDBR                                                      
                     DATASET (BSTCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q26-READEQ-BSTCNTL.                                                      
           MOVE +32767                 TO BSM-RECL.                             
           EXEC CICS READ                                                       
                     DATASET (BSTCNTL)                                          
                     INTO    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q26-READGE-BSTCNTL.                                                      
           MOVE +32767                 TO BSM-RECL.                             
           EXEC CICS READ                                                       
                     DATASET (BSTCNTL)                                          
                     INTO    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     GTEQ                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q26-READUP-BSTCNTL.                                                      
           MOVE +32767                 TO BSM-RECL.                             
           EXEC CICS READ                                                       
                     DATASET (BSTCNTL)                                          
                     INTO    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     EQUAL                                                      
                     UPDATE                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q27-WRITE-BSTCNTL.                                                       
           EXEC CICS WRITE                                                      
                     DATASET (BSTCNTL)                                          
                     FROM    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     RIDFLD  (BSM-KEY)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q27-REWRITE-BSTCNTL.                                                     
           EXEC CICS REWRITE                                                    
                     DATASET (BSTCNTL)                                          
                     FROM    (BSTCNTL-RECORD)                                   
                     LENGTH  (BSM-RECL)                                         
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q28-DELETE-SELECTED.                                                     
           IF  BSM-CLASS = WINVSEC-GROUP                                        
               PERFORM Q28-UNLOCK-BSTCNTL                                       
               EXEC CICS DELETE                                                 
                         DATASET (BSTCNTL)                                      
                         RIDFLD  (BSM-KEY)                                      
                         KEYLENGTH(+24)                                         
                         GENERIC                                                
                         NOHANDLE                                               
               END-EXEC                                                         
           ELSE                                                                 
               EXEC CICS DELETE                                                 
                         DATASET (BSTCNTL)                                      
                         NOHANDLE                                               
               END-EXEC                                                         
           END-IF.                                                              
       Q28-DELETE-BSTCNTL.                                                      
           EXEC CICS DELETE                                                     
                     DATASET (BSTCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q28-UNLOCK-BSTCNTL.                                                      
           EXEC CICS UNLOCK                                                     
                     DATASET (BSTCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q29-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q30-TS-DELETEQ.                                                          
           MOVE ZEROES                 TO TS-ITEM TS-TOTL.                      
           EXEC CICS DELETEQ TS                                                 
                     QUEUE   (TS-QUEUE)                                         
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q31-TS-NUMITEMS.                                                         
           MOVE 1                      TO TS-ITEM.                              
           EXEC CICS READQ TS                                                   
                     QUEUE   (TS-QUEUE)                                         
                     ITEM    (TS-ITEM)                                          
                     INTO    (TS-RECD)                                          
                     NUMITEMS(TS-TOTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP = DFHRESP(QIDERR)                                        
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
               MOVE ZEROES             TO TS-TOTL                               
                                          TS-ITEM                               
           END-IF.                                                              
       Q33-TS-WRITEQ.                                                           
           EXEC CICS WRITEQ TS                                                  
                     QUEUE   (TS-QUEUE)                                         
                     FROM    (TS-RECD)                                          
                     MAIN                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q41-STARTBR-IESCNTL.                                                     
           EXEC CICS STARTBR                                                    
                     DATASET (IESCNTL)                                          
                     RIDFLD  (IUI-KEY)                                          
                     GTEQ                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q43-READNEXT-IESCNTL.                                                    
           EXEC CICS READNEXT                                                   
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     RIDFLD  (IUI-KEY)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q45-ENDBR-IESCNTL.                                                       
           EXEC CICS ENDBR                                                      
                     DATASET (IESCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q46-READEQ-IESCNTL.                                                      
           MOVE LENGTH OF IESCNTL-RECORD TO IUI-RECL.                           
           EXEC CICS READ                                                       
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     LENGTH  (IUI-RECL)                                         
                     RIDFLD  (IUI-KEY)                                          
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q46-READUP-IESCNTL.                                                      
           MOVE LENGTH OF IESCNTL-RECORD TO IUI-RECL.                           
           EXEC CICS READ                                                       
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     LENGTH  (IUI-RECL)                                         
                     RIDFLD  (IUI-KEY)                                          
                     EQUAL                                                      
                     UPDATE                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q47-REWRITE-IESCNTL.                                                     
           EXEC CICS REWRITE                                                    
                     DATASET (IESCNTL)                                          
                     FROM    (IESCNTL-RECORD)                                   
                     LENGTH  (IUI-RECL)                                         
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q48-DELETE-IESCNTL.                                                      
           EXEC CICS DELETE                                                     
                     DATASET (IESCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q48-UNLOCK-IESCNTL.                                                      
           EXEC CICS UNLOCK                                                     
                     DATASET (IESCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q49-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q70-CHANGE-TO-GENERIC.                                                   
           SUBTRACT 1                FROM PTR.                                  
           IF  BSM-RESOURCE(PTR:1) = '*'                                        
               MOVE HIGH-VALUE         TO BSM-RESOURCE(PTR:1)                   
               SET  BSM-FAC-GENERIC    TO TRUE                                  
               SUBTRACT 1            FROM PTR                                   
           ELSE                                                                 
               MOVE LOW-VALUE          TO BSM-SEQUENCE                          
           END-IF.                                                              
       Q75-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q80-CHANGE-TO-ASTERISK.                                                  
           MOVE LOW-VALUES             TO TWO-BYTES.                            
           MOVE BSM-KEYVALEN           TO LO-LSB.                               
           MOVE HALF-WORD              TO LEN.                                  
           MOVE BSM-RESOURCE(1:LEN)    TO WINVSEC-RESID.                        
           IF  BSM-FAC-GENERIC                                                  
               ADD  1                  TO LEN                                   
               MOVE '*'                TO WINVSEC-RESID(LEN:1)                  
           END-IF.                                                              
           EVALUATE TRUE                                                        
           WHEN BSM-FACILITY                                                    
               MOVE LENGTH OF WINVSEC-PREFIX TO CNT                             
               SUBTRACT CNT          FROM LEN                                   
           WHEN BSM-GROUP                                                       
               MOVE ZEROES             TO CNT                                   
               SUBTRACT 8            FROM LEN                                   
           WHEN OTHER                                                           
               MOVE ZEROES             TO CNT                                   
           END-EVALUATE.                                                        
       Q85-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MULTI-FUNCTION ROUTINES                                     *        
      ******************************************************************        
                                                                                
       Q00-LOGIT.                                                               
           EXEC CICS WRITEQ TD                                                  
                     QUEUE   (DTLG)                                             
                     FROM    (LOGF-RECORD)                                      
           END-EXEC.                                                            
           MOVE SPACES                 TO LOGF-MESG.                            
       Q90-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM ERRORS (UNEXPECTED)                                 *        
      ******************************************************************        
       COPY UNEXERRP.                                                           
                                                                                

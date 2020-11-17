      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECREFR.                                                 
       AUTHOR.        DAVE L CLARK.                                             
       DATE-WRITTEN.  MAR 2008.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.                              
       SECURITY.      NONE.                                                     
      *REMARKS.       CICS RESOURCE SECURITY REFRESH BSM DATASPACE.             
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 03/09/2008 DLC ORIGINAL PROGRAM.                                        
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
         03  THIS-PGM                  PIC  X(08)   VALUE 'VSECREFR'.           
         03  DTLG                      PIC  X(04)   VALUE 'DTLG'.               
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.        
                                                                                
      * SUBMISSION JOB CONTROL AREAS                                            
       01  BSM-AREA.                                                            
         03  BSTADMIN                  PIC  X(08)   VALUE 'BSTADMIN'.           
       01  BSM-JCL.                                                             
         03  PIC X(40) VALUE '* $$ JOB JNM=BSTADMIN,DISP=D,CLASS=C    '.        
         03  PIC X(40) VALUE '* $$ LST LST=SYSLST,DISP=D,CLASS=Z      '.        
         03  PIC X(40) VALUE '// JOB    BSTADMIN   Dataspace Refresh  '.        
         03  PIC X(40) VALUE '// EXEC   PGM=BSTADMIN,SIZE=*           '.        
         03  PIC X(40) VALUE '   PERFORM DATASPACE REFRESH            '.        
         03  PIC X(40) VALUE '/* EOD                                  '.        
         03  PIC X(40) VALUE '/& EOJ                                  '.        
         03  PIC X(40) VALUE '* $$ EOJ                                '.        
       01  BSM-TABLE                   REDEFINES    BSM-JCL.                    
         03  BSM-ENTRY                 PIC  X(40)   OCCURS 8.                   
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       01  LOGF-RECORD.                                                         
         03  LOGF-DEST                 PIC  X(1)    VALUE '*'.                  
         03  LOGF-PROG                 PIC  X(9)    VALUE SPACES.               
         03  LOGF-MESG.                                                         
           05  LOGF-NUMB               PIC  ZZZZZZ9.                            
           05  LOGF-TEXT               PIC  X(63).                              
                                                                                
       COPY PRTMAN.                                                             
                                                                                
       COPY UNEXERRW.                                                           
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).                 
                                                                                
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
                                                                                
           MOVE '*'                    TO LOGF-DEST.                            
           MOVE THIS-PGM               TO LOGF-PROG.                            
           MOVE SPACES                 TO LOGF-MESG.                            
                                                                                
           MOVE SPACE                  TO PRT-LNK-REQU.                         
           SET  PRT-PWR-RDRQ           TO TRUE.                                 
           MOVE BSTADMIN               TO PRT-PWR-JOBN.                         
           MOVE 80                     TO PRT-STR-LENG.                         
           COMPUTE CNT                  = LENGTH OF BSM-JCL                     
                                        / LENGTH OF BSM-ENTRY.                  
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > CNT                                                  
                  OR NOT PRINT-COMPLETED                                        
             MOVE BSM-ENTRY(SUB)       TO PRT-STR-DATA                          
             PERFORM S00-JCL-SUBMIT  THRU S05-EXIT                              
           END-PERFORM.                                                         
                                                                                
           IF  PRINT-COMPLETED                                                  
             SET  PRINT-ENDOFMSG       TO TRUE                                  
             PERFORM S00-JCL-SUBMIT  THRU S05-EXIT                              
           END-IF.                                                              
                                                                                
           EXEC CICS RETURN END-EXEC.                                           
           GOBACK.                                                              
                                                                                
      /*****************************************************************        
      *    JCL SUBMISSION ROUTINE                                      *        
      ******************************************************************        
                                                                                
       S00-JCL-SUBMIT.                                                          
           EXEC CICS LINK                                                       
                     PROGRAM(PRT-PGM-NAME)                                      
                     COMMAREA(PRT-PGM-COMM)                                     
                     LENGTH(PRT-PGM-COML)                                       
           END-EXEC.                                                            
       S02-IS-COMPLETE.                                                         
           IF  PRINT-COMPLETED                                                  
               MOVE SPACES             TO PRT-STR-DATA                          
           ELSE                                                                 
               MOVE PRT-LNK-RESP       TO UNEX-RESP                             
               MOVE PRT-LNK-RSP2       TO UNEX-RESP2                            
               STRING PRT-PGM-NAME ' FAILURE: RC=' PRT-LNK-RETN                 
                      ', RS=' UNEX-RESP ', R2=' UNEX-RESP2                      
                   DELIMITED BY SIZE INTO LOGF-MESG                             
               PERFORM Q00-LOGIT THRU Q90-EXIT                                  
               IF  NOT PRINT-ENDOFMSG                                           
                   SET PRINT-ENDOFMSG  TO TRUE                                  
                   PERFORM S00-JCL-SUBMIT                                       
               END-IF                                                           
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
           END-IF.                                                              
       S05-EXIT.                                                                
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
                                                                                

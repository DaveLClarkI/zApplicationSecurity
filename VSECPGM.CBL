 CBL XOPTS(SP)                                                                  
      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECPGM.                                                  
       AUTHOR.        DAVE L CLARK.                                             
       DATE-WRITTEN.  JAN 2008.                                                 
       DATE-COMPILED.                                                           
       INSTALLATION.  WINWHOLESALE GROUP SERVICES.                              
       SECURITY.      INTERNAL, VIA "QUERY SECURITY".                           
      *REMARKS.       CICS RESOURCE SECURITY VIEWER & MAINT.                    
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 01/10/2008 DLC ORIGINAL PROGRAM.                                        
/DLC0/* 07/08/2008 DLC ALLOW NON-ICCF USERS TO BE DELETED.                      
/DLC1/* 09/21/2010 DLC LOGIC ERRORS -- PLUS, UPGRADE COMMON INTERFACE.          
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
      *                                                                         
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES                         
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='VSEC'==              
                                       =='PROGRAM'== BY =='VSECPGM'==.          
         03  VSECMS                    PIC  X(08)   VALUE 'VSECMS  '.           
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.           
         03  DETLMAP                   PIC  X(08)   VALUE 'DETLMAP '.           
         03  DETLXXX                   PIC  X(08)   VALUE 'DETLXXX '.           
         03  ADDPMAP                   PIC  X(08)   VALUE 'ADDPMAP '.           
         03  ADDPXXX                   PIC  X(08)   VALUE 'ADDPXXX '.           
         03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.           
      *                                                                         
      * FIELDS TO MANAGE REFERENCED FILE NAMES                                  
         03  BSTCNTL                   PIC  X(08)   VALUE 'BSTCNTL '.           
         03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.           
      *                                                                         
      * FIELDS TO MANAGE ERROR HANDLING                                         
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.               
           88  NO-ERRORS-FOUND                      VALUE ZEROES.               
           88  ERROR-AT-CURSOR                      VALUE -1.                   
           88  ERRORS-FOUND                         VALUES -9 THRU -1.          
         03  PERMITS-SWITCH            PIC  X(01)   VALUE 'N'.                  
           88  NO-PERMITS-ADDED-OR-DELETED          VALUE 'N'.                  
           88  PERMITS-WERE-ADDED-OR-DELETED        VALUE 'Y'.                  
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.        
      *                                                                         
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS           
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +06.           
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +15.           
         03  DETLHEAD                  PIC S9(04)   BINARY VALUE +06.           
         03  DETLROWS                  PIC S9(04)   BINARY VALUE +15.           
         03  ADDPHEAD                  PIC S9(04)   BINARY VALUE +07.           
         03  ADDPROWS                  PIC S9(04)   BINARY VALUE +14.           
      *                                                                         
      * TO CONVERT DATES AND TIMES                                              
         03  WS-INSDATE                PIC  9(8).                               
         03  NUMDATE                   PIC  9(8).                               
         03  WS-ABSTIME                PIC S9(15)   PACKED-DECIMAL.             
         03  WS-WRKTIME.                                                        
           05  WS-HRSTIME              PIC  9(02).                              
           05                          PIC  X(03).                              
           05  WS-XXXTIME              PIC  X(03).                              
      *                                                                         
      *                                                                         
         03  BSM-RECL                  PIC S9(04)   BINARY.                     
/DLC1/   03  TEMP-CLASS                PIC  X(08).                              
/DLC1/     88  TEMP-CLASS-FACILITY                  VALUE 'FACILITY'.           
/DLC1/     88  TEMP-CLASS-GROUP                     VALUE 'GROUP   '.           
/DLC1/     88  TEMP-CLASS-TCICSTRN                  VALUE 'TCICSTRN'.           
         03  ADDP-PREFIX               PIC  X(08).                              
         03  GROUP-HEADER.                                                      
           05                          PIC  X(2)    VALUE X'0000'.              
           05                          PIC  X(6)    VALUE 'GrpUID'.             
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.               
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
                                                                                
      * PERMISSIONS TEMP STORAGE RECORD LAYOUT                                  
       01  TS-RECD.                                                             
         03  TS-RSC-TYPE               PIC  X(1).                               
           88  TS-RSC-FACILITY                      VALUE 'F'.                  
           88  TS-RSC-GROUP                         VALUE 'G'.                  
           88  TS-RSC-SUBGROUP                      VALUE 'H'.                  
           88  TS-RSC-TRANS                         VALUE 'T'.                  
           88  TS-RSC-USER                          VALUE 'U'.                  
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
                                                                                
      * APPLICATION SECURITY CONTROL AREAS                                      
       01  WINVSEC-CONTROL.                                                     
         03  WINVSEC-CLASS             PIC  X(8)    VALUE 'FACILITY'.           
           88  WINVSEC-CLASS-FACILITY               VALUE 'FACILITY'.           
           88  WINVSEC-CLASS-GROUP                  VALUE 'GROUP   '.           
           88  WINVSEC-CLASS-TCICSTRN               VALUE 'TCICSTRN'.           
         03  WINVSEC-PREFIX            PIC  X(8)    VALUE 'WINVSEC.'.           
         03  WINVSEC-RESID             PIC  X(39).                              
         03  WINVSEC-RESID2            PIC  X(39).                              
         03  WINVSEC-RESIDLEN          PIC S9(9)    BINARY.                     
         03  QRY-READ                  PIC S9(9)    BINARY.                     
           88  QRY-READABLE                         VALUE +35.                  
           88  QRY-NOTREADABLE                      VALUE +36.                  
         03  QRY-UPDATE                PIC S9(9)    BINARY.                     
           88  QRY-UPDATABLE                        VALUE +37.                  
           88  QRY-NOTUPDATABLE                     VALUE +38.                  
         03  QRY-ALTER                 PIC S9(9)    BINARY.                     
           88  QRY-ALTERABLE                        VALUE +52.                  
           88  QRY-NOTALTERABLE                     VALUE +53.                  
         03  MSG-NOLOG                 PIC S9(9)    BINARY VALUE +55.           
       01  WINVSEC-DATA.                                                        
        03 PIC X(44) VALUE 'APPS     View Application Security List   '.        
        03 PIC X(44) VALUE 'CORP       View Company Security List     '.        
        03 PIC X(44) VALUE 'ROLE   View Employee Roles Security List  '.        
        03 PIC X(44) VALUE 'TRAN  View CICS Transaction Security List '.        
        03 PIC X(44) VALUE 'UGRP       View Groups Security List      '.        
        03 PIC X(44) VALUE 'USER        View Users Security List      '.        
        03 PIC X(44) VALUE 'VSEC    View VSEC Internal Security List  '.        
       01  WINVSEC-TABLE               REDEFINES    WINVSEC-DATA.               
         03  WINVSEC-ENTRY                          OCCURS 7.                   
           05  WINVSEC-CODE            PIC  X(4).                               
           05  WINVSEC-TITLE           PIC  X(40).                              
                                                                                
      * BSM SECURITY UPDATE INTERFACE                                           
       01  VSECSERV-AREA.                                                       
         03  VSECSERV                  PIC  X(8)    VALUE 'VSECSERV'.           
         03  VSECREFR                  PIC  X(8)    VALUE 'VSECREFR'.           
       01  VSECSERV-COMMAREA.                                                   
         COPY VSECSERV.                                                         
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       COPY BITMAN.                                                             
                                                                                
       COPY CICSSORT.                                                           
                                                                                
       COPY HEXMAN.                                                             
                                                                                
       COPY UNEXERRW.                                                           
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).                 
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
      * VSEC CICS COMMUNICATION AREA                                            
       01  DFHCOMMAREA.                                                         
         COPY COMMAREA.                                                         
           05  VSEC-CURR-PFX.                                                   
             07  VSEC-CURR-WIN         PIC  X(3).                               
             07  VSEC-CURR-CODE        PIC  X(4).                               
               88  VSEC-APPS-SECTION                VALUE 'APPS'.               
               88  VSEC-CORP-SECTION                VALUE 'CORP'.               
               88  VSEC-ROLE-SECTION                VALUE 'ROLE'.               
               88  VSEC-TRAN-SECTION                VALUE 'TRAN'.               
               88  VSEC-UGRP-SECTION                VALUE 'UGRP'.               
               88  VSEC-USER-SECTION                VALUE 'USER'.               
               88  VSEC-VSEC-SECTION                VALUE 'VSEC'.               
             07  VSEC-CURR-PRD         PIC  X.                                  
           05  VSEC-MAP-TITLE          PIC  X(40).                              
           05  VSEC-APPLICTN-AUTHORITY PIC  X(1).                               
             88  VSEC-APPLICTN-DENIED               VALUE '0'.                  
             88  VSEC-APPLICTN-ACCESSIBLE           VALUE '2', '3', '4'.        
             88  VSEC-APPLICTN-SUPERVISOR           VALUE '3', '4'.             
             88  VSEC-APPLICTN-ADMINISTRATOR        VALUE '4'.                  
           05  VSEC-SECTION-AUTHORITY  PIC  X(1).                               
             88  VSEC-SECTION-DENIED                VALUE '0'.                  
             88  VSEC-SECTION-ACCESSIBLE            VALUE '2', '3', '4'.        
             88  VSEC-SECTION-SUPERVISOR            VALUE '3', '4'.             
             88  VSEC-SECTION-ADMINISTRATOR         VALUE '4'.                  
           05  VSEC-GENERIC-AUTHORITY  PIC  X(1).                               
             88  VSEC-GENERIC-DENIED                VALUE '0'.                  
             88  VSEC-GENERIC-ACCESSIBLE            VALUE '2', '3', '4'.        
             88  VSEC-GENERIC-SUPERVISOR            VALUE '3', '4'.             
             88  VSEC-GENERIC-ADMINISTRATOR         VALUE '4'.                  
           05  VSEC-UPD-SWITCH         PIC  X(1).                               
             88  BSTCNTL-NOCHANGE                   VALUE 'N'.                  
             88  BSTCNTL-UPDATED                    VALUE 'Y'.                  
           05  TS-QUEUE                PIC  X(8).                               
           05  TS-TOTL                 PIC S9(4)    BINARY.                     
           05  TS-ITEM                 PIC S9(4)    BINARY.                     
           05  SS-ITEM                 PIC S9(4)    BINARY.                     
           05  VSEC-SEL-TYPE           PIC  X(1).                               
             88  VSEC-SEL-FACILITY                  VALUE 'F'.                  
             88  VSEC-SEL-GROUP                     VALUE 'G'.                  
             88  VSEC-SEL-SUBGROUP                  VALUE 'H'.                  
             88  VSEC-SEL-TRANS                     VALUE 'T'.                  
             88  VSEC-SEL-USER                      VALUE 'U'.                  
           05  VSEC-SEL-RSRC           PIC  X(39).                              
           05  VSEC-SEL-RDSC           PIC  X(20).                              
           05  VSEC-ADDP-CODE          PIC  X(4).                               
           05  VSEC-PER-TABLE.                                                  
             07  VSEC-PER-ENTRY                     OCCURS 5.                   
               09  VSEC-PER-TYPE       PIC  X(1).                               
               09  VSEC-PER-RSRC       PIC  X(39).                              
               09  VSEC-PER-RDSC       PIC  X(20).                              
           05  VSEC-SAVE-TIOA          PIC  X(1359).                            
           05  VSEC-DETL-TIOA          PIC  X(1563).                            
                                                                                
      * VSEC TEMP COMMUNICATION AREA                                            
       COPY COMMTEMP.                                                           
                                                                                
      * 3270 DATA STREAM CONTROL CHARACTERS                                     
       COPY IBM3270.                                                            
                                                                                
      * BSTCNTL RECORD I/O AREA                                                 
       COPY BSTCNTL.                                                            
                                                                                
      * IESCNTL RECORD I/O AREA                                                 
       COPY IESCNTL.                                                            
                                                                                
      * BMS TERMINAL I/O AREAS                                                  
       COPY VSECMS.                                                             
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION.                                                      
                                                                                
      ******************************************************************        
      *    PROGRAM INITIALIZATION                                      *        
      ******************************************************************        
       A00-VSEC-INITIALIZATION.                                                 
                                                                                
           COPY COMMENTR.                                                       
                                                                                
           COPY COMMMAPT.                                                       
                                                                                
           COPY COMM3270.                                                       
                                                                                
           IF  COMM-CURRPGM = THIS-PGM                                          
               GO TO B50-VSEC-ROUTINE-SELECTION                                 
           END-IF.                                                              
                                                                                
       B00-VSEC-FIRST-TIME-ONLY.                                                
                                                                                
           COPY COMMINIT.                                                       
                                                                                
           IF  COMM-CURRRTN = 'RETN'                                            
           AND COMM-SAVE-FUNC > SPACES                                          
               GO TO B50-VSEC-ROUTINE-SELECTION                                 
           END-IF.                                                              
                                                                                
           EXEC CICS QUERY SECURITY                                             
                     RESCLASS(WINVSEC-CLASS)                                    
                     RESID(WINVSEC-PREFIX)                                      
                     RESIDLENGTH(LENGTH OF WINVSEC-PREFIX)                      
                     READ(QRY-READ)                                             
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
           OR  NOT QRY-READABLE                                                 
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO X00-UNEX-ERR                                           
               END-IF                                                           
               MOVE 1                  TO UNEX-MSGL                             
               STRING 'Sorry, not authorized to application.'                   
                 DELIMITED BY SIZE INTO UNEX-MSG WITH POINTER UNEX-MSGL         
               MOVE UNEX-IC            TO UNEX-MSG(UNEX-MSGL:1)                 
               GO TO X50-SEND-MSG                                               
           END-IF.                                                              
                                                                                
           EXEC CICS QUERY SECURITY                                             
                     RESCLASS(WINVSEC-CLASS)                                    
                     RESID(WINVSEC-PREFIX)                                      
                     RESIDLENGTH(LENGTH OF WINVSEC-PREFIX)                      
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           EVALUATE TRUE                                                        
           WHEN QRY-ALTERABLE SET VSEC-APPLICTN-ADMINISTRATOR TO TRUE           
           WHEN QRY-UPDATABLE SET VSEC-APPLICTN-SUPERVISOR    TO TRUE           
           WHEN QRY-READABLE  SET VSEC-APPLICTN-ACCESSIBLE    TO TRUE           
           WHEN OTHER         SET VSEC-APPLICTN-DENIED        TO TRUE           
           END-EVALUATE.                                                        
                                                                                
           PERFORM P10-FIND-NEXT-CODE.                                          
           IF  VSEC-CURR-CODE NOT > SPACES                                      
               MOVE 1                  TO UNEX-MSGL                             
               STRING 'Sorry, not authorized to any security group.'            
                 DELIMITED BY SIZE INTO UNEX-MSG WITH POINTER UNEX-MSGL         
               MOVE UNEX-IC            TO UNEX-MSG(UNEX-MSGL:1)                 
               GO TO X50-SEND-MSG                                               
           END-IF.                                                              
                                                                                
           EXEC CICS SET TERMINAL(EIBTRMID)                                     
                     TRANIDONLY                                                 
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           MOVE EIBTRNID               TO TS-QUEUE(1:4).                        
           MOVE EIBTRMID               TO TS-QUEUE(5:4).                        
           MOVE ZEROES                 TO TS-ITEM TS-TOTL.                      
           SET  BSTCNTL-NOCHANGE       TO TRUE.                                 
                                                                                
       B50-VSEC-ROUTINE-SELECTION.                                              
                                                                                
           IF (COMM-CURRRTN = 'RETN' OR 'EXIT')                                 
           AND COMM-SAVE-FUNC > SPACES                                          
               MOVE COMM-CURRRTN       TO INIT-FLAG                             
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN                          
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC                        
           END-IF.                                                              
                                                                                
           EVALUATE COMM-CURRRTN                                                
           WHEN 'ADDP' GO TO H00-ADDP-ROUTINE                                   
           WHEN 'DETL' GO TO G00-DETL-ROUTINE                                   
           WHEN 'USER' GO TO F00-USER-ROUTINE                                   
           WHEN 'TRAN' GO TO E00-TRAN-ROUTINE                                   
           WHEN 'UGRP' GO TO D00-UGRP-ROUTINE                                   
           WHEN 'LIST' GO TO C00-LIST-ROUTINE                                   
           END-EVALUATE.                                                        
                                                                                
      /*****************************************************************        
      *    LIST ROUTINE                                                *        
      ******************************************************************        
       C00-LIST-ROUTINE.                                                        
                                                                                
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
           IF  ADDRESS OF LISTMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF LISTMAPI)                               
                         LENGTH(LENGTH OF LISTMAPI)                             
                         INITIMG(LOVALUE)                                       
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'LIST'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
               END-IF                                                           
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP                    
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
       C10-LIST-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(LISTMAP)                                               
                     INTO(LISTMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO C70-LIST-ERRORS                                            
           END-IF.                                                              
                                                                                
       C20-LIST-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               GO TO Z00-VSEC-TERMINATION                                       
                                                                                
           WHEN AIDPFK01                                                        
               MOVE LISTMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK05                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
           WHEN AIDPFK17                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO LIST-MESSAGE-O                        
               GO TO C80-LIST-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO LIST-MESSAGE-O                        
               GO TO C80-LIST-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       C30-LIST-PROCESS.                                                        
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * toggle which resource screen is displayed                               
           IF  EIBAID = AIDPFK05 OR AIDPFK17                                    
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               IF  EIBAID = AIDPFK05                                            
                 PERFORM P10-FIND-NEXT-CODE                                     
               ELSE                                                             
                 PERFORM P11-FIND-PREV-CODE                                     
               END-IF                                                           
               MOVE LOW-VALUES         TO LIST-SKIP-TO-O                        
                                          LIST-RSC-IDNT-O(1)                    
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F50-USER-DISPLAY                    
               WHEN VSEC-TRAN-SECTION GO TO E50-TRAN-DISPLAY                    
               WHEN VSEC-UGRP-SECTION GO TO D50-UGRP-DISPLAY                    
               WHEN OTHER             GO TO C50-LIST-DISPLAY                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
           SET  WINVSEC-CLASS-FACILITY TO TRUE.                                 
                                                                                
      * skip to selected key value                                              
           IF  LIST-SKIP-TO-I > SPACES                                          
               INSPECT LIST-SKIP-TO-I                                           
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               PERFORM Q20-INITKEY-SELECTED                                     
               STRING LIST-SKIP-TO-I      DELIMITED BY SPACE                    
                   INTO BSM-RESOURCE WITH POINTER PTR                           
               PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                      
               PERFORM Q21-STARTBR-BSTCNTL                                      
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               MOVE BSM-RESOURCE       TO WINVSEC-RESID                         
               PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT                     
               IF  WINVSEC-RESID(CNT + 1:LEN) = LIST-SKIP-TO-I                  
                   MOVE LIST-SKIP-TO-I TO LIST-RSC-IDNT-I(1)                    
               ELSE                                                             
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING CNT FROM 1 BY 1 UNTIL CNT > 2                      
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                     MOVE SPACES       TO LIST-RSC-IDNT-I(1)                    
                     MOVE 'This is the first entry.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                     MOVE BSM-RESOURCE TO WINVSEC-RESID                         
                     PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT               
                     MOVE WINVSEC-RESID(CNT + 1:LEN)                            
                                       TO LIST-RSC-IDNT-I(1)                    
                   END-IF                                                       
               END-IF                                                           
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO LIST-SKIP-TO-O.                       
                                                                                
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               IF  EIBAID = AIDPFK06                                            
                   PERFORM Q26-READGE-BSTCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND BSM-RESOURCE(1:8) = VSEC-CURR-PFX                        
                   AND BSM-RESOURCE(9:) = LIST-RSC-IDNT-I(1)                    
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O         
                   ELSE                                                         
                       MOVE LOW-VALUES TO LIST-RSC-IDNT-O(1)                    
                   END-IF                                                       
               ELSE                                                             
                   STRING HIGH-VALUE DELIMITED BY SPACE                         
                     INTO BSM-RESOURCE WITH POINTER PTR                         
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                   END-IF                                                       
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   IF  BSM-RESOURCE(1:8) = VSEC-CURR-PFX                        
                   AND BSM-RESOURCE(9:) = LIST-RSC-IDNT-I(1)                    
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                       MOVE BSM-RESOURCE(9:) TO LIST-RSC-IDNT-O(1)              
                   END-IF                                                       
                   PERFORM Q25-ENDBR-BSTCNTL                                    
               END-IF                                                           
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT LISTHEAD         FROM ROW.                                  
                                                                                
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               STRING LIST-RSC-IDNT-I(1)  DELIMITED BY SPACE                    
                   INTO BSM-RESOURCE WITH POINTER PTR                           
               PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                      
               IF  EIBAID = AIDPFK07                                            
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = LISTROWS - ROW                             
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O        
                       PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO C70-LIST-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = ROW - 1                                    
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O         
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO C70-LIST-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
               MOVE BSM-RESOURCE(9:)   TO LIST-RSC-IDNT-O(1)                    
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
           IF  ROW >= 1 AND <= LISTROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > LISTROWS                                         
                      OR LIST-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > LISTROWS                                               
                   MOVE 'P'            TO LIST-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM LISTROWS BY -1 UNTIL IDX < 1                      
               INSPECT LIST-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               IF  LIST-SELECT-I(IDX) = 'A' OR 'D' OR 'P'                       
                   PERFORM C40-LIST-UPDATES THRU C45-EXIT                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO C80-LIST-OUTPUT                                            
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO C70-LIST-ERRORS                                            
           END-IF.                                                              
                                                                                
           GO TO C50-LIST-DISPLAY.                                              
                                                                                
       C40-LIST-UPDATES.                                                        
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           EVALUATE LIST-SELECT-I(IDX)                                          
           WHEN 'P'    SET VSECSERV-GET-RESOURCE TO TRUE                        
           WHEN 'A'    SET VSECSERV-UPD-RESOURCE TO TRUE                        
           WHEN 'D'    SET VSECSERV-DEL-RESOURCE TO TRUE                        
           END-EVALUATE.                                                        
           SET  VSECUPD-RES-FACILITY   TO TRUE.                                 
           MOVE VSEC-CURR-CODE         TO VSECUPD-FAC-TYPE.                     
           MOVE LIST-RSC-IDNT-I(IDX)   TO VSECUPD-RES-NAME.                     
           IF  LIST-RSC-DESC-L(IDX) > ZEROES                                    
           OR  LIST-RSC-DESC-F(IDX) = FLDMODFY                                  
             MOVE LIST-RSC-DESC-I(IDX) TO VSECUPD-RES-DESC                      
           END-IF.                                                              
           IF  LIST-UNV-ACCS-L(IDX) > ZEROES                                    
           OR  LIST-UNV-ACCS-F(IDX) = FLDMODFY                                  
             MOVE LIST-UNV-ACCS-I(IDX) TO VSECUPD-RES-UACC                      
           END-IF.                                                              
                                                                                
           IF  LIST-SELECT-I(IDX) NOT = 'P'                                     
               PERFORM P00-BUILD-WINVSEC-RESID THRU P05-EXIT                    
               PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT                     
               IF  LIST-SELECT-I(IDX) = 'D'                                     
               AND NOT VSEC-APPLICTN-ADMINISTRATOR                              
               AND NOT VSEC-SECTION-ADMINISTRATOR                               
               AND NOT VSEC-GENERIC-ADMINISTRATOR                               
               AND NOT QRY-ALTERABLE                                            
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND NOT VSEC-APPLICTN-SUPERVISOR                                 
               AND NOT VSEC-SECTION-SUPERVISOR                                  
               AND NOT VSEC-GENERIC-SUPERVISOR                                  
               AND NOT QRY-UPDATABLE                                            
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND VSECUPD-RES-UACC > SPACES                                    
               AND ((VSEC-SECTION-SUPERVISOR AND                                
                     VSEC-SECTION-AUTHORITY < VSECUPD-RES-UACC)                 
                 OR (VSEC-GENERIC-SUPERVISOR AND                                
                     VSEC-GENERIC-AUTHORITY < VSECUPD-RES-UACC)                 
                 OR (NOT QRY-ALTERABLE AND                                      
                                        '3' < VSECUPD-RES-UACC))                
               THEN                                                             
                   MOVE -1             TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
                   MOVE EXHREVRS       TO LIST-MESSAGE-H                        
                   MOVE 'Not authorized for this action.'                       
                                       TO LIST-MESSAGE-O                        
                   GO TO C45-EXIT                                               
               END-IF                                                           
           END-IF.                                                              
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
             IF  LIST-SELECT-I(IDX) NOT = 'P'                                   
               MOVE '*'                TO LIST-SELECT-O(IDX)                    
               SET  BSTCNTL-UPDATED    TO TRUE                                  
             END-IF                                                             
           ELSE                                                                 
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE SPACES             TO LIST-MESSAGE-O                        
               STRING VSECSERV            DELIMITED BY SPACES                   
                 ': RC=' VSECSERV-RTN-CODE                                      
                   ', "' VSECSERV-RTN-MESG '"'                                  
                 DELIMITED BY SIZE   INTO LIST-MESSAGE-O                        
             END-IF                                                             
           END-IF.                                                              
                                                                                
       C45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       C50-LIST-DISPLAY.                                                        
                                                                                
           MOVE 'LIST'                 TO COMM-CURRRTN.                         
           SET  WINVSEC-CLASS-FACILITY TO TRUE.                                 
                                                                                
      * skip if heading selections changed or no detail lines                   
           IF  LIST-SKIP-TO-I > SPACES                                          
           OR  LIST-RSC-IDNT-I(1) NOT > SPACES                                  
               GO TO C60-LIST-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > LISTROWS                                             
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= LISTROWS                                                  
           AND NO-ERRORS-FOUND                                                  
               EVALUATE LIST-SELECT-I(ROW)                                      
               WHEN 'P'                                                         
                   MOVE '*'            TO LIST-SELECT-O(ROW)                    
                   SET  VSEC-SEL-FACILITY TO TRUE                               
                   MOVE VSEC-CURR-PFX  TO VSEC-SEL-RSRC                         
                   MOVE LIST-RSC-IDNT-I(ROW)                                    
                                       TO VSEC-SEL-RSRC(9:)                     
                   MOVE SPACES         TO VSEC-SEL-RDSC                         
                   MOVE LISTMAPI       TO VSEC-SAVE-TIOA                        
                   MOVE SPACES         TO VSEC-PER-TABLE                        
                   GO TO G00-DETL-ROUTINE                                       
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H                        
                 MOVE 'Unknown option selection.' TO LIST-MESSAGE-O             
               END-EVALUATE                                                     
               GO TO C80-LIST-OUTPUT                                            
           END-IF.                                                              
                                                                                
       C60-LIST-DETAIL.                                                         
                                                                                
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING LIST-RSC-IDNT-I(1)      DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR.                          
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           MOVE ZEROES                 TO HALF-WORD.                            
           MOVE LENGTH OF WINVSEC-PREFIX TO CNT.                                
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                   MOVE LOW-VALUES     TO LIST-SELECT-O(IDX)                    
                   MOVE FLDASBRF       TO LIST-RSC-IDNT-A(IDX)                  
                   MOVE BSM-RESOURCE   TO WINVSEC-RESID                         
                   PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT                 
                   MOVE WINVSEC-RESID(CNT + 1:LEN)                              
                                       TO LIST-RSC-IDNT-O(IDX)                  
                   MOVE BSM-RSRCDESC   TO LIST-RSC-DESC-O(IDX)                  
                   IF  BSM-FAC-UA-DENIED                                        
                       MOVE LOW-VALUE  TO LIST-UNV-ACCS-O(IDX)                  
                   ELSE                                                         
                       EVALUATE TRUE                                            
                       WHEN BSM-FAC-UA-ALTER                                    
                           MOVE '4'    TO LIST-UNV-ACCS-O(IDX)                  
                       WHEN BSM-FAC-UA-UPDATE                                   
                           MOVE '3'    TO LIST-UNV-ACCS-O(IDX)                  
                       WHEN BSM-FAC-UA-READ                                     
                           MOVE '2'    TO LIST-UNV-ACCS-O(IDX)                  
                       END-EVALUATE                                             
                   END-IF                                                       
               ELSE                                                             
                   MOVE  -1            TO LIST-SELECT-L(IDX)                    
                   SUBTRACT 1        FROM IDX                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > LISTROWS                                             
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
             MOVE EXHULINE             TO LIST-RSC-IDNT-H(IDX)                  
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(ENDFILE)                                   
               GO TO C70-LIST-ERRORS                                            
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               GO TO C80-LIST-OUTPUT                                            
           END-IF.                                                              
                                                                                
       C70-LIST-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO LIST-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       C80-LIST-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO LIST-COMPANY-O.                       
           MOVE VSEC-MAP-TITLE         TO LIST-TITLE-O.                         
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(LIST-SYSDATE-O)                                   
                     DATESEP('/')                                               
                     TIME(WS-WRKTIME)                                           
                     TIMESEP(':')                                               
           END-EXEC.                                                            
           IF  WS-HRSTIME > 12                                                  
               MOVE ' PM'              TO WS-XXXTIME                            
               SUBTRACT 12           FROM WS-HRSTIME                            
           ELSE                                                                 
               IF  WS-HRSTIME = 12                                              
                   MOVE ' PM'          TO WS-XXXTIME                            
               ELSE                                                             
                   MOVE ' AM'          TO WS-XXXTIME                            
                   IF  WS-HRSTIME = 00                                          
                       MOVE 12         TO WS-HRSTIME                            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.                       
                                                                                
           MOVE EIBTRMID               TO LIST-TERMID-O.                        
                                                                                
       C85-LIST-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(LISTMAP)                                           
                         FROM(LISTMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       C90-LIST-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(LISTMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    USER GROUP ROUTINE                                          *        
      ******************************************************************        
       D00-UGRP-ROUTINE.                                                        
                                                                                
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
           IF  ADDRESS OF LISTMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF LISTMAPI)                               
                         LENGTH(LENGTH OF LISTMAPI)                             
                         INITIMG(LOVALUE)                                       
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'UGRP'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
               END-IF                                                           
               PERFORM D80-UGRP-OUTPUT THRU D85-UGRP-SENDMAP                    
               GO TO D50-UGRP-DISPLAY                                           
           END-IF.                                                              
                                                                                
       D10-UGRP-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(LISTMAP)                                               
                     INTO(LISTMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO D70-UGRP-ERRORS                                            
           END-IF.                                                              
                                                                                
       D20-UGRP-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               GO TO Z00-VSEC-TERMINATION                                       
                                                                                
           WHEN AIDPFK01                                                        
               MOVE LISTMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK05                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
           WHEN AIDPFK17                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO LIST-MESSAGE-O                        
               GO TO D80-UGRP-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO LIST-MESSAGE-O                        
               GO TO D80-UGRP-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       D30-UGRP-PROCESS.                                                        
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * toggle which resource screen is displayed                               
           IF  EIBAID = AIDPFK05 OR AIDPFK17                                    
               IF  EIBAID = AIDPFK05                                            
                 PERFORM P10-FIND-NEXT-CODE                                     
               ELSE                                                             
                 PERFORM P11-FIND-PREV-CODE                                     
               END-IF                                                           
               MOVE LOW-VALUES         TO LIST-SKIP-TO-O                        
                                          LIST-RSC-IDNT-O(1)                    
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F50-USER-DISPLAY                    
               WHEN VSEC-TRAN-SECTION GO TO E50-TRAN-DISPLAY                    
               WHEN VSEC-UGRP-SECTION GO TO D50-UGRP-DISPLAY                    
               WHEN OTHER             GO TO C50-LIST-DISPLAY                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
           SET  WINVSEC-CLASS-GROUP    TO TRUE.                                 
                                                                                
      * skip to selected key value                                              
           IF  LIST-SKIP-TO-I > SPACES                                          
               INSPECT LIST-SKIP-TO-I                                           
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               PERFORM Q20-INITKEY-SELECTED                                     
               MOVE LIST-SKIP-TO-I     TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
               PERFORM Q21-STARTBR-BSTCNTL                                      
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               IF  BSM-RESOURCE(1:8) = LIST-SKIP-TO-I                           
                   MOVE BSM-RESOURCE(1:8) TO LIST-RSC-IDNT-I(1)                 
               ELSE                                                             
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING CNT FROM 1 BY 1 UNTIL CNT > 2                      
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                     MOVE SPACES       TO LIST-RSC-IDNT-I(1)                    
                     MOVE 'This is the first entry.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                     MOVE BSM-RESOURCE(1:8) TO LIST-RSC-IDNT-I(1)               
                   END-IF                                                       
               END-IF                                                           
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO D50-UGRP-DISPLAY                                           
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO LIST-SKIP-TO-O.                       
                                                                                
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               IF  EIBAID = AIDPFK06                                            
                   PERFORM Q26-READGE-BSTCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND BSM-RESOURCE(1:8) = LIST-RSC-IDNT-I(1)                   
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O         
                   ELSE                                                         
                       MOVE LOW-VALUES TO LIST-RSC-IDNT-O(1)                    
                   END-IF                                                       
               ELSE                                                             
                   MOVE HIGH-VALUES    TO BSM-RESOURCE                          
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                   END-IF                                                       
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   IF  BSM-RESOURCE(1:8) = LIST-RSC-IDNT-I(1)                   
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                       MOVE BSM-RESOURCE(1:8) TO LIST-RSC-IDNT-O(1)             
                   END-IF                                                       
                   PERFORM Q25-ENDBR-BSTCNTL                                    
               END-IF                                                           
               GO TO D50-UGRP-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT LISTHEAD         FROM ROW.                                  
                                                                                
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               MOVE LIST-RSC-IDNT-I(1) TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
               IF  EIBAID = AIDPFK07                                            
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = LISTROWS - ROW                             
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O        
                       PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO D70-UGRP-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = ROW - 1                                    
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO D70-UGRP-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O         
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO D70-UGRP-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
               MOVE BSM-RESOURCE(1:8)  TO LIST-RSC-IDNT-O(1)                    
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO D50-UGRP-DISPLAY                                           
           END-IF.                                                              
                                                                                
           IF  ROW >= 1 AND <= LISTROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > LISTROWS                                         
                      OR LIST-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > LISTROWS                                               
                   MOVE 'P'            TO LIST-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM LISTROWS BY -1 UNTIL IDX < 1                      
               INSPECT LIST-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               IF  LIST-SELECT-I(IDX) = 'A' OR 'D' OR 'P'                       
                   PERFORM D40-UGRP-UPDATES THRU D45-EXIT                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO D80-UGRP-OUTPUT                                            
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO D70-UGRP-ERRORS                                            
           END-IF.                                                              
                                                                                
           GO TO D50-UGRP-DISPLAY.                                              
                                                                                
       D40-UGRP-UPDATES.                                                        
                                                                                
           SET  VSECSERV-RTN-OKAY      TO TRUE.                                 
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           IF  LIST-SELECT-I(IDX) NOT = 'P'                                     
               SET  VSECUPD-RES-GROUP    TO TRUE                                
               MOVE LIST-RSC-IDNT-I(IDX) TO VSECUPD-RES-NAME                    
               PERFORM P00-BUILD-WINVSEC-RESID THRU P05-EXIT                    
               PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT                     
               IF  LIST-SELECT-I(IDX) = 'D'                                     
               AND NOT VSEC-APPLICTN-ADMINISTRATOR                              
               AND NOT VSEC-SECTION-ADMINISTRATOR                               
               AND NOT VSEC-GENERIC-ADMINISTRATOR                               
               AND NOT QRY-ALTERABLE                                            
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND NOT VSEC-APPLICTN-SUPERVISOR                                 
               AND NOT VSEC-SECTION-SUPERVISOR                                  
               AND NOT VSEC-GENERIC-SUPERVISOR                                  
               AND NOT QRY-UPDATABLE                                            
                   MOVE -1             TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
                   MOVE EXHREVRS       TO LIST-MESSAGE-H                        
                   MOVE 'Not authorized for this action.'                       
                                       TO LIST-MESSAGE-O                        
                   GO TO D45-EXIT                                               
               END-IF                                                           
           END-IF.                                                              
                                                                                
           IF  LIST-SELECT-I(IDX) = 'D'                                         
             INSPECT LIST-RSC-IDNT-I(IDX)                                       
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
             SET  BSM-FACILITY         TO TRUE                                  
             MOVE BSM-CLASS            TO WINVSEC-RESID                         
             MOVE SPACES               TO BSM-RESOURCE                          
             MOVE LOW-VALUE            TO BSM-SEQUENCE                          
             PERFORM R10-DEL-GROUP-PERMITS THRU R15-EXIT                        
             IF  VSECSERV-RTN-OKAY                                              
             AND EIBRESP = DFHRESP(NORMAL)                                      
               SET  BSM-TRANS          TO TRUE                                  
               MOVE BSM-CLASS          TO WINVSEC-RESID                         
               MOVE SPACES             TO BSM-RESOURCE                          
               MOVE LOW-VALUE          TO BSM-SEQUENCE                          
               PERFORM R10-DEL-GROUP-PERMITS THRU R15-EXIT                      
             END-IF                                                             
           END-IF.                                                              
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
                                                                                
             MOVE LOW-VALUES           TO VSECSERV-COMMAREA                     
             SET  VSECSERV-TRAN        TO TRUE                                  
             EVALUATE LIST-SELECT-I(IDX)                                        
             WHEN 'P'    SET VSECSERV-GET-RESOURCE TO TRUE                      
             WHEN 'A'    SET VSECSERV-UPD-RESOURCE TO TRUE                      
             WHEN 'D'    SET VSECSERV-DEL-RESOURCE TO TRUE                      
             END-EVALUATE                                                       
             SET  VSECUPD-RES-GROUP    TO TRUE                                  
             MOVE LIST-RSC-IDNT-I(IDX) TO VSECUPD-RES-NAME                      
             IF  LIST-RSC-DESC-L(IDX) > ZEROES                                  
             OR  LIST-RSC-DESC-F(IDX) = FLDMODFY                                
               MOVE LIST-RSC-DESC-I(IDX) TO VSECUPD-RES-DESC                    
             END-IF                                                             
                                                                                
             EXEC CICS LINK                                                     
                       PROGRAM(VSECSERV)                                        
                       COMMAREA(VSECSERV-COMMAREA)                              
                       NOHANDLE                                                 
             END-EXEC                                                           
                                                                                
             IF  VSECSERV-RTN-OKAY                                              
             AND EIBRESP = DFHRESP(NORMAL)                                      
               IF  LIST-SELECT-I(IDX) NOT = 'P'                                 
                 MOVE '*'              TO LIST-SELECT-O(IDX)                    
                 SET  BSTCNTL-UPDATED  TO TRUE                                  
               END-IF                                                           
             ELSE                                                               
               IF  NOT VSECSERV-RTN-OKAY                                        
                 MOVE -1               TO ERROR-SWITCH                          
                                          LIST-RSC-IDNT-L(IDX)                  
                 MOVE FLDUNFST         TO LIST-SELECT-A(IDX)                    
                                          LIST-RSC-DESC-A(IDX)                  
                                          LIST-UNV-ACCS-A(IDX)                  
                 MOVE EXHREVRS         TO LIST-MESSAGE-H                        
                 MOVE SPACES           TO LIST-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO LIST-MESSAGE-O                        
               END-IF                                                           
             END-IF                                                             
                                                                                
           END-IF.                                                              
                                                                                
       D45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       D50-UGRP-DISPLAY.                                                        
                                                                                
           MOVE 'UGRP'                 TO COMM-CURRRTN.                         
           SET  WINVSEC-CLASS-GROUP    TO TRUE.                                 
                                                                                
      * skip if heading selections changed or no detail lines                   
           IF  LIST-SKIP-TO-I > SPACES                                          
           OR  LIST-RSC-IDNT-I(1) NOT > SPACES                                  
               GO TO D60-UGRP-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > LISTROWS                                             
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= LISTROWS                                                  
           AND NO-ERRORS-FOUND                                                  
               EVALUATE LIST-SELECT-I(ROW)                                      
               WHEN 'P'                                                         
                   MOVE '*'            TO LIST-SELECT-O(ROW)                    
                   SET  VSEC-SEL-GROUP TO TRUE                                  
                   MOVE LIST-RSC-IDNT-I(ROW)                                    
                                       TO VSEC-SEL-RSRC                         
                   PERFORM Q20-INITKEY-SELECTED                                 
                   MOVE LIST-RSC-IDNT-I(ROW) TO BSM-RESOURCE                    
                   MOVE GROUP-HEADER   TO BSM-RESOURCE(17:8)                    
                   PERFORM Q26-READEQ-BSTCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                     MOVE BSM-RSRCDESC TO VSEC-SEL-RDSC                         
                   ELSE                                                         
                     MOVE SPACES       TO VSEC-SEL-RDSC                         
                   END-IF                                                       
                   MOVE LISTMAPI       TO VSEC-SAVE-TIOA                        
                   MOVE SPACES         TO VSEC-PER-TABLE                        
                   GO TO G00-DETL-ROUTINE                                       
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H                        
                 MOVE 'Unknown option selection.' TO LIST-MESSAGE-O             
               END-EVALUATE                                                     
               GO TO D80-UGRP-OUTPUT                                            
           END-IF.                                                              
                                                                                
       D60-UGRP-DETAIL.                                                         
                                                                                
           PERFORM Q20-INITKEY-SELECTED.                                        
           MOVE LIST-RSC-IDNT-I(1)     TO BSM-RESOURCE.                         
           MOVE GROUP-HEADER           TO BSM-RESOURCE(17:8).                   
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                 MOVE LOW-VALUES       TO LIST-SELECT-O(IDX)                    
                 MOVE FLDASBRF         TO LIST-RSC-IDNT-A(IDX)                  
                 MOVE BSM-RESOURCE(1:8) TO LIST-RSC-IDNT-O(IDX)                 
                 MOVE BSM-RSRCDESC     TO LIST-RSC-DESC-O(IDX)                  
                 MOVE FLDASDRK         TO LIST-UNV-ACCS-A(IDX)                  
               ELSE                                                             
                 MOVE  -1              TO LIST-SELECT-L(IDX)                    
                 SUBTRACT 1          FROM IDX                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > LISTROWS                                             
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
             MOVE EXHULINE             TO LIST-RSC-IDNT-H(IDX)                  
             MOVE FLDASDRK             TO LIST-UNV-ACCS-A(IDX)                  
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(ENDFILE)                                   
               GO TO D70-UGRP-ERRORS                                            
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               GO TO D80-UGRP-OUTPUT                                            
           END-IF.                                                              
                                                                                
       D70-UGRP-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO LIST-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       D80-UGRP-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO LIST-COMPANY-O.                       
           MOVE VSEC-MAP-TITLE         TO LIST-TITLE-O.                         
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(LIST-SYSDATE-O)                                   
                     DATESEP('/')                                               
                     TIME(WS-WRKTIME)                                           
                     TIMESEP(':')                                               
           END-EXEC.                                                            
           IF  WS-HRSTIME > 12                                                  
               MOVE ' PM'              TO WS-XXXTIME                            
               SUBTRACT 12           FROM WS-HRSTIME                            
           ELSE                                                                 
               IF  WS-HRSTIME = 12                                              
                   MOVE ' PM'          TO WS-XXXTIME                            
               ELSE                                                             
                   MOVE ' AM'          TO WS-XXXTIME                            
                   IF  WS-HRSTIME = 00                                          
                       MOVE 12         TO WS-HRSTIME                            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.                       
                                                                                
           MOVE EIBTRMID               TO LIST-TERMID-O.                        
                                                                                
       D85-UGRP-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(LISTMAP)                                           
                         FROM(LISTMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       D90-UGRP-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(LISTMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    TRAN ROUTINE                                                *        
      ******************************************************************        
       E00-TRAN-ROUTINE.                                                        
                                                                                
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
           IF  ADDRESS OF LISTMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF LISTMAPI)                               
                         LENGTH(LENGTH OF LISTMAPI)                             
                         INITIMG(LOVALUE)                                       
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'TRAN'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
               END-IF                                                           
               PERFORM E80-TRAN-OUTPUT THRU E85-TRAN-SENDMAP                    
               GO TO E50-TRAN-DISPLAY                                           
           END-IF.                                                              
                                                                                
       E10-TRAN-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(LISTMAP)                                               
                     INTO(LISTMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO E70-TRAN-ERRORS                                            
           END-IF.                                                              
                                                                                
       E20-TRAN-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               GO TO Z00-VSEC-TERMINATION                                       
                                                                                
           WHEN AIDPFK01                                                        
               MOVE LISTMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK05                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
           WHEN AIDPFK17                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO LIST-MESSAGE-O                        
               GO TO E80-TRAN-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO LIST-MESSAGE-O                        
               GO TO E80-TRAN-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       E30-TRAN-PROCESS.                                                        
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * toggle which resource screen is displayed                               
           IF  EIBAID = AIDPFK05 OR AIDPFK17                                    
               IF  EIBAID = AIDPFK05                                            
                 PERFORM P10-FIND-NEXT-CODE                                     
               ELSE                                                             
                 PERFORM P11-FIND-PREV-CODE                                     
               END-IF                                                           
               MOVE LOW-VALUES         TO LIST-SKIP-TO-O                        
                                          LIST-RSC-IDNT-O(1)                    
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F50-USER-DISPLAY                    
               WHEN VSEC-TRAN-SECTION GO TO E50-TRAN-DISPLAY                    
               WHEN VSEC-UGRP-SECTION GO TO D50-UGRP-DISPLAY                    
               WHEN OTHER             GO TO C50-LIST-DISPLAY                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
           SET  WINVSEC-CLASS-TCICSTRN TO TRUE.                                 
                                                                                
      * skip to selected key value                                              
           IF  LIST-SKIP-TO-I > SPACES                                          
               PERFORM Q20-INITKEY-SELECTED                                     
               STRING LIST-SKIP-TO-I      DELIMITED BY SPACE                    
                   INTO BSM-RESOURCE WITH POINTER PTR                           
               PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                      
               PERFORM Q21-STARTBR-BSTCNTL                                      
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               MOVE BSM-RESOURCE       TO WINVSEC-RESID                         
               PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT                     
               IF  WINVSEC-RESID(1:LEN) = LIST-SKIP-TO-I                        
                   MOVE LIST-SKIP-TO-I TO LIST-RSC-IDNT-I(1)                    
               ELSE                                                             
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING CNT FROM 1 BY 1 UNTIL CNT > 2                      
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                     MOVE SPACES       TO LIST-RSC-IDNT-I(1)                    
                     MOVE 'This is the first entry.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                     MOVE BSM-RESOURCE TO WINVSEC-RESID                         
                     PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT               
                     MOVE WINVSEC-RESID(1:LEN)                                  
                                       TO LIST-RSC-IDNT-I(1)                    
                   END-IF                                                       
               END-IF                                                           
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO E50-TRAN-DISPLAY                                           
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO LIST-SKIP-TO-O.                       
                                                                                
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               IF  EIBAID = AIDPFK06                                            
                   PERFORM Q26-READGE-BSTCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND BSM-RESOURCE = LIST-RSC-IDNT-I(1)                        
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O         
                   ELSE                                                         
                       MOVE LOW-VALUES TO LIST-RSC-IDNT-O(1)                    
                   END-IF                                                       
               ELSE                                                             
                   MOVE '9999'         TO BSM-RESOURCE                          
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                   END-IF                                                       
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   IF  BSM-RESOURCE = LIST-RSC-IDNT-I(1)                        
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                       MOVE BSM-RESOURCE TO LIST-RSC-IDNT-O(1)                  
                   END-IF                                                       
                   PERFORM Q25-ENDBR-BSTCNTL                                    
               END-IF                                                           
               GO TO E50-TRAN-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT LISTHEAD         FROM ROW.                                  
                                                                                
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               PERFORM Q20-INITKEY-SELECTED                                     
               MOVE LIST-RSC-IDNT-I(1) TO BSM-RESOURCE                          
               IF  EIBAID = AIDPFK07                                            
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = LISTROWS - ROW                             
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O        
                       PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO E70-TRAN-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = ROW - 1                                    
                   END-IF                                                       
                   PERFORM Q21-STARTBR-BSTCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO E70-TRAN-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O         
                       PERFORM Q24-READPREV-BSTCNTL THRU Q24-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO E70-TRAN-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
               MOVE BSM-RESOURCE       TO LIST-RSC-IDNT-O(1)                    
               PERFORM Q25-ENDBR-BSTCNTL                                        
               GO TO E50-TRAN-DISPLAY                                           
           END-IF.                                                              
                                                                                
           IF  ROW >= 1 AND <= LISTROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > LISTROWS                                         
                      OR LIST-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > LISTROWS                                               
                   MOVE 'P'            TO LIST-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM LISTROWS BY -1 UNTIL IDX < 1                      
               INSPECT LIST-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               IF  LIST-SELECT-I(IDX) = 'A' OR 'D' OR 'P'                       
                   PERFORM E40-TRAN-UPDATES THRU E45-EXIT                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO E80-TRAN-OUTPUT                                            
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO E70-TRAN-ERRORS                                            
           END-IF.                                                              
                                                                                
           GO TO E50-TRAN-DISPLAY.                                              
                                                                                
       E40-TRAN-UPDATES.                                                        
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           EVALUATE LIST-SELECT-I(IDX)                                          
           WHEN 'P'    SET VSECSERV-GET-RESOURCE TO TRUE                        
           WHEN 'A'    SET VSECSERV-UPD-RESOURCE TO TRUE                        
           WHEN 'D'    SET VSECSERV-DEL-RESOURCE TO TRUE                        
           END-EVALUATE.                                                        
           SET  VSECUPD-RES-TRANS      TO TRUE.                                 
           MOVE LIST-RSC-IDNT-I(IDX)   TO VSECUPD-RES-NAME.                     
           IF  LIST-RSC-DESC-L(IDX) > ZEROES                                    
           OR  LIST-RSC-DESC-F(IDX) = FLDMODFY                                  
             MOVE LIST-RSC-DESC-I(IDX) TO VSECUPD-RES-DESC                      
           END-IF.                                                              
           IF  LIST-UNV-ACCS-L(IDX) > ZEROES                                    
           OR  LIST-UNV-ACCS-F(IDX) = FLDMODFY                                  
             MOVE LIST-UNV-ACCS-I(IDX) TO VSECUPD-RES-UACC                      
           END-IF.                                                              
                                                                                
           IF  LIST-SELECT-I(IDX) NOT = 'P'                                     
               PERFORM P00-BUILD-WINVSEC-RESID THRU P05-EXIT                    
               PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT                     
               IF  LIST-SELECT-I(IDX) = 'D'                                     
               AND NOT VSEC-APPLICTN-ADMINISTRATOR                              
               AND NOT VSEC-SECTION-ADMINISTRATOR                               
               AND NOT VSEC-GENERIC-ADMINISTRATOR                               
               AND NOT QRY-ALTERABLE                                            
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND NOT VSEC-APPLICTN-SUPERVISOR                                 
               AND NOT VSEC-SECTION-SUPERVISOR                                  
               AND NOT VSEC-GENERIC-SUPERVISOR                                  
               AND NOT QRY-UPDATABLE                                            
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND VSECUPD-RES-UACC > SPACES                                    
               AND ((VSEC-SECTION-SUPERVISOR AND                                
                     VSEC-SECTION-AUTHORITY < VSECUPD-RES-UACC)                 
                 OR (VSEC-GENERIC-SUPERVISOR AND                                
                     VSEC-GENERIC-AUTHORITY < VSECUPD-RES-UACC)                 
                 OR (NOT QRY-ALTERABLE AND                                      
                                        '3' < VSECUPD-RES-UACC))                
               THEN                                                             
                   MOVE -1             TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
                   MOVE EXHREVRS       TO LIST-MESSAGE-H                        
                   MOVE 'Not authorized for this action.'                       
                                       TO LIST-MESSAGE-O                        
                   GO TO E45-EXIT                                               
               END-IF                                                           
           END-IF.                                                              
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
             IF  LIST-SELECT-I(IDX) NOT = 'P'                                   
               MOVE '*'                TO LIST-SELECT-O(IDX)                    
               SET  BSTCNTL-UPDATED    TO TRUE                                  
             END-IF                                                             
           ELSE                                                                 
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
                                          LIST-RSC-IDNT-L(IDX)                  
               MOVE FLDUNFST           TO LIST-SELECT-A(IDX)                    
                                          LIST-RSC-DESC-A(IDX)                  
                                          LIST-UNV-ACCS-A(IDX)                  
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE SPACES             TO LIST-MESSAGE-O                        
               STRING VSECSERV            DELIMITED BY SPACES                   
                 ': RC=' VSECSERV-RTN-CODE                                      
                   ', "' VSECSERV-RTN-MESG '"'                                  
                 DELIMITED BY SIZE   INTO LIST-MESSAGE-O                        
             END-IF                                                             
           END-IF.                                                              
                                                                                
       E45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       E50-TRAN-DISPLAY.                                                        
                                                                                
           MOVE 'TRAN'                 TO COMM-CURRRTN.                         
           SET  WINVSEC-CLASS-TCICSTRN TO TRUE.                                 
                                                                                
      * skip if heading selections changed or no detail lines                   
           IF  LIST-SKIP-TO-I > SPACES                                          
           OR  LIST-RSC-IDNT-I(1) NOT > SPACES                                  
               GO TO E60-TRAN-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > LISTROWS                                             
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= LISTROWS                                                  
           AND NO-ERRORS-FOUND                                                  
               EVALUATE LIST-SELECT-I(ROW)                                      
               WHEN 'P'                                                         
                   MOVE '*'            TO LIST-SELECT-O(ROW)                    
                   SET  VSEC-SEL-TRANS TO TRUE                                  
                   MOVE LIST-RSC-IDNT-I(ROW)                                    
                                       TO VSEC-SEL-RSRC                         
                   PERFORM Q20-INITKEY-SELECTED                                 
                   MOVE LIST-RSC-IDNT-I(ROW) TO BSM-RESOURCE                    
                   PERFORM Q26-READEQ-BSTCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                     MOVE BSM-RSRCDESC TO VSEC-SEL-RDSC                         
                   ELSE                                                         
                     MOVE SPACES       TO VSEC-SEL-RDSC                         
                   END-IF                                                       
                   MOVE LISTMAPI       TO VSEC-SAVE-TIOA                        
                   MOVE SPACES         TO VSEC-PER-TABLE                        
                   GO TO G00-DETL-ROUTINE                                       
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H                        
                 MOVE 'Unknown option selection.' TO LIST-MESSAGE-O             
               END-EVALUATE                                                     
               GO TO E80-TRAN-OUTPUT                                            
           END-IF.                                                              
                                                                                
       E60-TRAN-DETAIL.                                                         
                                                                                
           PERFORM Q20-INITKEY-SELECTED.                                        
           STRING LIST-RSC-IDNT-I(1)      DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR                           
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT.                         
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           MOVE ZEROES                 TO HALF-WORD.                            
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
               PERFORM Q23-READNEXT-BSTCNTL THRU Q23-IS-SELECTED                
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                   MOVE LOW-VALUES     TO LIST-SELECT-O(IDX)                    
                   MOVE FLDASBRF       TO LIST-RSC-IDNT-A(IDX)                  
                   MOVE BSM-RESOURCE   TO WINVSEC-RESID                         
                   PERFORM Q80-CHANGE-TO-ASTERISK THRU Q85-EXIT                 
                   MOVE WINVSEC-RESID(1:LEN)                                    
                                       TO LIST-RSC-IDNT-O(IDX)                  
                   MOVE BSM-RSRCDESC   TO LIST-RSC-DESC-O(IDX)                  
                   IF  BSM-FAC-UA-DENIED                                        
                       MOVE LOW-VALUE  TO LIST-UNV-ACCS-O(IDX)                  
                   ELSE                                                         
                       EVALUATE TRUE                                            
                       WHEN BSM-FAC-UA-ALTER                                    
                           MOVE '4'    TO LIST-UNV-ACCS-O(IDX)                  
                       WHEN BSM-FAC-UA-UPDATE                                   
                           MOVE '3'    TO LIST-UNV-ACCS-O(IDX)                  
                       WHEN BSM-FAC-UA-READ                                     
                           MOVE '2'    TO LIST-UNV-ACCS-O(IDX)                  
                       END-EVALUATE                                             
                   END-IF                                                       
               ELSE                                                             
                   MOVE  -1            TO LIST-SELECT-L(IDX)                    
                   SUBTRACT 1        FROM IDX                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > LISTROWS                                             
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
             MOVE EXHULINE             TO LIST-RSC-IDNT-H(IDX)                  
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(ENDFILE)                                   
               GO TO E70-TRAN-ERRORS                                            
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               GO TO E80-TRAN-OUTPUT                                            
           END-IF.                                                              
                                                                                
       E70-TRAN-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO LIST-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       E80-TRAN-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO LIST-COMPANY-O.                       
           MOVE VSEC-MAP-TITLE         TO LIST-TITLE-O.                         
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(LIST-SYSDATE-O)                                   
                     DATESEP('/')                                               
                     TIME(WS-WRKTIME)                                           
                     TIMESEP(':')                                               
           END-EXEC.                                                            
           IF  WS-HRSTIME > 12                                                  
               MOVE ' PM'              TO WS-XXXTIME                            
               SUBTRACT 12           FROM WS-HRSTIME                            
           ELSE                                                                 
               IF  WS-HRSTIME = 12                                              
                   MOVE ' PM'          TO WS-XXXTIME                            
               ELSE                                                             
                   MOVE ' AM'          TO WS-XXXTIME                            
                   IF  WS-HRSTIME = 00                                          
                       MOVE 12         TO WS-HRSTIME                            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.                       
                                                                                
           MOVE EIBTRMID               TO LIST-TERMID-O.                        
                                                                                
       E85-TRAN-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(LISTMAP)                                           
                         FROM(LISTMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       E90-TRAN-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(LISTMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    USER GROUP ROUTINE                                          *        
      ******************************************************************        
       F00-USER-ROUTINE.                                                        
                                                                                
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
           IF  ADDRESS OF LISTMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET(ADDRESS OF LISTMAPI)                               
                         LENGTH(LENGTH OF LISTMAPI)                             
                         INITIMG(LOVALUE)                                       
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'USER'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
               END-IF                                                           
               PERFORM F80-USER-OUTPUT THRU F85-USER-SENDMAP                    
               GO TO F50-USER-DISPLAY                                           
           END-IF.                                                              
                                                                                
       F10-USER-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(LISTMAP)                                               
                     INTO(LISTMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO F70-USER-ERRORS                                            
           END-IF.                                                              
                                                                                
       F20-USER-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               GO TO Z00-VSEC-TERMINATION                                       
                                                                                
           WHEN AIDPFK01                                                        
               MOVE LISTMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK05                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
           WHEN AIDPFK17                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO LIST-MESSAGE-O                        
               GO TO F80-USER-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO LIST-MESSAGE-O                        
               GO TO F80-USER-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       F30-USER-PROCESS.                                                        
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * toggle which resource screen is displayed                               
           IF  EIBAID = AIDPFK05 OR AIDPFK17                                    
               IF  EIBAID = AIDPFK05                                            
                 PERFORM P10-FIND-NEXT-CODE                                     
               ELSE                                                             
                 PERFORM P11-FIND-PREV-CODE                                     
               END-IF                                                           
               MOVE LOW-VALUES         TO LIST-SKIP-TO-O                        
                                          LIST-RSC-IDNT-O(1)                    
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F50-USER-DISPLAY                    
               WHEN VSEC-TRAN-SECTION GO TO E50-TRAN-DISPLAY                    
               WHEN VSEC-UGRP-SECTION GO TO D50-UGRP-DISPLAY                    
               WHEN OTHER             GO TO C50-LIST-DISPLAY                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
           SET  WINVSEC-CLASS-GROUP    TO TRUE.                                 
                                                                                
      * skip to selected key value                                              
           IF  LIST-SKIP-TO-I > SPACES                                          
               INSPECT LIST-SKIP-TO-I                                           
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               PERFORM Q40-INITKEY-IESCNTL                                      
               STRING LIST-SKIP-TO-I      DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
               PERFORM Q41-STARTBR-IESCNTL                                      
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               PERFORM Q43-READNEXT-IESCNTL THRU Q43-IS-SELECTED                
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
               MOVE SPACES             TO WINVSEC-RESID                         
               STRING IUI-US-USRIDNT      DELIMITED BY LOW-VALUE                
                                     INTO WINVSEC-RESID                         
               IF  WINVSEC-RESID = LIST-SKIP-TO-I                               
                   MOVE WINVSEC-RESID  TO LIST-RSC-IDNT-I(1)                    
               ELSE                                                             
                   PERFORM Q44-READPREV-IESCNTL THRU Q44-IS-SELECTED            
                     VARYING CNT FROM 1 BY 1 UNTIL CNT > 2                      
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                     MOVE SPACES       TO LIST-RSC-IDNT-I(1)                    
                     MOVE 'This is the first entry.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                     MOVE SPACES       TO WINVSEC-RESID                         
                     STRING IUI-US-USRIDNT DELIMITED BY LOW-VALUE               
                                     INTO WINVSEC-RESID                         
                     MOVE WINVSEC-RESID TO LIST-RSC-IDNT-I(1)                   
                   END-IF                                                       
               END-IF                                                           
               PERFORM Q45-ENDBR-IESCNTL                                        
               GO TO F50-USER-DISPLAY                                           
           END-IF.                                                              
                                                                                
           MOVE LOW-VALUES             TO LIST-SKIP-TO-O.                       
                                                                                
      * scroll to first/last page                                               
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               PERFORM Q40-INITKEY-IESCNTL                                      
               IF  EIBAID = AIDPFK06                                            
                   PERFORM Q46-READGE-IESCNTL                                   
                   MOVE SPACES         TO WINVSEC-RESID                         
                   STRING IUI-US-USRIDNT  DELIMITED BY LOW-VALUE                
                                     INTO WINVSEC-RESID                         
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND WINVSEC-RESID = LIST-RSC-IDNT-I(1)                       
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O         
                   ELSE                                                         
                       MOVE LOW-VALUES TO LIST-RSC-IDNT-O(1)                    
                   END-IF                                                       
               ELSE                                                             
                   MOVE HIGH-VALUES    TO IUI-US-USRIDNT                        
                   PERFORM Q41-STARTBR-IESCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q43-READNEXT-IESCNTL THRU Q43-IS-SELECTED            
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       PERFORM Q44-READPREV-IESCNTL THRU Q44-IS-SELECTED        
                   END-IF                                                       
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   MOVE SPACES         TO WINVSEC-RESID                         
                   STRING IUI-US-USRIDNT  DELIMITED BY LOW-VALUE                
                                     INTO WINVSEC-RESID                         
                   IF  WINVSEC-RESID = LIST-RSC-IDNT-I(1)                       
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O          
                   ELSE                                                         
                       MOVE WINVSEC-RESID TO LIST-RSC-IDNT-O(1)                 
                   END-IF                                                       
                   PERFORM Q45-ENDBR-IESCNTL                                    
               END-IF                                                           
               GO TO F50-USER-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT LISTHEAD         FROM ROW.                                  
                                                                                
      * scroll backward/forward by page                                         
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               PERFORM Q40-INITKEY-IESCNTL                                      
               STRING LIST-RSC-IDNT-I(1)  DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
               IF  EIBAID = AIDPFK07                                            
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = LISTROWS - ROW                             
                   END-IF                                                       
                   PERFORM Q41-STARTBR-IESCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q44-READPREV-IESCNTL THRU Q44-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O        
                       PERFORM Q43-READNEXT-IESCNTL THRU Q43-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO F70-USER-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-RSC-IDNT-I(ROW) <= SPACES                           
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = ROW - 1                                    
                   END-IF                                                       
                   PERFORM Q41-STARTBR-IESCNTL                                  
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   PERFORM Q43-READNEXT-IESCNTL THRU Q43-IS-SELECTED            
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO F70-USER-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O         
                       PERFORM Q44-READPREV-IESCNTL THRU Q44-IS-SELECTED        
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO F70-USER-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
               MOVE SPACES             TO WINVSEC-RESID                         
               STRING IUI-US-USRIDNT      DELIMITED BY LOW-VALUE                
                                     INTO WINVSEC-RESID                         
               MOVE WINVSEC-RESID      TO LIST-RSC-IDNT-O(1)                    
               PERFORM Q45-ENDBR-IESCNTL                                        
               GO TO F50-USER-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * set default line seletion                                               
           IF  ROW >= 1 AND <= LISTROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > LISTROWS                                         
                      OR LIST-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > LISTROWS                                               
                   MOVE 'P'            TO LIST-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM LISTROWS BY -1 UNTIL IDX < 1                      
               INSPECT LIST-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
/DLC0/*        IF  LIST-SELECT-I(IDX) = 'A' OR 'P'                              
/DLC0/         IF  LIST-SELECT-I(IDX) = 'A' OR 'D' OR 'P'                       
                   PERFORM F40-USER-UPDATES THRU F45-EXIT                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO F80-USER-OUTPUT                                            
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO F70-USER-ERRORS                                            
           END-IF.                                                              
                                                                                
           GO TO F50-USER-DISPLAY.                                              
                                                                                
       F40-USER-UPDATES.                                                        
                                                                                
           SET  VSECSERV-RTN-OKAY      TO TRUE.                                 
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           IF  LIST-SELECT-I(IDX) NOT = 'P'                                     
/DLC0/         IF  LIST-SELECT-I(IDX) = 'D'                                     
/DLC0/         AND NOT VSEC-APPLICTN-ADMINISTRATOR                              
/DLC0/         AND NOT VSEC-SECTION-ADMINISTRATOR                               
               OR  LIST-SELECT-I(IDX) = 'A'                                     
               AND NOT VSEC-APPLICTN-SUPERVISOR                                 
               AND NOT VSEC-SECTION-SUPERVISOR                                  
                   MOVE -1             TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
                   MOVE EXHREVRS       TO LIST-MESSAGE-H                        
                   MOVE 'Not authorized for this action.'                       
                                       TO LIST-MESSAGE-O                        
                   GO TO F45-EXIT                                               
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC0/     IF  LIST-SELECT-I(IDX) = 'D'                                         
/DLC0/       INSPECT LIST-RSC-IDNT-I(IDX)                                       
/DLC0/           CONVERTING LOWER-CASE TO UPPER-CASE                            
/DLC0/       PERFORM Q40-INITKEY-IESCNTL                                        
/DLC0/       STRING LIST-RSC-IDNT-I(IDX) DELIMITED BY SPACE                     
/DLC0/                               INTO IUI-US-USRIDNT                        
/DLC0/       PERFORM Q46-READEQ-IESCNTL                                         
/DLC0/       IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
/DLC0/           MOVE -1               TO ERROR-SWITCH                          
/DLC0/                                    LIST-SELECT-L(IDX)                    
/DLC0/           MOVE EXHREVRS         TO LIST-MESSAGE-H                        
/DLC0/           MOVE 'User is not defined.'                                    
/DLC0/                                 TO LIST-MESSAGE-O                        
/DLC0/           GO TO F45-EXIT                                                 
/DLC0/       END-IF                                                             
/DLC0/       SET  BIT-DECODE-FROM-BYTE TO TRUE                                  
/DLC0/       MOVE IUI-US-USRINFO       TO BIT-BYTE                              
/DLC0/       CALL 'BITMAN'          USING BITMAN-PARMS                          
/DLC0/       IF  BIT-1-IS-ON                                                    
/DLC0/           MOVE -1               TO ERROR-SWITCH                          
/DLC0/                                    LIST-SELECT-L(IDX)                    
/DLC0/           MOVE EXHREVRS         TO LIST-MESSAGE-H                        
/DLC0/           MOVE 'ICCF User cannot be deleted from here.'                  
/DLC0/                                 TO LIST-MESSAGE-O                        
/DLC0/           GO TO F45-EXIT                                                 
/DLC0/       END-IF                                                             
                                                                                
/DLC0/       SET  BSM-FACILITY         TO TRUE                                  
/DLC0/       MOVE BSM-CLASS            TO WINVSEC-RESID                         
/DLC0/       MOVE SPACES               TO BSM-RESOURCE                          
/DLC0/       MOVE LOW-VALUE            TO BSM-SEQUENCE                          
/DLC0/       PERFORM R10-DEL-GROUP-PERMITS THRU R15-EXIT                        
/DLC0/       IF  VSECSERV-RTN-OKAY                                              
/DLC0/       AND EIBRESP = DFHRESP(NORMAL)                                      
/DLC0/         SET  BSM-TRANS          TO TRUE                                  
/DLC0/         MOVE BSM-CLASS          TO WINVSEC-RESID                         
/DLC0/         MOVE SPACES             TO BSM-RESOURCE                          
/DLC0/         MOVE LOW-VALUE          TO BSM-SEQUENCE                          
/DLC0/         PERFORM R10-DEL-GROUP-PERMITS THRU R15-EXIT                      
/DLC0/       END-IF                                                             
/DLC0/       IF  VSECSERV-RTN-OKAY                                              
/DLC0/       AND EIBRESP = DFHRESP(NORMAL)                                      
/DLC0/         SET  BSM-GROUP          TO TRUE                                  
/DLC0/         MOVE SPACES             TO BSM-RESOURCE                          
/DLC0/         MOVE LOW-VALUE          TO BSM-SEQUENCE                          
/DLC0/         PERFORM R00-DEL-GROUP-MEMBERS THRU R05-EXIT                      
/DLC0/       END-IF                                                             
/DLC0/     END-IF.                                                              
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
                                                                                
             MOVE LOW-VALUES           TO VSECSERV-COMMAREA                     
             SET  VSECSERV-TRAN        TO TRUE                                  
             EVALUATE LIST-SELECT-I(IDX)                                        
             WHEN 'P'    SET VSECSERV-GET-RESOURCE TO TRUE                      
             WHEN 'A'    SET VSECSERV-UPD-RESOURCE TO TRUE                      
/DLC0/       WHEN 'D'    SET VSECSERV-DEL-RESOURCE TO TRUE                      
             END-EVALUATE                                                       
             SET  VSECUPD-RES-USER     TO TRUE                                  
             MOVE LIST-RSC-IDNT-I(IDX) TO VSECUPD-RES-NAME                      
             IF  LIST-RSC-DESC-L(IDX) > ZEROES                                  
             OR  LIST-RSC-DESC-F(IDX) = FLDMODFY                                
               MOVE LIST-RSC-DESC-I(IDX) TO VSECUPD-RES-DESC                    
             END-IF                                                             
                                                                                
             EXEC CICS LINK                                                     
                       PROGRAM(VSECSERV)                                        
                       COMMAREA(VSECSERV-COMMAREA)                              
                       NOHANDLE                                                 
             END-EXEC                                                           
                                                                                
             IF  VSECSERV-RTN-OKAY                                              
             AND EIBRESP = DFHRESP(NORMAL)                                      
               IF  LIST-SELECT-I(IDX) NOT = 'P'                                 
                 MOVE '*'              TO LIST-SELECT-O(IDX)                    
/DLC0/           SET  BSTCNTL-UPDATED  TO TRUE                                  
               END-IF                                                           
             ELSE                                                               
               IF  NOT VSECSERV-RTN-OKAY                                        
                 MOVE -1               TO ERROR-SWITCH                          
                                          LIST-RSC-IDNT-L(IDX)                  
                 MOVE FLDUNFST         TO LIST-SELECT-A(IDX)                    
                                          LIST-RSC-DESC-A(IDX)                  
                                          LIST-UNV-ACCS-A(IDX)                  
                 MOVE EXHREVRS         TO LIST-MESSAGE-H                        
                 MOVE SPACES           TO LIST-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO LIST-MESSAGE-O                        
               END-IF                                                           
             END-IF                                                             
                                                                                
           END-IF.                                                              
                                                                                
       F45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       F50-USER-DISPLAY.                                                        
                                                                                
           MOVE 'USER'                 TO COMM-CURRRTN.                         
           SET  WINVSEC-CLASS-GROUP    TO TRUE.                                 
                                                                                
      * skip if heading selections changed or no detail lines                   
           IF  LIST-SKIP-TO-I > SPACES                                          
           OR  LIST-RSC-IDNT-I(1) NOT > SPACES                                  
               GO TO F60-USER-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > LISTROWS                                             
                  OR LIST-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= LISTROWS                                                  
           AND NO-ERRORS-FOUND                                                  
               EVALUATE LIST-SELECT-I(ROW)                                      
               WHEN 'P'                                                         
                   MOVE '*'            TO LIST-SELECT-O(ROW)                    
                   SET  VSEC-SEL-USER  TO TRUE                                  
                   MOVE LIST-RSC-IDNT-I(ROW)                                    
                                       TO VSEC-SEL-RSRC                         
                   PERFORM Q40-INITKEY-IESCNTL                                  
                   STRING LIST-RSC-IDNT-I(ROW) DELIMITED BY SPACE               
                                     INTO IUI-US-USRIDNT                        
                   PERFORM Q46-READEQ-IESCNTL                                   
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                     MOVE IUI-US-USRNAME TO VSEC-SEL-RDSC                       
                   ELSE                                                         
                     MOVE SPACES       TO VSEC-SEL-RDSC                         
                   END-IF                                                       
                   MOVE LISTMAPI       TO VSEC-SAVE-TIOA                        
                   MOVE SPACES         TO VSEC-PER-TABLE                        
                   GO TO G00-DETL-ROUTINE                                       
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H                        
                 IF  LIST-SELECT-I(ROW) = 'D'                                   
                   MOVE 'User delete not allowed from this application.'        
                                       TO LIST-MESSAGE-O                        
                 ELSE                                                           
                   MOVE 'Unknown option selection.'                             
                                       TO LIST-MESSAGE-O                        
                 END-IF                                                         
               END-EVALUATE                                                     
               GO TO F80-USER-OUTPUT                                            
           END-IF.                                                              
                                                                                
       F60-USER-DETAIL.                                                         
                                                                                
           PERFORM Q40-INITKEY-IESCNTL.                                         
           STRING LIST-RSC-IDNT-I(1)      DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT.                       
           PERFORM Q41-STARTBR-IESCNTL.                                         
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
               PERFORM Q43-READNEXT-IESCNTL THRU Q43-IS-SELECTED                
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                 MOVE LOW-VALUES       TO LIST-SELECT-O(IDX)                    
                 MOVE FLDASBRF         TO LIST-RSC-IDNT-A(IDX)                  
                 MOVE SPACES           TO WINVSEC-RESID                         
                 STRING IUI-US-USRIDNT    DELIMITED BY LOW-VALUE                
                                     INTO WINVSEC-RESID                         
                 MOVE WINVSEC-RESID    TO LIST-RSC-IDNT-O(IDX)                  
                 MOVE IUI-US-USRNAME   TO LIST-RSC-DESC-O(IDX)                  
                 MOVE FLDASKIP         TO LIST-UNV-ACCS-A(IDX)                  
                 MOVE EXCBLUE          TO LIST-UNV-ACCS-C(IDX)                  
                 SET  BIT-DECODE-FROM-BYTE TO TRUE                              
                 MOVE IUI-US-USRINFO   TO BIT-BYTE                              
                 CALL 'BITMAN'      USING BITMAN-PARMS                          
                 EVALUATE TRUE                                                  
                 WHEN BIT-0-IS-ON                                               
                  AND BIT-1-IS-ON                                               
                     MOVE '4'          TO LIST-UNV-ACCS-O(IDX)                  
                 WHEN BIT-0-IS-OFF                                              
                  AND BIT-1-IS-ON                                               
                     MOVE '3'          TO LIST-UNV-ACCS-O(IDX)                  
                 WHEN OTHER                                                     
                     MOVE '2'          TO LIST-UNV-ACCS-O(IDX)                  
                 END-EVALUATE                                                   
               ELSE                                                             
                 MOVE  -1              TO LIST-SELECT-L(IDX)                    
                 SUBTRACT 1          FROM IDX                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > LISTROWS                                             
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
             MOVE FLDASDRK             TO LIST-SELECT-A(IDX)                    
                                          LIST-RSC-IDNT-A(IDX)                  
                                          LIST-RSC-DESC-A(IDX)                  
                                          LIST-UNV-ACCS-A(IDX)                  
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(ENDFILE)                                   
               GO TO F70-USER-ERRORS                                            
           END-IF.                                                              
                                                                                
           PERFORM Q45-ENDBR-IESCNTL.                                           
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               GO TO F80-USER-OUTPUT                                            
           END-IF.                                                              
                                                                                
       F70-USER-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO LIST-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       F80-USER-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO LIST-COMPANY-O.                       
           MOVE VSEC-MAP-TITLE         TO LIST-TITLE-O.                         
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(LIST-SYSDATE-O)                                   
                     DATESEP('/')                                               
                     TIME(WS-WRKTIME)                                           
                     TIMESEP(':')                                               
           END-EXEC.                                                            
           IF  WS-HRSTIME > 12                                                  
               MOVE ' PM'              TO WS-XXXTIME                            
               SUBTRACT 12           FROM WS-HRSTIME                            
           ELSE                                                                 
               IF  WS-HRSTIME = 12                                              
                   MOVE ' PM'          TO WS-XXXTIME                            
               ELSE                                                             
                   MOVE ' AM'          TO WS-XXXTIME                            
                   IF  WS-HRSTIME = 00                                          
                       MOVE 12         TO WS-HRSTIME                            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.                       
                                                                                
           MOVE EIBTRMID               TO LIST-TERMID-O.                        
                                                                                
       F85-USER-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(LISTMAP)                                       
                             DATAONLY                                           
                             FROM(LISTMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(LISTMAP)                                           
                         FROM(LISTMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       F90-USER-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(LISTMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    DETAIL ROUTINE                                              *        
      ******************************************************************        
       G00-DETL-ROUTINE.                                                        
                                                                                
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
           EXEC CICS GETMAIN                                                    
                     SET(ADDRESS OF DETLMAPI)                                   
                     LENGTH(LENGTH OF DETLMAPI)                                 
                     INITIMG(LOVALUE)                                           
           END-EXEC.                                                            
                                                                                
           IF  COMM-CURRRTN NOT = 'DETL'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
               END-IF                                                           
               PERFORM G80-DETL-OUTPUT THRU G85-DETL-SENDMAP                    
               GO TO G50-DETL-DISPLAY                                           
           END-IF.                                                              
                                                                                
       G10-DETL-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(DETLMAP)                                               
                     INTO(DETLMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO G70-DETL-ERRORS                                            
           END-IF.                                                              
                                                                                
       G20-DETL-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK04                                                        
               PERFORM Q30-TS-DELETEQ                                           
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               MOVE 'RETN'             TO COMM-CURRRTN                          
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F00-USER-ROUTINE                    
               WHEN VSEC-TRAN-SECTION GO TO E00-TRAN-ROUTINE                    
               WHEN VSEC-UGRP-SECTION GO TO D00-UGRP-ROUTINE                    
               WHEN OTHER             GO TO C00-LIST-ROUTINE                    
               END-EVALUATE                                                     
                                                                                
           WHEN AIDPFK01                                                        
               MOVE DETLMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK03                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
           WHEN AIDPFK10                                                        
           WHEN AIDPFK11                                                        
           WHEN AIDPFK12                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO DETL-MESSAGE-O                        
               GO TO G80-DETL-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO DETL-MESSAGE-O                        
               GO TO G80-DETL-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       G30-DETL-PROCESS.                                                        
                                                                                
      * select which popup list to display for adding permissions               
           IF  EIBAID = AIDPFK10 OR AIDPFK11 OR AIDPFK12                        
               MOVE LOW-VALUES         TO DETL-SKIP-TO-O                        
                                          DETL-SKIP-SAVE-O                      
               EVALUATE EIBAID ALSO TRUE                                        
      * ...add groups to selected resource                                      
               WHEN AIDPFK10   ALSO ANY                                         
                   MOVE 'GRUP'         TO VSEC-ADDP-CODE                        
      * ...add resources to selected resource                                   
               WHEN AIDPFK11   ALSO VSEC-SEL-GROUP                              
               WHEN AIDPFK11   ALSO VSEC-SEL-SUBGROUP                           
               WHEN AIDPFK11   ALSO VSEC-SEL-USER                               
                   MOVE 'APPS'         TO VSEC-ADDP-CODE                        
      * ...add users to selected resource                                       
               WHEN AIDPFK12   ALSO NOT VSEC-SEL-USER                           
                   MOVE 'USER'         TO VSEC-ADDP-CODE                        
      * ...invalid combination selected                                         
               WHEN OTHER                                                       
                 MOVE     -2           TO ERROR-SWITCH                          
                 MOVE  EXHREVRS        TO DETL-MESSAGE-H                        
                 MOVE 'Cannot add that type to selected resource.'              
                                       TO DETL-MESSAGE-O                        
                 GO TO G80-DETL-OUTPUT                                          
               END-EVALUATE                                                     
               GO TO H00-ADDP-ROUTINE                                           
           END-IF.                                                              
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * previous screen                                                         
           IF  EIBAID = AIDPFK03                                                
               MOVE LOW-VALUES         TO DETL-SKIP-TO-O                        
                                          DETL-SKIP-SAVE-O                      
               IF  VSEC-PER-RSRC(1) > SPACES                                    
                 MOVE VSEC-PER-TYPE(1) TO VSEC-SEL-TYPE                         
                 MOVE VSEC-PER-RSRC(1) TO VSEC-SEL-RSRC                         
                 MOVE VSEC-PER-RDSC(1) TO VSEC-SEL-RDSC                         
                 COMPUTE CNT            = LENGTH OF VSEC-PER-TABLE              
                                        / LENGTH OF VSEC-PER-ENTRY              
                 PERFORM WITH TEST BEFORE                                       
                   VARYING IDX FROM 2 BY 1 UNTIL IDX > CNT                      
                     MOVE VSEC-PER-ENTRY(IDX)                                   
                                       TO VSEC-PER-ENTRY(IDX - 1)               
                 END-PERFORM                                                    
                 MOVE SPACES           TO VSEC-PER-ENTRY(CNT)                   
                 MOVE ZEROES           TO TS-TOTL                               
                 GO TO G50-DETL-DISPLAY                                         
               END-IF                                                           
               PERFORM Q30-TS-DELETEQ                                           
               PERFORM Q90-BSM-REFRESH THRU Q95-EXIT                            
               MOVE 'RETN'             TO COMM-CURRRTN                          
               EVALUATE TRUE                                                    
               WHEN VSEC-USER-SECTION GO TO F00-USER-ROUTINE                    
               WHEN VSEC-TRAN-SECTION GO TO E00-TRAN-ROUTINE                    
               WHEN VSEC-UGRP-SECTION GO TO D00-UGRP-ROUTINE                    
               WHEN OTHER             GO TO C00-LIST-ROUTINE                    
               END-EVALUATE                                                     
           END-IF.                                                              
                                                                                
      * scroll to first/last page                                               
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               MOVE LOW-VALUES         TO DETL-SKIP-TO-O                        
                                          DETL-SKIP-SAVE-O                      
               IF  EIBAID = AIDPFK06                                            
                 MOVE 1                TO TS-ITEM                               
               ELSE                                                             
                 MOVE TS-TOTL          TO TS-ITEM                               
               END-IF                                                           
               GO TO G50-DETL-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT DETLHEAD         FROM ROW.                                  
                                                                                
      * scroll backward/forward by page                                         
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               MOVE LOW-VALUES         TO DETL-SKIP-TO-O                        
                                          DETL-SKIP-SAVE-O                      
               IF  EIBAID = AIDPFK07                                            
                 IF  (ROW < 1 OR >= DETLROWS)                                   
                   SUBTRACT DETLROWS FROM TS-ITEM                               
                 ELSE                                                           
                   COMPUTE TS-ITEM = TS-ITEM - (DETLROWS - ROW)                 
                 END-IF                                                         
                 IF  TS-ITEM < 1                                                
                   MOVE 1              TO TS-ITEM                               
                   MOVE 'This is the first Entry in the Table.'                 
                                       TO DETL-MESSAGE-O                        
                 END-IF                                                         
               ELSE                                                             
                 IF  (ROW <= 1 OR > DETLROWS)                                   
                   ADD  DETLROWS       TO TS-ITEM                               
                 ELSE                                                           
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)                        
                 END-IF                                                         
                 IF  TS-ITEM > TS-TOTL                                          
                   MOVE TS-TOTL    TO TS-ITEM                                   
                   MOVE 'This is the last Entry in the Table.'                  
                                       TO DETL-MESSAGE-O                        
                 END-IF                                                         
               END-IF                                                           
               GO TO G50-DETL-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * skip to selected key value                                              
           INSPECT DETL-SKIP-TO-I CONVERTING                                    
                            LOWER-CASE TO UPPER-CASE.                           
           IF  DETL-SKIP-TO-I > SPACES                                          
           AND DETL-SKIP-TO-I NOT = DETL-SKIP-SAVE-I                            
               MOVE DETL-SKIP-TO-I     TO DETL-SKIP-SAVE-O                      
               ADD  1   TO TS-ITEM GIVING SS-ITEM                               
               PERFORM WITH TEST AFTER                                          
                 VARYING SS-ITEM FROM SS-ITEM BY 1                              
                   UNTIL SS-ITEM >= TS-TOTL                                     
                      OR DETL-SKIP-TO-I <= TS-RESOURCE                          
                      OR EIBRESP NOT = DFHRESP(NORMAL)                          
                 PERFORM Q32-TS-READQ                                           
                 IF  EIBRESP = DFHRESP(NORMAL)                                  
      *            IF  TS-RSC-TRANS                                             
      *              INSPECT TS-RESOURCE CONVERTING                             
      *                     LOWER-CASE TO UPPER-CASE                            
      *            END-IF                                                       
                   IF  DETL-SKIP-TO-I <= TS-RESOURCE                            
                   AND SS-ITEM = (TS-ITEM + 1)                                  
                     MOVE SPACES       TO TS-RESOURCE                           
                   END-IF                                                       
                 END-IF                                                         
               END-PERFORM                                                      
               IF  DETL-SKIP-TO-I < TS-RESOURCE                                 
                 IF  SS-ITEM = 1                                                
                   MOVE SS-ITEM        TO TS-ITEM                               
                   MOVE 'This is the first entry.' TO DETL-MESSAGE-O            
                 ELSE                                                           
                   SUBTRACT 1 FROM SS-ITEM GIVING TS-ITEM                       
                 END-IF                                                         
               ELSE                                                             
                 MOVE SS-ITEM          TO TS-ITEM                               
               END-IF                                                           
               GO TO G50-DETL-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * set default line seletion                                               
           IF  ROW >= 1 AND <= DETLROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > DETLROWS                                         
                      OR DETL-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > DETLROWS                                               
                 MOVE 'P'              TO DETL-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           SET NO-PERMITS-ADDED-OR-DELETED TO TRUE.                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM DETLROWS BY -1 UNTIL IDX < 1                      
               INSPECT DETL-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               IF  DETL-SELECT-I(IDX) = 'A' OR 'D' OR 'P'                       
                 PERFORM G40-DETL-UPDATES THRU G45-EXIT                         
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
               GO TO G80-DETL-OUTPUT                                            
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO G70-DETL-ERRORS                                            
           END-IF.                                                              
                                                                                
           IF  PERMITS-WERE-ADDED-OR-DELETED                                    
               MOVE ZEROES             TO TS-TOTL                               
               MOVE LOW-VALUES         TO DETL-SKIP-SAVE-O                      
           END-IF.                                                              
                                                                                
           GO TO G50-DETL-DISPLAY.                                              
                                                                                
       G40-DETL-UPDATES.                                                        
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           COMPUTE SS-ITEM = TS-ITEM + IDX - 1.                                 
           PERFORM Q32-TS-READQ.                                                
                                                                                
           EVALUATE DETL-SELECT-I(IDX)                                          
                                                                                
           WHEN 'A'                                                             
      * add/alter processing                                                    
             IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
               MOVE DETL-RSC-IDNT-I(IDX) TO TS-RESOURCE                         
               MOVE DETL-RSC-DESC-I(IDX) TO TS-RSC-DESC                         
               MOVE DETL-RSC-ACCS-I(IDX) TO TS-ACCESS                           
               PERFORM R20-ADD-PERMITS THRU R25-EXIT                            
               IF  NO-ERRORS-FOUND                                              
               AND EIBRESP = DFHRESP(NORMAL)                                    
                 SET PERMITS-WERE-ADDED-OR-DELETED TO TRUE                      
               END-IF                                                           
             ELSE                                                               
               IF  TS-RSC-DESC <= SPACES                                        
               OR (DETL-RSC-DESC-I(IDX) > LOW-VALUES                            
               AND DETL-RSC-DESC-I(IDX) NOT = TS-RSC-DESC)                      
                 MOVE DETL-RSC-DESC-I(IDX) TO TS-RSC-DESC                       
               END-IF                                                           
               IF  TS-ACCESS <= SPACES                                          
               OR (DETL-RSC-ACCS-I(IDX) > LOW-VALUES                            
               AND DETL-RSC-ACCS-I(IDX) NOT = TS-ACCESS)                        
                 MOVE DETL-RSC-ACCS-I(IDX) TO TS-ACCESS                         
               END-IF                                                           
               PERFORM R30-ALTER-PERMITS THRU R35-EXIT                          
             END-IF                                                             
             IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
               MOVE -2                 TO ERROR-SWITCH                          
               GO TO G70-DETL-ERRORS                                            
             END-IF                                                             
                                                                                
           WHEN 'D'                                                             
      * delete processing                                                       
             IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
               MOVE -1                 TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'Cannot delete an undefined entry.'                         
                                       TO DETL-MESSAGE-O                        
             ELSE                                                               
               PERFORM R40-DELETE-PERMITS THRU R45-EXIT                         
               IF  NO-ERRORS-FOUND                                              
               AND EIBRESP = DFHRESP(NORMAL)                                    
                 SET PERMITS-WERE-ADDED-OR-DELETED TO TRUE                      
               ELSE                                                             
                 MOVE -2               TO ERROR-SWITCH                          
                 GO TO G70-DETL-ERRORS                                          
               END-IF                                                           
             END-IF                                                             
                                                                                
           WHEN 'P'                                                             
      * drill-down processing                                                   
             IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
               MOVE -1                 TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'Cannot drill down on undefined entry.'                     
                                       TO DETL-MESSAGE-O                        
             END-IF                                                             
                                                                                
           END-EVALUATE.                                                        
                                                                                
       G45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       G50-DETL-DISPLAY.                                                        
                                                                                
           MOVE 'DETL'                 TO COMM-CURRRTN.                         
                                                                                
      * skip if no detail lines                                                 
           IF  TS-TOTL NOT > ZERO                                               
               GO TO G60-DETL-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > DETLROWS                                             
                  OR DETL-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= DETLROWS                                                  
           AND NO-ERRORS-FOUND                                                  
               EVALUATE DETL-SELECT-I(ROW)                                      
               WHEN 'P'                                                         
                   COMPUTE SS-ITEM = TS-ITEM + ROW - 1                          
                   PERFORM Q32-TS-READQ                                         
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                       GO TO G70-DETL-ERRORS                                    
                   END-IF                                                       
                   COMPUTE CNT          = LENGTH OF VSEC-PER-TABLE              
                                        / LENGTH OF VSEC-PER-ENTRY              
                   COMPUTE IDX          = CNT - 1                               
                   PERFORM WITH TEST BEFORE                                     
                     VARYING IDX FROM IDX BY -1 UNTIL IDX < 1                   
                       MOVE VSEC-PER-ENTRY(IDX)                                 
                                       TO VSEC-PER-ENTRY(IDX + 1)               
                   END-PERFORM                                                  
                   MOVE VSEC-SEL-TYPE  TO VSEC-PER-TYPE(1)                      
                   MOVE VSEC-SEL-RSRC  TO VSEC-PER-RSRC(1)                      
                   MOVE VSEC-SEL-RDSC  TO VSEC-PER-RDSC(1)                      
                   MOVE TS-RSC-TYPE    TO VSEC-SEL-TYPE                         
                   MOVE TS-RESOURCE    TO VSEC-SEL-RSRC                         
                   MOVE TS-RSC-DESC    TO VSEC-SEL-RDSC                         
                   MOVE ZEROES         TO TS-TOTL                               
                   GO TO G60-DETL-DETAIL                                        
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          DETL-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO DETL-MESSAGE-H                        
                 MOVE 'Unknown option selection.' TO DETL-MESSAGE-O             
               END-EVALUATE                                                     
               GO TO G80-DETL-OUTPUT                                            
           END-IF.                                                              
                                                                                
       G60-DETL-DETAIL.                                                         
                                                                                
           MOVE VSEC-SEL-RSRC          TO DETL-RSRC-ID-O.                       
           IF  NOT VSEC-SEL-FACILITY                                            
               MOVE VSEC-SEL-RDSC      TO DETL-RSRC-ID-O(20:)                   
           END-IF.                                                              
                                                                                
           IF  TS-TOTL NOT > ZERO                                               
               PERFORM P30-TS-DETL-BUILD THRU P35-EXIT                          
               IF  ERRORS-FOUND                                                 
               OR  EIBRESP NOT = DFHRESP(NORMAL)                                
                   MOVE -1             TO ERROR-SWITCH                          
                                          DETL-SELECT-L(1)                      
                   PERFORM X00-UNEX-ERR                                         
                   MOVE EXHREVRS       TO DETL-MESSAGE-H                        
                   MOVE UNEX-MSG       TO DETL-MESSAGE-O                        
                   GO TO G80-DETL-OUTPUT                                        
               END-IF                                                           
               IF  TS-TOTL NOT > ZERO                                           
                   MOVE 1              TO IDX                                   
                   MOVE 'No permissions found.' TO DETL-MESSAGE-O               
                   GO TO G65-DETL-BLANKS                                        
               END-IF                                                           
               MOVE TS-TOTL            TO UNEX-RESP                             
               STRING UNEX-RESP 'records found.'                                
                   DELIMITED BY SIZE INTO DETL-MESSAGE-O                        
           END-IF.                                                              
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           MOVE 1                      TO IDX.                                  
           MOVE DFHRESP(NORMAL)        TO EIBRESP.                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SS-ITEM FROM TS-ITEM BY 1                                  
               UNTIL SS-ITEM > TS-TOTL                                          
                  OR IDX > DETLROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
             PERFORM Q32-TS-READQ                                               
             IF  EIBRESP = DFHRESP(NORMAL)                                      
               MOVE LOW-VALUES         TO DETL-SELECT-O(IDX)                    
               MOVE FLDASBRT           TO DETL-RSC-IDNT-A(IDX)                  
               MOVE TS-RESOURCE        TO DETL-RSC-IDNT-O(IDX)                  
               MOVE TS-RSC-DESC        TO DETL-RSC-DESC-O(IDX)                  
               IF (NOT VSEC-SEL-FACILITY AND NOT VSEC-SEL-TRANS)                
               AND (TS-RSC-GROUP OR TS-RSC-SUBGROUP)                            
                 MOVE FLDASDRK         TO DETL-RSC-ACCS-A(IDX)                  
               ELSE                                                             
                 IF (NOT VSEC-SEL-FACILITY AND NOT VSEC-SEL-TRANS)              
                 AND TS-RSC-USER                                                
                   MOVE FLDASKIP       TO DETL-RSC-ACCS-A(IDX)                  
                   MOVE EXCBLUE        TO DETL-RSC-ACCS-C(IDX)                  
                 ELSE                                                           
                   MOVE EXHULINE       TO DETL-RSC-ACCS-H(IDX)                  
                 END-IF                                                         
                 MOVE TS-ACCESS        TO DETL-RSC-ACCS-O(IDX)                  
               END-IF                                                           
               ADD  1                  TO IDX                                   
             ELSE                                                               
               MOVE  -1                TO DETL-SELECT-L(IDX - 1)                
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
       G65-DETL-BLANKS.                                                         
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > DETLROWS                                             
             MOVE LOW-VALUES           TO DETLMAP-ROW-DETAIL(IDX)               
             MOVE EXHULINE             TO DETL-RSC-IDNT-H(IDX)                  
                                          DETL-RSC-ACCS-H(IDX)                  
           END-PERFORM.                                                         
                                                                                
       G70-DETL-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO DETL-MESSAGE-O                        
           END-IF.                                                              
                                                                                
       G80-DETL-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO DETL-COMPANY-O.                       
                                                                                
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
                     FULLDATE(DETL-SYSDATE-O)                                   
                     DATESEP('/')                                               
                     TIME(WS-WRKTIME)                                           
                     TIMESEP(':')                                               
           END-EXEC.                                                            
           IF  WS-HRSTIME > 12                                                  
               MOVE ' PM'              TO WS-XXXTIME                            
               SUBTRACT 12           FROM WS-HRSTIME                            
           ELSE                                                                 
               IF  WS-HRSTIME = 12                                              
                   MOVE ' PM'          TO WS-XXXTIME                            
               ELSE                                                             
                   MOVE ' AM'          TO WS-XXXTIME                            
                   IF  WS-HRSTIME = 00                                          
                       MOVE 12         TO WS-HRSTIME                            
                   END-IF                                                       
               END-IF                                                           
           END-IF.                                                              
           MOVE WS-WRKTIME             TO DETL-SYSTIME-O.                       
                                                                                
           MOVE EIBTRMID               TO DETL-TERMID-O.                        
                                                                                
       G85-DETL-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(DETLMAP)                                       
                             DATAONLY                                           
                             FROM(DETLMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(DETLMAP)                                       
                             DATAONLY                                           
                             FROM(DETLMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(DETLMAP)                                           
                         FROM(DETLMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       G90-DETL-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(DETLMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    DETAIL ROUTINE                                              *        
      ******************************************************************        
       H00-ADDP-ROUTINE.                                                        
                                                                                
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
           EXEC CICS GETMAIN                                                    
                     SET(ADDRESS OF ADDPMAPI)                                   
                     LENGTH(LENGTH OF ADDPMAPI)                                 
                     INITIMG(LOVALUE)                                           
           END-EXEC.                                                            
           MOVE SPACES                 TO ADDP-TITLE-O                          
                                          ADDP-MESSAGE-O.                       
                                                                                
           IF  COMM-CURRRTN NOT = 'ADDP'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VSEC-SAVE-TIOA TO LISTMAPO                              
                   MOVE LOW-VALUES     TO VSEC-SAVE-TIOA                        
                   EXEC CICS SEND CONTROL                                       
                             TERMINAL                                           
                             ERASE                                              
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(DETLXXX)                                       
                             MAPSET(VSECMS)                                     
                             MAPONLY                                            
                             TERMINAL                                           
                             NOHANDLE                                           
                   END-EXEC                                                     
                   PERFORM L90-ERASE-SKIP-TO THRU L95-EXIT                      
               END-IF                                                           
               PERFORM H80-ADDP-OUTPUT THRU H85-ADDP-SENDMAP                    
               PERFORM Q30-TS-DELETEQ                                           
               GO TO H50-ADDP-DISPLAY                                           
           END-IF.                                                              
                                                                                
       H10-ADDP-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(ADDPMAP)                                               
                     INTO(ADDPMAPI)                                             
                     MAPSET(VSECMS)                                             
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO H70-ADDP-ERRORS                                            
           END-IF.                                                              
                                                                                
       H20-ADDP-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
           WHEN AIDPFK03                                                        
               PERFORM Q30-TS-DELETEQ                                           
               MOVE 'RETN'             TO COMM-CURRRTN                          
               GO TO G00-DETL-ROUTINE                                           
                                                                                
           WHEN AIDPFK01                                                        
               MOVE ADDPMAPI           TO VSEC-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDPFK04                                                        
           WHEN AIDPFK05                                                        
           WHEN AIDPFK10                                                        
           WHEN AIDPFK11                                                        
           WHEN AIDPFK12                                                        
               IF  VSEC-ADDP-CODE NOT = 'GRUP' AND 'USER'                       
                   EVALUATE EIBAID                                              
                   WHEN AIDPFK04 MOVE 'APPS' TO VSEC-ADDP-CODE                  
                   WHEN AIDPFK05 MOVE 'CORP' TO VSEC-ADDP-CODE                  
                   WHEN AIDPFK10 MOVE 'ROLE' TO VSEC-ADDP-CODE                  
                   WHEN AIDPFK11 MOVE 'TRAN' TO VSEC-ADDP-CODE                  
                   WHEN AIDPFK12 MOVE 'VSEC' TO VSEC-ADDP-CODE                  
                   END-EVALUATE                                                 
                   MOVE ZEROES         TO TS-TOTL                               
               END-IF                                                           
               GO TO H50-ADDP-DISPLAY                                           
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO ADDP-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
                                       TO ADDP-MESSAGE-O                        
               GO TO H80-ADDP-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO ADDP-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
                                       TO ADDP-MESSAGE-O                        
               GO TO H80-ADDP-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       H30-ADDP-PROCESS.                                                        
                                                                                
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * scroll to first/last page                                               
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
               PERFORM L90-ERASE-SKIP-TO THRU L95-EXIT                          
               IF  EIBAID = AIDPFK06                                            
                 MOVE 1                TO TS-ITEM                               
               ELSE                                                             
                 MOVE TS-TOTL          TO TS-ITEM                               
               END-IF                                                           
               GO TO H50-ADDP-DISPLAY                                           
           END-IF.                                                              
                                                                                
           SUBTRACT ADDPHEAD         FROM ROW.                                  
                                                                                
      * scroll backward/forward by page                                         
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
               PERFORM L90-ERASE-SKIP-TO THRU L95-EXIT                          
               IF  EIBAID = AIDPFK07                                            
                 IF  (ROW < 1 OR >= ADDPROWS)                                   
                   SUBTRACT ADDPROWS FROM TS-ITEM                               
                 ELSE                                                           
                   COMPUTE TS-ITEM = TS-ITEM - (ADDPROWS - ROW)                 
                 END-IF                                                         
                 IF  TS-ITEM < 1                                                
                   MOVE 1              TO TS-ITEM                               
                   MOVE 'This is the first Entry in the Table.'                 
                                       TO ADDP-MESSAGE-O(4:66)                  
                 END-IF                                                         
               ELSE                                                             
                 IF  (ROW <= 1 OR > ADDPROWS)                                   
                   ADD  ADDPROWS       TO TS-ITEM                               
                 ELSE                                                           
                   COMPUTE TS-ITEM = TS-ITEM + (ROW - 1)                        
                 END-IF                                                         
                 IF  TS-ITEM > TS-TOTL                                          
                   MOVE TS-TOTL    TO TS-ITEM                                   
                   MOVE 'This is the last Entry in the Table.'                  
                                       TO ADDP-MESSAGE-O(4:66)                  
                 END-IF                                                         
               END-IF                                                           
               GO TO H50-ADDP-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * skip to selected key value                                              
           IF  ADDP-SKIP-TO-I > SPACES                                          
               IF  VSEC-ADDP-CODE NOT = 'TRAN'                                  
                 INSPECT ADDP-SKIP-TO-I CONVERTING                              
                            LOWER-CASE TO UPPER-CASE                            
               END-IF                                                           
               PERFORM WITH TEST AFTER                                          
                 VARYING SS-ITEM FROM 1 BY 1                                    
                   UNTIL SS-ITEM >= TS-TOTL                                     
                      OR ADDP-SKIP-TO-I <= TS-RESOURCE                          
                      OR EIBRESP NOT = DFHRESP(NORMAL)                          
                 PERFORM Q32-TS-READQ                                           
                 IF  EIBRESP = DFHRESP(NORMAL)                                  
                 AND TS-RSC-USER                                                
                   INSPECT TS-RESOURCE CONVERTING                               
                            LOWER-CASE TO UPPER-CASE                            
                 END-IF                                                         
               END-PERFORM                                                      
               IF  ADDP-SKIP-TO-I < TS-RESOURCE                                 
                 IF  SS-ITEM = 1                                                
                   MOVE SS-ITEM        TO TS-ITEM                               
                   MOVE 'This is the first entry.'                              
                                       TO ADDP-MESSAGE-O(4:66)                  
                 ELSE                                                           
                   SUBTRACT 1 FROM SS-ITEM GIVING TS-ITEM                       
                 END-IF                                                         
               ELSE                                                             
                 MOVE SS-ITEM          TO TS-ITEM                               
               END-IF                                                           
               GO TO H50-ADDP-DISPLAY                                           
           ELSE                                                                 
               PERFORM L90-ERASE-SKIP-TO THRU L95-EXIT                          
           END-IF.                                                              
                                                                                
      * set default line seletion                                               
           IF  ROW >= 1 AND <= ADDPROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > ADDPROWS                                         
                      OR ADDP-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > ADDPROWS                                               
                 MOVE 'S'              TO ADDP-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           SET NO-PERMITS-ADDED-OR-DELETED TO TRUE.                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM ADDPROWS BY -1 UNTIL IDX < 1                      
               INSPECT ADDP-SELECT-I(IDX)                                       
                   CONVERTING LOWER-CASE TO UPPER-CASE                          
               IF  ADDP-SELECT-I(IDX) > SPACE                                   
                 PERFORM H40-ADDP-UPDATES                                       
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  ERRORS-FOUND                                                     
           OR  NO-PERMITS-ADDED-OR-DELETED                                      
             IF  ERROR-AT-CURSOR                                                
               GO TO H80-ADDP-OUTPUT                                            
             ELSE                                                               
               GO TO H50-ADDP-DISPLAY                                           
             END-IF                                                             
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO H70-ADDP-ERRORS                                            
           END-IF.                                                              
                                                                                
           PERFORM Q30-TS-DELETEQ.                                              
           GO TO G00-DETL-ROUTINE.                                              
                                                                                
       H40-ADDP-UPDATES.                                                        
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
           COMPUTE SS-ITEM = TS-ITEM + IDX - 1.                                 
           PERFORM Q32-TS-READQ.                                                
                                                                                
           EVALUATE ADDP-SELECT-I(IDX)                                          
                                                                                
           WHEN 'S'                                                             
      * selection processing                                                    
             IF  EIBRESP = DFHRESP(NORMAL)                                      
               IF  TS-RSC-USER                                                  
                 MOVE TS-RSC-DESC      TO TS-RESOURCE                           
                 MOVE LOW-VALUES       TO TS-RSC-DESC                           
               END-IF                                                           
               IF  TS-ACCESS <= SPACES                                          
               OR (ADDP-RSC-ACCS-I(IDX) > LOW-VALUES                            
               AND ADDP-RSC-ACCS-I(IDX) NOT = TS-ACCESS)                        
                 MOVE ADDP-RSC-ACCS-I(IDX) TO TS-ACCESS                         
               END-IF                                                           
               PERFORM R20-ADD-PERMITS THRU R25-EXIT                            
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                 SET PERMITS-WERE-ADDED-OR-DELETED TO TRUE                      
               END-IF                                                           
             END-IF                                                             
             IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
               MOVE -2                 TO ERROR-SWITCH                          
               GO TO H70-ADDP-ERRORS                                            
             END-IF                                                             
                                                                                
           WHEN OTHER                                                           
      * selection error                                                         
             MOVE -1                   TO ERROR-SWITCH                          
                                          ADDP-SELECT-L(IDX)                    
             MOVE EXCRED               TO ADDP-MESSAGE-C                        
             MOVE 'Invalid selection option specified.'                         
                                       TO ADDP-MESSAGE-O(4:66)                  
                                                                                
           END-EVALUATE.                                                        
                                                                                
       H50-ADDP-DISPLAY.                                                        
                                                                                
           MOVE 'ADDP'                 TO COMM-CURRRTN.                         
                                                                                
      * skip if no detail lines                                                 
           IF  TS-TOTL NOT > ZERO                                               
               GO TO H60-ADDP-DETAIL                                            
           END-IF.                                                              
                                                                                
      * CHECK FOR OPTION SELECTIONS                                             
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING ROW FROM 1 BY 1                                            
               UNTIL ROW > ADDPROWS                                             
                  OR ADDP-SELECT-I(ROW) > SPACE AND NOT = '*'                   
           END-PERFORM.                                                         
                                                                                
           IF  ROW <= ADDPROWS                                                  
               MOVE     -1             TO ERROR-SWITCH                          
                                          ADDP-SELECT-L(ROW)                    
               MOVE EXCRED             TO ADDP-MESSAGE-C                        
               MOVE 'Use "S" to select one or more items to add.'               
                                       TO ADDP-MESSAGE-O(4:66)                  
               GO TO H80-ADDP-OUTPUT                                            
           END-IF.                                                              
                                                                                
       H60-ADDP-DETAIL.                                                         
                                                                                
           IF  TS-TOTL NOT > ZERO                                               
               PERFORM L00-TS-ADDP-BUILD THRU L05-EXIT                          
               IF  ERRORS-FOUND                                                 
               OR  EIBRESP NOT = DFHRESP(NORMAL)                                
                   MOVE -2             TO ERROR-SWITCH                          
                   PERFORM X00-UNEX-ERR                                         
                   MOVE EXCRED         TO ADDP-MESSAGE-C                        
                   MOVE UNEX-MSG       TO ADDP-MESSAGE-O(4:66)                  
                   GO TO H80-ADDP-OUTPUT                                        
               END-IF                                                           
               IF  TS-TOTL NOT > ZERO                                           
                   MOVE 1              TO IDX                                   
                   MOVE 'No resources found.'                                   
                                       TO ADDP-MESSAGE-O(4:66)                  
                   GO TO H65-ADDP-BLANKS                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           MOVE 1                      TO IDX.                                  
           MOVE DFHRESP(NORMAL)        TO EIBRESP.                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SS-ITEM FROM TS-ITEM BY 1                                  
               UNTIL SS-ITEM > TS-TOTL                                          
                  OR IDX > ADDPROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
             PERFORM Q32-TS-READQ                                               
             IF  EIBRESP = DFHRESP(NORMAL)                                      
               MOVE SPACES             TO ADDP-SELECT-O(IDX)                    
               MOVE TS-RESOURCE        TO ADDP-RSC-IDNT-O(IDX)                  
               MOVE TS-RSC-DESC        TO ADDP-RSC-DESC-O(IDX)                  
               IF (NOT VSEC-SEL-FACILITY AND NOT VSEC-SEL-TRANS)                
               AND (TS-RSC-GROUP OR TS-RSC-SUBGROUP)                            
                 MOVE FLDASDRK         TO ADDP-RSC-ACCS-A(IDX)                  
                 MOVE SPACES           TO ADDP-RSC-ACCS-O(IDX)                  
               ELSE                                                             
                 IF (NOT VSEC-SEL-FACILITY AND NOT VSEC-SEL-TRANS)              
                 AND TS-RSC-USER                                                
                   MOVE FLDASKIP       TO ADDP-RSC-ACCS-A(IDX)                  
                   MOVE EXCBLUE        TO ADDP-RSC-ACCS-C(IDX)                  
                   MOVE TS-ACCESS      TO ADDP-RSC-ACCS-O(IDX)                  
                 ELSE                                                           
                   MOVE EXHULINE       TO ADDP-RSC-ACCS-H(IDX)                  
                   MOVE SPACES         TO ADDP-RSC-ACCS-O(IDX)                  
                 END-IF                                                         
               END-IF                                                           
               ADD  1                  TO IDX                                   
             ELSE                                                               
               MOVE  -1                TO ADDP-SELECT-L(IDX - 1)                
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
       H65-ADDP-BLANKS.                                                         
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM IDX BY 1                                          
               UNTIL IDX > ADDPROWS                                             
             MOVE LOW-VALUES           TO ADDPMAP-ROW-DETAIL(IDX)               
             MOVE FLDASDRK             TO ADDP-SELECT-A(IDX)                    
                                          ADDP-RSC-IDNT-A(IDX)                  
                                          ADDP-RSC-DESC-A(IDX)                  
                                          ADDP-RSC-ACCS-A(IDX)                  
             MOVE SPACES               TO ADDP-SELECT-O(IDX)                    
                                          ADDP-RSC-IDNT-O(IDX)                  
                                          ADDP-RSC-DESC-O(IDX)                  
                                          ADDP-RSC-ACCS-O(IDX)                  
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               GO TO H80-ADDP-OUTPUT                                            
           END-IF.                                                              
                                                                                
       H70-ADDP-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               PERFORM X00-UNEX-ERR                                             
               MOVE EXCRED             TO ADDP-MESSAGE-C                        
               MOVE UNEX-MSG           TO ADDP-MESSAGE-O(4:66)                  
           END-IF.                                                              
                                                                                
       H80-ADDP-OUTPUT.                                                         
                                                                                
           MOVE SPACES                 TO ADDP-TITLE-O.                         
           EVALUATE VSEC-ADDP-CODE                                              
           WHEN 'GRUP'                                                          
               MOVE ' GROUP SELECTION LIST '                                    
                                       TO ADDP-TITLE-O(26:22)                   
           WHEN 'USER'                                                          
               MOVE ' USER SELECTION LIST '                                     
                                       TO ADDP-TITLE-O(26:21)                   
           WHEN OTHER                                                           
               MOVE ' RESOURCE SELECTION LIST '                                 
                                       TO ADDP-TITLE-O(24:25)                   
           END-EVALUATE.                                                        
                                                                                
           IF  ADDP-MESSAGE-I NOT > SPACES                                      
             IF  VSEC-ADDP-CODE = 'GRUP' OR 'USER'                              
               MOVE 'S=Select ENTER=Add F3=Cancel'                              
                                       TO ADDP-MESSAGE-O(4:66)                  
             ELSE                                                               
               MOVE 'S=Select ENTER=Add F3=Cancel F4=APPS F5=CORP F10=RO        
      -             'LE F11=TRAN'      TO ADDP-MESSAGE-O(4:66)                  
             END-IF                                                             
           END-IF.                                                              
                                                                                
       H85-ADDP-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP(ADDPMAP)                                       
                             DATAONLY                                           
                             FROM(ADDPMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP(ADDPMAP)                                       
                             DATAONLY                                           
                             FROM(ADDPMAPO)                                     
                             MAPSET(VSECMS)                                     
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP(ADDPMAP)                                           
                         FROM(ADDPMAPO)                                         
                         MAPSET(VSECMS)                                         
                         TERMINAL                                               
                         FREEKB                                                 
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       H90-ADDP-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(ADDPMAPI)                                             
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(IESCNTL-RECORD)                                       
           END-EXEC.                                                            
           EXEC CICS FREEMAIN                                                   
                     DATA(BSTCNTL-RECORD)                                       
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
                     TRANSID(EIBTRNID)                                          
                     COMMAREA(DFHCOMMAREA)                                      
                     LENGTH(EIBCALEN)                                           
           END-EXEC.                                                            
                                                                                
      /*****************************************************************        
      *    SPECIALIZED PERFORMED ROUTINES                              *        
      ******************************************************************        
                                                                                
       L00-TS-ADDP-BUILD.                                                       
           MOVE 'Generating...'        TO POPUP-TEXT.                           
           PERFORM L10-DISPLAY-POPUP THRU L15-EXIT.                             
                                                                                
           PERFORM Q30-TS-DELETEQ.                                              
                                                                                
           EVALUATE VSEC-ADDP-CODE                                              
           WHEN 'APPS'                                                          
           WHEN 'CORP'                                                          
           WHEN 'ROLE'                                                          
           WHEN 'VSEC'                                                          
               STRING 'WIN' VSEC-ADDP-CODE '.'                                  
                   DELIMITED BY SIZE INTO ADDP-PREFIX                           
               PERFORM L20-CHASE-RESRCS   THRU L25-EXIT                         
           WHEN 'GRUP'                                                          
               PERFORM L30-CHASE-GROUPS   THRU L35-EXIT                         
           WHEN 'TRAN'                                                          
               PERFORM L40-CHASE-TRANS    THRU L45-EXIT                         
           WHEN 'USER'                                                          
               PERFORM L50-CHASE-USERS    THRU L55-EXIT                         
           END-EVALUATE                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q31-TS-NUMITEMS                                          
           END-IF.                                                              
       L05-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L10-DISPLAY-POPUP.                                                       
           EXEC CICS SEND                                                       
                     MAP(ADDPXXX)                                               
                     MAPSET(VSECMS)                                             
                     MAPONLY                                                    
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
           PERFORM Q02-DISPLAY-WINDOW THRU Q05-EXIT.                            
       L15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L20-CHASE-RESRCS.                                                        
           MOVE LOW-VALUES             TO TWO-BYTES                             
                                          TS-RECD.                              
           MOVE 'F'                    TO TS-RSC-TYPE.                          
           MOVE VSEC-ADDP-CODE         TO TS-FAC-CODE.                          
                                                                                
           SET  BSM-FACILITY           TO TRUE.                                 
           MOVE ADDP-PREFIX            TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-FACILITY                                             
                OR BSM-RESOURCE(1:8) NOT = ADDP-PREFIX                          
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-FACILITY                                                   
             AND BSM-RESOURCE(1:8) = ADDP-PREFIX                                
               MOVE BSM-KEYVALEN       TO LO-LSB                                
               MOVE HALF-WORD          TO LEN                                   
               MOVE BSM-RESOURCE(1:LEN)                                         
                                       TO TS-RESOURCE                           
               IF  BSM-FAC-GENERIC                                              
                   MOVE '*'            TO TS-RESOURCE(LEN + 1:1)                
               END-IF                                                           
               MOVE BSM-RSRCDESC       TO TS-RSC-DESC                           
               PERFORM Q33-TS-WRITEQ                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       L25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L30-CHASE-GROUPS.                                                        
           MOVE LOW-VALUES             TO TS-RECD.                              
           MOVE 'G'                    TO TS-RSC-TYPE.                          
                                                                                
           SET  BSM-GROUP              TO TRUE.                                 
           MOVE SPACES                 TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-GROUP                                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-GROUP                                                      
             AND BSM-RESOURCE(17:8) = GROUP-HEADER                              
               MOVE BSM-RESOURCE(1:16) TO TS-RESOURCE                           
               MOVE BSM-RSRCDESC       TO TS-RSC-DESC                           
               PERFORM Q33-TS-WRITEQ                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       L35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L40-CHASE-TRANS.                                                         
           MOVE LOW-VALUES             TO TWO-BYTES                             
                                          TS-RECD.                              
           MOVE 'T'                    TO TS-RSC-TYPE.                          
                                                                                
           SET  BSM-TRANS              TO TRUE.                                 
           MOVE SPACES                 TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-TRANS                                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-TRANS                                                      
               MOVE BSM-KEYVALEN       TO LO-LSB                                
               MOVE HALF-WORD          TO LEN                                   
               MOVE BSM-RESOURCE(1:LEN)                                         
                                       TO TS-RESOURCE                           
               IF  BSM-FAC-GENERIC                                              
                   MOVE '*'            TO TS-RESOURCE(LEN + 1:1)                
               END-IF                                                           
               MOVE BSM-RSRCDESC       TO TS-RSC-DESC                           
               PERFORM Q33-TS-WRITEQ                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q25-ENDBR-BSTCNTL                                        
           END-IF.                                                              
       L45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L50-CHASE-USERS.                                                         
           MOVE LOW-VALUES             TO TS-RECD.                              
           MOVE 'U'                    TO TS-RSC-TYPE.                          
                                                                                
           SET  IUI-USER-RECORD        TO TRUE.                                 
           MOVE LOW-VALUE              TO IUI-KEYD.                             
                                                                                
           PERFORM Q41-STARTBR-IESCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT IUI-USER-RECORD                                          
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q43-READNEXT-IESCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND IUI-USER-RECORD                                                
               MOVE SPACES             TO TS-RESOURCE                           
               INSPECT IUI-US-USRNAME                                           
                 CONVERTING UPPER-CASE TO LOWER-CASE                            
               INSPECT IUI-US-USRNAME(1:1)                                      
                 CONVERTING LOWER-CASE TO UPPER-CASE                            
               MOVE LENGTH OF IUI-US-USRNAME TO LEN                             
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM 1 BY 1 UNTIL SUB > LEN                        
                   IF (IUI-US-USRNAME(SUB:1) = SPACE OR '*')                    
                   AND IUI-US-USRNAME(SUB + 1:1) > SPACE                        
                       INSPECT IUI-US-USRNAME(SUB + 1:1)                        
                         CONVERTING LOWER-CASE TO UPPER-CASE                    
                   END-IF                                                       
               END-PERFORM                                                      
               PERFORM WITH TEST BEFORE                                         
                 VARYING CNT FROM LEN BY -1                                     
                   UNTIL CNT < 1 OR IUI-US-USRNAME(CNT:1) > SPACE               
               END-PERFORM                                                      
               PERFORM WITH TEST BEFORE                                         
                 VARYING POS FROM CNT BY -1                                     
                   UNTIL POS < 1 OR IUI-US-USRNAME(POS:1) = SPACE               
               END-PERFORM                                                      
               SUBTRACT POS          FROM CNT                                   
               MOVE IUI-US-USRNAME(POS + 1:CNT)                                 
                                       TO TS-RESOURCE                           
               IF  POS > 0                                                      
                 STRING ', ' IUI-US-USRNAME(1:POS - 1)                          
                   DELIMITED BY SIZE INTO TS-RESOURCE(CNT + 1:)                 
               END-IF                                                           
               MOVE SPACES             TO TS-RSC-DESC                           
               STRING IUI-US-USRIDNT      DELIMITED BY LOW-VALUE                
                                     INTO TS-RSC-DESC                           
               SET  BIT-DECODE-FROM-BYTE TO TRUE                                
               MOVE IUI-US-USRINFO     TO BIT-BYTE                              
               CALL 'BITMAN'        USING BITMAN-PARMS                          
               EVALUATE TRUE                                                    
               WHEN BIT-0-IS-ON                                                 
                AND BIT-1-IS-ON                                                 
                   MOVE '4'            TO TS-ACCESS                             
               WHEN BIT-0-IS-OFF                                                
                AND BIT-1-IS-ON                                                 
                   MOVE '3'            TO TS-ACCESS                             
               WHEN OTHER                                                       
                   MOVE '2'            TO TS-ACCESS                             
               END-EVALUATE                                                     
               PERFORM Q33-TS-WRITEQ                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
               PERFORM Q45-ENDBR-IESCNTL                                        
               PERFORM Q31-TS-NUMITEMS                                          
                                                                                
               MOVE 'Sorting...     '  TO POPUP-TEXT                            
               PERFORM L10-DISPLAY-POPUP THRU L15-EXIT                          
                                                                                
               MOVE TS-QUEUE           TO CICSSORT-QNAME                        
               MOVE TS-TOTL            TO CICSSORT-RECCNT                       
               MOVE LENGTH OF TS-RECD  TO CICSSORT-RECLEN                       
               MOVE +5                 TO CICSSORT-KEYOFF                       
               MOVE +18                TO CICSSORT-KEYLEN                       
               SET CICSSORT-ASCENDING  TO TRUE                                  
                                                                                
               MOVE CICSSORT-QNAME     TO CICSSORT-ONAME                        
               EXEC CICS LINK                                                   
                         PROGRAM(CICSSORT)                                      
                         COMMAREA(CICSSORT-PARMS)                               
               END-EXEC                                                         
               IF  CICSSORT-OK                                                  
                   EXEC CICS DELETEQ TS                                         
                             QUEUE(CICSSORT-ONAME)                              
                   END-EXEC                                                     
                   MOVE CICSSORT-QNAME TO TS-QUEUE                              
               END-IF                                                           
           END-IF.                                                              
       L55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       L90-ERASE-SKIP-TO.                                                       
           MOVE 1                      TO LEN.                                  
                                                                                
      *screen buffer address = (RW - 1) * SCRNWDTH + (COL - 1)                  
           COMPUTE HALF-WORD = (07 - 1) * SCRNWDTH + (013 - 1).                 
      *... starting at this screen position ...                                 
           STRING SCRSETBA TWO-BYTES      DELIMITED BY SIZE                     
               INTO POPUP-TEXT       WITH POINTER LEN.                          
                                                                                
           COMPUTE HALF-WORD = (07 - 1) * SCRNWDTH + (048 - 1).                 
      *... repeat low-value char up to but not including this position.         
           STRING SCREPEAT TWO-BYTES X'00' DELIMITED BY SIZE                    
               INTO POPUP-TEXT       WITH POINTER LEN.                          
                                                                                
           SUBTRACT 1                FROM LEN.                                  
           EXEC CICS SEND                                                       
                     FROM(POPUP-TEXT)                                           
                     LENGTH(LEN)                                                
                     CTLCHAR(WRTFKFRS)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       L95-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
       P00-BUILD-WINVSEC-RESID.                                                 
/DLC1/     EVALUATE TRUE                                                        
/DLC1/     WHEN VSECUPD-RES-USER                                                
/DLC1/     WHEN VSECUPD-RES-GROUP  SET TEMP-CLASS-GROUP    TO TRUE              
/DLC1/     WHEN VSECUPD-RES-TRANS  SET TEMP-CLASS-TCICSTRN TO TRUE              
/DLC1/     WHEN OTHER              SET TEMP-CLASS-FACILITY TO TRUE              
/DLC1/     END-EVALUATE.                                                        
           MOVE SPACES                 TO WINVSEC-RESID.                        
           MOVE 1                      TO WINVSEC-RESIDLEN.                     
           IF  VSECUPD-RES-FACILITY                                             
           AND (VSECUPD-RES-NAME(1:3) NOT = 'WIN'                               
             OR VSECUPD-RES-NAME(4:4) NOT = VSECUPD-FAC-TYPE                    
             OR VSECUPD-RES-NAME(8:1) NOT = '.')                                
           THEN                                                                 
               STRING 'WIN' VSECUPD-FAC-TYPE '.' DELIMITED BY SIZE              
                 INTO WINVSEC-RESID  WITH POINTER WINVSEC-RESIDLEN              
           END-IF.                                                              
           STRING VSECUPD-RES-NAME        DELIMITED BY SPACE                    
             INTO WINVSEC-RESID      WITH POINTER WINVSEC-RESIDLEN.             
           EVALUATE TRUE                                                        
           WHEN VSECUPD-RES-FACILITY                                            
           WHEN VSECUPD-RES-TRANS                                               
               SUBTRACT 1            FROM WINVSEC-RESIDLEN                      
           WHEN OTHER                                                           
               MOVE 8                  TO WINVSEC-RESIDLEN                      
           END-EVALUATE.                                                        
       P05-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P10-FIND-NEXT-CODE.                                                      
                                                                                
           COMPUTE CNT = LENGTH OF WINVSEC-DATA                                 
                       / LENGTH OF WINVSEC-ENTRY.                               
                                                                                
           IF  VSEC-CURR-CODE NOT > SPACES                                      
               MOVE WINVSEC-CODE(CNT)  TO VSEC-CURR-CODE                        
           END-IF.                                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL WINVSEC-CODE(SUB)  = VSEC-CURR-CODE                        
           END-PERFORM.                                                         
                                                                                
           ADD  1                      TO SUB.                                  
           MOVE ZERO                   TO IDX.                                  
                                                                                
           PERFORM WITH TEST AFTER                                              
             VARYING SUB FROM SUB BY 1                                          
               UNTIL VSEC-CURR-CODE NOT = HIGH-VALUES                           
             IF  SUB > CNT                                                      
                 MOVE 1                TO SUB                                   
             END-IF                                                             
             PERFORM P12-GET-SECTION-ACCESS                                     
           END-PERFORM.                                                         
                                                                                
           PERFORM P13-SET-SECTION-ACCESS.                                      
                                                                                
       P11-FIND-PREV-CODE.                                                      
                                                                                
           COMPUTE CNT = LENGTH OF WINVSEC-DATA                                 
                       / LENGTH OF WINVSEC-ENTRY.                               
                                                                                
           IF  VSEC-CURR-CODE NOT > SPACES                                      
               MOVE WINVSEC-CODE(1)    TO VSEC-CURR-CODE                        
           END-IF.                                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL WINVSEC-CODE(SUB)  = VSEC-CURR-CODE                        
           END-PERFORM.                                                         
                                                                                
           SUBTRACT 1                FROM SUB.                                  
           MOVE ZERO                   TO IDX.                                  
                                                                                
           PERFORM WITH TEST AFTER                                              
             VARYING SUB FROM SUB BY -1                                         
               UNTIL VSEC-CURR-CODE NOT = HIGH-VALUES                           
             IF  SUB < 1                                                        
                 MOVE CNT              TO SUB                                   
             END-IF                                                             
             PERFORM P12-GET-SECTION-ACCESS                                     
           END-PERFORM.                                                         
                                                                                
           PERFORM P13-SET-SECTION-ACCESS.                                      
                                                                                
       P12-GET-SECTION-ACCESS.                                                  
                                                                                
           ADD  1                      TO IDX.                                  
                                                                                
           MOVE SPACES                 TO WINVSEC-RESID.                        
           MOVE 1                      TO WINVSEC-RESIDLEN.                     
           STRING WINVSEC-PREFIX          DELIMITED BY SIZE                     
                  WINVSEC-CODE(SUB)       DELIMITED BY SPACE                    
               INTO WINVSEC-RESID    WITH POINTER WINVSEC-RESIDLEN.             
           SUBTRACT 1                FROM WINVSEC-RESIDLEN.                     
/DLC1/*              RESCLASS(WINVSEC-CLASS)                                    
           EXEC CICS QUERY SECURITY                                             
/DLC1/               RESCLASS('FACILITY')                                       
                     RESID(WINVSEC-RESID)                                       
                     RESIDLENGTH(WINVSEC-RESIDLEN)                              
                     READ(QRY-READ)                                             
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND (QRY-READABLE OR VSEC-APPLICTN-ADMINISTRATOR)                    
               MOVE 'WIN'              TO VSEC-CURR-WIN                         
               MOVE WINVSEC-CODE(SUB)  TO VSEC-CURR-CODE                        
               MOVE '.'                TO VSEC-CURR-PRD                         
           ELSE                                                                 
               IF  IDX > CNT                                                    
                   MOVE LOW-VALUES     TO VSEC-CURR-CODE                        
               ELSE                                                             
                   MOVE HIGH-VALUES    TO VSEC-CURR-CODE                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
       P13-SET-SECTION-ACCESS.                                                  
                                                                                
           MOVE WINVSEC-TITLE(SUB)     TO VSEC-MAP-TITLE.                       
                                                                                
           EVALUATE TRUE                                                        
           WHEN QRY-ALTERABLE SET VSEC-SECTION-ADMINISTRATOR TO TRUE            
           WHEN QRY-UPDATABLE SET VSEC-SECTION-SUPERVISOR    TO TRUE            
           WHEN QRY-READABLE  SET VSEC-SECTION-ACCESSIBLE    TO TRUE            
           WHEN OTHER         SET VSEC-SECTION-DENIED        TO TRUE            
           END-EVALUATE.                                                        
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND VSEC-CURR-CODE > SPACES                                          
               PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT                     
           END-IF.                                                              
                                                                                
       P14-SET-GENERIC-ACCESS.                                                  
                                                                                
           IF  VSEC-UGRP-SECTION                                                
           OR  VSEC-USER-SECTION                                                
               SET VSEC-GENERIC-DENIED TO TRUE                                  
               GO TO P15-SET-RESOURCE-ACCESS                                    
           END-IF.                                                              
                                                                                
           IF  VSEC-TRAN-SECTION                                                
               SET WINVSEC-CLASS-TCICSTRN TO TRUE                               
               MOVE SPACES             TO WINVSEC-RESID2                        
               MOVE ZEROES             TO FULL-WORD                             
           ELSE                                                                 
               SET WINVSEC-CLASS-FACILITY TO TRUE                               
               MOVE VSEC-CURR-PFX      TO WINVSEC-RESID2                        
               MOVE LENGTH OF VSEC-CURR-PFX TO FULL-WORD                        
           END-IF.                                                              
                                                                                
           EXEC CICS QUERY SECURITY                                             
                     RESCLASS(WINVSEC-CLASS)                                    
                     RESID(WINVSEC-RESID2)                                      
                     RESIDLENGTH(FULL-WORD)                                     
                     READ(QRY-READ)                                             
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
               SET VSEC-GENERIC-DENIED TO TRUE                                  
               GO TO P15-SET-RESOURCE-ACCESS                                    
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
           WHEN QRY-ALTERABLE SET VSEC-GENERIC-ADMINISTRATOR TO TRUE            
           WHEN QRY-UPDATABLE SET VSEC-GENERIC-SUPERVISOR    TO TRUE            
           WHEN QRY-READABLE  SET VSEC-GENERIC-ACCESSIBLE    TO TRUE            
           WHEN OTHER         SET VSEC-GENERIC-DENIED        TO TRUE            
           END-EVALUATE.                                                        
                                                                                
           IF  NOT VSEC-APPS-SECTION                                            
               GO TO P15-SET-RESOURCE-ACCESS                                    
           END-IF.                                                              
                                                                                
           MOVE ZEROES                 TO CNT.                                  
           PERFORM WITH TEST AFTER                                              
             VARYING FULL-WORD FROM 1 BY 1                                      
               UNTIL FULL-WORD > LENGTH OF WINVSEC-RESID                        
                  OR CNT = 2                                                    
               IF  WINVSEC-RESID(FULL-WORD:1) = '.'                             
                   ADD  1              TO CNT                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
           IF  CNT < 2                                                          
               GO TO P15-SET-RESOURCE-ACCESS                                    
           END-IF.                                                              
                                                                                
           SET WINVSEC-CLASS-FACILITY  TO TRUE.                                 
                                                                                
           EXEC CICS QUERY SECURITY                                             
                     RESCLASS(WINVSEC-CLASS)                                    
                     RESID(WINVSEC-RESID)                                       
                     RESIDLENGTH(FULL-WORD)                                     
                     READ(QRY-READ)                                             
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
           OR  NOT QRY-READABLE                                                 
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
               GO TO P15-SET-RESOURCE-ACCESS                                    
           END-IF.                                                              
                                                                                
           EVALUATE TRUE                                                        
           WHEN QRY-ALTERABLE SET VSEC-GENERIC-ADMINISTRATOR TO TRUE            
           WHEN QRY-UPDATABLE SET VSEC-GENERIC-SUPERVISOR    TO TRUE            
           WHEN OTHER         SET VSEC-GENERIC-ACCESSIBLE    TO TRUE            
           END-EVALUATE.                                                        
                                                                                
       P15-SET-RESOURCE-ACCESS.                                                 
                                                                                
           IF  WINVSEC-RESID NOT > SPACES                                       
               SET  QRY-NOTREADABLE    TO TRUE                                  
               SET  QRY-NOTUPDATABLE   TO TRUE                                  
               SET  QRY-NOTALTERABLE   TO TRUE                                  
               GO TO P16-EXIT                                                   
           END-IF.                                                              
                                                                                
/DLC1/*              RESCLASS(WINVSEC-CLASS)                                    
           EXEC CICS QUERY SECURITY                                             
/DLC1/               RESCLASS(TEMP-CLASS)                                       
                     RESID(WINVSEC-RESID)                                       
                     RESIDLENGTH(WINVSEC-RESIDLEN)                              
                     READ(QRY-READ)                                             
                     UPDATE(QRY-UPDATE)                                         
                     ALTER(QRY-ALTER)                                           
                     LOGMESSAGE(MSG-NOLOG)                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
               SET  QRY-NOTREADABLE    TO TRUE                                  
               SET  QRY-NOTUPDATABLE   TO TRUE                                  
               SET  QRY-NOTALTERABLE   TO TRUE                                  
               GO TO P16-EXIT                                                   
           END-IF.                                                              
                                                                                
       P16-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P30-TS-DETL-BUILD.                                                       
                                                                                
           MOVE 'Generating...'        TO POPUP-TEXT.                           
           PERFORM Q00-DISPLAY-POPUP THRU Q05-EXIT.                             
                                                                                
           PERFORM Q30-TS-DELETEQ.                                              
           MOVE +13                    TO CICSSORT-KEYLEN                       
                                                                                
           EVALUATE TRUE                                                        
           WHEN VSEC-SEL-FACILITY                                               
           WHEN VSEC-SEL-TRANS                                                  
               PERFORM P40-RSRC-PERMITS THRU P45-EXIT                           
           WHEN VSEC-SEL-GROUP                                                  
           WHEN VSEC-SEL-SUBGROUP                                               
               PERFORM P50-CHASE-GROUPS THRU P55-EXIT                           
               PERFORM P70-CHASE-RESRCS THRU P75-EXIT                           
               PERFORM P80-CHASE-TRANS  THRU P85-EXIT                           
           WHEN OTHER                                                           
               PERFORM P60-CHASE-USERS  THRU P65-EXIT                           
               PERFORM P70-CHASE-RESRCS THRU P75-EXIT                           
               PERFORM P80-CHASE-TRANS  THRU P85-EXIT                           
           END-EVALUATE.                                                        
                                                                                
           IF  EIBRESP = DFHRESP(NORMAL)                                        
                      OR DFHRESP(ENDFILE)                                       
               PERFORM Q31-TS-NUMITEMS                                          
           END-IF.                                                              
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P35-EXIT                                                   
           END-IF.                                                              
                                                                                
           IF  TS-TOTL > 1                                                      
               MOVE 'Sorting...     '  TO POPUP-TEXT                            
               PERFORM Q00-DISPLAY-POPUP THRU Q05-EXIT                          
                                                                                
               MOVE TS-QUEUE           TO CICSSORT-QNAME                        
               MOVE TS-TOTL            TO CICSSORT-RECCNT                       
               MOVE LENGTH OF TS-RECD  TO CICSSORT-RECLEN                       
               MOVE +0                 TO CICSSORT-KEYOFF                       
               SET CICSSORT-ASCENDING  TO TRUE                                  
                                                                                
               MOVE CICSSORT-QNAME     TO CICSSORT-ONAME                        
               EXEC CICS LINK                                                   
                         PROGRAM(CICSSORT)                                      
                         COMMAREA(CICSSORT-PARMS)                               
               END-EXEC                                                         
               IF  CICSSORT-OK                                                  
                   EXEC CICS DELETEQ TS                                         
                             QUEUE(CICSSORT-ONAME)                              
                   END-EXEC                                                     
                   MOVE CICSSORT-QNAME TO TS-QUEUE                              
               END-IF                                                           
           END-IF.                                                              
                                                                                
       P35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P40-RSRC-PERMITS.                                                        
           MOVE LOW-VALUE              TO TS-SELECT.                            
                                                                                
           IF  VSEC-SEL-TRANS                                                   
               SET  BSM-TRANS          TO TRUE                                  
           ELSE                                                                 
               SET  BSM-FACILITY       TO TRUE                                  
           END-IF.                                                              
           MOVE SPACES                 TO BSM-RESOURCE.                         
           MOVE 1                      TO PTR.                                  
           STRING VSEC-SEL-RSRC           DELIMITED BY SPACE                    
               INTO BSM-RESOURCE     WITH POINTER PTR                           
           PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                          
           PERFORM Q26-READEQ-BSTCNTL.                                          
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > BSM-FAC-PERMITS                                      
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
               PERFORM P92-ADD-USERS                                            
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P45-EXIT                                                   
           END-IF.                                                              
                                                                                
           PERFORM P93-UPD-GROUPS.                                              
       P45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P50-CHASE-GROUPS.                                                        
           MOVE LOW-VALUE              TO TS-SELECT.                            
                                                                                
           SET  BSM-GROUP              TO TRUE.                                 
           MOVE VSEC-SEL-RSRC          TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-GROUP                                                
                OR BSM-RESOURCE(1:8) NOT = VSEC-SEL-RSRC                        
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-GROUP                                                      
             AND BSM-RESOURCE(1:8) = VSEC-SEL-RSRC                              
               PERFORM P92-ADD-USERS                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P55-EXIT                                                   
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
                                                                                
           PERFORM P60-CHASE-USERS THRU P65-EXIT.                               
                                                                                
           PERFORM P93-UPD-GROUPS.                                              
       P55-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P60-CHASE-USERS.                                                         
           MOVE LOW-VALUE              TO TS-SELECT.                            
                                                                                
           SET  BSM-GROUP              TO TRUE.                                 
           MOVE SPACES                 TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-GROUP                                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-GROUP                                                      
             AND BSM-RESOURCE(17:8) = VSEC-SEL-RSRC                             
               PERFORM P92-ADD-USERS                                            
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P65-EXIT                                                   
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
                                                                                
           PERFORM P93-UPD-GROUPS.                                              
       P65-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P70-CHASE-RESRCS.                                                        
           MOVE LOW-VALUE              TO TS-SELECT.                            
                                                                                
           SET  BSM-FACILITY           TO TRUE.                                 
           MOVE 'WIN'                  TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-FACILITY                                             
                OR BSM-RESOURCE(1:3) NOT = 'WIN'                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-FACILITY                                                   
             AND BSM-RESOURCE(1:3) = 'WIN'                                      
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM 1 BY 1                                        
                   UNTIL SUB > BSM-FAC-PERMITS                                  
                      OR BSM-FAC-USERID(SUB) = VSEC-SEL-RSRC                    
               END-PERFORM                                                      
               IF  SUB <= BSM-FAC-PERMITS                                       
                   SET  TS-RSC-FACILITY TO TRUE                                 
                   PERFORM P94-ADD-RESRC                                        
               END-IF                                                           
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P75-EXIT                                                   
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
       P75-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P80-CHASE-TRANS.                                                         
           MOVE LOW-VALUE              TO TS-SELECT.                            
                                                                                
           SET  BSM-TRANS              TO TRUE.                                 
           MOVE SPACES                 TO BSM-RESOURCE.                         
           MOVE LOW-VALUE              TO BSM-SEQUENCE.                         
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT BSM-TRANS                                                
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-TRANS                                                      
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM 1 BY 1                                        
                   UNTIL SUB > BSM-FAC-PERMITS                                  
                      OR BSM-FAC-USERID(SUB) = VSEC-SEL-RSRC                    
               END-PERFORM                                                      
               IF  SUB <= BSM-FAC-PERMITS                                       
                   SET  TS-RSC-TRANS   TO TRUE                                  
                   PERFORM P94-ADD-RESRC                                        
               END-IF                                                           
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO P85-EXIT                                                   
           END-IF.                                                              
                                                                                
           PERFORM Q25-ENDBR-BSTCNTL.                                           
       P85-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P92-ADD-USERS.                                                           
           SET  IUI-USER-RECORD        TO TRUE.                                 
           MOVE LOW-VALUE              TO IUI-KEYD.                             
                                                                                
           SET  TS-RSC-GROUP           TO TRUE.                                 
           IF  BSM-GROUP                                                        
             IF  BSM-RESOURCE(17:8) = VSEC-SEL-RSRC                             
               MOVE BSM-RESOURCE(1:8)  TO TS-RESOURCE                           
               STRING BSM-RESOURCE(1:8)   DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
             ELSE                                                               
               MOVE BSM-RESOURCE(17:8) TO TS-RESOURCE                           
               STRING BSM-RESOURCE(17:8)  DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
             END-IF                                                             
           ELSE                                                                 
             MOVE BSM-FAC-USERID(SUB)  TO TS-RESOURCE                           
             MOVE SPACES               TO TS-RSC-DESC                           
             EVALUATE TRUE                                                      
             WHEN BSM-FAC-AC-ALTER(SUB)   MOVE '4' TO TS-ACCESS                 
             WHEN BSM-FAC-AC-UPDATE(SUB)  MOVE '3' TO TS-ACCESS                 
             WHEN BSM-FAC-AC-READ(SUB)    MOVE '2' TO TS-ACCESS                 
             WHEN OTHER                   MOVE ' ' TO TS-ACCESS                 
             END-EVALUATE                                                       
             STRING BSM-FAC-USERID(SUB)   DELIMITED BY SPACE                    
                                     INTO IUI-US-USRIDNT                        
           END-IF.                                                              
                                                                                
           PERFORM Q46-READEQ-IESCNTL.                                          
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
             MOVE DFHRESP(NORMAL)      TO EIBRESP                               
             IF  BSM-GROUP                                                      
               IF  BSM-RESOURCE(17:8) = VSEC-SEL-RSRC                           
                   SET TS-RSC-SUBGROUP TO TRUE                                  
               END-IF                                                           
               MOVE BSM-RSRCDESC       TO TS-RSC-DESC                           
               MOVE LOW-VALUE          TO TS-ACCESS                             
             END-IF                                                             
           ELSE                                                                 
             SET  TS-RSC-USER          TO TRUE                                  
             IF  IUI-US-USRNAME > SPACES                                        
               MOVE IUI-US-USRNAME     TO TS-RSC-DESC                           
             ELSE                                                               
               MOVE '* NAME UNSPECIFIED *' TO TS-RSC-DESC                       
             END-IF                                                             
             IF  BSM-GROUP                                                      
               SET  BIT-DECODE-FROM-BYTE TO TRUE                                
               MOVE IUI-US-USRINFO     TO BIT-BYTE                              
               CALL 'BITMAN'        USING BITMAN-PARMS                          
               EVALUATE TRUE                                                    
               WHEN BIT-0-IS-ON                                                 
                AND BIT-1-IS-ON                                                 
                   MOVE '4'            TO TS-ACCESS                             
               WHEN BIT-0-IS-OFF                                                
                AND BIT-1-IS-ON                                                 
                   MOVE '3'            TO TS-ACCESS                             
               WHEN OTHER                                                       
                   MOVE '2'            TO TS-ACCESS                             
               END-EVALUATE                                                     
             END-IF                                                             
           END-IF.                                                              
           PERFORM Q33-TS-WRITEQ.                                               
                                                                                
       P93-UPD-GROUPS.                                                          
           PERFORM Q31-TS-NUMITEMS.                                             
           PERFORM WITH TEST BEFORE                                             
             VARYING SS-ITEM FROM 1 BY 1                                        
               UNTIL SS-ITEM > TS-TOTL                                          
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
             PERFORM Q32-TS-READQ                                               
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND (TS-RSC-GROUP OR TS-RSC-SUBGROUP)                              
               SET  BSM-GROUP          TO TRUE                                  
               MOVE TS-RESOURCE(1:8)   TO BSM-RESOURCE                          
               MOVE GROUP-HEADER       TO BSM-RESOURCE(17:8)                    
               MOVE LOW-VALUE          TO BSM-SEQUENCE                          
               PERFORM Q26-READEQ-BSTCNTL                                       
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                 MOVE BSM-RSRCDESC     TO TS-RSC-DESC                           
               ELSE                                                             
                 SET  BSM-TRANS        TO TRUE                                  
                 MOVE 1                TO PTR                                   
                 STRING TS-RESOURCE       DELIMITED BY SPACE                    
                     INTO BSM-RESOURCE    WITH POINTER PTR                      
                 PERFORM Q70-CHANGE-TO-GENERIC THRU Q75-EXIT                    
                 PERFORM Q26-READEQ-BSTCNTL                                     
                 IF  EIBRESP = DFHRESP(NORMAL)                                  
                   SET  TS-RSC-TRANS   TO TRUE                                  
                   MOVE BSM-RSRCDESC   TO TS-RSC-DESC                           
                   IF  NOT BSM-FAC-UA-DENIED                                    
                     EVALUATE TRUE                                              
                     WHEN BSM-FAC-UA-ALTER  MOVE '4' TO TS-ACCESS               
                     WHEN BSM-FAC-UA-UPDATE MOVE '3' TO TS-ACCESS               
                     WHEN BSM-FAC-UA-READ   MOVE '2' TO TS-ACCESS               
                     WHEN OTHER             MOVE ' ' TO TS-ACCESS               
                     END-EVALUATE                                               
                   END-IF                                                       
                 ELSE                                                           
                   MOVE DFHRESP(NORMAL) TO EIBRESP                              
                   SET  TS-RSC-USER    TO TRUE                                  
                   MOVE '* USER NOT DEFINED *' TO TS-RSC-DESC                   
                   MOVE LOW-VALUE      TO TS-ACCESS                             
                 END-IF                                                         
               END-IF                                                           
               PERFORM Q34-TS-REWRITEQ                                          
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
       P94-ADD-RESRC.                                                           
           IF  TS-RSC-FACILITY                                                  
               MOVE BSM-RESOURCE(4:4)  TO TS-FAC-CODE                           
           ELSE                                                                 
               MOVE SPACES             TO TS-FAC-CODE                           
           END-IF.                                                              
           MOVE BSM-RESOURCE           TO TS-RESOURCE.                          
           MOVE LOW-VALUES             TO TWO-BYTES.                            
           MOVE BSM-KEYVALEN           TO LO-LSB.                               
           IF  BSM-FAC-GENERIC                                                  
               ADD  1                  TO HALF-WORD                             
               MOVE '*'                TO TS-RESOURCE(HALF-WORD:1)              
           END-IF.                                                              
           MOVE BSM-RSRCDESC           TO TS-RSC-DESC                           
           EVALUATE TRUE                                                        
           WHEN BSM-FAC-AC-ALTER(SUB)     MOVE '4' TO TS-ACCESS                 
           WHEN BSM-FAC-AC-UPDATE(SUB)    MOVE '3' TO TS-ACCESS                 
           WHEN BSM-FAC-AC-READ(SUB)      MOVE '2' TO TS-ACCESS                 
           WHEN OTHER                     MOVE ' ' TO TS-ACCESS                 
           END-EVALUATE                                                         
           IF  (HALF-WORD + 5) > CICSSORT-KEYLEN                                
               COMPUTE CICSSORT-KEYLEN  = HALF-WORD + 5                         
           END-IF.                                                              
           PERFORM Q33-TS-WRITEQ.                                               
                                                                                
       P95-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MORE PERFORMED ROUTINES                                     *        
      ******************************************************************        
                                                                                
       Q00-DISPLAY-POPUP.                                                       
                                                                                
           EXEC CICS SEND                                                       
                     MAP(DETLXXX)                                               
                     MAPSET(VSECMS)                                             
                     MAPONLY                                                    
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
       Q02-DISPLAY-WINDOW.                                                      
                                                                                
           EXEC CICS GETMAIN                                                    
                     SET(ADDRESS OF WAITWINI)                                   
                     LENGTH(LENGTH OF WAITWINI)                                 
                     INITIMG(LOVALUE)                                           
           END-EXEC.                                                            
                                                                                
           MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.                       
           EXEC CICS SEND                                                       
                     MAP(WAITWIN)                                               
                     MAPSET(VSECMS)                                             
                     FROM(WAITWINO)                                             
                     TERMINAL                                                   
                     CURSOR(+926)                                               
                     WAIT                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA(WAITWINI)                                             
           END-EXEC.                                                            
                                                                                
       Q05-EXIT.                                                                
           EXIT.                                                                
                                                                                
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
           IF  EIBRESP = DFHRESP(NOTFND) AND EIBRESP2 = +8                      
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
               SET  QRY-READABLE       TO TRUE                                  
           END-IF.                                                              
       Q15-EXIT-CHECK.                                                          
           EXIT.                                                                
                                                                                
       Q20-INITKEY-SELECTED.                                                    
           MOVE WINVSEC-CLASS          TO BSM-CLASS                             
           IF  VSEC-TRAN-SECTION                                                
           OR  VSEC-UGRP-SECTION                                                
           OR  VSEC-USER-SECTION                                                
               MOVE SPACES             TO BSM-RESOURCE                          
               MOVE 1                  TO PTR                                   
           ELSE                                                                 
               MOVE VSEC-CURR-PFX      TO BSM-RESOURCE                          
               COMPUTE PTR = LENGTH OF VSEC-CURR-PFX + 1                        
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
           EVALUATE TRUE                                                        
           WHEN VSEC-TRAN-SECTION                                               
               CONTINUE                                                         
           WHEN VSEC-UGRP-SECTION                                               
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(17:8) NOT = GROUP-HEADER                        
                   GO TO Q23-READNEXT-BSTCNTL                                   
               END-IF                                                           
           WHEN OTHER                                                           
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(1:8) = VSEC-CURR-PFX                            
                   MOVE ZEROES         TO WINVSEC-RESIDLEN                      
                   PERFORM Q10-CHECK-AUTHORIZATION THRU Q15-EXIT-CHECK          
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND NOT QRY-READABLE                                         
                   AND NOT VSEC-SECTION-ACCESSIBLE                              
                   AND NOT VSEC-APPLICTN-ADMINISTRATOR                          
                       GO TO Q23-READNEXT-BSTCNTL                               
                   END-IF                                                       
               END-IF                                                           
           END-EVALUATE.                                                        
       Q23-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND (BSM-CLASS NOT = WINVSEC-CLASS OR                                
                ((NOT VSEC-TRAN-SECTION AND NOT VSEC-UGRP-SECTION) AND          
                  BSM-RESOURCE(1:8) NOT = VSEC-CURR-PFX))                       
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
           EVALUATE TRUE                                                        
           WHEN VSEC-TRAN-SECTION                                               
               CONTINUE                                                         
           WHEN VSEC-UGRP-SECTION                                               
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(17:8) NOT = GROUP-HEADER                        
                   GO TO Q24-READPREV-BSTCNTL                                   
               END-IF                                                           
           WHEN OTHER                                                           
               IF  EIBRESP = DFHRESP(NORMAL)                                    
               AND BSM-CLASS = WINVSEC-CLASS                                    
               AND BSM-RESOURCE(1:8) = VSEC-CURR-PFX                            
                   MOVE ZEROES             TO WINVSEC-RESIDLEN                  
                   PERFORM Q10-CHECK-AUTHORIZATION THRU Q15-EXIT-CHECK          
                   IF  EIBRESP = DFHRESP(NORMAL)                                
                   AND NOT QRY-READABLE                                         
                   AND NOT VSEC-SECTION-ACCESSIBLE                              
                   AND NOT VSEC-APPLICTN-ADMINISTRATOR                          
                       GO TO Q24-READPREV-BSTCNTL                               
                   END-IF                                                       
               END-IF                                                           
           END-EVALUATE.                                                        
       Q24-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND (BSM-CLASS NOT = WINVSEC-CLASS OR                                
                ((NOT VSEC-TRAN-SECTION AND NOT VSEC-UGRP-SECTION) AND          
                  BSM-RESOURCE(1:8) NOT = VSEC-CURR-PFX))                       
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
       Q32-TS-READQ.                                                            
           EXEC CICS READQ TS                                                   
                     QUEUE   (TS-QUEUE)                                         
                     INTO    (TS-RECD)                                          
                     ITEM    (SS-ITEM)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q33-TS-WRITEQ.                                                           
           EXEC CICS WRITEQ TS                                                  
                     QUEUE   (TS-QUEUE)                                         
                     FROM    (TS-RECD)                                          
                     MAIN                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q34-TS-REWRITEQ.                                                         
           EXEC CICS WRITEQ TS                                                  
                     QUEUE   (TS-QUEUE)                                         
                     FROM    (TS-RECD)                                          
                     ITEM    (SS-ITEM)                                          
                     REWRITE                                                    
                     MAIN                                                       
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q40-INITKEY-IESCNTL.                                                     
           MOVE LOW-VALUES             TO IUI-KEY.                              
           SET  IUI-USER-RECORD        TO TRUE.                                 
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
       Q43-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND NOT IUI-USER-RECORD                                              
             PERFORM Q44-READPREV-IESCNTL                                       
             MOVE DFHRESP(ENDFILE)     TO EIBRESP                               
           END-IF.                                                              
       Q44-READPREV-IESCNTL.                                                    
           EXEC CICS READPREV                                                   
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     RIDFLD  (IUI-KEY)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q44-IS-SELECTED.                                                         
           IF  EIBRESP = DFHRESP(NORMAL)                                        
           AND NOT IUI-USER-RECORD                                              
             PERFORM Q43-READNEXT-IESCNTL                                       
             MOVE DFHRESP(ENDFILE)     TO EIBRESP                               
           END-IF.                                                              
       Q45-ENDBR-IESCNTL.                                                       
           EXEC CICS ENDBR                                                      
                     DATASET (IESCNTL)                                          
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q46-READEQ-IESCNTL.                                                      
           EXEC CICS READ                                                       
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     RIDFLD  (IUI-KEY)                                          
                     EQUAL                                                      
                     NOHANDLE                                                   
           END-EXEC.                                                            
       Q46-READGE-IESCNTL.                                                      
           EXEC CICS READ                                                       
                     DATASET (IESCNTL)                                          
                     INTO    (IESCNTL-RECORD)                                   
                     RIDFLD  (IUI-KEY)                                          
                     GTEQ                                                       
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
           IF  BSM-FAC-GENERIC                                                  
               ADD  1                  TO HALF-WORD                             
               MOVE '*'                TO WINVSEC-RESID(HALF-WORD:1)            
           END-IF.                                                              
           IF  VSEC-TRAN-SECTION                                                
               MOVE ZEROES             TO CNT                                   
               MOVE HALF-WORD          TO LEN                                   
           ELSE                                                                 
               MOVE LENGTH OF WINVSEC-PREFIX TO CNT                             
               SUBTRACT CNT FROM HALF-WORD GIVING LEN                           
           END-IF.                                                              
       Q85-EXIT.                                                                
           EXIT.                                                                
                                                                                
       Q90-BSM-REFRESH.                                                         
           IF  BSTCNTL-UPDATED                                                  
               EXEC CICS LINK PROGRAM(VSECREFR) END-EXEC                        
               SET  BSTCNTL-NOCHANGE   TO TRUE                                  
           END-IF.                                                              
       Q95-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MORE PERFORMED ROUTINES                                     *        
      ******************************************************************        
                                                                                
/DLC0/ R00-DEL-GROUP-MEMBERS.                                                   
                                                                                
/DLC0/     PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
/DLC0/     PERFORM WITH TEST AFTER                                              
/DLC0/       UNTIL NOT BSM-GROUP                                                
/DLC0/          OR NOT VSECSERV-RTN-OKAY                                        
/DLC0/          OR EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC0/       PERFORM Q23-READNEXT-BSTCNTL                                       
/DLC0/       IF  EIBRESP = DFHRESP(NORMAL)                                      
/DLC0/       AND BSM-GROUP                                                      
/DLC0/       AND BSM-RESOURCE(17:8) = LIST-RSC-IDNT-I(IDX)                      
/DLC0/           MOVE LOW-VALUES       TO VSECSERV-COMMAREA                     
/DLC0/           SET  VSECSERV-TRAN    TO TRUE                                  
/DLC0/           SET  VSECSERV-DEL-PERMISSION TO TRUE                           
/DLC0/           SET  VSECUPD-RES-GROUP TO TRUE                                 
/DLC0/           MOVE BSM-RESOURCE(1:8) TO VSECUPD-RES-NAME                     
/DLC0/           SET  VSECUPD-PER-USER  TO TRUE                                 
/DLC0/           MOVE LIST-RSC-IDNT-I(IDX) TO VSECUPD-PER-NAME                  
/DLC0/           EXEC CICS LINK                                                 
/DLC0/                     PROGRAM(VSECSERV)                                    
/DLC0/                     COMMAREA(VSECSERV-COMMAREA)                          
/DLC0/                     NOHANDLE                                             
/DLC0/           END-EXEC                                                       
/DLC0/           IF  NOT VSECSERV-RTN-OKAY                                      
/DLC0/             MOVE -1             TO ERROR-SWITCH                          
/DLC0/                                    LIST-RSC-IDNT-L(IDX)                  
/DLC0/             MOVE FLDUNFST       TO LIST-SELECT-A(IDX)                    
/DLC0/                                    LIST-RSC-DESC-A(IDX)                  
/DLC0/                                    LIST-UNV-ACCS-A(IDX)                  
/DLC0/             MOVE EXHREVRS       TO LIST-MESSAGE-H                        
/DLC0/             MOVE SPACES         TO LIST-MESSAGE-O                        
/DLC0/             STRING VSECSERV        DELIMITED BY SPACES                   
/DLC0/               ': RC=' VSECSERV-RTN-CODE                                  
/DLC0/                 ', "' VSECSERV-RTN-MESG '"'                              
/DLC0/               DELIMITED BY SIZE INTO LIST-MESSAGE-O                      
/DLC0/           END-IF                                                         
/DLC0/       END-IF                                                             
/DLC0/     END-PERFORM.                                                         
                                                                                
/DLC0/     IF  VSECSERV-RTN-OKAY                                                
/DLC0/     AND EIBRESP = DFHRESP(NORMAL)                                        
/DLC0/       PERFORM Q25-ENDBR-BSTCNTL                                          
/DLC0/     END-IF.                                                              
                                                                                
/DLC0/ R05-EXIT.                                                                
/DLC0/     EXIT.                                                                
                                                                                
       R10-DEL-GROUP-PERMITS.                                                   
                                                                                
           PERFORM Q21-STARTBR-BSTCNTL.                                         
                                                                                
           PERFORM WITH TEST AFTER                                              
             UNTIL BSM-CLASS NOT = WINVSEC-RESID                                
                OR NOT VSECSERV-RTN-OKAY                                        
                OR EIBRESP NOT = DFHRESP(NORMAL)                                
             PERFORM Q23-READNEXT-BSTCNTL                                       
             IF  EIBRESP = DFHRESP(NORMAL)                                      
             AND BSM-CLASS = WINVSEC-RESID                                      
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM 1 BY 1                                        
                   UNTIL SUB > BSM-FAC-PERMITS                                  
                      OR BSM-FAC-USERID(SUB) = LIST-RSC-IDNT-I(IDX)             
               END-PERFORM                                                      
               IF  SUB <= BSM-FAC-PERMITS                                       
                 MOVE LOW-VALUES       TO VSECSERV-COMMAREA                     
                 SET  VSECSERV-TRAN    TO TRUE                                  
                 SET  VSECSERV-DEL-PERMISSION TO TRUE                           
                 IF  BSM-TRANS                                                  
                   SET  VSECUPD-RES-TRANS TO TRUE                               
                   MOVE BSM-RESOURCE   TO VSECUPD-RES-NAME                      
                 ELSE                                                           
                   SET  VSECUPD-RES-FACILITY TO TRUE                            
                   MOVE BSM-RESOURCE(4:4) TO VSECUPD-FAC-TYPE                   
                   MOVE BSM-RESOURCE(9:)  TO VSECUPD-RES-NAME                   
                 END-IF                                                         
                 SET  VSECUPD-PER-GROUP TO TRUE                                 
                 MOVE LIST-RSC-IDNT-I(IDX) TO VSECUPD-PER-NAME                  
                 EXEC CICS LINK                                                 
                           PROGRAM(VSECSERV)                                    
                           COMMAREA(VSECSERV-COMMAREA)                          
                           NOHANDLE                                             
                 END-EXEC                                                       
                 IF  NOT VSECSERV-RTN-OKAY                                      
                   MOVE -1             TO ERROR-SWITCH                          
                                          LIST-RSC-IDNT-L(IDX)                  
                   MOVE FLDUNFST       TO LIST-SELECT-A(IDX)                    
                                          LIST-RSC-DESC-A(IDX)                  
                                          LIST-UNV-ACCS-A(IDX)                  
                   MOVE EXHREVRS       TO LIST-MESSAGE-H                        
                   MOVE SPACES         TO LIST-MESSAGE-O                        
                   STRING VSECSERV        DELIMITED BY SPACES                   
                     ': RC=' VSECSERV-RTN-CODE                                  
                       ', "' VSECSERV-RTN-MESG '"'                              
                     DELIMITED BY SIZE INTO LIST-MESSAGE-O                      
                 END-IF                                                         
               END-IF                                                           
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND (EIBRESP = DFHRESP(NORMAL)                                       
                       OR DFHRESP(ENDFILE))                                     
             PERFORM Q25-ENDBR-BSTCNTL                                          
           END-IF.                                                              
                                                                                
       R15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       R20-ADD-PERMITS.                                                         
                                                                                
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           SET  VSECSERV-GET-RESOURCE  TO TRUE.                                 
           MOVE TS-RESOURCE(4:4)       TO VSECUPD-FAC-TYPE.                     
           IF  TS-RESOURCE(1:3) = 'WIN'                                         
           AND (VSECUPD-FAC-APPS OR                                             
                VSECUPD-FAC-CORP OR                                             
                VSECUPD-FAC-ROLE OR                                             
                VSECUPD-FAC-VSEC)                                               
           AND TS-RESOURCE(8:1) = '.'                                           
               SET VSECUPD-RES-FACILITY TO TRUE                                 
               MOVE TS-RESOURCE(4:4)   TO VSECUPD-FAC-TYPE                      
               MOVE TS-RESOURCE(9:)    TO VSECUPD-RES-NAME                      
           ELSE                                                                 
               MOVE LOW-VALUES         TO VSECUPD-FAC-TYPE                      
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
           END-IF.                                                              
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  NOT VSECSERV-RTN-OKAY                                            
           OR  EIBRESP NOT = DFHRESP(NORMAL)                                    
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE ERROR-SWITCH     TO ADDP-SELECT-L(IDX)                    
                 MOVE EXCRED           TO ADDP-MESSAGE-C                        
                 MOVE SPACES           TO ADDP-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO ADDP-MESSAGE-O(4:66)                  
               ELSE                                                             
                 MOVE ERROR-SWITCH     TO DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE SPACES           TO DETL-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO DETL-MESSAGE-O                        
               END-IF                                                           
             END-IF                                                             
             GO TO R25-EXIT                                                     
           END-IF.                                                              
                                                                                
           IF  VSEC-SEL-GROUP                                                   
           AND VSECUPD-RES-GROUP                                                
               SET  TS-RSC-SUBGROUP    TO TRUE                                  
           ELSE                                                                 
               MOVE VSECUPD-RES-CLASS  TO TS-RSC-TYPE                           
           END-IF.                                                              
           MOVE VSECUPD-FAC-TYPE       TO TS-FAC-CODE.                          
           MOVE VSECUPD-RES-NAME       TO TS-RESOURCE.                          
           IF  TS-RSC-DESC NOT > LOW-VALUES                                     
             MOVE VSECUPD-RES-DESC     TO TS-RSC-DESC                           
           END-IF.                                                              
                                                                                
           PERFORM R30-ALTER-PERMITS THRU R35-EXIT.                             
                                                                                
       R25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       R30-ALTER-PERMITS.                                                       
                                                                                
      * update permission entry                                                 
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           SET VSECSERV-UPD-PERMISSION TO TRUE.                                 
                                                                                
           EVALUATE TRUE ALSO TRUE                                              
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               MOVE VSEC-SEL-RSRC(4:4) TO VSECUPD-FAC-TYPE                      
               MOVE VSEC-SEL-RSRC(9:)  TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-USER     ALSO TS-RSC-FACILITY                          
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               MOVE TS-FAC-CODE        TO VSECUPD-FAC-TYPE                      
               MOVE TS-RESOURCE(9:)    TO VSECUPD-RES-NAME                      
               IF  VSEC-SEL-SUBGROUP                                            
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE VSEC-SEL-TYPE    TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-TRANS                             
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               IF  VSEC-SEL-SUBGROUP                                            
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE VSEC-SEL-TYPE    TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-USER                              
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
               MOVE TS-RSC-DESC        TO VSECUPD-PER-DESC                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN VSEC-SEL-USER     ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-SUBGROUP                          
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               SET  VSECUPD-PER-USER   TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
               MOVE VSEC-SEL-RDSC      TO VSECUPD-PER-DESC                      
               MOVE TS-ACCESS          TO VSECUPD-PER-ACCS                      
                                                                                
           WHEN OTHER                                                           
               MOVE -1                 TO ERROR-SWITCH                          
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE ERROR-SWITCH     TO ADDP-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO ADDP-MESSAGE-H                        
                 MOVE 'Invalid combination of resource identifiers.'            
                                       TO ADDP-MESSAGE-O(4:66)                  
               ELSE                                                             
                 MOVE ERROR-SWITCH     TO DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE 'Invalid combination of resource identifiers.'            
                                       TO DETL-MESSAGE-O                        
               END-IF                                                           
               GO TO R35-EXIT                                                   
           END-EVALUATE.                                                        
                                                                                
           PERFORM P00-BUILD-WINVSEC-RESID THRU P05-EXIT.                       
           PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT.                        
           IF  NOT VSEC-APPLICTN-SUPERVISOR                                     
           AND NOT VSEC-SECTION-SUPERVISOR                                      
           AND NOT VSEC-GENERIC-SUPERVISOR                                      
           AND NOT QRY-UPDATABLE                                                
           OR  VSECUPD-PER-ACCS > SPACES                                        
           AND ((VSEC-SECTION-SUPERVISOR AND                                    
                 VSEC-SECTION-AUTHORITY < VSECUPD-PER-ACCS)                     
             OR (VSEC-GENERIC-SUPERVISOR AND                                    
                 VSEC-GENERIC-AUTHORITY < VSECUPD-PER-ACCS)                     
             OR (NOT QRY-ALTERABLE AND                                          
                                    '3' < VSECUPD-PER-ACCS))                    
           THEN                                                                 
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE -1               TO ERROR-SWITCH                          
                                          ADDP-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO ADDP-MESSAGE-H                        
                 MOVE 'Not authorized for this action.'                         
                                       TO ADDP-MESSAGE-O                        
               ELSE                                                             
                 MOVE -1               TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE 'Not authorized for this action.'                         
                                       TO DETL-MESSAGE-O                        
               END-IF                                                           
               GO TO R35-EXIT                                                   
           END-IF.                                                              
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  NOT VSECSERV-RTN-OKAY                                            
           OR  EIBRESP NOT = DFHRESP(NORMAL)                                    
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE ERROR-SWITCH     TO ADDP-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO ADDP-MESSAGE-H                        
                 MOVE SPACES           TO ADDP-MESSAGE-O(4:66)                  
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO ADDP-MESSAGE-O(4:66)                  
               ELSE                                                             
                 MOVE ERROR-SWITCH     TO DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE SPACES           TO DETL-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO DETL-MESSAGE-O                        
               END-IF                                                           
             END-IF                                                             
             GO TO R35-EXIT                                                     
           END-IF.                                                              
                                                                                
           SET  BSTCNTL-UPDATED        TO TRUE.                                 
                                                                                
      * update resource description                                             
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           SET  VSECSERV-UPD-RESOURCE  TO TRUE.                                 
                                                                                
           EVALUATE TRUE ALSO TRUE                                              
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-USER     ALSO TS-RSC-FACILITY                          
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               MOVE TS-FAC-CODE        TO VSECUPD-FAC-TYPE                      
               MOVE TS-RESOURCE(9:)    TO VSECUPD-RES-NAME                      
               MOVE TS-RSC-DESC        TO VSECUPD-RES-DESC                      
                                                                                
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-GROUP                             
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               MOVE TS-RSC-DESC        TO VSECUPD-RES-DESC                      
                                                                                
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-TRANS                             
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               MOVE TS-RSC-DESC        TO VSECUPD-RES-DESC                      
                                                                                
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-USER                              
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-USER                              
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-USER                              
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-USER   TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               MOVE TS-RSC-DESC        TO VSECUPD-RES-DESC                      
                                                                                
           WHEN OTHER                                                           
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE '*'              TO ADDP-SELECT-O(IDX)                    
               ELSE                                                             
                 MOVE '*'              TO DETL-SELECT-O(IDX)                    
               END-IF                                                           
               GO TO R34-SKIPIT                                                 
           END-EVALUATE.                                                        
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE '*'              TO ADDP-SELECT-O(IDX)                    
               ELSE                                                             
                 MOVE '*'              TO DETL-SELECT-O(IDX)                    
               END-IF                                                           
           ELSE                                                                 
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE ERROR-SWITCH     TO ADDP-SELECT-L(IDX)                    
                 MOVE EXCRED           TO ADDP-MESSAGE-C                        
                 MOVE SPACES           TO ADDP-MESSAGE-O(4:66)                  
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO ADDP-MESSAGE-O(4:66)                  
               ELSE                                                             
                 MOVE ERROR-SWITCH     TO DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE SPACES           TO DETL-MESSAGE-O                        
                 STRING VSECSERV          DELIMITED BY SPACES                   
                   ': RC=' VSECSERV-RTN-CODE                                    
                     ', "' VSECSERV-RTN-MESG '"'                                
                   DELIMITED BY SIZE INTO DETL-MESSAGE-O                        
               END-IF                                                           
             END-IF                                                             
             GO TO R35-EXIT                                                     
           END-IF.                                                              
                                                                                
       R34-SKIPIT.                                                              
      * update ts record                                                        
           IF  TS-TOTL > ZERO                                                   
           AND SS-ITEM <= TS-TOTL                                               
               PERFORM Q34-TS-REWRITEQ                                          
           END-IF.                                                              
                                                                                
       R35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       R40-DELETE-PERMITS.                                                      
                                                                                
           MOVE LOW-VALUES             TO VSECSERV-COMMAREA.                    
           SET  VSECSERV-TRAN          TO TRUE.                                 
           SET VSECSERV-DEL-PERMISSION TO TRUE.                                 
                                                                                
           EVALUATE TRUE ALSO TRUE                                              
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-FACILITY ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               MOVE VSEC-SEL-RSRC(4:4) TO VSECUPD-FAC-TYPE                      
               MOVE VSEC-SEL-RSRC(9:)  TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-TRANS    ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-FACILITY                          
           WHEN VSEC-SEL-USER     ALSO TS-RSC-FACILITY                          
               SET  VSECUPD-RES-FACILITY TO TRUE                                
               MOVE TS-FAC-CODE        TO VSECUPD-FAC-TYPE                      
               MOVE TS-RESOURCE(9:)    TO VSECUPD-RES-NAME                      
               IF  VSEC-SEL-SUBGROUP                                            
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE VSEC-SEL-TYPE    TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-TRANS                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-TRANS                             
               SET  VSECUPD-RES-TRANS  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               IF  VSEC-SEL-SUBGROUP                                            
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE VSEC-SEL-TYPE    TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-GROUP    ALSO TS-RSC-USER                              
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-SUBGROUP                          
           WHEN VSEC-SEL-SUBGROUP ALSO TS-RSC-USER                              
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-RES-NAME                      
               IF  TS-RSC-SUBGROUP                                              
                 SET VSECUPD-PER-GROUP TO TRUE                                  
               ELSE                                                             
                 MOVE TS-RSC-TYPE      TO VSECUPD-PER-TYPE                      
               END-IF                                                           
               MOVE TS-RESOURCE        TO VSECUPD-PER-NAME                      
           WHEN VSEC-SEL-USER     ALSO TS-RSC-GROUP                             
           WHEN VSEC-SEL-USER     ALSO TS-RSC-SUBGROUP                          
               SET  VSECUPD-RES-GROUP  TO TRUE                                  
               MOVE TS-RESOURCE        TO VSECUPD-RES-NAME                      
               SET  VSECUPD-PER-USER   TO TRUE                                  
               MOVE VSEC-SEL-RSRC      TO VSECUPD-PER-NAME                      
           WHEN OTHER                                                           
               MOVE -1                 TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'Invalid combination of resource identifiers.'              
                                       TO DETL-MESSAGE-O                        
               GO TO R45-EXIT                                                   
           END-EVALUATE.                                                        
                                                                                
           PERFORM P00-BUILD-WINVSEC-RESID THRU P05-EXIT.                       
           PERFORM P14-SET-GENERIC-ACCESS THRU P16-EXIT.                        
           IF  NOT VSEC-APPLICTN-ADMINISTRATOR                                  
           AND NOT VSEC-SECTION-ADMINISTRATOR                                   
           AND NOT VSEC-GENERIC-ADMINISTRATOR                                   
           AND NOT QRY-ALTERABLE                                                
               IF  COMM-CURRRTN = 'ADDP'                                        
                 MOVE -1               TO ERROR-SWITCH                          
                                          ADDP-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO ADDP-MESSAGE-H                        
                 MOVE 'Not authorized for this action.'                         
                                       TO ADDP-MESSAGE-O                        
               ELSE                                                             
                 MOVE -1               TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
                 MOVE EXHREVRS         TO DETL-MESSAGE-H                        
                 MOVE 'Not authorized for this action.'                         
                                       TO DETL-MESSAGE-O                        
               END-IF                                                           
               GO TO R45-EXIT                                                   
           END-IF.                                                              
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM(VSECSERV)                                          
                     COMMAREA(VSECSERV-COMMAREA)                                
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  VSECSERV-RTN-OKAY                                                
           AND EIBRESP = DFHRESP(NORMAL)                                        
               MOVE '*'                TO DETL-SELECT-O(IDX)                    
           ELSE                                                                 
             IF  NOT VSECSERV-RTN-OKAY                                          
               MOVE -1                 TO ERROR-SWITCH                          
                                          DETL-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE SPACES             TO DETL-MESSAGE-O                        
               STRING VSECSERV            DELIMITED BY SPACES                   
                 ': RC=' VSECSERV-RTN-CODE                                      
                   ', "' VSECSERV-RTN-MESG '"'                                  
                 DELIMITED BY SIZE   INTO DETL-MESSAGE-O                        
             END-IF                                                             
             GO TO R45-EXIT                                                     
           END-IF.                                                              
                                                                                
           SET  BSTCNTL-UPDATED        TO TRUE.                                 
                                                                                
       R45-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *        
      ******************************************************************        
       COPY COMMLINK.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    TRANSFER TO THE CICS HELP SYSTEM                            *        
      ******************************************************************        
       COPY COMMHELP.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    PROGRAM ERRORS (UNEXPECTED)                                 *        
      ******************************************************************        
       COPY UNEXERRP.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    PROGRAM TERMINATION (NORMAL)                                *        
      ******************************************************************        
       Z00-VSEC-TERMINATION.                                                    
                                                                                
           EXEC CICS SET TERMINAL(EIBTRMID)                                     
                     UCTRAN                                                     
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.                  
                                                                                
      * the above copybook ends with a return to cics and GOBACK                
                                                                                

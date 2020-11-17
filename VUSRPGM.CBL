      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VUSRPGM.                                                  
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  MAY 2008.                                                 
       DATE-COMPILED.                                                           
/DLC8/ INSTALLATION.  WINSUPPLY GROUP SERVICES.                                 
       SECURITY.      INTERNAL, VIA WINSCRTY, IESCNTL, AND VSEC.                
/DLC8/*REMARKS.       VIEW/MAINTAIN VUSERID FILE.                               
      *                                                                         
      * any changes here may also require changes in program WLSNPGM            
      *                                       and in program VUPMPGM            
      *                                       and in program VUSRATTR           
      *                                       and in program VUSRAUDT.          
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 05/30/2008 DLC ORIGINAL PROGRAM.                                        
/DLC0/* 06/16/2008 DLC PROTECT PWD DISPLAY WITH BSM RESOURCE IDENTIFIER         
/DLC1/* 11/30/2009 DLC UPGRADE TO LATEST CICS COMMON INTERFACE.                 
/DLC2/* 01/28/2011 DLC ADD BUSINESS LOGIC CHECK ON FIELD CONTENT.               
/DLC3/* 09/25/2015 DLC USE NEW DAPSCO CODE OF "X" - EXTERNAL COMPANIES.         
/DLC4/* 08/08/2016 DLC EMPTY VOPERID FIELD SHOULD BE BLANK NOT LOW-VALUE        
/DLC4/*                AND MAKE LOW-VALUE SHOW ON LIST SCREEN AS PERIOD.        
/DLC5/* 05/07/2019 DLC FIX USER NAME FILTER TO NON-CASE-SENSITIVE.              
/DLC6/* 05/07/2019 DLC SUPPORT TRANSFER FROM VUPM BY USER ID.                   
/DLC7/* 05/20/2019 DLC SUPPORT TRANSFER OF USER ID TO PREVIOUS SCREEN.          
/DLC8/* 08/02/2019 DLC CONVERT FROM VOPERID TO VUSERID FILE.                    
/DLC8/*                MAINTAIN VOPERID FILE SIDE-BY-SIDE FOR NOW.              
/DLC9/* 09/05/2019 DLC EXPAND VUSERID FILE FOR ACTIVE DIRECTORY ATTRS.          
/DLCA/* 10/03/2019 DLC EXPAND VUSERID FILE FOR DAPTRMID ATTRS.                  
/DLCB/* 10/08/2019 DLC PROVIDE RED HIGHLIGHTS FOR AD ATTR. MISMATCHES.          
/DLCC/* 12/23/2019 DLC ELIMINATE VOPERID FILE NOW.                              
/DLCD/* 01/10/2020 DLC UNPROTECT AD ATTRIBUTES IF USER IS NOT REFRESHED.        
/DLCE/* 01/10/2020 DLC USERS AUTHORIZED TO SEE PASSWORDS SHOULD NOT SEE         
/DLCE/*                THEM FOR USERS WITH HIGHER SECURITY THAN THEM.           
/DLCF/* 01/31/2020 DLC PROVIDE SUGGESTIONS FOR AD ATTR. MISMATCHES.             
/DLCG/* 02/04/2020 DLC ADD WINSCRTY TEST OPTION TO LIST SCREEN.                 
/DLCH/* 02/04/2020 DLC ADD AD QUERY OPTION TO REFRESH ATTRIBUTES.               
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
      *                                                                         
      * FIELDS TO MANAGE PROGRAM AND MAP RESOURCE NAMES                         
         COPY COMMWORK       REPLACING =='TRAN'==    BY =='VUSR'==              
                                       =='PROGRAM'== BY =='VUSRPGM'==.          
         03  VUSRMS                    PIC  X(08)   VALUE 'VUSRMS  '.           
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.           
/DLC8/*  03  OPERMAP                   PIC  X(08)   VALUE 'OPERMAP '.           
/DLC8/   03  DETLMAP                   PIC  X(08)   VALUE 'DETLMAP '.           
/DLCG/   03  TESTWIN                   PIC  X(08)   VALUE 'TESTWIN '.           
/DLCH/   03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.           
/DLCG/   03  LISTXXX                   PIC  X(08)   VALUE 'LISTXXX '.           
      *                                                                         
      * FIELDS TO MANAGE REFERENCED FILE NAMES                                  
/DLC8/   03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.           
/DLC8/   03  VUSEROP                   PIC  X(08)   VALUE 'VUSEROP '.           
/DLCC/*  03  VOPERID                   PIC  X(08)   VALUE 'VOPERID '.           
/DLCC/*  03  VOPERPU                   PIC  X(08)   VALUE 'VOPERPU '.           
/DLCE/   03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.           
/DLCB/   03  COMASTR                   PIC  X(08)   VALUE 'COMASTR '.           
/DLCB/   03  COMASTA                   PIC  X(08)   VALUE 'COMASTA '.           
/DLCB/   03  COMASTI                   PIC  X(08)   VALUE 'COMASTI '.           
/DLCB/   03  COMASTW                   PIC  X(08)   VALUE 'COMASTW '.           
/DLCH/   03  IESLDUV                   PIC  X(08)   VALUE 'IESLDUV '.           
      *                                                                         
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES                      
/DLC8/   03  JOBSSELT                  PIC  X(08)   VALUE 'JOBSSELT'.           
/DLC8/   03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.           
      *                                                                         
      * FIELDS TO MANAGE ERROR HANDLING                                         
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.               
           88  NO-ERRORS-FOUND                      VALUE ZEROES.               
           88  ERROR-AT-CURSOR                      VALUE -1.                   
           88  ERRORS-FOUND                         VALUES -9 THRU -1.          
/DLC8/   03  RECORD-SWITCH             PIC  X(01)   VALUE 'N'.                  
/DLC8/     88  RECORD-NOT-SELECTED                  VALUE 'N'.                  
/DLC8/     88  RECORD-IS-SELECTED                   VALUE 'Y'.                  
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.        
/DLC8/   03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.        
/DLC8/   03  POS                       PIC S9(04)   BINARY VALUE ZEROES.        
/DLCH/   03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.        
/DLCE/   03  CNTL-RECL                 PIC S9(04)   BINARY VALUE ZEROES.        
      *                                                                         
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS           
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.        
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +06.           
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +18.           
      *                                                                         
/DLC8/   03  CHECK-USERID              PIC  X(08).                              
/DLCE/   03  CHECKED-USER              PIC  X(01).                              
/DLCE/     88  CHECKED-GENERAL                      VALUE '3'.                  
/DLCE/     88  CHECKED-SUPER                        VALUE '2'.                  
/DLCE/     88  CHECKED-ADMIN                        VALUE '1'.                  
/DLCH/   03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.               
/DLC8/   03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.               
/DLC8/   03  VAR-DATA                  PIC  X(98)   VALUE SPACES.               
      *                                                                         
      * TO CONVERT NUMERIC TO BINARY-CODED CHARACTER AND VICE VERSA             
/DLC8/   03  DOUBLE-WORD               PIC S9(18)   BINARY.                     
/DLC8/   03  EIGHT-BYTES               REDEFINES    DOUBLE-WORD.                
/DLC8/     05                          PIC  X(2).                               
/DLC8/     05  SIX-BYTES.                                                       
/DLC8/       07                        PIC  X(2).                               
/DLC8/       07  FULL-WORD             PIC S9(9)    BINARY.                     
/DLC8/       07  FOUR-BYTES            REDEFINES    FULL-WORD.                  
/DLC8/         09  HI-MSB              PIC  X.                                  
/DLC8/         09  THREE-BYTES.                                                 
/DLC8/           11  HI-LSB            PIC  X.                                  
/DLC8/           11  HALF-WORD         PIC S9(4)    BINARY.                     
/DLC8/           11  TWO-BYTES         REDEFINES    HALF-WORD.                  
/DLC8/             13  LO-MSB          PIC  X.                                  
/DLC8/             13  LO-LSB          PIC  X.                                  
      *                                                                         
      * TO CONVERT DATES AND TIMES                                              
         03  NUM-DATE                  PIC  9(8).                               
         03  WRK-DATE                  REDEFINES    NUM-DATE.                   
           05  DTE-GRMO                PIC  99.                                 
           05  DTE-GRDA                PIC  99.                                 
           05  DTE-GRYR                PIC  9999.                               
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.        
/DLC8/   03  WS-WRKDATE.                                                        
/DLC8/     05  WS-MODATE               PIC  9(02).                              
/DLC8/     05                          PIC  X.                                  
/DLC8/     05  WS-DADATE               PIC  9(02).                              
/DLC8/     05                          PIC  X.                                  
/DLC8/     05  WS-CYDATE               PIC  9(04).                              
         03  WS-WRKTIME.                                                        
           05  WS-HRSTIME              PIC  9(02).                              
           05                          PIC  X(03).                              
           05  WS-XXXTIME              PIC  X(03).                              
      *                                                                         
/DLC5/   03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.        
/DLC5/   03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.        
/DLC4/   03  NON-DISPLAY               PIC  X(65)   VALUE                       
/DLC4/                                X'000102030405060708090A0B0C0D0E0F        
/DLC4/-                                '101112131415161718191A1B1C1D1E1F        
/DLC4/-                                '202122232425262728292A2B2C2D2E2F        
/DLC4/-                            '303132333435363738393A3B3C3D3E3FFF'.        
/DLC4/   03  PERIODS-65                PIC  X(65)   VALUE                       
/DLC4/                                 '................................        
/DLC4/-                            '.................................'.         
                                                                                
/DLCH/ 01  VATTR-AREA.                                                          
/DLCH/   03  VUSRATTR                  PIC  X(08)   VALUE 'VUSRATTR'.           
/DLCH/ 01  VATTR-JCL.                                                           
/DLCH/   03  PIC X(40) VALUE '* $$ JOB JNM=VUSRATTR,DISP=D,CLASS=0    '.        
/DLCH/   03  PIC X(40) VALUE '* $$ LST DISP=D,CLASS=Z,PURGE=0         '.        
/DLCH/   03  PIC X(40) VALUE '// JOB    VUSRATTR   LDAP USER MAPPING  '.        
/DLCH/   03  PIC X(40) VALUE '// UPSI   1    /* forced refresh */'.             
/DLCH/   03  PIC X(40) VALUE '// EXEC   PGM=VUSRATTR,PARM=''&MFID''   '.        
/DLCH/   03  PIC X(40) VALUE '/* EOD                                  '.        
/DLCH/   03  PIC X(40) VALUE '/& EOJ                                  '.        
/DLCH/   03  PIC X(40) VALUE '* $$ EOJ                                '.        
/DLCH/ 01  VATTR-TABLE                 REDEFINES    VATTR-JCL.                  
/DLCH/   03  VATTR-ENTRY               PIC  X(40)   OCCURS 8.                   
                                                                                
/DLC0/ 01  VSECSERV-CONTROL.                                                    
/DLC0/   03  VSECSERV                  PIC  X(8)    VALUE 'VSECSERV'.           
/DLC0/   03  VSECSERV-RESOURCE         PIC  X(39)   VALUE                       
/DLC8/*                       'WINAPPS.SY.VOPERID.PASSWORD'.                    
/DLC8/                        'WINAPPS.SY.VUSERID.PASSWORD'.                    
/DLC0/ 01  VSECSERV-COMMAREA.                                                   
/DLC0/   COPY VSECSERV.                                                         
                                                                                
      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS                     
                                                                                
       01  CICSINFO-PARMS.                                                      
         COPY CICSINFO.                                                         
                                                                                
/DLCE/ COPY BITMAN.                                                             
                                                                                
       COPY HEXMAN.                                                             
                                                                                
       COPY LOGGING.                                                            
                                                                                
/DLCH/ COPY PRTMAN.                                                             
                                                                                
/DLCH/ COPY TXTMAN.                                                             
                                                                                
       COPY UNEXERRW.                                                           
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).                 
                                                                                
       COPY WILDCOMP.                                                           
                                                                                
/DLCG/ 01  WINSCRTY-CONTROL.                                                    
/DLCG/   03  WINSCRTY                  PIC  X(08)   VALUE 'WINSCRTY'.           
/DLCG/  02  WINSCRTY-PARMS.                                                     
/DLCG/   COPY WINSCRTY.                                                         
                                                                                
      /*****************************************************************        
      *    LINKAGE SECTION                                             *        
      ******************************************************************        
       LINKAGE SECTION.                                                         
                                                                                
      * VUSR CICS COMMUNICATION AREA                                            
       01  DFHCOMMAREA.                                                         
         COPY COMMAREA.                                                         
      *                                                                         
           05  VUSR-USERID             PIC  X(8).                               
           05  VUSR-OPERID             PIC  X(4).                               
           05  SAVE-FILTERS.                                                    
             07  SAVE-USERNM-FILTER    PIC  X(30).                              
             07  SAVE-CLASS-FILTER     PIC  X(2).                               
             07  SAVE-DAPSCO-FILTER    PIC  X(1).                               
             07  SAVE-IND-LIST-FILTER  PIC  X(8).                               
             07  SAVE-IND-AUTH-FILTER  PIC  X(8).                               
/DLC8/*DLC9  07  SAVE-AP-SEC-FILTER    PIC  X(2).                               
/DLC9/       07  SAVE-PRIME-CO-FILTER  PIC  X(5).                               
/DLC9/       07  SAVE-PRIME-IND-FILTER PIC  X(1).                               
/DLC9/       07  SAVE-WIN-UNIT-FILTER  PIC  X(10).                              
/DLC9/       07  SAVE-WIN-POS-FILTER   PIC  X(5).                               
/DLC9/       07  SAVE-WIN-ORG-FILTER   PIC  X(5).                               
/DLC9/       07  SAVE-WIN-FUNC-FILTER  PIC  X(3).                               
/DLC9/       07  SAVE-WIN-DIR-FILTER   PIC  X(1).                               
/DLCA/       07  SAVE-REGION-FILTER    PIC  X(2).                               
/DLCA/       07  SAVE-REMOTE-FILTER    PIC  X(3).                               
           05  VUSR-PASSWORD           PIC  X.                                  
             88  CAN-SEE-PASSWORD                   VALUE 'Y'.                  
             88  NOT-SEE-PASSWORD                   VALUE 'N'.                  
           05  VUSR-PROCESS            PIC  X.                                  
             88  ADD-PROCESS                        VALUE 'A'.                  
             88  UPD-PROCESS                        VALUE 'U'.                  
           05  CURRENT-USER            PIC  X(01).                              
             88  USER-IS-GENERAL                    VALUE '3'.                  
             88  USER-IS-SUPER                      VALUE '2'.                  
             88  USER-IS-ADMIN                      VALUE '1'.                  
      *                                                                         
/DLC8/     05  VUSR-LIST-TIOA          PIC  X(3752).                            
/DLCF/     05  VUSR-SAVE-TIOA          PIC  X(0750).                            
                                                                                
      * VUSR TEMP COMMUNICATION AREA                                            
       COPY COMMTEMP.                                                           
                                                                                
      * 3270 DATA STREAM CONTROL CHARACTERS                                     
       COPY IBM3270.                                                            
                                                                                
/DLC8/* VUSERID RECORD I/O AREA                                                 
/DLC8/ COPY VUSERID.                                                            
                                                                                
/DLCC/* VOPERID RECORD I/O AREA                                                 
/DLCC/*COPY VOPERID.                                                            
                                                                                
/DLCE/* IESCNTL RECORD I/O AREA                                                 
/DLCE/ COPY IESCNTL.                                                            
                                                                                
/DLCB/* COMASTR RECORD I/O AREA                                                 
/DLCB/ COPY COMASTR.                                                            
                                                                                
/DLCH/* IESLDUM RECORD I/O AREA                                                 
/DLCH/ COPY IESLDUM.                                                            
                                                                                
      * BMS TERMINAL I/O AREAS                                                  
       COPY VUSRMS.                                                             
                                                                                
      /*****************************************************************        
      *                                                                *        
      *    PROCEDURE DIVISION                                          *        
      *                                                                *        
      ******************************************************************        
       PROCEDURE DIVISION.                                                      
                                                                                
      ******************************************************************        
      *    PROGRAM INITIALIZATION                                      *        
      ******************************************************************        
       A00-VUSR-INITIALIZATION.                                                 
                                                                                
           COPY COMMENTR.                                                       
                                                                                
           EXEC CICS LINK                                                       
                     PROGRAM  ('CICSINFO')                                      
                     COMMAREA (CICSINFO-PARMS)                                  
           END-EXEC.                                                            
           MOVE CICS-FULLDATE          TO WS-WRKDATE.                           
           MOVE CICS-FULLTIME          TO WS-WRKTIME.                           
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.                             
                                                                                
           COPY COMMMAPT.                                                       
                                                                                
           COPY COMM3270.                                                       
                                                                                
           EXEC CICS HANDLE CONDITION                                           
                     DISABLED (Z70-VUSR-CLOSED)                                 
                     NOTOPEN  (Z70-VUSR-CLOSED)                                 
           END-EXEC.                                                            
                                                                                
           IF  COMM-CURRPGM = THIS-PGM                                          
      * psuedo-conversational program continuation                              
               GO TO B50-VUSR-ROUTINE-SELECTION                                 
           END-IF.                                                              
                                                                                
       B00-VUSR-FIRST-TIME-ONLY.                                                
                                                                                
           COPY COMMINIT.                                                       
                                                                                
/DLC1/     IF  COMM-CURRRTN = 'RETN'                                            
/DLC1/     AND COMM-SAVE-FUNC > SPACES                                          
/DLC1/* return from 2nd-level program (such as the HELP system)                 
/DLC1/         GO TO B50-VUSR-ROUTINE-SELECTION                                 
/DLC1/     END-IF.                                                              
                                                                                
      * the following executes one-time-only per user session                   
                                                                                
/DLC8/*    MOVE SPACES                 TO VUSR-OPERID.                          
/DLC8/*    EXEC CICS ASSIGN                                                     
/DLC8/*              OPID(VUSR-OPERID)                                          
/DLC8/*    END-EXEC.                                                            
                                                                                
/DLC8/*    EXEC CICS READ                                                       
/DLC8/*              DATASET(VOPERID)                                           
/DLC8/*              SET(ADDRESS OF VOPERID-RECORD)                             
/DLC8/*              RIDFLD(VUSR-OPERID)                                        
/DLC8/*              EQUAL                                                      
/DLC8/*              NOHANDLE                                                   
/DLC8/*    END-EXEC.                                                            
/DLC8/*    IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/*    OR  VOPERID-CLASS NOT = '99'                                         
                                                                                
/DLC8/     MOVE SPACES                 TO WINSCRTY-PARMS.                       
/DLC8/     EXEC CICS LINK                                                       
/DLC8/               PROGRAM(WINSCRTY)                                          
/DLC8/               COMMAREA(WINSCRTY-PARMS)                                   
/DLC8/     END-EXEC.                                                            
                                                                                
/DLC8/     IF  SCRTY-OLDCLS NOT = '99'                                          
               MOVE 1                  TO UNEX-MSGL                             
               STRING 'Sorry, not authorized to application.'                   
                 DELIMITED BY SIZE INTO UNEX-MSG WITH POINTER UNEX-MSGL         
               MOVE UNEX-IC            TO UNEX-MSG(UNEX-MSGL:1)                 
               GO TO X50-SEND-MSG                                               
           END-IF.                                                              
/DLC8/     MOVE SCRTY-USERID           TO CHECK-USERID.                         
                                                                                
/DLC6/*DLC8IF  VUSR-USERID > SPACES                                             
/DLC6/*DLC8    EXEC CICS READ                                                   
/DLC6/*DLC8              DATASET(VOPERPU)                                       
/DLC6/*DLC8              SET(ADDRESS OF VOPERID-RECORD)                         
/DLC6/*DLC8              RIDFLD(VUSR-USERID)                                    
/DLC6/*DLC8              GTEQ                                                   
/DLC6/*DLC8              NOHANDLE                                               
/DLC6/*DLC8    END-EXEC                                                         
/DLC6/*DLC8    IF  EIBRESP = DFHRESP(NORMAL)                                    
/DLC6/*DLC8        MOVE VOPERID-CODE   TO VUSR-OPERID                           
/DLC6/*DLC8    ELSE                                                             
/DLC6/*DLC8        MOVE DFHRESP(NORMAL) TO EIBRESP                              
/DLC6/*DLC8    END-IF                                                           
/DLC6/*DLC8    MOVE SPACES             TO VUSR-USERID                           
/DLC6/*DLC8END-IF.                                                              
                                                                                
/DLC0/     MOVE SPACES                 TO VSECSERV-HEADER.                      
/DLC0/     SET  VSECSERV-TRAN          TO TRUE.                                 
/DLC0/     SET  VSECSERV-CHK-SECURITY  TO TRUE.                                 
/DLC0/     MOVE VSECSERV-RESOURCE      TO VSECSERV-CGI-REQU.                    
/DLC0/     EXEC CICS LINK                                                       
/DLC0/               PROGRAM(VSECSERV)                                          
/DLC0/               COMMAREA(VSECSERV-HEADER)                                  
/DLC0/               NOHANDLE                                                   
/DLC0/     END-EXEC.                                                            
/DLC0/     IF  EIBRESP = DFHRESP(NORMAL)                                        
/DLC0/     AND VSECSERV-SEC-READABLE                                            
/DLC0/         SET  CAN-SEE-PASSWORD   TO TRUE                                  
/DLC0/     ELSE                                                                 
/DLC0/         SET  NOT-SEE-PASSWORD   TO TRUE                                  
/DLC0/     END-IF.                                                              
                                                                                
/DLCE/     PERFORM Q30-INITKEY-IESCNTL.                                         
/DLCE/     PERFORM Q36-READEQ-IESCNTL.                                          
/DLCE/     MOVE CHECKED-USER           TO CURRENT-USER.                         
                                                                                
      * continue first time initialization                                      
                                                                                
           IF  COMM-CURRRTN NOT = 'RETN'                                        
                              AND 'EXIT'                                        
               MOVE SPACES             TO VUSR-USERID                           
           END-IF.                                                              
                                                                                
      * set default initial filter value                                        
           MOVE '*'                    TO SAVE-USERNM-FILTER                    
                                          SAVE-CLASS-FILTER                     
                                          SAVE-DAPSCO-FILTER                    
                                          SAVE-IND-LIST-FILTER                  
                                          SAVE-IND-AUTH-FILTER                  
/DLC8/*DLC9                               SAVE-AP-SEC-FILTER.                   
/DLC9/                                    SAVE-PRIME-CO-FILTER                  
/DLC9/                                    SAVE-PRIME-IND-FILTER                 
/DLC9/                                    SAVE-WIN-UNIT-FILTER                  
/DLC9/                                    SAVE-WIN-POS-FILTER                   
/DLC9/                                    SAVE-WIN-ORG-FILTER                   
/DLC9/                                    SAVE-WIN-FUNC-FILTER                  
/DLC9/                                    SAVE-WIN-DIR-FILTER                   
/DLCA/                                    SAVE-REGION-FILTER                    
/DLCA/                                    SAVE-REMOTE-FILTER.                   
                                                                                
       B50-VUSR-ROUTINE-SELECTION.                                              
                                                                                
/DLC1/     IF  COMM-CURRRTN = 'RETN'                                            
/DLC1/     AND COMM-SAVE-FUNC > SPACES                                          
/DLC1/         MOVE COMM-CURRRTN       TO INIT-FLAG                             
/DLC1/         MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN                          
/DLC1/         MOVE INIT-FLAG          TO COMM-SAVE-FUNC                        
/DLC1/     END-IF.                                                              
                                                                                
           EVALUATE COMM-CURRRTN                                                
/DLCG/     WHEN 'TEST' GO TO E00-TEST-ROUTINE                                   
/DLC8/*    WHEN 'OPER' GO TO D00-OPER-ROUTINE                                   
/DLC8/     WHEN 'DETL' GO TO D00-DETL-ROUTINE                                   
           WHEN 'LIST' GO TO C00-LIST-ROUTINE                                   
           END-EVALUATE.                                                        
                                                                                
      /*****************************************************************        
      *    LIST ROUTINE                                                *        
      ******************************************************************        
       C00-LIST-ROUTINE.                                                        
                                                                                
           IF  ADDRESS OF LISTMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF LISTMAPI)                         
                         LENGTH   (LENGTH OF LISTMAPI)                          
                         INITIMG  (LOVALUE)                                     
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'LIST'                                        
/DLC1/     OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
/DLC1/         IF  COMM-SAVE-FUNC = INIT-FLAG                                   
/DLC1/             MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
/DLC1/             MOVE SPACES         TO COMM-SAVE-FUNC                        
/DLC1/             MOVE HIGH-VALUES    TO INIT-FLAG                             
/DLC1/         END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
/DLC8/*        IF  COMM-CURRRTN > SPACES                                        
/DLC8/         IF  COMM-CURRRTN = 'RETN'                                        
/DLC8/*            MOVE VUSR-SAVE-TIOA TO LISTMAPO                              
/DLC8/             MOVE VUSR-LIST-TIOA TO LISTMAPO                              
/DLC8/             MOVE LOW-VALUES     TO VUSR-LIST-TIOA                        
/DLC8/*            PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP                
/DLC8/*            IF  VUSR-OPERID > SPACES                                     
/DLC8/*                MOVE VUSR-OPERID TO LIST-OPERID-SKIP-O                   
/DLC8/*                MOVE SPACES     TO VUSR-OPERID                           
/DLC8/*                MOVE 'RETN'     TO COMM-CURRRTN                          
/DLC8/*                GO TO C30-LIST-PROCESS                                   
/DLC8/*            END-IF                                                       
/DLC8/*            GO TO C50-LIST-DISPLAY                                       
               END-IF                                                           
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP                    
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
       C10-LIST-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP(LISTMAP)                                               
                     INTO(LISTMAPI)                                             
                     MAPSET(VUSRMS)                                             
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
/DLC8/         MOVE JOBSSELT           TO COMM-LINKPGM                          
/DLC8/         GO TO Z90-VUSR-TERMINATION                                       
                                                                                
           WHEN AIDPFK03                                                        
               GO TO Z90-VUSR-TERMINATION                                       
                                                                                
/DLC1/     WHEN AIDPFK01                                                        
/DLC1/         MOVE LISTMAPI           TO VUSR-LIST-TIOA                        
/DLC1/         GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
           WHEN AIDPFK06                                                        
           WHEN AIDPFK07                                                        
           WHEN AIDPFK08                                                        
           WHEN AIDPFK09                                                        
               GO TO C30-LIST-PROCESS                                           
                                                                                
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
                                                                                
      * convert cursor position to row and column coordinates                   
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * if any selections changed, go rebuild screen                            
/DLC8/     IF  LIST-USERID-SKIPTO-L    > ZERO                                   
/DLC8/     OR  LIST-USERID-SKIPTO-F    = FLDMODFY OR FLDCURSM                   
/DLC8/     OR  LIST-OPERID-SKIPTO-L    > ZERO                                   
/DLC8/     OR  LIST-OPERID-SKIPTO-F    = FLDMODFY OR FLDCURSM                   
           OR  LIST-USERNM-FILTER-L    > ZERO                                   
           OR  LIST-USERNM-FILTER-F    = FLDMODFY OR FLDCURSM                   
           OR  LIST-CLASS-FILTER-L     > ZERO                                   
           OR  LIST-CLASS-FILTER-F     = FLDMODFY OR FLDCURSM                   
           OR  LIST-DAPSCO-FILTER-L    > ZERO                                   
           OR  LIST-DAPSCO-FILTER-F    = FLDMODFY OR FLDCURSM                   
           OR  LIST-IND-LIST-FILTER-L  > ZERO                                   
           OR  LIST-IND-LIST-FILTER-F  = FLDMODFY OR FLDCURSM                   
           OR  LIST-IND-AUTH-FILTER-L  > ZERO                                   
           OR  LIST-IND-AUTH-FILTER-F  = FLDMODFY OR FLDCURSM                   
/DLC8/*DLC9OR  LIST-AP-SEC-FILTER-L    > ZERO                                   
/DLC8/*DLC9OR  LIST-AP-SEC-FILTER-F    = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-PRIME-CO-FILTER-L  > ZERO                                   
/DLC9/     OR  LIST-PRIME-CO-FILTER-F  = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-PRIME-IND-FILTER-L > ZERO                                   
/DLC9/     OR  LIST-PRIME-IND-FILTER-F = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-WIN-UNIT-FILTER-L  > ZERO                                   
/DLC9/     OR  LIST-WIN-UNIT-FILTER-F  = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-WIN-POS-FILTER-L   > ZERO                                   
/DLC9/     OR  LIST-WIN-POS-FILTER-F   = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-WIN-ORG-FILTER-L   > ZERO                                   
/DLC9/     OR  LIST-WIN-ORG-FILTER-F   = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-WIN-FUNC-FILTER-L  > ZERO                                   
/DLC9/     OR  LIST-WIN-FUNC-FILTER-F  = FLDMODFY OR FLDCURSM                   
/DLC9/     OR  LIST-WIN-DIR-FILTER-L   > ZERO                                   
/DLC9/     OR  LIST-WIN-DIR-FILTER-F   = FLDMODFY OR FLDCURSM                   
/DLCA/     OR  LIST-REGION-FILTER-L    > ZERO                                   
/DLCA/     OR  LIST-REGION-FILTER-F    = FLDMODFY OR FLDCURSM                   
/DLCA/     OR  LIST-REMOTE-FILTER-L    > ZERO                                   
/DLCA/     OR  LIST-REMOTE-FILTER-F    = FLDMODFY OR FLDCURSM                   
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * scroll to top or bottom of the list                                     
           IF  EIBAID = AIDPFK06 OR AIDPFK09                                    
/DLC8/         PERFORM Q20-INITKEY-VUSERID                                      
               IF  EIBAID = AIDPFK06                                            
/DLC8/*            MOVE LOW-VALUES     TO VOPERID-CODE                          
/DLC8/             MOVE LOW-VALUES     TO VUSER-KEY                             
/DLC8/*            PERFORM Q21-STARTBR-VOPERID                                  
/DLC8/             PERFORM Q21-STARTBR-VUSERID                                  
/DLC8/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                 IF  EIBRESP = DFHRESP(NOTFND)                            
/DLC8/                   MOVE 'No user profiles found.'                         
/DLC8/                                 TO LIST-MESSAGE-O                        
/DLC8/                   GO TO C50-LIST-DISPLAY                                 
/DLC8/                 END-IF                                                   
/DLC8/                 GO TO C70-LIST-ERRORS                                    
/DLC8/             END-IF                                                       
/DLC8/             PERFORM Q23-READNEXT-VUSERID THRU Q23-CHK-FILTER             
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                           AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
/DLC8/*            PERFORM P10-SELECT-NEXT-MATCH THRU P15-EXIT                  
/DLC8/*            IF  VOPERID-CODE = LIST-OPERID-I(1)                          
/DLC8/             IF  VUSER-KEY = LIST-USERID-I(1)                             
                   OR  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.'                          
                                       TO LIST-MESSAGE-O                        
                   END-IF                                                       
               ELSE                                                             
/DLC8/*            MOVE HIGH-VALUES    TO VOPERID-CODE                          
/DLC8/             MOVE HIGH-VALUES    TO VUSER-KEY                             
/DLC8/*            PERFORM Q21-STARTBR-VOPERID                                  
/DLC8/             PERFORM Q21-STARTEQ-VUSERID                                  
/DLC8/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                 IF  EIBRESP = DFHRESP(NOTFND)                            
/DLC8/                   MOVE 'No user profiles found.'                         
/DLC8/                                 TO LIST-MESSAGE-O                        
/DLC8/                   GO TO C50-LIST-DISPLAY                                 
/DLC8/                 END-IF                                                   
/DLC8/                 GO TO C70-LIST-ERRORS                                    
/DLC8/             END-IF                                                       
/DLC8/             PERFORM Q24-READPREV-VUSERID THRU Q24-CHK-FILTER             
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                           AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
/DLC8/*            PERFORM P20-SELECT-PREV-MATCH THRU P25-EXIT                  
/DLC8/*            IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/*                GO TO C70-LIST-ERRORS                                    
/DLC8/*            END-IF                                                       
/DLC8/*            IF  VOPERID-CODE = LIST-OPERID-I(1)                          
/DLC8/             IF  VUSER-KEY = LIST-USERID-I(1)                             
/DLC8/             OR  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.'                           
                                       TO LIST-MESSAGE-O                        
                   END-IF                                                       
               END-IF                                                           
/DLC8/         MOVE VUSER-KEY          TO LIST-USERID-O(1)                      
/DLC8/*        PERFORM Q25-ENDBR-VOPERID                                        
/DLC8/         PERFORM Q25-ENDBR-VUSERID                                        
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * adjust row and column coordinates for header/detail screens             
           SUBTRACT LISTHEAD         FROM ROW.                                  
                                                                                
      * scroll to previous or next page                                         
           IF  EIBAID = AIDPFK07 OR AIDPFK08                                    
/DLC8/         PERFORM Q20-INITKEY-VUSERID                                      
/DLC8/*        MOVE LIST-OPERID-I(1) TO VOPERID-CODE                            
/DLC8/         MOVE LIST-USERID-I(1) TO VUSER-KEY                               
               IF  EIBAID = AIDPFK07                                            
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-OPERID-I(ROW) <= SPACES                             
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = LISTROWS - ROW                             
                   END-IF                                                       
/DLC8/*            PERFORM Q21-STARTBR-VOPERID                                  
/DLC8/             PERFORM Q21-STARTBR-VUSERID                                  
/DLC8/*            IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/*                GO TO C70-LIST-ERRORS                                    
/DLC8/*            END-IF                                                       
/DLC8/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                 IF  EIBRESP = DFHRESP(NOTFND)                            
/DLC8/                   MOVE 'No user profiles found.'                         
/DLC8/                                 TO LIST-MESSAGE-O                        
/DLC8/                   GO TO C50-LIST-DISPLAY                                 
/DLC8/                 END-IF                                                   
/DLC8/                 GO TO C70-LIST-ERRORS                                    
/DLC8/             END-IF                                                       
/DLC8/*            PERFORM Q23-READNEXT-VOPERID                                 
/DLC8/             PERFORM Q23-READNEXT-VUSERID                                 
/DLC8/*            PERFORM P20-SELECT-PREV-MATCH THRU P25-EXIT                  
/DLC8/             PERFORM Q24-READPREV-VUSERID THRU Q24-CHK-FILTER             
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the first entry.'                          
                                       TO LIST-MESSAGE-O                        
/DLC8/*                PERFORM Q23-READNEXT-VOPERID                             
/DLC8/                 PERFORM Q23-READNEXT-VUSERID THRU Q23-CHK-FILTER         
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO C70-LIST-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               ELSE                                                             
                   IF  ROW <= 1 OR >= LISTROWS                                  
                   OR  LIST-OPERID-I(ROW) <= SPACES                             
                       COMPUTE IDX = LISTROWS                                   
                   ELSE                                                         
                       COMPUTE IDX = ROW - 1                                    
                   END-IF                                                       
/DLC8/*            PERFORM Q21-STARTBR-VOPERID                                  
/DLC8/             PERFORM Q21-STARTBR-VUSERID                                  
/DLC8/*            IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/*                GO TO C70-LIST-ERRORS                                    
/DLC8/*            END-IF                                                       
/DLC8/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLC8/                 IF  EIBRESP = DFHRESP(NOTFND)                            
/DLC8/                   MOVE 'No user profiles found.'                         
/DLC8/                                 TO LIST-MESSAGE-O                        
/DLC8/                   GO TO C50-LIST-DISPLAY                                 
/DLC8/                 END-IF                                                   
/DLC8/                 GO TO C70-LIST-ERRORS                                    
/DLC8/             END-IF                                                       
/DLC8/*            PERFORM P10-SELECT-NEXT-MATCH THRU P15-EXIT                  
/DLC8/             PERFORM Q23-READNEXT-VUSERID THRU Q23-CHK-FILTER             
                     VARYING IDX FROM IDX BY -1                                 
                       UNTIL IDX < ZERO                                         
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP NOT = DFHRESP(NORMAL)                            
                                 AND DFHRESP(ENDFILE)                           
                       GO TO C70-LIST-ERRORS                                    
                   END-IF                                                       
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
                       MOVE 'This is the last entry.'                           
                                       TO LIST-MESSAGE-O                        
/DLC8/*                PERFORM P20-SELECT-PREV-MATCH THRU P25-EXIT              
/DLC8/                 PERFORM Q24-READPREV-VUSERID THRU Q24-CHK-FILTER         
                       IF  EIBRESP NOT = DFHRESP(NORMAL)                        
                           GO TO C70-LIST-ERRORS                                
                       END-IF                                                   
                   END-IF                                                       
               END-IF                                                           
/DLC8/*        MOVE VOPERID-CODE       TO LIST-OPERID-O(1)                      
/DLC8/         MOVE VUSER-KEY          TO LIST-USERID-O(1)                      
/DLC8/*        PERFORM Q25-ENDBR-VOPERID                                        
/DLC8/         PERFORM Q25-ENDBR-VUSERID                                        
               GO TO C50-LIST-DISPLAY                                           
           END-IF.                                                              
                                                                                
      * check if default selection needs to be made                             
           IF  ROW >= 1 AND <= LISTROWS                                         
               PERFORM WITH TEST BEFORE                                         
                 VARYING IDX FROM 1 BY 1                                        
                   UNTIL IDX > LISTROWS                                         
                      OR LIST-SELECT-I(IDX) > SPACE                             
               END-PERFORM                                                      
               IF  IDX > LISTROWS                                               
                   MOVE 'U'            TO LIST-SELECT-O(ROW)                    
               END-IF                                                           
           END-IF.                                                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
           MOVE DFHRESP(NORMAL)        TO EIBRESP.                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
                                                                                
               IF  LIST-SELECT-I(IDX) = 'D'                                     
                   PERFORM C40-LIST-UPDATES THRU C45-EXIT                       
               END-IF                                                           
                                                                                
      * end of detail loop                                                      
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
                                                                                
/DLC8/     IF  LIST-USERID-I(IDX) NOT > SPACES                                  
/DLC8/         MOVE LOW-VALUES         TO LIST-SELECT-I(IDX)                    
/DLC8/         GO TO C45-EXIT                                                   
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/* confirm delete processing                                               
/DLC8/     COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 1.              
/DLC8/     EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC.                    
/DLC8/     MOVE SPACES                 TO MULTI-EDIT-MSG.                       
/DLC8/     MOVE 1                      TO LEN.                                  
/DLC8/     STRING 'Delete this user identification entry?  '                    
/DLC8/            'Press F2 to confirm.'  DELIMITED BY SIZE                     
/DLC8/         INTO MULTI-EDIT-MSG   WITH POINTER LEN.                          
/DLC8/     SUBTRACT 1                FROM LEN.                                  
/DLC8/     PERFORM V10-VERIFY-DELETE THRU V15-EXIT.                             
/DLC8/     IF  EIBAID  NOT = AIDPFK02                                           
/DLC8/     OR  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/         MOVE LOW-VALUES         TO LIST-SELECT-I(IDX)                    
/DLC8/         GO TO C45-EXIT                                                   
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     PERFORM Q20-INITKEY-VUSERID.                                         
/DLC8/*    MOVE LIST-OPERID-I(IDX)     TO VOPERID-CODE.                         
/DLC8/     MOVE LIST-USERID-I(IDX)     TO VUSER-KEY.                            
/DLC8/*    PERFORM Q26-READUP-VOPERID.                                          
/DLC8/     PERFORM Q26-READUP-VUSERID.                                          
           EVALUATE EIBRESP                                                     
           WHEN DFHRESP(NORMAL)                                                 
/DLC8/*        PERFORM Q28-DELETE-VOPERID                                       
/DLC8/         PERFORM Q28-DELETE-VUSERID                                       
/DLC8/*        IF  EIBRESP = DFHRESP(NORMAL)                                    
/DLC8/         IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   MOVE 'Delete failed.'                                        
                                       TO LIST-USERNM-O(IDX)                    
                   GO TO C45-EXIT                                               
               END-IF                                                           
               MOVE '*'                TO LIST-SELECT-O(IDX)                    
           WHEN OTHER                                                           
               MOVE -1                 TO ERROR-SWITCH                          
                                          LIST-SELECT-L(IDX)                    
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               MOVE 'Specified user id was not found.'                          
                                       TO LIST-MESSAGE-O                        
               GO TO C45-EXIT                                                   
           END-EVALUATE.                                                        
                                                                                
/DLC8/* maintain VOPERID file side-by-side                                      
/DLC8/*DLCCPERFORM Q30-INITKEY-VOPERID.                                         
/DLC8/*DLCCMOVE LIST-OPERID-I(IDX)     TO VOPERID-CODE.                         
/DLC8/*DLCCPERFORM Q36-READUP-VOPERID.                                          
/DLC8/*DLCCIF  EIBRESP = DFHRESP(NORMAL)                                        
/DLC8/*DLCC    PERFORM Q38-DELETE-VOPERID                                       
/DLC8/*DLCCELSE                                                                 
/DLC8/*DLCC    MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLC8/*DLCCEND-IF.                                                              
                                                                                
       C45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       C50-LIST-DISPLAY.                                                        
                                                                                
           MOVE 'LIST'                 TO COMM-CURRRTN.                         
                                                                                
      * skip if heading selections changed                                      
/DLC8/*    IF  LIST-OPERID-SKIP-I > SPACES                                      
/DLC8/*    OR  LIST-USERID-SKIP-I > SPACES                                      
/DLC8/*    OR  LIST-OPERID-I(1) NOT > SPACES                                    
/DLC8/     IF  LIST-USERID-SKIPTO-I > SPACES                                    
/DLC8/     OR  LIST-OPERID-SKIPTO-I > SPACES                                    
/DLC8/     OR  LIST-USERID-I(1) NOT > SPACES                                    
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
      * ... drill-down with key data to internal function ...                   
               WHEN 'U'                                                         
               WHEN 'A'                                                         
                 MOVE LIST-SELECT-I(ROW) TO VUSR-PROCESS                        
/DLC8/*          MOVE LIST-OPERID-I(ROW) TO VUSR-OPERID                         
/DLC8/           MOVE LIST-USERID-I(ROW) TO VUSR-USERID                         
                 MOVE '*'              TO LIST-SELECT-O(ROW)                    
                 MOVE LISTMAPI         TO VUSR-SAVE-TIOA                        
                 GO TO D00-DETL-ROUTINE                                         
/DLCH/* ... transfer key data to internal function ...                          
/DLCH/         WHEN 'Q'                                                         
/DLCH/             IF  LIST-USERID-I(ROW) NOT > SPACES                          
/DLCH/               MOVE LOW-VALUES   TO LIST-SELECT-O(ROW)                    
/DLCH/               GO TO C60-LIST-DETAIL                                      
/DLCH/             END-IF                                                       
/DLCH/             MOVE LIST-USERID-I(ROW)                                      
/DLCH/                                 TO VUSR-USERID                           
/DLCH/             PERFORM Q50-INITKEY-IESLDUM                                  
/DLCH/             MOVE VUSR-USERID    TO LDUM-MFUSRID                          
/DLCH/             PERFORM Q56-READEQ-IESLDUV                                   
/DLCH/             IF  EIBRESP = DFHRESP(NOTFND)                                
/DLCH/               MOVE 'The user is not mapped.'                             
/DLCH/                                 TO MULTI-EDIT-MSG                        
/DLCH/               CALL MULTMESG  USING DFHEIBLK MULTMESG                     
/DLCH/                                    ERROR-SWITCH                          
/DLCH/                                    LIST-SELECT-L(ROW)                    
/DLCH/                                    LIST-MESSAGE-L                        
/DLCH/                                    MULTI-EDIT-MSG                        
/DLCH/               GO TO C80-LIST-OUTPUT                                      
/DLCH/             END-IF                                                       
/DLCH/             PERFORM Q20-INITKEY-VUSERID                                  
/DLCH/             MOVE VUSR-USERID    TO VUSER-KEY                             
/DLCH/             PERFORM Q26-READEQ-VUSERID                                   
/DLCH/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLCH/               GO TO C70-LIST-ERRORS                                      
/DLCH/             END-IF                                                       
/DLCH/             IF  NOT VUSER-DO-REFRESH                                     
/DLCH/               COMPUTE EIBCPOSN = (LISTHEAD + ROW - 1)                    
/DLCH/                                * SCRNWDTH + 1                            
/DLCH/               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC           
/DLCH/               MOVE SPACES       TO MULTI-EDIT-MSG                        
/DLCH/               MOVE 1            TO LEN                                   
/DLCH/               STRING 'User id not flagged for refresh.  '                
/DLCH/                      'Press F2 to refresh.' DELIMITED BY SIZE            
/DLCH/                   INTO MULTI-EDIT-MSG WITH POINTER LEN                   
/DLCH/               SUBTRACT 1      FROM LEN                                   
/DLCH/               PERFORM V10-VERIFY-DELETE THRU V15-EXIT                    
/DLCH/               IF  EIBAID  NOT = AIDPFK02                                 
/DLCH/               OR  EIBRESP NOT = DFHRESP(NORMAL)                          
/DLCH/                   MOVE LOW-VALUES TO LIST-SELECT-I(ROW)                  
/DLCH/                   GO TO C50-LIST-DISPLAY                                 
/DLCH/               END-IF                                                     
/DLCH/             END-IF                                                       
/DLCH/             MOVE '*'            TO LIST-SELECT-O(ROW)                    
/DLCH/             MOVE LISTMAPI       TO VUSR-LIST-TIOA                        
/DLCH/             GO TO H00-QUERY-ROUTINE                                      
/DLCG/* ... drill-down with key data to internal function ...                   
/DLCG/         WHEN 'T'                                                         
/DLCG/             IF  LIST-USERID-I(ROW) NOT > SPACES                          
/DLCG/                 MOVE LOW-VALUES TO LIST-SELECT-O(ROW)                    
/DLCG/                 GO TO C60-LIST-DETAIL                                    
/DLCG/             END-IF                                                       
/DLCG/             MOVE LIST-USERID-I(ROW)                                      
/DLCG/                                 TO VUSR-USERID                           
/DLCG/             MOVE '*'            TO LIST-SELECT-O(ROW)                    
/DLCG/             MOVE LISTMAPI       TO VUSR-LIST-TIOA                        
/DLCG/             GO TO E00-TEST-ROUTINE                                       
/DLC7/* ... transfer key data to previous application ...                       
/DLC7/         WHEN 'X'                                                         
/DLC7/*DLC8      IF  LIST-OPERID-I(ROW) NOT > SPACES                            
/DLC8/           IF  LIST-USERID-I(ROW) NOT > SPACES                            
/DLC7/           OR  COMM-SAVE-FUNC NOT > SPACES                                
/DLC7/               MOVE LOW-VALUES TO LIST-SELECT-O(ROW)                      
/DLC7/               GO TO C60-LIST-DETAIL                                      
/DLC7/           END-IF                                                         
/DLC7/           MOVE 'RETN'           TO COMM-TRANSID                          
/DLC7/           MOVE LIST-USERID-I(ROW)                                        
/DLC7/                                 TO VUSR-USERID                           
/DLC7/           COMPUTE LINK-LEN = LENGTH OF COMM-HEADER                       
/DLC7/                            + LENGTH OF COMM-SHARED                       
/DLC7/                            + LENGTH OF VUSR-USERID                       
/DLC7/           GO TO T00-LINK-TRANSFER                                        
               WHEN OTHER                                                       
                 MOVE     -1           TO ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                 MOVE  EXHREVRS        TO LIST-MESSAGE-H                        
                 MOVE 'Unknown option selection.'                               
                                       TO LIST-MESSAGE-O                        
               END-EVALUATE                                                     
               GO TO C80-LIST-OUTPUT                                            
           END-IF.                                                              
                                                                                
       C60-LIST-DETAIL.                                                         
                                                                                
/DLC8/     PERFORM Q80-SAVE-FILTERS THRU Q85-EXIT.                              
/DLC8/     IF  ERRORS-FOUND                                                     
/DLC8/         GO TO C80-LIST-OUTPUT                                            
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     PERFORM Q20-INITKEY-VUSERID.                                         
                                                                                
      * skip to selected key value                                              
/DLC8/     EVALUATE TRUE                                                        
/DLC8/*    IF  LIST-OPERID-SKIP-I > SPACES                                      
/DLC8/     WHEN LIST-USERID-SKIPTO-L > ZEROES                                   
/DLC8/     AND  LIST-USERID-SKIPTO-I > SPACES                                   
/DLC8/*        MOVE LOW-VALUES         TO LIST-USERID-SKIP-O                    
/DLC8/         MOVE LOW-VALUES         TO LIST-OPERID-SKIPTO-O                  
/DLC8/*        MOVE LIST-OPERID-SKIP-I TO VOPERID-CODE                          
/DLC8/         MOVE LIST-USERID-SKIPTO-I TO VUSER-KEY                           
/DLC8/*        PERFORM Q21-STARTBR-VOPERID                                      
/DLC8/         PERFORM Q21-STARTBR-VUSERID                                      
/DLC8/         IF  EIBRESP = DFHRESP(NOTFND)                                    
/DLC8/             MOVE HIGH-VALUES    TO VUSER-KEY                             
/DLC8/             PERFORM Q21-STARTEQ-VUSERID                                  
/DLC8/             PERFORM Q24-READPREV-VUSERID THRU Q24-CHK-FILTER             
/DLC8/         END-IF                                                           
/DLC8/         IF  EIBRESP = DFHRESP(NORMAL)                                    
/DLC8/             PERFORM Q23-READNEXT-VUSERID THRU Q23-CHK-FILTER             
/DLC8/         END-IF                                                           
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
/DLC8/*        IF  VOPERID-CODE = LIST-OPERID-SKIP-I                            
/DLC8/*            MOVE VOPERID-CODE   TO LIST-OPERID-O(1)                      
/DLC8/*        ELSE                                                             
/DLC8/         IF  LIST-USERID-SKIPTO-I < VUSER-KEY                             
/DLC8/*            PERFORM Q24-READPREV-VOPERID                                 
/DLC8/             PERFORM Q24-READPREV-VUSERID THRU Q24-CHK-FILTER             
                     VARYING IDX FROM 1 BY 1 UNTIL IDX > 2                      
                          OR EIBRESP NOT = DFHRESP(NORMAL)                      
                   IF  EIBRESP = DFHRESP(ENDFILE)                               
/DLC8/*              MOVE LOW-VALUES   TO LIST-OPERID-O(1)                      
                     MOVE 'This is the first entry.'                            
                                       TO LIST-MESSAGE-O                        
/DLC8/*            ELSE                                                         
/DLC8/*              MOVE VOPERID-CODE TO LIST-OPERID-O(1)                      
                   END-IF                                                       
               END-IF                                                           
/DLC8/*        PERFORM Q25-ENDBR-VOPERID                                        
/DLC8/         PERFORM Q25-ENDBR-VUSERID                                        
/DLC8/*        IF  COMM-CURRRTN NOT = 'LIST'                                    
/DLC8/*            MOVE LOW-VALUES     TO LIST-OPERID-SKIP-O                    
/DLC8/*        ELSE                                                             
/DLC8/*            MOVE '*'            TO SAVE-USERNM-FILTER                    
/DLC8/*                                   SAVE-CLASS-FILTER                     
/DLC8/*                                   SAVE-DAPSCO-FILTER                    
/DLC8/*                                   SAVE-IND-LIST-FILTER                  
/DLC8/*                                   SAVE-IND-AUTH-FILTER                  
/DLC8/*        END-IF                                                           
/DLC8/*        GO TO C50-LIST-DISPLAY                                           
/DLC8/*    END-IF.                                                              
/DLC8/*    IF  LIST-USERID-SKIP-I > SPACES                                      
/DLC8/     WHEN LIST-OPERID-SKIPTO-L > ZEROES                                   
/DLC8/     AND  LIST-OPERID-SKIPTO-I > SPACES                                   
/DLC8/*        MOVE LOW-VALUES         TO LIST-OPERID-SKIP-O                    
/DLC8/         MOVE LOW-VALUES         TO LIST-USERID-SKIPTO-O                  
/DLC8/*        MOVE LIST-USERID-SKIP-I TO VOPERID-USERID                        
/DLC8/         MOVE LIST-OPERID-SKIPTO-I TO VUSER-OPID                          
/DLC8/*        PERFORM Q36-READGE-VOPERPU                                       
/DLC8/         PERFORM Q26-READGE-VUSEROP                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*                      AND DFHRESP(DUPKEY)                                
                             AND DFHRESP(NOTFND)                                
                   GO TO C70-LIST-ERRORS                                        
               END-IF                                                           
/DLC8/*        IF  EIBRESP NOT = DFHRESP(ENDFILE)                               
/DLC8/*            MOVE VOPERID-CODE   TO LIST-OPERID-O(1)                      
/DLC8/*        ELSE                                                             
/DLC8/         IF  EIBRESP = DFHRESP(NOTFND)                                    
                   MOVE DFHRESP(NORMAL) TO EIBRESP                              
/DLC8/*            MOVE LOW-VALUES     TO LIST-OPERID-O(1)                      
                   MOVE 'Operator id not found.'                                
                                       TO LIST-MESSAGE-O                        
               END-IF                                                           
/DLC8/*        MOVE '*'                TO SAVE-USERNM-FILTER                    
/DLC8/*                                   SAVE-CLASS-FILTER                     
/DLC8/*                                   SAVE-DAPSCO-FILTER                    
/DLC8/*                                   SAVE-IND-LIST-FILTER                  
/DLC8/*                                   SAVE-IND-AUTH-FILTER                  
/DLC8/*        GO TO C50-LIST-DISPLAY                                           
/DLC8/*    END-IF.                                                              
/DLC8/     WHEN VUSR-USERID > SPACES                                            
/DLC8/         MOVE VUSR-USERID        TO VUSER-KEY                             
/DLC8/     WHEN LIST-USERID-I(1) > SPACES                                       
/DLC8/         MOVE LIST-USERID-I(1)   TO VUSER-KEY                             
/DLC8/     END-EVALUATE.                                                        
                                                                                
/DLC8/*    MOVE LOW-VALUES             TO LIST-OPERID-SKIP-O                    
/DLC8/*                                   LIST-USERID-SKIP-O.                   
                                                                                
/DLC1/*DLC8PERFORM Q20-INITKEY-VOPERID.                                         
/DLC8/*    MOVE LIST-OPERID-I(1)       TO VOPERID-CODE.                         
/DLC8/*    PERFORM Q21-STARTBR-VOPERID.                                         
/DLC8/     PERFORM Q21-STARTBR-VUSERID.                                         
/DLC8/     IF  EIBRESP = DFHRESP(NOTFND)                                        
/DLC8/         MOVE LOW-VALUES         TO VUSER-KEY                             
/DLC8/         PERFORM Q21-STARTBR-VUSERID                                      
/DLC8/         IF  EIBRESP = DFHRESP(NOTFND)                                    
/DLC8/             MOVE DFHRESP(NORMAL) TO EIBRESP                              
/DLC8/             MOVE 2              TO IDX                                   
/DLC8/             GO TO C65-LIST-BLANKS                                        
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
                                                                                
           MOVE SPACES                 TO VUSR-USERID                           
                                          VUSR-OPERID.                          
                                                                                
      * BUILD OUTPUT DETAIL LINES                                               
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING IDX FROM 1 BY 1                                            
               UNTIL IDX > LISTROWS                                             
                  OR EIBRESP NOT = DFHRESP(NORMAL)                              
                                                                                
/DLC8/*        PERFORM P10-SELECT-NEXT-MATCH THRU P15-EXIT                      
/DLC8/         PERFORM Q23-READNEXT-VUSERID THRU Q23-CHK-FILTER                 
               IF  EIBRESP = DFHRESP(NORMAL)                                    
                                                                                
      * BUILD DETAIL LINE OUTPUT HERE                                           
/DLC8/             MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)               
/DLC8/*            MOVE LOW-VALUES     TO LIST-SELECT-O(IDX)                    
/DLC8/*            MOVE FLDASBRF       TO LIST-OPERID-A(IDX)                    
/DLC8/*            MOVE VOPERID-CODE   TO LIST-OPERID-O(IDX)                    
/DLC8/             MOVE VUSER-KEY      TO LIST-USERID-O(IDX)                    
/DLC8/*            MOVE VOPERID-USERID TO LIST-USERID-O(IDX)                    
/DLC8/             MOVE VUSER-OPID     TO LIST-OPERID-O(IDX)                    
/DLC8/*            MOVE VOPERID-NAME   TO LIST-USERNM-O(IDX)                    
/DLC8/             MOVE VUSER-NAME     TO LIST-USERNM-O(IDX)                    
/DLC8/*            MOVE VOPERID-CLASS  TO LIST-CLASS-O(IDX)                     
/DLC8/             MOVE VUSER-CLASS    TO LIST-CLASS-O(IDX)                     
/DLC8/*            MOVE VOPERID-DAPSCO TO LIST-DAPSCO-O(IDX)                    
/DLC8/             MOVE VUSER-DAPSCO   TO LIST-DAPSCO-O(IDX)                    
/DLC8/*            MOVE VOPERID-IND-LIST   TO LIST-IND-LIST-O(IDX)              
/DLC8/             MOVE VUSER-IND-LIST TO LIST-IND-LIST-O(IDX)                  
/DLC4/             INSPECT LIST-IND-LIST-O(IDX)                                 
/DLC4/                 CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-IND-AUTH   TO LIST-IND-AUTH-O(IDX)              
/DLC8/             MOVE VUSER-IND-AUTH TO LIST-IND-AUTH-O(IDX)                  
/DLC4/             INSPECT LIST-IND-AUTH-O(IDX)                                 
/DLC4/                 CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-DFQ-SELECT TO LIST-DFQ-SELECT-O(IDX)            
/DLC8/*DLC9        MOVE VUSER-DFQ-SELECT TO LIST-DFQ-SELECT-O(IDX)              
/DLC4/*DLC9        INSPECT LIST-DFQ-SELECT-O(IDX)                               
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-DFQ-BASE   TO LIST-DFQ-BASEN-O(IDX)             
/DLC8/*DLC9        MOVE VUSER-DFQ-BASE TO LIST-DFQ-BASEN-O(IDX)                 
/DLC4/*DLC9        INSPECT LIST-DFQ-BASEN-O(IDX)                                
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-DFQ-REPT   TO LIST-DFQ-REPTN-O(IDX)             
/DLC8/*DLC9        MOVE VUSER-DFQ-REPT TO LIST-DFQ-REPTN-O(IDX)                 
/DLC4/*DLC9        INSPECT LIST-DFQ-REPTN-O(IDX)                                
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-DFQ-SORT   TO LIST-DFQ-SORT-O(IDX)              
/DLC8/*DLC9        MOVE VUSER-DFQ-SORT TO LIST-DFQ-SORT-O(IDX)                  
/DLC4/*DLC9        INSPECT LIST-DFQ-SORT-O(IDX)                                 
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-NEWC-DAYX  TO LIST-NEWC-DAYS-O(IDX)             
/DLC8/*DLC9        MOVE VUSER-NEWC-DAYX TO LIST-NEWC-DAYS-O(IDX)                
/DLC4/*DLC9        INSPECT LIST-NEWC-DAYS-O(IDX)                                
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-NEWC-SORT  TO LIST-NEWC-SORT-O(IDX)             
/DLC8/*DLC9        MOVE VUSER-NEWC-SORT TO LIST-NEWC-SORT-O(IDX)                
/DLC4/*DLC9        INSPECT LIST-NEWC-SORT-O(IDX)                                
/DLC4/*DLC9            CONVERTING NON-DISPLAY TO PERIODS-65                     
/DLC8/*            MOVE VOPERID-AP-SEC     TO LIST-AP-SEC-O(IDX)                
/DLC8/*DLC9        MOVE VUSER-AP-SEC   TO LIST-AP-SEC-O(IDX)                    
                                                                                
/DLC9/             MOVE VUSER-PRIMARY-CO   TO LIST-PRIMARY-CO-O(IDX)            
/DLC9/             MOVE VUSER-PRIMARY-IND  TO LIST-PRIMARY-IND-O(IDX)           
/DLC9/             MOVE VUSER-WIN-FUNCTION TO LIST-WIN-FUNCTION-O(IDX)          
/DLC9/             MOVE VUSER-WIN-UNIT     TO LIST-WIN-UNIT-O(IDX)              
/DLC9/             MOVE VUSER-WIN-ORG      TO LIST-WIN-ORG-O(IDX)               
/DLC9/             MOVE VUSER-WIN-POSITION TO LIST-WIN-POSITION-O(IDX)          
/DLC9/             MOVE VUSER-WIN-DIRECTOR TO LIST-WIN-DIRECTOR-O(IDX)          
/DLCB/             PERFORM P40-CHECK-AD-ATTRIBUTES THRU P45-EXIT                
                                                                                
/DLCA/             MOVE VUSER-SVC-REGION   TO LIST-SVC-REGION-O(IDX)            
/DLCA/             MOVE VUSER-RPT-REMOTE   TO LIST-RPT-REMOTE-O(IDX)            
               ELSE                                                             
                   MOVE  -1            TO LIST-SELECT-L(IDX)                    
                   SUBTRACT 1        FROM IDX                                   
               END-IF                                                           
           END-PERFORM.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
/DLC8/*    PERFORM WITH TEST BEFORE                                             
/DLC8/*      VARYING IDX FROM IDX BY 1                                          
/DLC8/*        UNTIL IDX > LISTROWS                                             
/DLC8/*      MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
/DLC8/*      MOVE FLDASDRK             TO LIST-SELECT-A(IDX)                    
/DLC8/*    END-PERFORM.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(ENDFILE)                                   
/DLC8/         PERFORM C65-LIST-BLANKS                                          
               GO TO C70-LIST-ERRORS                                            
           END-IF.                                                              
                                                                                
/DLC8/*    PERFORM Q25-ENDBR-VOPERID.                                           
/DLC8/     PERFORM Q25-ENDBR-VUSERID.                                           
                                                                                
/DLC8/*    IF  EIBRESP = DFHRESP(NORMAL)                                        
/DLC8/*        GO TO C80-LIST-OUTPUT                                            
/DLC8/*    END-IF.                                                              
                                                                                
/DLC8/ C65-LIST-BLANKS.                                                         
                                                                                
      * HANDLE UNUSED DETAIL LINES                                              
                                                                                
/DLC8/     PERFORM WITH TEST BEFORE                                             
/DLC8/       VARYING IDX FROM IDX BY 1                                          
/DLC8/         UNTIL IDX > LISTROWS                                             
/DLC8/       MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)               
/DLC8/       MOVE FLDASDRK             TO LIST-SELECT-A(IDX)                    
/DLC8/     END-PERFORM.                                                         
                                                                                
       C70-LIST-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO LIST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO LIST-MESSAGE-O                        
/DLC8/         EXEC CICS SYNCPOINT ROLLBACK END-EXEC                            
           END-IF.                                                              
                                                                                
       C80-LIST-OUTPUT.                                                         
                                                                                
           MOVE MAPTITLE               TO LIST-COMPANY-O.                       
                                                                                
/DLC8/*    EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
/DLC8/*    EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
/DLC8/*              FULLDATE(LIST-SYSDATE-O)                                   
/DLC8/*              DATESEP('/')                                               
/DLC8/*              TIME(WS-WRKTIME)                                           
/DLC8/*              TIMESEP(':')                                               
/DLC8/*    END-EXEC.                                                            
/DLC8/*    IF  WS-HRSTIME > 12                                                  
/DLC8/*        MOVE ' PM'              TO WS-XXXTIME                            
/DLC8/*        SUBTRACT 12           FROM WS-HRSTIME                            
/DLC8/*    ELSE                                                                 
/DLC8/*        IF  WS-HRSTIME = 12                                              
/DLC8/*            MOVE ' PM'          TO WS-XXXTIME                            
/DLC8/*        ELSE                                                             
/DLC8/*            MOVE ' AM'          TO WS-XXXTIME                            
/DLC8/*            IF  WS-HRSTIME = 00                                          
/DLC8/*                MOVE 12         TO WS-HRSTIME                            
/DLC8/*            END-IF                                                       
/DLC8/*        END-IF                                                           
/DLC8/*    END-IF.                                                              
/DLC8/     MOVE WS-WRKDATE             TO LIST-SYSDATE-O.                       
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.                       
                                                                                
/DLC8/*    MOVE EIBTRMID               TO LIST-TERMID-O.                        
/DLC8/     MOVE SPACES                 TO LIST-NETNAME-O.                       
/DLC8/     EXEC CICS ASSIGN NETNAME(LIST-NETNAME-O) END-EXEC.                   
/DLC8/     IF  LIST-NETNAME-I NOT > SPACES                                      
/DLC8/         STRING 'Term: ' EIBTRMID                                         
/DLC8/             DELIMITED BY SIZE INTO LIST-NETNAME-O                        
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/*    MOVE SAVE-USERNM-FILTER        TO LIST-USERNM-FILTER-O.              
/DLC8/*    MOVE SAVE-CLASS-FILTER         TO LIST-CLASS-FILTER-O.               
/DLC8/*    MOVE SAVE-DAPSCO-FILTER        TO LIST-DAPSCO-FILTER-O.              
/DLC8/*    MOVE SAVE-IND-LIST-FILTER      TO LIST-IND-LIST-FILTER-O.            
/DLC8/*    MOVE SAVE-IND-AUTH-FILTER      TO LIST-IND-AUTH-FILTER-O.            
                                                                                
           IF  COMM-SAVE-FUNC <= SPACES                                         
               MOVE '= Test security.'    TO LIST-TEST-TEXT-O                   
               MOVE FLDASDRK              TO LIST-XFER-OPTION-A                 
                                             LIST-XFER-TEXT-A                   
           END-IF.                                                              
                                                                                
       C85-LIST-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP      (LISTMAP)                                 
                             DATAONLY                                           
                             FROM     (LISTMAPO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP      (LISTMAP)                                 
                             DATAONLY                                           
                             FROM     (LISTMAPO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP      (LISTMAP)                                     
                         FROM     (LISTMAPO)                                    
                         MAPSET   (VUSRMS)                                      
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       C90-LIST-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA     (LISTMAPI)                                        
           END-EXEC.                                                            
                                                                                
/DLC8/*    COPY COMMRETN.                                                       
/DLC8/     GO TO Z80-VUSR-RETURN.                                               
                                                                                
      /*****************************************************************        
      *    DETL ROUTINE                                                *        
      ******************************************************************        
       D00-DETL-ROUTINE.                                                        
                                                                                
           IF  ADDRESS OF DETLMAPI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF DETLMAPI)                         
                         LENGTH   (LENGTH OF DETLMAPI)                          
                         INITIMG  (LOVALUE)                                     
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
/DLC8/*    IF  COMM-CURRRTN NOT = 'OPER'                                        
/DLC8/     IF  COMM-CURRRTN NOT = 'DETL'                                        
/DLC1/     OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
/DLC1/         IF  COMM-SAVE-FUNC = INIT-FLAG                                   
/DLC8/             MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
/DLC1/             MOVE SPACES         TO COMM-SAVE-FUNC                        
/DLC8/             MOVE HIGH-VALUES    TO INIT-FLAG                             
/DLC1/         END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
/DLC8/         IF  COMM-CURRRTN = 'RETN'                                        
/DLC8/             MOVE VUSR-SAVE-TIOA TO DETLMAPO                              
/DLC8/             MOVE LOW-VALUES     TO VUSR-SAVE-TIOA                        
/DLC8/         END-IF                                                           
/DLC8/*        PERFORM D80-DETL-OUTPUT                                          
/DLC8/*        EXEC CICS SEND                                                   
/DLC8/*                  MAP(DETLMAP)                                           
/DLC8/*                  FROM(DETLMAPO)                                         
/DLC8/*                  MAPSET(VUSRMS)                                         
/DLC8/*                  TERMINAL                                               
/DLC8/*                  ERASE                                                  
/DLC8/*                  NOHANDLE                                               
/DLC8/*        END-EXEC                                                         
/DLC8/         PERFORM D80-DETL-OUTPUT THRU D85-DETL-SENDMAP                    
               GO TO D50-DETL-DISPLAY                                           
           END-IF.                                                              
                                                                                
       D10-DETL-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP      (DETLMAP)                                         
                     INTO     (DETLMAPI)                                        
                     MAPSET   (VUSRMS)                                          
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO D70-DETL-ERRORS                                            
           END-IF.                                                              
                                                                                
       D20-DETL-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
/DLC8/         MOVE JOBSSELT           TO COMM-LINKPGM                          
/DLC8/         GO TO Z90-VUSR-TERMINATION                                       
                                                                                
           WHEN AIDPFK03                                                        
/DLC8/         MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC                        
/DLC8/         MOVE 'RETN'             TO COMM-CURRRTN                          
/DLC8/         MOVE DETL-USERID-I      TO VUSR-USERID                           
               GO TO C00-LIST-ROUTINE                                           
                                                                                
/DLC1/     WHEN AIDPFK01                                                        
/DLC8/         MOVE DETLMAPI           TO VUSR-SAVE-TIOA                        
/DLC1/         GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDENTER                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
/DLC8/*        MOVE EXHREVRS           TO OPER-MESSAGE-H                        
/DLC8/         MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'That attention key has no current action assigned.'        
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO DETL-MESSAGE-O                        
               GO TO D80-DETL-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
/DLC8/*        MOVE EXHREVRS           TO OPER-MESSAGE-H                        
/DLC8/         MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               MOVE 'That function key has no current action assigned.'         
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO DETL-MESSAGE-O                        
               GO TO D80-DETL-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       D30-DETL-PROCESS.                                                        
                                                                                
      * convert cursor position to row and column coordinates                   
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.                
           ADD  1                      TO ROW COL.                              
                                                                                
      * PERFORM DETAIL FIELD EDITING HERE                                       
                                                                                
/DLC1/*DLC8PERFORM Q20-INITKEY-VOPERID.                                         
/DLC8/     PERFORM Q20-INITKEY-VUSERID.                                         
                                                                                
/DLC8/     MOVE DFHRESP(NORMAL)        TO EIBRESP.                              
                                                                                
           IF  ADD-PROCESS                                                      
/DLC8/*        IF  OPER-OPERID-I NOT > SPACES                                   
/DLC8/         IF  DETL-USERID-I NOT > SPACES                                   
/DLC8/*            MOVE -1             TO OPER-OPERID-L                         
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC8/*            MOVE 'Operator identifier is missing.'                       
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/             MOVE 'User identifier is required.'                          
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-USERID-L                         
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
                   GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
/DLC8/*        MOVE OPER-OPERID-I      TO VOPERID-CODE                          
/DLC8/         MOVE DETL-USERID-I      TO VUSER-KEY                             
/DLC8/*        PERFORM Q26-READEQ-VOPERID                                       
/DLC8/         PERFORM Q26-READEQ-VUSERID                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
                             AND DFHRESP(NOTFND)                                
                   GO TO D70-DETL-ERRORS                                        
               END-IF                                                           
               IF  EIBRESP NOT = DFHRESP(NOTFND)                                
/DLC8/*            MOVE -1             TO OPER-OPERID-L                         
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC8/*            MOVE 'Operator identifier already exists.'                   
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/             MOVE 'User identifier already exists.'                       
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-USERID-L                         
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
                   GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC8/*    MOVE VUSR-OPERID            TO VOPERID-CODE.                         
/DLC8/     MOVE VUSR-USERID            TO VUSER-KEY.                            
/DLC8/*    PERFORM Q26-READEQ-VOPERID.                                          
/DLC8/     PERFORM Q26-READEQ-VUSERID.                                          
                                                                                
/DLC8/*    IF  OPER-USERNM-I NOT > SPACES                                       
/DLC8/     IF  DETL-USERNM-I NOT > SPACES                                       
/DLC8/*    AND VOPERID-NAME  NOT > SPACES                                       
/DLC8/     AND VUSER-NAME    NOT > SPACES                                       
/DLC8/*        MOVE -1                 TO OPER-USERNM-L                         
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*        MOVE EXHREVRS           TO OPER-MESSAGE-H                        
/DLC8/*        MOVE 'User name is missing.'                                     
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/         MOVE 'User name is required.'                                    
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/         CALL MULTMESG        USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-USERNM-L                         
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*        GO TO D80-DETL-OUTPUT                                            
           END-IF.                                                              
                                                                                
/DLC8/*    IF  VOPERID-USERID > SPACES                                          
/DLC8/*    OR (OPER-USERID-L > ZEROES                                           
/DLC8/*    OR  OPER-USERID-F = FLDMODFY)                                        
/DLC8/*    AND OPER-USERID-I > SPACES                                           
/DLC8/     IF  VUSER-OPID     > SPACES                                          
/DLC8/     OR (DETL-OPERID-L > ZEROES                                           
/DLC8/     OR  DETL-OPERID-F = FLDMODFY)                                        
/DLC8/     AND DETL-OPERID-I > SPACES                                           
/DLC8/*        IF  OPER-USERID-I > SPACES                                       
/DLC8/         IF  DETL-OPERID-I > SPACES                                       
/DLC8/*            MOVE OPER-USERID-I  TO VOPERID-USERID                        
/DLC8/             MOVE DETL-OPERID-I  TO VUSER-OPID                            
               END-IF                                                           
/DLC8/*        PERFORM Q36-READEQ-VOPERPU                                       
/DLC8/         PERFORM Q26-READEQ-VUSEROP                                       
               IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*                      AND DFHRESP(DUPKEY)                                
                             AND DFHRESP(NOTFND)                                
                   GO TO D70-DETL-ERRORS                                        
               END-IF                                                           
               IF  ADD-PROCESS                                                  
               AND EIBRESP NOT = DFHRESP(NOTFND)                                
               OR  NOT ADD-PROCESS                                              
               AND (EIBRESP    = DFHRESP(DUPKEY)                                
/DLC8/*             OR VOPERID-CODE NOT = OPER-OPERID-I)                        
/DLC8/              OR VUSER-KEY NOT = DETL-USERID-I)                           
               THEN                                                             
/DLC8/*            MOVE -1             TO OPER-USERID-L                         
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC8/*            MOVE 'User identifier already exists.'                       
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/             String 'Operator identifier already '                        
/DLC8/                    'in use by '    DELIMITED BY SIZE                     
/DLC8/                    VUSER-KEY       DELIMITED BY SPACE                    
/DLC8/                    '.'             DELIMITED BY SIZE                     
/DLC8/                               INTO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-OPERID-L                         
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
/DLC8/         ELSE                                                             
/DLC8/*            MOVE VUSR-OPERID    TO VOPERID-CODE                          
/DLC8/             MOVE VUSR-USERID    TO VUSER-KEY                             
/DLC8/*            PERFORM Q26-READEQ-VOPERID                                   
/DLC8/             PERFORM Q26-READEQ-VUSERID                                   
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-CLASS-L > ZEROES                                            
/DLC8/     IF  DETL-CLASS-L > ZEROES                                            
/DLC8/*    OR  OPER-CLASS-F = FLDMODFY                                          
/DLC8/     OR  DETL-CLASS-F = FLDMODFY                                          
/DLC8/*        IF  OPER-CLASS-I NOT NUMERIC                                     
/DLC8/         IF  DETL-CLASS-I NOT NUMERIC                                     
/DLC8/*            MOVE -1             TO OPER-CLASS-L                          
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
                   MOVE 'Classification must be ''00'' to ''99''.'              
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-CLASS-L                          
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
/DLC8/         ELSE                                                             
/DLC8/*            MOVE OPER-CLASS-I   TO VOPERID-CLASS                         
/DLC8/             MOVE DETL-CLASS-I   TO VUSER-CLASS                           
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DAPSCO-L > ZEROES                                           
/DLC8/     IF  DETL-DAPSCO-L > ZEROES                                           
/DLC8/*    OR  OPER-DAPSCO-F = FLDMODFY                                         
/DLC8/     OR  DETL-DAPSCO-F = FLDMODFY                                         
/DLC8/*        MOVE OPER-DAPSCO-I      TO VOPERID-DAPSCO                        
/DLC8/         MOVE DETL-DAPSCO-I      TO VUSER-DAPSCO                          
/DLC8/*        IF  NOT VOPERID-DAPSCO-CODES                                     
/DLC8/*        AND NOT VOPERID-AREA-LEADER                                      
/DLC8/*        AND NOT VOPERID-WISE-USER-OR-AC                                  
/DLC3/*DLC8    AND NOT VOPERID-EXTERNAL-USER                                    
/DLC8/         IF  NOT VUSER-DAPSCO-CODES                                       
/DLC8/         AND NOT VUSER-AREA-LEADER                                        
/DLC8/         AND NOT VUSER-WISE-USER-OR-AC                                    
/DLC8/         AND NOT VUSER-EXTERNAL-USER                                      
/DLC8/*            MOVE -1             TO OPER-DAPSCO-L                         
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
                   MOVE 'DAPSCO code must be one of those shown.'               
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-DAPSCO-L                         
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC2/*DLC8IF  VOPERID-WISE-USER-OR-AC                                          
/DLC8/     IF  VUSER-WISE-USER-OR-AC                                            
/DLC2/*DLC8    IF  VOPERID-CLASS > '21'                                         
/DLC8/         IF  VUSER-CLASS > '21'                                           
/DLC2/*DLC8        MOVE -1             TO OPER-CLASS-L                          
/DLC2/*DLC8                               ERROR-SWITCH                          
/DLC2/*DLC8        MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC2/             MOVE 'WISE users cannot be above class ''21''.'              
/DLC2/*DLC8                            TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-CLASS-L                          
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC2/*DLC8        GO TO D80-DETL-OUTPUT                                        
/DLC2/         END-IF                                                           
/DLC2/     END-IF.                                                              
                                                                                
/DLC2/*DLC8IF  OPER-IND-LIST-L > ZEROES                                         
/DLC8/     IF  DETL-IND-LIST-L > ZEROES                                         
/DLC2/*DLC8OR  OPER-IND-LIST-F = FLDMODFY                                       
/DLC8/     OR  DETL-IND-LIST-F = FLDMODFY                                       
/DLC2/*DLC8     MOVE OPER-IND-LIST-I   TO VOPERID-IND-LIST                      
/DLC8/          MOVE DETL-IND-LIST-I   TO VUSER-IND-LIST                        
/DLC2/     END-IF.                                                              
                                                                                
/DLC2/*DLC8IF  VOPERID-WISE-USER-OR-AC                                          
/DLC8/     IF  VUSER-WISE-USER-OR-AC                                            
/DLC2/*DLC8    IF  VOPERID-IND-LIST(1:3) NOT NUMERIC                            
/DLC8/         IF  VUSER-IND-LIST(1:3) NOT NUMERIC                              
/DLC2/*DLC8    OR  VOPERID-IND-LIST(4:5) > SPACES                               
/DLC8/         OR  VUSER-IND-LIST(4:5) > SPACES                                 
/DLC2/*DLC8        MOVE -1             TO OPER-IND-LIST-L                       
/DLC2/*DLC8                               ERROR-SWITCH                          
/DLC2/*DLC8        MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC2/             MOVE 'WISE users require local company number in indu        
/DLC8/-                'stry code field.' TO MULTI-EDIT-MSG                     
/DLC2/*DLC8            'stry code field.' TO OPER-MESSAGE-O                     
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-IND-LIST-L                       
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC2/*DLC8        GO TO D80-DETL-OUTPUT                                        
/DLC2/         END-IF                                                           
/DLC2/     END-IF.                                                              
                                                                                
/DLC2/*DLC8IF  VOPERID-AREA-LEADER                                              
/DLC8/     IF  VUSER-AREA-LEADER                                                
/DLC2/*DLC8    IF  VOPERID-IND-LIST(1:1) NOT = '*'                              
/DLC8/         IF  VUSER-IND-LIST(1:1) NOT = '*'                                
/DLC2/*DLC8    OR  VOPERID-IND-LIST(2:3) NOT > SPACES                           
/DLC8/         OR  VUSER-IND-LIST(2:3) NOT > SPACES                             
/DLC2/*DLC8    OR  VOPERID-IND-LIST(5:4) > SPACES                               
/DLC8/         OR  VUSER-IND-LIST(5:4) > SPACES                                 
/DLC2/*DLC8        MOVE -1             TO OPER-IND-LIST-L                       
/DLC2/*DLC8                               ERROR-SWITCH                          
/DLC2/*DLC8        MOVE EXHREVRS       TO OPER-MESSAGE-H                        
/DLC2/             MOVE 'AL requires area in industry code field.'              
/DLC2/*DLC8                            TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-IND-LIST-L                       
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC2/*DLC8        GO TO D80-DETL-OUTPUT                                        
/DLC2/         END-IF                                                           
/DLC2/     END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-BASEN-L > ZEROES                                        
/DLC8/     IF  DETL-DFQ-BASEN-L > ZEROES                                        
/DLC8/*    OR  OPER-DFQ-BASEN-F = FLDMODFY                                      
/DLC8/     OR  DETL-DFQ-BASEN-F = FLDMODFY                                      
/DLC8/*        IF  OPER-DFQ-BASEN-L > ZEROES                                    
/DLC8/         IF  DETL-DFQ-BASEN-L > ZEROES                                    
/DLC8/*        AND OPER-DFQ-BASEN-I NOT NUMERIC                                 
/DLC8/         AND DETL-DFQ-BASEN-I NOT NUMERIC                                 
/DLC8/*            MOVE -1             TO OPER-DFQ-BASEN-L                      
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
                   MOVE 'Default Base no. must be numeric.'                     
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO DETL-MESSAGE-O                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-DFQ-BASEN-L                      
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-REPTN-L > ZEROES                                        
/DLC8/     IF  DETL-DFQ-REPTN-L > ZEROES                                        
/DLC8/*    OR  OPER-DFQ-REPTN-F = FLDMODFY                                      
/DLC8/     OR  DETL-DFQ-REPTN-F = FLDMODFY                                      
/DLC8/*        IF  OPER-DFQ-REPTN-L > ZEROES                                    
/DLC8/         IF  DETL-DFQ-REPTN-L > ZEROES                                    
/DLC8/*        AND OPER-DFQ-REPTN-I NOT NUMERIC                                 
/DLC8/         AND DETL-DFQ-REPTN-I NOT NUMERIC                                 
/DLC8/*            MOVE -1             TO OPER-DFQ-REPTN-L                      
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
                   MOVE 'Default Rept no. must be numeric.'                     
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-DFQ-REPTN-L                      
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-NEWC-DAYS-L > ZEROES                                        
/DLC8/     IF  DETL-NEWC-DAYS-L > ZEROES                                        
/DLC8/*    OR  OPER-NEWC-DAYS-F = FLDMODFY                                      
/DLC8/     OR  DETL-NEWC-DAYS-F = FLDMODFY                                      
/DLC8/*        IF  OPER-NEWC-DAYS-L > ZEROES                                    
/DLC8/         IF  DETL-NEWC-DAYS-L > ZEROES                                    
/DLC8/*        AND OPER-NEWC-DAYS-I NOT NUMERIC                                 
/DLC8/         AND DETL-NEWC-DAYS-I NOT NUMERIC                                 
/DLC8/*            MOVE -1             TO OPER-NEWC-DAYS-L                      
/DLC8/*                                   ERROR-SWITCH                          
/DLC8/*            MOVE EXHREVRS       TO OPER-MESSAGE-H                        
                   MOVE 'NewC days must be numeric.'                            
/DLC8/*                                TO OPER-MESSAGE-O                        
/DLC8/                                 TO MULTI-EDIT-MSG                        
/DLC8/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLC8/                                    ERROR-SWITCH                          
/DLC8/                                    DETL-NEWC-DAYS-L                      
/DLC8/                                    DETL-MESSAGE-L                        
/DLC8/                                    MULTI-EDIT-MSG                        
/DLC8/*            GO TO D80-DETL-OUTPUT                                        
               END-IF                                                           
           END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-SVC-REGION-L > ZEROES                                       
/DLCA/     OR  DETL-SVC-REGION-F = FLDMODFY                                     
/DLCA/         IF  DETL-SVC-REGION-L > ZEROES                                   
/DLCA/         AND DETL-SVC-REGION-I NOT NUMERIC                                
/DLCA/             MOVE 'Service region must be numeric.'                       
/DLCA/                                 TO MULTI-EDIT-MSG                        
/DLCA/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCA/                                    ERROR-SWITCH                          
/DLCA/                                    DETL-SVC-REGION-L                     
/DLCA/                                    DETL-MESSAGE-L                        
/DLCA/                                    MULTI-EDIT-MSG                        
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-ALT-REGION-L > ZEROES                                       
/DLCA/     OR  DETL-ALT-REGION-F = FLDMODFY                                     
/DLCA/         IF  DETL-ALT-REGION-L > ZEROES                                   
/DLCA/         AND DETL-ALT-REGION-I NOT NUMERIC                                
/DLCA/             MOVE 'Alternate region must be numeric.'                     
/DLCA/                                 TO MULTI-EDIT-MSG                        
/DLCA/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCA/                                    ERROR-SWITCH                          
/DLCA/                                    DETL-ALT-REGION-L                     
/DLCA/                                    DETL-MESSAGE-L                        
/DLCA/                                    MULTI-EDIT-MSG                        
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-RPT-REMOTE-L > ZEROES                                       
/DLCA/     OR  DETL-RPT-REMOTE-F = FLDMODFY                                     
/DLCA/         IF  DETL-RPT-REMOTE-L > ZEROES                                   
/DLCA/         AND DETL-RPT-REMOTE-I NOT NUMERIC                                
/DLCA/             MOVE 'Report remote must be numeric.'                        
/DLCA/                                 TO MULTI-EDIT-MSG                        
/DLCA/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCA/                                    ERROR-SWITCH                          
/DLCA/                                    DETL-RPT-REMOTE-L                     
/DLCA/                                    DETL-MESSAGE-L                        
/DLCA/                                    MULTI-EDIT-MSG                        
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-ALT-REMOTE-L > ZEROES                                       
/DLCA/     OR  DETL-ALT-REMOTE-F = FLDMODFY                                     
/DLCA/         IF  DETL-ALT-REMOTE-L > ZEROES                                   
/DLCA/         AND DETL-ALT-REMOTE-I NOT NUMERIC                                
/DLCA/             MOVE 'Alternate remote must be numeric.'                     
/DLCA/                                 TO MULTI-EDIT-MSG                        
/DLCA/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCA/                                    ERROR-SWITCH                          
/DLCA/                                    DETL-ALT-REMOTE-L                     
/DLCA/                                    DETL-MESSAGE-L                        
/DLCA/                                    MULTI-EDIT-MSG                        
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-DFT-PRINTER-L > ZEROES                                      
/DLCA/     OR  DETL-DFT-PRINTER-F = FLDMODFY                                    
/DLCA/         MOVE DETL-DFT-PRINTER-I TO UNEX-VARIABLE                         
/DLCA/         PERFORM Q202-SET-LENGTH                                          
/DLCA/         IF  DETL-DFT-PRINTER-L > ZEROES                                  
/DLCA/         AND DETL-DFT-PRINTER-I > SPACES                                  
/DLCA/         AND UNEX-LENG NOT = 4                                            
/DLCA/             MOVE 'Default printer must be 4 characters.'                 
/DLCA/                                 TO MULTI-EDIT-MSG                        
/DLCA/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCA/                                    ERROR-SWITCH                          
/DLCA/                                    DETL-DFT-PRINTER-L                    
/DLCA/                                    DETL-MESSAGE-L                        
/DLCA/                                    MULTI-EDIT-MSG                        
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-AD-REFRESH-L > ZEROES                                       
/DLCD/     OR  DETL-AD-REFRESH-F = FLDMODFY                                     
/DLCD/         IF  DETL-AD-REFRESH-I > SPACE                                    
/DLCD/             MOVE DETL-AD-REFRESH-I                                       
/DLCD/                                 TO VUSER-AD-REFRESH                      
/DLCD/         ELSE                                                             
/DLCD/             MOVE SPACE          TO VUSER-AD-REFRESH                      
/DLCD/         END-IF                                                           
/DLCD/         IF  NOT VUSER-NO-REFRESH                                         
/DLCD/         AND NOT VUSER-DO-REFRESH                                         
/DLCD/             MOVE 'Active Directory refresh must be Y or N.'              
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-AD-REFRESH-L                     
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-DEPT-NAME-L > ZEROES                                        
/DLCD/     OR  DETL-DEPT-NAME-F = FLDMODFY                                      
/DLCD/         IF  DETL-DEPT-NAME-I <= SPACES                                   
/DLCD/             MOVE 'Department name may not be blank.'                     
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-DEPT-NAME-L                      
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-DEPT-NAME-I                                        
/DLCD/                                 TO VUSER-DEPT-NAME                       
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-PRIMARY-CO-L > ZEROES                                       
/DLCD/     OR  DETL-PRIMARY-CO-F = FLDMODFY                                     
/DLCD/         IF  DETL-PRIMARY-CO-I <= SPACES                                  
/DLCD/             MOVE 'Primary company may not be blank.'                     
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-PRIMARY-CO-L                     
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF  DETL-PRIMARY-CO-I NOT NUMERIC                                
/DLCD/             MOVE 'Primary company must be numeric.'                      
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-PRIMARY-CO-L                     
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         PERFORM Q40-INITKEY-COMASTR                                      
/DLCD/         MOVE DETL-PRIMARY-CO-I  TO COMAST-KEY                            
/DLCD/         PERFORM Q46-READEQ-COMASTR                                       
/DLCD/         IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLCD/             MOVE 'Primary company is not on file.'                       
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-PRIMARY-CO-L                     
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-PRIMARY-CO-I                                       
/DLCD/                                 TO VUSER-PRIMARY-CO                      
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-PRIMARY-IND-L > ZEROES                                      
/DLCD/     OR  DETL-PRIMARY-IND-F = FLDMODFY                                    
/DLCD/         IF  DETL-PRIMARY-IND-I <= SPACES                                 
/DLCD/             MOVE 'Primary industry may not be blank.'                    
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-PRIMARY-IND-L                    
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         PERFORM Q40-INITKEY-COMASTR                                      
/DLCD/         MOVE DETL-PRIMARY-IND-I TO COMAST-IKEY                           
/DLCD/         PERFORM Q46-READGE-COMASTI                                       
/DLCD/         IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLCD/         OR  COMAST-IKEY-IND NOT = DETL-PRIMARY-IND-I                     
/DLCD/             MOVE 'Primary industry is not on file.'                      
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-PRIMARY-IND-L                    
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-PRIMARY-IND-I                                      
/DLCD/                                 TO VUSER-PRIMARY-IND                     
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-FUNCTION-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-FUNCTION-F = FLDMODFY                                   
/DLCD/         IF  DETL-WIN-FUNCTION-I <= SPACES                                
/DLCD/             MOVE 'WIN Function may not be blank.'                        
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-FUNCTION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF  DETL-WIN-FUNCTION-I NOT = 'WW' AND 'WG' AND 'WS'             
/DLCD/                                   AND 'CO' AND 'AC' AND 'LC'             
/DLCD/                                   AND 'VP' AND 'XT'                      
/DLCD/             MOVE 'WIN Function is not a known value.'                    
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-FUNCTION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-WIN-FUNCTION-I                                     
/DLCD/                                 TO VUSER-WIN-FUNCTION                    
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-UNIT-L > ZEROES                                         
/DLCD/     OR  DETL-WIN-UNIT-F = FLDMODFY                                       
/DLCD/         IF  DETL-WIN-UNIT-I <= SPACES                                    
/DLCD/             MOVE 'WIN Unit may not be blank.'                            
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-UNIT-L                       
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF (VUSER-WIN-FUNCTION = 'WW' OR 'WG' OR 'WS'                    
/DLCD/                               OR 'CO' OR 'VP')                           
/DLCD/         AND DETL-WIN-UNIT-I NOT = 'ACCT'       AND 'AL'                  
/DLCD/                               AND 'AP'         AND 'COMM'                
/DLCD/                               AND 'CONTRACTOR' AND 'CUSTSVC'             
/DLCD/                               AND 'DTAMGMT'    AND 'EXEC'                
/DLCD/                               AND 'FIN'        AND 'HR'                  
/DLCD/                               AND 'INVSTR'     AND 'IT'                  
/DLCD/                               AND 'MKT'        AND 'PRICING'             
/DLCD/                               AND 'PURCHS'     AND 'ROMS'                
/DLCD/                               AND 'SPS'        AND 'TRAINING'            
/DLCD/                               AND 'WCMS'       AND 'WINSUP'              
/DLCD/                               AND 'WSS'        AND 'DAYTON'              
/DLCD/                               AND 'DENVER'     AND 'JACKSONVLE'          
/DLCD/                               AND 'NHAVEN'     AND 'RCHMND'              
/DLCD/             MOVE 'WIN Unit is not a known corporate value.'              
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-FUNCTION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF (VUSER-WIN-FUNCTION = 'AC' OR 'LC' OR 'XT')                   
/DLCD/         AND (DETL-WIN-UNIT-I   = 'ACCT'       OR 'AL'                    
/DLCD/                               OR 'AP'         OR 'COMM'                  
/DLCD/                               OR 'CONTRACTOR' OR 'CUSTSVC'               
/DLCD/                               OR 'DTAMGMT'    OR 'EXEC'                  
/DLCD/                               OR 'FIN'        OR 'HR'                    
/DLCD/                               OR 'INVSTR'     OR 'IT'                    
/DLCD/                               OR 'MKT'        OR 'PRICING'               
/DLCD/                               OR 'PURCHS'     OR 'ROMS'                  
/DLCD/                               OR 'SPS'        OR 'TRAINING'              
/DLCD/                               OR 'WCMS'       OR 'WINSUP'                
/DLCD/                               OR 'WSS'        OR 'DAYTON'                
/DLCD/                               OR 'DENVER'     OR 'JACKSONVLE'            
/DLCD/                               OR 'NHAVEN'     OR 'RCHMND')               
/DLCD/             MOVE 'WIN Unit is a corporate, not local, value.'            
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-FUNCTION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-WIN-UNIT-I                                         
/DLCD/                                 TO VUSER-WIN-UNIT                        
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-ORG-L > ZEROES                                          
/DLCD/     OR  DETL-WIN-ORG-F = FLDMODFY                                        
/DLCD/         IF  DETL-WIN-ORG-I <= SPACES                                     
/DLCD/             MOVE 'WIN Org may not be blank.'                             
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-ORG-L                        
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF (VUSER-WIN-FUNCTION = 'AC'                                    
/DLCD/         OR  VUSER-WIN-UNIT = 'AL')                                       
/DLCD/         AND DETL-WIN-ORG-I(1:1) NOT ALPHABETIC                           
/DLCD/             MOVE 'WIN Org area must start with a letter.'                
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-ORG-L                        
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF  VUSER-WIN-FUNCTION NOT = 'AC'                                
/DLCD/         AND VUSER-WIN-UNIT NOT = 'AL'                                    
/DLCD/         AND DETL-WIN-ORG-I NOT NUMERIC                                   
/DLCD/             MOVE 'WIN Org company must be numeric.'                      
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-ORG-L                        
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         MOVE DETL-WIN-ORG-I     TO VUSER-WIN-ORG                         
/DLCD/         IF  VUSER-WIN-UNIT = 'AL'                                        
/DLCD/             PERFORM Q40-INITKEY-COMASTR                                  
/DLCD/             MOVE DETL-WIN-ORG-I(1:1)                                     
/DLCD/                                 TO COMAST-WKEY                           
/DLCD/             PERFORM Q46-READGE-COMASTW                                   
/DLCD/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLCD/             OR  COMAST-WKEY-WIN NOT = DETL-WIN-ORG-I(1:1)                
/DLCD/                 MOVE 'WIN Org company is not on file.'                   
/DLCD/                                     TO MULTI-EDIT-MSG                    
/DLCD/                 CALL MULTMESG    USING DFHEIBLK MULTMESG                 
/DLCD/                                        ERROR-SWITCH                      
/DLCD/                                        DETL-WIN-ORG-L                    
/DLCD/                                        DETL-MESSAGE-L                    
/DLCD/                                        MULTI-EDIT-MSG                    
/DLCD/             ELSE                                                         
/DLCD/               MOVE COMAST-WKEY-WIN  TO VUSER-WIN-ORG                     
/DLCD/               MOVE '00'             TO VUSER-WIN-ORG(2:)                 
/DLCD/               MOVE VUSER-WIN-ORG    TO DETL-WIN-ORG-O                    
/DLCD/             END-IF                                                       
/DLCD/         ELSE                                                             
/DLCD/         IF  VUSER-WIN-FUNCTION = 'AC'                                    
/DLCD/             PERFORM Q40-INITKEY-COMASTR                                  
/DLCD/             MOVE DETL-WIN-ORG-I(1:3)                                     
/DLCD/                                     TO COMAST-AKEY                       
/DLCD/             PERFORM Q46-READGE-COMASTA                                   
/DLCD/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLCD/             OR  COMAST-AKEY-AREA NOT = DETL-WIN-ORG-I(1:3)               
/DLCD/                 MOVE 'WIN Org area is not on file.'                      
/DLCD/                                     TO MULTI-EDIT-MSG                    
/DLCD/                 CALL MULTMESG    USING DFHEIBLK MULTMESG                 
/DLCD/                                        ERROR-SWITCH                      
/DLCD/                                        DETL-WIN-ORG-L                    
/DLCD/                                        DETL-MESSAGE-L                    
/DLCD/                                        MULTI-EDIT-MSG                    
/DLCD/             ELSE                                                         
/DLCD/               MOVE COMAST-AKEY-AREA TO VUSER-WIN-ORG                     
/DLCD/               MOVE VUSER-WIN-ORG    TO DETL-WIN-ORG-O                    
/DLCD/             END-IF                                                       
/DLCD/         ELSE                                                             
/DLCD/         IF  VUSER-WIN-FUNCTION NOT = 'AC'                                
/DLCD/         AND VUSER-WIN-UNIT NOT = 'AL'                                    
/DLCD/             PERFORM Q40-INITKEY-COMASTR                                  
/DLCD/             MOVE DETL-WIN-ORG-I TO COMAST-KEY                            
/DLCD/             PERFORM Q46-READEQ-COMASTR                                   
/DLCD/             IF  EIBRESP NOT = DFHRESP(NORMAL)                            
/DLCD/                 MOVE 'WIN Org company is not on file.'                   
/DLCD/                                     TO MULTI-EDIT-MSG                    
/DLCD/                 CALL MULTMESG    USING DFHEIBLK MULTMESG                 
/DLCD/                                        ERROR-SWITCH                      
/DLCD/                                        DETL-WIN-ORG-L                    
/DLCD/                                        DETL-MESSAGE-L                    
/DLCD/                                        MULTI-EDIT-MSG                    
/DLCD/             END-IF                                                       
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-POSITION-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-POSITION-F = FLDMODFY                                   
/DLCD/         IF  DETL-WIN-POSITION-I <= SPACES                                
/DLCD/             MOVE 'WIN Position may not be blank.'                        
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-POSITION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF  DETL-WIN-POSITION-I NOT = 'EMPL' AND 'PRES' AND 'SIC'        
/DLCD/             MOVE 'WIN Position is not a known value.'                    
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-POSITION-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-WIN-POSITION-I                                     
/DLCD/                                 TO VUSER-WIN-POSITION                    
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-DIRECTOR-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-DIRECTOR-F = FLDMODFY                                   
/DLCD/         IF  DETL-WIN-DIRECTOR-I <= SPACES                                
/DLCD/             MOVE 'WIN Director may not be blank.'                        
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-DIRECTOR-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/         IF  DETL-WIN-DIRECTOR-I NOT = 'Y' and 'N'                        
/DLCD/             MOVE 'WIN Director must be Y or N.'                          
/DLCD/                                 TO MULTI-EDIT-MSG                        
/DLCD/             CALL MULTMESG    USING DFHEIBLK MULTMESG                     
/DLCD/                                    ERROR-SWITCH                          
/DLCD/                                    DETL-WIN-DIRECTOR-L                   
/DLCD/                                    DETL-MESSAGE-L                        
/DLCD/                                    MULTI-EDIT-MSG                        
/DLCD/         ELSE                                                             
/DLCD/             MOVE DETL-WIN-DIRECTOR-I                                     
/DLCD/                                 TO VUSER-WIN-DIRECTOR                    
/DLCD/         END-IF                                                           
/DLCD/     END-IF.                                                              
                                                                                
      * END OF DETAIL FIELD EDITING                                             
                                                                                
/DLC8/     IF  ERRORS-FOUND                                                     
/DLC8/         GO TO D80-DETL-OUTPUT                                            
/DLC8/     END-IF.                                                              
/DLC8/     IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/         GO TO D70-DETL-ERRORS                                            
/DLC8/     END-IF.                                                              
                                                                                
       D40-DETL-UPDATES.                                                        
                                                                                
      * PROCESS ANY ALLOWED UPDATES HERE                                        
                                                                                
/DLC8/*    MOVE SPACES                 TO VOPERID-RECORD.                       
/DLC8/     PERFORM Q20-INITKEY-VUSERID.                                         
           IF  ADD-PROCESS                                                      
/DLC8/*        MOVE VUSR-OPERID        TO VOPERID-CODE                          
/DLC8/         MOVE VUSR-USERID        TO VUSER-KEY                             
/DLC8/*        PERFORM Q26-READEQ-VOPERID                                       
/DLC8/         PERFORM Q26-READEQ-VUSERID                                       
/DLC8/*        MOVE OPER-OPERID-I      TO VOPERID-CODE                          
/DLC8/         MOVE DETL-USERID-I      TO VUSER-KEY                             
           ELSE                                                                 
/DLC8/*        MOVE OPER-OPERID-I      TO VOPERID-CODE                          
/DLC8/         MOVE DETL-USERID-I      TO VUSER-KEY                             
/DLC8/*        PERFORM Q26-READUP-VOPERID                                       
/DLC8/         PERFORM Q26-READUP-VUSERID                                       
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO D70-DETL-ERRORS                                            
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-USERNM-L > ZEROES                                           
/DLC8/     IF  DETL-USERNM-L > ZEROES                                           
/DLC8/*    OR  OPER-USERNM-F = FLDMODFY                                         
/DLC8/     OR  DETL-USERNM-F = FLDMODFY                                         
/DLC8/*        MOVE OPER-USERNM-I      TO VOPERID-NAME                          
/DLC8/         MOVE DETL-USERNM-I      TO VUSER-NAME                            
/DLC4/*DLC8    INSPECT VOPERID-NAME                                             
/DLC8/         INSPECT VUSER-NAME                                               
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-USERID-L > ZEROES                                           
/DLC8/     IF  DETL-OPERID-L > ZEROES                                           
/DLC8/*    OR  OPER-USERID-F = FLDMODFY                                         
/DLC8/     OR  DETL-OPERID-F = FLDMODFY                                         
/DLC8/*        MOVE OPER-USERID-I      TO VOPERID-USERID                        
/DLC8/         MOVE DETL-OPERID-I      TO VUSER-OPID                            
/DLC4/*DLC8    INSPECT VOPERID-USERID                                           
/DLC8/         INSPECT VUSER-OPID                                               
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-CLASS-L > ZEROES                                            
/DLC8/     IF  DETL-CLASS-L > ZEROES                                            
/DLC8/*    OR  OPER-CLASS-F = FLDMODFY                                          
/DLC8/     OR  DETL-CLASS-F = FLDMODFY                                          
/DLC8/*        MOVE OPER-CLASS-I       TO VOPERID-CLASS                         
/DLC8/         MOVE DETL-CLASS-I       TO VUSER-CLASS                           
/DLC4/*DLC8    INSPECT VOPERID-CLASS                                            
/DLC8/         INSPECT VUSER-CLASS                                              
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DAPSCO-L > ZEROES                                           
/DLC8/     IF  DETL-DAPSCO-L > ZEROES                                           
/DLC8/*    OR  OPER-DAPSCO-F = FLDMODFY                                         
/DLC8/     OR  DETL-DAPSCO-F = FLDMODFY                                         
/DLC8/*        MOVE OPER-DAPSCO-I      TO VOPERID-DAPSCO                        
/DLC8/         MOVE DETL-DAPSCO-I      TO VUSER-DAPSCO                          
/DLC4/*DLC8    INSPECT VOPERID-DAPSCO                                           
/DLC8/         INSPECT VUSER-DAPSCO                                             
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-IND-LIST-L > ZEROES                                         
/DLC8/     IF  DETL-IND-LIST-L > ZEROES                                         
/DLC8/*    OR  OPER-IND-LIST-F = FLDMODFY                                       
/DLC8/     OR  DETL-IND-LIST-F = FLDMODFY                                       
/DLC8/*        MOVE OPER-IND-LIST-I    TO VOPERID-IND-LIST                      
/DLC8/         MOVE DETL-IND-LIST-I    TO VUSER-IND-LIST                        
/DLC4/*DLC8    INSPECT VOPERID-IND-LIST                                         
/DLC8/         INSPECT VUSER-IND-LIST                                           
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-IND-AUTH-L > ZEROES                                         
/DLC8/     IF  DETL-IND-AUTH-L > ZEROES                                         
/DLC8/*    OR  OPER-IND-AUTH-F = FLDMODFY                                       
/DLC8/     OR  DETL-IND-AUTH-F = FLDMODFY                                       
/DLC8/*        MOVE OPER-IND-AUTH-I    TO VOPERID-IND-AUTH                      
/DLC8/         MOVE DETL-IND-AUTH-I    TO VUSER-IND-AUTH                        
/DLC4/*DLC8    INSPECT VOPERID-IND-AUTH                                         
/DLC8/         INSPECT VUSER-IND-AUTH                                           
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-SELECT-L > ZEROES                                       
/DLC8/     IF  DETL-DFQ-SELECT-L > ZEROES                                       
/DLC8/*    OR  OPER-DFQ-SELECT-F = FLDMODFY                                     
/DLC8/     OR  DETL-DFQ-SELECT-F = FLDMODFY                                     
/DLC8/*        MOVE OPER-DFQ-SELECT-I  TO VOPERID-DFQ-SELECT                    
/DLC8/         MOVE DETL-DFQ-SELECT-I  TO VUSER-DFQ-SELECT                      
/DLC4/*DLC8    INSPECT VOPERID-DFQ-SELECT                                       
/DLC8/         INSPECT VUSER-DFQ-SELECT                                         
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-BASEN-L > ZEROES                                        
/DLC8/     IF  DETL-DFQ-BASEN-L > ZEROES                                        
/DLC8/*    OR  OPER-DFQ-BASEN-F = FLDMODFY                                      
/DLC8/     OR  DETL-DFQ-BASEN-F = FLDMODFY                                      
/DLC8/*        MOVE OPER-DFQ-BASEN-I   TO VOPERID-DFQ-BASE                      
/DLC8/         MOVE DETL-DFQ-BASEN-I   TO VUSER-DFQ-BASE                        
/DLC4/*DLC8    INSPECT VOPERID-DFQ-BASE                                         
/DLC8/         INSPECT VUSER-DFQ-BASE                                           
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-REPTN-L > ZEROES                                        
/DLC8/     IF  DETL-DFQ-REPTN-L > ZEROES                                        
/DLC8/*    OR  OPER-DFQ-REPTN-F = FLDMODFY                                      
/DLC8/     OR  DETL-DFQ-REPTN-F = FLDMODFY                                      
/DLC8/*        MOVE OPER-DFQ-REPTN-I   TO VOPERID-DFQ-REPT                      
/DLC8/         MOVE DETL-DFQ-REPTN-I   TO VUSER-DFQ-REPT                        
/DLC4/*DLC8    INSPECT VOPERID-DFQ-REPT                                         
/DLC8/         INSPECT VUSER-DFQ-REPT                                           
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-DFQ-SORT-L > ZEROES                                         
/DLC8/     IF  DETL-DFQ-SORT-L > ZEROES                                         
/DLC8/*    OR  OPER-DFQ-SORT-F = FLDMODFY                                       
/DLC8/     OR  DETL-DFQ-SORT-F = FLDMODFY                                       
/DLC8/*        MOVE OPER-DFQ-SORT-I    TO VOPERID-DFQ-SORT                      
/DLC8/         MOVE DETL-DFQ-SORT-I    TO VUSER-DFQ-SORT                        
/DLC4/*DLC8    INSPECT VOPERID-DFQ-SORT                                         
/DLC8/         INSPECT VUSER-DFQ-SORT                                           
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-NEWC-DAYS-L > ZEROES                                        
/DLC8/     IF  DETL-NEWC-DAYS-L > ZEROES                                        
/DLC8/*    OR  OPER-NEWC-DAYS-F = FLDMODFY                                      
/DLC8/     OR  DETL-NEWC-DAYS-F = FLDMODFY                                      
/DLC8/*        MOVE OPER-NEWC-DAYS-I   TO VOPERID-NEWC-DAYX                     
/DLC8/         MOVE DETL-NEWC-DAYS-I   TO VUSER-NEWC-DAYX                       
/DLC4/*DLC8    INSPECT VOPERID-NEWC-DAYX                                        
/DLC8/         INSPECT VUSER-NEWC-DAYX                                          
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-NEWC-SORT-L > ZEROES                                        
/DLC8/     IF  DETL-NEWC-SORT-L > ZEROES                                        
/DLC8/*    OR  OPER-NEWC-SORT-F = FLDMODFY                                      
/DLC8/     OR  DETL-NEWC-SORT-F = FLDMODFY                                      
/DLC8/*        MOVE OPER-NEWC-SORT-I   TO VOPERID-NEWC-SORT                     
/DLC8/         MOVE DETL-NEWC-SORT-I   TO VUSER-NEWC-SORT                       
/DLC4/*DLC8    INSPECT VOPERID-NEWC-SORT                                        
/DLC8/         INSPECT VUSER-NEWC-SORT                                          
/DLC4/                      REPLACING ALL LOW-VALUE BY SPACE                    
           END-IF.                                                              
                                                                                
/DLC8/*    IF  OPER-AP-SEC-L > ZEROES                                           
/DLC8/*DLC9IF  DETL-AP-SEC-L > ZEROES                                           
/DLC8/*    OR  OPER-AP-SEC-F = FLDMODFY                                         
/DLC8/*DLC9OR  DETL-AP-SEC-F = FLDMODFY                                         
/DLC8/*        MOVE OPER-AP-SEC-I      TO VOPERID-AP-SEC                        
/DLC8/*DLC9    MOVE DETL-AP-SEC-I      TO VUSER-AP-SEC                          
/DLC4/*DLC8    INSPECT VOPERID-AP-SEC                                           
/DLC8/*DLC9    INSPECT VUSER-AP-SEC                                             
/DLC4/*DLC9                 REPLACING ALL LOW-VALUE BY SPACE                    
/DLC9/*    END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-SVC-REGION-L > ZEROES                                       
/DLCA/     OR  DETL-SVC-REGION-F = FLDMODFY                                     
/DLCA/         MOVE DETL-SVC-REGION-I  TO VUSER-SVC-REGION                      
/DLCA/         INSPECT VUSER-SVC-REGION                                         
/DLCA/                      REPLACING ALL LOW-VALUE BY SPACE                    
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-ALT-REGION-L > ZEROES                                       
/DLCA/     OR  DETL-ALT-REGION-F = FLDMODFY                                     
/DLCA/         MOVE DETL-ALT-REGION-I  TO VUSER-ALT-REGION                      
/DLCA/         INSPECT VUSER-ALT-REGION                                         
/DLCA/                      REPLACING ALL LOW-VALUE BY SPACE                    
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-RPT-REMOTE-L > ZEROES                                       
/DLCA/     OR  DETL-RPT-REMOTE-F = FLDMODFY                                     
/DLCA/         MOVE DETL-RPT-REMOTE-I  TO VUSER-RPT-REMOTE                      
/DLCA/         INSPECT VUSER-RPT-REMOTE                                         
/DLCA/                      REPLACING ALL LOW-VALUE BY SPACE                    
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-ALT-REMOTE-L > ZEROES                                       
/DLCA/     OR  DETL-ALT-REMOTE-F = FLDMODFY                                     
/DLCA/         MOVE DETL-ALT-REMOTE-I  TO VUSER-ALT-REMOTE                      
/DLCA/         INSPECT VUSER-ALT-REMOTE                                         
/DLCA/                      REPLACING ALL LOW-VALUE BY SPACE                    
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  DETL-DFT-PRINTER-L > ZEROES                                      
/DLCA/     OR  DETL-DFT-PRINTER-F = FLDMODFY                                    
/DLCA/         MOVE DETL-DFT-PRINTER-I TO VUSER-DFT-PRINTER                     
/DLCA/         INSPECT VUSER-DFT-PRINTER                                        
/DLCA/                      REPLACING ALL LOW-VALUE BY SPACE                    
/DLCA/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-AD-REFRESH-L > ZEROES                                       
/DLCD/     OR  DETL-AD-REFRESH-F = FLDMODFY                                     
/DLCD/         MOVE DETL-AD-REFRESH-I  TO VUSER-AD-REFRESH                      
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-DEPT-NAME-L > ZEROES                                        
/DLCD/     OR  DETL-DEPT-NAME-F = FLDMODFY                                      
/DLCD/         MOVE DETL-DEPT-NAME-I   TO VUSER-DEPT-NAME                       
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-PRIMARY-CO-L > ZEROES                                       
/DLCD/     OR  DETL-PRIMARY-CO-F = FLDMODFY                                     
/DLCD/         MOVE DETL-PRIMARY-CO-I  TO VUSER-PRIMARY-CO                      
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-PRIMARY-IND-L > ZEROES                                      
/DLCD/     OR  DETL-PRIMARY-IND-F = FLDMODFY                                    
/DLCD/         MOVE DETL-PRIMARY-IND-I TO VUSER-PRIMARY-IND                     
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-FUNCTION-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-FUNCTION-F = FLDMODFY                                   
/DLCD/         MOVE DETL-WIN-FUNCTION-I                                         
/DLCD/                                 TO VUSER-WIN-FUNCTION                    
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-UNIT-L > ZEROES                                         
/DLCD/     OR  DETL-WIN-UNIT-F = FLDMODFY                                       
/DLCD/         MOVE DETL-WIN-UNIT-I    TO VUSER-WIN-UNIT                        
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-ORG-L > ZEROES                                          
/DLCD/     OR  DETL-WIN-ORG-F = FLDMODFY                                        
/DLCD/         MOVE DETL-WIN-ORG-I     TO VUSER-WIN-ORG                         
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-POSITION-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-POSITION-F = FLDMODFY                                   
/DLCD/         MOVE DETL-WIN-POSITION-I                                         
/DLCD/                                 TO VUSER-WIN-POSITION                    
/DLCD/     END-IF.                                                              
                                                                                
/DLCD/     IF  DETL-WIN-DIRECTOR-L > ZEROES                                     
/DLCD/     OR  DETL-WIN-DIRECTOR-F = FLDMODFY                                   
/DLCD/         MOVE DETL-WIN-DIRECTOR-I                                         
/DLCD/                                 TO VUSER-WIN-DIRECTOR                    
/DLCD/     END-IF.                                                              
                                                                                
           IF  ADD-PROCESS                                                      
/DLC8/*        PERFORM Q27-WRITE-VOPERID                                        
/DLC8/         PERFORM Q27-WRITE-VUSERID                                        
           ELSE                                                                 
/DLC8/*        PERFORM Q27-REWRITE-VOPERID                                      
/DLC8/         PERFORM Q27-REWRITE-VUSERID                                      
           END-IF.                                                              
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO D70-DETL-ERRORS                                            
           END-IF.                                                              
                                                                                
/DLC8/* maintain VOPERID file side-by-side                                      
/DLC8/*DLCCPERFORM Q30-INITKEY-VOPERID.                                         
/DLC8/*DLCCMOVE VUSER-KEY              TO VOPERID-USERID.                       
/DLC8/* retrieve VOPERID file by user id                                        
/DLC8/*DLCCPERFORM Q36-READEQ-VOPERPU.                                          
/DLC8/*DLCCEVALUATE TRUE                                                        
/DLC8/* if found but oper ids are mismatched                                    
/DLC8/*DLCCWHEN EIBRESP = DFHRESP(NORMAL)                                       
/DLC8/*DLCCAND  VOPERID-CODE NOT = VUSER-OPID                                   
/DLC8/*DLCC    PERFORM Q36-READUP-VOPERID                                       
/DLC8/*DLCC    IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*DLCC        GO TO D70-DETL-ERRORS                                        
/DLC8/*DLCC    END-IF                                                           
/DLC8/*DLCC    PERFORM Q38-DELETE-VOPERID                                       
/DLC8/*DLCC    IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*DLCC        GO TO D70-DETL-ERRORS                                        
/DLC8/*DLCC    END-IF                                                           
/DLC8/*DLCC    PERFORM P30-MOVE-VUSERID-TO-VOPERID THRU P35-EXIT                
/DLC8/*DLCC    PERFORM Q37-WRITE-VOPERID                                        
/DLC8/*DLCC    IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*DLCC        GO TO D70-DETL-ERRORS                                        
/DLC8/*DLCC    END-IF                                                           
/DLC8/* if not found or oper ids are matched                                    
/DLC8/*DLCCWHEN EIBRESP = DFHRESP(NOTFND)                                       
/DLC8/*DLCCOR   EIBRESP = DFHRESP(NORMAL)                                       
/DLC8/*DLCC    MOVE VUSER-OPID         TO VOPERID-CODE                          
/DLC8/*DLCC    PERFORM Q36-READUP-VOPERID                                       
/DLC8/*DLCC    PERFORM P30-MOVE-VUSERID-TO-VOPERID THRU P35-EXIT                
/DLC8/*DLCC    EVALUATE EIBRESP                                                 
/DLC8/*DLCC    WHEN DFHRESP(NOTFND)                                             
/DLC8/*DLCC        PERFORM Q37-WRITE-VOPERID                                    
/DLC8/*DLCC    WHEN DFHRESP(NORMAL)                                             
/DLC8/*DLCC        PERFORM Q37-REWRITE-VOPERID                                  
/DLC8/*DLCC    WHEN OTHER                                                       
/DLC8/*DLCC        GO TO D70-DETL-ERRORS                                        
/DLC8/*DLCC    END-EVALUATE                                                     
/DLC8/*DLCC    IF  EIBRESP NOT = DFHRESP(NORMAL)                                
/DLC8/*DLCC        GO TO D70-DETL-ERRORS                                        
/DLC8/*DLCC    END-IF                                                           
/DLC8/* else go report i/o error                                                
/DLC8/*DLCCWHEN OTHER                                                           
/DLC8/*DLCC    GO TO D70-DETL-ERRORS                                            
/DLC8/*DLCCEND-EVALUATE.                                                        
                                                                                
           SET  UPD-PROCESS            TO TRUE.                                 
/DLC8/*    MOVE OPER-OPERID-I          TO VUSR-OPERID.                          
/DLC8/     MOVE DETL-USERID-I          TO VUSR-USERID.                          
                                                                                
       D50-DETL-DISPLAY.                                                        
                                                                                
/DLC8/*    MOVE 'OPER'                 TO COMM-CURRRTN.                         
/DLC8/     MOVE 'DETL'                 TO COMM-CURRRTN.                         
                                                                                
      * PERFORM HEADER CONTROL FIELD EDITING HERE                               
                                                                                
       D60-DETL-DETAIL.                                                         
                                                                                
/DLC1/*DLC8PERFORM Q20-INITKEY-VOPERID.                                         
/DLC8/     PERFORM Q20-INITKEY-VUSERID.                                         
/DLC8/*    MOVE VUSR-OPERID            TO VOPERID-CODE.                         
/DLC8/     MOVE VUSR-USERID            TO VUSER-KEY.                            
/DLC8/*    PERFORM Q26-READEQ-VOPERID.                                          
/DLC8/     PERFORM Q26-READEQ-VUSERID.                                          
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/*        MOVE AIDCLEAR           TO EIBAID                                
/DLC8/         MOVE AIDPFK03           TO EIBAID                                
               GO TO D20-DETL-KEYS                                              
           END-IF.                                                              
                                                                                
      * BUILD OUTPUT DETAIL                                                     
                                                                                
/DLC8/*    MOVE VOPERID-CODE           TO OPER-OPERID-O.                        
/DLC8/     MOVE VUSER-KEY              TO DETL-USERID-O.                        
/DLC8/*    MOVE VOPERID-NAME           TO OPER-USERNM-O.                        
/DLC8/     MOVE VUSER-NAME             TO DETL-USERNM-O.                        
/DLC8/*    MOVE VOPERID-USERID         TO OPER-USERID-O.                        
/DLC8/     MOVE VUSER-OPID             TO DETL-OPERID-O.                        
/DLC8/*    MOVE VOPERID-CLASS          TO OPER-CLASS-O.                         
/DLC8/     MOVE VUSER-CLASS            TO DETL-CLASS-O.                         
/DLC8/*    MOVE VOPERID-DAPSCO         TO OPER-DAPSCO-O.                        
/DLC8/     MOVE VUSER-DAPSCO           TO DETL-DAPSCO-O.                        
/DLC8/*    MOVE VOPERID-IND-LIST       TO OPER-IND-LIST-O.                      
/DLC8/     MOVE VUSER-IND-LIST         TO DETL-IND-LIST-O.                      
/DLC8/*    MOVE VOPERID-IND-AUTH       TO OPER-IND-AUTH-O.                      
/DLC8/     MOVE VUSER-IND-AUTH         TO DETL-IND-AUTH-O.                      
/DLC8/*    MOVE VOPERID-DFQ-SELECT     TO OPER-DFQ-SELECT-O.                    
/DLC8/     MOVE VUSER-DFQ-SELECT       TO DETL-DFQ-SELECT-O.                    
/DLC8/*    MOVE VOPERID-DFQ-BASE       TO OPER-DFQ-BASEN-O.                     
/DLC8/     MOVE VUSER-DFQ-BASE         TO DETL-DFQ-BASEN-O.                     
/DLC8/*    MOVE VOPERID-DFQ-REPT       TO OPER-DFQ-REPTN-O.                     
/DLC8/     MOVE VUSER-DFQ-REPT         TO DETL-DFQ-REPTN-O.                     
/DLC8/*    MOVE VOPERID-DFQ-SORT       TO OPER-DFQ-SORT-O.                      
/DLC8/     MOVE VUSER-DFQ-SORT         TO DETL-DFQ-SORT-O.                      
/DLC8/*    MOVE VOPERID-NEWC-DAYX      TO OPER-NEWC-DAYS-O.                     
/DLC8/     MOVE VUSER-NEWC-DAYX        TO DETL-NEWC-DAYS-O.                     
/DLC8/*    MOVE VOPERID-NEWC-SORT      TO OPER-NEWC-SORT-O.                     
/DLC8/     MOVE VUSER-NEWC-SORT        TO DETL-NEWC-SORT-O.                     
/DLC8/*    MOVE VOPERID-AP-SEC         TO OPER-AP-SEC-O.                        
/DLC8/*DLC9MOVE VUSER-AP-SEC           TO DETL-AP-SEC-O.                        
/DLCA/     MOVE VUSER-SVC-REGION       TO DETL-SVC-REGION-O.                    
/DLCA/     MOVE VUSER-ALT-REGION       TO DETL-ALT-REGION-O.                    
/DLCA/     MOVE VUSER-RPT-REMOTE       TO DETL-RPT-REMOTE-O.                    
/DLCA/     MOVE VUSER-ALT-REMOTE       TO DETL-ALT-REMOTE-O.                    
/DLCA/     MOVE VUSER-DFT-PRINTER      TO DETL-DFT-PRINTER-O.                   
                                                                                
/DLCE/     MOVE VUSER-KEY              TO CHECK-USERID.                         
/DLCE/     PERFORM Q30-INITKEY-IESCNTL.                                         
/DLCE/     PERFORM Q36-READEQ-IESCNTL.                                          
                                                                                
/DLC0/     IF  VUSR-PASSWORD = 'Y'                                              
/DLCE/     AND CURRENT-USER <= CHECKED-USER                                     
/DLC8/*        MOVE VOPERID-PASSWORD   TO OPER-PASSWORD-O                       
/DLC8/         MOVE VUSER-PASSWORD     TO DETL-PASSWORD-O                       
/DLC0/     ELSE                                                                 
/DLC0/*        MOVE ALL '*'            TO OPER-PASSWORD-O                       
/DLC0/         MOVE FLDASDRK           TO DETL-PASSWORD-LABEL-A                 
/DLC0/                                    DETL-PASSWORD-A                       
/DLCE/                                    DETL-PASSWORD-TEXT-A                  
/DLC0/     END-IF.                                                              
                                                                                
/DLCD/     IF  VUSER-DO-REFRESH                                                 
/DLCD/         MOVE 'Y'                TO DETL-AD-REFRESH-O                     
/DLCD/     ELSE                                                                 
/DLCD/         MOVE 'N'                TO DETL-AD-REFRESH-O                     
/DLCD/     END-IF.                                                              
                                                                                
/DLC9/     MOVE VUSER-DEPT-NAME        TO DETL-DEPT-NAME-O.                     
/DLC9/     MOVE VUSER-PRIMARY-CO       TO DETL-PRIMARY-CO-O.                    
/DLC9/     MOVE VUSER-PRIMARY-IND      TO DETL-PRIMARY-IND-O.                   
/DLC9/     MOVE VUSER-WIN-FUNCTION     TO DETL-WIN-FUNCTION-O.                  
/DLC9/     MOVE VUSER-WIN-UNIT         TO DETL-WIN-UNIT-O.                      
/DLC9/     MOVE VUSER-WIN-ORG          TO DETL-WIN-ORG-O.                       
/DLC9/     MOVE VUSER-WIN-POSITION     TO DETL-WIN-POSITION-O.                  
/DLC9/     MOVE VUSER-WIN-DIRECTOR     TO DETL-WIN-DIRECTOR-O.                  
/DLCB/     PERFORM P40-CHECK-AD-ATTRIBUTES THRU P45-EXIT.                       
                                                                                
       D70-DETL-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
/DLC8/*        MOVE EXHREVRS           TO OPER-MESSAGE-H                        
/DLC8/         MOVE EXHREVRS           TO DETL-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
/DLC8/*        MOVE UNEX-MSG           TO OPER-MESSAGE-O                        
/DLC8/         MOVE UNEX-MSG           TO DETL-MESSAGE-O                        
/DLC8/         EXEC CICS SYNCPOINT ROLLBACK END-EXEC                            
           END-IF.                                                              
                                                                                
       D80-DETL-OUTPUT.                                                         
                                                                                
/DLC8/*    MOVE MAPTITLE               TO OPER-COMPANY-O.                       
/DLC8/     MOVE MAPTITLE               TO DETL-COMPANY-O.                       
                                                                                
/DLC8/*    EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
/DLC8/*    EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
/DLC8/*              FULLDATE(OPER-SYSDATE-O)                                   
/DLC8/*              DATESEP('/')                                               
/DLC8/*              TIME(WS-WRKTIME)                                           
/DLC8/*              TIMESEP(':')                                               
/DLC8/*    END-EXEC.                                                            
/DLC8/*    IF  WS-HRSTIME > 12                                                  
/DLC8/*        MOVE ' PM'              TO WS-XXXTIME                            
/DLC8/*        SUBTRACT 12           FROM WS-HRSTIME                            
/DLC8/*    ELSE                                                                 
/DLC8/*        IF  WS-HRSTIME = 12                                              
/DLC8/*            MOVE ' PM'          TO WS-XXXTIME                            
/DLC8/*        ELSE                                                             
/DLC8/*            MOVE ' AM'          TO WS-XXXTIME                            
/DLC8/*            IF  WS-HRSTIME = 00                                          
/DLC8/*                MOVE 12         TO WS-HRSTIME                            
/DLC8/*            END-IF                                                       
/DLC8/*        END-IF                                                           
/DLC8/*    END-IF.                                                              
/DLC8/     MOVE WS-WRKDATE             TO DETL-SYSDATE-O.                       
/DLC8/*    MOVE WS-WRKTIME             TO OPER-SYSTIME-O.                       
/DLC8/     MOVE WS-WRKTIME             TO DETL-SYSTIME-O.                       
                                                                                
/DLC8/*    MOVE EIBTRMID               TO OPER-TERMID-O.                        
/DLC8/     MOVE SPACES                 TO DETL-NETNAME-O.                       
/DLC8/     EXEC CICS ASSIGN NETNAME(DETL-NETNAME-O) END-EXEC.                   
/DLC8/     IF  DETL-NETNAME-I NOT > SPACES                                      
/DLC8/         STRING 'Term: ' EIBTRMID                                         
/DLC8/             DELIMITED BY SIZE INTO DETL-NETNAME-O                        
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     IF  ADD-PROCESS                                                      
/DLC8/*        MOVE '       Adding VOPERID File Record       '                  
/DLC8/         MOVE '       Adding VUSERID File Record       '                  
/DLC8/*                                TO OPER-TITLE-O                          
/DLC8/                                 TO DETL-TITLE-O                          
/DLC8/         MOVE EXHULINE           TO DETL-USERID-H                         
/DLC8/     ELSE                                                                 
/DLC8/*        MOVE '       Update VOPERID File Record       '                  
/DLC8/         MOVE '       Update VUSERID File Record       '                  
/DLC8/*                                TO OPER-TITLE-O                          
/DLC8/                                 TO DETL-TITLE-O                          
/DLC8/         MOVE FLDASBRF           TO DETL-USERID-A                         
/DLC8/     END-IF.                                                              
                                                                                
/DLCD/     IF  VUSER-NO-REFRESH                                                 
/DLCD/     AND NO-ERRORS-FOUND                                                  
/DLCD/         MOVE FLDUNPRO           TO DETL-DEPT-NAME-A                      
/DLCD/                                    DETL-PRIMARY-CO-A                     
/DLCD/                                    DETL-PRIMARY-IND-A                    
/DLCD/                                    DETL-WIN-FUNCTION-A                   
/DLCD/                                    DETL-WIN-UNIT-A                       
/DLCD/                                    DETL-WIN-ORG-A                        
/DLCD/                                    DETL-WIN-POSITION-A                   
/DLCD/                                    DETL-WIN-DIRECTOR-A                   
/DLCD/         MOVE EXHULINE           TO DETL-DEPT-NAME-H                      
/DLCD/                                    DETL-PRIMARY-CO-H                     
/DLCD/                                    DETL-PRIMARY-IND-H                    
/DLCD/                                    DETL-WIN-FUNCTION-H                   
/DLCD/                                    DETL-WIN-UNIT-H                       
/DLCD/                                    DETL-WIN-ORG-H                        
/DLCD/                                    DETL-WIN-POSITION-H                   
/DLCD/                                    DETL-WIN-DIRECTOR-H                   
/DLCD/     END-IF.                                                              
                                                                                
       D85-DETL-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP      (DETLMAP)                                 
                             DATAONLY                                           
                             FROM     (DETLMAPO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP      (DETLMAP)                                 
                             DATAONLY                                           
                             FROM     (DETLMAPO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR   (EIBCPOSN)                                
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               IF  ADD-PROCESS                                                  
/DLC8/*            MOVE -1                 TO OPER-OPERID-L                     
/DLC8/             MOVE -1                 TO DETL-USERID-L                     
               ELSE                                                             
/DLC8/*            MOVE -1                 TO OPER-USERID-L                     
/DLC8/             MOVE -1                 TO DETL-USERNM-L                     
               END-IF                                                           
               EXEC CICS SEND                                                   
                         MAP      (DETLMAP)                                     
                         FROM     (DETLMAPO)                                    
                         MAPSET   (VUSRMS)                                      
                         TERMINAL                                               
                         FREEKB                                                 
                         ERASE                                                  
                         CURSOR                                                 
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       D90-DETL-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
                     DATA     (DETLMAPI)                                        
           END-EXEC.                                                            
                                                                                
/DLC8/*    COPY COMMRETN.                                                       
/DLC8/     GO TO Z80-VUSR-RETURN.                                               
                                                                                
/DLCG//*****************************************************************        
  |   *    TEST MAPPING ROUTINE                                        *        
  |   ******************************************************************        
  v    E00-TEST-ROUTINE.                                                        
                                                                                
           IF  ADDRESS OF TESTWINI = NULL                                       
               EXEC CICS GETMAIN                                                
                         SET      (ADDRESS OF TESTWINI)                         
                         LENGTH   (LENGTH OF TESTWINI)                          
                         INITIMG  (LOVALUE)                                     
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
           IF  COMM-CURRRTN NOT = 'TEST'                                        
           OR  COMM-SAVE-FUNC   = INIT-FLAG                                     
               IF  COMM-SAVE-FUNC = INIT-FLAG                                   
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN                          
                   MOVE SPACES         TO COMM-SAVE-FUNC                        
                   MOVE HIGH-VALUES    TO INIT-FLAG                             
               END-IF                                                           
               SET  NO-ERRORS-FOUND    TO TRUE                                  
               IF  COMM-CURRRTN = 'RETN'                                        
                   MOVE VUSR-SAVE-TIOA TO TESTWINO                              
                   MOVE LOW-VALUES     TO VUSR-SAVE-TIOA                        
               END-IF                                                           
               EXEC CICS SEND                                                   
                         MAP      (LISTXXX)                                     
                         MAPONLY                                                
                         MAPSET   (VUSRMS)                                      
                         TERMINAL                                               
               END-EXEC                                                         
               PERFORM E80-TEST-OUTPUT THRU E85-TEST-SENDMAP                    
               GO TO E50-TEST-DISPLAY                                           
           END-IF.                                                              
                                                                                
       E10-TEST-RECEIVE.                                                        
                                                                                
           EXEC CICS RECEIVE                                                    
                     MAP      (TESTWIN)                                         
                     INTO     (TESTWINI)                                        
                     MAPSET   (VUSRMS)                                          
                     TERMINAL                                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
                         AND DFHRESP(MAPFAIL)                                   
               GO TO E70-TEST-ERRORS                                            
           END-IF.                                                              
                                                                                
       E20-TEST-KEYS.                                                           
                                                                                
           EVALUATE EIBAID                                                      
           WHEN AIDCLEAR                                                        
               MOVE JOBSSELT           TO COMM-LINKPGM                          
               GO TO Z90-VUSR-TERMINATION                                       
                                                                                
           WHEN AIDPFK03                                                        
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC                        
               MOVE 'RETN'             TO COMM-CURRRTN                          
               GO TO C00-LIST-ROUTINE                                           
                                                                                
           WHEN AIDPFK01                                                        
      *    ... save current screen data content in commarea ...                 
               MOVE TESTWINI           TO VUSR-SAVE-TIOA                        
               GO TO T50-HELP-TRANSFER                                          
                                                                                
           WHEN AIDPFK04                                                        
               GO TO Z90-VUSR-TERMINATION                                       
                                                                                
           WHEN AIDENTER                                                        
               CONTINUE                                                         
                                                                                
           WHEN AIDCURSR                                                        
           WHEN AIDPAK01                                                        
           WHEN AIDPAK02                                                        
           WHEN AIDPAK03                                                        
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO TEST-MESSAGE-H                        
               MOVE 'No current action assigned to attention key.'              
                                       TO TEST-MESSAGE-O                        
               GO TO E80-TEST-OUTPUT                                            
                                                                                
           WHEN OTHER                                                           
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO TEST-MESSAGE-H                        
               MOVE 'No current action assigned to function key.'               
                                       TO TEST-MESSAGE-O                        
               GO TO E80-TEST-OUTPUT                                            
           END-EVALUATE.                                                        
                                                                                
       E50-TEST-DISPLAY.                                                        
                                                                                
           MOVE 'TEST'                 TO COMM-CURRRTN.                         
                                                                                
       E60-TEST-DETAIL.                                                         
                                                                                
           MOVE VUSR-USERID            TO SCRTY-USERID.                         
           MOVE TEST-COMPANY-I         TO SCRTY-CONUM.                          
           EXEC CICS LINK                                                       
                     PROGRAM(WINSCRTY)                                          
                     COMMAREA(WINSCRTY-PARMS)                                   
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               GO TO E70-TEST-ERRORS                                            
           END-IF.                                                              
                                                                                
           MOVE SCRTY-USERID           TO TEST-USERID-O.                        
           IF  SCRTY-CONUM > SPACES                                             
               MOVE SCRTY-CONUM        TO TEST-COMPANY-O                        
           ELSE                                                                 
               MOVE SPACES             TO TEST-COMPANY-O                        
           END-IF                                                               
                                                                                
           EVALUATE TRUE                                                        
             WHEN SCRTY-ACCESS-ALTR                                             
               MOVE '*ALTERABLE'       TO TEST-ACCESS-O                         
             WHEN SCRTY-ACCESS-UPDT                                             
               MOVE '*UPDATABLE'       TO TEST-ACCESS-O                         
             WHEN SCRTY-ACCESS-READ                                             
               MOVE '*READABLE'        TO TEST-ACCESS-O                         
             WHEN OTHER                                                         
               MOVE '*DENIED'          TO TEST-ACCESS-O                         
           END-EVALUATE.                                                        
                                                                                
           MOVE SCRTY-SVCRGN           TO TEST-SVCRGN-O.                        
           MOVE SCRTY-ALTRGN           TO TEST-ALTRGN-O.                        
           MOVE SCRTY-RPTRMT           TO TEST-RPTRMT-O.                        
           MOVE SCRTY-ALTRMT           TO TEST-ALTRMT-O.                        
           MOVE SCRTY-DFTPTR           TO TEST-DFTPTR-O.                        
           MOVE SCRTY-WINFUNC          TO TEST-WIN-FUNC-O.                      
           MOVE SCRTY-WINUNIT          TO TEST-WIN-UNIT-O.                      
           MOVE SCRTY-WINPOS           TO TEST-WIN-POS-O.                       
           MOVE SCRTY-WINORG           TO TEST-WIN-ORG-O.                       
           MOVE SCRTY-WINDIR           TO TEST-WIN-DIR-O.                       
           MOVE SCRTY-OLDDAP           TO TEST-OLDDAP-O.                        
           MOVE SCRTY-OLDCLS           TO TEST-OLDCLS-O.                        
           MOVE SCRTY-OLDGRP           TO TEST-OLDGRP-O.                        
           MOVE SCRTY-OLDLVL           TO TEST-OLDLVL-O.                        
                                                                                
       E70-TEST-ERRORS.                                                         
                                                                                
           IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
               MOVE -2                 TO ERROR-SWITCH                          
               MOVE EXHREVRS           TO TEST-MESSAGE-H                        
               PERFORM X00-UNEX-ERR                                             
               MOVE UNEX-MSG           TO TEST-MESSAGE-O                        
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC                            
           END-IF.                                                              
                                                                                
       E80-TEST-OUTPUT.                                                         
                                                                                
       E85-TEST-SENDMAP.                                                        
                                                                                
           IF  ERRORS-FOUND                                                     
               IF  ERROR-AT-CURSOR                                              
                   EXEC CICS SEND                                               
                             MAP      (TESTWIN)                                 
                             DATAONLY                                           
                             FROM     (TESTWINO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR                                             
                   END-EXEC                                                     
               ELSE                                                             
                   EXEC CICS SEND                                               
                             MAP      (TESTWIN)                                 
                             DATAONLY                                           
                             FROM     (TESTWINO)                                
                             MAPSET   (VUSRMS)                                  
                             TERMINAL                                           
                             FREEKB                                             
                             ALARM                                              
                             CURSOR   (EIBCPOSN)                                
                   END-EXEC                                                     
               END-IF                                                           
           ELSE                                                                 
               EXEC CICS SEND                                                   
                         MAP      (TESTWIN)                                     
                         FROM     (TESTWINO)                                    
                         MAPSET   (VUSRMS)                                      
                         TERMINAL                                               
                         FREEKB                                                 
               END-EXEC                                                         
           END-IF.                                                              
                                                                                
       E90-TEST-RETURN.                                                         
                                                                                
           EXEC CICS FREEMAIN                                                   
  ^                  DATA     (TESTWINI)                                        
  |        END-EXEC.                                                            
  |                                                                             
/DLCG/     GO TO Z80-VUSR-RETURN.                                               
                                                                                
/DLCH//*****************************************************************        
  |   *    LOAD REPORT ROUTINE                                         *        
  |   ******************************************************************        
  V    H00-QUERY-ROUTINE.                                                       
                                                                                
           MOVE 'Generating...  '      TO POPUP-TEXT.                           
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.                             
                                                                                
           MOVE SPACE                  TO PRT-LNK-REQU.                         
           SET  PRT-PWR-RDRQ           TO TRUE.                                 
           MOVE VUSRATTR               TO PRT-PWR-JOBN.                         
           MOVE 80                     TO PRT-STR-LENG.                         
           COMPUTE CNT                  = LENGTH OF VATTR-JCL                   
                                        / LENGTH OF VATTR-ENTRY.                
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING SUB FROM 1 BY 1                                            
               UNTIL SUB > CNT                                                  
                  OR NOT PRINT-COMPLETED                                        
             SET  TXT-REQUEST-FIND     TO TRUE                                  
             MOVE ZEROES               TO TXT-PNTR                              
             MOVE 5                    TO TXT-PARM-STRLEN                       
             MOVE '&MFID'              TO TXT-PARM-STRING                       
             MOVE 80                   TO TXT-MSTR-BUFLEN                       
             MOVE VATTR-ENTRY(SUB)     TO TXT-MSTR-BUFFER                       
             CALL 'TXTMAN'          USING TXTMAN-PARMS                          
                                          TXTMAN-BUFFER                         
                                          TXTMAN-STRING                         
             IF  TXT-STRING-WAS-FOUND                                           
               SET  TXT-REQUEST-DELETE TO TRUE                                  
               MOVE SPACES             TO TXT-PARM-STRING                       
               CALL 'TXTMAN'        USING TXTMAN-PARMS                          
                                          TXTMAN-BUFFER                         
                                          TXTMAN-STRING                         
               SET  TXT-REQUEST-INSERT TO TRUE                                  
               MOVE 8                  TO TXT-PARM-STRLEN                       
               MOVE VUSR-USERID        TO TXT-PARM-STRING                       
               CALL 'TXTMAN'        USING TXTMAN-PARMS                          
                                          TXTMAN-BUFFER                         
                                          TXTMAN-STRING                         
             END-IF                                                             
             MOVE TXT-MSTR-BUFFER(1:80)                                         
                                       TO PRT-STR-DATA                          
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT                              
           END-PERFORM.                                                         
                                                                                
           IF  PRINT-COMPLETED                                                  
             SET  PRINT-ENDOFMSG       TO TRUE                                  
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT                              
           END-IF.                                                              
                                                                                
           IF  NOT PRINT-COMPLETED                                              
               MOVE 'Query submit failed.  See log.'                            
                                       TO MULTI-EDIT-MSG                        
               MOVE LENGTH OF LIST-MESSAGE-O                                    
                                       TO LIST-MESSAGE-L                        
               CALL MULTMESG        USING DFHEIBLK MULTMESG                     
                                          ERROR-SWITCH                          
                                          LIST-SELECT-L(ROW)                    
                                          LIST-MESSAGE-L                        
                                          MULTI-EDIT-MSG                        
               GO TO C80-LIST-OUTPUT                                            
           END-IF.                                                              
                                                                                
  ^    H90-QUERY-RETURN.                                                        
  |                                                                             
  |        MOVE 'Query submitted.'     TO LIST-MESSAGE-O.                       
/DLCG/     GO TO C50-LIST-DISPLAY.                                              
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
/DLCH/ P00-CALL-PRTMAN.                                                         
/DLCH/     EXEC CICS LINK                                                       
/DLCH/               PROGRAM  (PRT-PGM-NAME)                                    
/DLCH/               COMMAREA (PRT-PGM-COMM)                                    
/DLCH/               LENGTH   (PRT-PGM-COML)                                    
/DLCH/     END-EXEC.                                                            
/DLCH/ P02-CHK-COMPLETE.                                                        
/DLCH/     IF  PRINT-COMPLETED                                                  
/DLCH/         MOVE SPACES             TO PRT-STR-DATA                          
/DLCH/     ELSE                                                                 
/DLCH/         MOVE PRT-LNK-RESP       TO UNEX-RESP                             
/DLCH/         MOVE PRT-LNK-RSP2       TO UNEX-RESP2                            
/DLCH/         STRING PRT-PGM-NAME ' FAILURE: RC=' PRT-LNK-RETN                 
/DLCH/                ', RS=' UNEX-RESP ', R2=' UNEX-RESP2                      
/DLCH/             DELIMITED BY SIZE INTO LOGF-MESG                             
/DLCH/         PERFORM Q100-LOGIT THRU Q199-EXIT                                
/DLCH/         IF  NOT PRINT-ENDOFMSG                                           
/DLCH/             SET PRINT-ENDOFMSG  TO TRUE                                  
/DLCH/             PERFORM P00-CALL-PRTMAN                                      
/DLCH/         END-IF                                                           
/DLCH/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCH/     END-IF.                                                              
/DLCH/ P05-EXIT.                                                                
/DLCH/     EXIT.                                                                
                                                                                
/DLC8/*P10-SELECT-NEXT-MATCH.                                                   
/DLC8/*    PERFORM Q23-READNEXT-VOPERID.                                        
/DLC8/*    IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/*    OR  '*' = SAVE-USERNM-FILTER                                         
/DLC8/*          AND SAVE-CLASS-FILTER                                          
/DLC8/*          AND SAVE-DAPSCO-FILTER                                         
/DLC8/*          AND SAVE-IND-LIST-FILTER                                       
/DLC8/*          AND SAVE-IND-AUTH-FILTER                                       
/DLC8/*        GO TO P15-EXIT                                                   
/DLC8/*    END-IF.                                                              
/DLC8/*    IF  SAVE-USERNM-FILTER NOT = '*'                                     
/DLC8/*        MOVE LENGTH OF SAVE-USERNM-FILTER                                
/DLC8/*                                TO WILDCOMP-LEN                          
/DLC8/*        MOVE SAVE-USERNM-FILTER    TO WILDCOMP-STR1                      
/DLC8/*        INSPECT WILDCOMP-STR1 CONVERTING LOWER-CASE                      
/DLC8/*                                      TO UPPER-CASE                      
/DLC8/*        MOVE VOPERID-NAME       TO WILDCOMP-STR2                         
/DLC8/*        INSPECT WILDCOMP-STR2 CONVERTING LOWER-CASE                      
/DLC8/*                                      TO UPPER-CASE                      
/DLC8/*        CALL 'WILDCOMP'      USING WILDCOMP-PARMS                        
/DLC8/*    END-IF.                                                              
/DLC8/*    IF  (SAVE-USERNM-FILTER   = '*'                                      
/DLC8/*      OR WILDCOMP-STR1-EQ-STR2)                                          
/DLC8/*    AND (SAVE-CLASS-FILTER    = '*'                                      
/DLC8/*      OR SAVE-CLASS-FILTER    = VOPERID-CLASS)                           
/DLC8/*    AND (SAVE-DAPSCO-FILTER   = '*'                                      
/DLC8/*      OR SAVE-DAPSCO-FILTER   = VOPERID-DAPSCO)                          
/DLC8/*    AND (SAVE-IND-LIST-FILTER = '*'                                      
/DLC8/*      OR SAVE-IND-LIST-FILTER = VOPERID-IND-LIST)                        
/DLC8/*    AND (SAVE-IND-AUTH-FILTER = '*'                                      
/DLC8/*      OR SAVE-IND-AUTH-FILTER = VOPERID-IND-AUTH)                        
/DLC8/*    THEN                                                                 
/DLC8/*        GO TO P15-EXIT                                                   
/DLC8/*    END-IF.                                                              
/DLC8/*    GO TO P10-SELECT-NEXT-MATCH.                                         
/DLC8/*P15-EXIT.                                                                
/DLC8/*    EXIT.                                                                
                                                                                
/DLC8/*P20-SELECT-PREV-MATCH.                                                   
/DLC8/*    PERFORM Q24-READPREV-VOPERID.                                        
/DLC8/*    IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLC8/*    OR  '*' = SAVE-USERNM-FILTER                                         
/DLC8/*          AND SAVE-CLASS-FILTER                                          
/DLC8/*          AND SAVE-DAPSCO-FILTER                                         
/DLC8/*          AND SAVE-IND-LIST-FILTER                                       
/DLC8/*          AND SAVE-IND-AUTH-FILTER                                       
/DLC8/*        GO TO P25-EXIT                                                   
/DLC8/*    END-IF.                                                              
/DLC8/*    IF  SAVE-USERNM-FILTER NOT = '*'                                     
/DLC8/*        MOVE LENGTH OF SAVE-USERNM-FILTER                                
/DLC8/*                                TO WILDCOMP-LEN                          
/DLC8/*        MOVE SAVE-USERNM-FILTER    TO WILDCOMP-STR1                      
/DLC8/*        INSPECT WILDCOMP-STR1 CONVERTING LOWER-CASE                      
/DLC8/*                                      TO UPPER-CASE                      
/DLC8/*        MOVE VOPERID-NAME       TO WILDCOMP-STR2                         
/DLC8/*        INSPECT WILDCOMP-STR2 CONVERTING LOWER-CASE                      
/DLC8/*                                      TO UPPER-CASE                      
/DLC8/*        CALL 'WILDCOMP'      USING WILDCOMP-PARMS                        
/DLC8/*    END-IF.                                                              
/DLC8/*    IF  (SAVE-USERNM-FILTER   = '*'                                      
/DLC8/*      OR WILDCOMP-STR1-EQ-STR2)                                          
/DLC8/*    AND (SAVE-CLASS-FILTER    = '*'                                      
/DLC8/*      OR SAVE-CLASS-FILTER    = VOPERID-CLASS)                           
/DLC8/*    AND (SAVE-DAPSCO-FILTER   = '*'                                      
/DLC8/*      OR SAVE-DAPSCO-FILTER   = VOPERID-DAPSCO)                          
/DLC8/*    AND (SAVE-IND-LIST-FILTER = '*'                                      
/DLC8/*      OR SAVE-IND-LIST-FILTER = VOPERID-IND-LIST)                        
/DLC8/*    AND (SAVE-IND-AUTH-FILTER = '*'                                      
/DLC8/*      OR SAVE-IND-AUTH-FILTER = VOPERID-IND-AUTH)                        
/DLC8/*    THEN                                                                 
/DLC8/*        GO TO P25-EXIT                                                   
/DLC8/*    END-IF.                                                              
/DLC8/*    GO TO P20-SELECT-PREV-MATCH.                                         
/DLC8/*P25-EXIT.                                                                
/DLC8/*    EXIT.                                                                
                                                                                
/DLCC/*P30-MOVE-VUSERID-TO-VOPERID.                                             
/DLC8/*DLCCMOVE SPACES                 TO VOPERID-RECORD.                       
/DLC8/*DLCCMOVE VUSER-OPID             TO VOPERID-CODE.                         
/DLC8/*DLCCMOVE VUSER-NAME             TO VOPERID-NAME.                         
/DLC8/*DLCCMOVE VUSER-CLASS            TO VOPERID-CLASS.                        
/DLC8/*DLCCMOVE VUSER-DAPSCO           TO VOPERID-DAPSCO.                       
/DLC8/*DLCCMOVE VUSER-PASSWORD         TO VOPERID-PASSWORD.                     
/DLC8/*DLCCMOVE VUSER-DFQ-SELECT       TO VOPERID-DFQ-SELECT.                   
/DLC8/*DLCCMOVE VUSER-DFQ-BASE         TO VOPERID-DFQ-BASE.                     
/DLC8/*DLCCMOVE VUSER-DFQ-REPT         TO VOPERID-DFQ-REPT.                     
/DLC8/*DLCCMOVE VUSER-DFQ-SORT         TO VOPERID-DFQ-SORT.                     
/DLC8/*DLCCMOVE VUSER-IND-AUTH         TO VOPERID-IND-AUTH.                     
/DLC8/*DLCCMOVE VUSER-IND-LIST         TO VOPERID-IND-LIST.                     
/DLC8/*DLCCMOVE VUSER-NEWC-DAYX        TO VOPERID-NEWC-DAYX.                    
/DLC8/*DLCCMOVE VUSER-NEWC-SORT        TO VOPERID-NEWC-SORT.                    
/DLC8/*DLC9MOVE VUSER-AP-SEC           TO VOPERID-AP-SEC.                       
/DLC8/*DLCCMOVE VUSER-KEY              TO VOPERID-USERID.                       
/DLCC/*P35-EXIT.                                                                
/DLC8/*DLCCEXIT.                                                                
                                                                                
/DLCB/ P40-CHECK-AD-ATTRIBUTES.                                                 
/DLCB/     IF  VUSER-WIN-FUNCTION <= SPACES                                     
/DLCB/         GO TO P45-EXIT                                                   
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-FUNCTION NOT = 'WW' AND 'WG' AND 'WS'                  
/DLCB/                              AND 'CO' AND 'AC' AND 'LC'                  
/DLCB/                              AND 'VP' AND 'XT'                           
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-WIN-FUNCTION-C                   
/DLCF/             MOVE 'unknown'      TO DETL-WIN-FUNC-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF (VUSER-WIN-FUNCTION = 'WW' OR 'WG' OR 'WS'                        
/DLCB/                           OR 'CO' OR 'VP')                               
/DLCB/     AND VUSER-WIN-UNIT NOT = 'ACCT'       AND 'AL'                       
/DLCB/                          AND 'AP'         AND 'COMM'                     
/DLCB/                          AND 'CONTRACTOR' AND 'CUSTSVC'                  
/DLCB/                          AND 'DTAMGMT'    AND 'EXEC'                     
/DLCB/                          AND 'FIN'        AND 'HR'                       
/DLCB/                          AND 'INVSTR'     AND 'IT'                       
/DLCB/                          AND 'MKT'        AND 'PRICING'                  
/DLCB/                          AND 'PURCHS'     AND 'ROMS'                     
/DLCB/                          AND 'SPS'        AND 'TRAINING'                 
/DLCB/                          AND 'WCMS'       AND 'WINSUP'                   
/DLCB/                          AND 'WSS'        AND 'DAYTON'                   
/DLCB/                          AND 'DENVER'     AND 'JACKSONVLE'               
/DLCB/                          AND 'NHAVEN'     AND 'RCHMND'                   
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-FUNCTION-C                   
/DLCB/             MOVE EXCRED         TO DETL-WIN-UNIT-C                       
/DLCF/             MOVE 'not for corp' TO DETL-WIN-UNIT-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/             MOVE EXCRED         TO LIST-WIN-UNIT-C(IDX)                  
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF (VUSER-WIN-FUNCTION = 'AC' OR 'LC' OR 'XT')                       
/DLCB/     AND (VUSER-WIN-UNIT    = 'ACCT'       OR 'AL'                        
/DLCB/                           OR 'AP'         OR 'COMM'                      
/DLCB/                           OR 'CONTRACTOR' OR 'CUSTSVC'                   
/DLCB/                           OR 'DTAMGMT'    OR 'EXEC'                      
/DLCB/                           OR 'FIN'        OR 'HR'                        
/DLCB/                           OR 'INVSTR'     OR 'IT'                        
/DLCB/                           OR 'MKT'        OR 'PRICING'                   
/DLCB/                           OR 'PURCHS'     OR 'ROMS'                      
/DLCB/                           OR 'SPS'        OR 'TRAINING'                  
/DLCB/                           OR 'WCMS'       OR 'WINSUP'                    
/DLCB/                           OR 'WSS'        OR 'DAYTON'                    
/DLCB/                           OR 'DENVER'     OR 'JACKSONVLE'                
/DLCB/                           OR 'NHAVEN'     OR 'RCHMND')                   
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-FUNCTION-C                   
/DLCB/             MOVE EXCRED         TO DETL-WIN-UNIT-C                       
/DLCF/             MOVE 'not for local' TO DETL-WIN-UNIT-TEXT-O                 
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/             MOVE EXCRED         TO LIST-WIN-UNIT-C(IDX)                  
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-POSITION NOT = 'EMPL' AND 'PRES' AND 'SIC'             
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-WIN-POSITION-C                   
/DLCF/             MOVE 'unknown'      TO DETL-WIN-POS-TEXT-O                   
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-WIN-POSITION-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-POSITION     = 'PRES'                                  
/DLCB/     AND VUSER-WIN-DIRECTOR NOT = 'Y'                                     
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-POSITION-C                   
/DLCB/             MOVE EXCRED         TO DETL-WIN-DIRECTOR-C                   
/DLCF/             MOVE 'should be = "Y"'                                       
/DLCF/                                 TO DETL-WIN-DIR-TEXT-O                   
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-POSITION-C(IDX)              
/DLCB/             MOVE EXCRED         TO LIST-WIN-DIRECTOR-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-POSITION     = 'SIC'                                   
/DLCB/     AND VUSER-WIN-FUNCTION NOT = 'LC'                                    
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-POSITION-C                   
/DLCB/             MOVE EXCRED         TO DETL-WIN-FUNCTION-C                   
/DLCF/             MOVE 'should be = "LC"'                                      
/DLCF/                                 TO DETL-WIN-FUNC-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-POSITION-C(IDX)              
/DLCB/             MOVE EXCRED         TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-FUNCTION     = 'VP'                                    
/DLCB/     AND VUSER-WIN-UNIT     NOT = 'AL' AND 'EXEC' AND 'MKT'               
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-FUNCTION-C                   
/DLCB/             MOVE EXCRED         TO DETL-WIN-UNIT-C                       
/DLCF/             MOVE 'not a "VP"'   TO DETL-WIN-UNIT-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/             MOVE EXCRED         TO LIST-WIN-UNIT-C(IDX)                  
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-UNIT         = 'EXEC'                                  
/DLCB/     AND VUSER-WIN-FUNCTION NOT = 'WW' AND 'WS' AND 'WG'                  
/DLCB/                              AND 'VP' AND 'CO'                           
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-UNIT-C                       
/DLCB/             MOVE EXCRED         TO DETL-WIN-FUNCTION-C                   
/DLCF/             MOVE 'not an "EXEC"'                                         
/DLCF/                                 TO DETL-WIN-FUNC-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-UNIT-C(IDX)                  
/DLCB/             MOVE EXCRED         TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-UNIT         = 'AL'                                    
/DLCB/     AND VUSER-WIN-FUNCTION NOT = 'VP'                                    
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCPINK        TO DETL-WIN-UNIT-C                       
/DLCB/             MOVE EXCRED         TO DETL-WIN-FUNCTION-C                   
/DLCF/             MOVE 'not an "AL"'  TO DETL-WIN-FUNC-TEXT-O                  
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCPINK        TO LIST-WIN-UNIT-C(IDX)                  
/DLCB/             MOVE EXCRED         TO LIST-WIN-FUNCTION-C(IDX)              
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     PERFORM Q40-INITKEY-COMASTR.                                         
/DLCB/     MOVE VUSER-PRIMARY-CO       TO COMAST-KEY.                           
/DLCB/     PERFORM Q46-READEQ-COMASTR.                                          
/DLCB/     IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLCB/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-PRIMARY-CO-C                     
/DLCF/             MOVE 'not on file'  TO DETL-PRIM-CO-TEXT-O                   
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-PRIMARY-CO-C(IDX)                
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-PRIMARY-IND NOT = COMAST-IKEY-IND                          
/DLCB/     AND VUSER-PRIMARY-IND NOT = 'B'                                      
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-PRIMARY-IND-C                    
/DLCB/             MOVE EXCPINK        TO DETL-PRIMARY-CO-C                     
/DLCF/             IF  COMAST-IKEY-IND > SPACES                                 
/DLCF/                 MOVE 'should be = "?"'                                   
/DLCF/                                 TO DETL-PRIM-IND-TEXT-O                  
/DLCF/                 MOVE COMAST-IKEY-IND                                     
/DLCF/                                 TO DETL-PRIM-IND-TEXT-O(14:1)            
/DLCF/             END-IF                                                       
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-PRIMARY-IND-C(IDX)               
/DLCB/             MOVE EXCPINK        TO LIST-PRIMARY-CO-C(IDX)                
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-FUNCTION = 'AC'                                        
/DLCB/     AND VUSER-WIN-ORG NOT = COMAST-AKEY-AREA                             
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-WIN-ORG-C                        
/DLCB/             MOVE EXCPINK        TO DETL-PRIMARY-CO-C                     
/DLCF/             IF  COMAST-AKEY-AREA > SPACES                                
/DLCF/                 MOVE 'should be = "???"'                                 
/DLCF/                                 TO DETL-WIN-ORG-TEXT-O                   
/DLCF/                 MOVE COMAST-AKEY-AREA                                    
/DLCF/                                 TO DETL-WIN-ORG-TEXT-O(14:3)             
/DLCF/             END-IF                                                       
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-WIN-ORG-C(IDX)                   
/DLCB/             MOVE EXCPINK        TO LIST-PRIMARY-CO-C(IDX)                
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     MOVE VUSER-PRIMARY-IND      TO COMAST-IKEY-IND.                      
/DLCB/     MOVE ALL ZEROES             TO COMAST-IKEY-CO.                       
/DLCB/     PERFORM Q46-READGE-COMASTI.                                          
/DLCB/     IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLCB/     OR  COMAST-IKEY-IND NOT = VUSER-PRIMARY-IND                          
/DLCB/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-PRIMARY-IND-C                    
/DLCF/             IF  DETL-PRIM-IND-TEXT-I <= SPACES                           
/DLCF/                 MOVE 'not on file'                                       
/DLCF/                                 TO DETL-PRIM-IND-TEXT-O                  
/DLCF/             END-IF                                                       
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-PRIMARY-IND-C(IDX)               
/DLCB/         END-IF                                                           
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-FUNCTION = 'AC'                                        
/DLCB/       MOVE VUSER-WIN-ORG        TO COMAST-AKEY-AREA                      
/DLCB/       MOVE ALL ZEROES           TO COMAST-AKEY-CO                        
/DLCB/       PERFORM Q46-READGE-COMASTA                                         
/DLCB/       IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
/DLCB/       OR  COMAST-AKEY-AREA NOT = VUSER-WIN-ORG(1:3)                      
/DLCB/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-WIN-ORG-C                        
/DLCF/             IF  DETL-WIN-ORG-TEXT-I <= SPACES                            
/DLCF/                 MOVE 'not on file'                                       
/DLCF/                                 TO DETL-WIN-ORG-TEXT-O                   
/DLCF/             END-IF                                                       
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-WIN-ORG-C(IDX)                   
/DLCB/         END-IF                                                           
/DLCB/       END-IF                                                             
/DLCB/     END-IF.                                                              
                                                                                
/DLCB/     IF  VUSER-WIN-UNIT = 'AL'                                            
/DLCB/       MOVE VUSER-WIN-ORG        TO COMAST-WKEY-WIN                       
/DLCB/       MOVE ALL ZEROES           TO COMAST-WKEY-CO                        
/DLCB/       PERFORM Q46-READGE-COMASTW                                         
/DLCB/       IF  EIBRESP NOT = DFHRESP(NORMAL)                                  
/DLCB/       OR  COMAST-WKEY-WIN NOT = VUSER-WIN-ORG(1:1)                       
/DLCB/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCB/         IF  COMM-CURRRTN = 'DETL'                                        
/DLCB/             MOVE EXCRED         TO DETL-WIN-ORG-C                        
/DLCF/             IF  DETL-WIN-ORG-TEXT-I <= SPACES                            
/DLCF/                 MOVE 'not on file'                                       
/DLCF/                                 TO DETL-WIN-ORG-TEXT-O                   
/DLCF/             END-IF                                                       
/DLCB/         ELSE                                                             
/DLCB/             MOVE EXCRED         TO LIST-WIN-ORG-C(IDX)                   
/DLCB/         END-IF                                                           
/DLCB/       END-IF                                                             
/DLCB/     END-IF.                                                              
/DLCB/ P45-EXIT.                                                                
/DLCB/     EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    MORE PERFORMED ROUTINES                                     *        
      ******************************************************************        
                                                                                
/DLC8/ Q00-FORMAT-DATE-AND-TIME.                                                
/DLC8/     EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.                   
/DLC8/     EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)                             
/DLC8/               FULLDATE (WS-WRKDATE)          DATESEP                     
/DLC8/               TIME     (WS-WRKTIME)          TIMESEP                     
/DLC8/     END-EXEC.                                                            
/DLC8/ Q02-REFORMAT-TIME.                                                       
/DLC8/     IF  WS-HRSTIME < 12                                                  
/DLC8/         MOVE ' AM'              TO WS-XXXTIME                            
/DLC8/         IF  WS-HRSTIME = 00                                              
/DLC8/             MOVE 12             TO WS-HRSTIME                            
/DLC8/         END-IF                                                           
/DLC8/     ELSE                                                                 
/DLC8/         MOVE ' PM'              TO WS-XXXTIME                            
/DLC8/         IF  WS-HRSTIME > 12                                              
/DLC8/             SUBTRACT 12       FROM WS-HRSTIME                            
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/ Q05-EXIT.                                                                
/DLC8/     EXIT.                                                                
                                                                                
/DLCH/ Q10-DISPLAY-POPUP.                                                       
/DLCH/     EXEC CICS GETMAIN                                                    
/DLCH/               SET      (ADDRESS OF WAITWINI)                             
/DLCH/               LENGTH   (LENGTH OF WAITWINI)                              
/DLCH/               INITIMG  (LOVALUE)                                         
/DLCH/     END-EXEC.                                                            
/DLCH/     MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.                       
/DLCH/     EXEC CICS SEND                                                       
/DLCH/               MAP      (WAITWIN)                                         
/DLCH/               FROM     (WAITWINO)                                        
/DLCH/               MAPSET   (VUSRMS)                                          
/DLCH/               TERMINAL                                                   
/DLCH/               CURSOR(+926)                                               
/DLCH/               WAIT                                                       
/DLCH/               NOHANDLE                                                   
/DLCH/     END-EXEC.                                                            
/DLCH/     EXEC CICS FREEMAIN                                                   
/DLCH/               DATA     (WAITWINI)                                        
/DLCH/     END-EXEC.                                                            
/DLCH/ Q15-EXIT.                                                                
/DLCH/     EXIT.                                                                
                                                                                
/DLC8/ Q20-INITKEY-VUSERID.                                                     
/DLC8/     IF  ADDRESS OF VUSERID-RECORD = NULL                                 
/DLC8/         EXEC CICS GETMAIN                                                
/DLC8/                   SET      (ADDRESS OF VUSERID-RECORD)                   
/DLC8/                   LENGTH   (LENGTH OF VUSERID-RECORD)                    
/DLC8/         END-EXEC                                                         
/DLC8/     END-IF.                                                              
/DLC8/     MOVE LOW-VALUES             TO VUSER-KEY.                            
/DLC8/ Q21-STARTBR-VUSERID.                                                     
/DLC8/     EXEC CICS STARTBR                                                    
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               GTEQ                                                       
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q21-STARTEQ-VUSERID.                                                     
/DLC8/     EXEC CICS STARTBR                                                    
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               EQUAL                                                      
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q22-RESETBR-VUSERID.                                                     
/DLC8/     EXEC CICS RESETBR                                                    
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               GTEQ                                                       
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q23-READNEXT-VUSERID.                                                    
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READNEXT                                                   
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q23-CHK-FILTER.                                                          
/DLC8/     IF  EIBRESP = DFHRESP(NORMAL)                                        
/DLC8/         PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT                          
/DLC8/         IF  RECORD-NOT-SELECTED                                          
/DLC8/             GO TO Q23-READNEXT-VUSERID                                   
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/ Q24-READPREV-VUSERID.                                                    
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READPREV                                                   
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q24-CHK-FILTER.                                                          
/DLC8/     IF  EIBRESP = DFHRESP(NORMAL)                                        
/DLC8/         PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT                          
/DLC8/         IF  RECORD-NOT-SELECTED                                          
/DLC8/             GO TO Q24-READPREV-VUSERID                                   
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/ Q25-ENDBR-VUSERID.                                                       
/DLC8/     EXEC CICS ENDBR                                                      
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q26-READEQ-VUSERID.                                                      
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READ                                                       
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               EQUAL                                                      
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q26-READEQ-VUSEROP.                                                      
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READ                                                       
/DLC8/               DATASET(VUSEROP)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-OPID)                                         
/DLC8/               EQUAL                                                      
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q26-READGE-VUSERID.                                                      
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READ                                                       
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               GTEQ                                                       
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q26-READGE-VUSEROP.                                                      
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READ                                                       
/DLC8/               DATASET(VUSEROP)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-OPID)                                         
/DLC8/               GTEQ                                                       
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q26-READUP-VUSERID.                                                      
/DLC9/     MOVE SPACES                 TO VUSERID-RECORD(13:).                  
/DLC8/     EXEC CICS READ                                                       
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               INTO(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               EQUAL                                                      
/DLC8/               UPDATE                                                     
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q27-WRITE-VUSERID.                                                       
/DLC9/     IF  VUSERID-RECORD(128:) = LOW-VALUES                                
/DLC9/         MOVE SPACES             TO VUSERID-RECORD(128:)                  
/DLC9/     END-IF.                                                              
/DLC8/     EXEC CICS WRITE                                                      
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               FROM(VUSERID-RECORD)                                       
/DLC8/               RIDFLD(VUSER-KEY)                                          
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q27-REWRITE-VUSERID.                                                     
/DLC9/     IF  VUSERID-RECORD(128:) = LOW-VALUES                                
/DLC9/         MOVE SPACES             TO VUSERID-RECORD(128:)                  
/DLC9/     END-IF.                                                              
/DLC8/     EXEC CICS REWRITE                                                    
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               FROM(VUSERID-RECORD)                                       
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q28-DELETE-VUSERID.                                                      
/DLC8/     EXEC CICS DELETE                                                     
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q28-UNLOCK-VUSERID.                                                      
/DLC8/     EXEC CICS UNLOCK                                                     
/DLC8/               DATASET(VUSERID)                                           
/DLC8/               NOHANDLE                                                   
/DLC8/     END-EXEC.                                                            
/DLC8/ Q29-EXIT.                                                                
/DLC8/     EXIT.                                                                
                                                                                
/DLCC/*Q30-INITKEY-VOPERID.                                                     
/DLC8/*DLCCIF  ADDRESS OF VOPERID-RECORD = NULL                                 
/DLC8/*DLCC    EXEC CICS GETMAIN                                                
/DLC8/*DLCC              SET      (ADDRESS OF VOPERID-RECORD)                   
/DLC8/*DLCC              LENGTH   (LENGTH OF VOPERID-RECORD)                    
/DLC8/*DLCC    END-EXEC                                                         
/DLC8/*DLCCEND-IF.                                                              
/DLC8/*DLCCMOVE LOW-VALUES             TO VOPERID-CODE.                         
/DLCC/*Q36-READEQ-VOPERID.                                                      
/DLC8/*DLCCEXEC CICS READ                                                       
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          INTO(VOPERID-RECORD)                                       
/DLC8/*DLCC          RIDFLD(VOPERID-CODE)                                       
/DLC8/*DLCC          EQUAL                                                      
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q36-READEQ-VOPERPU.                                                      
/DLC8/*DLCCEXEC CICS READ                                                       
/DLC8/*DLCC          DATASET(VOPERPU)                                           
/DLC8/*DLCC          INTO(VOPERID-RECORD)                                       
/DLC8/*DLCC          RIDFLD(VOPERID-USERID)                                     
/DLC8/*DLCC          EQUAL                                                      
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q36-READUP-VOPERID.                                                      
/DLC8/*DLCCEXEC CICS READ                                                       
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          INTO(VOPERID-RECORD)                                       
/DLC8/*DLCC          RIDFLD(VOPERID-CODE)                                       
/DLC8/*DLCC          EQUAL                                                      
/DLC8/*DLCC          UPDATE                                                     
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q37-WRITE-VOPERID.                                                       
/DLC8/*DLCCEXEC CICS WRITE                                                      
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          FROM(VOPERID-RECORD)                                       
/DLC8/*DLCC          RIDFLD(VOPERID-CODE)                                       
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q37-REWRITE-VOPERID.                                                     
/DLC8/*DLCCEXEC CICS REWRITE                                                    
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          FROM(VOPERID-RECORD)                                       
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q38-DELETE-VOPERID.                                                      
/DLC8/*DLCCEXEC CICS DELETE                                                     
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q38-UNLOCK-VOPERID.                                                      
/DLC8/*DLCCEXEC CICS UNLOCK                                                     
/DLC8/*DLCC          DATASET(VOPERID)                                           
/DLC8/*DLCC          NOHANDLE                                                   
/DLC8/*DLCCEND-EXEC.                                                            
/DLCC/*Q39-EXIT.                                                                
/DLC8/*DLCCEXIT.                                                                
                                                                                
/DLCE/ Q30-INITKEY-IESCNTL.                                                     
/DLCE/     IF  ADDRESS OF IESCNTL-RECORD = NULL                                 
/DLCE/         EXEC CICS GETMAIN                                                
/DLCE/                   SET      (ADDRESS OF IESCNTL-RECORD)                   
/DLCE/                   LENGTH   (LENGTH OF IESCNTL-RECORD)                    
/DLCE/         END-EXEC                                                         
/DLCE/         SET ADDRESS OF IUI-US-RECORD                                     
/DLCE/                                 TO ADDRESS OF IESCNTL-RECORD             
/DLCE/     END-IF.                                                              
/DLCE/     MOVE LOW-VALUES             TO IUI-KEY.                              
/DLCE/     SET  IUI-USER-PROFILE       TO TRUE.                                 
/DLCE/     STRING CHECK-USERID            DELIMITED BY SPACE                    
/DLCE/                               INTO IUI-US-USRIDNT.                       
/DLCE/ Q36-READEQ-IESCNTL.                                                      
/DLCE/     MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.                          
/DLCE/     EXEC CICS READ                                                       
/DLCE/               DATASET  (IESCNTL)                                         
/DLCE/               INTO     (IUI-US-RECORD)                                   
/DLCE/               LENGTH   (CNTL-RECL)                                       
/DLCE/               RIDFLD   (IUI-US-KEY)                                      
/DLCE/               EQUAL                                                      
/DLCE/               NOHANDLE                                                   
/DLCE/     END-EXEC.                                                            
/DLCE/     IF  EIBRESP NOT = DFHRESP(NORMAL)                                    
/DLCE/         MOVE DFHRESP(NORMAL)    TO EIBRESP                               
/DLCE/         SET  CHECKED-GENERAL    TO TRUE                                  
/DLCE/     ELSE                                                                 
/DLCE/         MOVE 'D'                TO BIT-REQU                              
/DLCE/         MOVE IUI-US-IESINFO     TO BIT-BYTE                              
/DLCE/         CALL 'BITMAN'        USING BITMAN-PARMS                          
/DLCE/         IF  BIT-0-IS-ON                                                  
/DLCE/         AND BIT-1-IS-ON                                                  
/DLCE/             SET  CHECKED-ADMIN  TO TRUE                                  
/DLCE/         ELSE                                                             
/DLCE/             SET  CHECKED-SUPER  TO TRUE                                  
/DLCE/         END-IF                                                           
/DLCE/         IF  BIT-0-IS-OFF                                                 
/DLCE/         AND BIT-1-IS-OFF                                                 
/DLCE/             SET CHECKED-GENERAL TO TRUE                                  
/DLCE/         END-IF                                                           
/DLCE/     END-IF.                                                              
/DLCE/ Q39-EXIT.                                                                
/DLCE/     EXIT.                                                                
                                                                                
/DLCB/ Q40-INITKEY-COMASTR.                                                     
/DLCB/     IF  ADDRESS OF COMASTR-RECORD = NULL                                 
/DLCB/         EXEC CICS GETMAIN                                                
/DLCB/                   SET      (ADDRESS OF COMASTR-RECORD)                   
/DLCB/                   LENGTH   (LENGTH OF COMASTR-RECORD)                    
/DLCB/         END-EXEC                                                         
/DLCB/     END-IF.                                                              
/DLCB/     MOVE SPACES                 TO COMASTR-RECORD.                       
/DLCB/ Q46-READEQ-COMASTR.                                                      
/DLCB/     EXEC CICS READ                                                       
/DLCB/               DATASET(COMASTR)                                           
/DLCB/               INTO(COMASTR-RECORD)                                       
/DLCB/               RIDFLD(COMAST-KEY)                                         
/DLCB/               EQUAL                                                      
/DLCB/               NOHANDLE                                                   
/DLCB/     END-EXEC.                                                            
/DLCB/ Q46-READGE-COMASTA.                                                      
/DLCB/     EXEC CICS READ                                                       
/DLCB/               DATASET(COMASTA)                                           
/DLCB/               INTO(COMASTR-RECORD)                                       
/DLCB/               RIDFLD(COMAST-AKEY)                                        
/DLCB/               GTEQ                                                       
/DLCB/               NOHANDLE                                                   
/DLCB/     END-EXEC.                                                            
/DLCB/ Q46-READGE-COMASTI.                                                      
/DLCB/     EXEC CICS READ                                                       
/DLCB/               DATASET(COMASTI)                                           
/DLCB/               INTO(COMASTR-RECORD)                                       
/DLCB/               RIDFLD(COMAST-IKEY)                                        
/DLCB/               GTEQ                                                       
/DLCB/               NOHANDLE                                                   
/DLCB/     END-EXEC.                                                            
/DLCB/ Q46-READGE-COMASTW.                                                      
/DLCB/     EXEC CICS READ                                                       
/DLCB/               DATASET(COMASTW)                                           
/DLCB/               INTO(COMASTR-RECORD)                                       
/DLCB/               RIDFLD(COMAST-WKEY)                                        
/DLCB/               GTEQ                                                       
/DLCB/               NOHANDLE                                                   
/DLCB/     END-EXEC.                                                            
/DLCB/ Q49-EXIT.                                                                
/DLCB/     EXIT.                                                                
                                                                                
/DLCH/ Q50-INITKEY-IESLDUM.                                                     
/DLCH/     IF  ADDRESS OF IESLDUM-RECORD = NULL                                 
/DLCH/         EXEC CICS GETMAIN                                                
/DLCH/                   SET      (ADDRESS OF IESLDUM-RECORD)                   
/DLCH/                   LENGTH   (LENGTH OF IESLDUM-RECORD)                    
/DLCH/         END-EXEC                                                         
/DLCH/     END-IF.                                                              
/DLCH/     MOVE LOW-VALUES             TO LDUM-KEY.                             
/DLCH/     SET  LDUM-USRMAP-RECORD     TO TRUE.                                 
/DLCH/ Q56-READEQ-IESLDUV.                                                      
/DLCH/     EXEC CICS READ                                                       
/DLCH/               DATASET  (IESLDUV)                                         
/DLCH/               INTO     (IESLDUM-RECORD)                                  
/DLCH/               RIDFLD   (LDUM-MFUSRID)                                    
/DLCH/               EQUAL                                                      
/DLCH/               NOHANDLE                                                   
/DLCH/     END-EXEC.                                                            
/DLCH/ Q59-EXIT.                                                                
/DLCH/     EXIT.                                                                
                                                                                
/DLC8/ Q80-SAVE-FILTERS.                                                        
/DLC8/     IF  LIST-USERNM-FILTER-L > ZERO                                      
/DLC8/     OR  LIST-USERNM-FILTER-F = FLDMODFY OR FLDCURSM                      
/DLC8/         IF  LIST-USERNM-FILTER-I <= SPACES                               
/DLC8/         AND LIST-USERNM-FILTER-L  = ZERO                                 
/DLC8/             MOVE '*'            TO SAVE-USERNM-FILTER                    
/DLC8/         ELSE                                                             
/DLC8/             MOVE LIST-USERNM-FILTER-I                                    
/DLC8/                                 TO SAVE-USERNM-FILTER                    
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/     MOVE SAVE-USERNM-FILTER     TO LIST-USERNM-FILTER-O.                 
                                                                                
/DLC8/     IF  LIST-CLASS-FILTER-L > ZERO                                       
/DLC8/     OR  LIST-CLASS-FILTER-F = FLDMODFY OR FLDCURSM                       
/DLC8/         IF  LIST-CLASS-FILTER-I <= SPACES                                
/DLC8/         AND LIST-CLASS-FILTER-L  = ZERO                                  
/DLC8/             MOVE '*'            TO SAVE-CLASS-FILTER                     
/DLC8/         ELSE                                                             
/DLC8/             MOVE LIST-CLASS-FILTER-I                                     
/DLC8/                                 TO SAVE-CLASS-FILTER                     
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/     MOVE SAVE-CLASS-FILTER      TO LIST-CLASS-FILTER-O.                  
                                                                                
/DLC8/     IF  LIST-DAPSCO-FILTER-L > ZERO                                      
/DLC8/     OR  LIST-DAPSCO-FILTER-F = FLDMODFY OR FLDCURSM                      
/DLC8/         IF  LIST-DAPSCO-FILTER-I <= SPACES                               
/DLC8/         AND LIST-DAPSCO-FILTER-L  = ZERO                                 
/DLC8/             MOVE '*'            TO SAVE-DAPSCO-FILTER                    
/DLC8/         ELSE                                                             
/DLC8/             MOVE LIST-DAPSCO-FILTER-I                                    
/DLC8/                                 TO SAVE-DAPSCO-FILTER                    
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/     MOVE SAVE-DAPSCO-FILTER     TO LIST-DAPSCO-FILTER-O.                 
                                                                                
/DLC8/     IF  LIST-IND-LIST-FILTER-L > ZERO                                    
/DLC8/     OR  LIST-IND-LIST-FILTER-F = FLDMODFY OR FLDCURSM                    
/DLC8/         IF  LIST-IND-LIST-FILTER-I <= SPACES                             
/DLC8/         AND LIST-IND-LIST-FILTER-L  = ZERO                               
/DLC8/             MOVE '*'            TO SAVE-IND-LIST-FILTER                  
/DLC8/         ELSE                                                             
/DLC8/             MOVE LIST-IND-LIST-FILTER-I                                  
/DLC8/                                 TO SAVE-IND-LIST-FILTER                  
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/     MOVE SAVE-IND-LIST-FILTER   TO LIST-IND-LIST-FILTER-O.               
                                                                                
/DLC8/     IF  LIST-IND-AUTH-FILTER-L > ZERO                                    
/DLC8/     OR  LIST-IND-AUTH-FILTER-F = FLDMODFY OR FLDCURSM                    
/DLC8/         IF  LIST-IND-AUTH-FILTER-I <= SPACES                             
/DLC8/         AND LIST-IND-AUTH-FILTER-L  = ZERO                               
/DLC8/             MOVE '*'            TO SAVE-IND-AUTH-FILTER                  
/DLC8/         ELSE                                                             
/DLC8/             MOVE LIST-IND-AUTH-FILTER-I                                  
/DLC8/                                 TO SAVE-IND-AUTH-FILTER                  
/DLC8/         END-IF                                                           
/DLC8/     END-IF.                                                              
/DLC8/     MOVE SAVE-IND-AUTH-FILTER   TO LIST-IND-AUTH-FILTER-O.               
                                                                                
/DLC8/*DLC9IF  LIST-AP-SEC-FILTER-L > ZERO                                      
/DLC8/*DLC9OR  LIST-AP-SEC-FILTER-F = FLDMODFY OR FLDCURSM                      
/DLC8/*DLC9    IF  LIST-AP-SEC-FILTER-I <= SPACES                               
/DLC8/*DLC9    AND LIST-AP-SEC-FILTER-L  = ZERO                                 
/DLC8/*DLC9        MOVE '*'            TO SAVE-AP-SEC-FILTER                    
/DLC8/*DLC9    ELSE                                                             
/DLC8/*DLC9        MOVE LIST-AP-SEC-FILTER-I                                    
/DLC8/*DLC9                            TO SAVE-AP-SEC-FILTER                    
/DLC8/*DLC9    END-IF                                                           
/DLC8/*DLC9END-IF.                                                              
/DLC8/*DLC9MOVE SAVE-AP-SEC-FILTER     TO LIST-AP-SEC-FILTER-O.                 
                                                                                
/DLC9/     IF  LIST-PRIME-CO-FILTER-L > ZERO                                    
/DLC9/     OR  LIST-PRIME-CO-FILTER-F = FLDMODFY OR FLDCURSM                    
/DLC9/         IF  LIST-PRIME-CO-FILTER-I <= SPACES                             
/DLC9/         AND LIST-PRIME-CO-FILTER-L  = ZERO                               
/DLC9/             MOVE '*'            TO SAVE-PRIME-CO-FILTER                  
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-PRIME-CO-FILTER-I                                  
/DLC9/                                 TO SAVE-PRIME-CO-FILTER                  
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-PRIME-CO-FILTER   TO LIST-PRIME-CO-FILTER-O.               
                                                                                
/DLC9/     IF  LIST-PRIME-IND-FILTER-L > ZERO                                   
/DLC9/     OR  LIST-PRIME-IND-FILTER-F = FLDMODFY OR FLDCURSM                   
/DLC9/         IF  LIST-PRIME-IND-FILTER-I <= SPACES                            
/DLC9/         AND LIST-PRIME-IND-FILTER-L  = ZERO                              
/DLC9/             MOVE '*'            TO SAVE-PRIME-IND-FILTER                 
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-PRIME-IND-FILTER-I                                 
/DLC9/                                 TO SAVE-PRIME-IND-FILTER                 
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-PRIME-IND-FILTER  TO LIST-PRIME-IND-FILTER-O.              
                                                                                
/DLC9/     IF  LIST-WIN-UNIT-FILTER-L > ZERO                                    
/DLC9/     OR  LIST-WIN-UNIT-FILTER-F = FLDMODFY OR FLDCURSM                    
/DLC9/         IF  LIST-WIN-UNIT-FILTER-I <= SPACES                             
/DLC9/         AND LIST-WIN-UNIT-FILTER-L  = ZERO                               
/DLC9/             MOVE '*'            TO SAVE-WIN-UNIT-FILTER                  
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-WIN-UNIT-FILTER-I                                  
/DLC9/                                 TO SAVE-WIN-UNIT-FILTER                  
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-WIN-UNIT-FILTER   TO LIST-WIN-UNIT-FILTER-O.               
                                                                                
/DLC9/     IF  LIST-WIN-POS-FILTER-L > ZERO                                     
/DLC9/     OR  LIST-WIN-POS-FILTER-F = FLDMODFY OR FLDCURSM                     
/DLC9/         IF  LIST-WIN-POS-FILTER-I <= SPACES                              
/DLC9/         AND LIST-WIN-POS-FILTER-L  = ZERO                                
/DLC9/             MOVE '*'            TO SAVE-WIN-POS-FILTER                   
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-WIN-POS-FILTER-I                                   
/DLC9/                                 TO SAVE-WIN-POS-FILTER                   
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-WIN-POS-FILTER    TO LIST-WIN-POS-FILTER-O.                
                                                                                
/DLC9/     IF  LIST-WIN-ORG-FILTER-L > ZERO                                     
/DLC9/     OR  LIST-WIN-ORG-FILTER-F = FLDMODFY OR FLDCURSM                     
/DLC9/         IF  LIST-WIN-ORG-FILTER-I <= SPACES                              
/DLC9/         AND LIST-WIN-ORG-FILTER-L  = ZERO                                
/DLC9/             MOVE '*'            TO SAVE-WIN-ORG-FILTER                   
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-WIN-ORG-FILTER-I                                   
/DLC9/                                 TO SAVE-WIN-ORG-FILTER                   
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-WIN-ORG-FILTER    TO LIST-WIN-ORG-FILTER-O.                
                                                                                
/DLC9/     IF  LIST-WIN-FUNC-FILTER-L > ZERO                                    
/DLC9/     OR  LIST-WIN-FUNC-FILTER-F = FLDMODFY OR FLDCURSM                    
/DLC9/         IF  LIST-WIN-FUNC-FILTER-I <= SPACES                             
/DLC9/         AND LIST-WIN-FUNC-FILTER-L  = ZERO                               
/DLC9/             MOVE '*'            TO SAVE-WIN-FUNC-FILTER                  
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-WIN-FUNC-FILTER-I                                  
/DLC9/                                 TO SAVE-WIN-FUNC-FILTER                  
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-WIN-FUNC-FILTER   TO LIST-WIN-FUNC-FILTER-O.               
                                                                                
/DLC9/     IF  LIST-WIN-DIR-FILTER-L > ZERO                                     
/DLC9/     OR  LIST-WIN-DIR-FILTER-F = FLDMODFY OR FLDCURSM                     
/DLC9/         IF  LIST-WIN-DIR-FILTER-I <= SPACES                              
/DLC9/         AND LIST-WIN-DIR-FILTER-L  = ZERO                                
/DLC9/             MOVE '*'            TO SAVE-WIN-DIR-FILTER                   
/DLC9/         ELSE                                                             
/DLC9/             MOVE LIST-WIN-DIR-FILTER-I                                   
/DLC9/                                 TO SAVE-WIN-DIR-FILTER                   
/DLC9/         END-IF                                                           
/DLC9/     END-IF.                                                              
/DLC9/     MOVE SAVE-WIN-DIR-FILTER    TO LIST-WIN-DIR-FILTER-O.                
                                                                                
/DLCA/     IF  LIST-REGION-FILTER-L > ZERO                                      
/DLCA/     OR  LIST-REGION-FILTER-F = FLDMODFY OR FLDCURSM                      
/DLCA/         IF  LIST-REGION-FILTER-I <= SPACES                               
/DLCA/         AND LIST-REGION-FILTER-L  = ZERO                                 
/DLCA/             MOVE '*'            TO SAVE-REGION-FILTER                    
/DLCA/         ELSE                                                             
/DLCA/             MOVE LIST-REGION-FILTER-I                                    
/DLCA/                                 TO SAVE-REGION-FILTER                    
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
/DLCA/     MOVE SAVE-REGION-FILTER     TO LIST-REGION-FILTER-O.                 
                                                                                
/DLCA/     IF  LIST-REMOTE-FILTER-L > ZERO                                      
/DLCA/     OR  LIST-REMOTE-FILTER-F = FLDMODFY OR FLDCURSM                      
/DLCA/         IF  LIST-REMOTE-FILTER-I <= SPACES                               
/DLCA/         AND LIST-REMOTE-FILTER-L  = ZERO                                 
/DLCA/             MOVE '*'            TO SAVE-REMOTE-FILTER                    
/DLCA/         ELSE                                                             
/DLCA/             MOVE LIST-REMOTE-FILTER-I                                    
/DLCA/                                 TO SAVE-REMOTE-FILTER                    
/DLCA/         END-IF                                                           
/DLCA/     END-IF.                                                              
/DLCA/     MOVE SAVE-REMOTE-FILTER     TO LIST-REMOTE-FILTER-O.                 
                                                                                
/DLC8/ Q85-EXIT.                                                                
/DLC8/     EXIT.                                                                
                                                                                
/DLC8/ Q90-CHECK-FILTERS.                                                       
/DLC8/     SET  WILDCOMP-PGM           TO TRUE.                                 
/DLC8/     SET  RECORD-NOT-SELECTED    TO TRUE.                                 
                                                                                
/DLC8/     IF  SAVE-USERNM-FILTER NOT = '*'                                     
/DLC8/         MOVE LENGTH OF SAVE-USERNM-FILTER                                
/DLC8/                                 TO WILDCOMP-LEN                          
/DLC8/         EVALUATE SAVE-USERNM-FILTER                                      
/DLC8/         WHEN '>'                                                         
/DLC8/             IF  VUSER-NAME <= SPACES                                     
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         WHEN OTHER                                                       
/DLC8/             MOVE SAVE-USERNM-FILTER                                      
/DLC8/                                 TO WILDCOMP-STR1                         
/DLC8/             MOVE VUSER-NAME     TO WILDCOMP-STR2                         
/DLC8/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         END-EVALUATE                                                     
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     IF  SAVE-CLASS-FILTER NOT = '*'                                      
/DLC8/         MOVE LENGTH OF SAVE-CLASS-FILTER                                 
/DLC8/                                 TO WILDCOMP-LEN                          
/DLC8/         EVALUATE SAVE-CLASS-FILTER                                       
/DLC8/         WHEN '>'                                                         
/DLC8/             IF  VUSER-CLASS <= SPACES                                    
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         WHEN OTHER                                                       
/DLC8/             MOVE SAVE-CLASS-FILTER                                       
/DLC8/                                 TO WILDCOMP-STR1                         
/DLC8/             MOVE VUSER-CLASS    TO WILDCOMP-STR2                         
/DLC8/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         END-EVALUATE                                                     
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     IF  SAVE-DAPSCO-FILTER NOT = '*'                                     
/DLC8/         MOVE LENGTH OF SAVE-DAPSCO-FILTER                                
/DLC8/                                 TO WILDCOMP-LEN                          
/DLC8/         EVALUATE SAVE-DAPSCO-FILTER                                      
/DLC8/         WHEN '>'                                                         
/DLC8/             IF  VUSER-DAPSCO <= SPACES                                   
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         WHEN OTHER                                                       
/DLC8/             MOVE SAVE-DAPSCO-FILTER                                      
/DLC8/                                 TO WILDCOMP-STR1                         
/DLC8/             MOVE VUSER-DAPSCO   TO WILDCOMP-STR2                         
/DLC8/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         END-EVALUATE                                                     
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     IF  SAVE-IND-LIST-FILTER NOT = '*'                                   
/DLC8/         MOVE LENGTH OF SAVE-IND-LIST-FILTER                              
/DLC8/                                 TO WILDCOMP-LEN                          
/DLC8/         EVALUATE SAVE-IND-LIST-FILTER                                    
/DLC8/         WHEN '>'                                                         
/DLC8/             IF  VUSER-IND-LIST <= SPACES                                 
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         WHEN OTHER                                                       
/DLC8/             MOVE SAVE-IND-LIST-FILTER                                    
/DLC8/                                 TO WILDCOMP-STR1                         
/DLC8/             MOVE VUSER-IND-LIST TO WILDCOMP-STR2                         
/DLC8/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         END-EVALUATE                                                     
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/     IF  SAVE-IND-AUTH-FILTER NOT = '*'                                   
/DLC8/         MOVE LENGTH OF SAVE-IND-AUTH-FILTER                              
/DLC8/                                 TO WILDCOMP-LEN                          
/DLC8/         EVALUATE SAVE-IND-AUTH-FILTER                                    
/DLC8/         WHEN '>'                                                         
/DLC8/             IF  VUSER-IND-AUTH <= SPACES                                 
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         WHEN OTHER                                                       
/DLC8/             MOVE SAVE-IND-AUTH-FILTER                                    
/DLC8/                                 TO WILDCOMP-STR1                         
/DLC8/             MOVE VUSER-IND-AUTH TO WILDCOMP-STR2                         
/DLC8/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/                 GO TO Q95-EXIT                                           
/DLC8/             END-IF                                                       
/DLC8/         END-EVALUATE                                                     
/DLC8/     END-IF.                                                              
                                                                                
/DLC8/*DLC9IF  SAVE-AP-SEC-FILTER NOT = '*'                                     
/DLC8/*DLC9    MOVE LENGTH OF SAVE-AP-SEC-FILTER                                
/DLC8/*DLC9                            TO WILDCOMP-LEN                          
/DLC8/*DLC9    EVALUATE SAVE-AP-SEC-FILTER                                      
/DLC8/*DLC9    WHEN '>'                                                         
/DLC8/*DLC9        IF  VUSER-AP-SEC <= SPACES                                   
/DLC8/*DLC9            GO TO Q95-EXIT                                           
/DLC8/*DLC9        END-IF                                                       
/DLC8/*DLC9    WHEN OTHER                                                       
/DLC8/*DLC9        MOVE SAVE-AP-SEC-FILTER                                      
/DLC8/*DLC9                            TO WILDCOMP-STR1                         
/DLC8/*DLC9        MOVE VUSER-AP-SEC   TO WILDCOMP-STR2                         
/DLC8/*DLC9        CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC8/*DLC9        IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC8/*DLC9            GO TO Q95-EXIT                                           
/DLC8/*DLC9        END-IF                                                       
/DLC8/*DLC9    END-EVALUATE                                                     
/DLC8/*DLC9END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-PRIME-CO-FILTER NOT = '*'                                   
/DLC9/         MOVE LENGTH OF SAVE-PRIME-CO-FILTER                              
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-PRIME-CO-FILTER                                    
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-PRIMARY-CO <= SPACES                               
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-PRIME-CO-FILTER                                    
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-PRIMARY-CO                                        
/DLC9/                                 TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-PRIME-IND-FILTER NOT = '*'                                  
/DLC9/         MOVE LENGTH OF SAVE-PRIME-IND-FILTER                             
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-PRIME-IND-FILTER                                   
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-PRIMARY-IND <= SPACES                              
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-PRIME-IND-FILTER                                   
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-PRIMARY-IND                                       
/DLC9/                                 TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-WIN-UNIT-FILTER NOT = '*'                                   
/DLC9/         MOVE LENGTH OF SAVE-WIN-UNIT-FILTER                              
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-WIN-UNIT-FILTER                                    
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-WIN-UNIT <= SPACES                                 
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-WIN-UNIT-FILTER                                    
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-WIN-UNIT TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-WIN-POS-FILTER NOT = '*'                                    
/DLC9/         MOVE LENGTH OF SAVE-WIN-POS-FILTER                               
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-WIN-POS-FILTER                                     
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-WIN-POSITION <= SPACES                             
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-WIN-POS-FILTER                                     
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-WIN-POSITION                                      
/DLC9/                                 TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-WIN-ORG-FILTER NOT = '*'                                    
/DLC9/         MOVE LENGTH OF SAVE-WIN-ORG-FILTER                               
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-WIN-ORG-FILTER                                     
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-WIN-ORG <= SPACES                                  
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-WIN-ORG-FILTER                                     
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-WIN-ORG  TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-WIN-FUNC-FILTER NOT = '*'                                   
/DLC9/         MOVE LENGTH OF SAVE-WIN-FUNC-FILTER                              
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-WIN-FUNC-FILTER                                    
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-WIN-FUNCTION <= SPACES                             
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-WIN-FUNC-FILTER                                    
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-WIN-FUNCTION                                      
/DLC9/                                 TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLC9/     IF  SAVE-WIN-DIR-FILTER NOT = '*'                                    
/DLC9/         MOVE LENGTH OF SAVE-WIN-DIR-FILTER                               
/DLC9/                                 TO WILDCOMP-LEN                          
/DLC9/         EVALUATE SAVE-WIN-DIR-FILTER                                     
/DLC9/         WHEN '>'                                                         
/DLC9/             IF  VUSER-WIN-DIRECTOR <= SPACES                             
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         WHEN OTHER                                                       
/DLC9/             MOVE SAVE-WIN-DIR-FILTER                                     
/DLC9/                                 TO WILDCOMP-STR1                         
/DLC9/             MOVE VUSER-WIN-DIRECTOR                                      
/DLC9/                                 TO WILDCOMP-STR2                         
/DLC9/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLC9/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLC9/                 GO TO Q95-EXIT                                           
/DLC9/             END-IF                                                       
/DLC9/         END-EVALUATE                                                     
/DLC9/     END-IF.                                                              
                                                                                
/DLCA/     IF  SAVE-REGION-FILTER NOT = '*'                                     
/DLCA/         MOVE LENGTH OF SAVE-REGION-FILTER                                
/DLCA/                                 TO WILDCOMP-LEN                          
/DLCA/         EVALUATE SAVE-REGION-FILTER                                      
/DLCA/         WHEN '>'                                                         
/DLCA/             IF  VUSER-SVC-REGION <= SPACES                               
/DLCA/                 GO TO Q95-EXIT                                           
/DLCA/             END-IF                                                       
/DLCA/         WHEN OTHER                                                       
/DLCA/             MOVE SAVE-REGION-FILTER                                      
/DLCA/                                 TO WILDCOMP-STR1                         
/DLCA/             MOVE VUSER-SVC-REGION                                        
/DLCA/                                 TO WILDCOMP-STR2                         
/DLCA/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLCA/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLCA/                 GO TO Q95-EXIT                                           
/DLCA/             END-IF                                                       
/DLCA/         END-EVALUATE                                                     
/DLCA/     END-IF.                                                              
                                                                                
/DLCA/     IF  SAVE-REMOTE-FILTER NOT = '*'                                     
/DLCA/         MOVE LENGTH OF SAVE-REMOTE-FILTER                                
/DLCA/                                 TO WILDCOMP-LEN                          
/DLCA/         EVALUATE SAVE-REMOTE-FILTER                                      
/DLCA/         WHEN '>'                                                         
/DLCA/             IF  VUSER-RPT-REMOTE <= SPACES                               
/DLCA/                 GO TO Q95-EXIT                                           
/DLCA/             END-IF                                                       
/DLCA/         WHEN OTHER                                                       
/DLCA/             MOVE SAVE-REMOTE-FILTER                                      
/DLCA/                                 TO WILDCOMP-STR1                         
/DLCA/             MOVE VUSER-RPT-REMOTE                                        
/DLCA/                                 TO WILDCOMP-STR2                         
/DLCA/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS                        
/DLCA/             IF  NOT WILDCOMP-STR1-EQ-STR2                                
/DLCA/                 GO TO Q95-EXIT                                           
/DLCA/             END-IF                                                       
/DLCA/         END-EVALUATE                                                     
/DLCA/     END-IF.                                                              
                                                                                
/DLC8/     SET  RECORD-IS-SELECTED     TO TRUE.                                 
/DLC8/ Q95-EXIT.                                                                
/DLC8/     EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    UNEXPECTED ERRORS AND DEBUG LOGGING                         *        
      ******************************************************************        
       COPY LOGGINGP.                                                           
                                                                                
      /*****************************************************************        
      *    PROGRAM EXIT TO 2ND-LEVEL PGM OR RETURN TO 1ST-LEVEL PGM    *        
      ******************************************************************        
       COPY COMMLINK.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      ******************************************************************        
      *    TRANSFER TO THE CICS HELP SYSTEM                            *        
      ******************************************************************        
       COPY COMMHELP.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    VERIFICATION ROUTINE                                        *        
      ******************************************************************        
       V10-VERIFY-DELETE.                                                       
           MOVE AIDENTER               TO EIBAID.                               
           MOVE 1                      TO POS.                                  
      *                         row  adjust            col  adjust              
           COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (001 - 1).                
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'                             
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK                          
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.                
           STRING MULTI-EDIT-MSG(1:LEN)                                         
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.                
           EVALUATE TRUE                                                        
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2                              
      *                             row  adjust            col  adjust          
               COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (080 - 1)             
               STRING SCREPEAT TWO-BYTES X'40'                                  
                      SCRSTFLD FLDASKIP DELIMITED BY SIZE                       
                   INTO VAR-DATA WITH POINTER POS                               
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG                                  
               STRING SCRSTFLD FLDASKIP DELIMITED BY SIZE                       
                   INTO VAR-DATA WITH POINTER POS                               
           END-EVALUATE.                                                        
           SUBTRACT 1                FROM POS.                                  
                                                                                
           EXEC CICS CONVERSE                                                   
                     FROM     (VAR-DATA)                                        
                     FROMLENGTH(POS)                                            
                     CTLCHAR  (WRTFKFSA)                                        
                     NOHANDLE                                                   
           END-EXEC.                                                            
           IF  EIBRESP = DFHRESP(EOC)                                           
               MOVE DFHRESP(NORMAL)    TO EIBRESP                               
           END-IF.                                                              
       V15-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM ERRORS (UNEXPECTED)                                 *        
      ******************************************************************        
       COPY UNEXERRP.                                                           
      * the above copybook ends with a return to cics                           
                                                                                
      /*****************************************************************        
      *    PROGRAM TERMINATION (NORMAL)                                *        
      ******************************************************************        
                                                                                
       Z70-VUSR-CLOSED.                                                         
                                                                                
           COPY COMMXCTL.                                                       
      * the above transfers control to standard NOT OPEN screen                 
                                                                                
       Z80-VUSR-RETURN.                                                         
                                                                                
           COPY COMMRETN.                                                       
      * the above copybook is a psuedo-conversational return to cics            
                                                                                
       Z90-VUSR-TERMINATION.                                                    
                                                                                
/DLC7/     COMPUTE LINK-LEN = LENGTH OF COMM-HEADER                             
/DLC7/                      + LENGTH OF COMM-SHARED                             
/DLC7/                      + LENGTH OF VUSR-USERID.                            
/DLC7/*    COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.                  
/DLC7/     COPY COMMEXIT.                                                       
      * the above copybook ends with a return to cics                           
                                                                                

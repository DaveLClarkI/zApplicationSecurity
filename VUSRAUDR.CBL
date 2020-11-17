      ******************************************************************        
      *                                                                *        
      *    IDENTIFICATION DIVISION                                     *        
      *                                                                *        
      ******************************************************************        
       IDENTIFICATION DIVISION.                                                 
                                                                                
       PROGRAM-ID.    VSECAUDR.                                                 
       AUTHOR.        DAVE L CLARK I.                                           
       DATE-WRITTEN.  MARCH 2020.                                               
       DATE-COMPILED.                                                           
       INSTALLATION.  WINSUPPLY GROUP SERVICES.                                 
       SECURITY.      BATCH.                                                    
      *REMARKS.       zVSE USER SECURITY AUDITOR'S REPORT.                      
      *                                                                         
      * This program can select one or more users to report based on one        
      * of seven different selection mechanisms: WinFunction, WinUnit,          
      * WinPosition, IUI User type, Old DAPSCO Code (e.g., A or W), Old         
      * Classification code (00 to 99), or by User id (or *ALL).  This          
      * selection is made via the // EXEC PARM parameter.  Examples:            
      *                                                                         
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='FUNC=AC'                              
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='UNIT=AL'                              
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='POSN=SIC'                             
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='TYPE=1'                               
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='CODE=A'                               
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='CLASS=95'                             
      * // EXEC PGM=VUSRAUDR,SIZE=*,PARM='USER=*ALL'                            
      *                                                                         
      * For a selected user, this program lists all security resources          
      * to which the user has direct access.  Then, it lists all groups         
      * in which the user is a member plus all the security resources to        
      * which each group has access.  For this reason, reports can be           
      * quite large.  Be careful what you ask for.                              
      *                                                                         
                                                                                
      * CHANGE HISTORY ------------------------------------------------         
      * 03/13/2020 DLC ORIGINAL PROGRAM.                                        
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
                                                                                
      ******************************************************************        
      *    INPUT-OUTPUT SECTION                                        *        
      ******************************************************************        
       INPUT-OUTPUT SECTION.                                                    
                                                                                
       FILE-CONTROL.                                                            
                                                                                
           SELECT PRINTER                                                       
               ASSIGN TO SYS012-UR-IJSYSLS.                                     
                                                                                
           SELECT IESCNTL                                                       
               ASSIGN TO IESCNTL                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS IUI-KEY                                            
               FILE STATUS IS FILE1-STAT                                        
                              FILE1-FDBK.                                       
                                                                                
           SELECT VUSERID                                                       
               ASSIGN TO VUSERID                                                
               ACCESS IS RANDOM  INDEXED                                        
               RECORD KEY IS VUSER-KEY                                          
               FILE STATUS IS FILE2-STAT                                        
                              FILE2-FDBK.                                       
                                                                                
           SELECT BSM-SEC-CNTL                                                  
               ASSIGN TO BSTCNTL                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS BSM-SEC-KEY                                        
               FILE STATUS IS FILE3-STAT                                        
                              FILE3-FDBK.                                       
                                                                                
           SELECT BSM-GRP-CNTL                                                  
               ASSIGN TO BSTCNTL                                                
               ACCESS IS DYNAMIC INDEXED                                        
               RECORD KEY IS BSM-GRP-KEY                                        
               FILE STATUS IS FILE4-STAT                                        
                              FILE4-FDBK.                                       
                                                                                
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
                                                                                
       FD  PRINTER.                                                             
       01  PRINT-LINE.                                                          
         03  PRINT-CC                  PIC  X.                                  
         03  PRINT-DATA                PIC  X(132).                             
                                                                                
       FD  IESCNTL                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 100 TO 1000 CHARACTERS                                        
             DEPENDING ON IUI-RECL.                                             
       COPY IESCNTL.                                                            
                                                                                
       FD  VUSERID                                                              
           RECORD IS VARYING IN SIZE                                            
             FROM 127 TO 174 CHARACTERS                                         
             DEPENDING ON VUSR-RECL.                                            
       COPY VUSERID.                                                            
                                                                                
       FD  BSM-SEC-CNTL                                                         
           RECORD IS VARYING IN SIZE                                            
             FROM 280 TO 65535 CHARACTERS                                       
             DEPENDING ON BSM-RECL.                                             
       01  BSM-SEC-RECORD.                                               162    
         03                            PIC  X(4).                               
         03  BSM-SEC-KEY.                                                       
           05  BSM-SEC-CLASS           PIC  X(8).                               
             88  BSM-APPL                           VALUE 'APPL'.               
             88  BSM-CICS-PCT                       VALUE 'ACICSPCT'.           
             88  BSM-CICS-DCT                       VALUE 'DCICSDCT'.           
             88  BSM-CICS-FCT                       VALUE 'FCICSFCT'.           
             88  BSM-CICS-JCT                       VALUE 'JCICSJCT'.           
             88  BSM-CICS-PPT                       VALUE 'MCICSPPT'.           
             88  BSM-CICS-TST                       VALUE 'SCICSTST'.           
             88  BSM-TRANS                          VALUE 'TCICSTRN'.           
             88  BSM-FACILITY                       VALUE 'FACILITY'.           
             88  BSM-SEC-GROUP                      VALUE 'GROUP'.              
           05  BSM-SEC-RSCNAME         PIC  X(246).                             
           05  BSM-SEC-SEQU            PIC  X(1).                               
             88  BSM-SEC-GENERIC                    VALUE X'80'.                
         03  BSM-SEC-RSCLEN            PIC  X(1).                               
         03  BSM-SEC-DESC              PIC  X(20).                              
         03  BSM-SEC-UA-FLAG           PIC  X(1).                               
           88  BSM-SEC-UA-DENIED                    VALUE X'00'.                
           88  BSM-SEC-UA-READ                      VALUE X'20'.                
           88  BSM-SEC-UA-UPDATE                    VALUE X'40'.                
           88  BSM-SEC-UA-ALTER                     VALUE X'80'.                
         03  BSM-SEC-LOGGING1          PIC  X(2).                               
         03  BSM-SEC-LOGGING2          PIC  X(2).                               
         03  BSM-SEC-FILLER            PIC  X(1).                               
         03  BSM-SEC-PERMITS           PIC S9(04)   BINARY.                     
         03  BSM-SEC-PERMIT                         OCCURS 5437                 
                                                    DEPENDING ON                
                                                    BSM-SEC-PERMITS.            
           05  BSM-SEC-USERID          PIC  X(8).                               
           05  BSM-SEC-AC-FLAG         PIC  X(1).                               
             88  BSM-SEC-AC-DENIED                  VALUE X'00'.                
             88  BSM-SEC-AC-READ                    VALUE X'20'.                
             88  BSM-SEC-AC-UPDATE                  VALUE X'40'.                
             88  BSM-SEC-AC-ALTER                   VALUE X'80'.                
           05                          PIC  X(3).                               
                                                                                
       FD  BSM-GRP-CNTL                                                         
           RECORD IS VARYING IN SIZE                                            
             FROM 280 TO 65535 CHARACTERS                                       
             DEPENDING ON BSM-RECL.                                             
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
         03  THIS-PGM                  PIC  X(8)    VALUE 'VSECAUDR'.           
                                                                                
         03  IUI                       PIC S9(04)   BINARY VALUE 1.             
         03  VUSR                      PIC S9(04)   BINARY VALUE 2.             
         03  SEC                       PIC S9(04)   BINARY VALUE 3.             
         03  GRP                       PIC S9(04)   BINARY VALUE 4.             
                                                                                
         03  SUB                       PIC S9(04)   BINARY.                     
         03  IDX                       PIC S9(04)   BINARY.                     
         03  POS                       PIC S9(04)   BINARY.                     
         03  NAME-LENGTH               PIC S9(04)   BINARY.                     
                                                                                
         03  ACCESS-SWITCH             PIC  X(03).                              
           88  DIRECT-ACCESS                        VALUE 'DIR'.                
           88  GROUP-ACCESS                         VALUE 'GRP'.                
         03  SUBHEAD-SWITCH            PIC  X(01).                              
           88  SUBHEAD-PRINTED                      VALUE 'Y'.                  
           88  SUBHEAD-NEEDED                       VALUE 'N'.                  
         03  IUI-RECL                  PIC  9(05)   VALUE 1000.                 
         03  VUSR-RECL                 PIC  9(05)   VALUE 174.                  
         03  BSM-RECL                  PIC  9(05)   VALUE 65535.                
                                                                                
         03  WORK-USRIDNT              PIC  X(08).                              
         03  WORK-USRNAME              PIC  X(20).                              
         03  WORK-USRTYPE              PIC  X(01).                              
           88  IS-ADMINISTRATOR                     VALUE '1'.                  
           88  IS-PROGRAMMER                        VALUE '2'.                  
           88  IS-GENERAL-USER                      VALUE '3'.                  
         03  WORK-ICCFUSR              PIC  X(01).                              
           88  IS-ICCF-USER                         VALUE 'Y'.                  
           88  NOT-ICCF-USER                        VALUE 'N'.                  
         03  WORK-SIGNON               PIC  X(08).                              
         03  WORK-EXPIRED              PIC  X(08).                              
         03  WORK-REVOKED              PIC  X(08).                              
                                                                                
         03  GRP-ID.                                                            
           05                          PIC  X(02)   VALUE LOW-VALUES.           
           05                          PIC  X(06)   VALUE 'GrpUID'.             
                                                                                
         03  GRP-NAME                  PIC  X(08)   VALUE SPACES.               
         03  GRP-DESC                  PIC  X(20)   VALUE SPACES.               
         03  CURRENT-USER              PIC  X(08)   VALUE SPACES.               
                                                                                
         03  WIN-FUNC                  PIC  X(03)   VALUE SPACES.               
         03  WIN-UNIT                  PIC  X(10)   VALUE SPACES.               
         03  WIN-POSN                  PIC  X(05)   VALUE SPACES.               
         03  USR-TYPE                  PIC  X(01)   VALUE SPACES.               
         03  OLD-CODE                  PIC  X(01)   VALUE SPACES.               
         03  OLD-CLASS                 PIC  X(02)   VALUE SPACES.               
         03  USER-ID                   PIC  X(08)   VALUE SPACES.               
                                                                                
       01  UNSTRING-AREA.                                                       
         03  PARM-KEYWORD              PIC  X(8).                               
         03  PARM-KEYWLEN              PIC S9(4)    BINARY.                     
         03  PARM-EQUALS               PIC  X(1).                               
         03  PARM-DATA                 PIC  X(30).                              
         03  PARM-DATALEN              PIC S9(4)    BINARY.                     
         03  PARM-SPACE                PIC  X(1).                               
                                                                                
       01  PRINT-CONTROL.                                                       
         03  LINES-COUNT               PIC S9(03)   PACKED-DECIMAL.             
         03  LINE-MAXIMUM              PIC S9(03)   COMP-3 VALUE 66.            
         03  LINE-NUMBER               PIC S9(03)   COMP-3 VALUE 99.            
         03  PAGE-NUMBER               PIC S9(05)   COMP-3 VALUE ZEROES.        
         03  PRINTER-SWITCH            PIC  A(1)    VALUE 'C'.                  
           88  PRINTER-CLOSED                       VALUE 'C'.                  
           88  PRINTER-OPEN                         VALUE 'O'.                  
         03  SAVE-PRINT-LINE           PIC  X(133).                             
                                                                                
       01  HEAD-1.                                                              
         03                            PIC  X(01)   VALUE SPACE.                
         03  HEAD-CAI-INFO.                                                     
           05  HEAD-CO-NUM             PIC  X(05)   VALUE SPACES.               
           05                          PIC  X(01)   VALUE '-'.                  
           05  HEAD-CO-AREA            PIC  X(03)   VALUE SPACES.               
           05                          PIC  X(01)   VALUE '-'.                  
           05  HEAD-CO-RGN             PIC  X(02)   VALUE SPACES.               
           05                          PIC  X(01)   VALUE '-'.                  
           05  HEAD-CO-STATE           PIC  X(02)   VALUE SPACES.               
           05                          PIC  X(01)   VALUE '-'.                  
           05  HEAD-CO-IND             PIC  X(34)   VALUE SPACES.               
         03  HEAD-CO-NAME              PIC  X(71)   VALUE SPACES.               
         03  HEAD-RUN-DATE             PIC  B99/99/9999.                        
       01  HEAD-2.                                                              
         03                            PIC  X(01)   VALUE SPACE.                
         03  HEAD-PPV-INFO                          VALUE SPACES.               
           05  HEAD-POLICY             PIC  X(11).                              
           05  HEAD-PERIOD             PIC  9999/99B.                           
           05  HEAD-VERSION            PIC  X(31).                              
         03  HEAD-TITLE                PIC  X(74)   VALUE SPACES.               
         03  HEAD-RUN-TIME             PIC  X(08).                              
       01  HEAD-3.                                                              
         03                            PIC  X(01)   VALUE SPACE.                
         03  HEAD-PGM-INFO                          VALUE SPACES.               
           05  HEAD-PGM-NAME           PIC  X(11).                              
           05  HEAD-RPT-NAME           PIC  X(39).                              
         03  HEAD-SUB-TITLE            PIC  X(71)   VALUE SPACES.               
         03                            PIC  X(06)   VALUE ' PAGE '.             
         03  HEAD-PAGE-NO              PIC  ZZZZ9.                              
       01  HEAD-4.                                                              
         03                            PIC  X(01)   VALUE SPACE.                
         03                            PIC  X(50)   VALUE SPACES.               
         03  HEAD-RUN-INFO             PIC  X(71)   VALUE SPACES.               
         03                            PIC  X(11)   VALUE SPACES.               
                                                                                
       01  COLH-1.                                                              
         03                            PIC  X       VALUE '0'.                  
         03                            PIC  X(09)   VALUE '  User  '.           
         03                            PIC  X(41)   VALUE SPACES.               
         03                            PIC  X(06)   VALUE 'User'.               
         03                            PIC  X(10)   VALUE '  Last  '.           
         03                            PIC  X(10)   VALUE ' Expire '.           
         03                            PIC  X(13)   VALUE ' Revoke '.           
         03                            PIC  X(13)   VALUE '-Primary-'.          
         03                            PIC  X(30)   VALUE                       
             '----------- W I N ------------'.                                  
       01  COLH-2.                                                              
         03                            PIC  X       VALUE SPACE.                
         03                            PIC  X(09)   VALUE '   id   '.           
         03                            PIC  X(41)   VALUE                       
             '               User name                '.                        
         03                            PIC  X(06)   VALUE 'Type'.               
         03                            PIC  X(10)   VALUE '   on   '.           
         03                            PIC  X(10)   VALUE '  date  '.           
         03                            PIC  X(13)   VALUE '  date  '.           
         03                            PIC  X(06)   VALUE 'Comp.'.              
         03                            PIC  X(07)   VALUE 'Ind'.                
         03                            PIC  X(04)   VALUE 'Fnc'.                
         03                            PIC  X(11)   VALUE '   Unit   '.         
         03                            PIC  X(06)   VALUE ' Org '.              
         03                            PIC  X(06)   VALUE 'Posn.'.              
         03                            PIC  X(03)   VALUE 'Dir'.                
       01  COLH-3.                                                              
         03                            PIC  X       VALUE SPACE.                
         03                            PIC  X(09)   VALUE '--------'.           
         03                            PIC  X(41)   VALUE                       
             '----------------------------------------'.                        
         03                            PIC  X(06)   VALUE '----'.               
         03                            PIC  X(10)   VALUE '--------'.           
         03                            PIC  X(10)   VALUE '--------'.           
         03                            PIC  X(13)   VALUE '--------'.           
         03                            PIC  X(06)   VALUE '-----'.              
         03                            PIC  X(07)   VALUE '---'.                
         03                            PIC  X(04)   VALUE '---'.                
         03                            PIC  X(11)   VALUE '----------'.         
         03                            PIC  X(06)   VALUE '-----'.              
         03                            PIC  X(06)   VALUE '-----'.              
         03                            PIC  X(03)   VALUE '---'.                
                                                                                
       01  PRT-DETAIL                               VALUE SPACES.               
         03                            PIC  X.                                  
         03  PRT-USRIDNT               PIC  X(08).                              
         03                            PIC  X.                                  
         03  PRT-USRNAME               PIC  X(40).                              
         03                            PIC  XX.                                 
         03  PRT-USRTYPE               PIC  X(01).                              
         03                            PIC  XXXX.                               
         03  PRT-SIGNON                PIC  X(08).                              
         03                            PIC  XX.                                 
         03  PRT-EXPIRED               PIC  X(08).                              
         03                            PIC  XX.                                 
         03  PRT-REVOKED               PIC  X(08).                              
         03                            PIC  XXXXX.                              
         03  PRT-PRIMARY-CO            PIC  X(05).                              
         03                            PIC  XX.                                 
         03  PRT-PRIMARY-IND           PIC  X(01).                              
         03                            PIC  XXXXX.                              
         03  PRT-WIN-FUNCTION          PIC  X(03).                              
         03                            PIC  X.                                  
         03  PRT-WIN-UNIT              PIC  X(10).                              
         03                            PIC  X.                                  
         03  PRT-WIN-ORG               PIC  X(05).                              
         03                            PIC  X.                                  
         03  PRT-WIN-POSITION          PIC  X(05).                              
         03                            PIC  XX.                                 
         03  PRT-WIN-DIRECTOR          PIC  X(01).                              
         03                            PIC  X.                                  
                                                                                
       01  SUBH-1.                                                              
         03                            PIC  X       VALUE '0'.                  
         03                            PIC  X(09)   VALUE SPACES.               
         03                            PIC  X(07)   VALUE 'Type'.               
         03                            PIC  X(11)   VALUE ' Class  '.           
         03                            PIC  X(51)   VALUE                       
             '               Resource name'.                                    
         03                            PIC  X(23)   VALUE                       
             '    Description     '.                                            
         03                            PIC  X(09)   VALUE 'UAccs.'.             
         03                            PIC  X(06)   VALUE 'Access'.             
       01  SUBH-2.                                                              
         03                            PIC  X       VALUE SPACE.                
         03                            PIC  X(09)   VALUE SPACES.               
         03                            PIC  X(07)   VALUE '----'.               
         03                            PIC  X(11)   VALUE '--------'.           
         03                            PIC  X(51)   VALUE                       
             '------------------------------------------------'.                
         03                            PIC  X(23)   VALUE                       
             '--------------------'.                                            
         03                            PIC  X(09)   VALUE '------'.             
         03                            PIC  X(06)   VALUE '------'.             
                                                                                
       01  SEC-DETAIL                               VALUE SPACES.               
         03                            PIC  X.                                  
         03                            PIC  X(09).                              
         03  SEC-TYPE                  PIC  X(07).                              
         03  SEC-CLASS                 PIC  X(11).                              
         03  SEC-RSCNAME               PIC  X(51).                              
         03  SEC-RSCDESC               PIC  X(25).                              
         03  SEC-UACCESS               PIC  X(09).                              
         03  SEC-ACCESS                PIC  X(04).                              
                                                                                
       COPY DATETIME.                                                           
       COPY BINARIES.                                                           
                                                                                
       COPY BITMAN.                                                             
                                                                                
       COPY DTEMAN.                                                             
                                                                                
       COPY RTCMAN.                                                             
                                                                                
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
                                                                                
           PERFORM C00-PROCESS-IESCNTL THRU C99-EXIT                            
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
             EVALUATE TRUE                                                      
             WHEN EXEC-PLEN <= ZERO                                             
               DISPLAY THIS-PGM ': EXEC PARM MISSING'                           
                                     UPON CONSOLE                               
               SET  RTC-REQUEST-CANCEL TO TRUE                                  
               MOVE +16                TO RTC-CODE                              
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT                            
             WHEN EXEC-PLEN <= 5                                                
               DISPLAY THIS-PGM ': EXEC PARM INVALID - '                        
                       '"' EXEC-PARM(1:EXEC-PLEN) '"'                           
                                     UPON CONSOLE                               
               SET  RTC-REQUEST-CANCEL TO TRUE                                  
               MOVE +16                TO RTC-CODE                              
               PERFORM B80-CALL-RTCMAN THRU B85-EXIT                            
             WHEN OTHER                                                         
               UNSTRING EXEC-PARM(1:EXEC-PLEN)                                  
                 DELIMITED BY '=' OR ALL SPACES INTO                            
                   PARM-KEYWORD                                                 
                     DELIMITER         IN PARM-EQUALS                           
                     COUNT             IN PARM-KEYWLEN                          
                   PARM-DATA                                                    
                     DELIMITER         IN PARM-SPACE                            
                     COUNT             IN PARM-DATALEN                          
               END-UNSTRING                                                     
               IF  PARM-KEYWORD <= SPACES                                       
               OR  PARM-DATA    <= SPACES                                       
               OR  PARM-KEYWLEN > 5                                             
               OR  PARM-DATALEN > 10                                            
               OR  PARM-EQUALS  NOT = '='                                       
               OR  PARM-SPACE   NOT = ' '                                       
               OR (PARM-KEYWORD = 'FUNC'                                        
               AND PARM-DATALEN > LENGTH OF WIN-FUNC)                           
               OR (PARM-KEYWORD = 'UNIT'                                        
               AND PARM-DATALEN > LENGTH OF WIN-UNIT)                           
               OR (PARM-KEYWORD = 'POSN'                                        
               AND PARM-DATALEN > LENGTH OF WIN-POSN)                           
               OR (PARM-KEYWORD = 'TYPE'                                        
               AND PARM-DATALEN > LENGTH OF USR-TYPE)                           
               OR (PARM-KEYWORD = 'CODE'                                        
               AND PARM-DATALEN > LENGTH OF OLD-CODE)                           
               OR (PARM-KEYWORD = 'CLASS'                                       
               AND PARM-DATALEN > LENGTH OF OLD-CLASS)                          
               OR (PARM-KEYWORD = 'USER'                                        
               AND PARM-DATALEN > LENGTH OF USER-ID)                            
                 DISPLAY THIS-PGM ': EXEC PARM WRONG FORMAT - '                 
                         '"' EXEC-PARM(1:EXEC-PLEN) '"'                         
                                       UPON CONSOLE                             
                 SET  RTC-REQUEST-CANCEL TO TRUE                                
                 MOVE +16                TO RTC-CODE                            
                 PERFORM B80-CALL-RTCMAN THRU B85-EXIT                          
               END-IF                                                           
               EVALUATE PARM-KEYWORD                                            
               WHEN 'FUNC'  MOVE PARM-DATA TO WIN-FUNC                          
               WHEN 'UNIT'  MOVE PARM-DATA TO WIN-UNIT                          
               WHEN 'POSN'  MOVE PARM-DATA TO WIN-POSN                          
               WHEN 'TYPE'  MOVE PARM-DATA TO USR-TYPE                          
               WHEN 'CODE'  MOVE PARM-DATA TO OLD-CODE                          
               WHEN 'CLASS' MOVE PARM-DATA TO OLD-CLASS                         
               WHEN 'USER'  MOVE PARM-DATA TO USER-ID                           
               WHEN OTHER                                                       
                 DISPLAY THIS-PGM ': SELECTION INVALID - '                      
                         '"' EXEC-PARM(1:EXEC-PLEN) '"'                         
                                     UPON CONSOLE                               
                 SET  RTC-REQUEST-CANCEL                                        
                                       TO TRUE                                  
                 MOVE +16              TO RTC-CODE                              
                 PERFORM B80-CALL-RTCMAN THRU B85-EXIT                          
               END-EVALUATE                                                     
             END-EVALUATE                                                       
             DISPLAY THIS-PGM ': EXEC PARM'                                     
                     ' ' PARM-KEYWORD(1:PARM-KEYWLEN) ' = '                     
                     '"' PARM-DATA(1:PARM-DATALEN) '"'                          
                     ' ACCEPTED'     UPON CONSOLE                               
           END-IF.                                                              
                                                                                
           IF  RTC-CODE = ZERO                                                  
                                                                                
             MOVE 'IESCNTL'            TO VSAM-FILE(IUI)                        
             MOVE 'VUSERID'            TO VSAM-FILE(VUSR)                       
             MOVE 'SECCNTL'            TO VSAM-FILE(SEC)                        
             MOVE 'GRPCNTL'            TO VSAM-FILE(GRP)                        
                                                                                
             MOVE LENGTH OF IUI-KEY    TO VSAM-KEYL(IUI)                        
             MOVE LENGTH OF VUSER-KEY  TO VSAM-KEYL(VUSR)                       
             MOVE 48                   TO VSAM-KEYL(SEC)                        
             MOVE 32                   TO VSAM-KEYL(GRP)                        
                                                                                
             PERFORM WITH TEST BEFORE                                           
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                 SET FUNC-OPEN(VSUB)   TO TRUE                                  
             END-PERFORM                                                        
                                                                                
             OPEN INPUT  IESCNTL                                                
                         VUSERID                                                
                         BSM-SEC-CNTL                                           
                         BSM-GRP-CNTL                                           
                                                                                
             PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                       
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                                                                                
           END-IF.                                                              
                                                                                
           MOVE FUNCTION CURRENT-DATE  TO WRK-DATETIME.                         
           MOVE CORR ISO-DATE          TO USA-DATE.                             
           MOVE NUM-DATE               TO HEAD-RUN-DATE.                        
           MOVE NUM-TIME               TO ISO-DSP-TIME.                         
           INSPECT WRK-DSP-TIME REPLACING ALL SPACES BY ':'.                    
           IF  WRK-DSP-HH > 11                                                  
             MOVE ' PM'                TO WRK-DSP-AM-PM                         
             IF  WRK-DSP-HH > 12                                                
               SUBTRACT 12           FROM WRK-DSP-HH                            
             END-IF                                                             
           ELSE                                                                 
             MOVE ' AM'                TO WRK-DSP-AM-PM                         
             IF  WRK-DSP-HH = 00                                                
               MOVE     12             TO WRK-DSP-HH                            
             END-IF                                                             
           END-IF.                                                              
           MOVE WRK-DSP-TIME           TO HEAD-RUN-TIME.                        
                                                                                
           MOVE SPACES                 TO HEAD-CAI-INFO                         
                                          HEAD-PPV-INFO.                        
           MOVE THIS-PGM               TO HEAD-PGM-NAME.                        
                                                                                
           MOVE 'WINSUPPLY GROUP SERVICES                '                      
                                       TO HEAD-TITLE.                           
           MOVE 'zVSE USER SECURITY AUDITOR''S REPORT    '                      
                                       TO HEAD-SUB-TITLE.                       
           MOVE 'RUN TYPE: R- REGULAR                    '                      
                                       TO HEAD-RUN-INFO.                        
                                                                                
           IF  RTC-CODE = ZERO                                                  
                                                                                
             PERFORM WITH TEST BEFORE                                           
               VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                  
                 SET  FUNC-READ(VSUB)  TO TRUE                                  
             END-PERFORM                                                        
                                                                                
             MOVE LOW-VALUES           TO IESCNTL-RECORD                        
             SET  IUI-USER-PROFILE     TO TRUE                                  
             SET  FUNC-START(IUI)      TO TRUE                                  
             START IESCNTL KEY >= IUI-KEY END-START                             
             IF  NOT STAT-NORMAL(IUI)                                           
               MOVE IUI                TO VSUB                                  
               MOVE IUI-KEY            TO VSAM-KEYD(VSUB)                       
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                     
             ELSE                                                               
               SET  FUNC-READNEXT(IUI) TO TRUE                                  
             END-IF                                                             
                                                                                
           END-IF.                                                              
                                                                                
       B15-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PROGRAM TERMINATION ROUTINE                                 *        
      ******************************************************************        
       B20-TERMINATION.                                                         
                                                                                
           IF  PRINTER-OPEN                                                     
               CLOSE PRINTER                                                    
               SET  PRINTER-CLOSED     TO TRUE                                  
           END-IF.                                                              
                                                                                
           PERFORM WITH TEST BEFORE                                             
             VARYING VSUB FROM 1 BY 1 UNTIL VSUB > STAT-TOTL                    
               SET  FUNC-CLOSE(VSUB)   TO TRUE                                  
           END-PERFORM.                                                         
                                                                                
           CLOSE IESCNTL                                                        
                 VUSERID                                                        
                 BSM-SEC-CNTL                                                   
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
                                                                                
       C00-PROCESS-IESCNTL.                                                     
                                                                                
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
                                                                                
           PERFORM P10-IMPORT-CONTROL-DATA                                      
                                     THRU P15-EXIT.                             
                                                                                
           IF  IS-ICCF-USER                                                     
           AND NAME-LENGTH <= 4                                                 
               GO TO C00-PROCESS-IESCNTL                                        
           END-IF.                                                              
                                                                                
           IF  PARM-KEYWORD = 'TYPE'                                            
           AND WORK-USRTYPE       NOT = USR-TYPE                                
           OR  PARM-KEYWORD = 'USER'                                            
           AND WORK-USRIDNT       NOT = USER-ID                                 
           AND USER-ID            NOT = '*ALL'                                  
               GO TO C00-PROCESS-IESCNTL                                        
           END-IF.                                                              
                                                                                
           MOVE WORK-USRIDNT           TO VUSER-KEY.                            
           READ VUSERID RECORD END-READ.                                        
           IF  NOT STAT-NORMAL(VUSR)                                            
               IF  NOT STAT-NOTFND(VUSR)                                        
                   MOVE VUSR           TO VSUB                                  
                   MOVE VUSER-KEY      TO VSAM-KEYD(VSUB)                       
                   PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                 
                   GO TO C99-EXIT                                               
               END-IF                                                           
               MOVE SPACES             TO VUSERID-RECORD                        
           END-IF.                                                              
                                                                                
           IF  PARM-KEYWORD = 'FUNC'                                            
           AND VUSER-WIN-FUNCTION NOT = WIN-FUNC                                
           OR  PARM-KEYWORD = 'UNIT'                                            
           AND VUSER-WIN-UNIT     NOT = WIN-UNIT                                
           OR  PARM-KEYWORD = 'POSN'                                            
           AND VUSER-WIN-POSITION NOT = WIN-POSN                                
           OR  PARM-KEYWORD = 'CODE'                                            
           AND VUSER-DAPSCO       NOT = OLD-CODE                                
           OR  PARM-KEYWORD = 'CLASS'                                           
           AND VUSER-CLASS        NOT = OLD-CLASS                               
               GO TO C00-PROCESS-IESCNTL                                        
           END-IF.                                                              
                                                                                
      * report selected users...                                                
           MOVE WORK-USRIDNT           TO PRT-USRIDNT                           
                                          CURRENT-USER.                         
           MOVE WORK-USRNAME           TO PRT-USRNAME.                          
           MOVE WORK-USRTYPE           TO PRT-USRTYPE.                          
           MOVE WORK-SIGNON            TO PRT-SIGNON.                           
           MOVE WORK-EXPIRED           TO PRT-EXPIRED.                          
           MOVE WORK-REVOKED           TO PRT-REVOKED.                          
           MOVE VUSER-PRIMARY-CO       TO PRT-PRIMARY-CO.                       
           MOVE VUSER-PRIMARY-IND      TO PRT-PRIMARY-IND.                      
           MOVE VUSER-WIN-FUNCTION     TO PRT-WIN-FUNCTION.                     
           MOVE VUSER-WIN-UNIT         TO PRT-WIN-UNIT.                         
           MOVE VUSER-WIN-ORG          TO PRT-WIN-ORG.                          
           MOVE VUSER-WIN-POSITION     TO PRT-WIN-POSITION.                     
           MOVE VUSER-WIN-DIRECTOR     TO PRT-WIN-DIRECTOR.                     
                                                                                
      * ... each user on a new page...                                          
           SET  SUBHEAD-NEEDED         TO TRUE.                                 
           MOVE +99                    TO LINE-NUMBER.                          
           MOVE PRT-DETAIL             TO PRINT-LINE.                           
           PERFORM U20-PUT-PRINT     THRU U25-EXIT.                             
                                                                                
      * ... and all related security resource direct access...                  
           SET  DIRECT-ACCESS          TO TRUE.                                 
           MOVE ACCESS-SWITCH          TO SEC-TYPE.                             
           PERFORM P20-GET-SECURITY-RESOURCES THRU P25-EXIT.                    
           IF  NOT STAT-NORMAL(SEC)                                             
           AND NOT STAT-EOFILE(SEC)                                             
               MOVE SEC                TO VSUB                                  
               MOVE BSM-SEC-KEY        TO VSAM-KEYD(VSUB)                       
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                     
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
      * ... and all related security resource group access...                   
           SET  GROUP-ACCESS           TO TRUE.                                 
           MOVE LOW-VALUES             TO BSM-GRP-RECORD.                       
           SET  BSM-GROUP              TO TRUE.                                 
           SET  FUNC-START(GRP)        TO TRUE.                                 
           START BSM-GRP-CNTL KEY >= BSM-GRP-KEY END-START.                     
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT STAT-NORMAL(GRP)                                         
                OR NOT BSM-GROUP                                                
                OR RTC-CODE > ZERO                                              
                                                                                
             SET  FUNC-READNEXT(GRP)   TO TRUE                                  
             READ BSM-GRP-CNTL NEXT RECORD END-READ                             
             IF  STAT-NORMAL(GRP)                                               
             AND BSM-GROUP                                                      
                                                                                
               EVALUATE TRUE                                                    
               WHEN BSM-GRP-MEMBER = GRP-ID                                     
                 MOVE ACCESS-SWITCH    TO SEC-TYPE                              
                 MOVE BSM-GRP-DESC     TO GRP-DESC                              
                                                                                
      * ... where the current user is in the group...                           
               WHEN BSM-GRP-MEMBER = WORK-USRIDNT                               
                 MOVE BSM-GRP-CLASS    TO SEC-CLASS                             
                 MOVE BSM-GRP-NAME     TO SEC-RSCNAME                           
                                          CURRENT-USER                          
                 MOVE GRP-DESC         TO SEC-RSCDESC                           
                 MOVE SPACE            TO SEC-UACCESS                           
                                          SEC-ACCESS                            
                 MOVE SEC-DETAIL       TO PRINT-LINE                            
                 PERFORM U20-PUT-PRINT THRU U25-EXIT                            
                 MOVE SPACES           TO SEC-DETAIL                            
                 PERFORM P20-GET-SECURITY-RESOURCES THRU P25-EXIT               
               END-EVALUATE                                                     
                                                                                
             END-IF                                                             
           END-PERFORM.                                                         
                                                                                
           IF  NOT STAT-NORMAL(GRP)                                             
           AND NOT STAT-EOFILE(GRP)                                             
               MOVE GRP                TO VSUB                                  
               MOVE BSM-SEC-KEY        TO VSAM-KEYD(VSUB)                       
               PERFORM B90-CHECK-STATUS THRU B95-EXIT-CHECK                     
               GO TO C99-EXIT                                                   
           END-IF.                                                              
                                                                                
       C99-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    PERFORMED ROUTINES                                          *        
      ******************************************************************        
                                                                                
       P10-IMPORT-CONTROL-DATA.                                                 
           MOVE SPACES                 TO WORK-USRIDNT.                         
           MOVE 1                      TO POS.                                  
           STRING IUI-US-USRIDNT          DELIMITED BY LOW-VALUE                
                                     INTO WORK-USRIDNT.                         
           SUBTRACT 1 FROM POS     GIVING NAME-LENGTH.                          
                                                                                
           MOVE IUI-US-USRNAME         TO WORK-USRNAME.                         
                                                                                
      * extract settings for IUI information                                    
           SET  BIT-DECODE-FROM-BYTE   TO TRUE.                                 
           MOVE IUI-US-IESINFO         TO BIT-BYTE.                             
           CALL BITMAN              USING BITMAN-PARMS.                         
           EVALUATE TRUE                                                        
             WHEN BIT-0-IS-ON  AND BIT-1-IS-ON                                  
               SET  IS-ADMINISTRATOR   TO TRUE                                  
             WHEN BIT-0-IS-OFF AND BIT-1-IS-ON                                  
               SET  IS-PROGRAMMER      TO TRUE                                  
             WHEN OTHER                                                         
               SET  IS-GENERAL-USER    TO TRUE                                  
           END-EVALUATE.                                                        
                                                                                
      * extract settings for VSAM security                                      
           SET  BIT-DECODE-FROM-BYTE   TO TRUE.                                 
           MOVE IUI-US-IESVSAM         TO BIT-BYTE.                             
           CALL BITMAN              USING BITMAN-PARMS.                         
           IF  BIT-4-IS-ON                                                      
               SET  NOT-ICCF-USER      TO TRUE                                  
           ELSE                                                                 
               SET  IS-ICCF-USER       TO TRUE                                  
           END-IF.                                                              
                                                                                
      * extract settings for last signon date                                   
           IF  IUI-US-LASTLGN > ZEROES                                          
               SET  DTE-REQUEST-JULN-EDIT                                       
                                       TO TRUE                                  
               MOVE IUI-US-LASTLGN     TO JUL-DATE                              
               MOVE JUL-DATE           TO DTE-JULN                              
               CALL DTEMAN          USING DTEMAN-PARMS                          
               MOVE CORR DTE-GREG      TO USA-DATE                              
               MOVE NUM-DATE           TO USA-DSP-DATE                          
               MOVE USA-DSP-DATE       TO WORK-SIGNON                           
           ELSE                                                                 
               MOVE SPACES             TO WORK-SIGNON                           
           END-IF.                                                              
                                                                                
      * extract settings for expiration date                                    
           IF  IUI-US-EXPJULN > ZEROES                                          
               MOVE   'C'              TO DTE-REQU                              
               CALL 'DTEMAN'        USING DTEMAN-PARMS                          
               IF  DTE-YYDDD > IUI-US-EXPJULN                                   
                 SET  DTE-REQUEST-JULN-EDIT                                     
                                       TO TRUE                                  
                 MOVE IUI-US-EXPJULN   TO JUL-DATE                              
                 MOVE JUL-DATE         TO DTE-JULN                              
                 CALL DTEMAN        USING DTEMAN-PARMS                          
                 MOVE CORR DTE-GREG    TO USA-DATE                              
                 MOVE NUM-DATE         TO USA-DSP-DATE                          
                 MOVE USA-DSP-DATE     TO WORK-EXPIRED                          
               ELSE                                                             
                 MOVE SPACES           TO WORK-EXPIRED                          
               END-IF                                                           
           ELSE                                                                 
               MOVE SPACES             TO WORK-EXPIRED                          
           END-IF.                                                              
                                                                                
      * extract settings for revokation date                                    
           IF  IUI-US-REVOKEJ > ZEROES                                          
               MOVE   'C'              TO DTE-REQU                              
               CALL 'DTEMAN'        USING DTEMAN-PARMS                          
               IF  DTE-YYDDD > IUI-US-REVOKEJ                                   
                 SET  DTE-REQUEST-JULN-EDIT                                     
                                       TO TRUE                                  
                 MOVE IUI-US-REVOKEJ   TO JUL-DATE                              
                 MOVE JUL-DATE         TO DTE-JULN                              
                 CALL DTEMAN        USING DTEMAN-PARMS                          
                 MOVE CORR DTE-GREG    TO USA-DATE                              
                 MOVE NUM-DATE         TO USA-DSP-DATE                          
                 MOVE USA-DSP-DATE     TO WORK-REVOKED                          
               ELSE                                                             
                 MOVE SPACES           TO WORK-REVOKED                          
               END-IF                                                           
           ELSE                                                                 
               MOVE SPACES             TO WORK-REVOKED                          
           END-IF.                                                              
       P15-EXIT.                                                                
           EXIT.                                                                
                                                                                
       P20-GET-SECURITY-RESOURCES.                                              
           MOVE LOW-VALUES             TO BSM-SEC-RECORD.                       
           SET  BSM-APPL               TO TRUE.                                 
           SET  FUNC-START(SEC)        TO TRUE.                                 
           START BSM-SEC-CNTL KEY >= BSM-SEC-KEY END-START.                     
                                                                                
           PERFORM WITH TEST BEFORE                                             
             UNTIL NOT STAT-NORMAL(SEC)                                         
                OR RTC-CODE > ZERO                                              
                                                                                
             SET  FUNC-READNEXT(SEC)   TO TRUE                                  
             READ BSM-SEC-CNTL NEXT RECORD END-READ                             
             IF  STAT-NORMAL(SEC)                                               
             AND NOT BSM-SEC-GROUP                                              
                                                                                
               PERFORM WITH TEST BEFORE                                         
                 VARYING SUB FROM 1 BY 1                                        
                   UNTIL SUB > BSM-SEC-PERMITS                                  
                      OR BSM-SEC-USERID(SUB) = CURRENT-USER                     
               END-PERFORM                                                      
                                                                                
               IF  DIRECT-ACCESS                                                
               AND NOT BSM-SEC-UA-DENIED                                        
               OR  SUB <= BSM-SEC-PERMITS                                       
               AND BSM-SEC-USERID(SUB) = CURRENT-USER                           
                 IF  SUBHEAD-NEEDED                                             
                   PERFORM U50-PRINT-SUBHEADS THRU U55-EXIT                     
                 END-IF                                                         
                                                                                
                 MOVE BSM-SEC-CLASS    TO SEC-CLASS                             
                 MOVE BSM-SEC-RSCNAME  TO SEC-RSCNAME                           
                 INSPECT SEC-RSCNAME REPLACING ALL X'FF' BY '*'                 
                 MOVE BSM-SEC-DESC     TO SEC-RSCDESC                           
                 IF  GROUP-ACCESS                                               
                   MOVE SPACE          TO SEC-UACCESS                           
                 ELSE                                                           
                   EVALUATE TRUE                                                
                   WHEN BSM-SEC-UA-ALTER     MOVE '4' TO SEC-UACCESS            
                   WHEN BSM-SEC-UA-UPDATE    MOVE '3' TO SEC-UACCESS            
                   WHEN BSM-SEC-UA-READ      MOVE '2' TO SEC-UACCESS            
                   WHEN OTHER                MOVE ' ' TO SEC-UACCESS            
                   END-EVALUATE                                                 
                 END-IF                                                         
                 EVALUATE TRUE                                                  
                 WHEN BSM-SEC-AC-ALTER(SUB)  MOVE '4' TO SEC-ACCESS             
                 WHEN BSM-SEC-AC-UPDATE(SUB) MOVE '3' TO SEC-ACCESS             
                 WHEN BSM-SEC-AC-READ(SUB)   MOVE '2' TO SEC-ACCESS             
                 WHEN OTHER                  MOVE ' ' TO SEC-ACCESS             
                 END-EVALUATE                                                   
                                                                                
                 MOVE SEC-DETAIL       TO PRINT-LINE                            
                 PERFORM U20-PUT-PRINT THRU U25-EXIT                            
                 MOVE SPACES           TO SEC-DETAIL                            
               END-IF                                                           
                                                                                
             END-IF                                                             
           END-PERFORM.                                                         
       P25-EXIT.                                                                
           EXIT.                                                                
                                                                                
      /*****************************************************************        
      *    SHARED UTILITY ROUTINES                                     *        
      ******************************************************************        
                                                                                
       U20-PUT-PRINT.                                                           
           EVALUATE PRINT-CC                                                    
           WHEN '-'   MOVE +3          TO LINES-COUNT                           
           WHEN '0'   MOVE +2          TO LINES-COUNT                           
           WHEN '+'   MOVE +0          TO LINES-COUNT                           
           WHEN OTHER MOVE +1          TO LINES-COUNT                           
           END-EVALUATE.                                                        
                                                                                
           IF  LINE-MAXIMUM <= (LINE-NUMBER + LINES-COUNT - 1)                  
               MOVE PRINT-LINE         TO SAVE-PRINT-LINE                       
               PERFORM U30-PRINT-HEADERS THRU U35-EXIT                          
               MOVE SAVE-PRINT-LINE    TO PRINT-LINE                            
           END-IF.                                                              
                                                                                
           WRITE PRINT-LINE AFTER ADVANCING LINES-COUNT LINES.                  
                                                                                
           ADD  LINES-COUNT            TO LINE-NUMBER.                          
           MOVE SPACES                 TO PRINT-LINE.                           
       U25-EXIT.                                                                
           EXIT.                                                                
                                                                                
       U30-PRINT-HEADERS.                                                       
           IF  NOT PRINTER-OPEN                                                 
               OPEN OUTPUT PRINTER                                              
               SET  PRINTER-OPEN       TO TRUE                                  
           END-IF.                                                              
                                                                                
           ADD  +1                     TO PAGE-NUMBER.                          
           MOVE PAGE-NUMBER            TO HEAD-PAGE-NO.                         
                                                                                
           WRITE PRINT-LINE FROM HEAD-1 AFTER ADVANCING PAGE.                   
           WRITE PRINT-LINE FROM HEAD-2 AFTER ADVANCING 1 LINE.                 
           WRITE PRINT-LINE FROM HEAD-3 AFTER ADVANCING 1 LINE.                 
           WRITE PRINT-LINE FROM HEAD-4 AFTER ADVANCING 1 LINE.                 
                                                                                
           WRITE PRINT-LINE FROM COLH-1 AFTER ADVANCING 2 LINES.                
           WRITE PRINT-LINE FROM COLH-2 AFTER ADVANCING 1 LINE.                 
           WRITE PRINT-LINE FROM COLH-3 AFTER ADVANCING 1 LINE.                 
                                                                                
           MOVE +9                     TO LINE-NUMBER.                          
                                                                                
           IF  SUBHEAD-PRINTED                                                  
             PERFORM U50-PRINT-SUBHEADS THRU U55-EXIT                           
           ELSE                                                                 
             PERFORM U40-PRINT-BLANK-LINE THRU U45-EXIT                         
           END-IF.                                                              
       U35-EXIT.                                                                
           EXIT.                                                                
                                                                                
       U40-PRINT-BLANK-LINE.                                                    
           MOVE SPACES                 TO PRINT-LINE.                           
           WRITE PRINT-LINE               AFTER ADVANCING 1 LINE.               
           ADD  +1                     TO LINE-NUMBER.                          
           MOVE SPACES                 TO PRINT-LINE.                           
       U45-EXIT.                                                                
           EXIT.                                                                
                                                                                
       U50-PRINT-SUBHEADS.                                                      
           SET  SUBHEAD-PRINTED        TO TRUE.                                 
           IF  LINE-MAXIMUM <= (LINE-NUMBER + 4)                                
               PERFORM U30-PRINT-HEADERS THRU U35-EXIT                          
           ELSE                                                                 
               WRITE PRINT-LINE FROM SUBH-1 AFTER ADVANCING 2 LINES             
               WRITE PRINT-LINE FROM SUBH-2 AFTER ADVANCING 1 LINE              
               ADD  +3                 TO LINE-NUMBER                           
           END-IF.                                                              
       U55-EXIT.                                                                
           EXIT.                                                                

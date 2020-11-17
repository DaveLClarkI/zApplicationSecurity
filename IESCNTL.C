       01  IESCNTL-RECORD.                                               162    
         03  IUI-KEY.                                                           
           05  IUI-REC-TYPE            PIC  X(02).                              
             88  IUI-APPL-PROFILE                   VALUE 'AP'.                 
             88  IUI-NEWS-TEXT                      VALUE 'NW'.                 
             88  IUI-SELECTION-TEXT                 VALUE 'TX'.                 
             88  IUI-USER-PROFILE                   VALUE 'US'.                 
             88  IUI-FAST-PATH-SYN                  VALUE 'UT'.                 
           05  IUI-KEY-VALUE.                                                   
             07  IUI-KEY-SHORT         PIC  X(08).                              
             07                        PIC  X(02).                              
         03  IUI-VRSN                  PIC  X(02).                              
           88  IUI-US-VERSION-64                    VALUE '64'.                 
         03  IUI-DATA                  PIC  X(986).                             
      *                                                                         
       01  IUI-US-RECORD.                                                       
         03  IUI-US-KEY.                                                        
           05                          PIC  X(02).                              
           05  IUI-US-USRIDNT          PIC  X(08).                              
           05                          PIC  X(02).                              
         03                            PIC  X(02).                              
         03                            PIC  X(02).                              
         03  IUI-US-IESNATL            PIC  X(03).                              
         03  IUI-US-IESINFO            PIC  X(01).                              
      *    B'11.. ....' = ADMINISTRATOR (1)                                     
      *    B'01.. ....' = PROGRAMMER (2)                                        
      *    B'00.. ....' = GENERAL (3)                                           
      *    B'..1. ....' =  NO (2), B'0' = YES(1) -- RECEIVE NEWS?               
      *    B'...1 ....' = MENU(2), B'0' = APPL(1) -- INIT NAME TYPE             
         03                            PIC  X(03).                              
         03  IUI-US-INITNAM            PIC  X(08).                              
         03  IUI-US-USERCAT            PIC  X(07).                              
         03  IUI-US-IESVSAM            PIC  X(01).                              
      *    B'.... 1...' =  NO      B'0' = YES    -- ICCF USER                   
      *    B'.... .1..' = YES (1), B'0' = NO (2) -- CONFIRM DELETE?             
      *    B'.... ..1.' = YES (1), B'0' = NO (2) -- VSAM CATALOGS?              
      *    B'.... ...1' = YES (1), B'0' = NO (2) -- VSAM FILES?                 
         03  IUI-US-IESVSEA            PIC  X(01).                              
      *    B'.... .1..' =  NO (2), B'0' = YES(1) -- SUBMIT TO BATCH?            
      *    B'.... ..1.' = YES (1), B'0' = NO (2) -- VSE PRIMARY LIB?            
         03  IUI-US-IESACCS            PIC  X(01).                              
      *    B'.1.. ....' = YES (1), B'0' = NO (2) -- ESCAPE TO CICS?             
      *    B'..1. ....' = NO  (2), B'0' = YES(1) -- PSWRD ADMIN ONLY?           
      *    B'.... .1..' = NO  (2), B'0' = YES(1) -- ALL CONSOLE MSGS?           
      *    B'.... ..1.' = YES (1), B'0' = NO (2) -- ALL BATCH QUEUES?           
      *    B'.... ...1' = YES (1), B'0' = NO (2) -- CONSOLE COMMANDS?           
         03  IUI-US-IESAUTH            PIC  X(01).                              
      *    B'...1 ....' = YES (1), B'0' = NO (2) -- APPLICATION PROF?           
      *    B'.... .1..' = YES (1), B'0' = NO (2) -- SELECTION PANELS?           
      *    B'.... ..1.' = YES (1), B'0' = NO (2) -- OLPD?                       
      *    B'.... ...1' = YES (1), B'0' = NO (2) -- USER PANELS?                
         03  IUI-US-PASSWRD            PIC  X(08).                              
         03                            PIC  X(02).                              
         03  IUI-US-EXPJULN            PIC S9(05)   PACKED-DECIMAL.             
         03  IUI-US-REVOKEJ            PIC S9(05)   PACKED-DECIMAL.             
         03  IUI-US-EXPDAYS            PIC  9(03)   PACKED-DECIMAL.             
         03  IUI-US-EXPDATE            PIC  X(08).                              
         03                            PIC  X(02).                              
         03  IUI-US-USRMODL            PIC  X(08).                              
         03  IUI-US-PSWDTBL.                                                    
           05  IUI-US-PREVPSW          PIC  X(08)   OCCURS 12.                  
         03                            PIC  X(02).                              
         03  IUI-US-EXTNSEC            PIC  X(01).                              
      *    B'1... ....' = YES (1), B'0' = NO (2) -- EXTERNAL SECURITY?          
      *    B'.... ...1' = YES (1), B'0' = NO (2) -- XRF SIGN OFF?               
         03  IUI-US-TIMEOUT            PIC  X(01).                              
         03  IUI-US-OPERPRI            PIC  X(01).                              
         03  IUI-US-OPERCLS.                                                    
             07  IUI-US-OPERBYT        PIC  X(01)   OCCURS 3.                   
         03  IUI-US-OPRIDNT            PIC  X(03).                              
         03                            PIC  X(27).                              
         03  IUI-US-DTSECTB.                                                    
           05  IUI-US-ACCSLVL                       OCCURS 5.                   
             07  IUI-US-ACCSBYT        PIC  X(01)   OCCURS 4.                   
         03  IUI-US-CICSSEC.                                                    
             07  IUI-US-CICSBYT        PIC  X(01)   OCCURS 8.                   
         03                            PIC  X(04).                              
         03  IUI-US-DIRBYTE            PIC  X(01).                              
      *    B'1... ....' = YES (1), B'0' = NO (2) -- TOUCH B-TRANSIENT?          
      *    B'.1.. ....' = YES (1), B'0' = NO (2) -- READ WITH CONNECT?          
         03                            PIC  X(02).                              
         03                            PIC  X(01).                              
         03  IUI-US-OIDRDRL            PIC  X(01).                              
         03  IUI-US-OIDRDRV            PIC  X(20).                              
         03  IUI-US-USRNAME            PIC  X(20).                              
         03  IUI-US-LASTLGN            PIC  9(05)   PACKED-DECIMAL.             
         03  IUI-US-LASTLGS            PIC  X(04).                              
         03                            PIC  X(24).                              
         03  IUI-US-IESDEFD            PIC  X(03).                              

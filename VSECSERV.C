      *                                                                         
         03  VSECSERV-HEADER.                                                   
           05  VSECSERV-API-AREA.                                               
             07  VSECSERV-API-RETC     PIC  X(4).                               
               88  VSECSERV-SEC-DENIED              VALUE 'DENY',               
                                                          SPACES.               
               88  VSECSERV-SEC-READABLE            VALUE 'READ',               
                                                          'UPDT',               
                                                          'ALTR'.               
               88  VSECSERV-SEC-UPDATABLE           VALUE 'UPDT',               
                                                          'ALTR'.               
               88  VSECSERV-SEC-ALTERABLE           VALUE 'ALTR'.               
             07  VSECSERV-API-ADDR     PIC  X(4).                               
           05  VSECSERV-REQ-TRAN       PIC  X(4).                               
             88  VSECSERV-TRAN                      VALUE 'VSEC'.               
           05  VSECSERV-REQ-FUNC       PIC  X(4).                               
             88  VSECSERV-CHK-SECURITY              VALUE 'CSEC'.               
             88  VSECSERV-LST-SECURITY              VALUE 'LIST'.               
             88  VSECSERV-LST-APPS                  VALUE 'APPS'.               
             88  VSECSERV-LST-CORP                  VALUE 'CORP'.               
             88  VSECSERV-LST-ROLE                  VALUE 'ROLE'.               
             88  VSECSERV-LST-VSEC                  VALUE 'VSEC'.               
             88  VSECSERV-LST-GROUP                 VALUE 'GRUP'.               
             88  VSECSERV-LST-TRANS                 VALUE 'TRAN'.               
             88  VSECSERV-LST-USERS                 VALUE 'USER'.               
             88  VSECSERV-GET-RESOURCE              VALUE 'RGET'.               
             88  VSECSERV-UPD-RESOURCE              VALUE 'RUPD'.               
             88  VSECSERV-DEL-RESOURCE              VALUE 'RDEL'.               
             88  VSECSERV-GET-PERMISSION            VALUE 'PGET'.               
             88  VSECSERV-UPD-PERMISSION            VALUE 'PUPD'.               
             88  VSECSERV-DEL-PERMISSION            VALUE 'PDEL'.               
             88  VSECSERV-PRG-PERMISSIONS           VALUE 'PPRG'.               
           05  VSECSERV-CGI-REQU.                                               
             07  VSECSERV-RTN-QUEUE    PIC  X(16).                              
             07  VSECSERV-RTN-CODE     PIC  X(4).                               
               88  VSECSERV-RTN-OKAY                VALUE 'OKAY'.               
               88  VSECSERV-RTN-FULL                VALUE 'FULL'.               
               88  VSECSERV-RTN-UNEX                VALUE 'UNEX'.               
             07  VSECSERV-RTN-MESG     PIC  X(44).                              
      *                                                                         
         03  VSECLST-DETAIL.                                                    
           05  VSEC-USER-ID            PIC  X(08).                              
           05  VSEC-ROLE-AUTH          PIC  X(01).                              
             88  VSEC-RA-DENIED                     VALUE '0'.                  
             88  VSEC-RA-READ                       VALUE '2'.                  
             88  VSEC-RA-UPDATE                     VALUE '3'.                  
             88  VSEC-RA-ALTER                      VALUE '4'.                  
           05  VSEC-ROLE-NUMB          PIC  9(03).                              
             88  VSEC-ROLE-CRP-MANAGEMENT           VALUE 000 THRU 250,         
                                                          300, 400, 500.        
             88  VSEC-ROLE-CRP-CEO                  VALUE 050.                  
             88  VSEC-ROLE-CRP-CFO                  VALUE 100.                  
             88  VSEC-ROLE-CRP-CIO                  VALUE 150.                  
             88  VSEC-ROLE-CRP-SVP                  VALUE 200.                  
             88  VSEC-ROLE-CRP-RVP                  VALUE 250.                  
             88  VSEC-ROLE-CRP-IT-MGR               VALUE 300.                  
             88  VSEC-ROLE-CRP-IT-EMP               VALUE 350.                  
             88  VSEC-ROLE-CRP-WGS-MGR              VALUE 400.                  
             88  VSEC-ROLE-CRP-WGS-EMP              VALUE 450.                  
             88  VSEC-ROLE-CRP-WSS-MGR              VALUE 500.                  
             88  VSEC-ROLE-CRP-WSS-EMP              VALUE 550.                  
             88  VSEC-ROLE-RGN-MANAGEMENT           VALUE 600, 700.             
             88  VSEC-ROLE-RGN-WGS-MGR              VALUE 600.                  
             88  VSEC-ROLE-RGN-WGS-EMP              VALUE 650.                  
             88  VSEC-ROLE-RGN-WSS-MGR              VALUE 700.                  
             88  VSEC-ROLE-RGN-WSS-EMP              VALUE 750.                  
             88  VSEC-ROLE-LCL-AL                   VALUE 800.                  
             88  VSEC-ROLE-LCL-AC                   VALUE 850.                  
             88  VSEC-ROLE-LCL-MGR                  VALUE 900.                  
             88  VSEC-ROLE-LCL-EMP                  VALUE 950.                  
           05  VSEC-ENTRIES            PIC S9(04)   BINARY.                     
           05  VSEC-TABLE.                                                      
             07  VSEC-ENTRY                         OCCURS 2040.                
               09  VSEC-CORP-AUTH      PIC  X(01).                              
                 88  VSEC-CA-DENIED                 VALUE '0'.                  
                 88  VSEC-CA-READ                   VALUE '2'.                  
                 88  VSEC-CA-UPDATE                 VALUE '3'.                  
                 88  VSEC-CA-ALTER                  VALUE '4'.                  
               09  VSEC-CORP-NUMB      PIC  X(05).                              
      *                                                                         
         03  VSECUPD-DETAIL            REDEFINES    VSECLST-DETAIL.             
           05  VSECUPD-RESOURCE.                                                
             07  VSECUPD-RES-CLASS     PIC  X(1).                               
               88  VSECUPD-RES-FACILITY             VALUE 'F'.                  
               88  VSECUPD-RES-GROUP                VALUE 'G'.                  
               88  VSECUPD-RES-TRANS                VALUE 'T'.                  
               88  VSECUPD-RES-USER                 VALUE 'U'.                  
             07  VSECUPD-FAC-TYPE      PIC  X(4).                               
               88  VSECUPD-FAC-APPS                 VALUE 'APPS'.               
               88  VSECUPD-FAC-CORP                 VALUE 'CORP'.               
               88  VSECUPD-FAC-ROLE                 VALUE 'ROLE'.               
               88  VSECUPD-FAC-VSEC                 VALUE 'VSEC'.               
             07  VSECUPD-RES-NAME      PIC  X(31).                              
             07  VSECUPD-RES-DESC      PIC  X(20).                              
             07  VSECUPD-RES-UACC      PIC  X(1).                               
               88  VSECUPD-RES-UA-DENIED            VALUE '0'.                  
               88  VSECUPD-RES-UA-READ              VALUE '2'.                  
               88  VSECUPD-RES-UA-UPDATE            VALUE '3'.                  
               88  VSECUPD-RES-UA-ALTER             VALUE '4'.                  
           05  VSECUPD-PERMISSION.                                              
             07  VSECUPD-PER-TYPE      PIC  X(1).                               
               88  VSECUPD-PER-GROUP                VALUE 'G'.                  
               88  VSECUPD-PER-USER                 VALUE 'U'.                  
             07  VSECUPD-PER-NAME      PIC  X(08).                              
             07  VSECUPD-PER-DESC      PIC  X(20).                              
             07  VSECUPD-PER-ACCS      PIC  X(1).                               
               88  VSECUPD-PER-AC-DENIED            VALUE '0'.                  
               88  VSECUPD-PER-AC-READ              VALUE '2'.                  
               88  VSECUPD-PER-AC-UPDATE            VALUE '3'.                  
               88  VSECUPD-PER-AC-ALTER             VALUE '4'.                  
      *                                                                         

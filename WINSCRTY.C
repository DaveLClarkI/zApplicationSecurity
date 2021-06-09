         03  SCRTY-REQUEST.                                                     
      * optionally pass a user id for validation, otherwise                     
      * the currently signed-on user id will be returned here                   
           05  SCRTY-USERID            PIC  X(8).                               
             88  SCRTY-USERID-UNKNOWN               VALUE '*UNKNOWN'.           
             88  SCRTY-USERID-EXPIRED               VALUE '*EXPIRED'.           
             88  SCRTY-USERID-REVOKED               VALUE '*REVOKED'.           
      * optionally pass a company number for validation, otherwise              
      * the user's primary company will be returned here                        
           05  SCRTY-CONUM             PIC  X(5).                               
      *                                                                         
         03  SCRTY-RESPONSE.                                                    
      * the following fifteen fields are output-only                            
           05  SCRTY-ACCESS            PIC  X(1).                               
             88  SCRTY-ACCESS-DENY                  VALUE '0', '1', ' '.        
             88  SCRTY-ACCESS-READ                  VALUE '2', '3', '4'.        
             88  SCRTY-ACCESS-UPDT                  VALUE '3', '4'.             
             88  SCRTY-ACCESS-ALTR                  VALUE '4'.                  
      * the following fourteen fields are optional and are output-only          
           05  SCRTY-SVCRGN            PIC  X(2).                               
           05  SCRTY-ALTRGN            PIC  X(2).                               
           05  SCRTY-RPTRMT            PIC  X(3).                               
           05  SCRTY-ALTRMT            PIC  X(3).                               
           05  SCRTY-DFTPTR            PIC  X(4).                               
      * the following five fields are attributes from Active Directory          
           05  SCRTY-WINFUNC           PIC  X(3).                               
             88  SCRTY-WINFUNC-AREA-COORD           VALUE 'AC'.                 
             88  SCRTY-WINFUNC-WIN-CORP             VALUE 'CO'.                 
             88  SCRTY-WINFUNC-LOCAL-CO             VALUE 'LC'.                 
             88  SCRTY-WINFUNC-VICE-PRES            VALUE 'VP'.                 
             88  SCRTY-WINFUNC-WGS-EMPL             VALUE 'WG'.                 
             88  SCRTY-WINFUNC-WSS-EMPL             VALUE 'WS'.                 
             88  SCRTY-WINFUNC-WIN-EMPL             VALUE 'WW'.                 
             88  SCRTY-WINFUNC-EXTERNAL             VALUE 'XT'.                 
           05  SCRTY-WINUNIT           PIC  X(10).                              
             88  SCRTY-WINUNIT-ACCOUNTING           VALUE 'ACCT      '.         
             88  SCRTY-WINUNIT-AREA-LEAD            VALUE 'AL        '.         
             88  SCRTY-WINUNIT-PAYABLES             VALUE 'AP        '.         
             88  SCRTY-WINUNIT-COMMUNICATION        VALUE 'COMM      '.         
             88  SCRTY-WINUNIT-CONTRACTOR           VALUE 'CONTRACTOR'.         
             88  SCRTY-WINUNIT-CUST-SVC             VALUE 'CUSTSVC   '.         
             88  SCRTY-WINUNIT-DATA-MGMT            VALUE 'DTAMGMT   '.         
             88  SCRTY-WINUNIT-EXECUTIVE            VALUE 'EXEC      '.         
             88  SCRTY-WINUNIT-FINANCE              VALUE 'FIN       '.         
             88  SCRTY-WINUNIT-HUMAN-RSRCS          VALUE 'HR        '.         
             88  SCRTY-WINUNIT-INVSTR               VALUE 'INVSTR    '.         
             88  SCRTY-WINUNIT-INFO-TECH            VALUE 'IT        '.         
             88  SCRTY-WINUNIT-MARKETING            VALUE 'MKT       '.         
             88  SCRTY-WINUNIT-PRICING              VALUE 'PRICING   '.         
             88  SCRTY-WINUNIT-PURCHASING           VALUE 'PURCHS    '.         
             88  SCRTY-WINUNIT-ROMS                 VALUE 'ROMS      '.         
             88  SCRTY-WINUNIT-SPS                  VALUE 'SPS       '.         
             88  SCRTY-WINUNIT-TRAINING             VALUE 'TRAINING  '.         
             88  SCRTY-WINUNIT-WCMS                 VALUE 'WCMS      '.         
             88  SCRTY-WINUNIT-WINSUPPORT           VALUE 'WINSUP    '.         
             88  SCRTY-WINUNIT-SRCNG-SVCS           VALUE 'WSS       '.         
             88  SCRTY-WINUNIT-DAYTON-DC            VALUE 'DAYTON    '.         
             88  SCRTY-WINUNIT-DENVER-DC            VALUE 'DENVER    '.         
             88  SCRTY-WINUNIT-JACKSONVLE-DC        VALUE 'JACKSONVLE'.         
             88  SCRTY-WINUNIT-NHAVEN-DC            VALUE 'NHAVEN    '.         
             88  SCRTY-WINUNIT-RICHMOND-DC          VALUE 'RICHMOND  '.         
           05  SCRTY-WINORG            PIC  X(5).                               
           05  SCRTY-WINPOS            PIC  X(5).                               
             88  SCRTY-WINPOS-EMPL                  VALUE 'EMPL '.              
             88  SCRTY-WINPOS-LOCAL-PRES            VALUE 'PRES '.              
             88  SCRTY-WINPOS-LOCAL-SIC             VALUE 'SIC  '.              
           05  SCRTY-WINDIR            PIC  X(1).                               
             88  SCRTY-WINDIR-NO                    VALUE 'N', ' '.             
             88  SCRTY-WINDIR-YES                   VALUE 'Y'.                  
      * the following four fields are deprecated but still supported            
           05  SCRTY-OLDDAP            PIC  X(1).                               
             88  SCRTY-OLDDAP-DAPSCO                VALUE '9'.                  
             88  SCRTY-OLDDAP-VIRGINIA              VALUE '8'.                  
             88  SCRTY-OLDDAP-OMAHA                 VALUE '7'.                  
             88  SCRTY-OLDDAP-ATLANTA               VALUE '6'.                  
             88  SCRTY-OLDDAP-RENO                  VALUE '5'.                  
             88  SCRTY-OLDDAP-PHOENIX               VALUE '4'.                  
             88  SCRTY-OLDDAP-DENVER                VALUE '3'.                  
             88  SCRTY-OLDDAP-STLOUIS               VALUE '2'.                  
             88  SCRTY-OLDDAP-DAYTON                VALUE '1'.                  
             88  SCRTY-OLDDAP-NHAVEN                VALUE '0'.                  
             88  SCRTY-OLDDAP-AREALEAD              VALUE 'A'.                  
             88  SCRTY-OLDDAP-WISE                  VALUE 'W'.                  
             88  SCRTY-OLDDAP-EXTERNAL              VALUE 'X'.                  
           05  SCRTY-OLDCLS            PIC  X(2).                               
             88  SCRTY-OLDCLS-ALL-ACCESS            VALUE '99'.                 
             88  SCRTY-OLDCLS-WIN-DIR               VALUE '95'.                 
             88  SCRTY-OLDCLS-WIN-EMPL              VALUE '91'.                 
             88  SCRTY-OLDCLS-LOCAL-PRES            VALUE '21'.                 
             88  SCRTY-OLDCLS-LOCAL-USER            VALUE '20'.                 
           05  SCRTY-OLDGRP            PIC  X(2).                               
             88  SCRTY-OLDGRP-DAPSCO-INC            VALUE 'DI'.                 
             88  SCRTY-OLDGRP-PRIMUS                VALUE 'PR'.                 
             88  SCRTY-OLDGRP-IND-PRES              VALUE 'IP'.                 
             88  SCRTY-OLDGRP-AREA-LEAD             VALUE 'AL'.                 
             88  SCRTY-OLDGRP-AREA-COORD            VALUE 'AC'.                 
             88  SCRTY-OLDGRP-DISTRO-INC            VALUE 'DS'.                 
             88  SCRTY-OLDGRP-NORTHEAST             VALUE 'NE'.                 
             88  SCRTY-OLDGRP-DAYTON                VALUE 'DA'.                 
             88  SCRTY-OLDGRP-MIDWEST               VALUE 'MW'.                 
             88  SCRTY-OLDGRP-SOUTHWEST             VALUE 'SW'.                 
             88  SCRTY-OLDGRP-PHEONIX               VALUE 'DP'.                 
             88  SCRTY-OLDGRP-WEST                  VALUE 'DW'.                 
             88  SCRTY-OLDGRP-ATLANTA               VALUE 'AT'.                 
             88  SCRTY-OLDGRP-OMAHA                 VALUE 'OM'.                 
             88  SCRTY-OLDGRP-NOLAND                VALUE 'NO'.                 
             88  SCRTY-OLDGRP-NOL-CORP              VALUE 'NC'.                 
             88  SCRTY-OLDGRP-LOCAL-PC              VALUE 'PC'.                 
             88  SCRTY-OLDGRP-EXTERNAL              VALUE 'XT'.                 
           05  SCRTY-OLDLVL            PIC  X(1).                               
             88  SCRTY-OLDLVL-DENY                  VALUE '0', ' '.             
             88  SCRTY-OLDLVL-WISE                  VALUE '1'.                  
             88  SCRTY-OLDLVL-TEST                  VALUE '2'.                  
             88  SCRTY-OLDLVL-AL-AC                 VALUE '3'.                  
             88  SCRTY-OLDLVL-WSS-RVP               VALUE '4'.                  
             88  SCRTY-OLDLVL-WGS                   VALUE '5'.                  
             88  SCRTY-OLDLVL-CORP                  VALUE '9'.                  

       01  IESLDUM-RECORD.                                                      
         03  LDUM-KEY.                                                          
           05  LDUM-TYPE               PIC  X(02).                              
             88  LDUM-USRMAP-RECORD                 VALUE 'UM'.                 
           05  LDUM-NETUSRID           PIC  X(64).                              
         03                            PIC S9(09)   BINARY.                     
         03  LDUM-MFUSRID              PIC  X(08).                              
         03  LDUM-MFPSWRD              PIC  X(08).                              
         03  LDUM-MFPWKEY              PIC  X(08).                              
         03                            PIC  X(32).                              
         03  LDUM-DATE1                PIC  X(08).                              
         03  LDUM-TIME1                PIC  X(06).                              
         03  LDUM-DATE2                PIC  X(08).                              
         03  LDUM-TIME2                PIC  X(06).                              
         03  LDUM-DATE3                PIC  X(08).                              
         03  LDUM-TIME3                PIC  X(06).                              
         03  LDUM-USRDESC              PIC  X(64).                              
         03                            PIC  X(18).                              

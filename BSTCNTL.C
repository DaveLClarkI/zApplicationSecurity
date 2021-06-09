       01  BSTCNTL-RECORD.                                               162
         03  BSM-PFX.
           05  BSM-RECLENG             PIC  9(04)   BINARY.
           05  BSM-VERSION             PIC  X(01).
           05  BSM-MODLEVL             PIC  X(01).
         03  BSM-KEY.
           05  BSM-CLASS               PIC  X(08).
             88  BSM-APPL                           VALUE 'APPL'.
             88  BSM-CICS-PCT                       VALUE 'ACICSPCT'.
             88  BSM-CICS-DCT                       VALUE 'DCICSDCT'.
             88  BSM-CICS-FCT                       VALUE 'FCICSFCT'.
             88  BSM-CICS-JCT                       VALUE 'JCICSJCT'.
             88  BSM-CICS-PPT                       VALUE 'MCICSPPT'.
             88  BSM-CICS-TST                       VALUE 'SCICSTST'.
             88  BSM-TRANS                          VALUE 'TCICSTRN'.
             88  BSM-FACILITY                       VALUE 'FACILITY'.
             88  BSM-GROUP                          VALUE 'GROUP'.
           05  BSM-RESOURCE            PIC  X(246).
           05  BSM-SEQUENCE            PIC  X(01).
         03  BSM-KEYVALEN              PIC  X(01).
         03  BSM-RSRCDESC              PIC  X(20).
         03  BSM-DATA                  PIC  X(65255).
      *
       01  BSM-SECURITY-RECORD.                                          162
         03                            PIC  X(258).
         03                            PIC  X(01).
           88  BSM-SEC-GENERIC                      VALUE X'80'.
         03                            PIC  X(21).
         03  BSM-SEC-UA-FLAG           PIC  X(01).
           88  BSM-SEC-UA-DENIED                    VALUE X'00'.
           88  BSM-SEC-UA-READ                      VALUE X'20'.
           88  BSM-SEC-UA-UPDATE                    VALUE X'40'.
           88  BSM-SEC-UA-ALTER                     VALUE X'80'.
         03  BSM-SEC-LOGGING1          PIC  X(02).
         03  BSM-SEC-LOGGING2          PIC  X(02).
         03  BSM-SEC-FILLER            PIC  X(01).
         03  BSM-SEC-PERMITS           PIC S9(04)   BINARY.
         03  BSM-SEC-PERMIT                         OCCURS 5437
                                                    DEPENDING ON
                                                    BSM-SEC-PERMITS.
           05  BSM-SEC-USERID          PIC  X(08).
           05  BSM-SEC-AC-FLAG         PIC  X(01).
             88  BSM-SEC-AC-DENIED                  VALUE X'00'.
             88  BSM-SEC-AC-READ                    VALUE X'20'.
             88  BSM-SEC-AC-UPDATE                  VALUE X'40'.
             88  BSM-SEC-AC-ALTER                   VALUE X'80'.
           05                          PIC  X(03).
      *
       01  BSM-GROUP-RECORD.                                             162
         03                            PIC  X(12).
         03  BSM-GRP-NAME              PIC  X(08).
         03                            PIC  X(08).
         03  BSM-GRP-MEMBER            PIC  X(08).
         03                            PIC  X(223).
         03                            PIC  X(21).

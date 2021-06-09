VUPM     TITLE 'VUPMSKEL - CICS/VS SIGN-ON TABLE GENERATE SKELETON'     00000100
         PRINT ON,NOGEN                                                 00000200
VUPMSKEL AMODE 31
VUPMSKEL RMODE ANY
VUPMSKEL CSECT                                                          00000300
         SPACE 3                                                        00000400
* ******************************************************************* * 00000500
* NOTE: MAX JCL CARDS IN TABLE IS 98!                                 * 00000500
* ******************************************************************* * 00000500
         SPACE 1                                                        00000600
SKELJCLC DC    AL2((SKELJCLE-SKELJCLB)/L'SKELJCLT) SKELETON JCL COUNT   00000700
SKELJCLT DC    98CL80' '     USED FOR SKELETON JCL TABLE                00000800
         ORG   SKELJCLT                                                 00000900
SKELJCLB EQU   *
 DC CL80'* $$ JOB JNM=DFHSNT,DISP=D,CLASS=0                           ' 00001000
 DC CL80'* $$ LST LST=SYSLST,DISP=D,CLASS=Z,PURGE=0                   ' 00001100
 DC CL80'// JOB    DFHSNT   GENERATE CICS/VS SIGN-ON TABLE            ' 00001200
 DC CL80'// LIBDEF *,SEARCH=PRD2.CICSOLDP,TEMP                        ' 00001300
 DC CL80'// LIBDEF PHASE,CATALOG=PRD2.CONFIG,TEMP                     ' 00001400
 DC CL80'/. STEP01 - ASSEMBLE TABLE SOURCE                            ' 00001500
 DC CL80'// OPTION CATAL                                              ' 00001600
 DC CL71'// EXEC   PGM=ASMA90,SIZE=(ASMA90,64K),                      ' 00001700
 DC CL09'X'                                                             00001700
 DC CL15' '                                                             00001700
 DC CL65'PARM=''EXIT(LIBEXIT(EDECKXIT)),SIZE(MAX-200K,ABOVE)'''         00001700
 DC CL80'SNT     TITLE  ''DFHSNT   - CICS/VS SIGN-ON TABLE''          ' 00001800
 DC CL80'        PRINT  ON,NOGEN                                      ' 00001900
 DC CL80'SNTXX   DFHSNT TYPE=INITIAL,EXTSEC=NO                        ' 00002000
 DC CL80'*¦¦¦¦¦¦ VUPM SOURCE <<=== DO NOT CHANGE THIS LINE !!! ===<<<<' 00002100
 DC CL80'        DFHSNT TYPE=FINAL                                    ' 00002200
 DC CL80'        END    DFHSNTBA                                      ' 00002300
 DC CL80'/* EOD                                                       ' 00002400
 DC CL80'// IF     $RC > 0 THEN                                       ' 00002500
 DC CL80'// GOTO   EXIT                                               ' 00002600
 DC CL80'// RESET  PROG                                               ' 00002700
 DC CL80'/. STEP02 - LINKEDIT ASSEMBLED OBJECT                        ' 00002800
 DC CL80'// EXEC   PGM=LNKEDT,SIZE=256K                               ' 00002900
 DC CL80'/* EOD                                                       ' 00003000
 DC CL80'/. STEP03 - NEWCOPY LINKEDITED PHASE                         ' 00003100
 DC CL80'// OPTION MSG=ACTION,MID=''<MBTC004'',REPLY=DOWN,COUNT=10    ' 00003200
 DC CL80'// LIBDEF *,SEARCH=ESP.MKSMBC,TEMP                           '
 DC CL80'// EXEC   PROC=MBCCNTL                                       '
 DC CL80'// EXEC   PGM=MBCBATCH,SIZE=*,OS390                          '
 DC CL80'CICS ALL                                                     ' 00006300
 DC CL80'PHAS DFHSNT                                                  ' 00006300
 DC CL80'/* EOD                                                       ' 00007200
 DC CL80'   SET    MRCZERO                                            '
 DC CL80'/. EXIT                                                      ' 00007200
 DC CL80'/&& EOJ                                                      ' 00003900
 DC CL80'* $$ EOJ                                                     ' 00004000
SKELJCLE EQU   *
         ORG                                                            00004100
         SPACE 1                                                        00004200
* ******************************************************************* * 00004300
         SPACE 3                                                        00004400
* ******************************************************************* * 00004500
*        LITERAL TO CHECK DATA ALIGNMENT AGAINST                      * 00004600
* ******************************************************************* * 00004700
         SPACE 1                                                        00004800
VUPMLDOK DC    C'OK'                                                    00004900
         SPACE 1                                                        00005000
* ******************************************************************* * 00005100
         SPACE 3                                                        00005200
         END                                                            00005300

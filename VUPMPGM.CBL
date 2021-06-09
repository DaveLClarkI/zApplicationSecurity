 CBL XOPTS(SP)
      ******************************************************************
      *                                                                *
      *    IDENTIFICATION DIVISION                                     *
      *                                                                *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    VUPMPGM.
       AUTHOR.        DAVE L CLARK I.
       DATE-WRITTEN.  APRIL 2019.
       DATE-COMPILED.
       INSTALLATION.  WINSUPPLY GROUP SERVICES.
       SECURITY.      TRANSACTION SECURITY PLUS VSEC.
      *REMARKS.       VIEW CICS USER PROFILES FOR MAINTENANCE.
      *
      * this program checks active directory attributes in vuserid file
      * and highlights any conflicting or mismatching attribute values.
      * any changes here may also require changes in program WLSNPGM
      *                                       and in program VUSRPGM
      *                                       and in program VUSRATTR
      *                                       and in program VUSRAUDT.

      * CHANGE HISTORY ------------------------------------------------
      * 04/15/2019 DLC REWRITE FROM IUPMPGM.
      *                INCLUDES MAINTENANCE OF IESLDUM, BSTCNTL, AND
      *                                        VOPERID FILES.
/DLC0/* 08/07/2019 DLC CONVERT FROM VOPERID TO VUSERID FILE.
/DLC0/*                MAINTAIN VOPERID FILE SIDE-BY-SIDE FOR NOW.
/DLC1/* 10/11/2019 DLC ALLOW READ-ONLY ACCESS FOR GENERAL USERS.
/DLC2/* 11/19/2019 DLC REDESIGN FOR 132-COLUMN SCREENS.
/DLC2/*                PLUS ADD LINK TO VSEC PERMISSIONS.
/DLC3/* 12/23/2019 DLC ELIMINATE VOPERID FILE NOW.
/DLC4/* 01/07/2020 DLC ADD WINSCRTY TEST OPTION TO LIST SCREEN.
/DLC5/* 01/17/2020 DLC ADD AD QUERY OPTION TO REFRESH ATTRIBUTES.
/DLC6/* 03/17/2020 DLC ADD A REPORT OF ALL WINSCRTY TEST RESULTS.
/DLC7/* 04/08/2021 DLC ADD "OKCITY" AS WIN UNIT.
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
       OBJECT-COMPUTER. IBM-2096-N03.

       SPECIAL-NAMES.
           CLASS ALPHAMERIC-NATIONAL IS ' ', '@', '#', '$',
                                        'A' THRU 'I',
                                        'J' THRU 'R',
                                        'S' THRU 'Z',
                                        '0' THRU '9',
           CLASS DATE-FILTER-CHARS IS ' ', '?', '*', '<', '>', '/',
                                      '0' THRU '9'.

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
         COPY COMMWORK
                             REPLACING =='TRAN'==    BY =='VUPM'==
                                       =='PROGRAM'== BY =='VUPMPGM'==
                                       =='LINKPGM'== BY =='VUSRPGM'==.
         03  VUPMMS                    PIC  X(08)   VALUE 'VUPMMS  '.
         03  LISTMAP                   PIC  X(08)   VALUE 'LISTMAP '.
         03  UPM1MAP                   PIC  X(08)   VALUE 'UPM1MAP '.
         03  UPM2MAP                   PIC  X(08)   VALUE 'UPM2MAP '.
         03  UPM3MAP                   PIC  X(08)   VALUE 'UPM3MAP '.
         03  UPM4MAP                   PIC  X(08)   VALUE 'UPM4MAP '.
         03  DOCUMAP                   PIC  X(08)   VALUE 'DOCUMAP '.
         03  LDAPWIN                   PIC  X(08)   VALUE 'LDAPWIN '.
/DLC4/   03  TESTWIN                   PIC  X(08)   VALUE 'TESTWIN '.
         03  WAITWIN                   PIC  X(08)   VALUE 'WAITWIN '.
         03  LISTXXX                   PIC  X(08)   VALUE 'LISTXXX '.
         03  UPM2XXX                   PIC  X(08)   VALUE 'UPM2XXX '.
         03  UPM3XXX                   PIC  X(08)   VALUE 'UPM3XXX '.
         03  UPM4XXX                   PIC  X(08)   VALUE 'UPM4XXX '.
      *
      * FIELDS TO MANAGE REFERENCED FILE NAMES
         03  IESCNTL                   PIC  X(08)   VALUE 'IESCNTL '.
         03  IESLDUM                   PIC  X(08)   VALUE 'IESLDUM '.
         03  IESLDUV                   PIC  X(08)   VALUE 'IESLDUV '.
/DLC3/*  03  VOPERID                   PIC  X(08)   VALUE 'VOPERID '.
/DLC3/*  03  VOPERPU                   PIC  X(08)   VALUE 'VOPERPU '.
/DLC0/   03  VUSERID                   PIC  X(08)   VALUE 'VUSERID '.
/DLC0/   03  VUSEROP                   PIC  X(08)   VALUE 'VUSEROP '.
/DLC2/   03  COMASTR                   PIC  X(08)   VALUE 'COMASTR '.
/DLC2/   03  COMASTA                   PIC  X(08)   VALUE 'COMASTA '.
/DLC2/   03  COMASTI                   PIC  X(08)   VALUE 'COMASTI '.
/DLC2/   03  COMASTW                   PIC  X(08)   VALUE 'COMASTW '.
      *
      * FIELDS TO MANAGE REFERENCED EXTERNAL PROGRAM NAMES
         03  JOBSSELT                  PIC  X(08)   VALUE 'JOBSSELT'.
         03  IESLDCFG                  PIC  X(08)   VALUE 'IESLDCFG'.
         03  IESLDGAC                  PIC  X(08)   VALUE 'IESLDGAC'.
         03  MULTMESG                  PIC  X(08)   VALUE 'MULTMESG'.
         03  VSECREFR                  PIC  X(08)   VALUE 'VSECREFR'.
/DLC2/   03  VSECPGM                   PIC  X(08)   VALUE 'VSECPGM'.
         03  VUPMSKEL                  PIC  X(08)   VALUE 'VUPMSKEL'.
      *
      * FIELDS TO MANAGE ERROR HANDLING
         03  ERROR-SWITCH              PIC S9(01)   VALUE ZEROES.
           88  NO-ERRORS-FOUND                      VALUE ZEROES.
           88  ERROR-AT-CURSOR                      VALUE -1.
           88  ERRORS-FOUND                         VALUES -9 THRU -1.
         03  EXPIRE-SWITCH             PIC  X(01)   VALUE 'N'.
           88  SET-EXPIRATION                       VALUE 'Y'.
           88  KEEP-EXPIRATION                      VALUE 'N'.
         03  RECORD-SWITCH             PIC  X(01)   VALUE 'N'.
           88  RECORD-NOT-SELECTED                  VALUE 'N'.
           88  RECORD-IS-SELECTED                   VALUE 'Y'.
         03  UPDATE-SWITCH             PIC  X(01)   VALUE 'N'.
           88  UPDATES-PROCESSED                    VALUE 'Y'.
           88  NO-UPDATES-FOUND                     VALUE 'N'.
         03  IDX                       PIC S9(04)   BINARY VALUE ZEROES.
         03  CNT                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LEN                       PIC S9(04)   BINARY VALUE ZEROES.
         03  POS                       PIC S9(04)   BINARY VALUE ZEROES.
         03  PTR                       PIC S9(04)   BINARY VALUE ZEROES.
         03  SUB                       PIC S9(04)   BINARY VALUE ZEROES.
         03  JSUB                      PIC S9(04)   BINARY VALUE ZEROES.
         03  XGRP                      PIC S9(04)   BINARY VALUE ZEROES.
         03  XBYT                      PIC S9(04)   BINARY VALUE ZEROES.
         03  XBIT                      PIC S9(04)   BINARY VALUE ZEROES.
         03  CNTL-RECL                 PIC S9(04)   BINARY VALUE ZEROES.
      *
      * FIELDS TO CALCULATE CURSOR POSITION AND MANAGE MAPS W/ OCCURS
         03  ROW                       PIC S9(04)   BINARY VALUE ZEROES.
         03  COL                       PIC S9(04)   BINARY VALUE ZEROES.
         03  LISTHEAD                  PIC S9(04)   BINARY VALUE +06.
         03  LISTROWS                  PIC S9(04)   BINARY VALUE +18.
         03  UPM1HEAD                  PIC S9(04)   BINARY VALUE +21.
         03  UPM1ROWS                  PIC S9(04)   BINARY VALUE +02.
         03  UPM3HEAD                  PIC S9(04)   BINARY VALUE +14.
         03  UPM3ROWS                  PIC S9(04)   BINARY VALUE +08.
         03  UPM4HEAD                  PIC S9(04)   BINARY VALUE +05.
         03  UPM4ROWS                  PIC S9(04)   BINARY VALUE +03.
         03  UPM4HEAD2                 PIC S9(04)   BINARY VALUE +13.
         03  UPM4ROWS2                 PIC S9(04)   BINARY VALUE +04.
         03  LINE-CNT                  PIC S9(04)   BINARY VALUE +99.
         03  PAGE-CNT                  PIC S9(04)   BINARY VALUE ZEROES.
      *
         03  POPUP-TEXT                PIC  X(15)   VALUE SPACES.
         03  WORD1                     PIC  X(15)   VALUE SPACES.
         03  WORD2                     PIC  X(15)   VALUE SPACES.
         03  WORD3                     PIC  X(15)   VALUE SPACES.
         03  WORD4                     PIC  X(15)   VALUE SPACES.
         03  LOWER-CASE  PIC  X(26)  VALUE 'abcdefghijklmnopqrstuvwxyz'.
         03  UPPER-CASE  PIC  X(26)  VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
         03  MULTI-EDIT-MSG            PIC  X(78)   VALUE SPACES.
         03  VAR-DATA                  PIC  X(98)   VALUE SPACES.
      *
      * TO CONVERT NUMBERS TO ZONED-DECIMAL CHARACTER
         03  NUM-WORK                  PIC  9(04)   VALUE ZEROES.
         03  NUM-WRK4                  REDEFINES    NUM-WORK.
           05  FILLER                  PIC  X(01).
           05  NUM-WRK3.
             07  FILLER                PIC  X(01).
             07  NUM-WRK2.
               09  FILLER              PIC  X(01).
               09  NUM-WRK1            PIC  X(01).
      *
      * TO CONVERT NUMERIC TO BINARY-CODED CHARACTER AND VICE VERSA
         03  DOUBLE-WORD               PIC S9(18)   BINARY.
         03  EIGHT-BYTES               REDEFINES    DOUBLE-WORD.
           05                          PIC  X(2).
           05  SIX-BYTES.
             07                        PIC  X(2).
             07  FULL-WORD             PIC S9(9)    BINARY.
             07  FOUR-BYTES            REDEFINES    FULL-WORD.
               09  HI-MSB              PIC  X.
               09  THREE-BYTES.
                 11  HI-LSB            PIC  X.
                 11  HALF-WORD         PIC S9(4)    BINARY.
                 11  TWO-BYTES         REDEFINES    HALF-WORD.
                   13  LO-MSB          PIC  X.
                   13  LO-LSB          PIC  X.
      *
      * TO CONVERT DATES AND TIMES
         03  NUM-DATE                  PIC  9(8).
         03  WRK-DATE                  REDEFINES    NUM-DATE.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYR                PIC  9999.
         03  WS-ABSTIME                PIC S9(15)   COMP-3 VALUE ZEROES.
         03  WS-WRKDATE.
           05  WS-MODATE               PIC  9(02).
           05                          PIC  X.
           05  WS-DADATE               PIC  9(02).
           05                          PIC  X.
           05  WS-CYDATE               PIC  9(04).
         03  NUM-DATE6                 PIC  9(6).
         03  WRK-DATE6                 REDEFINES    NUM-DATE6.
           05  DTE-GRMO                PIC  99.
           05  DTE-GRDA                PIC  99.
           05  DTE-GRYY                PIC  99.
         03  EDT-DATE                  PIC  99/99/99.
         03  EDT-DATE-X                REDEFINES    EDT-DATE.
           05  EDT-YY                  PIC  9(02).
           05                          PIC  X(01).
           05  EDT-MM                  PIC  9(02).
           05                          PIC  X(01).
           05  EDT-DD                  PIC  9(02).
         03  WS-WRKTIME.
           05  WS-HRSTIME              PIC  9(02).
           05                          PIC  X(03).
           05  WS-XXXTIME              PIC  X(03).

       01  RPTDATA.
           05  FILLER                  PIC  X(40)   VALUE
               'USERIDNTIUI USER PROFILES BY USER ID    '.
           05  FILLER                  PIC  X(40)   VALUE
               'USERNAMEIUI USER PROFILES BY USER NAME  '.
           05  FILLER                  PIC  X(40)   VALUE
               'USERTYPEIUI USER PROFILES BY USER TYPE  '.
           05  FILLER                  PIC  X(40)   VALUE
               'INITNAMEIUI USER PROFILES BY INIT NAME  '.
/DLC6/     05  FILLER                  PIC  X(40)   VALUE
/DLC6/         'WINSFUNCALL WINSCRTY TESTS BY WINFUNCTIN'.
/DLC6/     05  FILLER                  PIC  X(40)   VALUE
/DLC6/         'WINSOGRPALL WINSCRTY TESTS BY OLD GROUP '.
       01  RPTTABLE        REDEFINES   RPTDATA.
           05  RPTENTRY    OCCURS 6.
             10  RPTTOKEN              PIC  X(08).
             10  RPTTITLE              PIC  X(32).

       01  DOCUHDRD.
         02  USERHDR1.
           05  FILLER                  PIC  X(01)   VALUE '1'.
           05  FILLER                  PIC  X(45)   VALUE 'APPLID: '.
           05  FILLER                  PIC  X(41)   VALUE
               '    VSE/SP INTERACTIVE USER INTERFACE    '.
           05  FILLER                  PIC  X(30)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'DATE: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  USERHDR2.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(50)   VALUE 'USERID: '.
           05  FILLER                  PIC  X(31)   VALUE
               '   USER PROFILES BY USER ID    '.
           05  FILLER                  PIC  X(35)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'TIME: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  USERHDR3.
           05  FILLER                  PIC  X(01)   VALUE '0'.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(40)   VALUE
               ' USERID         USER NAME         OPR   '.
           05  FILLER                  PIC  X(40)   VALUE
               'EXPIRES  F REVOKED    LAST ON    TYPE   '.
           05  FILLER                  PIC  X(43)   VALUE
               'INITNAME   TYPE    CICS SECURITY     OPRCLS'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  USERHDR4.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(40)   VALUE
               '--------   --------------------   ---   '.
           05  FILLER                  PIC  X(40)   VALUE
               '-------- - --------   --------   ----   '.
           05  FILLER                  PIC  X(43)   VALUE
               '--------   ----   ----------------   ------'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  USERHDR5.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(132)  VALUE SPACES.
         02  DESCHDR1.
           05  FILLER                  PIC  X(01)   VALUE '1'.
           05  FILLER                  PIC  X(45)   VALUE 'APPLID: '.
           05  FILLER                  PIC  X(41)   VALUE
               '    VSE/SP INTERACTIVE USER INTERFACE    '.
           05  FILLER                  PIC  X(30)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'DATE: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  DESCHDR2.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(50)   VALUE 'USERID: '.
           05  FILLER                  PIC  X(31)   VALUE
               '  USER PROFILES BY USER NAME   '.
           05  FILLER                  PIC  X(35)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'TIME: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  DESCHDR3.
           05  FILLER                  PIC  X(01)   VALUE '0'.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(40)   VALUE
               '     USER NAME          USERID    OPR   '.
           05  FILLER                  PIC  X(40)   VALUE
               'EXPIRES  F REVOKED    LAST ON    TYPE   '.
           05  FILLER                  PIC  X(43)   VALUE
               'INITNAME   TYPE    CICS SECURITY     OPRCLS'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  DESCHDR4.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(40)   VALUE
               '--------------------   --------   ---   '.
           05  FILLER                  PIC  X(40)   VALUE
               '-------- - --------   --------   ----   '.
           05  FILLER                  PIC  X(43)   VALUE
               '--------   ----   ----------------   ------'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  DESCHDR5.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(132)  VALUE SPACES.
         02  TYPEHDR1.
           05  FILLER                  PIC  X(01)   VALUE '1'.
           05  FILLER                  PIC  X(45)   VALUE 'APPLID: '.
           05  FILLER                  PIC  X(41)   VALUE
               '    VSE/SP INTERACTIVE USER INTERFACE    '.
           05  FILLER                  PIC  X(30)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'DATE: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  TYPEHDR2.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(50)   VALUE 'USERID: '.
           05  FILLER                  PIC  X(31)   VALUE
               '  USER PROFILES BY USER TYPE   '.
           05  FILLER                  PIC  X(35)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'TIME: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  TYPEHDR3.
           05  FILLER                  PIC  X(01)   VALUE '0'.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(41)   VALUE
               'TYPE    USERID         USER NAME         '.
           05  FILLER                  PIC  X(39)   VALUE
               'OPR   EXPIRES  F REVOKED    LAST ON    '.
           05  FILLER                  PIC  X(43)   VALUE
               'INITNAME   TYPE    CICS SECURITY     OPRCLS'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  TYPEHDR4.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(41)   VALUE
               '----   --------   --------------------   '.
           05  FILLER                  PIC  X(39)   VALUE
               '---   -------- - --------   --------   '.
           05  FILLER                  PIC  X(43)   VALUE
               '--------   ----   ----------------   ------'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  TYPEHDR5.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(132)  VALUE SPACES.
         02  INITHDR1.
           05  FILLER                  PIC  X(01)   VALUE '1'.
           05  FILLER                  PIC  X(45)   VALUE 'APPLID: '.
           05  FILLER                  PIC  X(41)   VALUE
               '    VSE/SP INTERACTIVE USER INTERFACE    '.
           05  FILLER                  PIC  X(30)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'DATE: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  INITHDR2.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(50)   VALUE 'USERID: '.
           05  FILLER                  PIC  X(31)   VALUE
               '  USER PROFILES BY INIT NAME   '.
           05  FILLER                  PIC  X(35)   VALUE SPACES.
           05  FILLER                  PIC  X(06)   VALUE 'TIME: '.
           05  FILLER                  PIC  X(10)   VALUE SPACES.
         02  INITHDR3.
           05  FILLER                  PIC  X(01)   VALUE '0'.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(52)   VALUE
               'INITNAME   TYPE    USERID         USER NAME         '.
           05  FILLER                  PIC  X(39)   VALUE
               'OPR   EXPIRES  F REVOKED    LAST ON    '.
           05  FILLER                  PIC  X(32)   VALUE
               'TYPE    CICS SECURITY     OPRCLS'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  INITHDR4.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(04)   VALUE SPACES.
           05  FILLER                  PIC  X(52)   VALUE
               '--------   ----   --------   --------------------   '.
           05  FILLER                  PIC  X(39)   VALUE
               '---   -------- - --------   --------   '.
           05  FILLER                  PIC  X(32)   VALUE
               '----   ----------------   ------'.
           05  FILLER                  PIC  X(05)   VALUE SPACES.
         02  INITHDR5.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(132)  VALUE SPACES.
/DLC6/   02  WINFHDR1.
/DLC6/     05                          PIC  X(01)   VALUE '1'.
/DLC6/     05                          PIC  X(45)   VALUE 'APPLID: '.
/DLC6/     05                          PIC  X(41)   VALUE
/DLC6/         '    VSE/SP INTERACTIVE USER INTERFACE    '.
/DLC6/     05                          PIC  X(30)   VALUE SPACES.
/DLC6/     05                          PIC  X(06)   VALUE 'DATE: '.
/DLC6/     05                          PIC  X(10)   VALUE SPACES.
/DLC6/   02  WINFHDR2.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(49)   VALUE 'USERID: '.
/DLC6/     05                          PIC  X(33)   VALUE
/DLC6/         'ALL WINSCRTY TESTS BY WINFUNCTION'.
/DLC6/     05                          PIC  X(34)   VALUE SPACES.
/DLC6/     05                          PIC  X(06)   VALUE 'TIME: '.
/DLC6/     05                          PIC  X(10)   VALUE SPACES.
/DLC6/   02  WINFHDR3.
/DLC6/     05                          PIC  X(01)   VALUE '0'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/     05                          PIC  X(47)   VALUE
/DLC6/         ' USERID       USER NAME       LAST ON    COMP# '.
/DLC6/     05                          PIC  X(46)   VALUE
/DLC6/         'ACS SVC ALT RMT ALT DFTP FNC    UNIT     ORG  '.
/DLC6/     05                          PIC  X(25)   VALUE
/DLC6/         ' POS  DIR DAP CLS GRP LVL'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/   02  WINFHDR4.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/     05                          PIC  X(47)   VALUE
/DLC6/         '-------- -------------------- --------   ----- '.
/DLC6/     05                          PIC  X(46)   VALUE
/DLC6/         '--- --- --- --- --- ---- --- ---------- ----- '.
/DLC6/     05                          PIC  X(25)   VALUE
/DLC6/         '----- --- --- --- --- ---'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/   02  WINFHDR5.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(132)  VALUE SPACES.
/DLC6/   02  WINGHDR1.
/DLC6/     05                          PIC  X(01)   VALUE '1'.
/DLC6/     05                          PIC  X(45)   VALUE 'APPLID: '.
/DLC6/     05                          PIC  X(41)   VALUE
/DLC6/         '    VSE/SP INTERACTIVE USER INTERFACE    '.
/DLC6/     05                          PIC  X(30)   VALUE SPACES.
/DLC6/     05                          PIC  X(06)   VALUE 'DATE: '.
/DLC6/     05                          PIC  X(10)   VALUE SPACES.
/DLC6/   02  WINGHDR2.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(49)   VALUE 'USERID: '.
/DLC6/     05                          PIC  X(33)   VALUE
/DLC6/         ' ALL WINSCRTY TESTS BY OLD GROUP '.
/DLC6/     05                          PIC  X(34)   VALUE SPACES.
/DLC6/     05                          PIC  X(06)   VALUE 'TIME: '.
/DLC6/     05                          PIC  X(10)   VALUE SPACES.
/DLC6/   02  WINGHDR3.
/DLC6/     05                          PIC  X(01)   VALUE '0'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/     05                          PIC  X(47)   VALUE
/DLC6/         ' USERID       USER NAME       LAST ON    COMP# '.
/DLC6/     05                          PIC  X(46)   VALUE
/DLC6/         'ACS SVC ALT RMT ALT DFTP FNC    UNIT     ORG  '.
/DLC6/     05                          PIC  X(25)   VALUE
/DLC6/         ' POS  DIR DAP CLS GRP LVL'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/   02  WINGHDR4.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/     05                          PIC  X(47)   VALUE
/DLC6/         '-------- -------------------- --------   ----- '.
/DLC6/     05                          PIC  X(46)   VALUE
/DLC6/         '--- --- --- --- --- ---- --- ---------- ----- '.
/DLC6/     05                          PIC  X(25)   VALUE
/DLC6/         '----- --- --- --- --- ---'.
/DLC6/     05                          PIC  X(07)   VALUE SPACES.
/DLC6/   02  WINGHDR5.
/DLC6/     05                          PIC  X(01)   VALUE ' '.
/DLC6/     05                          PIC  X(132)  VALUE SPACES.
       01  DOCUHDRT        REDEFINES   DOCUHDRD.
           05  DOCUHEAD                             OCCURS 6.
             07  DOCUHDRG.
               09  DOCUHDRE            PIC  X(133)  OCCURS 5.
             07  DOCUHDRX  REDEFINES   DOCUHDRG.
               09  DOCUHDR1.
                 11  FILLER            PIC  X(01).
                 11  FILLER            PIC  X(08).
                 11  HEADAPPL          PIC  X(08).
                 11  FILLER            PIC  X(100).
                 11  FILLER            PIC  X(06).
                 11  HEADDATE          PIC  X(10).
               09  DOCUHDR2.
                 11  FILLER            PIC  X(01).
                 11  FILLER            PIC  X(08).
                 11  HEADUSER          PIC  X(08).
                 11  FILLER            PIC  X(100).
                 11  FILLER            PIC  X(06).
                 11  HEADTIME          PIC  X(10).
               09  DOCUHDR3            PIC  X(133).
               09  DOCUHDR4            PIC  X(133).
               09  DOCUHDR5            PIC  X(133).

       01  DOCULINE.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(132)  VALUE SPACES.
       01  USERLINE        REDEFINES   DOCULINE.
           05  FILLER                  PIC  X(01).
           05  FILLER                  PIC  X(04).
           05  SORTUSER                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTDESC                PIC  X(20).
           05  FILLER                  PIC  X(03).
           05  SORTOPID                PIC  X(03).
           05  FILLER                  PIC  X(03).
           05  SORTEXPS                PIC  X(08).
           05  FILLER                  PIC  X(01).
           05  SORTEXPD                PIC  X(01).
           05  FILLER                  PIC  X(01).
           05  SORTREVK                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTLSGN                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTUTYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTINAM                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTITYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTCICS                PIC  X(16).
           05  FILLER                  PIC  X(03).
           05  SORTOPER                PIC  X(06).
           05  FILLER                  PIC  X(05).
       01  DESCLINE        REDEFINES   DOCULINE.
           05  FILLER                  PIC  X(01).
           05  FILLER                  PIC  X(04).
           05  SORTDESC                PIC  X(20).
           05  FILLER                  PIC  X(03).
           05  SORTUSER                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTOPID                PIC  X(03).
           05  FILLER                  PIC  X(03).
           05  SORTEXPS                PIC  X(08).
           05  FILLER                  PIC  X(01).
           05  SORTEXPD                PIC  X(01).
           05  FILLER                  PIC  X(01).
           05  SORTREVK                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTLSGN                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTUTYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTINAM                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTITYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTCICS                PIC  X(16).
           05  FILLER                  PIC  X(03).
           05  SORTOPER                PIC  X(06).
           05  FILLER                  PIC  X(05).
       01  TYPELINE        REDEFINES   DOCULINE.
           05  FILLER                  PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTUTYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTUSER                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTDESC                PIC  X(20).
           05  FILLER                  PIC  X(03).
           05  SORTOPID                PIC  X(03).
           05  FILLER                  PIC  X(03).
           05  SORTEXPS                PIC  X(08).
           05  FILLER                  PIC  X(01).
           05  SORTEXPD                PIC  X(01).
           05  FILLER                  PIC  X(01).
           05  SORTREVK                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTLSGN                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTINAM                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTITYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTCICS                PIC  X(16).
           05  FILLER                  PIC  X(03).
           05  SORTOPER                PIC  X(06).
           05  FILLER                  PIC  X(05).
       01  INITLINE        REDEFINES   DOCULINE.
           05  FILLER                  PIC  X(01).
           05  FILLER                  PIC  X(04).
           05  SORTINAM                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTITYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTUSER                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTDESC                PIC  X(20).
           05  FILLER                  PIC  X(03).
           05  SORTOPID                PIC  X(03).
           05  FILLER                  PIC  X(03).
           05  SORTEXPS                PIC  X(08).
           05  FILLER                  PIC  X(01).
           05  SORTEXPD                PIC  X(01).
           05  FILLER                  PIC  X(01).
           05  SORTREVK                PIC  X(08).
           05  FILLER                  PIC  X(03).
           05  SORTLSGN                PIC  X(08).
           05  FILLER                  PIC  X(04).
           05  SORTUTYP                PIC  X(01).
           05  FILLER                  PIC  X(05).
           05  SORTCICS                PIC  X(16).
           05  FILLER                  PIC  X(03).
           05  SORTOPER                PIC  X(06).
           05  FILLER                  PIC  X(05).
/DLC6/ 01  WINSLINE        REDEFINES   DOCULINE.
/DLC6/     05                          PIC  X(01).
/DLC6/     05                          PIC  X(07).
/DLC6/     05  SORTUSER                PIC  X(08).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTDESC                PIC  X(20).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTSIGN                PIC  X(08).
/DLC6/     05                          PIC  X(03).
/DLC6/     05  SORTCOMP                PIC  X(05).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTACCS                PIC  X(01).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTSRGN                PIC  X(02).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTARGN                PIC  X(02).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTRRMT                PIC  X(03).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTARMT                PIC  X(03).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTDFTP                PIC  X(04).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTFUNC                PIC  X(03).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTUNIT                PIC  X(10).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTWORG                PIC  X(05).
/DLC6/     05                          PIC  X(01).
/DLC6/     05  SORTWPOS                PIC  X(05).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTWDIR                PIC  X(01).
/DLC6/     05                          PIC  X(03).
/DLC6/     05  SORTODAP                PIC  X(01).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTOCLS                PIC  X(02).
/DLC6/     05                          PIC  X(02).
/DLC6/     05  SORTOGRP                PIC  X(02).
/DLC6/     05                          PIC  X(03).
/DLC6/     05  SORTOLVL                PIC  X(01).
/DLC6/     05                          PIC  X(01).
/DLC6/     05                          PIC  X(07).

       01  DOCUTAIL.
           05  FILLER                  PIC  X(01)   VALUE ' '.
           05  FILLER                  PIC  X(121)  VALUE
               'BY DAVE L. CLARK I'.
           05  FILLER                  PIC  X(06)   VALUE 'PAGE: '.
           05  DOCUPAGE                PIC  Z(05).

       01  VSECSERV-CONTROL.
         03  VSECSERV                  PIC  X(8)    VALUE 'VSECSERV'.
         03  VSECSERV-RESOURCE         PIC  X(39)   VALUE SPACES.
       01  VSECSERV-COMMAREA.
         COPY VSECSERV.

      * SUBMISSION JOB CONTROL AREAS

       01  LDUMA-AREA.
         03  IESLDUMA                  PIC  X(08)   VALUE 'IESLDUMA'.
       01  LDUMA-JCL.
         03  PIC X(40) VALUE '* $$ JOB JNM=IESLDUMA,DISP=D,CLASS=0    '.
         03  PIC X(40) VALUE '* $$ LST DISP=D,CLASS=Z,PURGE=0         '.
         03  PIC X(40) VALUE '// JOB    IESLDUMA   LDAP User Mapping  '.
         03  PIC X(40) VALUE '// EXEC   PGM=IESLDUMA,SIZE=*           '.
         03  PIC X(40) VALUE 'ID USER=''SYSA'' PWD=''BATCH$''         '.
         03  PIC X(40) VALUE '+++INPUT+++                             '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '// IF     $RC = 0 THEN                  '.
/DLC5/   03  PIC X(40) VALUE '// UPSI   0    /* conditional refresh */'.
         03  PIC X(40) VALUE '// EXEC   PGM=VUSRATTR,PARM=''&MFID''   '.
         03  PIC X(40) VALUE '/* EOD                                  '.
         03  PIC X(40) VALUE '/& EOJ                                  '.
         03  PIC X(40) VALUE '* $$ EOJ                                '.
       01  LDUMA-TABLE                 REDEFINES    LDUMA-JCL.
         03  LDUMA-ENTRY               PIC  X(40)   OCCURS 13.

/DLC5/ 01  VATTR-AREA.
/DLC5/   03  VUSRATTR                  PIC  X(08)   VALUE 'VUSRATTR'.
/DLC5/ 01  VATTR-JCL.
/DLC5/   03  PIC X(40) VALUE '* $$ JOB JNM=VUSRATTR,DISP=D,CLASS=0    '.
/DLC5/   03  PIC X(40) VALUE '* $$ LST DISP=D,CLASS=Z,PURGE=0         '.
/DLC5/   03  PIC X(40) VALUE '// JOB    VUSRATTR   LDAP USER MAPPING  '.
/DLC5/   03  PIC X(40) VALUE '// UPSI   1    /* forced refresh */'.
/DLC5/   03  PIC X(40) VALUE '// EXEC   PGM=VUSRATTR,PARM=''&MFID''   '.
/DLC5/   03  PIC X(40) VALUE '/* EOD                                  '.
/DLC5/   03  PIC X(40) VALUE '/& EOJ                                  '.
/DLC5/   03  PIC X(40) VALUE '* $$ EOJ                                '.
/DLC5/ 01  VATTR-TABLE                 REDEFINES    VATTR-JCL.
/DLC5/   03  VATTR-ENTRY               PIC  X(40)   OCCURS 8.

      * IBM LDAP GET ATTRIBUTE INTERFACE
       COPY IESLDGAC.

      * THE FOLLOWING AREAS ARE SUBROUTINE PARAMETER BLOCKS

       01  CICSINFO-CONTROL.
        02  CICSINFO                   PIC  X(08)   VALUE 'CICSINFO'.
        02  CICSINFO-PARMS.
         COPY CICSINFO.

       COPY CICSSORT.

       COPY BITMAN.

       COPY DTEMAN.

       COPY HEXMAN.

       COPY LOGGING.

       COPY NUMMAN.

       COPY PRTMAN.

       COPY TXTMAN.

       COPY UNEXERRW.
           05  CONFIRM-MSG   REDEFINES UNEX-MSG     PIC  X(79).

       COPY WILDCOMP.

/DLC4/ 01  WINSCRTY-CONTROL.
/DLC4/   03  WINSCRTY                  PIC  X(08)   VALUE 'WINSCRTY'.
/DLC4/  02  WINSCRTY-PARMS.
/DLC4/   COPY WINSCRTY.

      /*****************************************************************
      *    LINKAGE SECTION                                             *
      ******************************************************************
       LINKAGE SECTION.

      * VUPM CICS COMMUNICATION AREA
       01  DFHCOMMAREA.
         COPY COMMAREA.
      *
           05  VUPM-SAVEKEY            PIC  X(08).
           05  VUPM-CNTL-KEY           PIC  X(08).
           05  SAVE-FILTERS.
/DLC2/       07  SAVE-OPERID-FILTER    PIC  X(4).
             07  SAVE-USERNAME-FILTER  PIC  X(20).
             07  SAVE-EXPIRES-FILTER   PIC  X(08).
             07  SAVE-EXPIRED-FILTER   PIC  X(01).
             07  SAVE-REVOKED-FILTER   PIC  X(08).
             07  SAVE-SIGNON-FILTER    PIC  X(08).
             07  SAVE-USERTYPE-FILTER  PIC  X(01).
             07  SAVE-INITIAL-FILTER   PIC  X(08).
             07  SAVE-NAMETYPE-FILTER  PIC  X(01).
/DLC2/       07  SAVE-PRIME-CO-FILTER  PIC  X(5).
/DLC2/       07  SAVE-PRIME-IND-FILTER PIC  X(1).
/DLC2/       07  SAVE-WIN-UNIT-FILTER  PIC  X(10).
/DLC2/       07  SAVE-WIN-POS-FILTER   PIC  X(5).
/DLC2/       07  SAVE-WIN-ORG-FILTER   PIC  X(5).
/DLC2/       07  SAVE-WIN-FUNC-FILTER  PIC  X(3).
/DLC2/       07  SAVE-WIN-DIR-FILTER   PIC  X(1).
           05  VUPM-RPTOKEN            PIC  X(08).
           05  VUPM-RPTSEQU            PIC  9(01).
           05  LDAP-PREVRTN            PIC  X(04).
           05  VUPM-PROCESS            PIC  X(01).
             88  VIEW-PROCESS                       VALUE 'V'.
             88  ADD-PROCESS                        VALUE 'A'.
             88  CHG-PROCESS                        VALUE 'C'.
             88  SET-PROCESS                        VALUE 'S'.
           05  ICCF-SWITCH             PIC  X(01).
             88  IS-ICCF-USER                       VALUE 'Y'.
             88  NON-ICCF-USER                      VALUE 'N'.
           05  CURRENT-USER            PIC  X(01).
             88  USER-IS-ADMIN                      VALUE 'A'.
             88  USER-IS-SUPER                      VALUE 'S'.
             88  USER-IS-GENERAL                    VALUE 'G'.
           05  NAME-LENGTH             PIC S9(04)   BINARY.
      *
           05  VUPM-IUI-FIELDS.
             07  LIST-EXPIRES          PIC  X(08).
             07  LIST-EXPIRED          PIC  X(01).
             07  LIST-SIGNON           PIC  X(08).
             07  LIST-OPERCLS          PIC  X(02).
             07  LIST-OPERDAP          PIC  X(01).
             07  UPM1-USRIDNT          PIC  X(08).
             07  UPM1-OPRIDNT          PIC  X(03).
             07  UPM1-USRNAME          PIC  X(20).
             07  UPM1-USRTYPE          PIC  X(01).
               88  IS-ADMINISTRATOR                 VALUE '1'.
               88  IS-PROGRAMMER                    VALUE '2'.
               88  IS-GENERAL-USER                  VALUE '3'.
             07  UPM1-PASSWRD          PIC  X(08).
             07  UPM1-EXPDAYS          PIC  X(03).
             07  UPM1-REVOKED          PIC  X(08).
             07  UPM1-INITNAM          PIC  X(08).
             07  UPM1-INITYPE          PIC  X(01).
             07  UPM1-USRMODL          PIC  X(08).
             07  UPM1-OLDPSWTB.
               09  UPM1-OLDPSWD        PIC  X(08)   OCCURS 12.
             07  UPM2-IESNEWS          PIC  X(01).
             07  UPM2-IESESCP          PIC  X(01).
             07  UPM2-IESCPWD          PIC  X(01).
             07  UPM2-IESCDEL          PIC  X(01).
             07  UPM2-IESVPSL          PIC  X(01).
             07  UPM2-IESSUBM          PIC  X(01).
             07  UPM2-VSMFILE          PIC  X(01).
             07  UPM2-VSMCATL          PIC  X(01).
             07  UPM2-IESOLPD          PIC  X(01).
             07  UPM2-IESCNSL          PIC  X(01).
             07  UPM2-IESCMSG          PIC  X(01).
             07  UPM2-IESBQUE          PIC  X(01).
             07  UPM2-APMAUTH          PIC  X(01).
             07  UPM2-SPMAUTH          PIC  X(01).
             07  UPM2-UPMAUTH          PIC  X(01).
             07  UPM2-USERCAT          PIC  X(08).
             07  UPM3-OPERPRI          PIC  X(03).
             07  UPM3-XRFSGNO          PIC  X(01).
             07  UPM3-TIMEOUT          PIC  X(02).
             07  UPM3-NATLANG          PIC  X(01).
             07  UPM3-TRANSEC          PIC  X(01)   OCCURS 64.
             07  UPM4-OPCLASS          PIC  X(01)   OCCURS 24.
             07  UPM4-DTSECTAB.
               09  UPM4-DTSECTB        PIC  9(01)   OCCURS 32.
             07  UPM4-DTSREAD          PIC  X(01).
             07  UPM4-DTSBTRN          PIC  X(01).
      *
           05  TS-QUEUE.
             07  TS-QUE-TRAN           PIC  X(04).
             07  TS-QUE-TERM           PIC  X(04).
           05  TS-TOTL                 PIC S9(04)   BINARY.
           05  TS-ITEM                 PIC S9(04)   BINARY.
           05  SS-ITEM                 PIC S9(04)   BINARY.
      *
           05  VUPM-LIST-TIOA          PIC  X(3749).
           05  VUPM-SAVE-TIOA          PIC  X(982).

      * VUPM TEMP COMMUNICATION AREA
       COPY COMMTEMP.

      * 3270 DATA STREAM CONTROL CHARACTERS
       COPY IBM3270.

      * IESCNTL RECORD I/O AREA
       COPY IESCNTL.

      * IESLDUM RECORD I/O AREA
       COPY IESLDUM.

/DLC3/* VOPERID RECORD I/O AREA
/DLC3/*COPY VOPERID.

/DLC0/* VUSERID RECORD I/O AREA
/DLC0/ COPY VUSERID.

/DLC2/* COMASTR RECORD I/O AREA
/DLC2/ COPY COMASTR.

      * IUPM GENERATE SKELETON TABLE
       01  VUPMSKEL-JCL-AREA.
         02  SKELJCLC                  PIC S9(04)   COMP.
      *                                             SKELETON JCL COUNT
         02  SKELJCLT.
      *                                             SKELETON JCL TABLE
           05  SKELCARD    OCCURS 98   PIC  X(80).
      *                                             SKELETON JCL CARD
      *
      * CHECK THE FOLLOWING 88-LEVEL TO INSURE DATA ALIGNMENT
         02  SKELLDOK                  PIC  X(02).
           88  VUPMSKEL-LOADED-OK                   VALUE 'OK'.

      * QUEUE SORT I/O BUFFER
       01  SORTREC.
         03  FILLER                    PIC  X(88).
       01  SORTUID                     REDEFINES    SORTREC.
         03  SORTUSER                  PIC  X(08).
         03  SORTDESC                  PIC  X(20).
         03  SORTOPID                  PIC  X(03).
         03  SORTEXPS                  PIC  X(08).
         03  SORTEXPD                  PIC  X(01).
         03  SORTREVK                  PIC  X(08).
         03  SORTLSGN                  PIC  X(08).
         03  SORTUTYP                  PIC  X(01).
         03  SORTINAM                  PIC  X(08).
         03  SORTITYP                  PIC  X(01).
         03  SORTCICS                  PIC  X(16).
         03  SORTOPER                  PIC  X(06).
       01  SORTUNM                     REDEFINES    SORTREC.
         03  SORTDESC                  PIC  X(20).
         03  SORTUSER                  PIC  X(08).
         03  SORTOPID                  PIC  X(03).
         03  SORTEXPS                  PIC  X(08).
         03  SORTEXPD                  PIC  X(01).
         03  SORTREVK                  PIC  X(08).
         03  SORTLSGN                  PIC  X(08).
         03  SORTUTYP                  PIC  X(01).
         03  SORTINAM                  PIC  X(08).
         03  SORTITYP                  PIC  X(01).
         03  SORTCICS                  PIC  X(16).
         03  SORTOPER                  PIC  X(06).
       01  SORTUTP                     REDEFINES    SORTREC.
         03  SORTUTYP                  PIC  X(01).
         03  SORTUSER                  PIC  X(08).
         03  SORTDESC                  PIC  X(20).
         03  SORTOPID                  PIC  X(03).
         03  SORTEXPS                  PIC  X(08).
         03  SORTEXPD                  PIC  X(01).
         03  SORTREVK                  PIC  X(08).
         03  SORTLSGN                  PIC  X(08).
         03  SORTINAM                  PIC  X(08).
         03  SORTITYP                  PIC  X(01).
         03  SORTCICS                  PIC  X(16).
         03  SORTOPER                  PIC  X(06).
       01  SORTINM                     REDEFINES    SORTREC.
         03  SORTINAM                  PIC  X(08).
         03  SORTITYP                  PIC  X(01).
         03  SORTUSER                  PIC  X(08).
         03  SORTDESC                  PIC  X(20).
         03  SORTOPID                  PIC  X(03).
         03  SORTEXPS                  PIC  X(08).
         03  SORTEXPD                  PIC  X(01).
         03  SORTREVK                  PIC  X(08).
         03  SORTLSGN                  PIC  X(08).
         03  SORTUTYP                  PIC  X(01).
         03  SORTCICS                  PIC  X(16).
         03  SORTOPER                  PIC  X(06).
/DLC6/ 01  SORTWSF                     REDEFINES    SORTREC.
/DLC6/   03  SORTFUNC                  PIC  X(03).
/DLC6/   03  SORTUNIT                  PIC  X(10).
/DLC6/   03  SORTUSER                  PIC  X(08).
/DLC6/   03  SORTDESC                  PIC  X(20).
/DLC6/   03  SORTSIGN                  PIC  X(08).
/DLC6/   03  SORTCOMP                  PIC  X(05).
/DLC6/   03  SORTACCS                  PIC  X(01).
/DLC6/   03  SORTSRGN                  PIC  X(02).
/DLC6/   03  SORTARGN                  PIC  X(02).
/DLC6/   03  SORTRRMT                  PIC  X(03).
/DLC6/   03  SORTARMT                  PIC  X(03).
/DLC6/   03  SORTDFTP                  PIC  X(04).
/DLC6/   03  SORTWORG                  PIC  X(05).
/DLC6/   03  SORTWPOS                  PIC  X(05).
/DLC6/   03  SORTWDIR                  PIC  X(01).
/DLC6/   03  SORTODAP                  PIC  X(01).
/DLC6/   03  SORTOCLS                  PIC  X(02).
/DLC6/   03  SORTOGRP                  PIC  X(02).
/DLC6/   03  SORTOLVL                  PIC  X(01).
/DLC6/ 01  SORTWSG                     REDEFINES    SORTREC.
/DLC6/   03  SORTOGRP                  PIC  X(02).
/DLC6/   03  SORTFUNC                  PIC  X(03).
/DLC6/   03  SORTUNIT                  PIC  X(10).
/DLC6/   03  SORTUSER                  PIC  X(08).
/DLC6/   03  SORTDESC                  PIC  X(20).
/DLC6/   03  SORTSIGN                  PIC  X(08).
/DLC6/   03  SORTCOMP                  PIC  X(05).
/DLC6/   03  SORTACCS                  PIC  X(01).
/DLC6/   03  SORTSRGN                  PIC  X(02).
/DLC6/   03  SORTARGN                  PIC  X(02).
/DLC6/   03  SORTRRMT                  PIC  X(03).
/DLC6/   03  SORTARMT                  PIC  X(03).
/DLC6/   03  SORTDFTP                  PIC  X(04).
/DLC6/   03  SORTWORG                  PIC  X(05).
/DLC6/   03  SORTWPOS                  PIC  X(05).
/DLC6/   03  SORTWDIR                  PIC  X(01).
/DLC6/   03  SORTODAP                  PIC  X(01).
/DLC6/   03  SORTOCLS                  PIC  X(02).
/DLC6/   03  SORTOLVL                  PIC  X(01).

      * BMS TERMINAL I/O AREAS
       COPY VUPMMS.

       01  UPM1MAPX.
           05  FILLER                  PIC  X(100).
           05  FILLER                  PIC  X(177).
           05  UPM1-OLDPSWD-T                       OCCURS 12.
               10  UPM1-OLDPSWD-L      PIC S9(04)   COMP.
               10  UPM1-OLDPSWD-A      PIC  X(01).
               10  UPM1-OLDPSWD-C      PIC  X(01).
               10  UPM1-OLDPSWD-H      PIC  X(01).
               10  UPM1-OLDPSWD-X      PIC  X(08).
           05  FILLER                  PIC  X(83).

       01  UPM3MAPX.
           05  FILLER                  PIC  X(100).
           05  FILLER                  PIC  X(40).
           05  UPM3-SECKEY-T                        OCCURS 64.
               10  UPM3-SECKEY-L       PIC S9(04)   COMP.
               10  UPM3-SECKEY-A       PIC  X(01).
               10  UPM3-SECKEY-C       PIC  X(01).
               10  UPM3-SECKEY-H       PIC  X(01).
               10  UPM3-SECKEY-X       PIC  X(01).
           05  FILLER                  PIC  X(83).

       01  UPM4MAPX.
           05  FILLER                  PIC  X(100).
           05  FILLER                  PIC  X(13).
           05  UPM4-OPCLASS-T                       OCCURS 24.
               10  UPM4-OPCLASS-L      PIC S9(04)   COMP.
               10  UPM4-OPCLASS-A      PIC  X(01).
               10  UPM4-OPCLASS-C      PIC  X(01).
               10  UPM4-OPCLASS-H      PIC  X(01).
               10  UPM4-OPCLASS-X      PIC  X(01).
           05  UPM4-DTSECTAB-T                      OCCURS 32.
               10  UPM4-DTSECTAB-L     PIC S9(04)   COMP.
               10  UPM4-DTSECTAB-A     PIC  X(01).
               10  UPM4-DTSECTAB-C     PIC  X(01).
               10  UPM4-DTSECTAB-H     PIC  X(01).
               10  UPM4-DTSECTAB-X     PIC  X(01).
           05  FILLER                  PIC  X(12).
           05  FILLER                  PIC  X(83).

      /*****************************************************************
      *                                                                *
      *    PROCEDURE DIVISION                                          *
      *                                                                *
      ******************************************************************
       PROCEDURE DIVISION.

      ******************************************************************
      *    PROGRAM INITIALIZATION                                      *
      ******************************************************************
       A00-VUPM-PROGRAM-ENTRY.

           COPY COMMENTR.

           EXEC CICS LINK
                     PROGRAM  (CICSINFO)
                     COMMAREA (CICSINFO-PARMS)
           END-EXEC.
           MOVE CICS-FULLDATE          TO WS-WRKDATE.
           MOVE CICS-FULLTIME          TO WS-WRKTIME.
           PERFORM Q02-REFORMAT-TIME THRU Q05-EXIT.

           COPY COMMMAPT.

           COPY COMM3270.

           SET  DAPL-LOGGING           TO TRUE.

           EXEC CICS HANDLE CONDITION
                     DISABLED (Z70-VUPM-CLOSED)
                     NOTOPEN  (Z70-VUPM-CLOSED)
           END-EXEC.

           IF  COMM-CURRPGM = THIS-PGM
      * psuedo-conversational program continuation
               GO TO B50-VUPM-ROUTINE-SELECTION
           END-IF.

       B00-VUPM-FIRST-TIME-ONLY.

           COPY COMMINIT.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
      * return from 2nd-level program (such as the HELP system)
               GO TO B50-VUPM-ROUTINE-SELECTION
           END-IF.

      * the following executes one-time-only per user session

           MOVE CICS-USERID            TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           PERFORM Q26-READEQ-IESCNTL.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE 1                  TO UNEX-MSGL
               STRING 'Sorry, not authorized to application.'
                   DELIMITED BY SIZE INTO UNEX-MSG POINTER UNEX-MSGL
               MOVE UNEX-IC            TO UNEX-MSG(UNEX-MSGL:1)
               GO TO X50-SEND-MSG
           END-IF.

           MOVE 'D'                    TO BIT-REQU
           MOVE IUI-US-IESINFO         TO BIT-BYTE
           CALL 'BITMAN'            USING BITMAN-PARMS
/DLC1/*    IF  BIT-0-IS-OFF
/DLC1/*    AND BIT-1-IS-OFF
/DLC1/*        MOVE 1                  TO UNEX-MSGL
/DLC1/*        STRING 'Sorry, not authorized to application.'
/DLC1/*            DELIMITED BY SIZE INTO UNEX-MSG POINTER UNEX-MSGL
/DLC1/*        MOVE UNEX-IC            TO UNEX-MSG(UNEX-MSGL:1)
/DLC1/*        GO TO X50-SEND-MSG
/DLC1/*    END-IF.
           IF  BIT-0-IS-ON
           AND BIT-1-IS-ON
               SET  USER-IS-ADMIN      TO TRUE
           ELSE
               SET  USER-IS-SUPER      TO TRUE
           END-IF.
/DLC1/     IF  BIT-0-IS-OFF
/DLC1/     AND BIT-1-IS-OFF
/DLC1/         SET  USER-IS-GENERAL    TO TRUE
/DLC1/     END-IF.
           MOVE LOW-VALUES             TO VUPM-CNTL-KEY.

      * continue first time initialization

           IF  COMM-CURRRTN NOT = 'RETN'
                              AND 'EXIT'
               MOVE SPACES             TO VUPM-SAVEKEY
           END-IF.

      * set default initial filter value
           MOVE '*'                    TO SAVE-USERNAME-FILTER
/DLC2/                                    SAVE-OPERID-FILTER
                                          SAVE-EXPIRES-FILTER
                                          SAVE-EXPIRED-FILTER
                                          SAVE-REVOKED-FILTER
                                          SAVE-SIGNON-FILTER
                                          SAVE-USERTYPE-FILTER
                                          SAVE-INITIAL-FILTER
                                          SAVE-NAMETYPE-FILTER
/DLC2/                                    SAVE-PRIME-CO-FILTER
/DLC2/                                    SAVE-PRIME-IND-FILTER
/DLC2/                                    SAVE-WIN-UNIT-FILTER
/DLC2/                                    SAVE-WIN-POS-FILTER
/DLC2/                                    SAVE-WIN-ORG-FILTER
/DLC2/                                    SAVE-WIN-FUNC-FILTER
/DLC2/                                    SAVE-WIN-DIR-FILTER.

      * this program requires mixed-case terminal input
           EXEC CICS SET TERMINAL(EIBTRMID)
                     TRANIDONLY
                     NOHANDLE
           END-EXEC.

       B50-VUPM-ROUTINE-SELECTION.

           IF  COMM-CURRRTN = 'RETN'
           AND COMM-SAVE-FUNC > SPACES
               MOVE COMM-CURRRTN       TO INIT-FLAG
               MOVE COMM-SAVE-FUNC     TO COMM-CURRRTN
               MOVE INIT-FLAG          TO COMM-SAVE-FUNC
           END-IF.

           EVALUATE COMM-CURRRTN
/DLC4/     WHEN 'TEST' GO TO K00-TEST-ROUTINE
           WHEN 'LDAP' GO TO I00-LDAP-ROUTINE
           WHEN 'DOCU' GO TO H00-DOCU-ROUTINE
           WHEN 'UPM4' GO TO G00-UPM4-ROUTINE
           WHEN 'UPM3' GO TO F00-UPM3-ROUTINE
           WHEN 'UPM2' GO TO E00-UPM2-ROUTINE
           WHEN 'UPM1' GO TO D00-UPM1-ROUTINE
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
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-LIST-TIOA TO LISTMAPO
                   MOVE LOW-VALUES     TO VUPM-LIST-TIOA
               END-IF
               PERFORM C80-LIST-OUTPUT THRU C85-LIST-SENDMAP
               GO TO C50-LIST-DISPLAY
           END-IF.

       C10-LIST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LISTMAP)
                     INTO     (LISTMAPI)
                     MAPSET   (VUPMMS)
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
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK01
               PERFORM P20-DROP-LIST-OPTIONS THRU P25-EXIT
               MOVE LISTMAPI           TO VUPM-LIST-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDENTER
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
               CONTINUE

           WHEN AIDPFK10
               GO TO H00-DOCU-ROUTINE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO LIST-MESSAGE-O
               GO TO C80-LIST-OUTPUT
           END-EVALUATE.

       C30-LIST-PROCESS.

      * convert cursor position to row and column coordinates
           DIVIDE EIBCPOSN BY SCRNWDTH GIVING ROW REMAINDER COL.
           ADD  1                      TO ROW COL.

           INSPECT LIST-USERID-SKIPTO-I CONVERTING LOWER-CASE
                                                TO UPPER-CASE
           IF  LIST-USERID-SKIPTO-L    > ZERO
           OR  LIST-USERID-SKIPTO-F    = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-OPERID-FILTER-L    > ZERO
/DLC2/     OR  LIST-OPERID-FILTER-F    = FLDMODFY OR FLDCURSM
           OR  LIST-USERNAME-FILTER-L  > ZERO
           OR  LIST-USERNAME-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-EXPIRES-FILTER-L   > ZERO
           OR  LIST-EXPIRES-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-EXPIRED-FILTER-L   > ZERO
           OR  LIST-EXPIRED-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-REVOKED-FILTER-L   > ZERO
           OR  LIST-REVOKED-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-SIGNON-FILTER-L    > ZERO
           OR  LIST-SIGNON-FILTER-F    = FLDMODFY OR FLDCURSM
           OR  LIST-USERTYPE-FILTER-L  > ZERO
           OR  LIST-USERTYPE-FILTER-F  = FLDMODFY OR FLDCURSM
           OR  LIST-INITIAL-FILTER-L   > ZERO
           OR  LIST-INITIAL-FILTER-F   = FLDMODFY OR FLDCURSM
           OR  LIST-NAMETYPE-FILTER-L  > ZERO
           OR  LIST-NAMETYPE-FILTER-F  = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-PRIME-CO-FILTER-L  > ZERO
/DLC2/     OR  LIST-PRIME-CO-FILTER-F  = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-PRIME-IND-FILTER-L > ZERO
/DLC2/     OR  LIST-PRIME-IND-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-WIN-UNIT-FILTER-L  > ZERO
/DLC2/     OR  LIST-WIN-UNIT-FILTER-F  = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-WIN-POS-FILTER-L   > ZERO
/DLC2/     OR  LIST-WIN-POS-FILTER-F   = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-WIN-ORG-FILTER-L   > ZERO
/DLC2/     OR  LIST-WIN-ORG-FILTER-F   = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-WIN-FUNC-FILTER-L  > ZERO
/DLC2/     OR  LIST-WIN-FUNC-FILTER-F  = FLDMODFY OR FLDCURSM
/DLC2/     OR  LIST-WIN-DIR-FILTER-L   > ZERO
/DLC2/     OR  LIST-WIN-DIR-FILTER-F   = FLDMODFY OR FLDCURSM
               GO TO C50-LIST-DISPLAY
           END-IF.

      * scroll to top or bottom of the list
           IF  EIBAID = AIDPFK06 OR AIDPFK09
               PERFORM P20-DROP-LIST-OPTIONS THRU P25-EXIT
               IF  EIBAID = AIDPFK06
                   MOVE LOW-VALUES     TO VUPM-CNTL-KEY
               ELSE
                   MOVE HIGH-VALUES    TO VUPM-CNTL-KEY
               END-IF
               PERFORM Q20-INITKEY-IESCNTL
               IF  EIBAID = AIDPFK06
                   PERFORM Q21-STARTBR-IESCNTL
                   IF  EIBRESP = DFHRESP(NORMAL)
                       PERFORM Q23-READNEXT-IESCNTL THRU Q23-CHK-FILTER
                   END-IF
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  VUPM-CNTL-KEY = LIST-USERID-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first page.' TO LIST-MESSAGE-O
                   END-IF
               ELSE
                   PERFORM Q21-STARTBR-IESCNTL
                   IF  EIBRESP = DFHRESP(NORMAL)
                       PERFORM Q23-READNEXT-IESCNTL
                       PERFORM Q24-READPREV-IESCNTL
                       PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
                   END-IF
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  VUPM-CNTL-KEY = LIST-USERID-I(1)
                   OR  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last page.' TO LIST-MESSAGE-O
                   END-IF
               END-IF
               MOVE VUPM-CNTL-KEY      TO LIST-USERID-O(1)
               PERFORM Q25-ENDBR-IESCNTL
               GO TO C50-LIST-DISPLAY
           END-IF.

      * adjust row and column coordinates for header/detail screens
           SUBTRACT LISTHEAD         FROM ROW.

      * scroll to previous or next page
           IF  EIBAID = AIDPFK07 OR AIDPFK08
               PERFORM P20-DROP-LIST-OPTIONS THRU P25-EXIT
               MOVE LIST-USERID-I(1)   TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               IF  EIBAID = AIDPFK07
                   IF  ROW < 1 OR >= LISTROWS
                   OR  LIST-USERID-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = LISTROWS - ROW
                   END-IF
                   PERFORM Q21-STARTBR-IESCNTL
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-IESCNTL
                   PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.' TO LIST-MESSAGE-O
                       PERFORM Q23-READNEXT-IESCNTL THRU Q23-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               ELSE
                   IF  ROW <= 1 OR > LISTROWS
                   OR  LIST-USERID-I(ROW) <= SPACES
                       COMPUTE IDX = LISTROWS
                   ELSE
                       COMPUTE IDX = ROW - 1
                   END-IF
                   PERFORM Q21-STARTBR-IESCNTL
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   PERFORM Q23-READNEXT-IESCNTL THRU Q23-CHK-FILTER
                     VARYING IDX FROM IDX BY -1
                       UNTIL IDX < ZERO
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                                 AND DFHRESP(ENDFILE)
                       GO TO C70-LIST-ERRORS
                   END-IF
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the last entry.' TO LIST-MESSAGE-O
                       PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
                       IF  EIBRESP NOT = DFHRESP(NORMAL)
                                     AND DFHRESP(ENDFILE)
                           GO TO C70-LIST-ERRORS
                       END-IF
                   END-IF
               END-IF
               MOVE VUPM-CNTL-KEY      TO LIST-USERID-O(1)
               PERFORM Q25-ENDBR-IESCNTL
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
                   MOVE 'V'            TO LIST-SELECT-O(ROW)
               END-IF
           END-IF.

      * PERFORM DETAIL FIELD EDITING HERE

           SET  NO-UPDATES-FOUND       TO TRUE.
           MOVE DFHRESP(NORMAL)        TO EIBRESP.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > LISTROWS

               INSPECT LIST-SELECT-I(IDX) CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               IF  LIST-SELECT-I(IDX) > SPACE
/DLC1/             EVALUATE TRUE
/DLC1/             WHEN NOT USER-IS-GENERAL
/DLC1/             AND  LIST-SELECT-I(IDX) NOT = 'U' AND 'V' AND '*'
/DLC2/                                                       AND 'P'
/DLC5/                                                       AND 'Q'
/DLC4/                                                       AND 'T'
                       SET UPDATES-PROCESSED TO TRUE
                       PERFORM C40-LIST-UPDATES THRU C45-EXIT

/DLC1/             WHEN USER-IS-GENERAL
/DLC1/             AND  LIST-SELECT-I(IDX) NOT = 'U' AND 'V' AND '*'
/DLC2/                                                       AND 'P'
/DLC5/                                                       AND 'Q'
/DLC4/                                                       AND 'T'
/DLC1/                 MOVE LOW-VALUES TO LIST-SELECT-I(IDX)

/DLC1/             END-EVALUATE
               END-IF

      * end of detail loop
           END-PERFORM.

           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO C70-LIST-ERRORS
           END-IF.

           IF  UPDATES-PROCESSED
               EXEC CICS LINK PROGRAM(VSECREFR) END-EXEC
           END-IF.

           GO TO C50-LIST-DISPLAY.

       C40-LIST-UPDATES.

      * PROCESS ANY ALLOWED UPDATES HERE

           EVALUATE LIST-SELECT-I(IDX)
/DLC1/*    WHEN 'U'
/DLC1/*    WHEN 'V'
/DLC1/*    WHEN '*'
/DLC1/*        GO TO C45-EXIT

           WHEN 'A'
           WHEN 'C'
               MOVE LIST-USERID-I(IDX) TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
               MOVE 'D'                TO BIT-REQU
               MOVE IUI-US-IESVSAM     TO BIT-BYTE
               CALL 'BITMAN'        USING BITMAN-PARMS
               IF  BIT-4-IS-OFF
                   MOVE 'Cannot add or change ICCF users via this interf
      -                 'ace.'         TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
               GO TO C45-EXIT

           WHEN 'D'
               MOVE LIST-USERID-I(IDX) TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
               MOVE 'D'                TO BIT-REQU
               MOVE IUI-US-IESVSAM     TO BIT-BYTE
               CALL 'BITMAN'        USING BITMAN-PARMS
               IF  BIT-4-IS-OFF
                   MOVE 'Cannot delete ICCF users via this interface.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
               COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 1
               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
               MOVE SPACES             TO MULTI-EDIT-MSG
               MOVE 1                  TO LEN
               STRING 'Delete this userid and its '
                      'associated permissions?  '
                      'Press F2 to confirm.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               PERFORM V10-VERIFY-DELETE THRU V15-EXIT
               IF  EIBAID  NOT = AIDPFK02
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
      * delete persmissions
               MOVE LOW-VALUES             TO VSECSERV-COMMAREA
               SET  VSECSERV-TRAN          TO TRUE
               SET  VSECSERV-PRG-PERMISSIONS
                                           TO TRUE
               SET  VSECUPD-PER-USER       TO TRUE
               MOVE VUPM-CNTL-KEY          TO VSECUPD-PER-NAME
               EXEC CICS LINK
                         PROGRAM(VSECSERV)
                         COMMAREA(VSECSERV-COMMAREA)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                 GO TO C45-EXIT
               END-IF
               IF  NOT VSECSERV-RTN-OKAY
                 STRING VSECSERV-RTN-MESG '  rc='
                        VSECSERV-RTN-CODE DELIMITED BY SIZE
                                     INTO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
/DLC0/* delete VUSERID record
/DLC0/         PERFORM Q60-INITKEY-VUSERID
/DLC0/         MOVE VUPM-CNTL-KEY      TO VUSER-KEY
/DLC0/         PERFORM Q66-READEQ-VUSERID
/DLC0/         IF  EIBRESP = DFHRESP(NORMAL)
/DLC0/             PERFORM Q66-READUP-VUSERID
/DLC0/             IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC0/                 GO TO C45-EXIT
/DLC0/             END-IF
/DLC0/             PERFORM Q68-DELETE-VUSERID
/DLC0/         END-IF
/DLC0/         IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC0/                       AND DFHRESP(NOTFND)
/DLC0/             GO TO C45-EXIT
/DLC0/         END-IF
      * delete VOPERID record
/DLC3/*        PERFORM Q50-INITKEY-VOPERID
/DLC3/*        MOVE VUPM-CNTL-KEY      TO VOPERID-USERID
/DLC3/*        PERFORM Q56-READEQ-VOPERPU
/DLC3/*        IF  EIBRESP = DFHRESP(NORMAL)
/DLC3/*            PERFORM Q56-READUP-VOPERID
/DLC3/*            IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC3/*                GO TO C45-EXIT
/DLC3/*            END-IF
/DLC3/*            PERFORM Q58-DELETE-VOPERID
/DLC3/*        END-IF
/DLC3/*        IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC3/*                      AND DFHRESP(NOTFND)
/DLC3/*            GO TO C45-EXIT
/DLC3/*        END-IF
      * delete LDAP mapping
               PERFORM Q40-INITKEY-IESLDUM
               MOVE VUPM-CNTL-KEY      TO LDUM-MFUSRID
               PERFORM Q46-READEQ-IESLDUV
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE VUPM-CNTL-KEY  TO LDUM-NETUSRID
                   PERFORM Q46-READEQ-IESLDUM
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q46-READUP-IESLDUM
                   IF  EIBRESP NOT = DFHRESP(NORMAL)
                       GO TO C45-EXIT
                   END-IF
                   PERFORM Q48-DELETE-IESLDUM
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                             AND DFHRESP(NOTFND)
                             AND DFHRESP(FILENOTFOUND)
                   GO TO C45-EXIT
               END-IF
      * delete user profile
               PERFORM Q26-READUP-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
               PERFORM Q28-DELETE-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF

           WHEN 'S'
           WHEN 'R'
               MOVE LIST-USERID-I(IDX) TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C45-EXIT
               END-IF
               PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
               IF  LIST-EXPIRED = '*'
               OR  UPM1-REVOKED > SPACES
                   MOVE 'Profile is expired or revoked.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
               EXEC CICS LOAD
                         PROGRAM(IESLDCFG)
                         NOHANDLE
               END-EXEC
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE 'LDAP is not enabled at this time.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO C45-EXIT
               END-IF
               PERFORM Q40-INITKEY-IESLDUM
               MOVE VUPM-CNTL-KEY      TO LDUM-MFUSRID
               PERFORM Q46-READEQ-IESLDUV
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE VUPM-CNTL-KEY  TO LDUM-NETUSRID
                   PERFORM Q46-READEQ-IESLDUM
               END-IF
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 IF  LIST-SELECT-I(IDX) = 'S'
                   MOVE 'LDAP mapping already exists.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                 END-IF
               WHEN DFHRESP(NOTFND)
                 IF  LIST-SELECT-I(IDX) = 'R'
                   MOVE 'LDAP mapping does not exist.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
                 END-IF
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
               END-EVALUATE
               IF  LIST-SELECT-I(IDX) = 'S'
               OR  EIBRESP NOT = DFHRESP(NORMAL)
               OR  ERRORS-FOUND
                   GO TO C45-EXIT
               END-IF
               COMPUTE EIBCPOSN = (LISTHEAD + IDX - 1) * SCRNWDTH + 1
               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
               MOVE SPACES             TO MULTI-EDIT-MSG
               MOVE 1                  TO LEN
               STRING 'Remove this userid from '
                      'LDAP mapping?  '
                      'Press F2 to confirm.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER LEN
               SUBTRACT 1            FROM LEN
               PERFORM V10-VERIFY-DELETE THRU V15-EXIT
               IF  EIBAID  NOT = AIDPFK02
               OR  EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE LOW-VALUES     TO LIST-SELECT-I(IDX)
                   GO TO C45-EXIT
               END-IF
               PERFORM P30-RESET-LDAP-MAPPING THRU P35-EXIT
               IF  ERRORS-FOUND
                   GO TO C45-EXIT
               END-IF

           WHEN OTHER
               MOVE 'Unknown option selection.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO C45-EXIT
           END-EVALUATE.

           IF  EIBRESP = DFHRESP(NORMAL)
           AND LIST-SELECT-I(IDX) > SPACE
               MOVE '*'                TO LIST-SELECT-O(IDX)
           END-IF.

       C45-EXIT.
           EXIT.

       C50-LIST-DISPLAY.

           MOVE 'LIST'                 TO COMM-CURRRTN.

      * skip if heading selections changed or no detail lines
           IF  LIST-USERID-SKIPTO-L > ZERO
           OR  LIST-USERID-SKIPTO-F = FLDMODFY OR FLDCURSM
           OR  LIST-USERID-I(1) NOT > SPACES
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
      * ... transfer key data to internal function ...
               WHEN 'V'
               WHEN 'A'
               WHEN 'C'
                   IF  LIST-USERID-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE LIST-USERID-I(ROW)
                                       TO VUPM-SAVEKEY
                   MOVE LIST-SELECT-I(ROW)
                                       TO VUPM-PROCESS
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO VUPM-LIST-TIOA
                   GO TO D00-UPM1-ROUTINE
/DLC2/* ... drill-down with key data into 2nd-level trans/pgm ...
/DLC2/         WHEN 'P'
/DLC2/             IF  LIST-USERID-I(ROW) NOT > SPACES
/DLC2/                 MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
/DLC2/                 GO TO C60-LIST-DETAIL
/DLC2/             END-IF
/DLC2/             MOVE 'EXIT'         TO COMM-TRANSID
/DLC2/             MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC
/DLC2/             MOVE LIST-USERID-I(ROW)
/DLC2/                                 TO VUPM-SAVEKEY
/DLC2/             MOVE VSECPGM        TO LINK-PGM
/DLC2/             COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
/DLC2/                              + LENGTH OF COMM-SHARED
/DLC2/                              + LENGTH OF VUPM-SAVEKEY
/DLC2/             MOVE '*'            TO LIST-SELECT-O(ROW)
/DLC2/             MOVE LISTMAPI       TO VUPM-LIST-TIOA
/DLC2/             GO TO T00-LINK-TRANSFER
/DLC5/* ... transfer key data to internal function ...
/DLC5/         WHEN 'Q'
/DLC5/             IF  LIST-USERID-I(ROW) NOT > SPACES
/DLC5/               MOVE LOW-VALUES   TO LIST-SELECT-O(ROW)
/DLC5/               GO TO C60-LIST-DETAIL
/DLC5/             END-IF
/DLC5/             MOVE LIST-USERID-I(ROW)
/DLC5/                                 TO VUPM-CNTL-KEY
/DLC5/             PERFORM Q40-INITKEY-IESLDUM
/DLC5/             MOVE VUPM-CNTL-KEY  TO LDUM-MFUSRID
/DLC5/             PERFORM Q46-READEQ-IESLDUV
/DLC5/             IF  EIBRESP = DFHRESP(NOTFND)
/DLC5/               MOVE 'The user is not mapped.'
/DLC5/                                 TO MULTI-EDIT-MSG
/DLC5/               CALL MULTMESG  USING DFHEIBLK MULTMESG
/DLC5/                                    ERROR-SWITCH
/DLC5/                                    LIST-SELECT-L(ROW)
/DLC5/                                    LIST-MESSAGE-L
/DLC5/                                    MULTI-EDIT-MSG
/DLC5/               GO TO C80-LIST-OUTPUT
/DLC5/             END-IF
/DLC5/             PERFORM Q60-INITKEY-VUSERID
/DLC5/             MOVE VUPM-CNTL-KEY  TO VUSER-KEY
/DLC5/             PERFORM Q66-READEQ-VUSERID
/DLC5/             IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC5/               GO TO C70-LIST-ERRORS
/DLC5/             END-IF
/DLC5/             IF  NOT VUSER-DO-REFRESH
/DLC5/               COMPUTE EIBCPOSN = (LISTHEAD + ROW - 1)
/DLC5/                                * SCRNWDTH + 1
/DLC5/               EXEC CICS SEND CONTROL CURSOR(EIBCPOSN) END-EXEC
/DLC5/               MOVE SPACES       TO MULTI-EDIT-MSG
/DLC5/               MOVE 1            TO LEN
/DLC5/               STRING 'User id not flagged for refresh.  '
/DLC5/                      'Press F2 to refresh.' DELIMITED BY SIZE
/DLC5/                   INTO MULTI-EDIT-MSG WITH POINTER LEN
/DLC5/               SUBTRACT 1      FROM LEN
/DLC5/               PERFORM V10-VERIFY-DELETE THRU V15-EXIT
/DLC5/               IF  EIBAID  NOT = AIDPFK02
/DLC5/               OR  EIBRESP NOT = DFHRESP(NORMAL)
/DLC5/                   MOVE LOW-VALUES TO LIST-SELECT-I(ROW)
/DLC5/                   GO TO C50-LIST-DISPLAY
/DLC5/               END-IF
/DLC5/             END-IF
/DLC5/             MOVE VUPM-CNTL-KEY  TO VUPM-SAVEKEY
/DLC5/             MOVE '*'            TO LIST-SELECT-O(ROW)
/DLC5/             MOVE LISTMAPI       TO VUPM-LIST-TIOA
/DLC5/             GO TO O00-QUERY-ROUTINE
      * ... transfer key data to internal function ...
               WHEN 'S'
                   IF  LIST-USERID-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE LIST-USERID-I(ROW)
                                       TO VUPM-SAVEKEY
                   MOVE COMM-CURRRTN   TO LDAP-PREVRTN
                   MOVE LIST-SELECT-I(ROW)
                                       TO VUPM-PROCESS
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO VUPM-LIST-TIOA
                   GO TO I00-LDAP-ROUTINE
/DLC4/* ... transfer key data to internal function ...
/DLC4/         WHEN 'T'
/DLC4/             IF  LIST-USERID-I(ROW) NOT > SPACES
/DLC4/                 MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
/DLC4/                 GO TO C60-LIST-DETAIL
/DLC4/             END-IF
/DLC4/             MOVE LIST-USERID-I(ROW)
/DLC4/                                 TO VUPM-SAVEKEY
/DLC4/             MOVE '*'            TO LIST-SELECT-O(ROW)
/DLC4/             MOVE LISTMAPI       TO VUPM-LIST-TIOA
/DLC4/             GO TO K00-TEST-ROUTINE
      * ... drill-down with key data into 2nd-level trans/pgm ...
               WHEN 'U'
                   IF  LIST-USERID-I(ROW) NOT > SPACES
                       MOVE LOW-VALUES TO LIST-SELECT-O(ROW)
                       GO TO C60-LIST-DETAIL
                   END-IF
                   MOVE 'EXIT'         TO COMM-TRANSID
                   MOVE COMM-CURRRTN   TO COMM-SAVE-FUNC
                   MOVE LIST-USERID-I(ROW)
                                       TO VUPM-SAVEKEY
                   COMPUTE LINK-LEN = LENGTH OF COMM-HEADER
                                    + LENGTH OF COMM-SHARED
                                    + LENGTH OF VUPM-SAVEKEY
                   MOVE '*'            TO LIST-SELECT-O(ROW)
                   MOVE LISTMAPI       TO VUPM-LIST-TIOA
                   GO TO T00-LINK-TRANSFER
      *
               WHEN OTHER
                 MOVE 'Unknown option selection.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(ROW)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
               END-EVALUATE
               GO TO C80-LIST-OUTPUT
           END-IF.

       C60-LIST-DETAIL.

           PERFORM Q80-SAVE-FILTERS THRU Q85-EXIT.
           IF  ERRORS-FOUND
               GO TO C80-LIST-OUTPUT
           END-IF.

           EVALUATE TRUE
           WHEN LIST-USERID-SKIPTO-L > ZERO
            AND LIST-USERID-SKIPTO-I > SPACES
               MOVE LIST-USERID-SKIPTO-I
                                       TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q21-STARTBR-IESCNTL
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE HIGH-VALUES    TO VUPM-CNTL-KEY
                   PERFORM Q20-INITKEY-IESCNTL
                   PERFORM Q21-STARTBR-IESCNTL
                   PERFORM Q23-READNEXT-IESCNTL
                   PERFORM Q24-READPREV-IESCNTL
                   PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q23-READNEXT-IESCNTL THRU Q23-CHK-FILTER
               END-IF
               IF  EIBRESP = DFHRESP(ENDFILE)
                   PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
               END-IF
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO C70-LIST-ERRORS
               END-IF
               IF  LIST-USERID-SKIPTO-I < VUPM-CNTL-KEY
                   PERFORM Q24-READPREV-IESCNTL THRU Q24-CHK-FILTER
                     VARYING IDX FROM 1 BY 1 UNTIL IDX > 2
                          OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                       MOVE 'This is the first entry.'
                                       TO LIST-MESSAGE-O
                   END-IF
               END-IF
               PERFORM Q25-ENDBR-IESCNTL

           WHEN VUPM-SAVEKEY > SPACES
               MOVE VUPM-SAVEKEY       TO VUPM-CNTL-KEY

           WHEN LIST-USERID-I(1) > SPACES
               MOVE LIST-USERID-I(1)   TO VUPM-CNTL-KEY
           END-EVALUATE.

           PERFORM Q20-INITKEY-IESCNTL.
           PERFORM Q21-STARTBR-IESCNTL.
           IF  EIBRESP = DFHRESP(NOTFND)
               MOVE LOW-VALUES         TO VUPM-CNTL-KEY
               PERFORM Q21-STARTBR-IESCNTL
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE 1              TO IDX
                   GO TO C65-LIST-BLANKS
               END-IF
           END-IF.

           MOVE SPACES                 TO VUPM-SAVEKEY.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1
               UNTIL IDX > LISTROWS
                  OR EIBRESP NOT = DFHRESP(NORMAL)

               PERFORM Q23-READNEXT-IESCNTL THRU Q23-CHK-FILTER

               IF  EIBRESP = DFHRESP(NORMAL)

      * BUILD DETAIL LINE OUTPUT HERE
                   MOVE LOW-VALUES     TO LISTMAP-ROW-DETAIL(IDX)
                   MOVE UPM1-USRIDNT   TO LIST-USERID-O(IDX)
/DLC2/             MOVE UPM1-OPRIDNT   TO LIST-OPERID-O(IDX)
                   MOVE UPM1-USRNAME   TO LIST-USERNAME-O(IDX)
                   MOVE LIST-EXPIRES   TO LIST-EXPIRES-O(IDX)
                   MOVE LIST-EXPIRED   TO LIST-EXPIRED-O(IDX)
                   MOVE UPM1-REVOKED   TO LIST-REVOKED-O(IDX)
                   MOVE LIST-SIGNON    TO LIST-SIGNON-O(IDX)
                   MOVE UPM1-USRTYPE   TO LIST-USERTYPE-O(IDX)
                   MOVE UPM1-INITNAM   TO LIST-INITIAL-NAME-O(IDX)
                   MOVE UPM1-INITYPE   TO LIST-NAMETYPE-O(IDX)

/DLC2/             MOVE VUSER-PRIMARY-CO   TO LIST-PRIMARY-CO-O(IDX)
/DLC2/             MOVE VUSER-PRIMARY-IND  TO LIST-PRIMARY-IND-O(IDX)
/DLC2/             MOVE VUSER-WIN-FUNCTION TO LIST-WIN-FUNCTION-O(IDX)
/DLC2/             MOVE VUSER-WIN-UNIT     TO LIST-WIN-UNIT-O(IDX)
/DLC2/             MOVE VUSER-WIN-ORG      TO LIST-WIN-ORG-O(IDX)
/DLC2/             MOVE VUSER-WIN-POSITION TO LIST-WIN-POSITION-O(IDX)
/DLC2/             MOVE VUSER-WIN-DIRECTOR TO LIST-WIN-DIRECTOR-O(IDX)
/DLC2/             PERFORM R20-CHECK-AD-ATTRIBUTES THRU R25-EXIT

               ELSE
                   MOVE  -1            TO LIST-SELECT-L(IDX)
                   SUBTRACT 1        FROM IDX
               END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ENDFILE)
               PERFORM C65-LIST-BLANKS
               GO TO C70-LIST-ERRORS
           END-IF.

           PERFORM Q25-ENDBR-IESCNTL.

       C65-LIST-BLANKS.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM IDX BY 1
               UNTIL IDX > LISTROWS
             MOVE LOW-VALUES           TO LISTMAP-ROW-DETAIL(IDX)
             MOVE FLDASDRK             TO LIST-SELECT-A(IDX)
           END-PERFORM.

       C70-LIST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LIST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO LIST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       C80-LIST-OUTPUT.

           MOVE MAPTITLE               TO LIST-COMPANY-O.
           MOVE WS-WRKDATE             TO LIST-SYSDATE-O.
           MOVE WS-WRKTIME             TO LIST-SYSTIME-O.
           MOVE SPACES                 TO LIST-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(LIST-NETNAME-O) END-EXEC.
           IF  LIST-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO LIST-NETNAME-O
           END-IF.

       C85-LIST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (LISTMAP)
                             DATAONLY
                             FROM     (LISTMAPO)
                             MAPSET   (VUPMMS)
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
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (LISTMAP)
                         FROM     (LISTMAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
               IF  USER-IS-GENERAL
                 MOVE 1                TO PTR
      *                               row  adjust            col  adjust
                 COMPUTE HALF-WORD = (023 - 1) * SCRNWDTH + (017 - 1)
                 STRING SCRSETBA TWO-BYTES SCRSTFLD FLDASDRK
                   DELIMITED BY SIZE INTO VAR-DATA WITH POINTER PTR
      *                               row  adjust            col  adjust
                 COMPUTE HALF-WORD = (023 - 1) * SCRNWDTH + (047 - 1)
                 STRING SCREPEAT TWO-BYTES X'40' SCRSTFLD FLDASKIP
                   DELIMITED BY SIZE INTO VAR-DATA WITH POINTER PTR
                 SUBTRACT 1          FROM PTR
                 EXEC CICS SEND
                           FROM(VAR-DATA)
                           LENGTH(PTR)
                           CTLCHAR(WRTFKFST)
                           NOHANDLE
                 END-EXEC
               END-IF
           END-IF.

       C90-LIST-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LISTMAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    UPM PAGE 1 ROUTINE                                          *
      ******************************************************************
       D00-UPM1-ROUTINE.

           IF  ADDRESS OF UPM1MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF UPM1MAPI)
                         LENGTH   (LENGTH OF UPM1MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF UPM1MAPX TO ADDRESS OF UPM1MAPI
           END-IF.

           IF  COMM-CURRRTN NOT = 'UPM1'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO UPM1MAPO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM D80-UPM1-OUTPUT THRU D85-UPM1-SENDMAP
               GO TO D50-UPM1-DISPLAY
           END-IF.

       D10-UPM1-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (UPM1MAP)
                     INTO     (UPM1MAPI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO D70-UPM1-ERRORS
           END-IF.

       D20-UPM1-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE UPM1MAPI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDPFK05
           WHEN AIDPFK07
           WHEN AIDPFK08
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM1-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM1-MESSAGE-O
               GO TO D80-UPM1-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM1-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO UPM1-MESSAGE-O
               GO TO D80-UPM1-OUTPUT
           END-EVALUATE.

       D30-UPM1-PROCESS.

           IF  EIBAID = AIDPFK05
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM1-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM1-MESSAGE-O
               GO TO D80-UPM1-OUTPUT
           END-IF.

           INSPECT UPM1-USERID-I CONVERTING LOWER-CASE
                                         TO UPPER-CASE.
/DLC2/     INSPECT UPM1-OPERID-I CONVERTING LOWER-CASE
/DLC2/                                   TO UPPER-CASE.
           INSPECT UPM1-PASSWORD-I CONVERTING LOWER-CASE
                                           TO UPPER-CASE.
           INSPECT UPM1-INITIAL-NAME-I CONVERTING LOWER-CASE
                                               TO UPPER-CASE.
           INSPECT UPM1-MODEL-I CONVERTING LOWER-CASE
                                        TO UPPER-CASE.

           IF  NOT VIEW-PROCESS

           IF  ADD-PROCESS AND
              (UPM1-USERID-L       > ZERO
           OR  UPM1-USERID-F       = FLDMODFY OR FLDCURSM)
             IF  UPM1-USERID-I NOT > SPACES
             AND NOT ADD-PROCESS
               MOVE UPM1-USRIDNT       TO UPM1-USERID-O
             END-IF
             IF  UPM1-USERID-I NOT > SPACES
               MOVE 'User id is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERID-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE ' '                TO TXT-DLM1
               MOVE LENGTH OF UPM1-USERID-I
                                       TO TXT-PARM-STRLEN
               MOVE UPM1-USERID-I      TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               MOVE TXT-PARM-STRING    TO UPM1-USERID-O
               IF  TXT-PARM-STRLEN < 4
               OR  TXT-PARM-STRING NOT ALPHAMERIC-NATIONAL
                 MOVE 'User id must be 4 to 8 alpha-numeric and national
      -               ' characters.'   TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERID-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 IF  UPM1-USERID-I > SPACES
                 AND ADD-PROCESS
                   MOVE UPM1-USERID-I  TO VUPM-CNTL-KEY
                   PERFORM Q20-INITKEY-IESCNTL
                   PERFORM Q26-READEQ-IESCNTL
                   EVALUATE EIBRESP
                   WHEN DFHRESP(NOTFND)
                     MOVE DFHRESP(NORMAL)
                                       TO EIBRESP
                   WHEN DFHRESP(NORMAL)
                     MOVE 'User id already exists.'
                                       TO MULTI-EDIT-MSG
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERID-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
                   WHEN OTHER
                     GO TO D70-UPM1-ERRORS
                   END-EVALUATE
                 END-IF
               END-IF
               MOVE UPM1-USERID-I      TO UPM1-USRIDNT
             END-IF
           END-IF

           IF  UPM1-USERNAME-L     > ZERO
           OR  UPM1-USERNAME-F     = FLDMODFY OR FLDCURSM
             IF  UPM1-USERNAME-I NOT > SPACES
               MOVE UPM1-USRNAME       TO UPM1-USERNAME-O
             END-IF
             IF  UPM1-USERNAME-I NOT > SPACES
               MOVE 'User name is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERNAME-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
             MOVE UPM1-USERNAME-I      TO UPM1-USRNAME
           END-IF

           IF  UPM1-USERTYPE-L     > ZERO
           OR  UPM1-USERTYPE-F     = FLDMODFY OR FLDCURSM
             IF  UPM1-USERTYPE-I NOT = '1' AND '2' AND '3'
               MOVE 'User type must be 1, 2, or 3.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERTYPE-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               IF  NOT USER-IS-ADMIN
               AND UPM1-USERTYPE-I = '1'
                 MOVE 'Not authorized to grant administrator authority.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-USERTYPE-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 MOVE UPM1-USERTYPE-I  TO UPM1-USRTYPE
               END-IF
             END-IF
           END-IF

/DLC2/     IF  UPM1-OPERID-L       > ZERO
/DLC2/     OR  UPM1-OPERID-F       = FLDMODFY OR FLDCURSM
/DLC2/         MOVE UPM1-OPERID-I      TO UPM1-OPRIDNT
/DLC2/         PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT
/DLC2/         EVALUATE TRUE
/DLC2/         WHEN MULTI-EDIT-MSG > SPACES
/DLC2/             CALL MULTMESG    USING DFHEIBLK MULTMESG
/DLC2/                                    ERROR-SWITCH
/DLC2/                                    UPM1-OPERID-L
/DLC2/                                    UPM1-MESSAGE-L
/DLC2/                                    MULTI-EDIT-MSG
/DLC2/         WHEN EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/             GO TO D70-UPM1-ERRORS
/DLC2/         END-EVALUATE
/DLC2/     END-IF

           IF  UPM1-PASSWORD-L     > ZERO
           OR  UPM1-PASSWORD-F     = FLDMODFY OR FLDCURSM
             IF  UPM1-PASSWORD-I NOT > SPACES
               MOVE UPM1-PASSWRD       TO UPM1-PASSWORD-O
             END-IF
             IF  UPM1-PASSWORD-I NOT > SPACES
               MOVE 'Password is required.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-PASSWORD-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE ' '                TO TXT-DLM1
               MOVE LENGTH OF UPM1-PASSWORD-I
                                       TO TXT-PARM-STRLEN
               MOVE UPM1-PASSWORD-I    TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               MOVE TXT-PARM-STRING    TO UPM1-PASSWORD-O
               IF  TXT-PARM-STRLEN < 4
               OR  TXT-PARM-STRING NOT ALPHAMERIC-NATIONAL
                 MOVE 'Password must be 4 to 8 alpha-numeric and nationa
      -               'l characters.'  TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-PASSWORD-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 MOVE UPM1-PASSWORD-I  TO UPM1-PASSWRD
               END-IF
             END-IF
           END-IF

           IF  UPM1-EXPIRES-L      > ZERO
           OR  UPM1-EXPIRES-F      = FLDMODFY OR FLDCURSM
             IF  UPM1-EXPIRES-I NOT > SPACES
               MOVE 'Expire days is required.  Specify 0 (zero) for pass
      -             'words that don''t expire.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-EXPIRES-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               SET NUM-REQUEST-NORMAL-EDIT TO TRUE
               MOVE 03                 TO NUM-TOTL
               MOVE 0                  TO NUM-DECM
               MOVE UPM1-EXPIRES-I     TO NUM-DATA
               CALL 'NUMMAN'        USING NUMMAN-PARMS
               EVALUATE TRUE
               WHEN NOT NUM-REQUEST-COMPLETED
                 STRING 'Expire days is not valid.'
                        '  rc=''' NUM-RETN ''''
                   DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-EXPIRES-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN NUM-T03-D0 < ZERO
               OR   NUM-T03-D0 > 365
                 MOVE 'Expire days value is out of range.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-EXPIRES-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 MOVE NUM-T03-D0       TO UPM1-EXPDAYS
                                          UPM1-EXPIRES-O
               END-EVALUATE
             END-IF
           END-IF

           IF  UPM1-REVOKE-L       > ZERO
           OR  UPM1-REVOKE-F       = FLDMODFY OR FLDCURSM
             IF  UPM1-REVOKE-I > SPACES
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE '/'                TO TXT-DLM1
               MOVE LENGTH OF UPM1-REVOKE-I
                                       TO TXT-PARM-STRLEN
               MOVE UPM1-REVOKE-I      TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               IF  TXT-PARM-STRLEN < 5 OR > 6
               OR  TXT-PARM-STRING NOT NUMERIC
                 MOVE 'Revoke date is not mmddyy or mm/dd/yy format.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-REVOKE-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 SET DTE-REQUEST-GREG-EDIT TO TRUE
                 MOVE ZEROES           TO DTE-GNUM
                 MOVE TXT-PARM-STRING(1:TXT-PARM-STRLEN)
                                       TO NUM-DATE6
                 MOVE CORR WRK-DATE6   TO DTE-GREG
                 MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                 CALL 'DTEMAN'      USING DTEMAN-PARMS
                 EVALUATE TRUE
                 WHEN NOT DTE-REQUEST-COMPLETED
                   STRING 'Revoke date is not valid.'
                          '  rc=''' DTE-RETN ''''
                     DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-REVOKE-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
                 WHEN OTHER
                   MOVE ALL '/'        TO EDT-DATE-X
                   MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
                   MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
                   MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
                   MOVE EDT-DATE-X     TO UPM1-REVOKED
                                          UPM1-REVOKE-O
                 END-EVALUATE
               END-IF
             ELSE
               MOVE SPACES             TO UPM1-REVOKED
             END-IF
           END-IF

           IF  UPM1-INITIAL-NAME-L > ZERO
           OR  UPM1-INITIAL-NAME-F = FLDMODFY OR FLDCURSM
             IF  UPM1-INITIAL-NAME-I NOT > SPACES
               EVALUATE UPM1-USRTYPE
               WHEN '1'
                 MOVE 'IESEADM '       TO UPM1-INITIAL-NAME-O
               WHEN '2'
                 MOVE 'IESEPROG'       TO UPM1-INITIAL-NAME-O
               WHEN OTHER
                 MOVE 'DFLESEL '       TO UPM1-INITIAL-NAME-O
               END-EVALUATE
             END-IF
             MOVE UPM1-INITIAL-NAME-I  TO UPM1-INITNAM
           END-IF

           IF  UPM1-NAME-TYPE-L    > ZERO
           OR  UPM1-NAME-TYPE-F    = FLDMODFY OR FLDCURSM
             IF  UPM1-NAME-TYPE-I NOT > SPACES
                 MOVE '2'              TO UPM1-NAME-TYPE-O
             END-IF
             IF  UPM1-NAME-TYPE-I NOT = '1' AND '2'
               MOVE 'Initial name type must be 1 or 2.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-NAME-TYPE-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               MOVE UPM1-NAME-TYPE-I   TO UPM1-INITYPE
             END-IF
           END-IF

           IF  UPM1-INITNAM NOT = 'DFLESEL' AND 'DUMMY'
               PERFORM Q20-INITKEY-IESCNTL
               IF  UPM1-INITYPE = '1'
                   SET  IUI-APPL-PROFILE   TO TRUE
               ELSE
                   SET  IUI-SELECTION-TEXT TO TRUE
               END-IF
               MOVE UPM1-INITNAM           TO IUI-KEY-SHORT
               PERFORM Q26-READEQ-IESCNTL
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 CONTINUE
               WHEN DFHRESP(NOTFND)
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
                 IF  UPM1-INITYPE = '1'
                   MOVE 'Initial name is not an application profile.'
                                           TO MULTI-EDIT-MSG
                 ELSE
                   MOVE 'Initial name is not a selection panel.'
                                           TO MULTI-EDIT-MSG
                 END-IF
                 CALL MULTMESG          USING DFHEIBLK MULTMESG
                                              ERROR-SWITCH
                                              UPM1-INITIAL-NAME-L
                                              UPM1-MESSAGE-L
                                              MULTI-EDIT-MSG
               WHEN OTHER
                 GO TO D70-UPM1-ERRORS
               END-EVALUATE
           END-IF

           IF  UPM1-MODEL-L        > ZERO
           OR  UPM1-MODEL-F        = FLDMODFY OR FLDCURSM
             EVALUATE TRUE
             WHEN UPM1-MODEL-I NOT > SPACES
               MOVE SPACES             TO UPM1-USRMODL
             WHEN UPM1-MODEL-I NOT = 'SYSA' AND 'OPER'
                                 AND 'PROG' AND 'USER'
               MOVE 'Synonym model not accepted.  See HELP.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-MODEL-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
             WHEN OTHER
               MOVE UPM1-MODEL-I       TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               EVALUATE EIBRESP
               WHEN DFHRESP(NORMAL)
                 CONTINUE
               WHEN DFHRESP(NOTFND)
                 MOVE DFHRESP(NORMAL)  TO EIBRESP
                 MOVE 'Synonym model does not exist.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM1-MODEL-L
                                          UPM1-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 GO TO D70-UPM1-ERRORS
               END-EVALUATE
               MOVE UPM1-MODEL-I       TO UPM1-USRMODL
             END-EVALUATE
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO D80-UPM1-OUTPUT
           END-IF.

           EVALUATE EIBAID ALSO TRUE
             WHEN AIDPFK05 ALSO NOT VIEW-PROCESS
               GO TO D40-UPM1-UPDATES
             WHEN AIDPFK07 ALSO TRUE
               GO TO G00-UPM4-ROUTINE
             WHEN AIDPFK08 ALSO TRUE
               GO TO E00-UPM2-ROUTINE
           END-EVALUATE.

           GO TO D50-UPM1-DISPLAY.

       D40-UPM1-UPDATES.

           PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT.
           EVALUATE TRUE
           WHEN MULTI-EDIT-MSG > SPACES
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM1-MESSAGE-H
               MOVE MULTI-EDIT-MSG     TO UPM1-MESSAGE-O
               GO TO D80-UPM1-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-UPM1-ERRORS
           END-EVALUATE.

           MOVE UPM1-USRIDNT           TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           IF  CHG-PROCESS
               PERFORM Q26-READUP-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-UPM1-ERRORS
               END-IF
           END-IF.
           IF  ADD-PROCESS
               SET  IUI-US-VERSION-64  TO TRUE
               MOVE LOW-VALUES         TO IUI-DATA
               PERFORM Q27-WRITE-IESCNTL
           ELSE
               PERFORM Q27-REWRITE-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO D70-UPM1-ERRORS
           END-IF.

           MOVE VUPM-CNTL-KEY          TO VUPM-SAVEKEY.
           IF  ADD-PROCESS
               MOVE COMM-CURRRTN       TO LDAP-PREVRTN
               GO TO I00-LDAP-ROUTINE
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       D50-UPM1-DISPLAY.

           MOVE 'UPM1'                 TO COMM-CURRRTN.

       D60-UPM1-DETAIL.

           IF  VUPM-SAVEKEY > SPACES
               MOVE VUPM-SAVEKEY       TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO D70-UPM1-ERRORS
               END-IF
               MOVE SPACES             TO VUPM-SAVEKEY
               PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
               PERFORM P80-IMPORT-RECORD-DATA THRU P85-EXIT
               IF  ADD-PROCESS
                   MOVE SPACES         TO LIST-EXPIRES
                                          LIST-EXPIRED
                                          LIST-SIGNON
                                          UPM1-USRIDNT
                                          UPM1-OPRIDNT
                                          UPM1-USRNAME
                                          UPM1-PASSWRD
                                          UPM1-REVOKED
                   MOVE ALL '.'        TO UPM1-OLDPSWTB
                   MOVE FLDUNFST       TO UPM1-USERNAME-A
                                          UPM1-PASSWORD-A
                   MOVE 'Adding new user profile...'
                                       TO UPM1-MESSAGE-O
               END-IF
           END-IF.

           MOVE UPM1-USRIDNT           TO UPM1-USERID-O.
           MOVE UPM1-USRNAME           TO UPM1-USERNAME-O.

           PERFORM Q40-INITKEY-IESLDUM.
           MOVE UPM1-USRIDNT           TO LDUM-MFUSRID.
           PERFORM Q46-READEQ-IESLDUV.
           IF  EIBRESP = DFHRESP(NOTFND)
               MOVE UPM1-USRIDNT       TO LDUM-NETUSRID
               PERFORM Q46-READEQ-IESLDUM
           END-IF
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  LDUM-MFUSRID <= SPACES
                   MOVE EXCBLUE        TO UPM1-MAPPED-TO-C
                   MOVE '(mapped as VSE-only user)'
                                       TO UPM1-MAPPED-TO-O
               ELSE
                   MOVE LDUM-NETUSRID  TO UPM1-MAPPED-TO-O
               END-IF
           ELSE
               MOVE EXCBLUE            TO UPM1-MAPPED-TO-C
               MOVE '(not mapped)'     TO UPM1-MAPPED-TO-O
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.

           MOVE UPM1-USRTYPE           TO UPM1-USERTYPE-O.
/DLC2/     MOVE UPM1-OPRIDNT           TO UPM1-OPERID-O.

           IF  USER-IS-ADMIN
               MOVE UPM1-PASSWRD       TO UPM1-PASSWORD-O
           ELSE
               MOVE LOW-VALUES         TO UPM1-PASSWORD-O
           END-IF.
           MOVE UPM1-EXPDAYS           TO UPM1-EXPIRES-O.
           MOVE UPM1-REVOKED           TO UPM1-REVOKE-O.

           MOVE UPM1-INITNAM           TO UPM1-INITIAL-NAME-O.
           MOVE UPM1-INITYPE           TO UPM1-NAME-TYPE-O.
           MOVE UPM1-USRMODL           TO UPM1-MODEL-O.

           IF  USER-IS-ADMIN
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
                   MOVE UPM1-OLDPSWD(IDX)
                                       TO UPM1-OLDPSWD-X(IDX)
               END-PERFORM
           ELSE
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1 UNTIL IDX > 12
                   MOVE ALL '.'        TO UPM1-OLDPSWD-X(IDX)
               END-PERFORM
           END-IF.

       D70-UPM1-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM1-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO UPM1-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       D80-UPM1-OUTPUT.

           MOVE MAPTITLE               TO UPM1-COMPANY-O.
           MOVE WS-WRKDATE             TO UPM1-SYSDATE-O.
           MOVE WS-WRKTIME             TO UPM1-SYSTIME-O.
           MOVE SPACES                 TO UPM1-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(UPM1-NETNAME-O) END-EXEC.
           IF  UPM1-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO UPM1-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
               MOVE FLDASFST           TO UPM1-USERID-A
               MOVE FLDASKIP           TO UPM1-USERNAME-A
                                          UPM1-USERTYPE-A
/DLC2/                                    UPM1-OPERID-A
                                          UPM1-PASSWORD-A
                                          UPM1-EXPIRES-A
                                          UPM1-REVOKE-A
                                          UPM1-INITIAL-NAME-A
                                          UPM1-NAME-TYPE-A
                                          UPM1-MODEL-A
               MOVE FLDASDRK           TO UPM1-UPDATE-KEY-A
                                          UPM1-UPDATE-TEXT-A
           ELSE
               IF  NOT ADD-PROCESS
                   MOVE FLDASFST       TO UPM1-USERID-A
               ELSE
                   MOVE EXHULINE       TO UPM1-USERID-H
               END-IF
               MOVE EXHULINE           TO UPM1-USERNAME-H
                                          UPM1-USERTYPE-H
/DLC2/                                    UPM1-OPERID-H
                                          UPM1-PASSWORD-H
                                          UPM1-EXPIRES-H
                                          UPM1-REVOKE-H
                                          UPM1-INITIAL-NAME-H
                                          UPM1-NAME-TYPE-H
                                          UPM1-MODEL-H
           END-IF.

       D85-UPM1-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (UPM1MAP)
                             DATAONLY
                             FROM     (UPM1MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (UPM1MAP)
                             DATAONLY
                             FROM     (UPM1MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               IF  CHG-PROCESS
                   MOVE -1             TO UPM1-USERNAME-L
               ELSE
                   MOVE -1             TO UPM1-USERID-L
               END-IF
               EXEC CICS SEND
                         MAP      (UPM1MAP)
                         FROM     (UPM1MAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
                         CURSOR
               END-EXEC
           END-IF.

       D90-UPM1-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (UPM1MAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    UPM PAGE 2 ROUTINE                                          *
      ******************************************************************
       E00-UPM2-ROUTINE.

           IF  ADDRESS OF UPM2MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF UPM2MAPI)
                         LENGTH   (LENGTH OF UPM2MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'UPM2'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO UPM2MAPO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM E80-UPM2-OUTPUT THRU E85-UPM2-SENDMAP
               GO TO E50-UPM2-DISPLAY
           END-IF.

       E10-UPM2-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (UPM2MAP)
                     INTO     (UPM2MAPI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO E70-UPM2-ERRORS
           END-IF.

       E20-UPM2-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE UPM2MAPI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDPFK05
           WHEN AIDPFK07
           WHEN AIDPFK08
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM2-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM2-MESSAGE-O
               GO TO E80-UPM2-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM2-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO UPM2-MESSAGE-O
               GO TO E80-UPM2-OUTPUT
           END-EVALUATE.

       E30-UPM2-PROCESS.

           IF  EIBAID = AIDPFK05
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM2-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM2-MESSAGE-O
               GO TO E80-UPM2-OUTPUT
           END-IF.

           INSPECT UPM2-DEFCATALOG-I CONVERTING LOWER-CASE
                                             TO UPPER-CASE.

           IF  NOT VIEW-PROCESS

           IF  UPM2-NEWS-L         > ZERO
           OR  UPM2-NEWS-F         = FLDMODFY OR FLDCURSM
               IF  UPM2-NEWS-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-NEWS-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-NEWS-I    TO UPM2-IESNEWS
               END-IF
           END-IF

           IF  UPM2-ESCAPE-L       > ZERO
           OR  UPM2-ESCAPE-F       = FLDMODFY OR FLDCURSM
               IF  UPM2-ESCAPE-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-ESCAPE-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-ESCAPE-I  TO UPM2-IESESCP
               END-IF
           END-IF

           IF  UPM2-CHGPSWD-L      > ZERO
           OR  UPM2-CHGPSWD-F      = FLDMODFY OR FLDCURSM
               IF  UPM2-CHGPSWD-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-CHGPSWD-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-CHGPSWD-I TO UPM2-IESCPWD
               END-IF
           END-IF

           IF  UPM2-CONFIRM-L      > ZERO
           OR  UPM2-CONFIRM-F      = FLDMODFY OR FLDCURSM
               IF  UPM2-CONFIRM-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-CONFIRM-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-CONFIRM-I TO UPM2-IESCDEL
               END-IF
           END-IF

           IF  UPM2-SUBLIB-L       > ZERO
           OR  UPM2-SUBLIB-F       = FLDMODFY OR FLDCURSM
               IF  UPM2-SUBLIB-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-SUBLIB-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-SUBLIB-I  TO UPM2-IESVPSL
               END-IF
           END-IF

           IF  UPM2-SUBMIT-L       > ZERO
           OR  UPM2-SUBMIT-F       = FLDMODFY OR FLDCURSM
               IF  UPM2-SUBMIT-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-SUBMIT-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-SUBMIT-I  TO UPM2-IESSUBM
               END-IF
           END-IF

           IF  UPM2-VSAMFILES-L    > ZERO
           OR  UPM2-VSAMFILES-F    = FLDMODFY OR FLDCURSM
               IF  UPM2-VSAMFILES-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-VSAMFILES-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-VSAMFILES-I
                                       TO UPM2-VSMFILE
               END-IF
           END-IF

           IF  UPM2-VSAMCATALOGS-L > ZERO
           OR  UPM2-VSAMCATALOGS-F = FLDMODFY OR FLDCURSM
               IF  UPM2-VSAMCATALOGS-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-VSAMCATALOGS-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-VSAMCATALOGS-I
                                       TO UPM2-VSMCATL
               END-IF
           END-IF

           IF  UPM2-OLPD-L         > ZERO
           OR  UPM2-OLPD-F         = FLDMODFY OR FLDCURSM
               IF  UPM2-OLPD-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-OLPD-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-OLPD-I    TO UPM2-IESOLPD
               END-IF
           END-IF

           IF  UPM2-COMMANDS-L     > ZERO
           OR  UPM2-COMMANDS-F     = FLDMODFY OR FLDCURSM
               IF  UPM2-COMMANDS-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-COMMANDS-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-COMMANDS-I
                                       TO UPM2-IESCNSL
               END-IF
           END-IF

           IF  UPM2-CONSOLE-L      > ZERO
           OR  UPM2-CONSOLE-F      = FLDMODFY OR FLDCURSM
               IF  UPM2-CONSOLE-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-CONSOLE-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-CONSOLE-I TO UPM2-IESCMSG
               END-IF
           END-IF

           IF  UPM2-POWER-L        > ZERO
           OR  UPM2-POWER-F        = FLDMODFY OR FLDCURSM
               IF  UPM2-POWER-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-POWER-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-POWER-I   TO UPM2-IESBQUE
               END-IF
           END-IF

           IF  UPM2-APPLICATIONS-L > ZERO
           OR  UPM2-APPLICATIONS-F = FLDMODFY OR FLDCURSM
               IF  UPM2-APPLICATIONS-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-APPLICATIONS-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-APPLICATIONS-I
                                       TO UPM2-APMAUTH
               END-IF
           END-IF

           IF  UPM2-SELECTIONS-L   > ZERO
           OR  UPM2-SELECTIONS-F   = FLDMODFY OR FLDCURSM
               IF  UPM2-SELECTIONS-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-SELECTIONS-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-SELECTIONS-I
                                       TO UPM2-SPMAUTH
               END-IF
           END-IF

           IF  UPM2-USERPROFILES-L > ZERO
           OR  UPM2-USERPROFILES-F = FLDMODFY OR FLDCURSM
               IF  UPM2-USERPROFILES-I NOT = '1' AND '2'
                   MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM2-USERPROFILES-L
                                          UPM2-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                   MOVE UPM2-USERPROFILES-I
                                       TO UPM2-UPMAUTH
               END-IF
           END-IF

           IF  UPM2-DEFCATALOG-L   > ZERO
           OR  UPM2-DEFCATALOG-F   = FLDMODFY OR FLDCURSM
               IF  UPM2-DEFCATALOG-I > SPACES
                   MOVE UPM2-DEFCATALOG-I
                                       TO UPM2-USERCAT
               ELSE
                   MOVE SPACES         TO UPM2-USERCAT
               END-IF
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO E80-UPM2-OUTPUT
           END-IF.

           EVALUATE EIBAID ALSO TRUE
             WHEN AIDPFK05 ALSO NOT VIEW-PROCESS
               GO TO E40-UPM2-UPDATES
             WHEN AIDPFK07 ALSO TRUE
               GO TO D00-UPM1-ROUTINE
             WHEN AIDPFK08 ALSO TRUE
               GO TO F00-UPM3-ROUTINE
           END-EVALUATE.

           GO TO E50-UPM2-DISPLAY.

       E40-UPM2-UPDATES.

           PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT.
           EVALUATE TRUE
           WHEN MULTI-EDIT-MSG > SPACES
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM2-MESSAGE-H
               MOVE MULTI-EDIT-MSG     TO UPM2-MESSAGE-O
               GO TO E80-UPM2-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO E70-UPM2-ERRORS
           END-EVALUATE.

           MOVE UPM1-USRIDNT           TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           IF  CHG-PROCESS
               PERFORM Q26-READUP-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO E70-UPM2-ERRORS
               END-IF
           END-IF.
           IF  ADD-PROCESS
               SET  IUI-US-VERSION-64  TO TRUE
               MOVE LOW-VALUES         TO IUI-DATA
               PERFORM Q27-WRITE-IESCNTL
           ELSE
               PERFORM Q27-REWRITE-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO E70-UPM2-ERRORS
           END-IF.

           MOVE VUPM-CNTL-KEY          TO VUPM-SAVEKEY.
           IF  ADD-PROCESS
               MOVE COMM-CURRRTN       TO LDAP-PREVRTN
               GO TO I00-LDAP-ROUTINE
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       E50-UPM2-DISPLAY.

           MOVE 'UPM2'                 TO COMM-CURRRTN.

       E60-UPM2-DETAIL.

           MOVE UPM1-USRIDNT           TO UPM2-USERID-O.
           MOVE UPM2-IESNEWS           TO UPM2-NEWS-O.
           MOVE UPM2-IESESCP           TO UPM2-ESCAPE-O.
           MOVE UPM2-IESCPWD           TO UPM2-CHGPSWD-O.
           MOVE UPM2-IESCDEL           TO UPM2-CONFIRM-O.
           MOVE UPM2-IESVPSL           TO UPM2-SUBLIB-O.
           MOVE UPM2-IESSUBM           TO UPM2-SUBMIT-O.
           MOVE UPM2-VSMFILE           TO UPM2-VSAMFILES-O.
           MOVE UPM2-VSMCATL           TO UPM2-VSAMCATALOGS-O.
           MOVE UPM2-IESOLPD           TO UPM2-OLPD-O.
           MOVE UPM2-IESCNSL           TO UPM2-COMMANDS-O.
           MOVE UPM2-IESCMSG           TO UPM2-CONSOLE-O.
           MOVE UPM2-IESBQUE           TO UPM2-POWER-O.
           MOVE UPM2-APMAUTH           TO UPM2-APPLICATIONS-O.
           MOVE UPM2-SPMAUTH           TO UPM2-SELECTIONS-O.
           MOVE UPM2-UPMAUTH           TO UPM2-USERPROFILES-O.
           MOVE UPM2-USERCAT           TO UPM2-DEFCATALOG-O.

       E70-UPM2-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM2-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO UPM2-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       E80-UPM2-OUTPUT.

           MOVE MAPTITLE               TO UPM2-COMPANY-O.
           MOVE WS-WRKDATE             TO UPM2-SYSDATE-O.
           MOVE WS-WRKTIME             TO UPM2-SYSTIME-O.
           MOVE SPACES                 TO UPM2-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(UPM2-NETNAME-O) END-EXEC.
           IF  UPM2-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO UPM2-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
               MOVE FLDASKIP           TO UPM2-NEWS-A
                                          UPM2-ESCAPE-A
                                          UPM2-CHGPSWD-A
                                          UPM2-CONFIRM-A
                                          UPM2-SUBLIB-A
                                          UPM2-SUBMIT-A
                                          UPM2-VSAMFILES-A
                                          UPM2-VSAMCATALOGS-A
                                          UPM2-OLPD-A
                                          UPM2-COMMANDS-A
                                          UPM2-CONSOLE-A
                                          UPM2-POWER-A
                                          UPM2-APPLICATIONS-A
                                          UPM2-SELECTIONS-A
                                          UPM2-USERPROFILES-A
                                          UPM2-DEFCATALOG-A
               MOVE FLDASDRK           TO UPM2-UPDATE-KEY-A
                                          UPM2-UPDATE-TEXT-A
           ELSE
               MOVE EXHULINE           TO UPM2-NEWS-H
                                          UPM2-ESCAPE-H
                                          UPM2-CHGPSWD-H
                                          UPM2-CONFIRM-H
                                          UPM2-SUBLIB-H
                                          UPM2-SUBMIT-H
                                          UPM2-VSAMFILES-H
                                          UPM2-VSAMCATALOGS-H
                                          UPM2-OLPD-H
                                          UPM2-COMMANDS-H
                                          UPM2-CONSOLE-H
                                          UPM2-POWER-H
                                          UPM2-APPLICATIONS-H
                                          UPM2-SELECTIONS-H
                                          UPM2-USERPROFILES-H
                                          UPM2-DEFCATALOG-H
           END-IF.

           IF  IS-GENERAL-USER
             MOVE FLDASDRK             TO UPM2-VSAMFILES-LABEL-A
                                          UPM2-VSAMCATALOGS-LABEL-A
                                          UPM2-OLPD-LABEL-A
                                          UPM2-COMMANDS-LABEL-A
                                          UPM2-CONSOLE-LABEL-A
                                          UPM2-POWER-LABEL-A
                                          UPM2-DEFCATALOG-LABEL-A
             MOVE FLDASDRK             TO UPM2-VSAMFILES-A
                                          UPM2-VSAMCATALOGS-A
                                          UPM2-OLPD-A
                                          UPM2-COMMANDS-A
                                          UPM2-CONSOLE-A
                                          UPM2-POWER-A
                                          UPM2-DEFCATALOG-A
             MOVE FLDASDRK             TO UPM2-VSAMFILES-TEXT-A
                                          UPM2-VSAMCATALOGS-TEXT-A
                                          UPM2-OLPD-TEXT-A
                                          UPM2-COMMANDS-TEXT-A
                                          UPM2-CONSOLE-TEXT-A
                                          UPM2-POWER-TEXT-A
           END-IF.
           IF  NOT IS-ADMINISTRATOR
             MOVE FLDASDRK             TO UPM2-APPLICATIONS-LABEL-A
                                          UPM2-SELECTIONS-LABEL-A
                                          UPM2-USERPROFILES-LABEL-A
             MOVE FLDASDRK             TO UPM2-APPLICATIONS-A
                                          UPM2-SELECTIONS-A
                                          UPM2-USERPROFILES-A
             MOVE FLDASDRK             TO UPM2-APPLICATIONS-TEXT-A
                                          UPM2-SELECTIONS-TEXT-A
                                          UPM2-USERPROFILES-TEXT-A
           END-IF.

       E85-UPM2-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (UPM2MAP)
                             DATAONLY
                             FROM     (UPM2MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (UPM2MAP)
                             DATAONLY
                             FROM     (UPM2MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (UPM2MAP)
                         FROM     (UPM2MAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       E90-UPM2-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (UPM2MAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    UPM PAGE 3 ROUTINE                                          *
      ******************************************************************
       F00-UPM3-ROUTINE.

           IF  ADDRESS OF UPM3MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF UPM3MAPI)
                         LENGTH   (LENGTH OF UPM3MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF UPM3MAPX TO ADDRESS OF UPM3MAPI
           END-IF.

           IF  COMM-CURRRTN NOT = 'UPM3'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO UPM3MAPO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM F80-UPM3-OUTPUT THRU F85-UPM3-SENDMAP
               GO TO F50-UPM3-DISPLAY
           END-IF.

       F10-UPM3-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (UPM3MAP)
                     INTO     (UPM3MAPI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO F70-UPM3-ERRORS
           END-IF.

       F20-UPM3-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE UPM3MAPI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDPFK05
           WHEN AIDPFK07
           WHEN AIDPFK08
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM3-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM3-MESSAGE-O
               GO TO F80-UPM3-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM3-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO UPM3-MESSAGE-O
               GO TO F80-UPM3-OUTPUT
           END-EVALUATE.

       F30-UPM3-PROCESS.

           IF  EIBAID = AIDPFK05
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM3-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM3-MESSAGE-O
               GO TO F80-UPM3-OUTPUT
           END-IF.

/DLC2/*    INSPECT UPM3-OPERID-I CONVERTING LOWER-CASE
/DLC2/*                                  TO UPPER-CASE.
           INSPECT UPM3-LANGUAGE-I CONVERTING LOWER-CASE
                                           TO UPPER-CASE.

           IF  NOT VIEW-PROCESS

/DLC2/*    IF  UPM3-OPERID-L       > ZERO
/DLC2/*    OR  UPM3-OPERID-F       = FLDMODFY OR FLDCURSM
/DLC2/*        MOVE UPM3-OPERID-I      TO UPM1-OPRIDNT
/DLC2/*        PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT
/DLC2/*        EVALUATE TRUE
/DLC2/*        WHEN MULTI-EDIT-MSG > SPACES
/DLC2/*            CALL MULTMESG    USING DFHEIBLK MULTMESG
/DLC2/*                                   ERROR-SWITCH
/DLC2/*                                   UPM3-OPERID-L
/DLC2/*                                   UPM3-MESSAGE-L
/DLC2/*                                   MULTI-EDIT-MSG
/DLC2/*        WHEN EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/*            GO TO F70-UPM3-ERRORS
/DLC2/*        END-EVALUATE
/DLC2/*    END-IF

           IF  UPM3-PRIORITY-L     > ZERO
           OR  UPM3-PRIORITY-F     = FLDMODFY OR FLDCURSM
             IF  UPM3-PRIORITY-I NOT > SPACES
                 MOVE '100'            TO UPM3-OPERPRI
                                          UPM3-PRIORITY-O
             ELSE
               SET NUM-REQUEST-NORMAL-EDIT TO TRUE
               MOVE 03                 TO NUM-TOTL
               MOVE 0                  TO NUM-DECM
               MOVE UPM3-PRIORITY-I    TO NUM-DATA
               CALL 'NUMMAN'        USING NUMMAN-PARMS
               EVALUATE TRUE
               WHEN NOT NUM-REQUEST-COMPLETED
                 STRING 'Operator priority is not valid.'
                        '  rc=''' NUM-RETN ''''
                   DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-PRIORITY-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN NUM-T03-D0 < ZERO
               OR   NUM-T03-D0 > 255
                 MOVE 'Operator priority value is out of range.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-PRIORITY-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 MOVE NUM-T03-D0       TO UPM3-OPERPRI
                                          UPM3-PRIORITY-O
               END-EVALUATE
             END-IF
           END-IF

           IF  UPM3-TIMEOUT-L     > ZERO
           OR  UPM3-TIMEOUT-F     = FLDMODFY OR FLDCURSM
             IF  UPM3-TIMEOUT-I NOT > SPACES
               MOVE 'Timeout is required.  Specify 0 (zero) for inactive
      -             ' sessions that don''t time out.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-TIMEOUT-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               SET NUM-REQUEST-NORMAL-EDIT TO TRUE
               MOVE 02                 TO NUM-TOTL
               MOVE 0                  TO NUM-DECM
               MOVE UPM3-TIMEOUT-I     TO NUM-DATA
               CALL 'NUMMAN'        USING NUMMAN-PARMS
               EVALUATE TRUE
               WHEN NOT NUM-REQUEST-COMPLETED
                 STRING 'Session timeout is not valid.'
                        '  rc=''' NUM-RETN ''''
                   DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-TIMEOUT-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN NUM-T02-D0 < ZERO
               OR   NUM-T02-D0 > 60
                 MOVE 'Session timeout value is out of range.'
                                       TO MULTI-EDIT-MSG
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-TIMEOUT-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
               WHEN OTHER
                 MOVE NUM-T02-D0       TO UPM3-TIMEOUT
                                          UPM3-TIMEOUT-O
               END-EVALUATE
             END-IF
           END-IF

           IF  UPM3-LANGUAGE-L     > ZERO
           OR  UPM3-LANGUAGE-F     = FLDMODFY OR FLDCURSM
             IF  UPM3-LANGUAGE-I = LOW-VALUE
               MOVE SPACES             TO UPM3-LANGUAGE-I
             END-IF
             IF  UPM3-LANGUAGE-I NOT = SPACE AND 'E' AND 'C'
                                             AND 'D' AND 'J'
               MOVE 'Valid entries are blank, E, C, D, and J.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-LANGUAGE-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               MOVE UPM3-LANGUAGE-I    TO UPM3-NATLANG
             END-IF
           END-IF

           IF  UPM3-XRFSIGNOFF-L   > ZERO
           OR  UPM3-XRFSIGNOFF-F   = FLDMODFY OR FLDCURSM
             IF  UPM3-XRFSIGNOFF-I NOT = '1' AND '2'
               MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM3-XRFSIGNOFF-L
                                          UPM3-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               MOVE UPM3-XRFSIGNOFF-I  TO UPM3-XRFSGNO
             END-IF
           END-IF

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 64
               IF  UPM3-SECKEY-L(IDX) > ZERO
               OR  UPM3-SECKEY-A(IDX) = FLDMODFY OR FLDCURSM
                 IF  UPM3-SECKEY-X(IDX) NOT > SPACE
                   MOVE SPACE          TO UPM3-TRANSEC(IDX)
                 ELSE
                   MOVE 'X'            TO UPM3-TRANSEC(IDX)
                 END-IF
               END-IF
           END-PERFORM

           END-IF.

           IF  ERRORS-FOUND
               GO TO F80-UPM3-OUTPUT
           END-IF.

           EVALUATE EIBAID ALSO TRUE
             WHEN AIDPFK05 ALSO NOT VIEW-PROCESS
               GO TO F40-UPM3-UPDATES
             WHEN AIDPFK07 ALSO TRUE
               GO TO E00-UPM2-ROUTINE
             WHEN AIDPFK08 ALSO TRUE
               GO TO G00-UPM4-ROUTINE
           END-EVALUATE.

           GO TO F50-UPM3-DISPLAY.

       F40-UPM3-UPDATES.

           PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT.
           EVALUATE TRUE
           WHEN MULTI-EDIT-MSG > SPACES
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM3-MESSAGE-H
               MOVE MULTI-EDIT-MSG     TO UPM3-MESSAGE-O
               GO TO F80-UPM3-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO F70-UPM3-ERRORS
           END-EVALUATE.

           MOVE UPM1-USRIDNT           TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           IF  CHG-PROCESS
               PERFORM Q26-READUP-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO F70-UPM3-ERRORS
               END-IF
           END-IF.
           IF  ADD-PROCESS
               SET  IUI-US-VERSION-64  TO TRUE
               MOVE LOW-VALUES         TO IUI-DATA
               PERFORM Q27-WRITE-IESCNTL
           ELSE
               PERFORM Q27-REWRITE-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO F70-UPM3-ERRORS
           END-IF.

           MOVE VUPM-CNTL-KEY          TO VUPM-SAVEKEY.
           IF  ADD-PROCESS
               MOVE COMM-CURRRTN       TO LDAP-PREVRTN
               GO TO I00-LDAP-ROUTINE
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       F50-UPM3-DISPLAY.

           MOVE 'UPM3'                 TO COMM-CURRRTN.

       F60-UPM3-DETAIL.

           MOVE UPM1-USRIDNT           TO UPM3-USERID-O.
/DLC2/*    MOVE UPM3-OPRIDNT           TO UPM3-OPERID-O.
           MOVE UPM3-OPERPRI           TO UPM3-PRIORITY-O.
           MOVE UPM3-XRFSGNO           TO UPM3-XRFSIGNOFF-O.
           MOVE UPM3-TIMEOUT           TO UPM3-TIMEOUT-O.
           MOVE UPM3-NATLANG           TO UPM3-LANGUAGE-O.

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 64
               MOVE UPM3-TRANSEC(IDX)  TO UPM3-SECKEY-X(IDX)
           END-PERFORM.

       F70-UPM3-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM3-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO UPM3-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       F80-UPM3-OUTPUT.

           MOVE MAPTITLE               TO UPM3-COMPANY-O.
           MOVE WS-WRKDATE             TO UPM3-SYSDATE-O.
           MOVE WS-WRKTIME             TO UPM3-SYSTIME-O.
           MOVE SPACES                 TO UPM3-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(UPM3-NETNAME-O) END-EXEC.
           IF  UPM3-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO UPM3-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
/DLC2/*        MOVE FLDASKIP           TO UPM3-OPERID-A
               MOVE FLDASKIP           TO UPM3-PRIORITY-A
                                          UPM3-XRFSIGNOFF-A
                                          UPM3-TIMEOUT-A
                                          UPM3-LANGUAGE-A
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 2 BY 1 UNTIL IDX > 64
                   MOVE FLDASKIP       TO UPM3-SECKEY-A(IDX)
               END-PERFORM
               MOVE FLDASDRK           TO UPM3-UPDATE-KEY-A
                                          UPM3-UPDATE-TEXT-A
           ELSE
/DLC2/*        MOVE EXHULINE           TO UPM3-OPERID-H
               MOVE EXHULINE           TO UPM3-PRIORITY-H
                                          UPM3-XRFSIGNOFF-H
                                          UPM3-TIMEOUT-H
                                          UPM3-LANGUAGE-H
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 2 BY 1 UNTIL IDX > 64
                   IF  IDX NOT = 61
                       MOVE EXHULINE   TO UPM3-SECKEY-H(IDX)
                   END-IF
               END-PERFORM
           END-IF.

       F85-UPM3-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (UPM3MAP)
                             DATAONLY
                             FROM     (UPM3MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (UPM3MAP)
                             DATAONLY
                             FROM     (UPM3MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (UPM3MAP)
                         FROM     (UPM3MAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       F90-UPM3-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (UPM3MAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    UPM PAGE 4 ROUTINE                                          *
      ******************************************************************
       G00-UPM4-ROUTINE.

           IF  ADDRESS OF UPM4MAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF UPM4MAPI)
                         LENGTH   (LENGTH OF UPM4MAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
               SET ADDRESS OF UPM4MAPX TO ADDRESS OF UPM4MAPI
           END-IF.

           IF  COMM-CURRRTN NOT = 'UPM4'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO UPM4MAPO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM G80-UPM4-OUTPUT THRU G85-UPM4-SENDMAP
               GO TO G50-UPM4-DISPLAY
           END-IF.

       G10-UPM4-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (UPM4MAP)
                     INTO     (UPM4MAPI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO G70-UPM4-ERRORS
           END-IF.

       G20-UPM4-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE UPM4MAPI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDPFK05
           WHEN AIDPFK07
           WHEN AIDPFK08
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM4-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM4-MESSAGE-O
               GO TO G80-UPM4-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM4-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO UPM4-MESSAGE-O
               GO TO G80-UPM4-OUTPUT
           END-EVALUATE.

       G30-UPM4-PROCESS.

           IF  EIBAID = AIDPFK05
           AND VIEW-PROCESS
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM4-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO UPM4-MESSAGE-O
               GO TO G80-UPM4-OUTPUT
           END-IF.

           IF  NOT VIEW-PROCESS

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 24
               IF  UPM4-OPCLASS-L(IDX) > ZERO
               OR  UPM4-OPCLASS-A(IDX) = FLDMODFY OR FLDCURSM
                 IF  UPM4-OPCLASS-X(IDX) NOT > SPACE
                   MOVE SPACE          TO UPM4-OPCLASS(IDX)
                 ELSE
                   MOVE 'X'            TO UPM4-OPCLASS(IDX)
                 END-IF
               END-IF
           END-PERFORM

           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
               IF  UPM4-DTSECTAB-L(IDX) > ZERO
               OR  UPM4-DTSECTAB-A(IDX) = FLDMODFY OR FLDCURSM
                 IF  UPM4-DTSECTAB-X(IDX) NOT = LOW-VALUE
                 AND UPM4-DTSECTAB-X(IDX) NOT = SPACE
                 AND UPM4-DTSECTAB-X(IDX) NOT NUMERIC
                 OR  UPM4-DTSECTAB-X(IDX) > '4'
                   MOVE 'Valid entry is blank or 1 to 4.'
                                       TO MULTI-EDIT-MSG
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM4-DTSECTAB-L(IDX)
                                          UPM4-MESSAGE-L
                                          MULTI-EDIT-MSG
                 ELSE
                   IF  UPM4-DTSECTAB-X(IDX) NOT > SPACE
                     MOVE ZERO         TO UPM4-DTSECTB(IDX)
                   ELSE
                     MOVE UPM4-DTSECTAB-X(IDX)
                                       TO UPM4-DTSECTB(IDX)
                   END-IF
                 END-IF
               END-IF
           END-PERFORM

           IF  UPM4-DIRECTORY-L    > ZERO
           OR  UPM4-DIRECTORY-F    = FLDMODFY OR FLDCURSM
             IF  UPM4-DIRECTORY-I NOT = '1' AND '2'
               MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM4-DIRECTORY-L
                                          UPM4-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               MOVE UPM4-DIRECTORY-I  TO UPM4-DTSREAD
             END-IF
           END-IF

           IF  UPM4-BTRANSIENTS-L  > ZERO
           OR  UPM4-BTRANSIENTS-F  = FLDMODFY OR FLDCURSM
             IF  UPM4-BTRANSIENTS-I NOT = '1' AND '2'
               MOVE 'Valid entry is 1 for YES or 2 for NO.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          UPM4-BTRANSIENTS-L
                                          UPM4-MESSAGE-L
                                          MULTI-EDIT-MSG
             ELSE
               MOVE UPM4-BTRANSIENTS-I TO UPM4-DTSBTRN
             END-IF
           END-IF

           END-IF.

           IF  ERRORS-FOUND
               GO TO G80-UPM4-OUTPUT
           END-IF.

           EVALUATE EIBAID ALSO TRUE
             WHEN AIDPFK05 ALSO NOT VIEW-PROCESS
               GO TO G40-UPM4-UPDATES
             WHEN AIDPFK07 ALSO TRUE
               GO TO F00-UPM3-ROUTINE
             WHEN AIDPFK08 ALSO TRUE
               GO TO D00-UPM1-ROUTINE
           END-EVALUATE.

           GO TO G50-UPM4-DISPLAY.

       G40-UPM4-UPDATES.

           PERFORM P60-CHECK-OPERATOR-ID THRU P65-EXIT.
           EVALUATE TRUE
           WHEN MULTI-EDIT-MSG > SPACES
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM4-MESSAGE-H
               MOVE MULTI-EDIT-MSG     TO UPM4-MESSAGE-O
               GO TO G80-UPM4-OUTPUT
           WHEN EIBRESP NOT = DFHRESP(NORMAL)
               GO TO G70-UPM4-ERRORS
           END-EVALUATE.

           MOVE UPM1-USRIDNT           TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           IF  CHG-PROCESS
               PERFORM Q26-READUP-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO G70-UPM4-ERRORS
               END-IF
           END-IF.
           IF  ADD-PROCESS
               SET  IUI-US-VERSION-64  TO TRUE
               MOVE LOW-VALUES         TO IUI-DATA
               PERFORM Q27-WRITE-IESCNTL
           ELSE
               PERFORM Q27-REWRITE-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO G70-UPM4-ERRORS
           END-IF.

           MOVE VUPM-CNTL-KEY          TO VUPM-SAVEKEY.
           IF  ADD-PROCESS
               MOVE COMM-CURRRTN       TO LDAP-PREVRTN
               GO TO I00-LDAP-ROUTINE
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           GO TO C00-LIST-ROUTINE.

       G50-UPM4-DISPLAY.

           MOVE 'UPM4'                 TO COMM-CURRRTN.

       G60-UPM4-DETAIL.

           MOVE UPM1-USRIDNT           TO UPM4-USERID-O.
           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 24
               MOVE UPM4-OPCLASS(IDX)  TO UPM4-OPCLASS-X(IDX)
           END-PERFORM.
           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
               IF  UPM4-DTSECTB(IDX) = ZERO
                 MOVE SPACE            TO UPM4-DTSECTAB-X(IDX)
               ELSE
                 MOVE UPM4-DTSECTB(IDX) TO UPM4-DTSECTAB-X(IDX)
               END-IF
           END-PERFORM.
           MOVE UPM4-DTSREAD           TO UPM4-DIRECTORY-O.
           MOVE UPM4-DTSBTRN           TO UPM4-BTRANSIENTS-O.

       G70-UPM4-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO UPM4-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO UPM4-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       G80-UPM4-OUTPUT.

           MOVE MAPTITLE               TO UPM4-COMPANY-O.
           MOVE WS-WRKDATE             TO UPM4-SYSDATE-O.
           MOVE WS-WRKTIME             TO UPM4-SYSTIME-O.
           MOVE SPACES                 TO UPM4-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(UPM4-NETNAME-O) END-EXEC.
           IF  UPM4-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO UPM4-NETNAME-O
           END-IF.

           IF  NOT ADD-PROCESS
           AND NOT CHG-PROCESS
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 2 BY 1 UNTIL IDX > 24
                   MOVE FLDASKIP       TO UPM4-OPCLASS-A(IDX)
               END-PERFORM
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
                   MOVE FLDASKIP       TO UPM4-DTSECTAB-A(IDX)
               END-PERFORM
               MOVE FLDASKIP           TO UPM4-DIRECTORY-A
                                          UPM4-BTRANSIENTS-A
               MOVE FLDASDRK           TO UPM4-UPDATE-KEY-A
                                          UPM4-UPDATE-TEXT-A
           ELSE
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 2 BY 1 UNTIL IDX > 24
                   MOVE EXHULINE       TO UPM4-OPCLASS-H(IDX)
               END-PERFORM
               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1 UNTIL IDX > 32
                   MOVE EXHULINE       TO UPM4-DTSECTAB-H(IDX)
               END-PERFORM
               MOVE EXHULINE           TO UPM4-DIRECTORY-H
                                          UPM4-BTRANSIENTS-H
           END-IF.

           IF  IS-GENERAL-USER
             MOVE FLDASDRK             TO UPM4-DIRECTORY-LABEL-A
                                          UPM4-BTRANSIENTS-LABEL-A
             MOVE FLDASDRK             TO UPM4-DIRECTORY-A
                                          UPM4-BTRANSIENTS-A
             MOVE FLDASDRK             TO UPM4-DIRECTORY-TEXT-A
                                          UPM4-BTRANSIENTS-TEXT-A
           END-IF.

       G85-UPM4-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (UPM4MAP)
                             DATAONLY
                             FROM     (UPM4MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (UPM4MAP)
                             DATAONLY
                             FROM     (UPM4MAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (UPM4MAP)
                         FROM     (UPM4MAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       G90-UPM4-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (UPM4MAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    DOCUMENTS ROUTINE                                           *
      ******************************************************************
       H00-DOCU-ROUTINE.

           IF  ADDRESS OF DOCUMAPI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF DOCUMAPI)
                         LENGTH   (LENGTH OF DOCUMAPI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'DOCU'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO DOCUMAPO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM H80-DOCU-OUTPUT THRU H85-DOCU-SENDMAP
               GO TO H50-DOCU-DISPLAY
           END-IF.

       H10-DOCU-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (DOCUMAP)
                     INTO     (DOCUMAPI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO H70-DOCU-ERRORS
           END-IF.

       H20-DOCU-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE DOCUMAPI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDCURSR
           WHEN AIDPFK05
           WHEN AIDPFK06
           WHEN AIDPFK07
           WHEN AIDPFK08
           WHEN AIDPFK09
/DLC6/     WHEN AIDPFK10
/DLC6/     WHEN AIDPFK11
               CONTINUE

           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DOCU-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO DOCU-MESSAGE-O
               GO TO H80-DOCU-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DOCU-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO DOCU-MESSAGE-O
               GO TO H80-DOCU-OUTPUT
           END-EVALUATE.

       H30-DOCU-PROCESS.

           EVALUATE TRUE

           WHEN EIBAID = AIDPFK05
            OR  DOCU-BYUSERID-F = FLDMODFY OR FLDCURSM
               MOVE    1     TO VUPM-RPTSEQU
               GO TO L00-LOAD-ROUTINE

           WHEN EIBAID = AIDPFK06
            OR  DOCU-BYUSERNAME-F = FLDMODFY OR FLDCURSM
               MOVE    2     TO VUPM-RPTSEQU
               GO TO L00-LOAD-ROUTINE

           WHEN EIBAID = AIDPFK07
            OR  DOCU-BYUSERTYPE-F = FLDMODFY OR FLDCURSM
               MOVE    3     TO VUPM-RPTSEQU
               GO TO L00-LOAD-ROUTINE

           WHEN EIBAID = AIDPFK08
            OR  DOCU-BYINITIALNAME-F = FLDMODFY OR FLDCURSM
               MOVE    4     TO VUPM-RPTSEQU
               GO TO L00-LOAD-ROUTINE

           WHEN EIBAID = AIDPFK09
            OR  DOCU-GENSIGNONS-F = FLDMODFY OR FLDCURSM
               GO TO J00-SUBM-ROUTINE

/DLC6/     WHEN EIBAID = AIDPFK10
/DLC6/      OR  DOCU-WIN-FUNCTION-F = FLDMODFY OR FLDCURSM
/DLC6/         MOVE    5     TO VUPM-RPTSEQU
/DLC6/         GO TO L00-LOAD-ROUTINE

/DLC6/     WHEN EIBAID = AIDPFK11
/DLC6/      OR  DOCU-OLD-GROUP-F = FLDMODFY OR FLDCURSM
/DLC6/         MOVE    6     TO VUPM-RPTSEQU
/DLC6/         GO TO L00-LOAD-ROUTINE

           END-EVALUATE.

       H50-DOCU-DISPLAY.

           MOVE 'DOCU'                 TO COMM-CURRRTN.

       H60-DOCU-DETAIL.

       H70-DOCU-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO DOCU-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO DOCU-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       H80-DOCU-OUTPUT.

           MOVE MAPTITLE               TO DOCU-COMPANY-O.
           MOVE WS-WRKDATE             TO DOCU-SYSDATE-O.
           MOVE WS-WRKTIME             TO DOCU-SYSTIME-O.
           MOVE SPACES                 TO DOCU-NETNAME-O.
           EXEC CICS ASSIGN NETNAME(DOCU-NETNAME-O) END-EXEC.
           IF  DOCU-NETNAME-I NOT > SPACES
               STRING 'Term: ' EIBTRMID
                   DELIMITED BY SIZE INTO DOCU-NETNAME-O
           END-IF.

       H85-DOCU-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (DOCUMAP)
                             DATAONLY
                             FROM     (DOCUMAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (DOCUMAP)
                             DATAONLY
                             FROM     (DOCUMAPO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (DOCUMAP)
                         FROM     (DOCUMAPO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
                         ERASE
               END-EXEC
           END-IF.

       H90-DOCU-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (DOCUMAPI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    LDAP MAPPING ROUTINE                                        *
      ******************************************************************
       I00-LDAP-ROUTINE.

           IF  ADDRESS OF LDAPWINI = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF LDAPWINI)
                         LENGTH   (LENGTH OF LDAPWINI)
                         INITIMG  (LOVALUE)
               END-EXEC
           END-IF.

           IF  COMM-CURRRTN NOT = 'LDAP'
           OR  COMM-SAVE-FUNC   = INIT-FLAG
               IF  COMM-SAVE-FUNC = INIT-FLAG
                   MOVE COMM-SAVE-FUNC TO COMM-CURRRTN
                   MOVE SPACES         TO COMM-SAVE-FUNC
                   MOVE HIGH-VALUES    TO INIT-FLAG
               END-IF
               SET  NO-ERRORS-FOUND    TO TRUE
               IF  COMM-CURRRTN = 'RETN'
                   MOVE VUPM-SAVE-TIOA TO LDAPWINO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               PERFORM I80-LDAP-OUTPUT THRU I85-LDAP-SENDMAP
               GO TO I50-LDAP-DISPLAY
           END-IF.

       I10-LDAP-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (LDAPWIN)
                     INTO     (LDAPWINI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO I70-LDAP-ERRORS
           END-IF.

       I20-LDAP-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               MOVE VUPM-CNTL-KEY      TO VUPM-SAVEKEY
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE LDAPWINI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDENTER
           WHEN AIDPFK05
               CONTINUE

           WHEN AIDCURSR
           WHEN AIDPAK01
           WHEN AIDPAK02
           WHEN AIDPAK03
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LDAP-MESSAGE-H
               MOVE 'No current action assigned to attention key.'
                                       TO LDAP-MESSAGE-O
               GO TO I80-LDAP-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LDAP-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO LDAP-MESSAGE-O
               GO TO I80-LDAP-OUTPUT
           END-EVALUATE.

       I30-LDAP-PROCESS.

           IF  LDAP-AUTHTYPE-L     > ZERO
           OR  LDAP-AUTHTYPE-F     = FLDMODFY OR FLDCURSM
             IF  LDAP-AUTHTYPE-I NOT = '1' AND '2'
               MOVE 'Authorization type must be 1 or 2.'
                                       TO MULTI-EDIT-MSG
               MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-AUTHTYPE-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF.

           IF  LDAP-GENPWD-L       > ZERO
           OR  LDAP-GENPWD-F       = FLDMODFY OR FLDCURSM
             IF  LDAP-GENPWD-I NOT = '1' AND '2'
               MOVE 'Generate password must be 1 or 2.'
                                       TO MULTI-EDIT-MSG
               MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-GENPWD-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF.

           IF  LDAP-PATTERN-L      > ZERO
           OR  LDAP-PATTERN-F      = FLDMODFY OR FLDCURSM
             IF  LDAP-PATTERN-I NOT > SPACES
               MOVE 'caaaaaaa'         TO LDAP-PATTERN-O
             END-IF
             IF  LDAP-PATTERN-I NOT > SPACES
             AND LDAP-GENPWD-I = '1'
               MOVE 'Pattern is required.'
                                       TO MULTI-EDIT-MSG
               MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-PATTERN-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF.

           IF  LDAP-DESCRIPTION-L  > ZERO
           OR  LDAP-DESCRIPTION-F  = FLDMODFY OR FLDCURSM
             IF  LDAP-DESCRIPTION-I NOT > SPACES
               MOVE UPM1-USRNAME       TO LDAP-DESCRIPTION-O
             END-IF
             IF  LDAP-DESCRIPTION-I NOT > SPACES
               MOVE 'Description is required.'
                                       TO MULTI-EDIT-MSG
               MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-DESCRIPTION-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
             END-IF
           END-IF.

           IF  LDAP-NETWORKID-L    > ZERO
           OR  LDAP-NETWORKID-F    = FLDMODFY OR FLDCURSM
             IF  LDAP-AUTHTYPE-I = '1'
               IF  LDAP-NETWORKID-I NOT > SPACES
                 MOVE 'Network id is required.'
                                       TO MULTI-EDIT-MSG
                 MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
                 CALL MULTMESG      USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-NETWORKID-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
               ELSE
                 INSPECT LDAP-NETWORKID-I CONVERTING UPPER-CASE
                                                  TO LOWER-CASE
                 PERFORM P40-CHECK-NETWORK-ID THRU P45-EXIT
                 IF  MULTI-EDIT-MSG > SPACES
                   MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-NETWORKID-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
                   GO TO I80-LDAP-OUTPUT
                 END-IF
                 PERFORM Q40-INITKEY-IESLDUM
                 MOVE LDAP-NETWORKID-I TO LDUM-NETUSRID
                 PERFORM Q46-READEQ-IESLDUM
                 IF  EIBRESP = DFHRESP(NORMAL)
                   STRING 'Network id already ' DELIMITED BY SIZE
                          'mapped to '          DELIMITED BY SIZE
                          LDUM-MFUSRID          DELIMITED BY SPACE
                          '.'                   DELIMITED BY SIZE
                                     INTO MULTI-EDIT-MSG
                   MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
                   CALL MULTMESG    USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-NETWORKID-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
                 ELSE
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE LDGA-ATTR-VALUE(2 1) TO WORD1
                   INSPECT WORD1 CONVERTING LOWER-CASE
                                         TO UPPER-CASE
                   IF  WORD1 NOT = UPM1-USRIDNT
                     MOVE LDGA-ATTR-VALUE(2 1) TO UNEX-VARIABLE
                     PERFORM Q202-SET-LENGTH
                     STRING UPM1-USRIDNT        DELIMITED BY SPACE
                            ' does not match AD MFID '
                            '(' UNEX-VARIABLE(1:UNEX-LENG) ')'
                       DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                     MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-NETWORKID-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
                   END-IF
                   IF  LDGA-ATTR-VALUE(3 1) <= spaces
                     STRING UPM1-USRIDNT        DELIMITED BY SPACE
                            ' does not have an AD mail attribute value.'
                       DELIMITED BY SIZE INTO MULTI-EDIT-MSG
                     MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
                     CALL MULTMESG  USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-NETWORKID-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-IF.

           IF  ERRORS-FOUND
               GO TO I80-LDAP-OUTPUT
           END-IF.

           IF  EIBAID = AIDPFK05
               GO TO I40-LDAP-UPDATES
           END-IF.

           GO TO I50-LDAP-DISPLAY.

       I40-LDAP-UPDATES.

           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-RDRQ           TO TRUE.
           MOVE IESLDUMA               TO PRT-PWR-JOBN.
           MOVE 80                     TO PRT-STR-LENG.
           COMPUTE CNT                  = LENGTH OF LDUMA-JCL
                                        / LENGTH OF LDUMA-ENTRY.
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1
               UNTIL SUB > CNT
                  OR LDUMA-ENTRY(SUB) = '+++INPUT+++'
                  OR NOT PRINT-COMPLETED
             MOVE LDUMA-ENTRY(SUB)     TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  PRINT-COMPLETED
           AND LDUMA-ENTRY(SUB) = '+++INPUT+++'
               MOVE SPACES             TO PRT-STR-DATA
               MOVE 1                  TO POS
               STRING 'ADD LDAPUSER='''   DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
               IF  LDAP-AUTHTYPE-I = '1'
                 MOVE 1                TO UNEX-LENG
                 STRING LDAP-NETWORKID-I  LDAP-NETWORK2-I
                  DELIMITED BY SPACE INTO UNEX-VARIABLE
                                     WITH POINTER UNEX-LENG
                 SUBTRACT 1          FROM UNEX-LENG
                 INSPECT UNEX-VARIABLE(1:UNEX-LENG)
                                          REPLACING ALL LOW-VALUES
                                                     BY SPACES
                 PERFORM R10-FIX-APOSTROPHE THRU R15-EXIT
                 STRING UNEX-VARIABLE(1:UNEX-LENG)
                        ''' TYPE=LDAP '   DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
               ELSE
                 STRING LDAP-USERID-I     DELIMITED BY SPACE
                        ''' TYPE=VSE '    DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
               END-IF
               INSPECT LDAP-DESCRIPTION-I REPLACING ALL LOW-VALUES
                                                     BY SPACES
               MOVE LDAP-DESCRIPTION-I TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               PERFORM R10-FIX-APOSTROPHE THRU R15-EXIT
               STRING 'DESC=''' UNEX-VARIABLE(1:UNEX-LENG)
                      ''''                DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
               IF  LDAP-AUTHTYPE-I = '1'
                 STRING ' -'              DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
               END-IF
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               IF  LDAP-AUTHTYPE-I = '1'
                 MOVE SPACES           TO PRT-STR-DATA
                 MOVE 5                TO POS
                 STRING 'VSEUSER='''      DELIMITED BY SIZE
                        LDAP-USERID-I     DELIMITED BY SPACE
                        ''' '             DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
                 IF  LDAP-GENPWD-I = '1'
                   STRING 'OLDPWD='''     DELIMITED BY SIZE
                          LDAP-PASSWORD-I DELIMITED BY SPACE
                          ''' GENPWD '
                          'PWDPATTERN=''' DELIMITED BY SIZE
                          LDAP-PATTERN-I  DELIMITED BY SPACE
                          ''''            DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
                 ELSE
                   STRING 'VSEPWD='''     DELIMITED BY SIZE
                          LDAP-PASSWORD-I DELIMITED BY SPACE
                          ''''            DELIMITED BY SIZE
                                     INTO PRT-STR-DATA
                                     WITH POINTER POS
                 END-IF
                 PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               END-IF
               ADD  1                  TO SUB
           END-IF.

           PERFORM WITH TEST BEFORE
             VARYING SUB FROM SUB BY 1
               UNTIL SUB > CNT
                  OR NOT PRINT-COMPLETED
             SET  TXT-REQUEST-FIND     TO TRUE
             MOVE ZEROES               TO TXT-PNTR
             MOVE 5                    TO TXT-PARM-STRLEN
             MOVE '&MFID'              TO TXT-PARM-STRING
             MOVE 80                   TO TXT-MSTR-BUFLEN
             MOVE LDUMA-ENTRY(SUB)     TO TXT-MSTR-BUFFER
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
               MOVE LDAP-USERID-I      TO TXT-PARM-STRING
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
               MOVE 'LDAP submit failed.  See log.'
                                       TO MULTI-EDIT-MSG
               MOVE LENGTH OF LDAP-MESSAGE-O
                                       TO LDAP-MESSAGE-L
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LDAP-AUTHTYPE-L
                                          LDAP-MESSAGE-L
                                          MULTI-EDIT-MSG
               GO TO I80-LDAP-OUTPUT
           END-IF.

           MOVE COMM-CURRRTN           TO COMM-SAVE-FUNC.
           MOVE 'RETN'                 TO COMM-CURRRTN.
           MOVE VUPM-CNTL-KEY          TO VUPM-SAVEKEY.
           GO TO C00-LIST-ROUTINE.

       I50-LDAP-DISPLAY.

           MOVE 'LDAP'                 TO COMM-CURRRTN.

       I60-LDAP-DETAIL.

           IF  VUPM-SAVEKEY > SPACES
               MOVE VUPM-SAVEKEY       TO VUPM-CNTL-KEY
               PERFORM Q20-INITKEY-IESCNTL
               PERFORM Q26-READEQ-IESCNTL
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO I70-LDAP-ERRORS
               END-IF

               MOVE SPACES             TO VUPM-SAVEKEY
               PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
               MOVE UPM1-USRIDNT       TO LDAP-USERID-O
               MOVE UPM1-PASSWRD       TO LDAP-PASSWORD-O
               MOVE '1'                TO LDAP-AUTHTYPE-O
               MOVE '2'                TO LDAP-GENPWD-O
               MOVE 'caaaaaaa'         TO LDAP-PATTERN-O
               MOVE UPM1-USRNAME       TO LDAP-DESCRIPTION-O
               MOVE SPACES             TO LDAP-NETWORKID-O
                                          LDAP-NETWORK2-O

               EVALUATE LDAP-PREVRTN
               WHEN 'UPM1'
                   CONTINUE
               WHEN 'UPM2'
                   EXEC CICS SEND
                             MAP      (UPM2XXX)
                             MAPONLY
                             MAPSET   (VUPMMS)
                             TERMINAL
                   END-EXEC
               WHEN 'UPM3'
                   EXEC CICS SEND
                             MAP      (UPM3XXX)
                             MAPONLY
                             MAPSET   (VUPMMS)
                             TERMINAL
                   END-EXEC
               WHEN 'UPM4'
                   EXEC CICS SEND
                             MAP      (UPM4XXX)
                             MAPONLY
                             MAPSET   (VUPMMS)
                             TERMINAL
                   END-EXEC
               WHEN OTHER
                   EXEC CICS SEND
                             MAP      (LISTXXX)
                             MAPONLY
                             MAPSET   (VUPMMS)
                             TERMINAL
                   END-EXEC
               END-EVALUATE
           END-IF.

       I70-LDAP-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO LDAP-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO LDAP-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       I80-LDAP-OUTPUT.

       I85-LDAP-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (LDAPWIN)
                             DATAONLY
                             FROM     (LDAPWINO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR
                   END-EXEC
               ELSE
                   EXEC CICS SEND
                             MAP      (LDAPWIN)
                             DATAONLY
                             FROM     (LDAPWINO)
                             MAPSET   (VUPMMS)
                             TERMINAL
                             FREEKB
                             ALARM
                             CURSOR   (EIBCPOSN)
                   END-EXEC
               END-IF
           ELSE
               EXEC CICS SEND
                         MAP      (LDAPWIN)
                         FROM     (LDAPWINO)
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
               END-EXEC
           END-IF.

       I90-LDAP-RETURN.

           EXEC CICS FREEMAIN
                     DATA     (LDAPWINI)
           END-EXEC.

           GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    SUBMIT ROUTINE                                              *
      ******************************************************************
       J00-SUBM-ROUTINE.

           MOVE 'Generating...  '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

           EXEC CICS LOAD
                     PROGRAM(VUPMSKEL)
                     SET(ADDRESS OF VUPMSKEL-JCL-AREA)
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  NOT VUPMSKEL-LOADED-OK
               IF  EIBRESP = DFHRESP(NORMAL)
                   MOVE -2             TO ERROR-SWITCH
                   MOVE 'DFHSNT skeleton not aligned properly for use.'
                                       TO DOCU-MESSAGE-O
               END-IF
               GO TO J90-SUBM-RETURN
           END-IF.

       J10-SUBM-STARTBR.

           MOVE LOW-VALUES             TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           PERFORM Q21-STARTBR-IESCNTL.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q23-READNEXT-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO J90-SUBM-RETURN
           END-IF.

           IF  NOT IUI-USER-PROFILE
               PERFORM Q25-ENDBR-IESCNTL
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'No user profile records found.'
                                       TO DOCU-MESSAGE-O
               GO TO J90-SUBM-RETURN
           END-IF.

       J20-SUBM-INITIALIZE.

           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-RDRQ           TO TRUE.
           MOVE 'DFHSNT'               TO PRT-PWR-JOBN.
           MOVE CICS-USERID            TO PRT-PWR-USER.
           MOVE 80                     TO PRT-STR-LENG.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM 1 BY 1
               UNTIL JSUB > SKELJCLC
                  OR SKELCARD(JSUB)(1:7) = '*¦¦¦¦¦¦'
                  OR NOT PRINT-COMPLETED
             MOVE SKELCARD(JSUB)       TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  NOT PRINT-COMPLETED
           OR  JSUB > SKELJCLC
             IF  NOT PRINT-COMPLETED
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'DFHSNT generation failed.  See log.'
                                       TO DOCU-MESSAGE-O
             ELSE
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'DFHSNT skeleton missing data inclusion point.'
                                       TO DOCU-MESSAGE-O
             END-IF
             GO TO J90-SUBM-RETURN
           END-IF.

           STRING '*¦¦¦¦¦¦ IUPM SOURCE REQUESTED BY '
                  PRT-PWR-USER            DELIMITED BY SIZE
                                     INTO PRT-STR-DATA.
           PERFORM P00-CALL-PRTMAN   THRU P05-EXIT.
           ADD  1                      TO JSUB.

       J30-SUBM-PROCESS.

           PERFORM WITH TEST BEFORE
             UNTIL NOT IUI-USER-PROFILE
                OR NOT PRINT-COMPLETED
                OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
             PERFORM P80-IMPORT-RECORD-DATA THRU P85-EXIT

             IF  LIST-EXPIRED NOT = '*'
             AND (IUI-US-REVOKEJ = ZEROES OR
                 (IUI-US-REVOKEJ + 100000) > EIBDATE)
             AND NOT (IS-ICCF-USER AND NAME-LENGTH = 4)

               IF  UPM1-USRNAME NOT > SPACES
                 MOVE UPM1-USRIDNT     TO UPM1-USRNAME
               END-IF
               MOVE UPM1-USRNAME       TO UNEX-VARIABLE
               PERFORM Q202-SET-LENGTH
               INSPECT UPM1-USRNAME(1:UNEX-LENG)
                 CONVERTING LOWER-CASE TO UPPER-CASE

      * start a new user entry
               MOVE 9                  TO PTR
               STRING 'DFHSNT TYPE=ENTRY,'
                      'OPNAME=''' UPM1-USRNAME(1:UNEX-LENG) ''','
                                          DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR

               PERFORM P10-SET-CONTINUATION
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT

               MOVE 16                 TO PTR
               STRING 'USERID=' UPM1-USRIDNT ','  DELIMITED BY SPACE
                      'PASSWRD=' UPM1-PASSWRD ',' DELIMITED BY SPACE
                   INTO PRT-STR-DATA WITH POINTER PTR

               PERFORM P10-SET-CONTINUATION
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT

               MOVE 16                 TO PTR
               STRING 'OPIDENT=' UPM1-OPRIDNT ',' DELIMITED BY SPACE
                      'OPPRTY=' UPM3-OPERPRI ','  DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR

               IF  UPM3-TIMEOUT NOT = '00'
                 STRING 'TIMEOUT=' UPM3-TIMEOUT ',' DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
               END-IF

      * add operator class(es)
               STRING 'OPCLASS=('         DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR

               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > 24
                 IF  UPM4-OPCLASS(IDX) = 'X'
                   IF  PTR > (72 - 3)
                     PERFORM P10-SET-CONTINUATION
                     PERFORM P00-CALL-PRTMAN THRU P05-EXIT
                     MOVE 16           TO PTR
                   END-IF
                   MOVE IDX            TO NUM-WORK
                   STRING NUM-WRK2 ','    DELIMITED BY SIZE
                     INTO PRT-STR-DATA WITH POINTER PTR
                 END-IF
               END-PERFORM

               IF  PTR > (72 - 2 + 1)
                 PERFORM P10-SET-CONTINUATION
                 PERFORM P00-CALL-PRTMAN THRU P05-EXIT
                 MOVE 16               TO PTR
               ELSE
                 SUBTRACT 1          FROM PTR
               END-IF

               STRING '),'                DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR

               PERFORM P10-SET-CONTINUATION
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT

      * add security key(s)
               MOVE 16                 TO PTR
               STRING 'SCTYKEY=('         DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR

               PERFORM WITH TEST BEFORE
                 VARYING IDX FROM 1 BY 1
                   UNTIL IDX > 64
                 IF  UPM3-TRANSEC(IDX) = 'X'
                   IF  PTR > (72 - 3)
                     PERFORM P10-SET-CONTINUATION
                     PERFORM P00-CALL-PRTMAN THRU P05-EXIT
                     MOVE 16           TO PTR
                   END-IF
                   MOVE IDX            TO NUM-WORK
                   STRING NUM-WRK2 ','    DELIMITED BY SIZE
                     INTO PRT-STR-DATA WITH POINTER PTR
                 END-IF
               END-PERFORM

               IF  PTR > (72 - 1 + 1)
                 PERFORM P10-SET-CONTINUATION
                 PERFORM P00-CALL-PRTMAN THRU P05-EXIT
                 MOVE 16               TO PTR
               ELSE
                 SUBTRACT 1          FROM PTR
               END-IF

               STRING ')'                 DELIMITED BY SIZE
                   INTO PRT-STR-DATA WITH POINTER PTR
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT

             END-IF

             IF  EIBRESP = DFHRESP(NORMAL)
                 PERFORM Q23-READNEXT-IESCNTL
             END-IF

           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(LENGERR)
                         AND DFHRESP(ENDFILE)
               GO TO J90-SUBM-RETURN
           END-IF.

           PERFORM Q25-ENDBR-IESCNTL.

       J80-SUBM-COMPLETE.

           PERFORM WITH TEST BEFORE
             VARYING JSUB FROM JSUB BY 1
               UNTIL JSUB > SKELJCLC
                  OR NOT PRINT-COMPLETED
             MOVE SKELCARD(JSUB)       TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  NOT PRINT-COMPLETED
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'DFHSNT generation failed.  See log.'
                                       TO DOCU-MESSAGE-O
               GO TO J90-SUBM-RETURN
           END-IF.

           EXEC CICS RELEASE
                     PROGRAM(VUPMSKEL)
                     NOHANDLE
           END-EXEC.

           MOVE SPACES                 TO VUPM-CNTL-KEY.
           MOVE 'DFHSNT table generated and submitted for assembly.'
                                       TO DOCU-MESSAGE-O.
       J90-SUBM-RETURN.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM H70-DOCU-ERRORS
           END-IF.

           GO TO H80-DOCU-OUTPUT.

/DLC4//*****************************************************************
  |   *    TEST MAPPING ROUTINE                                        *
  |   ******************************************************************
  v    K00-TEST-ROUTINE.

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
                   MOVE VUPM-SAVE-TIOA TO TESTWINO
                   MOVE LOW-VALUES     TO VUPM-SAVE-TIOA
               END-IF
               EXEC CICS SEND
                         MAP      (LISTXXX)
                         MAPONLY
                         MAPSET   (VUPMMS)
                         TERMINAL
               END-EXEC
               PERFORM K80-TEST-OUTPUT THRU K85-TEST-SENDMAP
               GO TO K50-TEST-DISPLAY
           END-IF.

       K10-TEST-RECEIVE.

           EXEC CICS RECEIVE
                     MAP      (TESTWIN)
                     INTO     (TESTWINI)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     NOHANDLE
           END-EXEC.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(MAPFAIL)
               GO TO K70-TEST-ERRORS
           END-IF.

       K20-TEST-KEYS.

           EVALUATE EIBAID
           WHEN AIDCLEAR
               MOVE JOBSSELT           TO COMM-LINKPGM
               GO TO Z90-VUPM-TERMINATION

           WHEN AIDPFK03
               MOVE COMM-CURRRTN       TO COMM-SAVE-FUNC
               MOVE 'RETN'             TO COMM-CURRRTN
               GO TO C00-LIST-ROUTINE

           WHEN AIDPFK01
      *    ... save current screen data content in commarea ...
               MOVE TESTWINI           TO VUPM-SAVE-TIOA
               GO TO T50-HELP-TRANSFER

           WHEN AIDPFK04
               GO TO Z90-VUPM-TERMINATION

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
               GO TO K80-TEST-OUTPUT

           WHEN OTHER
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO TEST-MESSAGE-H
               MOVE 'No current action assigned to function key.'
                                       TO TEST-MESSAGE-O
               GO TO K80-TEST-OUTPUT
           END-EVALUATE.

       K50-TEST-DISPLAY.

           MOVE 'TEST'                 TO COMM-CURRRTN.

       K60-TEST-DETAIL.

           MOVE VUPM-SAVEKEY           TO SCRTY-USERID.
           MOVE TEST-COMPANY-I         TO SCRTY-CONUM.
           EXEC CICS LINK
                     PROGRAM(WINSCRTY)
                     COMMAREA(WINSCRTY-PARMS)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO K70-TEST-ERRORS
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

       K70-TEST-ERRORS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               MOVE -2                 TO ERROR-SWITCH
               MOVE EXHREVRS           TO TEST-MESSAGE-H
               PERFORM X00-UNEX-ERR
               MOVE UNEX-MSG           TO TEST-MESSAGE-O
               EXEC CICS SYNCPOINT ROLLBACK END-EXEC
           END-IF.

       K80-TEST-OUTPUT.

       K85-TEST-SENDMAP.

           IF  ERRORS-FOUND
               IF  ERROR-AT-CURSOR
                   EXEC CICS SEND
                             MAP      (TESTWIN)
                             DATAONLY
                             FROM     (TESTWINO)
                             MAPSET   (VUPMMS)
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
                             MAPSET   (VUPMMS)
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
                         MAPSET   (VUPMMS)
                         TERMINAL
                         FREEKB
               END-EXEC
           END-IF.

       K90-TEST-RETURN.

           EXEC CICS FREEMAIN
  ^                  DATA     (TESTWINI)
  |        END-EXEC.
  |
/DLC4/     GO TO Z80-VUPM-RETURN.

      /*****************************************************************
      *    LOAD REPORT ROUTINE                                         *
      ******************************************************************
       L00-LOAD-ROUTINE.

           MOVE 'Generating...  '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

           PERFORM Q30-INIT-TS.

       L10-LOAD-STARTBR.

           MOVE LOW-VALUES             TO VUPM-CNTL-KEY.
           PERFORM Q20-INITKEY-IESCNTL.
           PERFORM Q21-STARTBR-IESCNTL.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q23-READNEXT-IESCNTL
           END-IF.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO L90-LOAD-RETURN
           END-IF.

           IF  NOT IUI-USER-PROFILE
               PERFORM Q25-ENDBR-IESCNTL
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'No user profile records found.'
                                       TO DOCU-MESSAGE-O
               GO TO L90-LOAD-RETURN
           END-IF.

       L20-LOAD-DELETE.

           PERFORM Q35-DELETEQ-TS.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO L90-LOAD-RETURN
           END-IF.

       L30-LOAD-PROCESS.

           PERFORM WITH TEST BEFORE
             UNTIL NOT IUI-USER-PROFILE
                OR NOT PRINT-COMPLETED
                OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
             PERFORM P80-IMPORT-RECORD-DATA THRU P85-EXIT

             SET  HEX-DECODE-FROM-CHAR TO TRUE
             MOVE 8                    TO HEX-LENG
             MOVE IUI-US-CICSSEC       TO HEX-CHRT
             CALL 'HEXMAN'          USING HEXMAN-PARMS

             EVALUATE VUPM-RPTSEQU
             WHEN 1
                 MOVE UPM1-USRIDNT     TO SORTUSER IN SORTUID
                 MOVE UPM1-USRNAME     TO SORTDESC IN SORTUID
                 MOVE UPM1-OPRIDNT     TO SORTOPID IN SORTUID
                 MOVE LIST-EXPIRES     TO SORTEXPS IN SORTUID
                 MOVE LIST-EXPIRED     TO SORTEXPD IN SORTUID
                 MOVE UPM1-REVOKED     TO SORTREVK IN SORTUID
                 MOVE LIST-SIGNON      TO SORTLSGN IN SORTUID
                 MOVE UPM1-USRTYPE     TO SORTUTYP IN SORTUID
                 MOVE UPM1-INITNAM     TO SORTINAM IN SORTUID
                 MOVE UPM1-INITYPE     TO SORTITYP IN SORTUID
                 MOVE HEX-HEXT         TO SORTCICS IN SORTUID
             WHEN 2
                 MOVE UPM1-USRNAME     TO SORTDESC IN SORTUNM
                 MOVE UPM1-USRIDNT     TO SORTUSER IN SORTUNM
                 MOVE UPM1-OPRIDNT     TO SORTOPID IN SORTUNM
                 MOVE LIST-EXPIRES     TO SORTEXPS IN SORTUNM
                 MOVE LIST-EXPIRED     TO SORTEXPD IN SORTUNM
                 MOVE UPM1-REVOKED     TO SORTREVK IN SORTUNM
                 MOVE LIST-SIGNON      TO SORTLSGN IN SORTUNM
                 MOVE UPM1-USRTYPE     TO SORTUTYP IN SORTUNM
                 MOVE UPM1-INITNAM     TO SORTINAM IN SORTUNM
                 MOVE UPM1-INITYPE     TO SORTITYP IN SORTUNM
                 MOVE HEX-HEXT         TO SORTCICS IN SORTUNM
             WHEN 3
                 MOVE UPM1-USRTYPE     TO SORTUTYP IN SORTUTP
                 MOVE UPM1-USRIDNT     TO SORTUSER IN SORTUTP
                 MOVE UPM1-USRNAME     TO SORTDESC IN SORTUTP
                 MOVE UPM1-OPRIDNT     TO SORTOPID IN SORTUTP
                 MOVE LIST-EXPIRES     TO SORTEXPS IN SORTUTP
                 MOVE LIST-EXPIRED     TO SORTEXPD IN SORTUTP
                 MOVE UPM1-REVOKED     TO SORTREVK IN SORTUTP
                 MOVE LIST-SIGNON      TO SORTLSGN IN SORTUTP
                 MOVE UPM1-INITNAM     TO SORTINAM IN SORTUTP
                 MOVE UPM1-INITYPE     TO SORTITYP IN SORTUTP
                 MOVE HEX-HEXT         TO SORTCICS IN SORTUTP
             WHEN 4
                 MOVE UPM1-INITNAM     TO SORTINAM IN SORTINM
                 MOVE UPM1-INITYPE     TO SORTITYP IN SORTINM
                 MOVE UPM1-USRIDNT     TO SORTUSER IN SORTINM
                 MOVE UPM1-USRNAME     TO SORTDESC IN SORTINM
                 MOVE UPM1-OPRIDNT     TO SORTOPID IN SORTINM
                 MOVE LIST-EXPIRES     TO SORTEXPS IN SORTINM
                 MOVE LIST-EXPIRED     TO SORTEXPD IN SORTINM
                 MOVE UPM1-REVOKED     TO SORTREVK IN SORTINM
                 MOVE LIST-SIGNON      TO SORTLSGN IN SORTINM
                 MOVE UPM1-USRTYPE     TO SORTUTYP IN SORTINM
                 MOVE HEX-HEXT         TO SORTCICS IN SORTINM
/DLC6/       WHEN 5
/DLC6/           MOVE SPACES           TO SORTREC
/DLC6/           IF (LIST-EXPIRED NOT = '*' AND 'V')
/DLC6/           AND (IUI-US-REVOKEJ = ZEROES OR
/DLC6/               (IUI-US-REVOKEJ + 100000) > EIBDATE)
/DLC6/           AND NOT (IS-ICCF-USER AND NAME-LENGTH <= 4)
/DLC6/             MOVE UPM1-USRNAME   TO SORTDESC IN SORTWSF
/DLC6/             MOVE UPM1-USRIDNT   TO SORTUSER IN SORTWSF
/DLC6/                                    SCRTY-USERID
/DLC6/             MOVE SPACES         TO SCRTY-CONUM
/DLC6/             EXEC CICS LINK
/DLC6/                       PROGRAM(WINSCRTY)
/DLC6/                       COMMAREA(WINSCRTY-PARMS)
/DLC6/             END-EXEC
/DLC6/             MOVE LIST-SIGNON    TO SORTSIGN IN SORTWSF
/DLC6/             MOVE SCRTY-CONUM    TO SORTCOMP IN SORTWSF
/DLC6/             MOVE SCRTY-ACCESS   TO SORTACCS IN SORTWSF
/DLC6/             MOVE SCRTY-SVCRGN   TO SORTSRGN IN SORTWSF
/DLC6/             MOVE SCRTY-ALTRGN   TO SORTARGN IN SORTWSF
/DLC6/             MOVE SCRTY-RPTRMT   TO SORTRRMT IN SORTWSF
/DLC6/             MOVE SCRTY-ALTRMT   TO SORTARMT IN SORTWSF
/DLC6/             MOVE SCRTY-DFTPTR   TO SORTDFTP IN SORTWSF
/DLC6/             MOVE SCRTY-WINFUNC  TO SORTFUNC IN SORTWSF
/DLC6/             MOVE SCRTY-WINUNIT  TO SORTUNIT IN SORTWSF
/DLC6/             MOVE SCRTY-WINORG   TO SORTWORG IN SORTWSF
/DLC6/             MOVE SCRTY-WINPOS   TO SORTWPOS IN SORTWSF
/DLC6/             MOVE SCRTY-WINDIR   TO SORTWDIR IN SORTWSF
/DLC6/             MOVE SCRTY-OLDDAP   TO SORTODAP IN SORTWSF
/DLC6/             MOVE SCRTY-OLDCLS   TO SORTOCLS IN SORTWSF
/DLC6/             MOVE SCRTY-OLDGRP   TO SORTOGRP IN SORTWSF
/DLC6/             MOVE SCRTY-OLDLVL   TO SORTOLVL IN SORTWSF
/DLC6/           END-IF
/DLC6/       WHEN 6
/DLC6/           MOVE SPACES           TO SORTREC
/DLC6/           IF (LIST-EXPIRED NOT = '*' AND 'V')
/DLC6/           AND (IUI-US-REVOKEJ = ZEROES OR
/DLC6/               (IUI-US-REVOKEJ + 100000) > EIBDATE)
/DLC6/           AND NOT (IS-ICCF-USER AND NAME-LENGTH <= 4)
/DLC6/             MOVE UPM1-USRNAME   TO SORTDESC IN SORTWSG
/DLC6/             MOVE UPM1-USRIDNT   TO SORTUSER IN SORTWSG
/DLC6/                                    SCRTY-USERID
/DLC6/             MOVE SPACES         TO SCRTY-CONUM
/DLC6/             EXEC CICS LINK
/DLC6/                       PROGRAM(WINSCRTY)
/DLC6/                       COMMAREA(WINSCRTY-PARMS)
/DLC6/             END-EXEC
/DLC6/             MOVE LIST-SIGNON    TO SORTSIGN IN SORTWSG
/DLC6/             MOVE SCRTY-CONUM    TO SORTCOMP IN SORTWSG
/DLC6/             MOVE SCRTY-ACCESS   TO SORTACCS IN SORTWSG
/DLC6/             MOVE SCRTY-SVCRGN   TO SORTSRGN IN SORTWSG
/DLC6/             MOVE SCRTY-ALTRGN   TO SORTARGN IN SORTWSG
/DLC6/             MOVE SCRTY-RPTRMT   TO SORTRRMT IN SORTWSG
/DLC6/             MOVE SCRTY-ALTRMT   TO SORTARMT IN SORTWSG
/DLC6/             MOVE SCRTY-DFTPTR   TO SORTDFTP IN SORTWSG
/DLC6/             MOVE SCRTY-WINFUNC  TO SORTFUNC IN SORTWSG
/DLC6/             MOVE SCRTY-WINUNIT  TO SORTUNIT IN SORTWSG
/DLC6/             MOVE SCRTY-WINORG   TO SORTWORG IN SORTWSG
/DLC6/             MOVE SCRTY-WINPOS   TO SORTWPOS IN SORTWSG
/DLC6/             MOVE SCRTY-WINDIR   TO SORTWDIR IN SORTWSG
/DLC6/             MOVE SCRTY-OLDDAP   TO SORTODAP IN SORTWSG
/DLC6/             MOVE SCRTY-OLDCLS   TO SORTOCLS IN SORTWSG
/DLC6/             MOVE SCRTY-OLDGRP   TO SORTOGRP IN SORTWSG
/DLC6/             MOVE SCRTY-OLDLVL   TO SORTOLVL IN SORTWSG
/DLC6/           END-IF
             END-EVALUATE

             SET  HEX-DECODE-FROM-CHAR TO TRUE
             MOVE 3                    TO HEX-LENG
             MOVE IUI-US-OPERCLS       TO HEX-CHRT
             CALL 'HEXMAN'          USING HEXMAN-PARMS

             EVALUATE VUPM-RPTSEQU
             WHEN 1
               MOVE HEX-HEXT           TO SORTOPER IN SORTUID
             WHEN 2
               MOVE HEX-HEXT           TO SORTOPER IN SORTUNM
             WHEN 3
               MOVE HEX-HEXT           TO SORTOPER IN SORTUTP
             WHEN 4
               MOVE HEX-HEXT           TO SORTOPER IN SORTINM
             END-EVALUATE

/DLC6/       IF  SORTREC > SPACES
               PERFORM Q33-WRITEQ-TS
/DLC6/       END-IF

             IF  EIBRESP = DFHRESP(NORMAL)
                 PERFORM Q23-READNEXT-IESCNTL
             END-IF

           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(LENGERR)
                         AND DFHRESP(ENDFILE)
               GO TO L90-LOAD-RETURN
           END-IF.

           PERFORM Q25-ENDBR-IESCNTL.

       L40-LOAD-SORTDATA.

           PERFORM Q31-NUMITEMS-TS.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
           OR  TS-TOTL NOT > ZERO
               IF  EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO L90-LOAD-RETURN
               END-IF
               PERFORM Q35-DELETEQ-TS
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'Temp storage queue is empty.'
                                       TO DOCU-MESSAGE-O
               GO TO L90-LOAD-RETURN
           END-IF.

           IF  VUPM-RPTSEQU = 1
               GO TO L50-LOAD-INITIALIZE
           END-IF.

           MOVE 'Sorting...     '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

      * setup to call CICSSORT
           MOVE TS-QUEUE               TO CICSSORT-QNAME.
           MOVE TS-TOTL                TO CICSSORT-RECCNT.
           MOVE LENGTH OF SORTREC      TO CICSSORT-RECLEN.
           EVALUATE VUPM-RPTSEQU
           WHEN 1
               MOVE 0                  TO CICSSORT-KEYOFF
               MOVE 8                  TO CICSSORT-KEYLEN
           WHEN 2
               MOVE 0                  TO CICSSORT-KEYOFF
               MOVE 20                 TO CICSSORT-KEYLEN
           WHEN 3
               MOVE 0                  TO CICSSORT-KEYOFF
               MOVE 9                  TO CICSSORT-KEYLEN
           WHEN 4
               MOVE 0                  TO CICSSORT-KEYOFF
               MOVE 17                 TO CICSSORT-KEYLEN
/DLC6/     WHEN 5
/DLC6/         MOVE 0                  TO CICSSORT-KEYOFF
/DLC6/         MOVE 21                 TO CICSSORT-KEYLEN
/DLC6/     WHEN 6
/DLC6/         MOVE 0                  TO CICSSORT-KEYOFF
/DLC6/         MOVE 23                 TO CICSSORT-KEYLEN
           END-EVALUATE
           SET CICSSORT-ASCENDING      TO TRUE.

           EXEC CICS LINK
                     PROGRAM  (CICSSORT)
                     COMMAREA (CICSSORT-PARMS)
           END-EXEC.

      * delete the old queue
           PERFORM Q35-DELETEQ-TS.

      * check if sort completed
           IF  CICSSORT-RC NOT = ZEROES
               MOVE SPACES             TO DOCU-MESSAGE-O
               MOVE CICSSORT-RC        TO NUM-WORK
               MOVE -2                 TO ERROR-SWITCH
               STRING 'Report sort failure, rc=' NUM-WRK2 '.'
                 DELIMITED BY SIZE   INTO DOCU-MESSAGE-O
               GO TO L90-LOAD-RETURN
           END-IF.

      * save new queue name
           MOVE CICSSORT-QNAME         TO TS-QUEUE.
           PERFORM Q31-NUMITEMS-TS.

       L50-LOAD-INITIALIZE.

           MOVE 'Printing...    '      TO POPUP-TEXT.
           PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-LSTQ           TO TRUE.
           MOVE 'L813'                 TO PRT-PWR-FNO.
           MOVE RPTTOKEN(VUPM-RPTSEQU) TO PRT-PWR-JOBN.
           MOVE CICS-USERID            TO PRT-PWR-USER.
           EXEC CICS ASSIGN NETNAME(PRT-PWR-UINF) END-EXEC.
           MOVE 10                     TO PTR.
           STRING CICS-OPID               DELIMITED BY SIZE
               INTO PRT-PWR-UINF     WITH POINTER PTR.
           MOVE 132                    TO PRT-STR-LENG.

           MOVE CICS-APPLID            TO HEADAPPL(VUPM-RPTSEQU).
           MOVE CICS-USERID            TO HEADUSER(VUPM-RPTSEQU).
           MOVE WS-WRKDATE             TO HEADDATE(VUPM-RPTSEQU).
           MOVE WS-WRKTIME             TO HEADTIME(VUPM-RPTSEQU).

       L60-LOAD-OUTPUT.

           PERFORM WITH TEST BEFORE
             VARYING SS-ITEM FROM 1 BY 1
               UNTIL SS-ITEM > TS-TOTL
                  OR NOT PRINT-COMPLETED
                  OR EIBRESP NOT = DFHRESP(NORMAL)

             PERFORM Q32-READQ-TS
             IF  EIBRESP = DFHRESP(NORMAL)

               IF  LINE-CNT >= 60
                 IF  PAGE-CNT > ZERO
                   MOVE PAGE-CNT       TO DOCUPAGE
                   MOVE DOCUTAIL(1:1)  TO PRT-LNK-REQU
                   MOVE DOCUTAIL(2:)   TO PRT-STR-DATA
                   PERFORM L70-LOAD-PRINT-IT
                 END-IF
                 PERFORM WITH TEST BEFORE
                   VARYING CNT FROM 1 BY 1
                     UNTIL CNT > 5
                        OR NOT PRINT-COMPLETED
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   MOVE DOCUHDRE(VUPM-RPTSEQU CNT)(1:1)
                                       TO PRT-LNK-REQU
                   MOVE DOCUHDRE(VUPM-RPTSEQU CNT)(2:)
                                       TO PRT-STR-DATA
                   PERFORM L70-LOAD-PRINT-IT
                 END-PERFORM
                 ADD  1                TO PAGE-CNT
               END-IF

               EVALUATE VUPM-RPTSEQU
               WHEN 1
                   MOVE CORR SORTUID   TO USERLINE
               WHEN 2
                   MOVE CORR SORTUNM   TO DESCLINE
               WHEN 3
                   MOVE CORR SORTUTP   TO TYPELINE
               WHEN 4
                   MOVE CORR SORTINM   TO INITLINE
/DLC6/         WHEN 5
/DLC6/             IF  SORTFUNC IN SORTWSF > SPACES
/DLC6/             AND SORTFUNC IN SORTWSF NOT = SORTFUNC IN WINSLINE
/DLC6/             OR  SORTFUNC IN SORTWSF = 'WW'
/DLC6/             AND SORTUNIT IN SORTWSF NOT = SORTUNIT IN WINSLINE
/DLC6/               MOVE SPACES       TO PRT-LNK-REQU
/DLC6/                                    PRT-STR-DATA
/DLC6/               PERFORM L70-LOAD-PRINT-IT
/DLC6/             END-IF
/DLC6/             MOVE CORR SORTWSF   TO WINSLINE
/DLC6/         WHEN 6
/DLC6/             IF  SORTOGRP IN SORTWSG > SPACES
/DLC6/             AND SORTOGRP IN SORTWSG NOT = SORTOGRP IN WINSLINE
/DLC6/             OR  SORTOGRP IN SORTWSG = 'DI'
/DLC6/             AND (SORTFUNC IN SORTWSG NOT = SORTFUNC IN WINSLINE
/DLC6/             OR   SORTFUNC IN SORTWSG = 'WW'
/DLC6/             AND  SORTUNIT IN SORTWSG NOT = SORTUNIT IN WINSLINE)
/DLC6/               MOVE SPACES       TO PRT-LNK-REQU
/DLC6/                                    PRT-STR-DATA
/DLC6/               PERFORM L70-LOAD-PRINT-IT
/DLC6/             END-IF
/DLC6/             MOVE CORR SORTWSG   TO WINSLINE
               END-EVALUATE

               MOVE DOCULINE(1:1)      TO PRT-LNK-REQU
               MOVE DOCULINE(2:)       TO PRT-STR-DATA
               PERFORM L70-LOAD-PRINT-IT

             END-IF
           END-PERFORM.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
                         AND DFHRESP(ITEMERR)
               GO TO L90-LOAD-RETURN
           END-IF.

           IF  LINE-CNT < 60
               MOVE SPACES             TO PRT-LNK-REQU
                                          PRT-STR-DATA
               PERFORM WITH TEST BEFORE
                 UNTIL LINE-CNT >= 60
                    OR NOT PRINT-COMPLETED
                   PERFORM L70-LOAD-PRINT-IT
               END-PERFORM
               MOVE PAGE-CNT           TO DOCUPAGE
               MOVE DOCUTAIL(1:1)      TO PRT-LNK-REQU
               MOVE DOCUTAIL(2:)       TO PRT-STR-DATA
               PERFORM L70-LOAD-PRINT-IT
           END-IF.

           GO TO L80-LOAD-COMPLETE.

       L70-LOAD-PRINT-IT.

      * analyze ASA control characters
           EVALUATE PRT-LNK-REQU
           WHEN '1'
               MOVE 1                  TO LINE-CNT
           WHEN '+'
               CONTINUE
           WHEN SPACE
               ADD  1                  TO LINE-CNT
           WHEN '0'
               ADD  2                  TO LINE-CNT
           WHEN '-'
               ADD  3                  TO LINE-CNT
           END-EVALUATE.

      * send print line to POWER queue
           PERFORM P00-CALL-PRTMAN   THRU P05-EXIT.

       L80-LOAD-COMPLETE.

           PERFORM Q35-DELETEQ-TS.

           IF  NOT PRINT-COMPLETED
               SET  PRINT-DELSPOOL     TO TRUE
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               MOVE -2                 TO ERROR-SWITCH
               MOVE 'DFHSNT generation failed.  See log.'
                                       TO DOCU-MESSAGE-O
               GO TO L90-LOAD-RETURN
           END-IF.

           MOVE SPACES                 TO VUPM-CNTL-KEY.
           MOVE SPACES                 TO DOCU-MESSAGE-O.
           STRING 'Report '               DELIMITED BY SIZE
                  PRT-PWR-JOBN            DELIMITED BY SPACE
                  ' generated into LST queue class '
                  PRT-PWR-JCLS
               DELIMITED BY SIZE     INTO DOCU-MESSAGE-O.
       L90-LOAD-RETURN.

           IF  EIBRESP NOT = DFHRESP(NORMAL)
               PERFORM H70-DOCU-ERRORS
           END-IF.

           GO TO H80-DOCU-OUTPUT.

/DLC5//*****************************************************************
/DLC5/*    LOAD REPORT ROUTINE                                         *
/DLC5/******************************************************************
/DLC5/ O00-QUERY-ROUTINE.

/DLC5/     MOVE 'Generating...  '      TO POPUP-TEXT.
/DLC5/     PERFORM Q10-DISPLAY-POPUP THRU Q15-EXIT.

/DLC5/     MOVE SPACE                  TO PRT-LNK-REQU.
/DLC5/     SET  PRT-PWR-RDRQ           TO TRUE.
/DLC5/     MOVE VUSRATTR               TO PRT-PWR-JOBN.
/DLC5/     MOVE 80                     TO PRT-STR-LENG.
/DLC5/     COMPUTE CNT                  = LENGTH OF VATTR-JCL
/DLC5/                                  / LENGTH OF VATTR-ENTRY.

/DLC5/     PERFORM WITH TEST BEFORE
/DLC5/       VARYING SUB FROM 1 BY 1
/DLC5/         UNTIL SUB > CNT
/DLC5/            OR NOT PRINT-COMPLETED
/DLC5/       SET  TXT-REQUEST-FIND     TO TRUE
/DLC5/       MOVE ZEROES               TO TXT-PNTR
/DLC5/       MOVE 5                    TO TXT-PARM-STRLEN
/DLC5/       MOVE '&MFID'              TO TXT-PARM-STRING
/DLC5/       MOVE 80                   TO TXT-MSTR-BUFLEN
/DLC5/       MOVE VATTR-ENTRY(SUB)     TO TXT-MSTR-BUFFER
/DLC5/       CALL 'TXTMAN'          USING TXTMAN-PARMS
/DLC5/                                    TXTMAN-BUFFER
/DLC5/                                    TXTMAN-STRING
/DLC5/       IF  TXT-STRING-WAS-FOUND
/DLC5/         SET  TXT-REQUEST-DELETE TO TRUE
/DLC5/         MOVE SPACES             TO TXT-PARM-STRING
/DLC5/         CALL 'TXTMAN'        USING TXTMAN-PARMS
/DLC5/                                    TXTMAN-BUFFER
/DLC5/                                    TXTMAN-STRING
/DLC5/         SET  TXT-REQUEST-INSERT TO TRUE
/DLC5/         MOVE 8                  TO TXT-PARM-STRLEN
/DLC5/         MOVE VUPM-SAVEKEY       TO TXT-PARM-STRING
/DLC5/         CALL 'TXTMAN'        USING TXTMAN-PARMS
/DLC5/                                    TXTMAN-BUFFER
/DLC5/                                    TXTMAN-STRING
/DLC5/       END-IF
/DLC5/       MOVE TXT-MSTR-BUFFER(1:80)
/DLC5/                                 TO PRT-STR-DATA
/DLC5/       PERFORM P00-CALL-PRTMAN THRU P05-EXIT
/DLC5/     END-PERFORM.

/DLC5/     IF  PRINT-COMPLETED
/DLC5/       SET  PRINT-ENDOFMSG       TO TRUE
/DLC5/       PERFORM P00-CALL-PRTMAN THRU P05-EXIT
/DLC5/     END-IF.

/DLC5/     IF  NOT PRINT-COMPLETED
/DLC5/         MOVE 'Query submit failed.  See log.'
/DLC5/                                 TO MULTI-EDIT-MSG
/DLC5/         MOVE LENGTH OF LIST-MESSAGE-O
/DLC5/                                 TO LIST-MESSAGE-L
/DLC5/         CALL MULTMESG        USING DFHEIBLK MULTMESG
/DLC5/                                    ERROR-SWITCH
/DLC5/                                    LIST-SELECT-L(ROW)
/DLC5/                                    LIST-MESSAGE-L
/DLC5/                                    MULTI-EDIT-MSG
/DLC5/         GO TO C80-LIST-OUTPUT
/DLC5/     END-IF.

/DLC5/ O90-QUERY-RETURN.

/DLC5/     MOVE 'Query submitted.'     TO LIST-MESSAGE-O.
/DLC5/     GO TO C50-LIST-DISPLAY.

      /*****************************************************************
      *    PERFORMED ROUTINES                                          *
      ******************************************************************

       P00-CALL-PRTMAN.
           EXEC CICS LINK
                     PROGRAM  (PRT-PGM-NAME)
                     COMMAREA (PRT-PGM-COMM)
                     LENGTH   (PRT-PGM-COML)
           END-EXEC.
       P02-CHK-COMPLETE.
           IF  PRINT-COMPLETED
               MOVE SPACES             TO PRT-STR-DATA
           ELSE
               MOVE PRT-LNK-RESP       TO UNEX-RESP
               MOVE PRT-LNK-RSP2       TO UNEX-RESP2
               STRING PRT-PGM-NAME ' FAILURE: RC=' PRT-LNK-RETN
                      ', RS=' UNEX-RESP ', R2=' UNEX-RESP2
                   DELIMITED BY SIZE INTO LOGF-MESG
               PERFORM Q100-LOGIT THRU Q199-EXIT
               IF  NOT PRINT-ENDOFMSG
                   SET PRINT-ENDOFMSG  TO TRUE
                   PERFORM P00-CALL-PRTMAN
               END-IF
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       P05-EXIT.
           EXIT.

       P10-SET-CONTINUATION.
           MOVE 72                     TO PTR.
           STRING 'X'                     DELIMITED BY SIZE
               INTO PRT-STR-DATA     WITH POINTER PTR.
       P15-EXIT.
           EXIT.

       P20-DROP-LIST-OPTIONS.
           PERFORM WITH TEST BEFORE
             VARYING IDX FROM 1 BY 1 UNTIL IDX > LISTROWS
               MOVE LOW-VALUES TO LISTMAP-ROW-DETAIL(IDX)(1:6)
           END-PERFORM.
       P25-EXIT.
           EXIT.

       P30-RESET-LDAP-MAPPING.
           MOVE SPACE                  TO PRT-LNK-REQU.
           SET  PRT-PWR-RDRQ           TO TRUE.
           MOVE IESLDUMA               TO PRT-PWR-JOBN.
           MOVE 80                     TO PRT-STR-LENG.
           COMPUTE CNT                  = LENGTH OF LDUMA-JCL
                                        / LENGTH OF LDUMA-ENTRY.
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1
               UNTIL SUB > CNT
                  OR LDUMA-ENTRY(SUB) = '+++INPUT+++'
                  OR NOT PRINT-COMPLETED
             MOVE LDUMA-ENTRY(SUB)     TO PRT-STR-DATA
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-PERFORM.

           IF  LDUMA-ENTRY(SUB) = '+++INPUT+++'
               MOVE SPACES             TO PRT-STR-DATA
               MOVE 1                  TO POS
               MOVE 1                  TO UNEX-LENG
               STRING LDUM-NETUSRID
                 DELIMITED BY SPACE  INTO UNEX-VARIABLE
                                     WITH POINTER UNEX-LENG
               SUBTRACT 1            FROM UNEX-LENG
               PERFORM R10-FIX-APOSTROPHE THRU R15-EXIT
               STRING 'DELETE LDAPUSER='''
                      UNEX-VARIABLE(1:UNEX-LENG)
                      ''''
                 DELIMITED BY SIZE   INTO PRT-STR-DATA
                                     WITH POINTER POS
               PERFORM P00-CALL-PRTMAN THRU P05-EXIT
               ADD  1                  TO SUB
           END-IF.

           PERFORM WITH TEST BEFORE
             VARYING SUB FROM SUB BY 1
               UNTIL SUB > CNT
                  OR NOT PRINT-COMPLETED
             SET  TXT-REQUEST-FIND     TO TRUE
             MOVE ZEROES               TO TXT-PNTR
             MOVE 5                    TO TXT-PARM-STRLEN
             MOVE '&MFID'              TO TXT-PARM-STRING
             MOVE 80                   TO TXT-MSTR-BUFLEN
             MOVE LDUMA-ENTRY(SUB)     TO TXT-MSTR-BUFFER
             CALL 'TXTMAN'          USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
             IF  NOT TXT-STRING-WAS-FOUND
                 MOVE LDUMA-ENTRY(SUB) TO PRT-STR-DATA
                 PERFORM P00-CALL-PRTMAN THRU P05-EXIT
             END-IF
           END-PERFORM.

           IF  PRINT-COMPLETED
             SET  PRINT-ENDOFMSG       TO TRUE
             PERFORM P00-CALL-PRTMAN THRU P05-EXIT
           END-IF.

           IF  NOT PRINT-COMPLETED
               MOVE 'LDAP reset failed.  See log.'
                                       TO MULTI-EDIT-MSG
               CALL MULTMESG        USING DFHEIBLK MULTMESG
                                          ERROR-SWITCH
                                          LIST-SELECT-L(IDX)
                                          LIST-MESSAGE-L
                                          MULTI-EDIT-MSG
           END-IF.
       P35-EXIT.
           EXIT.

       P40-CHECK-NETWORK-ID.
      * this code looks up LDAP attributes for a network user; but,
      * when you don't know the network user id, use the IESLDUM file
      * to translate a vse user id back into a network user id
           INITIALIZE IESLDGA-COMMAREA.
           MOVE LENGTH OF IESLDGA-COMMAREA
                                       TO LDGA-AREA-LENGTH.
           MOVE LDAP-NETWORKID-I       TO LDGA-USER-ID.
           MOVE '&(objectClass=person)(objectClass=user)'
                                       TO LDGA-SEARCH-FILTER.
           MOVE 3                      TO LDGA-ATTR-COUNT.

           MOVE 'displayName'          TO LDGA-ATTR-NAME(1).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(1).
           MOVE 8                      TO LDGA-VALUE-COUNT(1).

           MOVE 'mfid'                 TO LDGA-ATTR-NAME(2).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(2).
           MOVE 8                      TO LDGA-VALUE-COUNT(2).

           MOVE 'mail'                 TO LDGA-ATTR-NAME(3).
           MOVE LENGTH OF LDGA-ATTR-VALUE
                                       TO LDGA-VALUE-LENGTH(3).
           MOVE 8                      TO LDGA-VALUE-COUNT(3).

           EXEC CICS LINK
                     PROGRAM  (IESLDGAC)
                     COMMAREA (IESLDGA-COMMAREA)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP NOT = DFHRESP(NORMAL)
               GO TO P45-EXIT
           END-IF.

           IF  LDGA-RET-CODE NOT = ZERO
               MOVE 1                  TO PTR
               INSPECT LDGA-USER-ID       CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
               MOVE LDGA-RET-CODE      TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING 'Unable to retrieve ' DELIMITED BY SIZE
                      LDGA-USER-ID        DELIMITED BY SPACE
                      ' - rc='            DELIMITED BY SIZE
                      UNEX-VARIABLE       DELIMITED BY SPACE
                      ', rsn='            DELIMITED BY SIZE
                 INTO MULTI-EDIT-MSG WITH POINTER PTR
               MOVE LDGA-LDAP-CODE     TO UNEX-RESP
               MOVE UNEX-RESP          TO UNEX-VARIABLE
               PERFORM Q203-LEFT-JUSTIFY
               STRING UNEX-VARIABLE       DELIMITED BY SPACE
                 INTO MULTI-EDIT-MSG WITH POINTER PTR
           END-IF.
       P45-EXIT.
           EXIT.

/DLC3/*P50-SET-VOPERID-FILE.
/DLC3/*    PERFORM Q50-INITKEY-VOPERID
/DLC3/*    MOVE VUPM-CNTL-KEY          TO VOPERID-USERID.
/DLC3/*    PERFORM Q56-READEQ-VOPERPU
/DLC3/*    EVALUATE EIBRESP
/DLC3/*    WHEN DFHRESP(NORMAL)
/DLC3/*        PERFORM Q56-READUP-VOPERID
/DLC3/*        IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC3/*            GO TO P55-EXIT
/DLC3/*        END-IF
/DLC3/*        IF  VOPERID-CODE(1:3) NOT = UPM1-OPRIDNT
/DLC3/*            PERFORM Q58-DELETE-VOPERID
/DLC3/*            MOVE DFHRESP(NOTFND) TO EIBRESP
/DLC3/*        END-IF
/DLC3/*    WHEN DFHRESP(NOTFND)
/DLC3/*        MOVE LIST-OPERCLS       TO VOPERID-CLASS
/DLC3/*        MOVE LIST-OPERDAP       TO VOPERID-DAPSCO
/DLC3/*    WHEN OTHER
/DLC3/*        GO TO P55-EXIT
/DLC3/*    END-EVALUATE.

/DLC3/*    MOVE UPM1-OPRIDNT           TO VOPERID-CODE.
/DLC3/*    MOVE UPM1-PASSWRD           TO VOPERID-PASSWORD.

/DLC3/*    MOVE SPACES                 TO VOPERID-NAME
/DLC3/*                                   WORD1 WORD2 WORD3 WORD4.
/DLC3/*    UNSTRING UPM1-USRNAME          DELIMITED BY ALL SPACES
/DLC3/*                              INTO WORD1 WORD2 WORD3 WORD4.

/DLC3/*    INSPECT WORD1 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC3/*    INSPECT WORD2 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC3/*    INSPECT WORD3 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC3/*    INSPECT WORD4 CONVERTING LOWER-CASE TO UPPER-CASE.

/DLC3/*    EVALUATE TRUE
/DLC3/*    WHEN WORD4 > SPACES
/DLC3/*    AND (WORD4 = 'JR' OR 'SR' OR '1' OR '2' OR '3'
/DLC3/*                              OR 'I' OR 'II' OR 'III')
/DLC3/*        STRING WORD3               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD2               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD4               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN WORD3 > SPACES
/DLC3/*    AND (WORD3 = 'JR' OR 'SR' OR '1' OR '2' OR '3'
/DLC3/*                              OR 'I' OR 'II' OR 'III')
/DLC3/*        STRING WORD2               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD3               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN WORD4 > SPACES
/DLC3/*        STRING WORD3               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD4               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD2               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN WORD3 > SPACES
/DLC3/*    AND (WORD2 = 'DE' OR 'DA' OR 'DI' OR 'VAN' OR 'VON'
/DLC3/*              OR 'LE' OR 'LA' OR 'DU' OR 'DES' OR 'DEL')
/DLC3/*        STRING WORD2               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD3               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN WORD3 > SPACES
/DLC3/*        STRING WORD3               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*               ' '                 DELIMITED BY SIZE
/DLC3/*               WORD2               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN WORD2 > SPACES
/DLC3/*        STRING WORD2               DELIMITED BY SPACES
/DLC3/*               ', '                DELIMITED BY SIZE
/DLC3/*               WORD1               DELIMITED BY SPACES
/DLC3/*                              INTO VOPERID-NAME
/DLC3/*    WHEN OTHER
/DLC3/*        MOVE WORD1              TO VOPERID-NAME
/DLC3/*    END-EVALUATE.
/DLC3/*
/DLC3/*    IF  EIBRESP = DFHRESP(NOTFND)
/DLC3/*        PERFORM Q57-WRITE-VOPERID
/DLC3/*    ELSE
/DLC3/*        PERFORM Q57-REWRITE-VOPERID
/DLC3/*    END-IF.
/DLC3/*P55-EXIT.
/DLC3/*    EXIT.

/DLC0/ P50-SET-VUSERID-FILE.
/DLC0/     PERFORM Q60-INITKEY-VUSERID
/DLC0/     MOVE VUPM-CNTL-KEY          TO VUSER-KEY.
/DLC0/     PERFORM Q66-READEQ-VUSERID
/DLC0/     EVALUATE EIBRESP
/DLC0/     WHEN DFHRESP(NORMAL)
/DLC0/         PERFORM Q66-READUP-VUSERID
/DLC0/         IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC0/             GO TO P56-EXIT
/DLC0/         END-IF
/DLC0/     WHEN DFHRESP(NOTFND)
/DLC0/         MOVE LIST-OPERCLS       TO VUSER-CLASS
/DLC0/         MOVE LIST-OPERDAP       TO VUSER-DAPSCO
/DLC0/     WHEN OTHER
/DLC0/         GO TO P56-EXIT
/DLC0/     END-EVALUATE.
/DLC0/
/DLC0/     MOVE UPM1-OPRIDNT           TO VUSER-OPID.
/DLC0/     MOVE UPM1-PASSWRD           TO VUSER-PASSWORD.
/DLC0/
/DLC0/     MOVE SPACES                 TO VUSER-NAME
/DLC0/                                    WORD1 WORD2 WORD3 WORD4.
/DLC0/     UNSTRING UPM1-USRNAME          DELIMITED BY ALL SPACES
/DLC0/                               INTO WORD1 WORD2 WORD3 WORD4.
/DLC0/
/DLC0/     INSPECT WORD1 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC0/     INSPECT WORD2 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC0/     INSPECT WORD3 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC0/     INSPECT WORD4 CONVERTING LOWER-CASE TO UPPER-CASE.
/DLC0/
/DLC0/     EVALUATE TRUE
/DLC0/     WHEN WORD4 > SPACES
/DLC0/     AND (WORD4 = 'JR' OR 'SR' OR '1' OR '2' OR '3'
/DLC0/                               OR 'I' OR 'II' OR 'III')
/DLC0/         STRING WORD3               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD2               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD4               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN WORD3 > SPACES
/DLC0/     AND (WORD3 = 'JR' OR 'SR' OR '1' OR '2' OR '3'
/DLC0/                               OR 'I' OR 'II' OR 'III')
/DLC0/         STRING WORD2               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD3               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN WORD4 > SPACES
/DLC0/         STRING WORD3               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD4               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD2               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN WORD3 > SPACES
/DLC0/     AND (WORD2 = 'DE' OR 'DA' OR 'DI' OR 'VAN' OR 'VON'
/DLC0/               OR 'LE' OR 'LA' OR 'DU' OR 'DES' OR 'DEL')
/DLC0/         STRING WORD2               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD3               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN WORD3 > SPACES
/DLC0/         STRING WORD3               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                ' '                 DELIMITED BY SIZE
/DLC0/                WORD2               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN WORD2 > SPACES
/DLC0/         STRING WORD2               DELIMITED BY SPACES
/DLC0/                ', '                DELIMITED BY SIZE
/DLC0/                WORD1               DELIMITED BY SPACES
/DLC0/                               INTO VUSER-NAME
/DLC0/     WHEN OTHER
/DLC0/         MOVE WORD1              TO VUSER-NAME
/DLC0/     END-EVALUATE.
/DLC0/
/DLC0/     IF  EIBRESP = DFHRESP(NOTFND)
/DLC0/         PERFORM Q67-WRITE-VUSERID
/DLC0/     ELSE
/DLC0/         PERFORM Q67-REWRITE-VUSERID
/DLC0/     END-IF.
/DLC0/ P56-EXIT.
/DLC0/     EXIT.

       P60-CHECK-OPERATOR-ID.
           MOVE SPACES                 TO MULTI-EDIT-MSG.
           IF  NOT VIEW-PROCESS
             IF  UPM1-OPRIDNT <= SPACES
               MOVE 1                  TO PTR
               STRING 'Operator id is required.' DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER PTR
               IF  COMM-CURRRTN NOT = 'UPM1'
                 STRING '  See page 1.'     DELIMITED BY SIZE
                   INTO MULTI-EDIT-MSG WITH POINTER PTR
               END-IF
             ELSE
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE ' '                TO TXT-DLM1
               MOVE LENGTH OF UPM1-OPRIDNT
                                       TO TXT-PARM-STRLEN
               MOVE UPM1-OPRIDNT       TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               MOVE TXT-PARM-STRING    TO UPM1-OPRIDNT
               IF  TXT-PARM-STRLEN < 3
               OR  TXT-PARM-STRING NOT ALPHAMERIC-NATIONAL
                 MOVE 'Operator id must be 3 alpha-numeric and national
      -               'characters.'    TO MULTI-EDIT-MSG
               ELSE
                 MOVE UPM1-USRIDNT     TO UNEX-VARIABLE
                 PERFORM Q202-SET-LENGTH
                 MOVE UNEX-LENG        TO LEN
                 MOVE LOW-VALUES       TO VUPM-CNTL-KEY
                 PERFORM Q20-INITKEY-IESCNTL
                 PERFORM Q21-STARTBR-IESCNTL
                 IF EIBRESP = DFHRESP(NORMAL)
                   PERFORM Q23-READNEXT-IESCNTL
                     UNTIL IUI-US-OPRIDNT = UPM1-OPRIDNT
                       AND IUI-US-USRIDNT(1:LEN) NOT = UPM1-USRIDNT
                        OR NOT IUI-USER-PROFILE
                        OR EIBRESP NOT = DFHRESP(NORMAL)
                   IF  EIBRESP = DFHRESP(ENDFILE)
                     MOVE HIGH-VALUES  TO IESCNTL-RECORD
                     MOVE DFHRESP(NORMAL) TO EIBRESP
                   END-IF
                 ELSE
                   MOVE DFHRESP(NORMAL) TO EIBRESP
                   MOVE ALL '?'        TO IUI-US-OPRIDNT
                 END-IF
                 EVALUATE TRUE
                 WHEN EIBRESP NOT = DFHRESP(NORMAL)
                   GO TO P65-EXIT
                 WHEN NOT IUI-USER-PROFILE
                   CONTINUE
                 WHEN IUI-US-OPRIDNT = UPM1-OPRIDNT
                   MOVE 1              TO PTR
                   STRING 'Operator id already '
                          'in use by '    DELIMITED BY SIZE
                          IUI-US-USRIDNT  DELIMITED BY LOW-VALUE
                          '.'             DELIMITED BY SIZE
                     INTO MULTI-EDIT-MSG WITH POINTER PTR
                   IF  COMM-CURRRTN NOT = 'UPM1'
                     STRING '  See page 1.' DELIMITED BY SIZE
                       INTO MULTI-EDIT-MSG WITH POINTER PTR
                   END-IF
                 END-EVALUATE
                 PERFORM Q25-ENDBR-IESCNTL
               END-IF
             END-IF
           END-IF.
       P65-EXIT.
           EXIT.

       P70-IMPORT-CONTROL-DATA.
      * extract settings for user id and name
           MOVE SPACES                 TO UPM1-USRIDNT.
           MOVE 1                      TO NAME-LENGTH.
           STRING IUI-US-USRIDNT          DELIMITED BY LOW-VALUE
               INTO UPM1-USRIDNT     WITH POINTER NAME-LENGTH.
           SUBTRACT 1                FROM NAME-LENGTH.
           MOVE UPM1-USRIDNT           TO VUPM-CNTL-KEY.

           MOVE IUI-US-USRNAME         TO UPM1-USRNAME.
/DLC2/     MOVE IUI-US-OPRIDNT         TO UPM1-OPRIDNT.

      * extract vuserid class and dapsco information
/DLC0/     PERFORM Q60-INITKEY-VUSERID.
/DLC0/     MOVE VUPM-CNTL-KEY          TO VUSER-KEY.
/DLC0/     PERFORM Q66-READEQ-VUSERID.
/DLC0/     IF  EIBRESP = DFHRESP(NORMAL)
/DLC0/         MOVE VUSER-CLASS        TO LIST-OPERCLS
/DLC0/         MOVE VUSER-DAPSCO       TO LIST-OPERDAP
/DLC0/     ELSE
/DLC0/         MOVE DFHRESP(NORMAL)    TO EIBRESP
/DLC0/         MOVE '00'               TO LIST-OPERCLS
/DLC0/         MOVE 'W'                TO LIST-OPERDAP
/DLC0/     END-IF.

      * extract settings for user password
           MOVE 'O'                    TO BIT-REQU.
           MOVE IUI-US-PASSWRD         TO BIT-DATA.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-DATA               TO UPM1-PASSWRD.

      * extract settings for expiration date
           IF  IUI-US-EXPJULN > ZEROES
           OR  IUI-US-EXPDATE > SPACES
               IF  IUI-US-EXPJULN > ZEROES
                   MOVE 'J'            TO DTE-REQU
                   MOVE IUI-US-EXPJULN TO DTE-JNUM
                   CALL 'DTEMAN'    USING DTEMAN-PARMS
                   MOVE ALL '/'        TO EDT-DATE-X
                   MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
                   MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
                   MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
                   MOVE EDT-DATE-X     TO LIST-EXPIRES
               ELSE
                   MOVE IUI-US-EXPDATE TO LIST-EXPIRES
               END-IF
               MOVE   'G'              TO DTE-REQU
               MOVE CICS-CCYYMMDD      TO DTE-GNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               IF  DTE-YYDDD > IUI-US-EXPJULN
                   MOVE '*'            TO LIST-EXPIRED
               ELSE
                   MOVE SPACE          TO LIST-EXPIRED
               END-IF
           ELSE
               MOVE SPACES             TO LIST-EXPIRES
                                          LIST-EXPIRED
           END-IF.

      * extract settings for LDAP user mappings
           IF  LIST-EXPIRED NOT = '*'
               PERFORM Q40-INITKEY-IESLDUM
               MOVE VUPM-CNTL-KEY      TO LDUM-MFUSRID
               PERFORM Q46-READEQ-IESLDUV
               IF  EIBRESP = DFHRESP(NOTFND)
                   MOVE VUPM-CNTL-KEY  TO LDUM-NETUSRID
                   PERFORM Q46-READEQ-IESLDUM
               END-IF
               IF  EIBRESP = DFHRESP(NORMAL)
                   IF  LDUM-MFUSRID <= SPACES
                       MOVE 'V'        TO LIST-EXPIRED
                   ELSE
                       MOVE 'M'        TO LIST-EXPIRED
                   END-IF
               ELSE
                   MOVE DFHRESP(NORMAL) TO EIBRESP
               END-IF
               IF  LIST-EXPIRED NOT = 'M' AND 'V'
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-IESVSAM TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
                   IF  BIT-4-IS-OFF
                       MOVE 'I'        TO LIST-EXPIRED
                   END-IF
               END-IF
           END-IF.

      * extract settings for revokation date
           IF  IUI-US-REVOKEJ > ZEROES
               MOVE   'J'              TO DTE-REQU
               MOVE IUI-US-REVOKEJ     TO DTE-JNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE ALL '/'            TO EDT-DATE-X
               MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
               MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
               MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
               MOVE EDT-DATE-X         TO UPM1-REVOKED
           ELSE
               MOVE SPACES             TO UPM1-REVOKED
           END-IF.

      * extract settings for last signon date
           IF  IUI-US-LASTLGN > ZEROES
               MOVE   'J'              TO DTE-REQU
               MOVE IUI-US-LASTLGN     TO DTE-JNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE ALL '/'            TO EDT-DATE-X
               MOVE DTE-GRMO IN DTE-GREG
                                       TO EDT-YY
               MOVE DTE-GRDA IN DTE-GREG
                                       TO EDT-MM
               MOVE DTE-GRYY IN DTE-GREG
                                       TO EDT-DD
               MOVE EDT-DATE-X         TO LIST-SIGNON
           ELSE
               MOVE SPACES             TO LIST-SIGNON
           END-IF.

      * extract settings for IUI information
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESINFO         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           EVALUATE TRUE ALSO TRUE
             WHEN BIT-0-IS-ON  ALSO BIT-1-IS-ON
               MOVE '1'                TO UPM1-USRTYPE
             WHEN BIT-0-IS-OFF ALSO BIT-1-IS-ON
               MOVE '2'                TO UPM1-USRTYPE
             WHEN OTHER
               MOVE '3'                TO UPM1-USRTYPE
           END-EVALUATE.
           IF  BIT-2-IS-ON
               MOVE '2'                TO UPM2-IESNEWS
           ELSE
               MOVE '1'                TO UPM2-IESNEWS
           END-IF.
           IF  BIT-3-IS-ON
               MOVE '2'                TO UPM1-INITYPE
           ELSE
               MOVE '1'                TO UPM1-INITYPE
           END-IF.

      * extract settings for IUI experience
           MOVE IUI-US-INITNAM         TO UPM1-INITNAM.
       P75-EXIT.
           EXIT.

       P80-IMPORT-RECORD-DATA.
      * extract settings for profile expiration
           MOVE IUI-US-EXPDAYS         TO NUM-WORK.
           MOVE NUM-WRK3               TO UPM1-EXPDAYS.

      * extract up to 12 old passwords
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
               SUBTRACT 1            FROM SUB
               SUBTRACT SUB FROM 12 GIVING CNT
               ADD  1                  TO SUB
               IF  IUI-US-PREVPSW(CNT) > LOW-VALUES
                   MOVE 'O'            TO BIT-REQU
                   MOVE IUI-US-PREVPSW(CNT)
                                       TO BIT-DATA
                   CALL 'BITMAN'    USING BITMAN-PARMS
                   MOVE BIT-DATA       TO UPM1-OLDPSWD(SUB)
               ELSE
                   MOVE ALL '.'        TO UPM1-OLDPSWD(SUB)
               END-IF
           END-PERFORM.

      * extract settings for VSAM security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESVSAM         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-4-IS-ON
               SET  NON-ICCF-USER      TO TRUE
           ELSE
               SET  IS-ICCF-USER       TO TRUE
           END-IF.
           IF  BIT-5-IS-ON
               MOVE '1'                TO UPM2-IESCDEL
           ELSE
               MOVE '2'                TO UPM2-IESCDEL
           END-IF.
           IF  BIT-6-IS-ON
               MOVE '1'                TO UPM2-VSMCATL
           ELSE
               MOVE '2'                TO UPM2-VSMCATL
           END-IF.
           IF  BIT-7-IS-ON
               MOVE '1'                TO UPM2-VSMFILE
           ELSE
               MOVE '2'                TO UPM2-VSMFILE
           END-IF.

      * extract settings for VSE authorities
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESVSEA         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-5-IS-ON
               MOVE '2'                TO UPM2-IESSUBM
           ELSE
               MOVE '1'                TO UPM2-IESSUBM
           END-IF.
           IF  BIT-6-IS-ON
               MOVE '1'                TO UPM2-IESVPSL
           ELSE
               MOVE '2'                TO UPM2-IESVPSL
           END-IF.

      * extract settings for VSE access
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESACCS         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-1-IS-ON
               MOVE '1'                TO UPM2-IESESCP
           ELSE
               MOVE '2'                TO UPM2-IESESCP
           END-IF.
           IF  BIT-2-IS-ON
               MOVE '2'                TO UPM2-IESCPWD
           ELSE
               MOVE '1'                TO UPM2-IESCPWD
           END-IF.
           IF  BIT-5-IS-ON
               MOVE '2'                TO UPM2-IESCMSG
           ELSE
               MOVE '1'                TO UPM2-IESCMSG
           END-IF.
           IF  BIT-6-IS-ON
               MOVE '1'                TO UPM2-IESBQUE
           ELSE
               MOVE '2'                TO UPM2-IESBQUE
           END-IF.
           IF  BIT-7-IS-ON
               MOVE '1'                TO UPM2-IESCNSL
           ELSE
               MOVE '2'                TO UPM2-IESCNSL
           END-IF.

      * extract settings for IUI authorities
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESAUTH         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-3-IS-ON
               MOVE '1'                TO UPM2-APMAUTH
           ELSE
               MOVE '2'                TO UPM2-APMAUTH
           END-IF.
           IF  BIT-5-IS-ON
               MOVE '1'                TO UPM2-SPMAUTH
           ELSE
               MOVE '2'                TO UPM2-SPMAUTH
           END-IF.
           IF  BIT-6-IS-ON
               MOVE '1'                TO UPM2-IESOLPD
           ELSE
               MOVE '2'                TO UPM2-IESOLPD
           END-IF.
           IF  BIT-7-IS-ON
               MOVE '1'                TO UPM2-UPMAUTH
           ELSE
               MOVE '2'                TO UPM2-UPMAUTH
           END-IF.

      * extract settings for user information
           MOVE IUI-US-USRMODL         TO UPM1-USRMODL.
           MOVE IUI-US-USERCAT         TO UPM2-USERCAT.
/DLC2/*    MOVE IUI-US-OPRIDNT         TO UPM3-OPRIDNT.

      * extract settings for operator priority
           MOVE ZEROES                 TO HALF-WORD.
           MOVE IUI-US-OPERPRI         TO LO-LSB.
           MOVE HALF-WORD              TO NUM-WORK.
           MOVE NUM-WRK3               TO UPM3-OPERPRI.

      * extract settings for external security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-EXTNSEC         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-7-IS-ON
               MOVE '1'                TO UPM3-XRFSGNO
           ELSE
               MOVE '2'                TO UPM3-XRFSGNO
           END-IF.

      * extract settings for session timeout
           MOVE ZEROES                 TO HALF-WORD.
           MOVE IUI-US-TIMEOUT         TO LO-LSB.
           MOVE HALF-WORD              TO NUM-WORK.
           MOVE NUM-WRK2               TO UPM3-TIMEOUT.

      * extract settings for national language support
           IF  IUI-US-IESNATL <= SPACES
               MOVE SPACES             TO UPM3-NATLANG
           ELSE
               EVALUATE IUI-US-IESNATL
               WHEN 'ENU'
                 MOVE 'E'              TO UPM3-NATLANG
               WHEN 'CHS'
                 MOVE 'C'              TO UPM3-NATLANG
               WHEN 'DEU'
                 MOVE 'D'              TO UPM3-NATLANG
               WHEN 'JPN'
                 MOVE 'J'              TO UPM3-NATLANG
               WHEN OTHER
                 MOVE IUI-US-IESNATL   TO UPM3-NATLANG
               END-EVALUATE
           END-IF.

      * extract settings for transaction group security
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 64
      * bits are set from right to left over 8 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 8 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-CICSBYT(XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF (XBYT = 8 AND XBIT = 8)
               OR (XBYT = 1 AND XBIT = 4)
                   MOVE 'X'            TO UPM3-TRANSEC(SUB)
               ELSE
                   IF  BIT-IS-ON(XBIT)
                       MOVE 'X'        TO UPM3-TRANSEC(SUB)
                   ELSE
                       MOVE ' '        TO UPM3-TRANSEC(SUB)
                   END-IF
               END-IF
           END-PERFORM.

      * extract settings for operator class security
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 24
      * bits are set from right to left over 3 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 3 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-OPERBYT(XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF (XBYT = 3 AND XBIT = 8)
                   MOVE 'X'            TO UPM4-OPCLASS(SUB)
               ELSE
                   IF  BIT-IS-ON(XBIT)
                       MOVE 'X'        TO UPM4-OPCLASS(SUB)
                   ELSE
                       MOVE ' '        TO UPM4-OPCLASS(SUB)
                   END-IF
               END-IF
           END-PERFORM.

      * extract settings for VSE DTSECTAB resource security
           MOVE ALL '0'                TO UPM4-DTSECTAB.
           PERFORM WITH TEST BEFORE
             VARYING XGRP FROM 5 BY -1 UNTIL XGRP = 1
           PERFORM WITH TEST BEFORE
             VARYING SUB  FROM 1 BY +1 UNTIL SUB > 32
      * bits are set from right to left in 4 groups of 4 bytes (32 bits)
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 4 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-ACCSBYT(XGRP XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF  BIT-IS-ON(XBIT)
                   ADD  1              TO UPM4-DTSECTB(SUB)
               END-IF
           END-PERFORM
           END-PERFORM.

      * extract settings for VSE DIRECTORY security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-DIRBYTE         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  BIT-0-IS-ON
               MOVE '1'                TO UPM4-DTSBTRN
           ELSE
               MOVE '2'                TO UPM4-DTSBTRN
           END-IF.
           IF  BIT-1-IS-ON
               MOVE '1'                TO UPM4-DTSREAD
           ELSE
               MOVE '2'                TO UPM4-DTSREAD
           END-IF.
       P85-EXIT.
           EXIT.

       P90-EXPORT-RECORD-DATA.
      * replace user id and name
           MOVE UPM1-USRIDNT           TO IUI-US-USRIDNT.
           INSPECT IUI-US-USRIDNT REPLACING ALL SPACE BY LOW-VALUE.

           MOVE UPM1-USRNAME           TO IUI-US-USRNAME.

      * replace user password
           SET  KEEP-EXPIRATION   TO TRUE.
           IF  UPM1-PASSWRD > SPACES
               MOVE 'I'                TO BIT-REQU
               MOVE UPM1-PASSWRD       TO BIT-DATA
               CALL 'BITMAN'        USING BITMAN-PARMS
               IF  BIT-DATA NOT = IUI-US-PASSWRD
               AND NOT ADD-PROCESS
                 SET  SET-EXPIRATION   TO TRUE
                 PERFORM WITH TEST BEFORE
                     VARYING SUB FROM 1 BY 1 UNTIL SUB > 12
                   IF  SUB < 12
                     MOVE IUI-US-PREVPSW(SUB + 1)
                                       TO IUI-US-PREVPSW(SUB)
                   ELSE
                     MOVE IUI-US-PASSWRD
                                       TO IUI-US-PREVPSW(SUB)
                   END-IF
                 END-PERFORM
               END-IF
               MOVE BIT-DATA           TO IUI-US-PASSWRD
           END-IF.

      * replace profile expiration days
           SET NUM-REQUEST-NORMAL-EDIT TO TRUE.
           MOVE 3                      TO NUM-TOTL.
           MOVE 0                      TO NUM-DECM.
           MOVE UPM1-EXPDAYS           TO NUM-DATA.
           CALL 'NUMMAN'            USING NUMMAN-PARMS.
           IF  NUM-T03-D0 NOT = IUI-US-EXPDAYS
           OR  ADD-PROCESS
               SET  SET-EXPIRATION     TO TRUE
               MOVE NUM-T03-D0         TO IUI-US-EXPDAYS
           END-IF.

      * if password or expire days were changed, set new expire date
           IF  SET-EXPIRATION
               SET KEEP-EXPIRATION TO TRUE
               IF  IUI-US-EXPDAYS = ZERO
                   MOVE ZEROES         TO IUI-US-EXPJULN
                   MOVE SPACES         TO IUI-US-EXPDATE
               ELSE
               MOVE 'G'                TO DTE-REQU
               MOVE CICS-CCYYMMDD      TO DTE-GNUM
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE 'T'                TO DTE-REQU
               ADD  NUM-T03-D0         TO DTE-TOTD
               CALL 'DTEMAN'        USING DTEMAN-PARMS
               MOVE DTE-YYDDD          TO IUI-US-EXPJULN
               MOVE SPACES             TO IUI-US-EXPDATE
           ELSE
               IF  LIST-EXPIRES > SPACES
               AND IUI-US-EXPDAYS = ZERO
                   MOVE LIST-EXPIRES   TO IUI-US-EXPDATE
               END-IF
           END-IF.

      * replace profile revokation date
           IF  UPM1-REVOKED > SPACES
               SET TXT-REQUEST-JUSTIFY-L TO TRUE
               MOVE LOW-VALUES         TO TXT-DLMS
               MOVE '/'                TO TXT-DLM1
               MOVE LENGTH OF UPM1-REVOKED
                                       TO TXT-PARM-STRLEN
               MOVE UPM1-REVOKED       TO TXT-PARM-STRING
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
               SET DTE-REQUEST-GREG-EDIT TO TRUE
               MOVE ZEROES             TO DTE-GNUM
               MOVE TXT-PARM-STRING(1:TXT-PARM-STRLEN)
                                       TO NUM-DATE6
               MOVE CORR WRK-DATE6     TO DTE-GREG
               MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
               CALL 'DTEMAN'      USING DTEMAN-PARMS
               MOVE DTE-YYDDD          TO IUI-US-REVOKEJ
           ELSE
               MOVE ZEROES             TO IUI-US-REVOKEJ
           END-IF.

      * replace settings for IUI information
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESINFO         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           EVALUATE UPM1-USRTYPE
             WHEN '1'
               SET  BIT-0-IS-ON        TO TRUE
               SET  BIT-1-IS-ON        TO TRUE
             WHEN '2'
               SET  BIT-0-IS-OFF       TO TRUE
               SET  BIT-1-IS-ON        TO TRUE
             WHEN OTHER
               SET  BIT-0-IS-OFF       TO TRUE
               SET  BIT-1-IS-OFF       TO TRUE
           END-EVALUATE.
           IF  UPM1-INITYPE = '2'
               SET  BIT-3-IS-ON        TO TRUE
           ELSE
               SET  BIT-3-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-IESNEWS = '2'
               SET  BIT-2-IS-ON        TO TRUE
           ELSE
               SET  BIT-2-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-IESINFO.

      * replace settings for IUI experience
           MOVE UPM1-INITNAM           TO IUI-US-INITNAM.
           MOVE UPM1-USRMODL           TO IUI-US-USRMODL.

      * replace settings for VSAM security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESVSAM         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  IS-ICCF-USER
               SET  BIT-4-IS-OFF       TO TRUE
           ELSE
               SET  BIT-4-IS-ON        TO TRUE
           END-IF.
           IF  UPM2-IESCDEL = '1'
               SET  BIT-5-IS-ON        TO TRUE
           ELSE
               SET  BIT-5-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-VSMCATL = '1'
               SET  BIT-6-IS-ON        TO TRUE
           ELSE
               SET  BIT-6-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-VSMFILE = '1'
               SET  BIT-7-IS-ON        TO TRUE
           ELSE
               SET  BIT-7-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-IESVSAM.

      * replace settings for VSE authorities
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESVSEA         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  UPM2-IESSUBM = '1'
               SET  BIT-5-IS-OFF       TO TRUE
           ELSE
               SET  BIT-5-IS-ON        TO TRUE
           END-IF.
           IF  UPM2-IESVPSL = '1'
               SET  BIT-6-IS-ON        TO TRUE
           ELSE
               SET  BIT-6-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-IESVSEA.

      * replace settings for VSE access
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESACCS         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  UPM2-IESESCP = '1'
               SET  BIT-1-IS-ON        TO TRUE
           ELSE
               SET  BIT-1-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-IESCPWD = '1'
               SET  BIT-2-IS-OFF       TO TRUE
           ELSE
               SET  BIT-2-IS-ON        TO TRUE
           END-IF.
           IF  UPM2-IESCMSG = '1'
               SET  BIT-5-IS-OFF       TO TRUE
           ELSE
               SET  BIT-5-IS-ON        TO TRUE
           END-IF.
           IF  UPM2-IESBQUE = '1'
               SET  BIT-6-IS-ON        TO TRUE
           ELSE
               SET  BIT-6-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-IESCNSL = '1'
               SET  BIT-7-IS-ON        TO TRUE
           ELSE
               SET  BIT-7-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-IESACCS.

      * replace settings for IUI authorities
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-IESAUTH         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  UPM2-APMAUTH = '1'
               SET  BIT-3-IS-ON        TO TRUE
           ELSE
               SET  BIT-3-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-SPMAUTH = '1'
               SET  BIT-5-IS-ON        TO TRUE
           ELSE
               SET  BIT-5-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-IESOLPD = '1'
               SET  BIT-6-IS-ON        TO TRUE
           ELSE
               SET  BIT-6-IS-OFF       TO TRUE
           END-IF.
           IF  UPM2-UPMAUTH = '1'
               SET  BIT-7-IS-ON        TO TRUE
           ELSE
               SET  BIT-7-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-IESAUTH.

      * replace settings for user information
           MOVE UPM2-USERCAT           TO IUI-US-USERCAT.
           MOVE UPM1-OPRIDNT           TO IUI-US-OPRIDNT.

      * replace settings for operator priority
           SET NUM-REQUEST-NORMAL-EDIT TO TRUE.
           MOVE 3                      TO NUM-TOTL.
           MOVE 0                      TO NUM-DECM.
           MOVE UPM3-OPERPRI           TO NUM-DATA.
           CALL 'NUMMAN'            USING NUMMAN-PARMS.
           MOVE NUM-T03-D0             TO HALF-WORD.
           MOVE LO-LSB                 TO IUI-US-OPERPRI.

      * replace settings for external security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-EXTNSEC         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  UPM3-XRFSGNO = '1'
               SET  BIT-7-IS-ON        TO TRUE
           ELSE
               SET  BIT-7-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-EXTNSEC.

      * replace settings for session timeout
           SET NUM-REQUEST-NORMAL-EDIT TO TRUE.
           MOVE 2                      TO NUM-TOTL.
           MOVE 0                      TO NUM-DECM.
           MOVE UPM3-TIMEOUT           TO NUM-DATA.
           CALL 'NUMMAN'            USING NUMMAN-PARMS.
           MOVE NUM-T02-D0             TO HALF-WORD.
           MOVE LO-LSB                 TO IUI-US-TIMEOUT.

      * replace settings for national language support
           EVALUATE UPM3-NATLANG
           WHEN 'E'
             MOVE 'ENU'                TO IUI-US-IESNATL
           WHEN 'C'
             MOVE 'CHS'                TO IUI-US-IESNATL
           WHEN 'D'
             MOVE 'DEU'                TO IUI-US-IESNATL
           WHEN 'J'
             MOVE 'JPN'                TO IUI-US-IESNATL
           WHEN OTHER
             MOVE UPM3-NATLANG         TO IUI-US-IESNATL
           END-EVALUATE

      * replace settings for transaction group security
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 64
      * bits are set from right to left over 8 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 8 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-CICSBYT(XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF  UPM3-TRANSEC(SUB) > SPACES
                   SET BIT-IS-ON(XBIT) TO TRUE
               ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
               END-IF
      *
               IF  XBIT = 1
                   MOVE 'E'            TO BIT-REQU
                   CALL 'BITMAN'    USING BITMAN-PARMS
                   MOVE BIT-BYTE       TO IUI-US-CICSBYT(XBYT)
               END-IF
           END-PERFORM.

      * replace settings for operator class security
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 24
      * bits are set from right to left over 3 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 3 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-OPERBYT(XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF  UPM4-OPCLASS(SUB) > SPACES
                   SET BIT-IS-ON(XBIT) TO TRUE
               ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
               END-IF
      *
               IF  XBIT = 1
                   MOVE 'E'            TO BIT-REQU
                   CALL 'BITMAN'    USING BITMAN-PARMS
                   MOVE BIT-BYTE       TO IUI-US-OPERBYT(XBYT)
               END-IF
           END-PERFORM.

      * replace settings for VSE DTSECTAB resource security
           MOVE LOW-VALUES             TO IUI-US-DTSECTB.
           PERFORM WITH TEST BEFORE
             VARYING XGRP FROM 5 BY -1 UNTIL XGRP < 1
           PERFORM WITH TEST BEFORE
             VARYING SUB  FROM 1 BY +1 UNTIL SUB > 32
      * bits are set from right to left in 4 groups of 4 bytes (32 bits)
      *   then 32 usage flag bits in the last group of 4 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 4 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-ACCSBYT(XGRP XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               EVALUATE XGRP
               WHEN 5
                 IF  UPM4-DTSECTB(SUB) >= 1
                   SET BIT-IS-ON(XBIT) TO TRUE
                 ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
                 END-IF
               WHEN 4
                 IF  UPM4-DTSECTB(SUB) >= 2
                   SET BIT-IS-ON(XBIT) TO TRUE
                 ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
                 END-IF
               WHEN 3
                 IF  UPM4-DTSECTB(SUB) >= 3
                   SET BIT-IS-ON(XBIT) TO TRUE
                 ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
                 END-IF
               WHEN 2
                 IF  UPM4-DTSECTB(SUB) >= 4
                   SET BIT-IS-ON(XBIT) TO TRUE
                 ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
                 END-IF
               WHEN 1
                 IF  UPM4-DTSECTB(SUB) >  0
                   SET BIT-IS-ON(XBIT) TO TRUE
                 ELSE
                   SET BIT-IS-OFF(XBIT) TO TRUE
                 END-IF
               END-EVALUATE
      *
               IF  XBIT = 1
                   MOVE 'E'            TO BIT-REQU
                   CALL 'BITMAN'    USING BITMAN-PARMS
                   MOVE BIT-BYTE       TO IUI-US-ACCSBYT(XGRP XBYT)
               END-IF
           END-PERFORM
           END-PERFORM.

      * replace settings for VSE DIRECTORY security
           MOVE 'D'                    TO BIT-REQU.
           MOVE IUI-US-DIRBYTE         TO BIT-BYTE.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           IF  UPM4-DTSBTRN = '1'
               SET  BIT-0-IS-ON        TO TRUE
           ELSE
               SET  BIT-0-IS-OFF       TO TRUE
           END-IF.
           IF  UPM4-DTSREAD = '1'
               SET  BIT-1-IS-ON        TO TRUE
           ELSE
               SET  BIT-1-IS-OFF       TO TRUE
           END-IF.
           MOVE 'E'                    TO BIT-REQU.
           CALL 'BITMAN'            USING BITMAN-PARMS.
           MOVE BIT-BYTE               TO IUI-US-DIRBYTE.
       P95-EXIT.
           EXIT.

       P96-SET-PERMISSIONS.
           PERFORM WITH TEST BEFORE
             VARYING SUB FROM 1 BY 1 UNTIL SUB > 64
      * bits are set from right to left over 8 bytes
               SUBTRACT 1            FROM SUB
               DIVIDE SUB BY 8 GIVING XBYT REMAINDER XBIT
               ADD  1                  TO SUB
               SUBTRACT XBYT FROM 8 GIVING XBYT
               SUBTRACT XBIT FROM 8 GIVING XBIT
               IF  XBIT = 8
                   MOVE 'D'            TO BIT-REQU
                   MOVE IUI-US-CICSBYT(XBYT)
                                       TO BIT-BYTE
                   CALL 'BITMAN'    USING BITMAN-PARMS
               END-IF
      *
               IF (XBYT = 8 AND XBIT = 8)
               OR (XBYT = 1 AND XBIT = 4)
                   PERFORM P97-GRANT-PERMISSION
               ELSE
                   IF  BIT-IS-ON(XBIT)
                       PERFORM P97-GRANT-PERMISSION
                   ELSE
                       PERFORM P98-REVOKE-PERMISSION
                   END-IF
               END-IF
           END-PERFORM.

           EXEC CICS LINK PROGRAM(VSECREFR) END-EXEC.
           GO TO P99-EXIT.

       P97-GRANT-PERMISSION.
           MOVE SPACES                 TO VSECSERV-COMMAREA.
           SET  VSECSERV-TRAN          TO TRUE.
           SET VSECSERV-UPD-PERMISSION TO TRUE.
           SET  VSECUPD-RES-GROUP      TO TRUE.
           MOVE SUB                    TO NUM-WORK.
           STRING 'GROUP' NUM-WRK2
               DELIMITED BY SIZE     INTO VSECUPD-RES-NAME.
           SET  VSECUPD-PER-USER       TO TRUE.
           MOVE VUPM-CNTL-KEY          TO VSECUPD-PER-NAME.
           SET  VSECUPD-PER-AC-READ    TO TRUE.
           EXEC CICS LINK
                     PROGRAM(VSECSERV)
                     COMMAREA(VSECSERV-COMMAREA)
                     NOHANDLE
           END-EXEC.

       P98-REVOKE-PERMISSION.
           MOVE SPACES                 TO VSECSERV-COMMAREA.
           SET  VSECSERV-TRAN          TO TRUE.
           SET VSECSERV-DEL-PERMISSION TO TRUE.
           SET  VSECUPD-RES-GROUP      TO TRUE.
           MOVE SUB                    TO NUM-WORK.
           STRING 'GROUP' NUM-WRK2
               DELIMITED BY SIZE     INTO VSECUPD-RES-NAME.
           SET  VSECUPD-PER-USER       TO TRUE.
           MOVE VUPM-CNTL-KEY          TO VSECUPD-PER-NAME.
           SET  VSECUPD-PER-AC-READ    TO TRUE.
           EXEC CICS LINK
                     PROGRAM(VSECSERV)
                     COMMAREA(VSECSERV-COMMAREA)
                     NOHANDLE
           END-EXEC.

       P99-EXIT.
           EXIT.

      /*****************************************************************
      *    MORE PERFORMED ROUTINES                                     *
      ******************************************************************

       Q00-FORMAT-DATE-AND-TIME.
           EXEC CICS ASKTIME    ABSTIME(WS-ABSTIME) END-EXEC.
           EXEC CICS FORMATTIME ABSTIME(WS-ABSTIME)
                     FULLDATE (WS-WRKDATE)          DATESEP
                     TIME     (WS-WRKTIME)          TIMESEP
           END-EXEC.
       Q02-REFORMAT-TIME.
           IF  WS-HRSTIME < 12
               MOVE ' AM'              TO WS-XXXTIME
               IF  WS-HRSTIME = 00
                   MOVE 12             TO WS-HRSTIME
               END-IF
           ELSE
               MOVE ' PM'              TO WS-XXXTIME
               IF  WS-HRSTIME > 12
                   SUBTRACT 12       FROM WS-HRSTIME
               END-IF
           END-IF.
       Q05-EXIT.
           EXIT.

       Q10-DISPLAY-POPUP.

           EXEC CICS GETMAIN
                     SET      (ADDRESS OF WAITWINI)
                     LENGTH   (LENGTH OF WAITWINI)
                     INITIMG  (LOVALUE)
           END-EXEC.

           MOVE POPUP-TEXT             TO WAIT-MESSAGE-O.
           EXEC CICS SEND
                     MAP      (WAITWIN)
                     FROM     (WAITWINO)
                     MAPSET   (VUPMMS)
                     TERMINAL
                     CURSOR(+926)
                     WAIT
                     NOHANDLE
           END-EXEC.

           EXEC CICS FREEMAIN
                     DATA     (WAITWINI)
           END-EXEC.

       Q15-EXIT.
           EXIT.

       Q20-INITKEY-IESCNTL.
           IF  ADDRESS OF IESCNTL-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESCNTL-RECORD)
                         LENGTH   (LENGTH OF IESCNTL-RECORD)
               END-EXEC
               SET ADDRESS OF IUI-US-RECORD
                                       TO ADDRESS OF IESCNTL-RECORD
           END-IF.
           MOVE LOW-VALUES             TO IUI-KEY.
           SET  IUI-USER-PROFILE       TO TRUE.
           STRING VUPM-CNTL-KEY           DELIMITED BY SPACE
                                     INTO IUI-US-USRIDNT.
       Q21-STARTBR-IESCNTL.
           EXEC CICS STARTBR
                     DATASET  (IESCNTL)
                     RIDFLD   (IUI-US-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q21-STARTEQ-IESCNTL.
           EXEC CICS STARTBR
                     DATASET  (IESCNTL)
                     RIDFLD   (IUI-US-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q22-RESETBR-IESCNTL.
           EXEC CICS RESETBR
                     DATASET  (IESCNTL)
                     RIDFLD   (IUI-US-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q23-READNEXT-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READNEXT
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     NOHANDLE
           END-EXEC.
       Q23-CHK-PREFIX.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  IUI-USER-PROFILE
                 PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
               ELSE
                 PERFORM Q24-READPREV-IESCNTL
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
           END-IF.
       Q23-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q23-READNEXT-IESCNTL
               END-IF
               PERFORM P80-IMPORT-RECORD-DATA THRU P85-EXIT
           END-IF.
       Q24-READPREV-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READPREV
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     NOHANDLE
           END-EXEC.
       Q24-CHK-PREFIX.
           IF  EIBRESP = DFHRESP(NORMAL)
               IF  IUI-USER-PROFILE
                 PERFORM P70-IMPORT-CONTROL-DATA THRU P75-EXIT
               ELSE
                 PERFORM Q23-READNEXT-IESCNTL
                 MOVE DFHRESP(ENDFILE) TO EIBRESP
               END-IF
           END-IF.
       Q24-CHK-FILTER.
           IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM Q90-CHECK-FILTERS THRU Q95-EXIT
               IF  RECORD-NOT-SELECTED
                   GO TO Q24-READPREV-IESCNTL
               END-IF
               PERFORM P80-IMPORT-RECORD-DATA THRU P85-EXIT
           END-IF.
       Q25-ENDBR-IESCNTL.
           EXEC CICS ENDBR
                     DATASET  (IESCNTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(INVREQ)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q26-READEQ-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READ
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q26-READGE-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READ
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     GTEQ
                     NOHANDLE
           END-EXEC.
       Q26-READUP-IESCNTL.
           MOVE LENGTH OF IESCNTL-RECORD TO CNTL-RECL.
           EXEC CICS READ
                     DATASET  (IESCNTL)
                     INTO     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q27-WRITE-IESCNTL.
           PERFORM P90-EXPORT-RECORD-DATA THRU P95-EXIT.
           MOVE LENGTH OF IUI-US-RECORD TO CNTL-RECL.
           EXEC CICS WRITE
                     DATASET  (IESCNTL)
                     FROM     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     RIDFLD   (IUI-US-KEY)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
/DLC0/       PERFORM P50-SET-VUSERID-FILE THRU P56-EXIT
/DLC0/*DLC3  IF  EIBRESP = DFHRESP(NORMAL)
/DLC3/*        PERFORM P50-SET-VOPERID-FILE THRU P55-EXIT
/DLC0/*DLC3  END-IF
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM P96-SET-PERMISSIONS THRU P99-EXIT
             END-IF
           END-IF.
       Q27-REWRITE-IESCNTL.
           PERFORM P90-EXPORT-RECORD-DATA THRU P95-EXIT.
           MOVE LENGTH OF IUI-US-RECORD TO CNTL-RECL.
           EXEC CICS REWRITE
                     DATASET  (IESCNTL)
                     FROM     (IUI-US-RECORD)
                     LENGTH   (CNTL-RECL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(NORMAL)
/DLC0/       PERFORM P50-SET-VUSERID-FILE THRU P56-EXIT
/DLC0/*DLC3  IF  EIBRESP = DFHRESP(NORMAL)
/DLC3/*        PERFORM P50-SET-VOPERID-FILE THRU P55-EXIT
/DLC0/*DLC3  END-IF
             IF  EIBRESP = DFHRESP(NORMAL)
               PERFORM P96-SET-PERMISSIONS THRU P99-EXIT
             END-IF
           END-IF.
       Q28-DELETE-IESCNTL.
           EXEC CICS DELETE
                     DATASET  (IESCNTL)
                     NOHANDLE
           END-EXEC.
       Q28-UNLOCK-IESCNTL.
           EXEC CICS UNLOCK
                     DATASET  (IESCNTL)
                     NOHANDLE
           END-EXEC.
       Q29-EXIT.
           EXIT.

       Q30-INIT-TS.
           IF  ADDRESS OF SORTREC = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF SORTREC)
                         LENGTH   (LENGTH OF SORTREC)
               END-EXEC
           END-IF.
           MOVE THIS-TRN               TO TS-QUE-TRAN.
           MOVE EIBTRMID               TO TS-QUE-TERM.
       Q31-NUMITEMS-TS.
           MOVE 1                      TO TS-ITEM.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     ITEM    (TS-ITEM)
                     INTO    (SORTREC)
                     NUMITEMS(TS-TOTL)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
               MOVE ZEROES             TO TS-TOTL
                                          TS-ITEM
           END-IF.
       Q32-READQ-TS.
           EXEC CICS READQ TS
                     QUEUE   (TS-QUEUE)
                     INTO    (SORTREC)
                     ITEM    (SS-ITEM)
                     NOHANDLE
           END-EXEC.
       Q33-WRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (SORTREC)
                     MAIN
                     NOHANDLE
           END-EXEC.
       Q34-REWRITEQ-TS.
           EXEC CICS WRITEQ TS
                     QUEUE   (TS-QUEUE)
                     FROM    (SORTREC)
                     ITEM    (SS-ITEM)
                     REWRITE
                     MAIN
                     NOHANDLE
           END-EXEC.
       Q35-DELETEQ-TS.
           MOVE ZEROES                 TO TS-ITEM TS-TOTL.
           EXEC CICS DELETEQ TS
                     QUEUE   (TS-QUEUE)
                     NOHANDLE
           END-EXEC.
           IF  EIBRESP = DFHRESP(QIDERR)
               MOVE DFHRESP(NORMAL)    TO EIBRESP
           END-IF.
       Q35-EXIT.
           EXIT.

       Q40-INITKEY-IESLDUM.
           IF  ADDRESS OF IESLDUM-RECORD = NULL
               EXEC CICS GETMAIN
                         SET      (ADDRESS OF IESLDUM-RECORD)
                         LENGTH   (LENGTH OF IESLDUM-RECORD)
               END-EXEC
           END-IF.
           MOVE LOW-VALUES             TO LDUM-KEY.
           SET  LDUM-USRMAP-RECORD     TO TRUE.
       Q46-READEQ-IESLDUM.
           EXEC CICS READ
                     DATASET  (IESLDUM)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-KEY)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q46-READEQ-IESLDUV.
           EXEC CICS READ
                     DATASET  (IESLDUV)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-MFUSRID)
                     EQUAL
                     NOHANDLE
           END-EXEC.
       Q46-READUP-IESLDUM.
           EXEC CICS READ
                     DATASET  (IESLDUM)
                     INTO     (IESLDUM-RECORD)
                     RIDFLD   (LDUM-KEY)
                     EQUAL
                     UPDATE
                     NOHANDLE
           END-EXEC.
       Q48-DELETE-IESLDUM.
           EXEC CICS DELETE
                     DATASET  (IESLDUM)
                     NOHANDLE
           END-EXEC.
       Q48-UNLOCK-IESLDUM.
           EXEC CICS UNLOCK
                     DATASET  (IESLDUM)
                     NOHANDLE
           END-EXEC.
       Q49-EXIT.
           EXIT.

/DLC3/*Q50-INITKEY-VOPERID.
/DLC3/*    IF  ADDRESS OF VOPERID-RECORD = NULL
/DLC3/*        EXEC CICS GETMAIN
/DLC3/*                  SET      (ADDRESS OF VOPERID-RECORD)
/DLC3/*                  LENGTH   (LENGTH OF VOPERID-RECORD)
/DLC3/*        END-EXEC
/DLC3/*    END-IF.
/DLC3/*    MOVE SPACES                 TO VOPERID-RECORD.
/DLC3/*Q56-READEQ-VOPERPU.
/DLC3/*    EXEC CICS READ
/DLC3/*              DATASET  (VOPERPU)
/DLC3/*              INTO     (VOPERID-RECORD)
/DLC3/*              RIDFLD   (VOPERID-USERID)
/DLC3/*              EQUAL
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q56-READUP-VOPERID.
/DLC3/*    EXEC CICS READ
/DLC3/*              DATASET  (VOPERID)
/DLC3/*              INTO     (VOPERID-RECORD)
/DLC3/*              RIDFLD   (VOPERID-CODE)
/DLC3/*              EQUAL
/DLC3/*              UPDATE
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q57-WRITE-VOPERID.
/DLC3/*    EXEC CICS WRITE
/DLC3/*              DATASET  (VOPERID)
/DLC3/*              FROM     (VOPERID-RECORD)
/DLC3/*              RIDFLD   (VOPERID-CODE)
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q57-REWRITE-VOPERID.
/DLC3/*    EXEC CICS REWRITE
/DLC3/*              DATASET  (VOPERID)
/DLC3/*              FROM     (VOPERID-RECORD)
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q58-DELETE-VOPERID.
/DLC3/*    EXEC CICS DELETE
/DLC3/*              DATASET  (VOPERID)
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q58-UNLOCK-VOPERID.
/DLC3/*    EXEC CICS UNLOCK
/DLC3/*              DATASET  (VOPERID)
/DLC3/*              NOHANDLE
/DLC3/*    END-EXEC.
/DLC3/*Q59-EXIT.
/DLC3/*    EXIT.

/DLC0/ Q60-INITKEY-VUSERID.
/DLC0/     IF  ADDRESS OF VUSERID-RECORD = NULL
/DLC0/         EXEC CICS GETMAIN
/DLC0/                   SET      (ADDRESS OF VUSERID-RECORD)
/DLC0/                   LENGTH   (LENGTH OF VUSERID-RECORD)
/DLC0/         END-EXEC
/DLC0/     END-IF.
/DLC0/     MOVE SPACES                 TO VUSERID-RECORD.
/DLC0/     MOVE LOW-VALUES             TO VUSER-KEY.
/DLC0/ Q66-READEQ-VUSERID.
/DLC2/     MOVE SPACES                 TO VUSERID-RECORD(13:).
/DLC0/     EXEC CICS READ
/DLC0/               DATASET  (VUSERID)
/DLC0/               INTO     (VUSERID-RECORD)
/DLC0/               RIDFLD   (VUSER-KEY)
/DLC0/               EQUAL
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q66-READEQ-VUSEROP.
/DLC2/     MOVE SPACES                 TO VUSERID-RECORD(13:).
/DLC0/     EXEC CICS READ
/DLC0/               DATASET  (VUSEROP)
/DLC0/               INTO     (VUSERID-RECORD)
/DLC0/               RIDFLD   (VUSER-OPID)
/DLC0/               EQUAL
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC2/ Q66-READGE-VUSEROP.
/DLC2/     MOVE SPACES                 TO VUSERID-RECORD(13:).
/DLC2/     EXEC CICS READ
/DLC2/               DATASET  (VUSEROP)
/DLC2/               INTO     (VUSERID-RECORD)
/DLC2/               RIDFLD   (VUSER-OPID)
/DLC2/               GTEQ
/DLC2/               NOHANDLE
/DLC2/     END-EXEC.
/DLC0/ Q66-READUP-VUSERID.
/DLC2/     MOVE SPACES                 TO VUSERID-RECORD(13:).
/DLC0/     EXEC CICS READ
/DLC0/               DATASET  (VUSERID)
/DLC0/               INTO     (VUSERID-RECORD)
/DLC0/               RIDFLD   (VUSER-KEY)
/DLC0/               EQUAL
/DLC0/               UPDATE
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q67-WRITE-VUSERID.
/DLC2/     IF  VUSERID-RECORD(128:) = LOW-VALUES
/DLC2/         MOVE SPACES             TO VUSERID-RECORD(128:)
/DLC2/     END-IF.
/DLC0/     EXEC CICS WRITE
/DLC0/               DATASET  (VUSERID)
/DLC0/               FROM     (VUSERID-RECORD)
/DLC0/               RIDFLD   (VUSER-KEY)
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q67-REWRITE-VUSERID.
/DLC2/     IF  VUSERID-RECORD(128:) = LOW-VALUES
/DLC2/         MOVE SPACES             TO VUSERID-RECORD(128:)
/DLC2/     END-IF.
/DLC0/     EXEC CICS REWRITE
/DLC0/               DATASET  (VUSERID)
/DLC0/               FROM     (VUSERID-RECORD)
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q68-DELETE-VUSERID.
/DLC0/     EXEC CICS DELETE
/DLC0/               DATASET  (VUSERID)
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q68-UNLOCK-VUSERID.
/DLC0/     EXEC CICS UNLOCK
/DLC0/               DATASET  (VUSERID)
/DLC0/               NOHANDLE
/DLC0/     END-EXEC.
/DLC0/ Q69-EXIT.
/DLC0/     EXIT.

/DLC2/ Q70-INITKEY-COMASTR.
/DLC2/     IF  ADDRESS OF COMASTR-RECORD = NULL
/DLC2/         EXEC CICS GETMAIN
/DLC2/                   SET      (ADDRESS OF COMASTR-RECORD)
/DLC2/                   LENGTH   (LENGTH OF COMASTR-RECORD)
/DLC2/         END-EXEC
/DLC2/     END-IF.
/DLC2/     MOVE SPACES                 TO COMASTR-RECORD.
/DLC2/ Q76-READEQ-COMASTR.
/DLC2/     EXEC CICS READ
/DLC2/               DATASET(COMASTR)
/DLC2/               INTO(COMASTR-RECORD)
/DLC2/               RIDFLD(COMAST-KEY)
/DLC2/               EQUAL
/DLC2/               NOHANDLE
/DLC2/     END-EXEC.
/DLC2/ Q76-READGE-COMASTA.
/DLC2/     EXEC CICS READ
/DLC2/               DATASET(COMASTA)
/DLC2/               INTO(COMASTR-RECORD)
/DLC2/               RIDFLD(COMAST-AKEY)
/DLC2/               GTEQ
/DLC2/               NOHANDLE
/DLC2/     END-EXEC.
/DLC2/ Q76-READGE-COMASTI.
/DLC2/     EXEC CICS READ
/DLC2/               DATASET(COMASTI)
/DLC2/               INTO(COMASTR-RECORD)
/DLC2/               RIDFLD(COMAST-IKEY)
/DLC2/               GTEQ
/DLC2/               NOHANDLE
/DLC2/     END-EXEC.
/DLC2/ Q76-READGE-COMASTW.
/DLC2/     EXEC CICS READ
/DLC2/               DATASET(COMASTW)
/DLC2/               INTO(COMASTR-RECORD)
/DLC2/               RIDFLD(COMAST-WKEY)
/DLC2/               GTEQ
/DLC2/               NOHANDLE
/DLC2/     END-EXEC.
/DLC2/ Q79-EXIT.
/DLC2/     EXIT.

       Q80-SAVE-FILTERS.
/DLC2/     IF  LIST-OPERID-FILTER-L > ZERO
/DLC2/     OR  LIST-OPERID-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-OPERID-FILTER-I <= SPACES
/DLC2/         AND LIST-OPERID-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-OPERID-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-OPERID-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-OPERID-FILTER-I
/DLC2/                                 TO SAVE-OPERID-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-OPERID-FILTER     TO LIST-OPERID-FILTER-O.

           IF  LIST-USERNAME-FILTER-L > ZERO
           OR  LIST-USERNAME-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-USERNAME-FILTER-I <= SPACES
               AND LIST-USERNAME-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-USERNAME-FILTER
               ELSE
                   MOVE LIST-USERNAME-FILTER-I
                                       TO SAVE-USERNAME-FILTER
               END-IF
           END-IF.
           MOVE SAVE-USERNAME-FILTER   TO LIST-USERNAME-FILTER-O.

           IF  LIST-EXPIRES-FILTER-L > ZERO
           OR  LIST-EXPIRES-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-EXPIRES-FILTER-I <= SPACES
               AND LIST-EXPIRES-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-EXPIRES-FILTER
               ELSE
                   MOVE LIST-EXPIRES-FILTER-I
                                       TO SAVE-EXPIRES-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-EXPIRES-FILTER NOT DATE-FILTER-CHARS
                       MOVE 'Expires filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-EXPIRES-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-EXPIRES-FILTER(1:1) = '<' OR '>')
                   AND SAVE-EXPIRES-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-EXPIRES-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                       MOVE 'Expires filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-EXPIRES-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-EXPIRES-FILTER    TO LIST-EXPIRES-FILTER-O.

           IF  LIST-EXPIRED-FILTER-L > ZERO
           OR  LIST-EXPIRED-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-EXPIRED-FILTER-I <= SPACES
               AND LIST-EXPIRED-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-EXPIRED-FILTER
               ELSE
                   INSPECT LIST-EXPIRED-FILTER-I
                                          CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
                   MOVE LIST-EXPIRED-FILTER-I
                                       TO SAVE-EXPIRED-FILTER
               END-IF
           END-IF.
           MOVE SAVE-EXPIRED-FILTER    TO LIST-EXPIRED-FILTER-O.

           IF  LIST-REVOKED-FILTER-L > ZERO
           OR  LIST-REVOKED-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-REVOKED-FILTER-I <= SPACES
               AND LIST-REVOKED-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-REVOKED-FILTER
               ELSE
                   MOVE LIST-REVOKED-FILTER-I
                                       TO SAVE-REVOKED-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-REVOKED-FILTER NOT DATE-FILTER-CHARS
                       MOVE 'Revoked filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-REVOKED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-REVOKED-FILTER(1:1) = '<' OR '>')
                   AND SAVE-REVOKED-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-REVOKED-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                       MOVE 'Revoked filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-REVOKED-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-REVOKED-FILTER    TO LIST-REVOKED-FILTER-O.

           IF  LIST-SIGNON-FILTER-L > ZERO
           OR  LIST-SIGNON-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-SIGNON-FILTER-I <= SPACES
               AND LIST-SIGNON-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-SIGNON-FILTER
               ELSE
                   MOVE LIST-SIGNON-FILTER-I
                                       TO SAVE-SIGNON-FILTER
                   EVALUATE TRUE
                   WHEN SAVE-SIGNON-FILTER NOT DATE-FILTER-CHARS
                       MOVE 'Signon filter is invalid.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SIGNON-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                   WHEN (SAVE-SIGNON-FILTER(1:1) = '<' OR '>')
                   AND SAVE-SIGNON-FILTER(2:) > SPACES
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SAVE-SIGNON-FILTER(2:)
                                       TO DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     IF  NOT DTE-REQUEST-COMPLETED
                       MOVE 'Signon filter has invalid date format.'
                                       TO MULTI-EDIT-MSG
                       CALL MULTMESG USING DFHEIBLK MULTMESG
                                           ERROR-SWITCH
                                           LIST-SIGNON-FILTER-L
                                           LIST-MESSAGE-L
                                           MULTI-EDIT-MSG
                     END-IF
                   END-EVALUATE
               END-IF
           END-IF.
           MOVE SAVE-SIGNON-FILTER      TO LIST-SIGNON-FILTER-O.

           IF  LIST-USERTYPE-FILTER-L > ZERO
           OR  LIST-USERTYPE-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-USERTYPE-FILTER-I <= SPACES
               AND LIST-USERTYPE-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-USERTYPE-FILTER
               ELSE
                   MOVE LIST-USERTYPE-FILTER-I
                                       TO SAVE-USERTYPE-FILTER
               END-IF
           END-IF.
           MOVE SAVE-USERTYPE-FILTER   TO LIST-USERTYPE-FILTER-O.

           IF  LIST-INITIAL-FILTER-L > ZERO
           OR  LIST-INITIAL-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-INITIAL-FILTER-I <= SPACES
               AND LIST-INITIAL-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-INITIAL-FILTER
               ELSE
                   INSPECT LIST-INITIAL-FILTER-I
                                          CONVERTING LOWER-CASE
                                                  TO UPPER-CASE
                   MOVE LIST-INITIAL-FILTER-I
                                       TO SAVE-INITIAL-FILTER
               END-IF
           END-IF.
           MOVE SAVE-INITIAL-FILTER    TO LIST-INITIAL-FILTER-O.

           IF  LIST-NAMETYPE-FILTER-L > ZERO
           OR  LIST-NAMETYPE-FILTER-F = FLDMODFY OR FLDCURSM
               IF  LIST-NAMETYPE-FILTER-I <= SPACES
               AND LIST-NAMETYPE-FILTER-L  = ZERO
                   MOVE '*'            TO SAVE-NAMETYPE-FILTER
               ELSE
                   MOVE LIST-NAMETYPE-FILTER-I
                                       TO SAVE-NAMETYPE-FILTER
               END-IF
           END-IF.
           MOVE SAVE-NAMETYPE-FILTER   TO LIST-NAMETYPE-FILTER-O.

/DLC2/     IF  LIST-PRIME-CO-FILTER-L > ZERO
/DLC2/     OR  LIST-PRIME-CO-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-PRIME-CO-FILTER-I <= SPACES
/DLC2/         AND LIST-PRIME-CO-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-PRIME-CO-FILTER
/DLC2/         ELSE
/DLC2/             MOVE LIST-PRIME-CO-FILTER-I
/DLC2/                                 TO SAVE-PRIME-CO-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-PRIME-CO-FILTER   TO LIST-PRIME-CO-FILTER-O.

/DLC2/     IF  LIST-PRIME-IND-FILTER-L > ZERO
/DLC2/     OR  LIST-PRIME-IND-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-PRIME-IND-FILTER-I <= SPACES
/DLC2/         AND LIST-PRIME-IND-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-PRIME-IND-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-PRIME-IND-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-PRIME-IND-FILTER-I
/DLC2/                                 TO SAVE-PRIME-IND-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-PRIME-IND-FILTER  TO LIST-PRIME-IND-FILTER-O.

/DLC2/     IF  LIST-WIN-UNIT-FILTER-L > ZERO
/DLC2/     OR  LIST-WIN-UNIT-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-WIN-UNIT-FILTER-I <= SPACES
/DLC2/         AND LIST-WIN-UNIT-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-WIN-UNIT-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-WIN-UNIT-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-WIN-UNIT-FILTER-I
/DLC2/                                 TO SAVE-WIN-UNIT-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-WIN-UNIT-FILTER   TO LIST-WIN-UNIT-FILTER-O.

/DLC2/     IF  LIST-WIN-POS-FILTER-L > ZERO
/DLC2/     OR  LIST-WIN-POS-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-WIN-POS-FILTER-I <= SPACES
/DLC2/         AND LIST-WIN-POS-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-WIN-POS-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-WIN-POS-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-WIN-POS-FILTER-I
/DLC2/                                 TO SAVE-WIN-POS-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-WIN-POS-FILTER    TO LIST-WIN-POS-FILTER-O.

/DLC2/     IF  LIST-WIN-ORG-FILTER-L > ZERO
/DLC2/     OR  LIST-WIN-ORG-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-WIN-ORG-FILTER-I <= SPACES
/DLC2/         AND LIST-WIN-ORG-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-WIN-ORG-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-WIN-ORG-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-WIN-ORG-FILTER-I
/DLC2/                                 TO SAVE-WIN-ORG-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-WIN-ORG-FILTER    TO LIST-WIN-ORG-FILTER-O.

/DLC2/     IF  LIST-WIN-FUNC-FILTER-L > ZERO
/DLC2/     OR  LIST-WIN-FUNC-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-WIN-FUNC-FILTER-I <= SPACES
/DLC2/         AND LIST-WIN-FUNC-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-WIN-FUNC-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-WIN-FUNC-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-WIN-FUNC-FILTER-I
/DLC2/                                 TO SAVE-WIN-FUNC-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-WIN-FUNC-FILTER   TO LIST-WIN-FUNC-FILTER-O.

/DLC2/     IF  LIST-WIN-DIR-FILTER-L > ZERO
/DLC2/     OR  LIST-WIN-DIR-FILTER-F = FLDMODFY OR FLDCURSM
/DLC2/         IF  LIST-WIN-DIR-FILTER-I <= SPACES
/DLC2/         AND LIST-WIN-DIR-FILTER-L  = ZERO
/DLC2/             MOVE '*'            TO SAVE-WIN-DIR-FILTER
/DLC2/         ELSE
/DLC2/             INSPECT LIST-WIN-DIR-FILTER-I
/DLC2/                                    CONVERTING LOWER-CASE
/DLC2/                                            TO UPPER-CASE
/DLC2/             MOVE LIST-WIN-DIR-FILTER-I
/DLC2/                                 TO SAVE-WIN-DIR-FILTER
/DLC2/         END-IF
/DLC2/     END-IF.
/DLC2/     MOVE SAVE-WIN-DIR-FILTER    TO LIST-WIN-DIR-FILTER-O.

       Q85-EXIT.
           EXIT.

       Q90-CHECK-FILTERS.
           SET  WILDCOMP-PGM           TO TRUE.
           SET  RECORD-NOT-SELECTED    TO TRUE.

/DLC2/     IF  SAVE-OPERID-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-OPERID-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-OPERID-FILTER(1:1)
/DLC2/         WHEN '<'
/DLC2/             IF  SAVE-OPERID-FILTER(2:) <= SPACES
/DLC2/               IF  UPM1-OPRIDNT > SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/               END-IF
/DLC2/             ELSE
/DLC2/               IF  UPM1-OPRIDNT <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/               END-IF
/DLC2/               MOVE SAVE-OPERID-FILTER(2:)
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/               MOVE UPM1-OPRIDNT TO WILDCOMP-STR2
/DLC2/               CALL 'WILDCOMP' USING WILDCOMP-PARMS
/DLC2/               IF  NOT WILDCOMP-STR1-GT-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/               END-IF
/DLC2/             END-IF
/DLC2/         WHEN '>'
/DLC2/             IF  SAVE-OPERID-FILTER(2:) <= SPACES
/DLC2/               IF  UPM1-OPRIDNT > SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/               END-IF
/DLC2/             ELSE
/DLC2/               MOVE SAVE-OPERID-FILTER(2:)
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/               MOVE UPM1-OPRIDNT TO WILDCOMP-STR2
/DLC2/               CALL 'WILDCOMP' USING WILDCOMP-PARMS
/DLC2/               IF  NOT WILDCOMP-STR1-LT-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/               END-IF
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-OPERID-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE UPM1-OPRIDNT   TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

           IF  SAVE-USERNAME-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-USERNAME-FILTER
                                       TO WILDCOMP-LEN
               MOVE SAVE-USERNAME-FILTER
                                       TO WILDCOMP-STR1
               INSPECT WILDCOMP-STR1 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               MOVE UPM1-USRNAME       TO WILDCOMP-STR2
               INSPECT WILDCOMP-STR2 CONVERTING LOWER-CASE
                                             TO UPPER-CASE
               CALL 'WILDCOMP'      USING WILDCOMP-PARMS
               IF  NOT WILDCOMP-STR1-EQ-STR2
                   GO TO Q95-EXIT
               END-IF
           END-IF.

           IF  SAVE-EXPIRES-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-EXPIRES-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-EXPIRES-FILTER(1:1)
               WHEN '<'
                   IF  LIST-EXPIRES <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT 365  FROM DTE-TOTD
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-EXPIRES-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF LIST-EXPIRES
                                       TO TXT-PARM-STRLEN
                     MOVE LIST-EXPIRES TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  LIST-EXPIRES <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-EXPIRES-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF LIST-EXPIRES
                                       TO TXT-PARM-STRLEN
                     MOVE LIST-EXPIRES TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-EXPIRES-FILTER
                                       TO WILDCOMP-STR1
                   MOVE LIST-EXPIRES   TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-EXPIRED-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-EXPIRED-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-EXPIRED-FILTER
               WHEN '<'
                   IF  LIST-EXPIRED NOT = '*'
                       GO TO Q95-EXIT
                   END-IF
               WHEN '>'
                   IF  LIST-EXPIRED <= SPACES
                   OR  LIST-EXPIRED  = '*'
                       GO TO Q95-EXIT
                   END-IF
               WHEN OTHER
                   MOVE SAVE-EXPIRED-FILTER
                                       TO WILDCOMP-STR1
                   MOVE LIST-EXPIRED   TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-REVOKED-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-REVOKED-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-REVOKED-FILTER(1:1)
               WHEN '<'
                   IF  UPM1-REVOKED <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD TO DTE-GREG
                       CALL 'DTEMAN'  USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT  365   FROM DTE-TOTD
                       CALL 'DTEMAN'  USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-REVOKED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF UPM1-REVOKED
                                       TO TXT-PARM-STRLEN
                     MOVE UPM1-REVOKED TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  UPM1-REVOKED <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-REVOKED-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF UPM1-REVOKED
                                       TO TXT-PARM-STRLEN
                     MOVE UPM1-REVOKED TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-REVOKED-FILTER
                                       TO WILDCOMP-STR1
                   MOVE UPM1-REVOKED   TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-SIGNON-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-SIGNON-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-SIGNON-FILTER(1:1)
               WHEN '<'
                   IF  LIST-SIGNON <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '<'
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE CICS-CCYYMMDD TO DTE-GREG
                       CALL 'DTEMAN'  USING DTEMAN-PARMS
                       SET DTE-REQUEST-TOTL-DAYS TO TRUE
                       SUBTRACT  365   FROM DTE-TOTD
                       CALL 'DTEMAN'  USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-SIGNON-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF LIST-SIGNON
                                       TO TXT-PARM-STRLEN
                     MOVE LIST-SIGNON  TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 < WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  LIST-SIGNON <= SPACES
                     GO TO Q95-EXIT
                   ELSE
                     IF  SAVE-EXPIRES-FILTER = '>'
                       MOVE CICS-CCYYMMDD TO WILDCOMP-STR1
                     ELSE
                       SET DTE-REQUEST-GREG-EDIT TO TRUE
                       MOVE SAVE-SIGNON-FILTER(2:)
                                       TO DTE-GREG
                       CALL 'DTEMAN' USING DTEMAN-PARMS
                       MOVE DTE-CCYYMMDD TO WILDCOMP-STR1
                     END-IF
                     SET TXT-REQUEST-JUSTIFY-L TO TRUE
                     MOVE LOW-VALUES   TO TXT-DLMS
                     MOVE '/'          TO TXT-DLM1
                     MOVE LENGTH OF LIST-SIGNON
                                       TO TXT-PARM-STRLEN
                     MOVE LIST-SIGNON  TO TXT-PARM-STRING
                     CALL 'TXTMAN'  USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
                     MOVE TXT-PARM-STRING(1:6) TO WRK-DATE6
                     SET DTE-REQUEST-GREG-EDIT TO TRUE
                     MOVE SPACES       TO DTE-GREG
                     MOVE CORR WRK-DATE6 TO DTE-GREG
                     MOVE DTE-GRYY IN WRK-DATE6
                                       TO DTE-GRYY IN DTE-GREG
                     CALL 'DTEMAN'  USING DTEMAN-PARMS
                     MOVE DTE-CCYYMMDD TO WILDCOMP-STR2
                     IF  WILDCOMP-STR1 > WILDCOMP-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-SIGNON-FILTER
                                       TO WILDCOMP-STR1
                   MOVE LIST-SIGNON    TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-USERTYPE-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-USERTYPE-FILTER
                                       TO WILDCOMP-LEN
               MOVE SAVE-USERTYPE-FILTER
                                       TO WILDCOMP-STR1
               MOVE UPM1-USRTYPE       TO WILDCOMP-STR2
               CALL 'WILDCOMP'      USING WILDCOMP-PARMS
               IF  NOT WILDCOMP-STR1-EQ-STR2
                   GO TO Q95-EXIT
               END-IF
           END-IF.

           IF  SAVE-INITIAL-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-INITIAL-FILTER
                                       TO WILDCOMP-LEN
               EVALUATE SAVE-INITIAL-FILTER(1:1)
               WHEN '<'
                   IF  SAVE-INITIAL-FILTER(2:) <= SPACES
                     IF  UPM1-INITNAM > SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     IF  UPM1-INITNAM <= SPACES
                       GO TO Q95-EXIT
                     END-IF
                     MOVE SAVE-INITIAL-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE UPM1-INITNAM TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-GT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN '>'
                   IF  SAVE-INITIAL-FILTER(2:) <= SPACES
                     IF  UPM1-INITNAM > SPACES
                       GO TO Q95-EXIT
                     END-IF
                   ELSE
                     MOVE SAVE-INITIAL-FILTER(2:)
                                       TO WILDCOMP-STR1
                     MOVE UPM1-INITNAM TO WILDCOMP-STR2
                     CALL 'WILDCOMP' USING WILDCOMP-PARMS
                     IF  NOT WILDCOMP-STR1-LT-STR2
                       GO TO Q95-EXIT
                     END-IF
                   END-IF
               WHEN OTHER
                   MOVE SAVE-INITIAL-FILTER
                                       TO WILDCOMP-STR1
                   MOVE UPM1-INITNAM   TO WILDCOMP-STR2
                   CALL 'WILDCOMP'  USING WILDCOMP-PARMS
                   IF  NOT WILDCOMP-STR1-EQ-STR2
                       GO TO Q95-EXIT
                   END-IF
               END-EVALUATE
           END-IF.

           IF  SAVE-NAMETYPE-FILTER NOT = '*'
               MOVE LENGTH OF SAVE-NAMETYPE-FILTER
                                       TO WILDCOMP-LEN
               MOVE SAVE-NAMETYPE-FILTER
                                       TO WILDCOMP-STR1
               MOVE UPM1-INITYPE       TO WILDCOMP-STR2
               CALL 'WILDCOMP'      USING WILDCOMP-PARMS
               IF  NOT WILDCOMP-STR1-EQ-STR2
                   GO TO Q95-EXIT
               END-IF
           END-IF.

/DLC2/     IF  SAVE-PRIME-CO-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-PRIME-CO-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-PRIME-CO-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-PRIMARY-CO <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-PRIME-CO-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-PRIMARY-CO
/DLC2/                                 TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-PRIME-IND-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-PRIME-IND-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-PRIME-IND-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-PRIMARY-IND <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-PRIME-IND-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-PRIMARY-IND
/DLC2/                                 TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-WIN-UNIT-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-WIN-UNIT-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-WIN-UNIT-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-WIN-UNIT <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-WIN-UNIT-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-WIN-UNIT TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-WIN-POS-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-WIN-POS-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-WIN-POS-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-WIN-POSITION <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-WIN-POS-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-WIN-POSITION
/DLC2/                                 TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-WIN-ORG-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-WIN-ORG-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-WIN-ORG-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-WIN-ORG <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-WIN-ORG-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-WIN-ORG  TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-WIN-FUNC-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-WIN-FUNC-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-WIN-FUNC-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-WIN-FUNCTION <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-WIN-FUNC-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-WIN-FUNCTION
/DLC2/                                 TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

/DLC2/     IF  SAVE-WIN-DIR-FILTER NOT = '*'
/DLC2/         MOVE LENGTH OF SAVE-WIN-DIR-FILTER
/DLC2/                                 TO WILDCOMP-LEN
/DLC2/         EVALUATE SAVE-WIN-DIR-FILTER
/DLC2/         WHEN '>'
/DLC2/             IF  VUSER-WIN-DIRECTOR <= SPACES
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         WHEN OTHER
/DLC2/             MOVE SAVE-WIN-DIR-FILTER
/DLC2/                                 TO WILDCOMP-STR1
/DLC2/             MOVE VUSER-WIN-DIRECTOR
/DLC2/                                 TO WILDCOMP-STR2
/DLC2/             CALL 'WILDCOMP'  USING WILDCOMP-PARMS
/DLC2/             IF  NOT WILDCOMP-STR1-EQ-STR2
/DLC2/                 GO TO Q95-EXIT
/DLC2/             END-IF
/DLC2/         END-EVALUATE
/DLC2/     END-IF.

           SET  RECORD-IS-SELECTED     TO TRUE.
       Q95-EXIT.
           EXIT.

      /*****************************************************************
      *    EVEN MORE PERFORMED ROUTINES                                *
      ******************************************************************

       R10-FIX-APOSTROPHE.
           MOVE UNEX-LENG              TO TXT-MSTR-BUFLEN.
           MOVE UNEX-VARIABLE          TO TXT-MSTR-BUFFER.
           MOVE ZEROES                 TO TXT-PNTR.

           PERFORM WITH TEST AFTER UNTIL TXT-STRING-NOT-FOUND
             SET  TXT-REQUEST-FIND     TO TRUE
             MOVE 1                    TO TXT-PARM-STRLEN
             MOVE ''''                 TO TXT-PARM-STRING
             CALL 'TXTMAN'          USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
             IF  TXT-STRING-WAS-FOUND
               SET  TXT-REQUEST-INSERT TO TRUE
               ADD  1                  TO TXT-MSTR-BUFLEN
               CALL 'TXTMAN'        USING TXTMAN-PARMS
                                          TXTMAN-BUFFER
                                          TXTMAN-STRING
             END-IF
           END-PERFORM.

           MOVE TXT-MSTR-BUFLEN        TO UNEX-LENG.
           MOVE TXT-MSTR-BUFFER        TO UNEX-VARIABLE.
       R15-EXIT.
           EXIT.

/DLC2/ R20-CHECK-AD-ATTRIBUTES.
/DLC2/     IF  VUSER-WIN-FUNCTION <= SPACES
/DLC2/         GO TO R25-EXIT
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-FUNCTION NOT = 'WW' AND 'WG' AND 'WS'
/DLC2/                              AND 'CO' AND 'AC' AND 'LC'
/DLC2/                              AND 'VP' AND 'XT'
/DLC2/         MOVE EXCRED             TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF (VUSER-WIN-FUNCTION = 'WW' OR 'WG' OR 'WS'
/DLC2/                           OR 'CO' OR 'VP')
/DLC2/     AND VUSER-WIN-UNIT NOT = 'ACCT'       AND 'AL'
/DLC2/                          AND 'AP'         AND 'COMM'
/DLC2/                          AND 'CONTRACTOR' AND 'CUSTSVC'
/DLC2/                          AND 'DTAMGMT'    AND 'EXEC'
/DLC2/                          AND 'FIN'        AND 'HR'
/DLC2/                          AND 'INVSTR'     AND 'IT'
/DLC2/                          AND 'MKT'        AND 'PRICING'
/DLC2/                          AND 'PURCHS'     AND 'ROMS'
/DLC2/                          AND 'SPS'        AND 'TRAINING'
/DLC2/                          AND 'WCMS'       AND 'WINSUP'
/DLC2/                          AND 'WSS'        AND 'DAYTON'
/DLC2/                          AND 'DENVER'     AND 'JACKSONVLE'
/DLC2/*DLC7                     AND 'NHAVEN'     AND 'RCHMND'
/DLC7/                          AND 'NHAVEN'     AND 'OKCITY'
/DLC7/                                           AND 'RCHMND'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-UNIT-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF (VUSER-WIN-FUNCTION = 'AC' OR 'LC' OR 'XT')
/DLC2/     AND (VUSER-WIN-UNIT    = 'ACCT'       OR 'AL'
/DLC2/                           OR 'AP'         OR 'COMM'
/DLC2/                           OR 'CONTRACTOR' OR 'CUSTSVC'
/DLC2/                           OR 'DTAMGMT'    OR 'EXEC'
/DLC2/                           OR 'FIN'        OR 'HR'
/DLC2/                           OR 'INVSTR'     OR 'IT'
/DLC2/                           OR 'MKT'        OR 'PRICING'
/DLC2/                           OR 'PURCHS'     OR 'ROMS'
/DLC2/                           OR 'SPS'        OR 'TRAINING'
/DLC2/                           OR 'WCMS'       OR 'WINSUP'
/DLC2/                           OR 'WSS'        OR 'DAYTON'
/DLC2/                           OR 'DENVER'     OR 'JACKSONVLE'
/DLC2/*DLC7                      OR 'NHAVEN'     OR 'RCHMND')
/DLC7/                           OR 'NHAVEN'     OR 'OKCITY'
/DLC7/                                           OR 'RCHMND')
/DLC2/         MOVE EXCPINK            TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-UNIT-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-POSITION NOT = 'EMPL' AND 'PRES' AND 'SIC'
/DLC2/         MOVE EXCRED             TO LIST-WIN-POSITION-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-POSITION     = 'PRES'
/DLC2/     AND VUSER-WIN-DIRECTOR NOT = 'Y'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-POSITION-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-DIRECTOR-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-POSITION     = 'SIC'
/DLC2/     AND VUSER-WIN-FUNCTION NOT = 'LC'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-POSITION-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-FUNCTION     = 'VP'
/DLC2/     AND VUSER-WIN-UNIT     NOT = 'AL' AND 'EXEC' AND 'MKT'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-UNIT-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-UNIT         = 'EXEC'
/DLC2/     AND VUSER-WIN-FUNCTION NOT = 'WW' AND 'WS' AND 'WG'
/DLC2/                              AND 'VP' AND 'CO'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-UNIT-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-UNIT         = 'AL'
/DLC2/     AND VUSER-WIN-FUNCTION NOT = 'VP'
/DLC2/         MOVE EXCPINK            TO LIST-WIN-UNIT-C(IDX)
/DLC2/         MOVE EXCRED             TO LIST-WIN-FUNCTION-C(IDX)
/DLC2/     END-IF.

/DLC2/     PERFORM Q70-INITKEY-COMASTR.
/DLC2/     MOVE VUSER-PRIMARY-CO       TO COMAST-KEY.
/DLC2/     PERFORM Q76-READEQ-COMASTR.
/DLC2/     IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/         MOVE EXCRED             TO LIST-PRIMARY-CO-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-PRIMARY-IND NOT = COMAST-IKEY-IND
/DLC2/     AND VUSER-PRIMARY-IND NOT = 'B'
/DLC2/         MOVE EXCRED             TO LIST-PRIMARY-IND-C(IDX)
/DLC2/         MOVE EXCPINK            TO LIST-PRIMARY-CO-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-FUNCTION = 'AC'
/DLC2/     AND VUSER-WIN-ORG NOT = COMAST-AKEY-AREA
/DLC2/         MOVE EXCRED             TO LIST-WIN-ORG-C(IDX)
/DLC2/         MOVE EXCPINK            TO LIST-PRIMARY-CO-C(IDX)
/DLC2/     END-IF.

/DLC2/     MOVE VUSER-PRIMARY-IND      TO COMAST-IKEY-IND.
/DLC2/     MOVE ALL ZEROES             TO COMAST-IKEY-CO.
/DLC2/     PERFORM Q76-READGE-COMASTI.
/DLC2/     IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/     OR  COMAST-IKEY-IND NOT = VUSER-PRIMARY-IND
/DLC2/         MOVE DFHRESP(NORMAL)    TO EIBRESP
/DLC2/         MOVE EXCRED             TO LIST-PRIMARY-IND-C(IDX)
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-FUNCTION = 'AC'
/DLC2/       MOVE VUSER-WIN-ORG        TO COMAST-AKEY-AREA
/DLC2/       MOVE ALL ZEROES           TO COMAST-AKEY-CO
/DLC2/       PERFORM Q76-READGE-COMASTA
/DLC2/       IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/       OR  COMAST-AKEY-AREA NOT = VUSER-WIN-ORG(1:3)
/DLC2/         MOVE DFHRESP(NORMAL)    TO EIBRESP
/DLC2/         MOVE EXCRED             TO LIST-WIN-ORG-C(IDX)
/DLC2/       END-IF
/DLC2/     END-IF.

/DLC2/     IF  VUSER-WIN-UNIT = 'AL'
/DLC2/       MOVE VUSER-WIN-ORG        TO COMAST-WKEY-WIN
/DLC2/       MOVE ALL ZEROES           TO COMAST-WKEY-CO
/DLC2/       PERFORM Q76-READGE-COMASTW
/DLC2/       IF  EIBRESP NOT = DFHRESP(NORMAL)
/DLC2/       OR  COMAST-WKEY-WIN NOT = VUSER-WIN-ORG(1:1)
/DLC2/         MOVE DFHRESP(NORMAL)    TO EIBRESP
/DLC2/         MOVE EXCRED             TO LIST-WIN-ORG-C(IDX)
/DLC2/       END-IF
/DLC2/     END-IF.
/DLC2/ R25-EXIT.
/DLC2/     EXIT.

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
           COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (003 - 1).
           STRING SCRSETBA TWO-BYTES SCRSTFEX X'02'
                  TYPFIELD FLDASBRT  TYPHILIT EXHBLINK
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           STRING MULTI-EDIT-MSG(1:LEN)
               DELIMITED BY SIZE INTO VAR-DATA WITH POINTER POS.
           EVALUATE TRUE
           WHEN LEN < LENGTH OF MULTI-EDIT-MSG - 2
      *                             row  adjust            col  adjust
               COMPUTE HALF-WORD = (025 - 1) * SCRNWDTH + (082 - 1)
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
      *    PROGRAM EXIT ROUTINES (NORMAL)                              *
      ******************************************************************

       Z70-VUPM-CLOSED.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

           COPY COMMXCTL.
      * the above transfers control to standard NOT OPEN screen

       Z80-VUPM-RETURN.

           COPY COMMRETN.
      * the above copybook is a psuedo-conversational return to cics

       Z90-VUPM-TERMINATION.

           EXEC CICS SET TERMINAL(EIBTRMID)
                     UCTRAN
                     NOHANDLE
           END-EXEC.

      * use the following if you don't want to pass back any data
           COPY COMMEXIT REPLACING DFHCOMMAREA BY COMM-HEADER.

      * the above copybook ends with a return to cics and GOBACK


* $$ JOB JNM=XXXCNTLR,DISP=D,CLASS=C
* $$ LST LST=SYSLST,DISP=D,CLASS=Z
// JOB    IESCNTLR   Revoke IESCNTL Users Disabled in AD
/* LIBDEF *,SEARCH=(ESP.MKSXRA,DAP.PROD),TEMP
/* OPTION DUMP
// UPSI   XXXXXXX1   /* 0 = REPORT, ONLY; 1 = UPDATE IESCNTL FILE */
// UPSI   XXXXXX0X   /* 0 = REPORT; 1 = DISABLE IF MISSING IN AD */
// UPSI   0XXXXXXX   /* 0 = TO SYSLST, ONLY; 1 = SEND EMAIL, TOO */
// EXEC   PGM=IESCNTLR,SIZE=*
/* EXEC   PGM=XRAY,SIZE=768K
/* XRAY MODE=DIRECT PROG=IESCNTLR LU=DAP9I54G
/* EOD
/& EOJ
* $$ EOJ

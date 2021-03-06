* $$ JOB JNM=IESCNTLP,DISP=L,CLASS=C,USER='*DO NOT DELETE*'
* $$ LST LST=SYSLST,DISP=D,CLASS=Z,DEST=*,PURGE=0
// JOB    IESCNTLP   AGE EXPIRED/REVOKED IESCNTL/BSTCNTL RECS
// UPSI   XXXXXXX1           /* 0 = REPORT, ONLY; 1 = PURGE */
/. STEP01 - CLOSE FILES TO CICS (MBCBATCH)
// EXEC   PROC=MBCCNTL
// EXEC   PGM=MBCBATCH,SIZE=*,OS390
   CICS ALL
   CLOS   IESLDUM
   CLOS   IESLDUV
/* EOD
   SET    MRCZERO
/. STEP02 - AGE THE IESCNTL FILE (IESCNTLP)
* $$ LST LST=SYSLST,DISP=D,CLASS=Z,DEST=*
// ON     $ABEND OR $CANCEL GOTO STEP04
// ON     $RC >= 16 GOTO STEP04
// EXEC   PGM=IESCNTLP
/* EOD
// IF     $RC �= 2 THEN
// GOTO   STEP04
/. STEP03 - REFRESH BSM DATASPACE (BSTADMIN)
// EXEC   PGM=BSTADMIN,SIZE=*
   PERFORM DATASPACE REFRESH
/* EOD
/. STEP04 - OPEN FILES TO CICS (MBCBATCH)
* $$ LST LST=SYSLST,DISP=D,CLASS=Z,DEST=*,PURGE=0
// EXEC   PROC=MBCCNTL
// EXEC   PGM=MBCBATCH,SIZE=*,OS390
   CICS ALL
   OPEN   IESLDUM
   OPEN   IESLDUV
/* EOD
   SET    MRCZERO
/& EOJ
* $$ EOJ

* $$ JOB JNM=XXAUDRPT,DISP=D,CLASS=C                                            
* $$ LST LST=SYSLST,DISP=D,CLASS=Z,FNO=L913                                     
// JOB    VUSRAUDR   zVSE User Security Auditor's Report                        
// ASSGN  SYS012,SYSLST                                                         
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='FUNC=&func'                                 
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='UNIT=&unit'                                 
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='POSN=&posn'                                 
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='TYPE=<1|2|3>'                               
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='CODE=d'                                     
/* EXEC   PGM=VUSRAUDR,SIZE=*,PARM='CLASS=nn'                                   
// EXEC   PGM=VUSRAUDR,SIZE=*,PARM='USER=<*ALL|&mfid>'                          
/* EOD                                                                          
/& EOJ                                                                          
* $$ EOJ                                                                        

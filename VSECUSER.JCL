* $$ JOB JNM=VSECUSER,DISP=D,CLASS=0                                            
* $$ LST LST=SYSLST,DISP=D,CLASS=Z                                              
// JOB    VSECUSER   CICS RESOURCE SECURITY USERNAME INIT                       
/* ------------------------------------------------------------- */             
/* IN THE UPSI SETTINGS FOLLOWING (IF SET ON, I.E., SET TO 1):   */             
/* UPSI7 = PERFORM FOR UPDATE                                    */             
/* ------------------------------------------------------------- */             
// EXEC   PGM=VSECUSER,SIZE=*,PARM='/UPSI(00000001)'                            
/* EOD                                                                          
/& EOJ                                                                          
* $$ EOJ                                                                        

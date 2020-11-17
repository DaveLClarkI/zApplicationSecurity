* $$ JOB JNM=XXUSRATR,DISP=D,CLASS=C                                            
* $$ LST LST=SYSLST,DISP=D,CLASS=Z                                              
// JOB    VUSRATTR   Get Active Directory Attrs for User(s)                     
// UPSI   0          /* 0 = CONDITIONAL, 1 = FORCED REFRESH */                  
// EXEC   PGM=VUSRATTR,SIZE=*          /* ALL USERS */                          
/* EXEC   PGM=VUSRATTR,PARM='&mfid'    /* SINGLE USER */                        
/* EOD                                                                          
/& EOJ                                                                          
* $$ EOJ                                                                        

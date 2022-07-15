//FOUR022X JOB 'COMP CO',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID              00010002
// JCLLIB ORDER=GR.GERAL.PROCLIB                                        00130001
//COBCOMP  EXEC DFHCOBLB,LIB1=GR.FOUR022,NOME='FR22RELB',LIB2=GR.GERAL  00140006
//SYSTERM  DD  SYSOUT=*                                                 00141000
//LKED.SYSIN    DD   *                                                  00150000
  NAME FR22RELB (R) *                                                   00160009
/*                                                                      00161002
//*--------------------------------------------------------------------*00170000
//*          FIM JOB DE COMPILACAO COBOL BATCH                         *00180000
//*--------------------------------------------------------------------*00190000

//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010000
//EXECUTAR EXEC PGM=ARQ2205                                             00020000
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00030000
//ALUNOX   DD DSN=PR.FOUR022.ALUNOS,DISP=OLD                            00040009
//LOGERROS DD DSN=PR.FOUR022.LOGERROS,DISP=OLD                          00050001
//RELREC   DD SYSOUT=*                                                  00060002

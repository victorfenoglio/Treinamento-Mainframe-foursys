//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010000
//EXECUTAR EXEC PGM=ARQ2205                                             00020000
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00030000
//ALUNOS   DD DSN=PR.FOUR022.ALUNOS,DISP=SHR                            00031000
//RELREC   DD SYSOUT=*                                                  00032002

//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010000
//EXECUTAR EXEC PGM=ARQ2203                                             00020001
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00030000
//JCLCLI   DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00031000
//SYSIN    DD *                                                         00032000

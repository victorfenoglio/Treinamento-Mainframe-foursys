//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010001
//EXECUTAR EXEC PGM=PROGX                                               00020006
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00030001
//JCLCLI   DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00031002
//SYSIN    DD *                                                         00040001

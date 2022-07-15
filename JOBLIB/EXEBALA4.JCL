//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010000
//STEP01   EXEC PGM=SORT                                                00011000
//SORTIN   DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00012000
//SORTOUT  DD DSN=PR.FOUR022.CLIENTES,DISP=OLD                          00013000
//SYSOUT   DD SYSOUT=*                                                  00013100
//SYSIN    DD *                                                         00013200
  SORT FIELDS=(01,08,CH,A)                                              00013300
//*----------------------------------------                             00013400
//STEP02   EXEC PGM=SORT                                                00013500
//SORTIN   DD DSN=PR.FOUR022.MOV0106,DISP=SHR                           00013600
//SORTOUT  DD DSN=PR.FOUR022.MOV0106,DISP=OLD                           00013700
//SYSOUT   DD SYSOUT=*                                                  00013800
//SYSIN    DD *                                                         00013900
  SORT FIELDS=(01,08,CH,A,41,01,CH,A)                                   00014000
//*----------------------------------------                             00014100
//STEP03   EXEC PGM=FR22EX04                                            00014200
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00014300
//CLIENTES DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00014400
//MOV0106  DD DSN=PR.FOUR022.MOV0106,DISP=SHR                           00014500
//LOGERROS DD DSN=PR.FOUR022.LOGERROS,DISP=OLD                          00014600
//MOV0106A DD DSN=PR.FOUR022.MOV0106A,DISP=(OLD)                        00014800
//MOV0106C DD DSN=PR.FOUR022.MOV0106C,DISP=(OLD)                        00014900

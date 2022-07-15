//FOUR022X JOB 'COMP COB',TIME=(0,01),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010000
//STEP01   EXEC PGM=SORT                                                00011000
//SORTIN   DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00012000
//SORTOUT  DD DSN=PR.FOUR022.CLIENTES,DISP=OLD                          00013000
//SYSOUT   DD SYSOUT=*                                                  00013100
//SYSIN    DD *                                                         00013200
  SORT FIELDS=(01,08,CH,A)                                              00013300
//*----------------------------------------                             00013400
//STEP02   EXEC PGM=SORT                                                00013500
//SORTIN   DD DSN=PR.FOUR022.MOV3105,DISP=SHR                           00013600
//SORTOUT  DD DSN=PR.FOUR022.MOV3105,DISP=OLD                           00013700
//SYSOUT   DD SYSOUT=*                                                  00013800
//SYSIN    DD *                                                         00013900
  SORT FIELDS=(01,08,CH,A,41,01,CH,A)                                   00014004
//*----------------------------------------                             00014100
//STEP03   EXEC PGM=FR22EX03                                            00014200
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00014300
//CLIENTES DD DSN=PR.FOUR022.CLIENTES,DISP=SHR                          00014402
//MOV3105  DD DSN=PR.FOUR022.MOV3105,DISP=SHR                           00014503
//LOGERROS DD DSN=PR.FOUR022.LOGERROS,DISP=OLD                          00014600
//*CLI3105 DD SYSOUT=*                                                  00014705
//CLI3105  DD DSN=PR.FOUR022.CLI3105,DISP=(OLD)                         00014805

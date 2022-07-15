//FOUR022X JOB 'COMP COB',TIME=(0,02),MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID 00010011
//STEP01   EXEC PGM=SORT                                                00011004
//SORTIN   DD DSN=PR.FOUR022.FUNC,DISP=SHR                              00012004
//SORTOUT  DD DSN=PR.FOUR022.FUNC,DISP=OLD                              00013004
//SYSOUT   DD SYSOUT=*                                                  00013104
//SYSIN    DD *                                                         00013204
  SORT FIELDS=(01,05,CH,A)                                              00013304
//*----------------------------------------                             00013404
//STEP02   EXEC PGM=SORT                                                00013504
//SORTIN   DD DSN=PR.FOUR022.MOVPROJ,DISP=SHR                           00013604
//SORTOUT  DD DSN=PR.FOUR022.MOVPROJ,DISP=OLD                           00013704
//SYSOUT   DD SYSOUT=*                                                  00013804
//SYSIN    DD *                                                         00013904
  SORT FIELDS=(01,05,CH,A)                                              00014004
//*----------------------------------------                             00014104
//STEP03   EXEC PGM=FR22EX02                                            00014204
//STEPLIB  DD DSN=PR.FOUR022.OBJ,DISP=SHR                               00014300
//FUNC     DD DSN=PR.FOUR022.FUNC,DISP=SHR                              00014403
//MOVPROJ  DD DSN=PR.FOUR022.MOVPROJ,DISP=SHR                           00014503
//LOGERROS DD DSN=PR.FOUR022.LOGERROS,DISP=OLD                          00014600
//*FUNPROJ3 DD SYSOUT=*                                                 00014714
//FUNPROJ3 DD DSN=PR.FOUR022.FUNPROJ3,DISP=(OLD)                        00014814
//*FUNPROJ3 DD DSN=PR.FOUR022.FUNPROJ3,DISP=(NEW,CATLG,DELETE),         00014913
//*         SPACE=(TRK,(1,1)),                                          00015013
//*         DCB=(RECFM=FB,LRECL=58,BLKSIZE=0)                           00016013

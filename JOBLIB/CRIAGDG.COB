//FOUR022X  JOB 'CRIAGDG',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID,TIME=(0,01) 00010000
//STEP1     EXEC PGM=IDCAMS                                             00020000
//SYSPRINT  DD   SYSOUT=*                                               00030000
//SYSIN     DD   *                                                      00040000
  DEFINE GENERATIONDATAGROUP -                                          00050000
     (NAME(GR.FOUR022.EXCSAIDA) -                                       00060000
     NOEMPTY   -                                                        00070000
     NOSCRATCH -                                                        00080000
     LIMIT(03)  )                                                       00090000

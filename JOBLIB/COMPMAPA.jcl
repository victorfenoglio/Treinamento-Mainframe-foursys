//FOUR022X JOB 'COMP MAPA',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID            00010003
//*================================================================     00020000
//* JCL PARA COMPILAR MAPAS                                             00030000
//*  NAME - SEU MAPA   -   INSTR = SUA CHAVE                            00040000
//*================================================================     00050000
//         JCLLIB ORDER=GR.GERAL.PROCLIB                                00060000
//MAPSAMP  EXEC DFHMAPCS,NAME=F22CIM1,INSTR=FOUR022                     00070004

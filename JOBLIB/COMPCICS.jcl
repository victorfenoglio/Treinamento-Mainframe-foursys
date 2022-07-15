//FOUR022X JOB 'ALUNO',MSGCLASS=X,CLASS=C,NOTIFY=FOUR022                00010001
//*=================================================                    00020000
//* JCL PARA COMPILAR PROGRAMAS CICS                                    00030000
//*  NAME - NOME DO SEU PROGRAMA                                        00040000
//*  INSTR - SUA CHAVE                                                  00050000
//*=================================================                    00060000
//         JCLLIB ORDER=GR.GERAL.PROCLIB                                00070000
//*                                                                     00080000
//COMPPLI  EXEC DFHCICS1,NAME=F22CIP1,INSTR=FOUR022,                    00090002
//         LIB2='GR.GERAL'                                              00100000

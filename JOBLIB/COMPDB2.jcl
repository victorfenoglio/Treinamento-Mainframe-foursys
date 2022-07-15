//FOUR022X JOB 'COMP DB2',MSGCLASS=X,CLASS=C,NOTIFY=&SYSUID
//*-------------------------------------------------------------------*
//*      JOB DE COMPILACAO DE PROGRAMA COBOL COM DB2 BATCH            *
//*      NAME = SEU PROGRAMA  -   INSTR = SUA CHAVE
//*-------------------------------------------------------------------*
//         JCLLIB ORDER=GR.GERAL.PROCLIB
//COMPDB2B EXEC DFHCODB2,NAME=FR22DB10,INSTR=FOUR022
//LKED.SYSIN    DD   *
  NAME FR22DB10(R)
//*
//BIND.SYSTSIN  DD *
 DSN  SYSTEM(DB8G)
 BIND PLAN(FR22DB10) MEM(FR22DB10) ACT(REP) ISOLATION(CS) -
       LIB('DSN810.DBRMLIB.DATA')
 END

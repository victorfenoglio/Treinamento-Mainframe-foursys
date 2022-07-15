F22CIM1 DFHMSD TYPE=MAP,MODE=INOUT,LANG=COBOL,CTRL=FREEKB,TIOAPFX=YES, *00010010
               MAPATTS=(COLOR)                                          00011010
MAPA01  DFHMDI SIZE=(24,80)                                             00020000
*------------------------------------------------------------------     00030000
        DFHMDF POS=(01,30),LENGTH=23,INITIAL='======================='  00031009
        DFHMDF POS=(02,30),LENGTH=23,INITIAL='|CONSULTA FUNCIONARIOS|',*00040010
               COLOR=RED                                                00040110
        DFHMDF POS=(03,30),LENGTH=23,INITIAL='======================='  00041009
*------------------------------------------------------------------     00050000
        DFHMDF POS=(05,15),LENGTH=10,INITIAL='ID.......:',COLOR=GREEN   00060010
ID      DFHMDF POS=(05,26),LENGTH=05,ATTRB=(UNPROT,IC,NUM)              00070000
        DFHMDF POS=(05,32),LENGTH=01,ATTRB=ASKIP                        00080000
*------------------------------------------------------------------     00090000
        DFHMDF POS=(07,15),LENGTH=10,INITIAL='NOME.....:',COLOR=GREEN   00100010
NOME    DFHMDF POS=(07,26),LENGTH=30,ATTRB=UNPROT                       00110000
        DFHMDF POS=(07,72),LENGTH=01,ATTRB=ASKIP                        00120000
*------------------------------------------------------------------     00130000
        DFHMDF POS=(09,15),LENGTH=10,INITIAL='SETOR....:',COLOR=GREEN   00131010
SETOR   DFHMDF POS=(09,26),LENGTH=04,ATTRB=UNPROT                       00132002
        DFHMDF POS=(09,31),LENGTH=01,ATTRB=ASKIP                        00133002
*------------------------------------------------------------------     00134002
        DFHMDF POS=(11,15),LENGTH=10,INITIAL='SALARIO..:',COLOR=GREEN   00135010
SALARIO DFHMDF POS=(11,26),LENGTH=10,ATTRB=(UNPROT,NUM),               *00136018
               PICOUT='9(08)V99'                                        00136118
        DFHMDF POS=(11,37),LENGTH=01,ATTRB=ASKIP                        00137002
*------------------------------------------------------------------     00138002
        DFHMDF POS=(13,15),LENGTH=10,INITIAL='DATAADM..:',COLOR=GREEN   00138110
DATADIA DFHMDF POS=(13,26),LENGTH=02,ATTRB=(UNPROT,NUM)                 00138206
        DFHMDF POS=(13,29),LENGTH=01,INITIAL='/',ATTRB=ASKIP            00138306
DATAMES DFHMDF POS=(13,31),LENGTH=02,ATTRB=(UNPROT,NUM)                 00138406
        DFHMDF POS=(13,34),LENGTH=01,INITIAL='/',ATTRB=ASKIP            00138506
DATAANO DFHMDF POS=(13,36),LENGTH=04,ATTRB=(UNPROT,NUM)                 00138607
        DFHMDF POS=(13,41),LENGTH=01,ATTRB=ASKIP                        00138707
*------------------------------------------------------------------     00138802
        DFHMDF POS=(15,15),LENGTH=10,INITIAL='EMAIL....:',COLOR=GREEN   00138910
EMAIL   DFHMDF POS=(15,26),LENGTH=40,ATTRB=UNPROT                       00139002
        DFHMDF POS=(15,67),LENGTH=01,ATTRB=ASKIP                        00139102
*------------------------------------------------------------------     00139202
        DFHMDF POS=(19,10),LENGTH=10,INITIAL='MSG......:',COLOR=GREEN   00139310
MSG     DFHMDF POS=(19,21),LENGTH=40,ATTRB=ASKIP                        00139404
        DFHMDF POS=(19,62),LENGTH=01,ATTRB=UNPROT                       00139504
*------------------------------------------------------------------     00139604
        DFHMDF POS=(21,10),LENGTH=11,INITIAL='TAB-NAVEGAR',            *00139721
               ATTRB=ASKIP,COLOR=PINK                                   00139820
        DFHMDF POS=(22,10),LENGTH=17,INITIAL='ENTER-LIMPA TELA|',      *00139916
               ATTRB=ASKIP,COLOR=PINK                                   00140014
        DFHMDF POS=(22,28),LENGTH=13,INITIAL='F5-CONSULTAR|',          *00140116
               ATTRB=ASKIP,COLOR=PINK                                   00140214
        DFHMDF POS=(22,42),LENGTH=11,INITIAL='F6-INCLUIR|',            *00140319
               ATTRB=ASKIP,COLOR=PINK                                   00140410
        DFHMDF POS=(22,54),LENGTH=11,INITIAL='F7-EXCLUIR|',            *00140519
               ATTRB=ASKIP,COLOR=PINK                                   00140614
        DFHMDF POS=(22,66),LENGTH=08,INITIAL='F10-SAIR',               *00140719
               ATTRB=ASKIP,COLOR=PINK                                   00140814
*------------------------------------------------------------------     00140904
        DFHMSD TYPE=FINAL                                               00141000
        END                                                             00150000

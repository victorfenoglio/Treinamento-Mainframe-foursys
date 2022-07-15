      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22RELB.                                            00040022
      *===================================================*             00050019
      *  AUTOR   : VICTOR LEAL                            *             00060028
      *  EMPRESA : FOURSYS                                *             00070028
      *  OBJETIVO: EXIBIR RELATORIO DA RELACAO ENTRE AS   *             00080019
      *            TABELAS FUNC E BENEFICIOS QUE ESTA NO  *             00080119
      *            ARQUIVO SRELBENE, GRAVAR O  RELATORIO  *             00080219
      *            NO ARQUIVO RELBENE PARA IMPRIMIR.      *             00080319
      *---------------------------------------------------*             00080421
      *  ARQUIVOS:                                        *             00080521
      *  DDNAME             I/O           INCLUDE/BOOK    *             00080621
      *  SRELBENE            I             ---------      *             00080721
      *  RELBENE             O             ---------      *             00080821
      *===================================================*             00080919
                                                                        00081000
      *====================================================             00081100
       ENVIRONMENT                               DIVISION.              00081200
      *====================================================             00081300
       CONFIGURATION                             SECTION.               00081400
       SPECIAL-NAMES.                                                   00081500
           DECIMAL-POINT  IS COMMA.                                     00081600
                                                                        00081700
       INPUT-OUTPUT                              SECTION.               00081800
       FILE-CONTROL.                                                    00081900
           SELECT SRELBENE ASSIGN TO SRELBENE                           00082001
               FILE STATUS IS WRK-FS-SRELBENE.                          00082101
                                                                        00082200
           SELECT RELBENE ASSIGN TO RELBENE                             00082301
               FILE STATUS IS WRK-FS-RELBENE.                           00082401
                                                                        00082500
      *====================================================             00082600
       DATA                                      DIVISION.              00082700
      *====================================================             00082800
      *-----------------------------------------------------            00082900
       FILE                                      SECTION.               00083000
      *-----------------------------------------------------            00083100
       FD SRELBENE                                                      00083201
           RECORDING MODE IS F                                          00083300
           BLOCK CONTAINS 0 RECORDS.                                    00083400
       01 FD-SRELBENE.                                                  00083501
          05 FD-ID         PIC 9(05).                                   00084001
          05 FD-NOME       PIC X(30).                                   00085001
          05 FD-SALARIO    PIC 9(10).                                   00087001
          05 FD-PLANMED    PIC X(10).                                   00087101
          05 FD-PLANDENT   PIC X(10).                                   00087201
          05 FD-COTAS      PIC 9(03).                                   00087301
                                                                        00088000
       FD RELBENE                                                       00089001
           RECORDING MODE IS F                                          00090000
           BLOCK CONTAINS 0 RECORDS.                                    00100000
       01 FD-RELBENE       PIC X(81).                                   00110015
                                                                        00120000
      *-----------------------------------------------------            00130000
       WORKING-STORAGE                           SECTION.               00130100
      *-----------------------------------------------------            00130200
           COPY '#GLOG'.                                                00130300
      *-----------------------------------------------------            00130400
       01 FILLER PIC X(48) VALUE                                        00130500
            '--------------VARIAVEIS PARA CABECALHO-------'.            00130600
      *-----------------------------------------------------            00130700
       01 WRK-CABEC.                                                    00130800
          05 WRK-CABEC1.                                                00130900
            10 FILLER       PIC X(32).                                  00131015
            10 FILLER       PIC X(37) VALUE                             00132000
            'RELATORIO DE FUNCIONARIOS       PAG: '.                    00133000
            10 WRK-PAG      PIC 9(02) VALUE 1.                          00134000
            10 FILLER       PIC X(10).                                  00134115
          05 WRK-CABEC2.                                                00134200
            10 FILLER       PIC X(02) VALUE 'ID'.                       00134302
            10 FILLER       PIC X(05).                                  00134402
            10 FILLER       PIC X(04) VALUE 'NOME'.                     00134500
            10 FILLER       PIC X(28).                                  00134609
            10 FILLER       PIC X(07) VALUE 'SALARIO'.                  00134702
            10 FILLER       PIC X(05).                                  00134812
            10 FILLER       PIC X(07) VALUE 'PLANMED'.                  00134902
            10 FILLER       PIC X(05).                                  00135013
            10 FILLER       PIC X(08) VALUE 'PLANDENT'.                 00135102
            10 FILLER       PIC X(04).                                  00135214
            10 FILLER       PIC X(05) VALUE 'COTAS'.                    00135301
      *-----------------------------------------------------            00135500
       01 FILLER PIC X(48) VALUE                                        00135600
            '--------------VARIAVEIS PARA EXIBICAO--------'.            00135700
      *-----------------------------------------------------            00135800
       01 WRK-RELBENE-SAIDA.                                            00135904
          05 WRK-ID-SAIDA       PIC X(05).                              00136002
          05 FILLER             PIC X(02).                              00136106
          05 WRK-NOME-SAIDA     PIC X(30).                              00136202
          05 FILLER             PIC X(02).                              00136308
          05 WRK-SALARIO-SAIDA  PIC 9(10).                              00136502
          05 FILLER             PIC X(02).                              00136608
          05 WRK-PLANMED-SAIDA  PIC X(10).                              00136702
          05 FILLER             PIC X(02).                              00136808
          05 WRK-PLANDENT-SAIDA PIC X(10).                              00136902
          05 FILLER             PIC X(02).                              00137008
          05 WRK-COTAS-SAIDA    PIC 9(03).                              00137115
          05 FILLER             PIC X(03).                              00137215
      *-----------------------------------------------------            00137300
       01 FILLER PIC X(48) VALUE                                        00137400
            '-------VARIAVEIS PARA ACUMUCACAO/EXIBICAO---'.             00137500
      *-----------------------------------------------------            00137600
       77 WRK-ACUM-LIDOS       PIC 9(02).                               00137800
       77 WRK-LINHAS           PIC 9(02) VALUE 1.                       00137900
      *-----------------------------------------------------            00138100
       01 FILLER PIC X(48) VALUE                                        00138200
            '-------VARIAVEIS PARA FILE STATUS------------'.            00138300
      *-----------------------------------------------------            00138400
       77 WRK-FS-SRELBENE      PIC 9(02).                               00138501
       77 WRK-FS-RELBENE       PIC 9(02).                               00138601
      *-----------------------------------------------------            00138700
       01 FILLER PIC X(48) VALUE                                        00138800
            '-------VARIAVEIS PARA MENSAGEM---------------'.            00138900
      *-----------------------------------------------------            00139000
       77 WRK-MENSAGEM-FIM     PIC X(16) VALUE                          00139100
           'FIM DO ARQUIVO.'.                                           00139200
       77 WRK-LINHABRANCO      PIC X(58) VALUE SPACES.                  00139300
      *====================================================             00139400
       PROCEDURE                                 DIVISION.              00139500
      *====================================================             00139600
      *----------------------------------------------------             00139724
       0000-PRINCIPAL                             SECTION.              00139800
      *----------------------------------------------------             00139924
           PERFORM 1000-INICIALIZAR.                                    00140000
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-SRELBENE                 00140101
                    EQUAL 10.                                           00140200
           PERFORM 3000-FINALIZAR.                                      00140300
           STOP RUN.                                                    00140400
      *----------------------------------------------------             00140524
       0000-99-FIM.                                  EXIT.              00140600
      *----------------------------------------------------             00140724
                                                                        00140800
      *----------------------------------------------------             00140924
       1000-INICIALIZAR                           SECTION.              00141000
      *----------------------------------------------------             00141124
           OPEN INPUT SRELBENE                                          00141201
                OUTPUT RELBENE.                                         00141301
             PERFORM 4000-TESTAR-STATUS.                                00141400
             PERFORM 1050-LER.                                          00141500
      *----------------------------------------------------             00141624
       1000-99-FIM.                                  EXIT.              00141700
      *----------------------------------------------------             00141824
                                                                        00141900
      *----------------------------------------------------             00142024
       1050-LER                                   SECTION.              00142100
      *----------------------------------------------------             00142224
           READ SRELBENE.                                               00142301
           IF WRK-FS-SRELBENE EQUAL 10                                  00142502
            DISPLAY WRK-MENSAGEM-FIM                                    00142600
           END-IF.                                                      00142700
           PERFORM 1100-IMPRIME-CABEC.                                  00142800
      *----------------------------------------------------             00142924
       1050-99-FIM.                                  EXIT.              00143000
      *----------------------------------------------------             00143124
                                                                        00143200
      *----------------------------------------------------             00143324
       1100-IMPRIME-CABEC                         SECTION.              00143400
      *----------------------------------------------------             00143524
            WRITE FD-RELBENE FROM WRK-LINHABRANCO.                      00143601
             IF WRK-PAG EQUAL 1                                         00143700
              WRITE FD-RELBENE FROM WRK-CABEC1                          00143801
             ELSE                                                       00143900
              WRITE FD-RELBENE FROM WRK-CABEC1 AFTER PAGE               00144001
             END-IF.                                                    00144100
            WRITE FD-RELBENE FROM WRK-LINHABRANCO.                      00144201
            WRITE FD-RELBENE FROM WRK-CABEC2 AFTER 2 LINE .             00144301
             ADD 4 TO WRK-LINHAS.                                       00144400
      *----------------------------------------------------             00144524
       1100-99-FIM.                                  EXIT.              00144600
      *----------------------------------------------------             00144724
                                                                        00144800
      *----------------------------------------------------             00144924
       2000-PROCESSAR                             SECTION.              00145000
      *----------------------------------------------------             00145124
           ADD 1 TO WRK-ACUM-LIDOS.                                     00145200
            IF WRK-FS-RELBENE EQUAL 0                                   00145402
             MOVE FD-ID       TO WRK-ID-SAIDA                           00146203
             MOVE FD-NOME     TO WRK-NOME-SAIDA                         00146303
             MOVE FD-SALARIO  TO WRK-SALARIO-SAIDA                      00146503
             MOVE FD-PLANMED  TO WRK-PLANMED-SAIDA                      00146603
             MOVE FD-PLANDENT TO WRK-PLANDENT-SAIDA                     00146703
             MOVE FD-COTAS    TO WRK-COTAS-SAIDA                        00146803
              WRITE FD-RELBENE FROM WRK-RELBENE-SAIDA                   00146904
             ADD 1 TO WRK-LINHAS                                        00147003
            ELSE                                                        00147102
             DISPLAY WRK-MENSAGEM-FIM                                   00147227
            END-IF.                                                     00147502
                                                                        00147602
             IF WRK-LINHAS GREATER 10                                   00147718
              ADD 1 TO WRK-PAG                                          00147800
              MOVE 1 TO WRK-LINHAS                                      00147900
              PERFORM 1100-IMPRIME-CABEC                                00148000
             END-IF.                                                    00148100
                                                                        00148200
           READ SRELBENE.                                               00148301
      *----------------------------------------------------             00148724
       2000-99-FIM.                                  EXIT.              00148800
      *----------------------------------------------------             00148924
                                                                        00149000
      *----------------------------------------------------             00149124
       3000-FINALIZAR                             SECTION.              00149200
      *----------------------------------------------------             00149324
           CLOSE SRELBENE RELBENE.                                      00149402
           DISPLAY WRK-LINHABRANCO.                                     00149500
           DISPLAY WRK-MENSAGEM-FIM.                                    00149616
           DISPLAY 'TOTAL REGISTROS LIDOS:' WRK-ACUM-LIDOS.             00149724
      *----------------------------------------------------             00150024
       3000-99-FIM.                                  EXIT.              00151000
      *----------------------------------------------------             00151124
                                                                        00151200
      *----------------------------------------------------             00151324
       4000-TESTAR-STATUS                         SECTION.              00151400
      *----------------------------------------------------             00151524
           IF WRK-FS-SRELBENE NOT EQUAL 0                               00151602
             MOVE 'FR22RELB'              TO WRK-PROGRAMA               00151722
             MOVE '4000'                  TO WRK-SECAO                  00151802
             MOVE 'ERRO NO OPEN SRELBENE' TO WRK-MENSAGEM               00151902
             MOVE WRK-FS-SRELBENE         TO WRK-STATUS                 00152002
             PERFORM 9000-ERRO                                          00153000
           END-IF.                                                      00154000
      *----------------------------------------------------             00155024
       4000-99-FIM.                                  EXIT.              00156000
      *----------------------------------------------------             00157024
                                                                        00158000
      *----------------------------------------------------             00159024
       9000-ERRO                                  SECTION.              00160000
      *----------------------------------------------------             00160124
      *    CALL 'GRAVALOG' USING WRK-DADOS.                             00160326
           GOBACK.                                                      00160400
      *----------------------------------------------------             00160524
       9000-99-FIM.                                  EXIT.              00160600
      *----------------------------------------------------             00160724

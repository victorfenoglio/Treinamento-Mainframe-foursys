      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22EX04.                                            00040000
      *===================================================*             00050001
      * AUTOR   : VICTOR LEAL                             *             00060001
      * EMPRESA : FOURSYS                                 *             00070001
      * OBJETIVO: PROGRAMA TEM A FINALIDADE DE RECEBER    *             00080001
      *           DADOS DOS ARQUIVOS DE ENTRADA CLIENTES E*             00080101
      *           MOV0106, FAZER A RELACAO(BALANCO) ENTRE *             00080201
      *           AS CHAVES E GRAVAR NO ARQUIVO DE SAIDA  *             00080301
      *           MOV0106A, MOV0106C.                     *             00080401
      *---------------------------------------------------*             00080501
      *  ARQUIVOS:                                        *             00080601
      *  DDNAME             I/O           INCLUDE/BOOK    *             00080701
      *  CLIENTES            I             ---------      *             00080801
      *  MOV0106             I             ---------      *             00080901
      *  MOV0106A            O             ---------      *             00081001
      *  MOV0106C            O             ---------      *             00081101
      *===================================================*             00081201
                                                                        00081301
      *====================================================             00081401
       ENVIRONMENT                               DIVISION.              00081500
      *====================================================             00081600
       CONFIGURATION                             SECTION.               00081700
       SPECIAL-NAMES.                                                   00081800
           DECIMAL-POINT  IS COMMA.                                     00081900
                                                                        00082000
       INPUT-OUTPUT                              SECTION.               00082100
       FILE-CONTROL.                                                    00082200
           SELECT CLIENTES ASSIGN TO CLIENTES                           00082300
               FILE STATUS IS WRK-FS-CLIENTES.                          00082400
                                                                        00082500
           SELECT MOV0106 ASSIGN TO MOV0106                             00082600
               FILE STATUS IS WRK-FS-MOV0106.                           00082700
                                                                        00082800
           SELECT MOV0106A ASSIGN TO MOV0106A                           00082900
               FILE STATUS IS WRK-FS-MOV0106A.                          00083000
                                                                        00083100
           SELECT MOV0106C ASSIGN TO MOV0106C                           00083200
               FILE STATUS IS WRK-FS-MOV0106C.                          00083300
                                                                        00083400
      *====================================================             00083500
       DATA                                      DIVISION.              00083600
      *====================================================             00083700
      *-----------------------------------------------------            00083800
       FILE                                      SECTION.               00083900
      *-----------------------------------------------------            00084000
       FD CLIENTES                                                      00084100
           RECORDING MODE IS F                                          00084200
           BLOCK CONTAINS 0 RECORDS.                                    00084300
      *-----------LRECL 46-----------------------------------           00084400
       01 FD-CLIENTES.                                                  00084500
          05 FD-CLIENTES-CHAVE.                                         00084600
             10 FD-CLIENTES-AGENCIA PIC X(04).                          00084700
             10 FD-CLIENTES-CONTA   PIC X(04).                          00084800
          05 FD-CLIENTES-NOME       PIC X(30).                          00084900
          05 FD-CLIENTES-SALDO      PIC 9(08).                          00085000
                                                                        00085100
       FD MOV0106                                                       00085200
           RECORDING MODE IS F                                          00085300
           BLOCK CONTAINS 0 RECORDS.                                    00085400
      *-----------LRECL 47----------------------------------            00085500
       01 FD-MOV0106.                                                   00085600
          05 FD-MOV0106-CHAVE.                                          00085700
             10 FD-MOV0106-AGENCIA  PIC X(04).                          00085800
             10 FD-MOV0106-CONTA    PIC X(04).                          00085900
          05 FD-MOV0106-MOVIMENTO   PIC X(30).                          00086000
          05 FD-MOV0106-VALORMOV    PIC 9(08).                          00086100
          05 FD-MOV0106-TIPOMOV     PIC X(01).                          00087000
                                                                        00087100
       FD MOV0106A                                                      00087200
           RECORDING MODE IS F                                          00087300
           BLOCK CONTAINS 0 RECORDS.                                    00087400
      *-----------LRECL 46----------------------------------            00087500
       01 FD-MOV0106A.                                                  00087600
          05 FD-MOV0106A-CHAVE.                                         00087700
             10 FD-MOV0106A-AGENCIA PIC X(04).                          00087800
             10 FD-MOV0106A-CONTA   PIC X(04).                          00087900
          05 FD-MOV0106A-NOME       PIC X(30).                          00088000
          05 FD-MOV0106A-SALDO      PIC 9(08).                          00089000
                                                                        00090000
       FD MOV0106C                                                      00091000
           RECORDING MODE IS F                                          00092000
           BLOCK CONTAINS 0 RECORDS.                                    00093000
      *-----------LRECL 46----------------------------------            00094000
       01 FD-MOV0106C.                                                  00095000
          05 FD-MOV0106C-CHAVE.                                         00096000
             10 FD-MOV0106C-AGENCIA PIC X(04).                          00097000
             10 FD-MOV0106C-CONTA   PIC X(04).                          00098000
          05 FD-MOV0106C-NOME       PIC X(30).                          00099000
          05 FD-MOV0106C-SALDO      PIC 9(08).                          00099100
      *-----------------------------------------------------            00100000
       WORKING-STORAGE                           SECTION.               00110000
      *-----------------------------------------------------            00120000
           COPY '#GLOG'.                                                00130000
      *-----------------------------------------------------            00130100
       01 FILLER PIC X(48) VALUE                                        00130200
            '-------VARIAVEIS PARA FILE STATUS------------'.            00130300
      *-----------------------------------------------------            00130400
       77 WRK-FS-CLIENTES     PIC 9(02).                                00130500
       77 WRK-FS-MOV0106      PIC 9(02).                                00130600
       77 WRK-FS-MOV0106A     PIC 9(02).                                00130700
       77 WRK-FS-MOV0106C     PIC 9(02).                                00130800
      *-----------------------------------------------------            00130900
       01 FILLER PIC X(48) VALUE                                        00131000
            '-------VARIAVEIS PARA ACUMULACAO OU OPERACAO-'.            00131100
      *-----------------------------------------------------            00131200
       77 WRK-REGISGRAV        PIC 9(02) VALUE ZEROS.                   00131300
       77 WRK-REGISLIDO        PIC 9(02) VALUE ZEROS.                   00131400
       77 WRK-PR               PIC 9(02).                               00131500
      *-----------------------------------------------------            00131600
       01 FILLER PIC X(48) VALUE                                        00131700
            '-------VARIAVEIS PARA MENSAGEM---------------'.            00131800
      *-----------------------------------------------------            00132000
       77 WRK-MENSAGEM-FIM     PIC X(21) VALUE                          00133000
           'FIM DO PROCESSAMENTO.'.                                     00134000
       77 WRK-MENSAGEM-OK      PIC X(40) VALUE                          00134100
           'OS REGISTROS LIDOS FORAM TODOS GRAVADOS.'.                  00134200
       77 WRK-MENSAGEM-ERRO    PIC X(05) VALUE 'ERRO.'.                 00134400
      *====================================================             00135000
       PROCEDURE                                 DIVISION.              00136000
      *====================================================             00137000
      *-----------------------------------------------------            00138000
       0000-PRINCIPAL                             SECTION.              00138100
      *-----------------------------------------------------            00138200
           PERFORM 1000-INICIALIZAR.                                    00138300
            PERFORM 1100-VERIFICA-VAZIO.                                00138400
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES                 00138500
                       EQUAL 10 AND WRK-FS-MOV0106 EQUAL 10.            00138600
           PERFORM 3000-FINALIZAR.                                      00138700
           STOP RUN.                                                    00138800
      *-----------------------------------------------------            00138900
       0000-99-FIM.                                  EXIT.              00139000
      *-----------------------------------------------------            00139100
                                                                        00139200
      *-----------------------------------------------------            00139300
       1000-INICIALIZAR                           SECTION.              00139400
      *-----------------------------------------------------            00139500
           OPEN INPUT CLIENTES MOV0106                                  00139600
                OUTPUT MOV0106A MOV0106C.                               00139700
             PERFORM 4000-TESTAR-STATUS.                                00139800
      *-----------------------------------------------------            00139900
       1000-99-FIM.                                  EXIT.              00140000
      *-----------------------------------------------------            00140100
                                                                        00140200
      *-----------------------------------------------------            00140300
       1100-VERIFICA-VAZIO                       SECTION.               00140400
      *-----------------------------------------------------            00140500
           READ CLIENTES.                                               00140600
           READ MOV0106.                                                00140700
      *-----------------------------------------------------            00140800
       1100-99-FIM.                                  EXIT.              00140900
      *-----------------------------------------------------            00141000
                                                                        00142000
      *-----------------------------------------------------            00143000
       2000-PROCESSAR                             SECTION.              00143100
      *-----------------------------------------------------            00143200
           EVALUATE TRUE                                                00143300
            WHEN FD-CLIENTES-CHAVE LESS FD-MOV0106-CHAVE                00143400
             IF FD-CLIENTES-SALDO GREATER THAN OR EQUAL 10000           00143500
              MOVE FD-CLIENTES-CHAVE  TO FD-MOV0106A-CHAVE              00143600
              MOVE FD-CLIENTES-NOME   TO FD-MOV0106A-NOME               00143700
              MOVE FD-CLIENTES-SALDO  TO FD-MOV0106A-SALDO              00143800
               WRITE FD-MOV0106A                                        00143900
               ADD 1 TO WRK-REGISGRAV                                   00144000
             ELSE                                                       00144100
              MOVE FD-CLIENTES-CHAVE  TO FD-MOV0106C-CHAVE              00144200
              MOVE FD-CLIENTES-NOME   TO FD-MOV0106C-NOME               00144300
              MOVE FD-CLIENTES-SALDO  TO FD-MOV0106C-SALDO              00144400
               WRITE FD-MOV0106C                                        00144500
               ADD 1 TO WRK-REGISGRAV                                   00144600
             END-IF                                                     00144700
              READ CLIENTES                                             00144800
              ADD 1 TO WRK-REGISLIDO                                    00144900
                                                                        00145000
            WHEN FD-CLIENTES-CHAVE EQUAL FD-MOV0106-CHAVE               00145100
             IF FD-MOV0106-TIPOMOV EQUAL 'C'                            00145200
              ADD FD-MOV0106-VALORMOV TO FD-CLIENTES-SALDO              00145300
             ELSE                                                       00145400
              IF FD-MOV0106-VALORMOV LESS THAN OR EQUAL                 00145500
                 FD-CLIENTES-SALDO                                      00145600
                SUBTRACT FD-MOV0106-VALORMOV FROM                       00145700
                 FD-CLIENTES-SALDO                                      00145800
              END-IF                                                    00145900
             END-IF                                                     00146000
              READ MOV0106                                              00146100
              IF WRK-FS-MOV0106 EQUAL 10                                00146200
               MOVE HIGH-VALUES TO FD-MOV0106-CHAVE                     00146300
              END-IF                                                    00146400
            END-EVALUATE.                                               00146500
      *-----------------------------------------------------            00146600
       2000-99-FIM.                                  EXIT.              00146700
      *-----------------------------------------------------            00146800
                                                                        00146900
      *-----------------------------------------------------            00147000
       3000-FINALIZAR                             SECTION.              00147100
      *-----------------------------------------------------            00147200
           DISPLAY 'REGISTROS LIDOS:'    WRK-REGISLIDO.                 00147300
           DISPLAY 'REGISTROS GRAVADOS:' WRK-REGISGRAV.                 00147400
            COMPUTE WRK-PR = WRK-REGISLIDO - WRK-REGISGRAV              00147500
             IF WRK-PR NOT EQUAL 0                                      00147600
              DISPLAY WRK-MENSAGEM-ERRO                                 00147700
             ELSE                                                       00147800
              DISPLAY WRK-MENSAGEM-OK                                   00147900
             END-IF.                                                    00148100
           CLOSE CLIENTES MOV0106                                       00148200
                 MOV0106A MOV0106C.                                     00148300
           DISPLAY WRK-MENSAGEM-FIM.                                    00148400
      *-----------------------------------------------------            00148500
       3000-99-FIM.                                  EXIT.              00148600
      *-----------------------------------------------------            00148700
                                                                        00148800
      *-----------------------------------------------------            00148900
       4000-TESTAR-STATUS                         SECTION.              00149000
      *-----------------------------------------------------            00150000
           PERFORM 4100-TESTARSTATUS-CLIENTES.                          00151000
           PERFORM 4200-TESTARSTATUS-MOV0106.                           00151100
           PERFORM 4300-TESTARSTATUS-MOV0106A.                          00151200
           PERFORM 4400-TESTARSTATUS-MOV0106C.                          00151300
      *-----------------------------------------------------            00151400
       4000-99-FIM.                                  EXIT.              00151500
      *-----------------------------------------------------            00151600
                                                                        00151700
      *-----------------------------------------------------            00151800
       4100-TESTARSTATUS-CLIENTES                 SECTION.              00151900
      *-----------------------------------------------------            00152000
           IF WRK-FS-CLIENTES NOT EQUAL 00                              00152100
            MOVE 'FR22EX04'              TO WRK-PROGRAMA                00153000
            MOVE 'ERRO NO OPEN CLIENTES' TO WRK-MENSAGEM                00154000
            MOVE '1000'                  TO WRK-SECAO                   00155000
            MOVE WRK-FS-CLIENTES         TO WRK-STATUS                  00156000
            PERFORM 9000-TRATAERROS                                     00157000
           END-IF.                                                      00158000
      *-----------------------------------------------------            00158100
       4100-99-FIM.                                  EXIT.              00158200
      *-----------------------------------------------------            00158300
                                                                        00158400
      *-----------------------------------------------------            00158500
       4200-TESTARSTATUS-MOV0106                  SECTION.              00158600
      *-----------------------------------------------------            00158700
           IF WRK-FS-MOV0106 NOT EQUAL 00                               00158800
             MOVE 'FR22EX04'              TO WRK-PROGRAMA               00158900
             MOVE 'ERRO NO OPEN MOV0106'  TO WRK-MENSAGEM               00159000
             MOVE '1000'                  TO WRK-SECAO                  00159100
             MOVE WRK-FS-MOV0106          TO WRK-STATUS                 00159200
             PERFORM 9000-TRATAERROS                                    00159300
            END-IF.                                                     00159400
      *-----------------------------------------------------            00159500
       4200-99-FIM.                                  EXIT.              00159600
      *-----------------------------------------------------            00159700
                                                                        00159800
      *-----------------------------------------------------            00159900
       4300-TESTARSTATUS-MOV0106A                 SECTION.              00160000
      *-----------------------------------------------------            00160100
           IF WRK-FS-MOV0106A NOT EQUAL 00                              00160200
            MOVE 'FR22EX04'              TO WRK-PROGRAMA                00160300
            MOVE 'ERRO NO OPEN MOV0106A' TO WRK-MENSAGEM                00160400
            MOVE '1000'                  TO WRK-SECAO                   00160500
            MOVE WRK-FS-MOV0106A         TO WRK-STATUS                  00160600
            PERFORM 9000-TRATAERROS                                     00160700
           END-IF.                                                      00160800
      *-----------------------------------------------------            00160900
       4300-99-FIM.                                  EXIT.              00161000
      *-----------------------------------------------------            00161100
                                                                        00161200
      *-----------------------------------------------------            00161300
       4400-TESTARSTATUS-MOV0106C                 SECTION.              00161400
      *-----------------------------------------------------            00161500
           IF WRK-FS-MOV0106C NOT EQUAL 00                              00161600
            MOVE 'FR22EX04'              TO WRK-PROGRAMA                00161700
            MOVE 'ERRO NO OPEN MOV0106C' TO WRK-MENSAGEM                00161800
            MOVE '1000'                  TO WRK-SECAO                   00161900
            MOVE WRK-FS-MOV0106C         TO WRK-STATUS                  00162000
            PERFORM 9000-TRATAERROS                                     00162100
           END-IF.                                                      00162200
      *-----------------------------------------------------            00162300
       4400-99-FIM.                                  EXIT.              00162400
      *-----------------------------------------------------            00162500
                                                                        00162600
      *-----------------------------------------------------            00162700
       9000-TRATAERROS                            SECTION.              00162800
      *-----------------------------------------------------            00162900
            CALL 'GRAVALOG' USING WRK-DADOS.                            00163000
           GOBACK.                                                      00163100
      *-----------------------------------------------------            00163200
       9000-99-FIM.                                  EXIT.              00163300
      *-----------------------------------------------------            00164000

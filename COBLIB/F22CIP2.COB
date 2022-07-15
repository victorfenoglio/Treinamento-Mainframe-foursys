      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00011000
      *====================================================             00012000
       PROGRAM-ID. F22CIP2.                                             00013000
      *===================================================*             00014000
      *  AUTOR   : VICTOR LEAL                            *             00015000
      *  EMPRESA : FOURSYS                                *             00016000
      *  OBJETIVO: CRIANDO PROGRAMA DE CONSULTA NO CICS   *             00017000
      *            (SEM MAPA),USANDO ARQUIVO VSAM E TAMBEM*             00018000
      *            STRING PARA TRANSMISSAO DE DADOS.      *             00018100
      *---------------------------------------------------*             00018300
      *  BASE  DE DADOS:                                  *             00018700
      *  TABELA             I/O           INCLUDE/BOOK    *             00018800
      *  DFH310.CICS.FUNC    I             ---------      *             00018900
      *===================================================*             00019000
       DATA                                      DIVISION.              00020000
      *====================================================             00021000
      *-----------------------------------------------------            00022000
       WORKING-STORAGE                            SECTION.              00023000
      *-----------------------------------------------------            00024000
             COPY DFHAID.                                               00026000
                                                                        00026100
       01 WRK-ENTRADA.                                                  00027000
          05 WRK-TRANS     PIC X(04).                                   00027100
          05 WRK-ID        PIC X(05).                                   00027200
          05 WRK-NOME      PIC X(30) VALUE SPACES.                      00027300
          05 WRK-SETOR     PIC X(04) VALUE SPACES.                      00027400
          05 WRK-SALARIO   PIC X(10) VALUE SPACES.                      00027500
          05 WRK-DATAADM   PIC X(10) VALUE SPACES.                      00027600
          05 WRK-EMAIL     PIC X(40) VALUE SPACES.                      00027700
          05 WRK-STATUS    PIC X(01) VALUE SPACES.                      00027800
                                                                        00027900
                                                                        00028000
       01 WRK-DADOS.                                                    00028100
          05 WRK-CHAVE     PIC X(05) VALUE SPACES.                      00029000
          05 WRK-NOME      PIC X(30) VALUE SPACES.                      00030000
          05 WRK-SETOR     PIC X(04) VALUE SPACES.                      00040000
          05 WRK-SALARIO   PIC X(10) VALUE SPACES.                      00050000
          05 WRK-DATAADM   PIC X(10) VALUE SPACES.                      00051000
          05 WRK-EMAIL     PIC X(40) VALUE SPACES.                      00057000
                                                                        00058000
      *----------------------------------------------------             00059000
       01 FILLER PIC X(48) VALUE                                        00059100
           '--------------VARIAVEIS DE APOIO-------------'.             00059200
      *----------------------------------------------------             00059300
       77 WRK-RESP       PIC S9(04) COMP.                               00059400
      *====================================================             00059800
       PROCEDURE                                 DIVISION.              00059900
      *====================================================             00060000
      *-----------------------------------------------------            00070000
       0000-PRINCIPAL                             SECTION.              00071000
      *-----------------------------------------------------            00072000
            PERFORM 1000-INICIALIZAR.                                   00073000
            PERFORM 2000-PROCESSAR.                                     00073200
                                                                        00073702
            EXEC CICS SEND                                              00073800
              FROM(WRK-ENTRADA)                                         00073900
              ERASE                                                     00074000
            END-EXEC.                                                   00074100
                                                                        00074200
            EXEC CICS                                                   00074300
              RETURN                                                    00074400
            END-EXEC.                                                   00074500
      *-----------------------------------------------------            00074600
       0000-99-FIM.                                  EXIT.              00074700
      *-----------------------------------------------------            00075000
                                                                        00076000
      *-----------------------------------------------------            00077000
       1000-INICIALIZAR                           SECTION.              00078000
      *-----------------------------------------------------            00079000
             EXEC CICS RECEIVE                                          00079100
               INTO(WRK-ENTRADA)                                        00079400
             END-EXEC.                                                  00079600
      *-----------------------------------------------------            00080300
       1000-99-FIM.                                  EXIT.              00080400
      *-----------------------------------------------------            00080500
                                                                        00080600
      *-----------------------------------------------------            00080700
       2000-PROCESSAR                             SECTION.              00080800
      *-----------------------------------------------------            00080900
           MOVE WRK-ID TO WRK-CHAVE                                     00081000
            EXEC CICS READ                                              00082000
                      FILE('FUNC')                                      00083000
                      RIDFLD(WRK-CHAVE)                                 00084000
                      INTO(WRK-DADOS)                                   00085000
                      RESP(WRK-RESP)                                    00086000
            END-EXEC.                                                   00087000
                                                                        00088000
           IF WRK-RESP EQUAL DFHRESP(NORMAL)                            00088100
              MOVE CORR WRK-DADOS TO WRK-ENTRADA                        00088201
              MOVE '1' TO WRK-STATUS                                    00088301
              IF WRK-EMAIL OF WRK-DADOS EQUAL SPACES                    00088401
                 MOVE '4' TO WRK-STATUS                                 00088501
              END-IF                                                    00088601
              IF WRK-SALARIO OF WRK-DADOS EQUAL ZEROES OR               00088701
                 WRK-SALARIO OF WRK-DADOS IS NOT NUMERIC                00088801
                 MOVE '3' TO WRK-STATUS                                 00088901
                 END-IF                                                 00089001
           END-IF.                                                      00089101
           IF WRK-RESP EQUAL DFHRESP(NOTFND)                            00089201
              MOVE '2' TO WRK-STATUS                                    00089301
           END-IF.                                                      00089401
           IF WRK-STATUS NOT EQUAL 1                                    00089501
              INITIALIZE WRK-DADOS                                      00089601
              MOVE CORR WRK-DADOS TO WRK-ENTRADA                        00089701
           END-IF.                                                      00089801
      *-----------------------------------------------------            00090200
       2000-99-FIM.                                  EXIT.              00091000
      *-----------------------------------------------------            00100000
                                                                        00257000
      *-----------------------------------------------------            00258000
       3000-FINALIZAR                             SECTION.              00259000
      *-----------------------------------------------------            00260000
      *-----------------------------------------------------            00339000
       3000-99-FIM.                                  EXIT.              00340000
      *-----------------------------------------------------            00350000

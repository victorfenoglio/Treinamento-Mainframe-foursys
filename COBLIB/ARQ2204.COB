      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. ARQ2204.                                             00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060000
      *  EMPRESA : FOURSYS                                *             00070000
      *  OBJETIVO: ABRIR E FECHAR ARQUIVOS COM LRECL=37   *             00080000
      *            COM TOTAL DE REGISTROS LIDOS E TOTAL   *             00081000
      *            DE ANALISTAS.                          *             00081100
      *---------------------------------------------------*             00081200
      *  ARQUIVOS:                                        *             00081300
      *  DDNAME             I/O           INCLUDE/BOOK    *             00081400
      *  CLIENTES            I             ---------      *             00081500
      *===================================================*             00082000
                                                                        00083000
      *====================================================             00084000
       ENVIRONMENT                               DIVISION.              00085000
      *====================================================             00086000
       CONFIGURATION                             SECTION.               00087000
       SPECIAL-NAMES.                                                   00088000
           DECIMAL-POINT  IS COMMA.                                     00089000
                                                                        00090000
       INPUT-OUTPUT                              SECTION.               00100000
       FILE-CONTROL.                                                    00110000
           SELECT CLIENTES ASSIGN TO JCLCLI                             00120000
               FILE STATUS IS WRK-FS-CLIENTES.                          00130000
                                                                        00131000
      *====================================================             00132000
       DATA                                      DIVISION.              00133000
      *====================================================             00133100
      *-----------------------------------------------------            00133200
       FILE                                      SECTION.               00133300
      *-----------------------------------------------------            00133400
       FD CLIENTES                                                      00133500
           RECORDING MODE IS F                                          00133600
           BLOCK CONTAINS 0 RECORDS.                                    00133700
                                                                        00133800
       01 FD-CLIENTES.                                                  00133900
          05 FD-CODIGO       PIC 9(05).                                 00134000
          05 FD-NOME         PIC X(20).                                 00135000
          05 FD-CODCARGO     PIC X(04).                                 00136000
          05 FD-SALARIO      PIC 9(08).                                 00137000
                                                                        00137200
      *-----------------------------------------------------            00137300
       WORKING-STORAGE                           SECTION.               00137400
      *-----------------------------------------------------            00137500
      *-----------------------------------------------------            00137600
       01 FILLER PIC X(48) VALUE                                        00137700
           '--------------VARIAVEIS ACUMULAR/MASCARA-----'.             00137800
      *-----------------------------------------------------            00137900
       77 WRK-COUNTREG       PIC 9(04) VALUE ZEROS.                     00138000
       77 WRK-COUNTANA       PIC 9(04) VALUE ZEROS.                     00138100
       77 WRK-SALACUM        PIC 9(09) VALUE ZEROS.                     00138200
       77 WRK-SALACUM-ED     PIC ZZZ.ZZ9.999.                           00138300
      *-----------------------------------------------------            00138400
       01 FILLER PIC X(48) VALUE                                        00138500
           '--------------VARIAVEIS PARA MENSAGEM--------'.             00138600
      *-----------------------------------------------------            00138700
       77 WRK-MSG-OPEN       PIC X(30) VALUE 'STATUS OPEN '.            00138800
       77 WRK-MSG-CLOSE      PIC X(30) VALUE 'STATUS CLOSE'.            00138900
       77 WRK-MSG            PIC X(30) VALUE SPACES.                    00139000
      *-----------------------------------------------------            00139100
       01 FILLER PIC X(48) VALUE                                        00139200
           '--------------VARIAVEIS PARA FILE STATUS-----'.             00139300
      *-----------------------------------------------------            00139400
       77 WRK-FS-CLIENTES    PIC 9(02).                                 00139500
                                                                        00139600
      *====================================================             00139700
       PROCEDURE                                 DIVISION.              00139800
      *====================================================             00139900
      *-----------------------------------------------------            00140000
       0000-PRINCIPAL                             SECTION.              00140100
      *-----------------------------------------------------            00140200
           PERFORM 1000-INICIALIZAR.                                    00140300
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-CLIENTES                 00140400
                    EQUAL 10.                                           00140500
           PERFORM 3000-FINALIZAR.                                      00140600
           STOP RUN.                                                    00141000
      *-----------------------------------------------------            00150000
       0000-99-FIM.                                  EXIT.              00151000
      *-----------------------------------------------------            00152000
                                                                        00153000
      *-----------------------------------------------------            00154000
       1000-INICIALIZAR                           SECTION.              00155000
      *-----------------------------------------------------            00156000
           OPEN INPUT CLIENTES.                                         00157000
            MOVE WRK-MSG-OPEN TO WRK-MSG.                               00157100
      *     PERFORM 4000-TESTAR-STATUS.                                 00157200
             EVALUATE WRK-FS-CLIENTES                                   00157300
              WHEN 00                                                   00157400
               PERFORM 5000-TESTAR-VAZIO                                00157500
              WHEN 35                                                   00157600
               DISPLAY 'ARQUIVO NAO EXISTE'                             00157700
               STOP RUN                                                 00157800
              WHEN OTHER                                                00157900
               DISPLAY 'ERRO AO ABRIR: ' WRK-FS-CLIENTES                00158000
               STOP RUN                                                 00158100
              END-EVALUATE.                                             00158200
           DISPLAY '-----RELATORIO DE CLIENTES----------'.              00158300
           DISPLAY '                                    '.              00158400
      *-----------------------------------------------------            00158500
       1000-99-FIM.                                  EXIT.              00158600
      *-----------------------------------------------------            00158700
                                                                        00158800
      *-----------------------------------------------------            00158900
       2000-PROCESSAR                             SECTION.              00159000
      *-----------------------------------------------------            00159100
           DISPLAY 'REGISTRO:' FD-CODIGO FD-NOME FD-CODCARGO.           00159200
            ADD 1 TO WRK-COUNTREG.                                      00159300
             IF FD-CODCARGO EQUAL 0234                                  00159400
              ADD 1 TO WRK-COUNTANA                                     00159500
              ADD FD-SALARIO TO WRK-SALACUM                             00159600
             END-IF.                                                    00159700
           PERFORM 5000-TESTAR-VAZIO.                                   00160300
      *-----------------------------------------------------            00160400
       2000-99-FIM.                                  EXIT.              00160500
      *-----------------------------------------------------            00160600
                                                                        00160700
      *-----------------------------------------------------            00160800
       3000-FINALIZAR                             SECTION.              00160900
      *-----------------------------------------------------            00161000
            DISPLAY '                                    '.             00161100
            DISPLAY '------------------------------------'.             00161200
            DISPLAY '                                    '.             00161300
            DISPLAY 'TOTAL DE REGISTROS LIDOS:           '              00161400
                     WRK-COUNTREG.                                      00161500
            DISPLAY 'TOTAL DE REGISTROS COM ANALISTAS:   '              00161600
                     WRK-COUNTANA.                                      00161700
            MOVE WRK-SALACUM TO WRK-SALACUM-ED.                         00161800
            DISPLAY 'TOTAL SALARIO DOS ANALISTAS:        '              00161900
                     WRK-SALACUM-ED.                                    00162000
           CLOSE CLIENTES.                                              00162100
            MOVE WRK-MSG-CLOSE TO WRK-MSG.                              00162200
      *     PERFORM 4000-TESTAR-STATUS.                                 00162300
            DISPLAY '                                    '.             00162400
            DISPLAY '----------FIM RELATORIO-------------'.             00162500
      *-----------------------------------------------------            00162600
       3000-99-FIM.                                  EXIT.              00162700
      *-----------------------------------------------------            00162800
                                                                        00162900
      *-----------------------------------------------------            00163000
       4000-TESTAR-STATUS                         SECTION.              00163100
      *-----------------------------------------------------            00163200
           DISPLAY '----------------'.                                  00163300
           DISPLAY WRK-MSG '=' WRK-FS-CLIENTES.                         00163400
           DISPLAY '----------------'.                                  00163500
      *-----------------------------------------------------            00163600
       4000-99-FIM.                                  EXIT.              00163700
      *-----------------------------------------------------            00163800
                                                                        00163900
      *-----------------------------------------------------            00164000
       5000-TESTAR-VAZIO                          SECTION.              00164100
      *-----------------------------------------------------            00164200
           READ CLIENTES.                                               00164300
            IF WRK-FS-CLIENTES EQUAL 10                                 00164400
             DISPLAY 'FINAL DO ARQUIVO'                                 00164500
            END-IF.                                                     00164600
      *-----------------------------------------------------            00164700
       5000-99-FIM.                                  EXIT.              00164800
      *-----------------------------------------------------            00164900
                                                                        00165000
      *-----------------------------------------------------            00165100
       9000-TRATA-ERROS                           SECTION.              00165200
      *-----------------------------------------------------            00165300
      *-----------------------------------------------------            00165400
       9000-99-FIM.                                  EXIT.              00165500
      *-----------------------------------------------------            00166000

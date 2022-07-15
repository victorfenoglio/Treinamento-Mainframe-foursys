      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22JOIN.                                            00040008
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060009
      *  EMPRESA : FOURSYS                                *             00070009
      *  OBJETIVO: LEITURA TABELA FUNC COM JOIN NA TABELA *             00080000
      *            BENEFICIOS, COM SAIDA PARA O ARQUIVO   *             00080100
      *            SRELBENE                               *             00080200
      *---------------------------------------------------*             00080300
      *  BASE  DE DADOS:                                  *             00080400
      *  TABELA             I/O           INCLUDE/BOOK    *             00080500
      *  IVAN.FUNC           I               #BKFUNC      *             00080600
      *  IVAN.BENEF          I               #BKBENEF     *             00080700
      *---------------------------------------------------*             00080800
      *  ARQUIVOS:                                        *             00080900
      *  DDNAME             I/O           INCLUDE/BOOK    *             00081000
      *  SRELBENE            O             ---------      *             00081100
      *===================================================*             00081500
                                                                        00081600
      *====================================================             00081700
       ENVIRONMENT                               DIVISION.              00081800
      *====================================================             00081900
       CONFIGURATION                             SECTION.               00082000
       SPECIAL-NAMES.                                                   00083000
           DECIMAL-POINT  IS COMMA.                                     00084000
                                                                        00084103
       INPUT-OUTPUT                              SECTION.               00084203
        FILE-CONTROL.                                                   00084303
           SELECT SRELBENE ASSIGN TO SRELBENE                           00084403
               FILE STATUS IS WRK-FS-SRELBENE.                          00084503
                                                                        00085000
      *====================================================             00086000
       DATA                                      DIVISION.              00087000
      *====================================================             00088000
      *----------------------------------------------------             00089000
       FILE                                      SECTION.               00090000
      *----------------------------------------------------             00100000
       FD SRELBENE                                                      00110003
           RECORDING MODE IS F                                          00111003
           BLOCK CONTAINS 0 RECORDS.                                    00112003
      *-----------LRECL 68---------------------------------             00113003
       01 FD-SRELBENE PIC X(68).                                        00114003
                                                                        00115003
      *----------------------------------------------------             00120001
       WORKING-STORAGE                           SECTION.               00130000
      *----------------------------------------------------             00131001
           EXEC SQL                                                     00132000
              INCLUDE #BKFUNC                                           00133000
           END-EXEC.                                                    00134000
                                                                        00134100
           EXEC SQL                                                     00134200
              INCLUDE #BKBENEF                                          00134300
           END-EXEC.                                                    00134400
                                                                        00135000
           EXEC SQL                                                     00136000
              INCLUDE SQLCA                                             00137000
           END-EXEC.                                                    00137100
                                                                        00137200
           EXEC SQL                                                     00137300
              DECLARE CFUNC CURSOR FOR                                  00137400
               SELECT ID,NOME,SALARIO,PLANMED,PLANDENT,COTAS            00137502
                FROM IVAN.FUNC F, IVAN.BENEF B                          00137600
                 WHERE F.ID = B.IDFUNC                                  00137700
           END-EXEC.                                                    00137800
                                                                        00137903
       01 WRK-SRELBENE.                                                 00138004
          05 WRK-ID       PIC 99999.                                    00138103
          05 WRK-NOME     PIC X(30).                                    00138203
          05 WRK-SALARIO  PIC 9999999999.                               00138403
          05 WRK-PLANMED  PIC X(10).                                    00138503
          05 WRK-PLANDENT PIC X(10).                                    00138603
          05 WRK-COTAS    PIC 999.                                      00138706
      *----------------------------------------------------             00138807
       01 FILLER PIC X(48) VALUE                                        00138907
            '-----------VARIAVEIS DE APOIO--------------'.              00139007
      *----------------------------------------------------             00139107
       77 WRK-FS-SRELBENE  PIC 9(02).                                   00139207
       77 WRK-SQLCODE     PIC -999.                                     00139300
      *----------------------------------------------------             00139407
       01 FILLER PIC X(48) VALUE                                        00139507
            '-----------VARIAVEIS ACUMULAR/CALCULAR-----'.              00139609
      *----------------------------------------------------             00139707
       77 WRK-REGLIDOS    PIC 9(02)    VALUE ZEROS.                     00139900
       77 WRK-REGGRAV     PIC 9(02)    VALUE ZEROS.                     00140003
       77 WRK-SALACUM     PIC 9(08)V99 VALUE ZEROS.                     00140100
       77 WRK-MEDIA       PIC 9(08)V99 VALUE ZEROS.                     00140203
      *====================================================             00140300
       PROCEDURE                                 DIVISION.              00140400
      *====================================================             00140500
      *----------------------------------------------------             00140603
       0000-PRINCIPAL                             SECTION.              00140700
      *----------------------------------------------------             00140803
           PERFORM 1000-INICIALIZAR.                                    00140900
           PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.              00141000
           PERFORM 3000-FINALIZAR.                                      00141100
           STOP RUN.                                                    00141200
      *----------------------------------------------------             00141303
       0000-99-FIM.                                  EXIT.              00141400
      *----------------------------------------------------             00141503
                                                                        00141600
      *----------------------------------------------------             00141703
       1000-INICIALIZAR                           SECTION.              00141800
      *----------------------------------------------------             00141903
           EXEC SQL                                                     00142000
              OPEN CFUNC                                                00142100
           END-EXEC.                                                    00142200
                                                                        00142300
           EVALUATE SQLCODE                                             00142400
            WHEN 0                                                      00142500
             PERFORM 4000-LER-FUNCIONARIO                               00142600
            WHEN 100                                                    00142700
             DISPLAY 'SEM FUNCIONARIOS'                                 00142800
            WHEN OTHER                                                  00142900
             MOVE SQLCODE TO WRK-SQLCODE                                00143000
             DISPLAY 'ERRO' WRK-SQLCODE 'NO OPEN CURSOR'                00143100
             STOP RUN                                                   00143200
           END-EVALUATE.                                                00143300
                                                                        00143403
           OPEN OUTPUT SRELBENE.                                        00143504
           PERFORM 1050-TESTARSTATUS.                                   00143603
      *----------------------------------------------------             00143703
       1000-99-FIM.                                  EXIT.              00143800
      *----------------------------------------------------             00143903
                                                                        00144003
      *----------------------------------------------------             00144103
       1050-TESTARSTATUS                          SECTION.              00144203
      *----------------------------------------------------             00144303
           IF WRK-FS-SRELBENE NOT EQUAL 0                               00144405
            DISPLAY 'ERRO NA ABERTURA DO ARQUIVO'                       00144505
            STOP RUN                                                    00144605
           END-IF.                                                      00144705
      *----------------------------------------------------             00144803
       1050-99-FIM.                                  EXIT.              00144903
      *----------------------------------------------------             00145003
                                                                        00145103
      *----------------------------------------------------             00145203
       2000-PROCESSAR                             SECTION.              00145300
      *----------------------------------------------------             00145403
           INITIALIZE WRK-SRELBENE.                                     00145504
            MOVE DB2-ID       TO WRK-ID.                                00145603
            MOVE DB2-NOME     TO WRK-NOME.                              00145703
            MOVE DB2-SALARIO  TO WRK-SALARIO.                           00145803
            MOVE DB2-PLANMED  TO WRK-PLANMED.                           00145903
            MOVE DB2-PLANDENT TO WRK-PLANDENT.                          00146003
            MOVE DB2-COTAS    TO WRK-COTAS.                             00146103
           WRITE FD-SRELBENE FROM WRK-SRELBENE.                         00146204
            ADD 1 TO WRK-REGGRAV.                                       00146303
           PERFORM 4000-LER-FUNCIONARIO.                                00146400
      *----------------------------------------------------             00146503
       2000-99-FIM.                                  EXIT.              00146600
      *----------------------------------------------------             00146703
                                                                        00146800
      *----------------------------------------------------             00147003
       3000-FINALIZAR                             SECTION.              00148000
      *----------------------------------------------------             00149003
           EXEC SQL                                                     00150000
              CLOSE CFUNC                                               00160000
           END-EXEC.                                                    00160100
                                                                        00160203
           CLOSE SRELBENE.                                              00160305
                                                                        00160405
           DISPLAY ' '.                                                 00160506
           DISPLAY 'REGISTROS LIDOS:    '   WRK-REGLIDOS.               00160607
           DISPLAY 'REGISTROS GRAVADOS: '   WRK-REGGRAV.                00160707
           DISPLAY 'SOMA DOS SALARIOS:  '   WRK-SALACUM.                00160807
           DIVIDE WRK-SALACUM BY WRK-REGLIDOS GIVING                    00160905
                  WRK-MEDIA.                                            00161006
           DISPLAY 'MEDIA DOS SALARIOS: ' WRK-MEDIA.                    00161107
      *----------------------------------------------------             00161205
       3000-99-FIM.                                  EXIT.              00161300
      *----------------------------------------------------             00161403
                                                                        00161500
      *----------------------------------------------------             00161603
       4000-LER-FUNCIONARIO                       SECTION.              00161700
      *----------------------------------------------------             00161803
           EXEC SQL                                                     00161900
             FETCH CFUNC                                                00162000
              INTO :DB2-ID,                                             00162100
                   :DB2-NOME,                                           00163000
                   :DB2-SALARIO,                                        00165000
                   :DB2-PLANMED,                                        00166000
                   :DB2-PLANDENT,                                       00166100
                   :DB2-COTAS                                           00166200
           END-EXEC.                                                    00166300
                                                                        00166400
           EVALUATE SQLCODE                                             00166500
            WHEN 0                                                      00166600
             CONTINUE                                                   00166700
             ADD 1 TO WRK-REGLIDOS                                      00166800
             ADD DB2-SALARIO TO WRK-SALACUM                             00166900
            WHEN 100                                                    00167000
             DISPLAY 'FIM DO ARQUIVO'                                   00167105
            WHEN OTHER                                                  00167800
             MOVE SQLCODE TO WRK-SQLCODE                                00167900
             DISPLAY 'ERRO' WRK-SQLCODE                                 00168000
           END-EVALUATE.                                                00168100
      *----------------------------------------------------             00168203
       4000-99-FIM.                                  EXIT.              00168300
      *----------------------------------------------------             00168403

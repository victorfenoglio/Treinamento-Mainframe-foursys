      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22DB06.                                            00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060007
      *  EMPRESA : FOURSYS                                *             00070007
      *  OBJETIVO: LEITURA DE TODOS OS REGISTROS DE UMA   *             00080000
      *            TABELA USANDO DB2,GRAVANDO EM RELDB2   *             00080106
      *---------------------------------------------------*             00080200
      *  ARQUIVOS:                                        *             00080407
      *  DDNAME            I/O             COPY/BOOK      *             00080507
      *  RELDB2             O               --------      *             00080607
      *---------------------------------------------------*             00080707
      *  BASE  DE DADOS:                                  *             00080800
      *  TABELA            I/O           INCLUDE/BOOK     *             00080907
      *  IVAN.FUNC          I               #BKFUNC       *             00081007
      *===================================================*             00081500
                                                                        00081600
      *====================================================             00081700
       ENVIRONMENT                               DIVISION.              00081800
      *====================================================             00081900
       CONFIGURATION                             SECTION.               00082000
       SPECIAL-NAMES.                                                   00083000
           DECIMAL-POINT  IS COMMA.                                     00084000
                                                                        00084100
       INPUT-OUTPUT                              SECTION.               00084200
        FILE-CONTROL.                                                   00084300
           SELECT RELDB2 ASSIGN TO RELDB2                               00084400
               FILE STATUS IS WRK-FS-RELDB2.                            00084500
                                                                        00085000
      *====================================================             00086000
       DATA                                      DIVISION.              00087000
      *====================================================             00088000
      *----------------------------------------------------             00089000
       FILE                                      SECTION.               00090000
      *----------------------------------------------------             00100000
       FD RELDB2                                                        00110006
           RECORDING MODE IS F                                          00111000
           BLOCK CONTAINS 0 RECORDS.                                    00112000
      *-----------LRECL 99----------------------------------            00113000
       01 FD-RELDB2 PIC X(99).                                          00114003
                                                                        00119000
      *----------------------------------------------------             00120000
       WORKING-STORAGE                           SECTION.               00130000
      *----------------------------------------------------             00131000
           EXEC SQL                                                     00132000
              INCLUDE #BKFUNC                                           00133000
           END-EXEC.                                                    00134000
                                                                        00135000
           EXEC SQL                                                     00136000
              INCLUDE SQLCA                                             00137000
           END-EXEC.                                                    00137100
                                                                        00137200
           EXEC SQL                                                     00137300
              DECLARE CFUNC CURSOR FOR                                  00137400
               SELECT * FROM IVAN.FUNC                                  00137500
                ORDER BY ID                                             00137600
           END-EXEC.                                                    00137700
                                                                        00137800
       01 WRK-RELDB2.                                                   00137903
          05 WRK-ID      PIC 99999.                                     00138003
          05 WRK-NOME    PIC X(30).                                     00138103
          05 WRK-SETOR   PIC X(04).                                     00138203
          05 WRK-SALARIO PIC 9999999999.                                00138303
          05 WRK-DATAADM PIC X(10).                                     00138403
          05 WRK-EMAIL   PIC X(40).                                     00138503
      *----------------------------------------------------             00138607
       01 FILLER PIC X(48) VALUE                                        00138707
           '--------------VARIAVEIS PARA APOIO-----------'.             00138807
      *----------------------------------------------------             00138907
       77 WRK-FS-RELDB2   PIC 9(02).                                    00139003
       77 WRK-SQLCODE     PIC -999.                                     00139100
       77 WRK-NULL-EMAIL  PIC S9(04) COMP.                              00139200
      *----------------------------------------------------             00139407
       01 FILLER PIC X(48) VALUE                                        00139507
           '--------------VARIAVEIS CALCULAR/ACUMULAR----'.             00139607
      *----------------------------------------------------             00139707
       77 WRK-MEDIA       PIC 9(08)V99 VALUE ZEROS.                     00139807
       77 WRK-REGLIDOS    PIC 9(02)    VALUE ZEROS.                     00139900
       77 WRK-SALACUM     PIC 9(08)V99 VALUE ZEROS.                     00140000
      *====================================================             00140100
       PROCEDURE                                 DIVISION.              00140200
      *====================================================             00140300
      *-----------------------------------------------------            00140400
       0000-PRINCIPAL                             SECTION.              00140500
      *-----------------------------------------------------            00140600
           PERFORM 1000-INICIALIZAR.                                    00140700
           PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.              00140800
           PERFORM 3000-FINALIZAR.                                      00140900
           STOP RUN.                                                    00141000
      *-----------------------------------------------------            00141100
       0000-99-FIM.                                  EXIT.              00141200
      *-----------------------------------------------------            00141300
                                                                        00141400
      *-----------------------------------------------------            00141500
       1000-INICIALIZAR                           SECTION.              00141600
      *-----------------------------------------------------            00141700
           EXEC SQL                                                     00141800
              OPEN CFUNC                                                00141900
           END-EXEC.                                                    00142000
                                                                        00142100
           EVALUATE SQLCODE                                             00142200
            WHEN 0                                                      00142300
             PERFORM 4000-LER-FUNCIONARIO                               00142400
            WHEN 100                                                    00142500
             DISPLAY 'SEM FUNCIONARIOS'                                 00142600
            WHEN OTHER                                                  00142700
             MOVE SQLCODE TO WRK-SQLCODE                                00142800
             DISPLAY 'ERRO' WRK-SQLCODE 'NO OPEN CURSOR'                00142900
             STOP RUN                                                   00143000
           END-EVALUATE.                                                00143100
                                                                        00143203
           OPEN OUTPUT RELDB2.                                          00143303
           PERFORM 1050-TESTARSTATUS.                                   00143403
      *-----------------------------------------------------            00143500
       1000-99-FIM.                                  EXIT.              00143600
      *-----------------------------------------------------            00143700
                                                                        00143800
      *-----------------------------------------------------            00143903
       1050-TESTARSTATUS                          SECTION.              00144003
      *-----------------------------------------------------            00144103
           IF WRK-FS-RELDB2 NOT EQUAL 0                                 00144206
            DISPLAY 'ERRO NA ABERTURA DO ARQUIVO'                       00144303
            STOP RUN                                                    00144404
           END-IF.                                                      00144504
      *-----------------------------------------------------            00144603
       1050-99-FIM.                                  EXIT.              00144703
      *-----------------------------------------------------            00144803
                                                                        00144903
      *-----------------------------------------------------            00145000
       2000-PROCESSAR                             SECTION.              00145100
      *-----------------------------------------------------            00145200
           INITIALIZE WRK-RELDB2.                                       00145305
            MOVE DB2-ID      TO WRK-ID.                                 00145405
            MOVE DB2-NOME    TO WRK-NOME.                               00145505
            MOVE DB2-SETOR   TO WRK-SETOR.                              00145605
            MOVE DB2-SALARIO TO WRK-SALARIO.                            00145705
            MOVE DB2-DATAADM TO WRK-DATAADM.                            00145805
            MOVE DB2-EMAIL   TO WRK-EMAIL.                              00145905
           WRITE FD-RELDB2 FROM WRK-RELDB2.                             00146005
                                                                        00146105
           PERFORM 4000-LER-FUNCIONARIO.                                00146200
                                                                        00146300
      *-----------------------------------------------------            00146400
       2000-99-FIM.                                  EXIT.              00146500
      *-----------------------------------------------------            00146600
                                                                        00146700
      *-----------------------------------------------------            00147000
       3000-FINALIZAR                             SECTION.              00148000
      *-----------------------------------------------------            00149000
           EXEC SQL                                                     00150000
              CLOSE CFUNC                                               00160000
           END-EXEC.                                                    00160100
                                                                        00160205
           CLOSE RELDB2.                                                00160305
      *-----------------------------------------------------            00160400
       3000-99-FIM.                                  EXIT.              00160500
      *-----------------------------------------------------            00160600
                                                                        00160700
      *-----------------------------------------------------            00160800
       4000-LER-FUNCIONARIO                       SECTION.              00160900
      *-----------------------------------------------------            00161000
           EXEC SQL                                                     00161100
             FETCH CFUNC                                                00161200
              INTO :DB2-ID,                                             00162000
                   :DB2-NOME,                                           00163000
                   :DB2-SETOR,                                          00164000
                   :DB2-SALARIO,                                        00165000
                   :DB2-DATAADM,                                        00166000
                   :DB2-EMAIL      :WRK-NULL-EMAIL                      00166100
           END-EXEC.                                                    00166200
                                                                        00166300
           EVALUATE SQLCODE                                             00166400
            WHEN 0                                                      00166500
             CONTINUE                                                   00166600
             ADD 1 TO WRK-REGLIDOS                                      00166700
             ADD DB2-SALARIO TO WRK-SALACUM                             00166800
            WHEN 100                                                    00166900
             DISPLAY 'FINAL DA TABELA'                                  00167000
             DISPLAY ' '                                                00167100
             DISPLAY 'REGISTROS LIDOS: '   WRK-REGLIDOS                 00167200
             DISPLAY 'SOMA DOS SALARIOS:'  WRK-SALACUM                  00167300
              DIVIDE WRK-SALACUM BY WRK-REGLIDOS GIVING                 00167400
                     WRK-MEDIA                                          00167500
             DISPLAY 'MEDIA DOS SALARIOS:' WRK-MEDIA                    00167600
            WHEN OTHER                                                  00167700
             MOVE SQLCODE TO WRK-SQLCODE                                00167800
             DISPLAY 'ERRO' WRK-SQLCODE                                 00167900
           END-EVALUATE.                                                00168000
      *-----------------------------------------------------            00168100
       4000-99-FIM.                                  EXIT.              00168200
      *-----------------------------------------------------            00168300

      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22DB07.                                            00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060004
      *  EMPRESA : FOURSYS                                *             00070004
      *  OBJETIVO: LEITURA DE TODOS OS REGISTROS DA TABELA*             00080000
      *            FUNC E DA TABELA SETOR, REALIZANDO JOIN*             00080100
      *---------------------------------------------------*             00080200
      *  BASE  DE DADOS:                                  *             00080300
      *  TABELA             I/O           INCLUDE/BOOK    *             00080400
      *  IVAN.FUNC           I               #BKFUNC      *             00080500
      *  IVAN.SETOR          I               #BKSETOR     *             00080600
      *===================================================*             00080700
                                                                        00080800
      *====================================================             00080900
       ENVIRONMENT                               DIVISION.              00081000
      *====================================================             00081100
       CONFIGURATION                             SECTION.               00082000
       SPECIAL-NAMES.                                                   00083000
           DECIMAL-POINT  IS COMMA.                                     00084000
                                                                        00085000
      *====================================================             00086000
       DATA                                      DIVISION.              00087000
      *====================================================             00088000
      *----------------------------------------------------             00089000
       FILE                                      SECTION.               00090000
      *----------------------------------------------------             00100000
                                                                        00110000
      *-----------------------------------------------------            00120000
       WORKING-STORAGE                           SECTION.               00130000
      *-----------------------------------------------------            00131000
           EXEC SQL                                                     00132000
              INCLUDE #BKFUNC                                           00133000
           END-EXEC.                                                    00134000
                                                                        00135000
           EXEC SQL                                                     00135100
              INCLUDE #BKSETOR                                          00135200
           END-EXEC.                                                    00135300
                                                                        00135400
           EXEC SQL                                                     00136000
              INCLUDE SQLCA                                             00137000
           END-EXEC.                                                    00137100
                                                                        00137200
           EXEC SQL                                                     00137300
              DECLARE CFUNC CURSOR FOR                                  00137400
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL,DESCSETOR           00137500
                FROM IVAN.FUNC F, IVAN.SETOR S                          00137600
                 WHERE F.SETOR = S.IDSETOR                              00137700
           END-EXEC.                                                    00137900
                                                                        00138002
       01 WRK-REG.                                                      00138102
          05 WRK-ID        PIC 99999.                                   00138202
          05 WRK-NOME      PIC X(30).                                   00138302
          05 WRK-SETOR     PIC X(04).                                   00138402
          05 WRK-SALARIO   PIC 9999999999.                              00138502
          05 WRK-DATAADM   PIC X(10).                                   00138602
          05 WRK-EMAIL     PIC X(40).                                   00138702
          05 WRK-DESCSETOR PIC X(40).                                   00138802
      *----------------------------------------------------             00138904
       01 FILLER PIC X(48) VALUE                                        00139004
           '--------------VARIAVEIS PARA APOIO-----------'.             00139104
      *----------------------------------------------------             00139204
       77 WRK-SQLCODE     PIC -999.                                     00139300
       77 WRK-NULL-EMAIL  PIC S9(04) COMP.                              00139400
      *----------------------------------------------------             00139504
       01 FILLER PIC X(48) VALUE                                        00139604
           '--------------VARIAVEIS CALCULAR/ACUMULAR----'.             00139704
      *----------------------------------------------------             00139804
       77 WRK-MEDIA       PIC 9(08)V99 VALUE ZEROS.                     00139904
       77 WRK-REGLIDOS    PIC 9(02)    VALUE ZEROS.                     00140000
       77 WRK-SALACUM     PIC 9(08)V99 VALUE ZEROS.                     00140100
      *====================================================             00140200
       PROCEDURE                                 DIVISION.              00140300
      *====================================================             00140400
      *-----------------------------------------------------            00140500
       0000-PRINCIPAL                             SECTION.              00140600
      *-----------------------------------------------------            00140700
           PERFORM 1000-INICIALIZAR.                                    00140800
           PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.              00140900
           PERFORM 3000-FINALIZAR.                                      00141000
           STOP RUN.                                                    00141100
      *-----------------------------------------------------            00141200
       0000-99-FIM.                                  EXIT.              00141300
      *-----------------------------------------------------            00141400
                                                                        00141500
      *-----------------------------------------------------            00141600
       1000-INICIALIZAR                           SECTION.              00141700
      *-----------------------------------------------------            00141800
           EXEC SQL                                                     00141900
              OPEN CFUNC                                                00142000
           END-EXEC.                                                    00142100
                                                                        00142200
           EVALUATE SQLCODE                                             00142300
            WHEN 0                                                      00142400
             PERFORM 4000-LER-FUNCIONARIO                               00142500
            WHEN 100                                                    00142600
             DISPLAY 'SEM FUNCIONARIOS'                                 00142700
            WHEN OTHER                                                  00142800
             MOVE SQLCODE TO WRK-SQLCODE                                00142900
             DISPLAY 'ERRO' WRK-SQLCODE 'NO OPEN CURSOR'                00143000
             STOP RUN                                                   00143100
           END-EVALUATE.                                                00143200
      *-----------------------------------------------------            00143300
       1000-99-FIM.                                  EXIT.              00143400
      *-----------------------------------------------------            00143500
                                                                        00143600
      *-----------------------------------------------------            00143700
       2000-PROCESSAR                             SECTION.              00143800
      *-----------------------------------------------------            00143900
           DISPLAY '---------------------------------------'            00144000
           MOVE DB2-ID      TO WRK-ID                                   00144102
           MOVE DB2-NOME    TO WRK-NOME                                 00144202
           MOVE DB2-SALARIO TO WRK-SALARIO                              00144302
           MOVE DB2-DATAADM TO WRK-DATAADM                              00144402
                                                                        00144501
            IF WRK-NULL-EMAIL = 0                                       00144600
             MOVE DB2-EMAIL   TO WRK-EMAIL                              00144702
            ELSE                                                        00144800
             MOVE 'EMAIL NULO' TO WRK-EMAIL                             00144902
            END-IF.                                                     00145000
           MOVE DB2-DESCSETOR TO WRK-DESCSETOR                          00145102
           DISPLAY WRK-REG                                              00145202
                                                                        00145301
           PERFORM 4000-LER-FUNCIONARIO.                                00145400
                                                                        00145500
      *-----------------------------------------------------            00145600
       2000-99-FIM.                                  EXIT.              00145700
      *-----------------------------------------------------            00145800
                                                                        00146000
      *-----------------------------------------------------            00147000
       3000-FINALIZAR                             SECTION.              00148000
      *-----------------------------------------------------            00149000
           EXEC SQL                                                     00150000
              CLOSE CFUNC                                               00160000
           END-EXEC.                                                    00160100
      *-----------------------------------------------------            00160200
       3000-99-FIM.                                  EXIT.              00160300
      *-----------------------------------------------------            00160400
                                                                        00160500
      *-----------------------------------------------------            00160600
       4000-LER-FUNCIONARIO                       SECTION.              00160700
      *-----------------------------------------------------            00160800
           EXEC SQL                                                     00160900
             FETCH CFUNC                                                00161000
              INTO :DB2-ID,                                             00162000
                   :DB2-NOME,                                           00163000
                   :DB2-SALARIO,                                        00165000
                   :DB2-DATAADM,                                        00166000
                   :DB2-EMAIL      :WRK-NULL-EMAIL,                     00166101
                   :DB2-DESCSETOR                                       00166201
           END-EXEC.                                                    00166300
                                                                        00166400
           EVALUATE SQLCODE                                             00166500
            WHEN 0                                                      00166600
             CONTINUE                                                   00166700
             ADD 1 TO WRK-REGLIDOS                                      00166800
             ADD DB2-SALARIO TO WRK-SALACUM                             00166900
            WHEN 100                                                    00167000
             DISPLAY 'FINAL DA TABELA'                                  00167100
             DISPLAY ' '                                                00167200
             DISPLAY 'REGISTROS LIDOS: '   WRK-REGLIDOS                 00167300
             DISPLAY 'SOMA DOS SALARIOS:'  WRK-SALACUM                  00167400
              DIVIDE WRK-SALACUM BY WRK-REGLIDOS GIVING                 00167500
                     WRK-MEDIA                                          00167600
             DISPLAY 'MEDIA DOS SALARIOS:' WRK-MEDIA                    00167700
            WHEN OTHER                                                  00167800
             MOVE SQLCODE TO WRK-SQLCODE                                00167900
             DISPLAY 'ERRO' WRK-SQLCODE                                 00168000
           END-EVALUATE.                                                00168100
      *-----------------------------------------------------            00168200
       4000-99-FIM.                                  EXIT.              00168300
      *-----------------------------------------------------            00168400

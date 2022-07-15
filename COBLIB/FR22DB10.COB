      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22DB10.                                            00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060004
      *  EMPRESA : FOURSYS                                *             00070004
      *  OBJETIVO: LEITURA DE TODOS OS REGISTROS DE UMA   *             00080000
      *           TABELA USANDO DB2, REALIZANDO CKECKPOINT*             00080101
      *           E RESTART.                              *             00080202
      *---------------------------------------------------*             00080300
      *  BASE  DE DADOS:                                  *             00080400
      *  TABELA             I/O           INCLUDE/BOOK    *             00080500
      *  IVAN.FUNC           I               #BKFUNC      *             00080600
      *  IVAN.CHECKP         I               #BKCHECK     *             00080700
      *===================================================*             00080800
                                                                        00080900
      *====================================================             00081000
       ENVIRONMENT                               DIVISION.              00081100
      *====================================================             00081200
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
              INCLUDE #BKCHECK                                          00135200
           END-EXEC.                                                    00135300
                                                                        00135400
           EXEC SQL                                                     00136000
              INCLUDE SQLCA                                             00137000
           END-EXEC.                                                    00137100
                                                                        00137200
           EXEC SQL                                                     00137300
              DECLARE CFUNC CURSOR FOR                                  00137400
               SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL               00137500
               FROM IVAN.FUNC F WHERE F.ID >=                           00137600
                (SELECT REGISTRO FROM IVAN.CHECKP C                     00137700
                  WHERE C.ID = 'IVAN')                                  00137800
                ORDER BY ID                                             00137900
           END-EXEC.                                                    00138000
                                                                        00138100
      *----------------------------------------------------             00138200
       01 FILLER PIC X(48) VALUE                                        00138300
           '--------------VARIAVEIS PARA APOIO-----------'.             00138404
      *----------------------------------------------------             00138500
       77 WRK-ID          PIC 9(05)    VALUE ZEROS.                     00138600
       77 WRK-SQLCODE     PIC -999.                                     00138700
       77 WRK-NULL-EMAIL  PIC S9(04) COMP.                              00138800
      *----------------------------------------------------             00139000
       01 FILLER PIC X(48) VALUE                                        00139100
           '--------------VARIAVEIS PARA ACUMULAR--------'.             00139200
      *----------------------------------------------------             00139300
       77 WRK-MEDIA       PIC 9(08)V99 VALUE ZEROS.                     00139400
       77 WRK-REGLIDOS    PIC 9(02)    VALUE ZEROS.                     00139500
       77 WRK-SALACUM     PIC 9(08)V99 VALUE ZEROS.                     00139600
      *====================================================             00139700
       PROCEDURE                                 DIVISION.              00139800
      *====================================================             00139900
      *-----------------------------------------------------            00140000
       0000-PRINCIPAL                             SECTION.              00140100
      *-----------------------------------------------------            00140200
           PERFORM 1000-INICIALIZAR.                                    00140300
           PERFORM 2000-PROCESSAR UNTIL SQLCODE EQUAL 100.              00140400
            PERFORM 2900-ZERACKECK.                                     00140500
           PERFORM 3000-FINALIZAR.                                      00140600
           STOP RUN.                                                    00140700
      *-----------------------------------------------------            00140800
       0000-99-FIM.                                  EXIT.              00140900
      *-----------------------------------------------------            00141000
                                                                        00141100
      *-----------------------------------------------------            00141200
       1000-INICIALIZAR                           SECTION.              00141300
      *-----------------------------------------------------            00141400
           EXEC SQL                                                     00141500
              OPEN CFUNC                                                00141600
           END-EXEC.                                                    00141700
                                                                        00141800
           EVALUATE SQLCODE                                             00141900
            WHEN 0                                                      00142000
             PERFORM 4000-LER-FUNCIONARIO                               00142100
            WHEN 100                                                    00142200
             DISPLAY 'SEM FUNCIONARIOS'                                 00142300
            WHEN OTHER                                                  00142400
             MOVE SQLCODE TO WRK-SQLCODE                                00142500
             DISPLAY 'ERRO' WRK-SQLCODE 'NO OPEN CURSOR'                00142600
             STOP RUN                                                   00142700
           END-EVALUATE.                                                00142800
      *-----------------------------------------------------            00142900
       1000-99-FIM.                                  EXIT.              00143000
      *-----------------------------------------------------            00143100
                                                                        00143200
      *-----------------------------------------------------            00143300
       2000-PROCESSAR                             SECTION.              00143400
      *-----------------------------------------------------            00143500
           IF DB2-SALARIO EQUAL ZEROS                                   00143600
            DISPLAY 'ERRO NO REGISTRO' DB2-ID                           00143700
            EXEC SQL                                                    00143800
             UPDATE IVAN.CHECKP SET REGISTRO = :DB2-ID                  00143902
              WHERE ID = 'IVAN'                                         00144000
            END-EXEC                                                    00144100
            PERFORM 3000-FINALIZAR                                      00144200
            GOBACK                                                      00144303
           END-IF.                                                      00144400
                                                                        00144500
           DISPLAY '---------------------------------------'            00144600
           DISPLAY 'ID:     ' DB2-ID                                    00144700
           DISPLAY 'NOME:   ' DB2-NOME                                  00144800
           DISPLAY 'SETOR:  ' DB2-SETOR                                 00144900
           DISPLAY 'SALARIO:' DB2-SALARIO                               00145000
           DISPLAY 'DATAADM:' DB2-DATAADM                               00145100
            IF WRK-NULL-EMAIL = 0                                       00145200
             DISPLAY 'EMAIL' DB2-EMAIL                                  00145300
            ELSE                                                        00145400
             DISPLAY 'EMAIL NULO'                                       00145500
            END-IF.                                                     00145600
                                                                        00145700
           PERFORM 4000-LER-FUNCIONARIO.                                00145800
                                                                        00145900
      *-----------------------------------------------------            00146000
       2000-99-FIM.                                  EXIT.              00146100
      *-----------------------------------------------------            00146200
                                                                        00146300
      *-----------------------------------------------------            00146400
       2900-ZERACKECK                             SECTION.              00146503
      *-----------------------------------------------------            00146600
           EXEC SQL                                                     00146700
            UPDATE IVAN.CHECKP SET REGISTRO = 0                         00146800
            WHERE ID = 'IVAN'                                           00146900
           END-EXEC.                                                    00147000
      *-----------------------------------------------------            00147100
       2900-99-FIM.                                  EXIT.              00147200
      *-----------------------------------------------------            00147300
                                                                        00147400
      *-----------------------------------------------------            00147500
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

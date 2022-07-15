      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22DB08.                                            00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060005
      *  EMPRESA : FOURSYS                                *             00070005
      *  OBJETIVO: LEITURA DE TODOS OS REGISTROS DA TABELA*             00080000
      *            FUNC E DA TABELA SETOR, REALIZANDO JOIN*             00080100
      *            SYSOUT COM REGISTROS QUE NAO BATEM NO  *             00080204
      *            CAMPO SETOR(LEFT EXCLUDE JOIN).        *             00080304
      *---------------------------------------------------*             00080400
      *  BASE  DE DADOS:                                  *             00080500
      *  TABELA             I/O           INCLUDE/BOOK    *             00080600
      *  IVAN.FUNC           I               #BKFUNC      *             00080700
      *  IVAN.SETOR          I               #BKSETOR     *             00080800
      *===================================================*             00080900
                                                                        00081000
      *====================================================             00081100
       ENVIRONMENT                               DIVISION.              00081200
      *====================================================             00081300
       CONFIGURATION                             SECTION.               00081400
       SPECIAL-NAMES.                                                   00081500
           DECIMAL-POINT  IS COMMA.                                     00081600
                                                                        00081700
      *====================================================             00081800
       DATA                                      DIVISION.              00081900
      *====================================================             00082000
      *----------------------------------------------------             00082100
       FILE                                      SECTION.               00082200
      *----------------------------------------------------             00083000
                                                                        00084000
      *-----------------------------------------------------            00085000
       WORKING-STORAGE                           SECTION.               00086000
      *-----------------------------------------------------            00087000
           EXEC SQL                                                     00088000
              INCLUDE #BKFUNC                                           00089000
           END-EXEC.                                                    00090000
                                                                        00100000
           EXEC SQL                                                     00110000
              INCLUDE #BKSETOR                                          00120000
           END-EXEC.                                                    00130000
                                                                        00131000
           EXEC SQL                                                     00132000
              INCLUDE SQLCA                                             00133000
           END-EXEC.                                                    00134000
                                                                        00135000
           EXEC SQL                                                     00136000
              DECLARE CFUNC CURSOR FOR                                  00137000
               SELECT ID,NOME,SALARIO,DATAADM,EMAIL                     00137103
                FROM IVAN.FUNC F WHERE NOT EXISTS                       00137201
            (SELECT IDSETOR FROM IVAN.SETOR S WHERE F.SETOR = S.IDSETOR)00137301
           END-EXEC.                                                    00137400
                                                                        00137500
       01 WRK-REG.                                                      00137600
          05 WRK-ID        PIC 99999.                                   00137700
          05 WRK-NOME      PIC X(30).                                   00137800
          05 WRK-SETOR     PIC X(04).                                   00137900
          05 WRK-SALARIO   PIC 9999999999.                              00138000
          05 WRK-DATAADM   PIC X(10).                                   00138100
          05 WRK-EMAIL     PIC X(40).                                   00138200
          05 WRK-DESCSETOR PIC X(40).                                   00138300
      *----------------------------------------------------             00138405
       01 FILLER PIC X(48) VALUE                                        00138505
           '--------------VARIAVEIS PARA APOIO-----------'.             00138605
      *----------------------------------------------------             00138705
       77 WRK-SQLCODE     PIC -999.                                     00138800
       77 WRK-NULL-EMAIL  PIC S9(04) COMP.                              00138900
      *----------------------------------------------------             00139005
       01 FILLER PIC X(48) VALUE                                        00139105
           '--------------VARIAVEIS PARA ACUMULAR--------'.             00139205
      *----------------------------------------------------             00139305
       77 WRK-MEDIA       PIC 9(08)V99 VALUE ZEROS.                     00139405
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
           PERFORM 3000-FINALIZAR.                                      00140500
           STOP RUN.                                                    00140600
      *-----------------------------------------------------            00140700
       0000-99-FIM.                                  EXIT.              00140800
      *-----------------------------------------------------            00140900
                                                                        00141000
      *-----------------------------------------------------            00141100
       1000-INICIALIZAR                           SECTION.              00141200
      *-----------------------------------------------------            00141300
           EXEC SQL                                                     00141400
              OPEN CFUNC                                                00141500
           END-EXEC.                                                    00141600
                                                                        00141700
           EVALUATE SQLCODE                                             00141800
            WHEN 0                                                      00141900
             PERFORM 4000-LER-FUNCIONARIO                               00142000
            WHEN 100                                                    00142100
             DISPLAY 'SEM FUNCIONARIOS'                                 00142200
            WHEN OTHER                                                  00142300
             MOVE SQLCODE TO WRK-SQLCODE                                00142400
             DISPLAY 'ERRO' WRK-SQLCODE 'NO OPEN CURSOR'                00142500
             STOP RUN                                                   00142600
           END-EVALUATE.                                                00142700
      *-----------------------------------------------------            00142800
       1000-99-FIM.                                  EXIT.              00142900
      *-----------------------------------------------------            00143000
                                                                        00143100
      *-----------------------------------------------------            00143200
       2000-PROCESSAR                             SECTION.              00143300
      *-----------------------------------------------------            00143400
           DISPLAY '---------------------------------------'            00143500
           MOVE DB2-ID      TO WRK-ID                                   00143600
           MOVE DB2-NOME    TO WRK-NOME                                 00143700
           MOVE DB2-SALARIO TO WRK-SALARIO                              00143800
           MOVE DB2-DATAADM TO WRK-DATAADM                              00143900
                                                                        00144000
            IF WRK-NULL-EMAIL = 0                                       00144100
             MOVE DB2-EMAIL   TO WRK-EMAIL                              00144200
            ELSE                                                        00144300
             MOVE 'EMAIL NULO' TO WRK-EMAIL                             00144400
            END-IF.                                                     00144500
           MOVE DB2-DESCSETOR TO WRK-DESCSETOR                          00144600
           DISPLAY WRK-REG                                              00144700
                                                                        00144800
           PERFORM 4000-LER-FUNCIONARIO.                                00144900
                                                                        00145000
      *-----------------------------------------------------            00145100
       2000-99-FIM.                                  EXIT.              00145200
      *-----------------------------------------------------            00145300
                                                                        00145400
      *-----------------------------------------------------            00145500
       3000-FINALIZAR                             SECTION.              00145600
      *-----------------------------------------------------            00146000
           EXEC SQL                                                     00147000
              CLOSE CFUNC                                               00148000
           END-EXEC.                                                    00149000
      *-----------------------------------------------------            00150000
       3000-99-FIM.                                  EXIT.              00160000
      *-----------------------------------------------------            00160100
                                                                        00160200
      *-----------------------------------------------------            00160300
       4000-LER-FUNCIONARIO                       SECTION.              00160400
      *-----------------------------------------------------            00160500
           EXEC SQL                                                     00160600
             FETCH CFUNC                                                00160700
              INTO :DB2-ID,                                             00160800
                   :DB2-NOME,                                           00160900
                   :DB2-SALARIO,                                        00161000
                   :DB2-DATAADM,                                        00162000
                   :DB2-EMAIL      :WRK-NULL-EMAIL,                     00163000
                   :DB2-DESCSETOR                                       00164000
           END-EXEC.                                                    00165000
                                                                        00166000
           EVALUATE SQLCODE                                             00166100
            WHEN 0                                                      00166200
             CONTINUE                                                   00166300
             ADD 1 TO WRK-REGLIDOS                                      00166400
             ADD DB2-SALARIO TO WRK-SALACUM                             00166500
            WHEN 100                                                    00166600
             DISPLAY 'FINAL DA TABELA'                                  00166700
             DISPLAY ' '                                                00166800
             DISPLAY 'REGISTROS LIDOS: '   WRK-REGLIDOS                 00166900
             DISPLAY 'SOMA DOS SALARIOS:'  WRK-SALACUM                  00167000
              DIVIDE WRK-SALACUM BY WRK-REGLIDOS GIVING                 00167100
                     WRK-MEDIA                                          00167200
             DISPLAY 'MEDIA DOS SALARIOS:' WRK-MEDIA                    00167300
            WHEN OTHER                                                  00167400
             MOVE SQLCODE TO WRK-SQLCODE                                00167500
             DISPLAY 'ERRO' WRK-SQLCODE                                 00167600
           END-EVALUATE.                                                00167700
      *-----------------------------------------------------            00167800
       4000-99-FIM.                                  EXIT.              00167900
      *-----------------------------------------------------            00168000

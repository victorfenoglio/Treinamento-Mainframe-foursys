      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22DB05.                                            00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060021
      *  EMPRESA : FOURSYS                                *             00070021
      *  OBJETIVO: ALTERAR DADO EM UMA TABELA USANDO DB2  *             00080000
      *---------------------------------------------------*             00080100
      *  BASE  DE DADOS:                                  *             00080200
      *  TABELA             I/O           INCLUDE/BOOK    *             00080300
      *  IVAN.FUNC           I               #BKFUNC      *             00080400
      *===================================================*             00080500
                                                                        00080600
      *====================================================             00080700
       ENVIRONMENT                               DIVISION.              00080800
      *====================================================             00080900
       CONFIGURATION                             SECTION.               00081000
       SPECIAL-NAMES.                                                   00082000
           DECIMAL-POINT  IS COMMA.                                     00083000
                                                                        00084000
      *====================================================             00085000
       DATA                                      DIVISION.              00086000
      *====================================================             00087000
      *----------------------------------------------------             00088000
       FILE                                      SECTION.               00089000
      *----------------------------------------------------             00090000
                                                                        00100000
      *-----------------------------------------------------            00110000
       WORKING-STORAGE                           SECTION.               00120000
      *-----------------------------------------------------            00130000
           EXEC SQL                                                     00131000
              INCLUDE #BKFUNC                                           00132000
           END-EXEC.                                                    00133000
                                                                        00134000
           EXEC SQL                                                     00135000
              INCLUDE SQLCA                                             00136000
           END-EXEC.                                                    00137000
                                                                        00137121
       01 WRK-ID.                                                       00137606
          02 FILLER          PIC X(10).                                 00137706
          02 WRK-ID-AC       PIC 9(05).                                 00137806
                                                                        00137906
       01 WRK-NOME.                                                     00138006
          02 FILLER          PIC X(10).                                 00138106
          02 WRK-NOME-AC     PIC X(30).                                 00138206
                                                                        00138306
       01 WRK-SETOR.                                                    00138406
          02 FILLER          PIC X(10).                                 00138506
          02 WRK-SETOR-AC    PIC X(04).                                 00138606
                                                                        00138706
       01 WRK-SALARIO.                                                  00138813
          02 FILLER          PIC X(10).                                 00138906
          02 WRK-SALARIO-AC  PIC 9(08)V9(02).                           00139006
                                                                        00139106
       01 WRK-DATAADM.                                                  00139206
          02 FILLER          PIC X(10).                                 00139306
          02 WRK-DATAADM-AC  PIC X(10).                                 00139406
                                                                        00139506
       01 WRK-EMAIL.                                                    00139606
          02 FILLER          PIC X(10).                                 00139706
          02 WRK-EMAIL-AC    PIC X(40).                                 00139806
                                                                        00139900
      *----------------------------------------------------             00140021
       01 FILLER PIC X(48) VALUE                                        00140121
           '--------------VARIAVEIS PARA APOIO-----------'.             00140221
      *----------------------------------------------------             00140321
       77 WRK-SQLCODE        PIC -999.                                  00140400
       77 WRK-NULL-EMAIL     PIC S9(04) COMP.                           00140503
      *====================================================             00140600
       PROCEDURE                                 DIVISION.              00140700
      *====================================================             00140800
      *-----------------------------------------------------            00140900
       0000-PRINCIPAL                             SECTION.              00141000
      *-----------------------------------------------------            00141100
           PERFORM 1000-INICIALIZAR.                                    00141200
           PERFORM 2000-PROCESSAR.                                      00141300
           PERFORM 3000-FINALIZAR.                                      00141400
           STOP RUN.                                                    00141500
      *-----------------------------------------------------            00141600
       0000-99-FIM.                                  EXIT.              00141700
      *-----------------------------------------------------            00141800
                                                                        00141900
      *-----------------------------------------------------            00142000
       1000-INICIALIZAR                           SECTION.              00142100
      *-----------------------------------------------------            00142200
           ACCEPT WRK-ID      FROM SYSIN.                               00142319
           ACCEPT WRK-NOME    FROM SYSIN.                               00142419
           ACCEPT WRK-SETOR   FROM SYSIN.                               00142519
           ACCEPT WRK-SALARIO FROM SYSIN.                               00142614
           ACCEPT WRK-DATAADM FROM SYSIN.                               00142715
           ACCEPT WRK-EMAIL   FROM SYSIN.                               00142819
      *-----------------------------------------------------            00143000
       1000-99-FIM.                                  EXIT.              00144000
      *-----------------------------------------------------            00144100
                                                                        00144200
      *-----------------------------------------------------            00144300
       2000-PROCESSAR                             SECTION.              00144400
      *-----------------------------------------------------            00144500
           MOVE WRK-ID-AC TO DB2-ID.                                    00144617
           EXEC SQL                                                     00144700
            SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL                  00144801
            INTO :DB2-ID,                                               00144901
                 :DB2-NOME,                                             00145001
                 :DB2-SETOR,                                            00145101
                 :DB2-SALARIO,                                          00145201
                 :DB2-DATAADM,                                          00145301
                 :DB2-EMAIL      :WRK-NULL-EMAIL                        00145401
            FROM IVAN.FUNC                                              00145512
             WHERE ID = :DB2-ID                                         00145601
           END-EXEC.                                                    00145700
                                                                        00145800
           IF WRK-NOME-AC NOT EQUAL DB2-NOME AND                        00145919
              WRK-NOME-AC NOT EQUAL SPACES                              00146019
            MOVE WRK-NOME-AC TO DB2-NOME                                00146101
           END-IF.                                                      00146201
                                                                        00146301
           IF WRK-SETOR-AC NOT EQUAL DB2-SETOR AND                      00146419
              WRK-SETOR-AC NOT EQUAL SPACES                             00146519
            MOVE WRK-SETOR-AC TO DB2-SETOR                              00146601
           END-IF.                                                      00146701
                                                                        00146801
           IF WRK-SALARIO-AC NOT EQUAL DB2-SALARIO AND                  00146919
              WRK-SALARIO-AC IS NUMERIC                                 00147019
            MOVE WRK-SALARIO-AC TO DB2-SALARIO                          00147101
           END-IF.                                                      00147201
                                                                        00147301
           IF WRK-DATAADM-AC NOT EQUAL DB2-DATAADM AND                  00147419
              WRK-DATAADM-AC NOT EQUAL SPACES                           00147519
            MOVE WRK-DATAADM-AC TO DB2-DATAADM                          00147605
           END-IF.                                                      00147701
                                                                        00147801
           IF WRK-NULL-EMAIL NOT EQUAL 0 AND                            00147918
              WRK-EMAIL-AC NOT EQUAL SPACES                             00148020
            MOVE WRK-EMAIL-AC TO DB2-EMAIL                              00148120
           END-IF.                                                      00148218
                                                                        00148318
           IF WRK-EMAIL-AC NOT EQUAL DB2-EMAIL AND                      00148419
              WRK-EMAIL-AC NOT EQUAL SPACES                             00148519
            MOVE WRK-EMAIL-AC TO DB2-EMAIL                              00148601
           END-IF.                                                      00148701
                                                                        00148801
                                                                        00149201
           EVALUATE SQLCODE                                             00149300
            WHEN 0                                                      00149400
             PERFORM 2100-ALTERAR                                       00149518
             DISPLAY 'REGISTRO ' WRK-ID ' ALTERADO COM SUCESSO'         00149600
            WHEN 100                                                    00149700
             DISPLAY WRK-ID ' NAO ENCONTRADO'                           00149800
            WHEN OTHER                                                  00149900
             MOVE SQLCODE TO WRK-SQLCODE                                00150000
             DISPLAY 'ERRO'  WRK-SQLCODE                                00150100
           END-EVALUATE.                                                00150200
      *-----------------------------------------------------            00150300
       2000-99-FIM.                                  EXIT.              00150400
      *-----------------------------------------------------            00150500
                                                                        00150618
      *-----------------------------------------------------            00150718
       2100-ALTERAR                               SECTION.              00150818
      *-----------------------------------------------------            00150918
           IF WRK-NULL-EMAIL EQUAL 0                                    00151019
            EXEC SQL                                                    00151119
             UPDATE IVAN.FUNC SET NOME    = :DB2-NOME,                  00151219
                                  SETOR   = :DB2-SETOR,                 00151319
                                  SALARIO = :DB2-SALARIO,               00151419
                                  DATAADM = :DB2-DATAADM,               00151519
                                  EMAIL   = :DB2-EMAIL                  00151619
                  WHERE ID = :DB2-ID                                    00151718
            END-EXEC                                                    00151819
           ELSE                                                         00151919
            EXEC SQL                                                    00152019
             UPDATE IVAN.FUNC SET NOME    = :DB2-NOME,                  00152119
                                  SETOR   = :DB2-SETOR,                 00152219
                                  SALARIO = :DB2-SALARIO,               00152319
                                  DATAADM = :DB2-DATAADM,               00152419
                                  EMAIL   = NULL                        00152519
                  WHERE ID = :DB2-ID                                    00152619
            END-EXEC                                                    00152719
           END-IF.                                                      00152819
      *-----------------------------------------------------            00152918
       2100-99-FIM.                                  EXIT.              00153018
      *-----------------------------------------------------            00153118
                                                                        00153200
      *-----------------------------------------------------            00153300
       3000-FINALIZAR                             SECTION.              00153400
      *-----------------------------------------------------            00153500
           DISPLAY 'FIM DE PROCESSAMENTO'.                              00153600
      *-----------------------------------------------------            00153700
       3000-99-FIM.                                  EXIT.              00154000
      *-----------------------------------------------------            00160000

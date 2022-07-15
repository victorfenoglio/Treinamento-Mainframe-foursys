      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. REL2203.                                             00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060000
      *  EMPRESA : FOURSYS                                *             00070000
      *  OBJETIVO: EXIBIR RELATORIO PRODUTOS COM LRECL=45,*             00080000
      *            INSERINDO CABECALHO E NUMERO DE PAGINA.*             00080100
      *---------------------------------------------------*             00081100
      *  ARQUIVOS:                                        *             00081200
      *  DDNAME             I/O           INCLUDE/BOOK    *             00081300
      *  FUNC                I             ---------      *             00081400
      *===================================================*             00082000
       ENVIRONMENT                               DIVISION.              00084000
      *====================================================             00085000
       CONFIGURATION                             SECTION.               00086000
       SPECIAL-NAMES.                                                   00087000
           DECIMAL-POINT  IS COMMA.                                     00088000
                                                                        00089000
       INPUT-OUTPUT                              SECTION.               00090000
       FILE-CONTROL.                                                    00100000
           SELECT FUNC ASSIGN TO FUNC                                   00110000
               FILE STATUS IS WRK-FS-FUNC.                              00120000
                                                                        00130000
      *====================================================             00130100
       DATA                                      DIVISION.              00130200
      *====================================================             00130300
      *-----------------------------------------------------            00130400
       FILE                                      SECTION.               00130500
      *-----------------------------------------------------            00130600
       FD FUNC                                                          00130700
           RECORDING MODE IS F                                          00130800
           LABEL RECORD IS STANDARD                                     00130900
           BLOCK CONTAINS 0 RECORDS.                                    00131000
                                                                        00132000
       01 FD-FUNC.                                                      00133000
          05 FD-IDFUNC     PIC 9(05).                                   00133100
          05 FD-NOMEFUNC   PIC X(30).                                   00133200
          05 FD-SETORFUNC  PIC 9(04).                                   00133300
          05 FD-SALFUNC    PIC 9(06).                                   00133400
                                                                        00133500
      *-----------------------------------------------------            00133600
       WORKING-STORAGE                           SECTION.               00133700
      *-----------------------------------------------------            00133900
           COPY '#GLOG'.                                                00134000
                                                                        00134100
       01 WRK-CABEC.                                                    00134200
          05 WRK-CABEC1.                                                00134300
            10 FILLER       PIC X(03).                                  00134400
            10 FILLER       PIC X(12).                                  00134500
            10 FILLER       PIC X(37) VALUE                             00134600
            'RELATORIO DE FUNCIONARIOS       PAG: '.                    00134700
            10 WRK-PAG      PIC 9(02) VALUE 1.                          00134800
            10 FILLER       PIC X(07).                                  00134900
          05 WRK-CABEC2.                                                00135000
            10 FILLER       PIC X(03).                                  00135100
            10 FILLER       PIC X(09) VALUE 'ID       '.                00135200
            10 FILLER       PIC X(04) VALUE 'NOME'.                     00135300
            10 FILLER       PIC X(26).                                  00135400
            10 FILLER       PIC X(12) VALUE 'SETOR       '.             00135500
            10 FILLER       PIC X(07) VALUE 'SALARIO'.                  00135600
                                                                        00135700
       01 WRK-FUNC-SAIDA.                                               00135800
          05 WRK-ID-SAIDA      PIC X(05).                               00135900
          05 FILLER            PIC X(04).                               00136000
          05 WRK-NOME-SAIDA    PIC X(30).                               00136100
          05 WRK-SETOR-SAIDA   PIC X(04).                               00136200
          05 FILLER            PIC X(08).                               00136300
          05 WRK-SALARIO-SAIDA PIC X(06).                               00136400
          05 FILLER            PIC X(01).                               00136500
      *----------------------------------------------------             00136600
       01 FILLER PIC X(48) VALUE                                        00136700
           '--------------VARIAVEIS PARA FILE STATUS-----'.             00136800
      *----------------------------------------------------             00136900
       77 WRK-FS-FUNC          PIC 9(02).                               00137000
      *----------------------------------------------------             00137100
       01 FILLER PIC X(48) VALUE                                        00137200
           '--------------VARIAVEIS PARA ACUMULAR--------'.             00137300
      *----------------------------------------------------             00137400
       77 WRK-ACUM-SAL         PIC 9(06).                               00137500
       77 WRK-ACUM-LIDOS       PIC 9(02).                               00137600
       77 WRK-SALTOTAL         PIC 9(06).                               00137700
       77 WRK-LINHAS           PIC 9(02) VALUE 1.                       00137800
      *====================================================             00137900
       PROCEDURE                                 DIVISION.              00138000
      *====================================================             00138100
      *-----------------------------------------------------            00138200
       0000-PRINCIPAL                             SECTION.              00138300
      *-----------------------------------------------------            00138400
           PERFORM 1000-INICIALIZAR.                                    00138500
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-FUNC                     00138600
                    EQUAL 10.                                           00138700
           PERFORM 3000-FINALIZAR.                                      00138800
           STOP RUN.                                                    00138900
      *-----------------------------------------------------            00139000
       0000-99-FIM.                                  EXIT.              00139100
      *-----------------------------------------------------            00139200
                                                                        00139300
      *-----------------------------------------------------            00139400
       1000-INICIALIZAR                           SECTION.              00139500
      *-----------------------------------------------------            00139600
           OPEN INPUT FUNC.                                             00139700
             PERFORM 4000-TESTAR-STATUS.                                00139800
           READ FUNC.                                                   00139900
           IF WRK-FS-FUNC EQUAL 10                                      00140000
            DISPLAY 'FINAL DE ARQUIVO'                                  00140100
           END-IF.                                                      00140200
            PERFORM 1100-IMPRIME-CABEC.                                 00140300
      *-----------------------------------------------------            00140400
       1000-99-FIM.                                  EXIT.              00140500
      *-----------------------------------------------------            00140600
                                                                        00140700
      *-----------------------------------------------------            00140800
       1100-IMPRIME-CABEC                         SECTION.              00140900
      *-----------------------------------------------------            00141000
            DISPLAY ' '.                                                00141100
            DISPLAY WRK-CABEC1.                                         00141200
            DISPLAY ' '.                                                00141300
            DISPLAY WRK-CABEC2.                                         00141400
             ADD 4 TO WRK-LINHAS.                                       00141500
      *-----------------------------------------------------            00141600
       1100-99-FIM.                                  EXIT.              00141700
      *-----------------------------------------------------            00141800
                                                                        00141900
      *-----------------------------------------------------            00142000
       2000-PROCESSAR                             SECTION.              00142100
      *-----------------------------------------------------            00142200
           ADD 1 TO WRK-ACUM-LIDOS.                                     00142300
           ADD FD-SALFUNC TO WRK-ACUM-SAL.                              00142400
           ADD FD-SALFUNC TO WRK-SALTOTAL.                              00142500
            MOVE FD-IDFUNC      TO WRK-ID-SAIDA.                        00142600
            MOVE FD-NOMEFUNC    TO WRK-NOME-SAIDA.                      00142700
            MOVE FD-SETORFUNC   TO WRK-SETOR-SAIDA.                     00142800
            MOVE FD-SALFUNC     TO WRK-SALARIO-SAIDA.                   00142900
             DISPLAY WRK-LINHAS '-' WRK-FUNC-SAIDA.                     00143000
             ADD 1 TO WRK-LINHAS.                                       00143100
                                                                        00143200
              IF WRK-LINHAS GREATER 6                                   00143300
               DISPLAY 'TOTAL SALARIO DO SETOR:' WRK-ACUM-SAL           00143400
               MOVE 0 TO WRK-ACUM-SAL                                   00143500
               ADD 1 TO WRK-PAG                                         00143600
               MOVE 1 TO WRK-LINHAS                                     00143700
               PERFORM 1100-IMPRIME-CABEC                               00143800
              END-IF.                                                   00143900
                                                                        00144000
           READ FUNC.                                                   00144100
           IF WRK-FS-FUNC EQUAL 10                                      00144200
              DISPLAY 'TOTAL SALARIO DO SETOR: ' WRK-ACUM-SAL           00144300
           END-IF.                                                      00144400
      *-----------------------------------------------------            00144500
       2000-99-FIM.                                  EXIT.              00144600
      *-----------------------------------------------------            00145000
                                                                        00146000
      *-----------------------------------------------------            00147000
       3000-FINALIZAR                             SECTION.              00148000
      *-----------------------------------------------------            00149000
           CLOSE FUNC.                                                  00150000
           DISPLAY ' '.                                                 00150100
           DISPLAY 'TOTAL REGISTROS LIDOS:' WRK-ACUM-LIDOS.             00151000
           DISPLAY 'TOTAL SALARIOS:'        WRK-SALTOTAL.               00151100
      *-----------------------------------------------------            00152000
       3000-99-FIM.                                  EXIT.              00153000
      *-----------------------------------------------------            00154000
                                                                        00155000
      *-----------------------------------------------------            00156000
       4000-TESTAR-STATUS                         SECTION.              00157000
      *-----------------------------------------------------            00158000
           IF WRK-FS-FUNC NOT EQUAL 0                                   00159000
             MOVE 'REL2203'             TO WRK-PROGRAMA                 00160000
             MOVE '4000'                TO WRK-SECAO                    00160100
             MOVE 'ERRO NO OPEN PRODUT' TO WRK-MENSAGEM                 00160200
             MOVE WRK-FS-FUNC           TO WRK-STATUS                   00160300
             PERFORM 9000-ERRO                                          00160400
           END-IF.                                                      00160500
      *-----------------------------------------------------            00160600
       4000-99-FIM.                                  EXIT.              00160700
      *-----------------------------------------------------            00160800
                                                                        00160900
      *-----------------------------------------------------            00161000
       9000-ERRO                                  SECTION.              00162000
      *-----------------------------------------------------            00163000
           DISPLAY WRK-MENSAGEM.                                        00163100
            CALL 'GRAVALOG' USING WRK-DADOS.                            00163200
           GOBACK.                                                      00163300
      *-----------------------------------------------------            00163400
       9000-99-FIM.                                  EXIT.              00163500
      *-----------------------------------------------------            00163600

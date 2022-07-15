      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. REL2204.                                             00040000
      *===================================================*             00050000
      *  AUTOR   : VICTOR LEAL                            *             00060000
      *  EMPRESA : FOURSYS                                *             00070000
      *  OBJETIVO: EXIBIR RELATORIO PRODUTOS COM LRECL=45,*             00080000
      *            INSERINDO CABECALHO E NUMERO DE PAGINA,*             00080100
      *            SOMANDO SALARIO POR SETOR.             *             00080200
      *---------------------------------------------------*             00080300
      *  ARQUIVOS:                                        *             00080400
      *  DDNAME             I/O           INCLUDE/BOOK    *             00080500
      *  FUNC                I             ---------      *             00080600
      *===================================================*             00080700
                                                                        00080900
      *====================================================             00081000
       ENVIRONMENT                               DIVISION.              00081100
      *====================================================             00081200
       CONFIGURATION                             SECTION.               00081300
       SPECIAL-NAMES.                                                   00081400
           DECIMAL-POINT  IS COMMA.                                     00081500
                                                                        00081600
       INPUT-OUTPUT                              SECTION.               00082000
       FILE-CONTROL.                                                    00083000
           SELECT FUNC ASSIGN TO FUNC                                   00084000
               FILE STATUS IS WRK-FS-FUNC.                              00085000
                                                                        00086000
      *====================================================             00087000
       DATA                                      DIVISION.              00088000
      *====================================================             00089000
      *-----------------------------------------------------            00090000
       FILE                                      SECTION.               00100000
      *-----------------------------------------------------            00110000
       FD FUNC                                                          00120000
           RECORDING MODE IS F                                          00130000
           LABEL RECORD IS STANDARD                                     00130100
           BLOCK CONTAINS 0 RECORDS.                                    00130200
                                                                        00130300
       01 FD-FUNC.                                                      00130400
          05 FD-IDFUNC     PIC 9(05).                                   00130500
          05 FD-NOMEFUNC   PIC X(30).                                   00130600
          05 FD-SETORFUNC  PIC 9(04).                                   00130700
          05 FD-SALFUNC    PIC 9(06).                                   00130800
                                                                        00130900
      *-----------------------------------------------------            00131000
       WORKING-STORAGE                           SECTION.               00132000
      *-----------------------------------------------------            00133000
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
      *----------------------------------------------------             00137900
       01 FILLER PIC X(48) VALUE                                        00138000
           '--------------VARIAVEIS PARA APOIO-----------'.             00138100
      *----------------------------------------------------             00138200
       77 WRK-SETOR-ATIVO      PIC X(04).                               00138300
      *====================================================             00138400
       PROCEDURE                                 DIVISION.              00138500
      *====================================================             00138600
      *-----------------------------------------------------            00138700
       0000-PRINCIPAL                             SECTION.              00138800
      *-----------------------------------------------------            00138900
           PERFORM 1000-INICIALIZAR.                                    00139000
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-FUNC                     00139100
                    EQUAL 10.                                           00139200
           PERFORM 3000-FINALIZAR.                                      00139300
           STOP RUN.                                                    00139400
      *-----------------------------------------------------            00139500
       0000-99-FIM.                                  EXIT.              00139600
      *-----------------------------------------------------            00139700
                                                                        00139800
      *-----------------------------------------------------            00139900
       1000-INICIALIZAR                           SECTION.              00140000
      *-----------------------------------------------------            00140100
           OPEN INPUT FUNC.                                             00140200
             PERFORM 4000-TESTAR-STATUS.                                00140300
           READ FUNC.                                                   00140400
           MOVE FD-SETORFUNC TO WRK-SETOR-ATIVO.                        00140500
            IF WRK-FS-FUNC EQUAL 10                                     00140600
             DISPLAY 'FINAL DE ARQUIVO'                                 00140700
            END-IF.                                                     00140800
           PERFORM 1100-IMPRIME-CABEC.                                  00140900
      *-----------------------------------------------------            00141000
       1000-99-FIM.                                  EXIT.              00141100
      *-----------------------------------------------------            00141200
                                                                        00141300
      *-----------------------------------------------------            00141400
       1100-IMPRIME-CABEC                         SECTION.              00141500
      *-----------------------------------------------------            00141600
            DISPLAY ' '.                                                00141700
            DISPLAY WRK-CABEC1.                                         00141800
            DISPLAY ' '.                                                00141900
            DISPLAY WRK-CABEC2.                                         00142000
             ADD 4 TO WRK-LINHAS.                                       00142100
      *-----------------------------------------------------            00142200
       1100-99-FIM.                                  EXIT.              00142300
      *-----------------------------------------------------            00142400
                                                                        00142500
      *-----------------------------------------------------            00142600
       2000-PROCESSAR                             SECTION.              00142700
      *-----------------------------------------------------            00142800
           ADD 1 TO WRK-ACUM-LIDOS.                                     00142900
           ADD FD-SALFUNC TO WRK-SALTOTAL.                              00143000
            IF FD-SETORFUNC NOT EQUAL WRK-SETOR-ATIVO                   00143100
             DISPLAY 'TOTAL DO SETOR:' WRK-ACUM-SAL                     00143200
             MOVE 0 TO WRK-ACUM-SAL                                     00143300
             MOVE FD-SETORFUNC TO WRK-SETOR-ATIVO                       00143400
             ADD FD-SALFUNC TO WRK-ACUM-SAL                             00143500
            ELSE                                                        00143600
             ADD FD-SALFUNC TO WRK-ACUM-SAL                             00143700
            END-IF.                                                     00143800
           MOVE FD-IDFUNC      TO WRK-ID-SAIDA.                         00143900
           MOVE FD-NOMEFUNC    TO WRK-NOME-SAIDA.                       00144000
           MOVE FD-SETORFUNC   TO WRK-SETOR-SAIDA.                      00144100
           MOVE FD-SALFUNC     TO WRK-SALARIO-SAIDA.                    00144200
            DISPLAY WRK-LINHAS '-' WRK-FUNC-SAIDA.                      00144300
             ADD 1 TO WRK-LINHAS.                                       00144400
                                                                        00144500
             IF WRK-LINHAS GREATER 10                                   00144600
              ADD 1 TO WRK-PAG                                          00144700
              MOVE 1 TO WRK-LINHAS                                      00144800
              PERFORM 1100-IMPRIME-CABEC                                00144900
             END-IF.                                                    00145000
                                                                        00145100
           READ FUNC.                                                   00145200
           IF WRK-FS-FUNC EQUAL 10                                      00145300
              DISPLAY 'TOTAL DO SETOR:' WRK-ACUM-SAL                    00145400
              DISPLAY 'FINAL DE ARQUIVO'                                00145500
           END-IF.                                                      00145600
      *-----------------------------------------------------            00145700
       2000-99-FIM.                                  EXIT.              00145800
      *-----------------------------------------------------            00145900
                                                                        00146000
      *-----------------------------------------------------            00146100
       3000-FINALIZAR                             SECTION.              00147000
      *-----------------------------------------------------            00148000
           CLOSE FUNC.                                                  00149000
           DISPLAY ' '.                                                 00150000
           DISPLAY 'TOTAL REGISTROS LIDOS:' WRK-ACUM-LIDOS.             00151000
           DISPLAY 'TOTAL SALARIOS:'        WRK-SALTOTAL.               00151100
      *-----------------------------------------------------            00151200
       3000-99-FIM.                                  EXIT.              00151300
      *-----------------------------------------------------            00151400
                                                                        00151500
      *-----------------------------------------------------            00151600
       4000-TESTAR-STATUS                         SECTION.              00151700
      *-----------------------------------------------------            00151800
           IF WRK-FS-FUNC NOT EQUAL 0                                   00151900
             MOVE 'REL2203'             TO WRK-PROGRAMA                 00152000
             MOVE '4000'                TO WRK-SECAO                    00153000
             MOVE 'ERRO NO OPEN PRODUT' TO WRK-MENSAGEM                 00154000
             MOVE WRK-FS-FUNC           TO WRK-STATUS                   00155000
             PERFORM 9000-ERRO                                          00156000
           END-IF.                                                      00157000
      *-----------------------------------------------------            00158000
       4000-99-FIM.                                  EXIT.              00159000
      *-----------------------------------------------------            00160000
                                                                        00160100
      *-----------------------------------------------------            00160200
       9000-ERRO                                  SECTION.              00160300
      *-----------------------------------------------------            00160400
           DISPLAY WRK-MENSAGEM.                                        00160500
            CALL 'GRAVALOG' USING WRK-DADOS.                            00160600
           GOBACK.                                                      00160700
      *-----------------------------------------------------            00160800
       9000-99-FIM.                                  EXIT.              00160900
      *-----------------------------------------------------            00161000

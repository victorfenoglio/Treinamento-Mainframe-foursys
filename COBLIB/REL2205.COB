      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. REL2205.                                             00040000
      *===================================================*             00050001
      * AUTOR   : VICTOR LEAL                             *             00060001
      * EMPRESA : FOURSYS                                 *             00070001
      * OBJETIVO: EXIBIR RELATORIO PRODUTOS COM LRECL=45, *             00080001
      *           INSERINDO CABECALHO E NUMERO DE PAGINA, *             00080101
      *           SOMANDO SALARIO POR SETOR, GRAVAR O     *             00080201
      *           RELATORIO EM OUTRO ARQUIVO PARA IMPRIMIR*             00080301
      *---------------------------------------------------*             00080401
      *  ARQUIVOS:                                        *             00080501
      *  DDNAME             I/O           INCLUDE/BOOK    *             00080601
      *  FUNC                I             ---------      *             00080701
      *  RELAT               O             ---------      *             00080801
      *===================================================*             00080901
                                                                        00081000
      *====================================================             00081100
       ENVIRONMENT                               DIVISION.              00081200
      *====================================================             00081301
       CONFIGURATION                             SECTION.               00081401
       SPECIAL-NAMES.                                                   00081501
           DECIMAL-POINT  IS COMMA.                                     00081600
                                                                        00081700
       INPUT-OUTPUT                              SECTION.               00081800
       FILE-CONTROL.                                                    00081900
           SELECT FUNC ASSIGN TO FUNC                                   00082000
               FILE STATUS IS WRK-FS-FUNC.                              00082100
                                                                        00082200
           SELECT RELAT ASSIGN TO RELAT                                 00082300
               FILE STATUS IS WRK-FS-RELAT.                             00082400
                                                                        00082500
      *====================================================             00082600
       DATA                                      DIVISION.              00082700
      *====================================================             00082800
      *-----------------------------------------------------            00082900
       FILE                                      SECTION.               00083000
      *-----------------------------------------------------            00084000
       FD FUNC                                                          00085000
           RECORDING MODE IS F                                          00086000
           BLOCK CONTAINS 0 RECORDS.                                    00088000
       01 FD-FUNC.                                                      00090000
          05 FD-IDFUNC     PIC 9(05).                                   00100000
          05 FD-NOMEFUNC   PIC X(30).                                   00110000
          05 FD-SETORFUNC  PIC 9(04).                                   00120000
          05 FD-SALFUNC    PIC 9(06).                                   00130000
                                                                        00130100
       FD RELAT                                                         00130200
           RECORDING MODE IS F                                          00130300
           BLOCK CONTAINS 0 RECORDS.                                    00130500
       01 FD-RELAT         PIC X(58).                                   00130600
                                                                        00130700
      *-----------------------------------------------------            00130800
       WORKING-STORAGE                           SECTION.               00130900
      *-----------------------------------------------------            00131000
           COPY '#GLOG'.                                                00131100
      *-----------------------------------------------------            00131200
       01 FILLER PIC X(48) VALUE                                        00131300
            '--------------VARIAVEIS PARA CABECALHO-------'.            00131400
      *-----------------------------------------------------            00131500
       01 WRK-CABEC.                                                    00131600
          05 WRK-CABEC1.                                                00131700
            10 FILLER       PIC X(12).                                  00131800
            10 FILLER       PIC X(37) VALUE                             00132000
            'RELATORIO DE FUNCIONARIOS       PAG: '.                    00133000
            10 WRK-PAG      PIC 9(02) VALUE 1.                          00134000
            10 FILLER       PIC X(07).                                  00134100
          05 WRK-CABEC2.                                                00134200
            10 FILLER       PIC X(09) VALUE 'ID       '.                00134400
            10 FILLER       PIC X(04) VALUE 'NOME'.                     00134500
            10 FILLER       PIC X(26).                                  00134600
            10 FILLER       PIC X(12) VALUE 'SETOR       '.             00134700
            10 FILLER       PIC X(07) VALUE 'SALARIO'.                  00134800
      *-----------------------------------------------------            00134900
       01 FILLER PIC X(48) VALUE                                        00135000
            '--------------VARIAVEIS PARA EXIBICAO--------'.            00135100
      *-----------------------------------------------------            00135200
       01 WRK-FUNC-SAIDA.                                               00135300
          05 WRK-ID-SAIDA      PIC X(05).                               00135400
          05 FILLER            PIC X(04).                               00135500
          05 WRK-NOME-SAIDA    PIC X(30).                               00135600
          05 WRK-SETOR-SAIDA   PIC X(04).                               00135700
          05 FILLER            PIC X(08).                               00135800
          05 WRK-SALARIO-SAIDA PIC X(06).                               00135900
          05 FILLER            PIC X(01).                               00136000
      *-----------------------------------------------------            00136100
       01 FILLER PIC X(48) VALUE                                        00136200
            '-------VARIAVEIS PARA ACUMUCACAO/EXIBICAO---'.             00136300
      *-----------------------------------------------------            00136400
       01 WRK-SAL-PRINCIPAL.                                            00136500
          05 FILLER            PIC X(45) VALUE                          00136600
           '                                       TOTAL:'.             00136700
          05 WRK-ACUM-SAL      PIC 9(06).                               00136800
       77 WRK-SALTOTAL         PIC 9(06).                               00136900
       77 WRK-ACUM-LIDOS       PIC 9(02).                               00137000
       77 WRK-LINHAS           PIC 9(02) VALUE 1.                       00137100
       77 WRK-SETOR-ATIVO      PIC X(04).                               00137200
      *-----------------------------------------------------            00137300
       01 FILLER PIC X(48) VALUE                                        00137400
            '-------VARIAVEIS PARA FILE STATUS------------'.            00137500
      *-----------------------------------------------------            00137600
       77 WRK-FS-FUNC          PIC 9(02).                               00137700
       77 WRK-FS-RELAT         PIC 9(02).                               00137800
      *-----------------------------------------------------            00137900
       01 FILLER PIC X(48) VALUE                                        00138000
            '-------VARIAVEIS PARA MENSAGEM---------------'.            00138100
      *-----------------------------------------------------            00138200
       77 WRK-MENSAGEM-FIM     PIC X(16) VALUE                          00138300
           'FIM DO ARQUIVO.'.                                           00138400
       77 WRK-LINHABRANCO      PIC X(58) VALUE SPACES.                  00138500
      *====================================================             00138600
       PROCEDURE                                 DIVISION.              00138700
      *====================================================             00138800
      *-----------------------------------------------------            00138900
       0000-PRINCIPAL                             SECTION.              00139000
      *-----------------------------------------------------            00139100
           PERFORM 1000-INICIALIZAR.                                    00139200
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-FUNC                     00139300
                    EQUAL 10.                                           00139400
           PERFORM 3000-FINALIZAR.                                      00139500
           STOP RUN.                                                    00139600
      *-----------------------------------------------------            00139700
       0000-99-FIM.                                  EXIT.              00139800
      *-----------------------------------------------------            00139900
                                                                        00140000
      *-----------------------------------------------------            00140100
       1000-INICIALIZAR                           SECTION.              00140200
      *-----------------------------------------------------            00140300
           OPEN INPUT FUNC                                              00140400
                OUTPUT RELAT.                                           00140500
             PERFORM 4000-TESTAR-STATUS.                                00140600
             PERFORM 1050-LER.                                          00140700
      *-----------------------------------------------------            00141300
       1000-99-FIM.                                  EXIT.              00141400
      *-----------------------------------------------------            00141500
                                                                        00141600
      *-----------------------------------------------------            00141700
       1050-LER                                   SECTION.              00141800
      *-----------------------------------------------------            00141900
           READ FUNC.                                                   00142000
            MOVE FD-SETORFUNC TO WRK-SETOR-ATIVO.                       00142100
           IF WRK-FS-FUNC EQUAL 10                                      00142200
            DISPLAY WRK-MENSAGEM-FIM                                    00142300
           END-IF.                                                      00142400
           PERFORM 1100-IMPRIME-CABEC.                                  00142500
      *-----------------------------------------------------            00142600
       1050-99-FIM.                                  EXIT.              00142700
      *-----------------------------------------------------            00142800
                                                                        00142900
      *-----------------------------------------------------            00143000
       1100-IMPRIME-CABEC                         SECTION.              00143100
      *-----------------------------------------------------            00143200
            WRITE FD-RELAT FROM WRK-LINHABRANCO.                        00143300
             IF WRK-PAG EQUAL 1                                         00143400
              WRITE FD-RELAT FROM WRK-CABEC1                            00143500
             ELSE                                                       00143600
              WRITE FD-RELAT FROM WRK-CABEC1 AFTER PAGE                 00143700
             END-IF.                                                    00143800
            WRITE FD-RELAT FROM WRK-LINHABRANCO.                        00143900
            WRITE FD-RELAT FROM WRK-CABEC2 AFTER 2 LINE .               00144000
             ADD 4 TO WRK-LINHAS.                                       00144100
      *-----------------------------------------------------            00144200
       1100-99-FIM.                                  EXIT.              00144300
      *-----------------------------------------------------            00144400
                                                                        00144500
      *-----------------------------------------------------            00144600
       2000-PROCESSAR                             SECTION.              00144700
      *-----------------------------------------------------            00144800
           ADD 1 TO WRK-ACUM-LIDOS.                                     00144900
           ADD FD-SALFUNC TO WRK-SALTOTAL.                              00145000
            IF FD-SETORFUNC NOT EQUAL WRK-SETOR-ATIVO                   00145100
             WRITE FD-RELAT FROM WRK-SAL-PRINCIPAL                      00145200
             MOVE 0 TO WRK-ACUM-SAL                                     00145300
             MOVE FD-SETORFUNC TO WRK-SETOR-ATIVO                       00145400
             ADD FD-SALFUNC TO WRK-ACUM-SAL                             00145500
            ELSE                                                        00145600
             ADD FD-SALFUNC TO WRK-ACUM-SAL                             00145700
            END-IF.                                                     00145800
           MOVE FD-IDFUNC      TO WRK-ID-SAIDA.                         00145900
           MOVE FD-NOMEFUNC    TO WRK-NOME-SAIDA.                       00146000
           MOVE FD-SETORFUNC   TO WRK-SETOR-SAIDA.                      00146100
           MOVE FD-SALFUNC     TO WRK-SALARIO-SAIDA.                    00146200
             WRITE FD-RELAT FROM WRK-FUNC-SAIDA.                        00146300
             ADD 1 TO WRK-LINHAS.                                       00146400
                                                                        00146500
             IF WRK-LINHAS GREATER 10                                   00146600
              ADD 1 TO WRK-PAG                                          00146700
              MOVE 1 TO WRK-LINHAS                                      00146800
              PERFORM 1100-IMPRIME-CABEC                                00146900
             END-IF.                                                    00147000
                                                                        00147100
           READ FUNC.                                                   00147200
           IF WRK-FS-FUNC EQUAL 10                                      00147300
              WRITE FD-RELAT FROM WRK-SAL-PRINCIPAL                     00147400
              DISPLAY WRK-MENSAGEM-FIM                                  00147500
           END-IF.                                                      00147600
      *-----------------------------------------------------            00147700
       2000-99-FIM.                                  EXIT.              00147800
      *-----------------------------------------------------            00147900
                                                                        00148000
      *-----------------------------------------------------            00148100
       3000-FINALIZAR                             SECTION.              00148200
      *-----------------------------------------------------            00148300
           CLOSE FUNC RELAT.                                            00149000
           DISPLAY WRK-LINHABRANCO.                                     00150000
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

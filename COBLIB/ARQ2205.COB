      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. ARQ2205.                                             00040000
      *===================================================*             00050001
      *  AUTOR   : VICTOR LEAL                            *             00060001
      *  EMPRESA : FOURSYS                                *             00070001
      *  OBJETIVO: ABRIR,LER GRAVAR ARQUIVOS COM LRECL=32 *             00080001
      *---------------------------------------------------*             00081001
      *  ARQUIVOS:                                        *             00081101
      *  DDNAME             I/O           INCLUDE/BOOK    *             00081201
      *  BOOKLIB             I               #GLOG        *             00081301
      *  ALUNOS              I             ---------      *             00081401
      *  RELREC              O             ---------      *             00081501
      *===================================================*             00082001
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
           SELECT ALUNOS ASSIGN TO ALUNOS                               00120000
               FILE STATUS IS WRK-FS-ALUNOS.                            00130000
                                                                        00130100
           SELECT RELREC ASSIGN TO RELREC                               00130200
               FILE STATUS IS WRK-FS-RELREC.                            00130300
                                                                        00131000
      *====================================================             00132000
       DATA                                      DIVISION.              00133000
      *====================================================             00133100
      *-----------------------------------------------------            00133200
       FILE                                      SECTION.               00133300
      *-----------------------------------------------------            00133400
       FD ALUNOS                                                        00133500
           RECORDING MODE IS F                                          00133600
           LABEL RECORD IS STANDARD                                     00133700
           BLOCK CONTAINS 0 RECORDS.                                    00133800
                                                                        00133900
       01 FD-ALUNOS.                                                    00134000
          05 FD-RM           PIC X(05).                                 00134100
          05 FD-NOME         PIC X(20).                                 00135000
          05 FD-SERIE        PIC X(03).                                 00136000
          05 FD-MEDIA        PIC X(04).                                 00137000
                                                                        00137100
       FD RELREC                                                        00137200
           RECORDING MODE IS F.                                         00137300
                                                                        00137400
       01 FD-RELREC          PIC X(32).                                 00137500
                                                                        00137600
      *-----------------------------------------------------            00137700
       WORKING-STORAGE                           SECTION.               00137800
      *-----------------------------------------------------            00137900
           COPY '#GLOG'.                                                00138000
      *-----------------------------------------------------            00138101
       01 FILLER PIC X(48) VALUE                                        00138201
           '--------------VARIAVEIS PARA CALCULAR--------'.             00138301
      *-----------------------------------------------------            00138601
       77 WRK-MEDIA          PIC X(04).                                 00139100
      *-----------------------------------------------------            00139201
       01 FILLER PIC X(48) VALUE                                        00139301
           '--------------VARIAVEIS PARA FILE STATUS-----'.             00139401
      *-----------------------------------------------------            00139501
       77 WRK-FS-ALUNOS      PIC 9(02).                                 00139600
       77 WRK-FS-RELREC      PIC 9(02).                                 00139700
      *====================================================             00139800
       PROCEDURE                                 DIVISION.              00139900
      *====================================================             00140000
      *-----------------------------------------------------            00140100
       0000-PRINCIPAL                             SECTION.              00140200
      *-----------------------------------------------------            00140300
           PERFORM 1000-INICIALIZAR.                                    00140400
           PERFORM 2000-PROCESSAR UNTIL WRK-FS-ALUNOS                   00140500
                    EQUAL 10.                                           00140600
           PERFORM 3000-FINALIZAR.                                      00140700
           STOP RUN.                                                    00140800
      *-----------------------------------------------------            00140900
       0000-99-FIM.                                  EXIT.              00141000
      *-----------------------------------------------------            00141100
                                                                        00141200
      *-----------------------------------------------------            00141300
       1000-INICIALIZAR                           SECTION.              00142000
      *-----------------------------------------------------            00150000
           OPEN INPUT ALUNOS                                            00151000
                OUTPUT RELREC.                                          00151100
             PERFORM 4000-TESTAR-STATUS.                                00153000
           READ ALUNOS.                                                 00154000
           IF WRK-FS-ALUNOS EQUAL 10                                    00155000
            DISPLAY 'FINAL DE ARQUIVO'                                  00156000
           END-IF.                                                      00157200
      *-----------------------------------------------------            00157900
       1000-99-FIM.                                  EXIT.              00158000
      *-----------------------------------------------------            00158100
                                                                        00158200
      *-----------------------------------------------------            00158300
       2000-PROCESSAR                             SECTION.              00158400
      *-----------------------------------------------------            00158500
           DISPLAY FD-ALUNOS                                            00158600
            MOVE FD-MEDIA TO WRK-MEDIA.                                 00158700
            DISPLAY WRK-MEDIA.                                          00158800
           IF WRK-MEDIA (01:02) LESS 07                                 00158900
            WRITE FD-RELREC FROM FD-ALUNOS                              00159000
           END-IF.                                                      00159100
           READ ALUNOS.                                                 00160000
      *-----------------------------------------------------            00160200
       2000-99-FIM.                                  EXIT.              00160300
      *-----------------------------------------------------            00160400
                                                                        00160500
      *-----------------------------------------------------            00160600
       3000-FINALIZAR                             SECTION.              00160700
      *-----------------------------------------------------            00160800
           CLOSE ALUNOS RELREC.                                         00160900
      *-----------------------------------------------------            00162400
       3000-99-FIM.                                  EXIT.              00162500
      *-----------------------------------------------------            00162600
                                                                        00162700
      *-----------------------------------------------------            00162800
       4000-TESTAR-STATUS                         SECTION.              00162900
      *-----------------------------------------------------            00163000
           PERFORM 4100-TESTASTATUS-ALUNOS.                             00163100
           PERFORM 4200-TESTASTATUS-RELREC.                             00163200
      *-----------------------------------------------------            00163300
       4000-99-FIM.                                  EXIT.              00163400
      *-----------------------------------------------------            00163500
                                                                        00164000
      *-----------------------------------------------------            00164100
       4100-TESTASTATUS-ALUNOS                    SECTION.              00164200
      *-----------------------------------------------------            00164300
           IF WRK-FS-ALUNOS NOT EQUAL 0                                 00164400
             MOVE 'ARQ2205'             TO WRK-PROGRAMA                 00164500
             MOVE '4100'                TO WRK-SECAO                    00164600
             MOVE 'ERRO NO OPEN ALUNOS' TO WRK-MENSAGEM                 00164800
             MOVE WRK-FS-ALUNOS         TO WRK-STATUS                   00164900
             PERFORM 9000-ERRO                                          00165000
           END-IF.                                                      00165100
      *-----------------------------------------------------            00165200
       4100-99-FIM.                                  EXIT.              00165300
      *-----------------------------------------------------            00165400
                                                                        00165500
      *-----------------------------------------------------            00165600
       4200-TESTASTATUS-RELREC                    SECTION.              00165700
      *-----------------------------------------------------            00165800
           IF WRK-FS-RELREC NOT EQUAL 0                                 00165900
             MOVE 'ARQ2205'             TO WRK-PROGRAMA                 00166000
             MOVE '4200'                TO WRK-SECAO                    00166100
             MOVE 'ERRO NO OPEN RELREC' TO WRK-MENSAGEM                 00166200
             MOVE WRK-FS-RELREC         TO WRK-STATUS                   00166300
              PERFORM 9000-ERRO                                         00166402
           END-IF.                                                      00166500
      *-----------------------------------------------------            00166600
       4200-99-FIM.                                  EXIT.              00166700
      *-----------------------------------------------------            00166800
                                                                        00166900
      *-----------------------------------------------------            00167000
       9000-ERRO                                  SECTION.              00167100
      *-----------------------------------------------------            00167200
           DISPLAY WRK-MENSAGEM.                                        00167300
            CALL 'GRAVALOG' USING WRK-DADOS.                            00167400
           GOBACK.                                                      00167500
      *-----------------------------------------------------            00167600
       9000-99-FIM.                                  EXIT.              00167700
      *-----------------------------------------------------            00168000

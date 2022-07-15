      *====================================================             00010008
       IDENTIFICATION                            DIVISION.              00011008
      *====================================================             00020008
       PROGRAM-ID. F22CIP1.                                             00021009
      *===================================================*             00022010
      *  AUTOR: VICTOR LEAL                               *             00023010
      *  EMPRESA: FOURSYS                                 *             00024010
      *  OBJETIVO: PROGRAMA DE LEITURA MAPA BMS CICS COM  *             00025026
      *            ACESSO AOS REGISTROS DB2, SEM LOGICA   *             00025145
      *            DE SAIDA.                              *             00025245
      *---------------------------------------------------*             00026010
      *  ARQUIVOS:                                        *             00027010
      *  DDNAME             I/O           INCLUDE/BOOK    *             00028010
      *  F22CIM1             I             ---------      *             00029044
      *---------------------------------------------------*             00029121
      *  BASE  DE DADOS:                                  *             00029221
      *  TABELA             I/O           INCLUDE/BOOK    *             00029321
      *  IVAN.FUNC           I               #BKFUNC      *             00029421
      *===================================================*             00029510
       DATA                                      DIVISION.              00030007
      *====================================================             00031008
      *-----------------------------------------------------            00032021
       WORKING-STORAGE                            SECTION.              00040007
      *-----------------------------------------------------            00041021
            COPY F22CIM1.                                               00050021
             COPY DFHAID.                                               00052021
                                                                        00052121
            EXEC SQL                                                    00053021
               INCLUDE #BKFUNC                                          00054021
            END-EXEC.                                                   00055021
                                                                        00056021
            EXEC SQL                                                    00057021
               INCLUDE SQLCA                                            00058021
            END-EXEC.                                                   00059021
                                                                        00059122
       01 WRK-DATAADM.                                                  00059222
          05 WRK-DATAANO  PIC X(04).                                    00059322
          05 FILLER       PIC X(01) VALUE '-'.                          00059432
          05 WRK-DATAMES  PIC X(02).                                    00059522
          05 FILLER       PIC X(01) VALUE '-'.                          00059632
          05 WRK-DATADIA  PIC X(02).                                    00059722
                                                                        00059821
      *----------------------------------------------------             00059921
       01 FILLER PIC X(48) VALUE                                        00060021
           '--------------VARIAVEIS PARA APOIO-----------'.             00060144
      *----------------------------------------------------             00060221
       77 WRK-SQLCODE     PIC -999.                                     00060321
       77 WRK-NULL-EMAIL  PIC S9(04) COMP.                              00060421
       77 WRK-FLAG        PIC 9(02).                                    00060538
      *====================================================             00061008
       PROCEDURE                                 DIVISION.              00070007
      *====================================================             00071010
      *-----------------------------------------------------            00072021
       0000-PRINCIPAL                             SECTION.              00073021
      *-----------------------------------------------------            00074021
            PERFORM 1000-INICIALIZAR.                                   00075021
            PERFORM 2000-PROCESSAR.                                     00076021
            PERFORM 3000-FINALIZAR.                                     00077042
      *-----------------------------------------------------            00079121
       0000-99-FIM.                                  EXIT.              00079221
      *-----------------------------------------------------            00079321
                                                                        00079421
      *-----------------------------------------------------            00079521
       1000-INICIALIZAR                           SECTION.              00079621
      *-----------------------------------------------------            00079721
             EXEC CICS SEND                                             00080012
               MAPSET('F22CIM1')                                        00090001
               MAP('MAPA01')                                            00100001
                ERASE                                                   00110001
               MAPONLY                                                  00120001
             END-EXEC.                                                  00130012
      *-----------------------------------------------------            00140021
       1000-99-FIM.                                  EXIT.              00141021
      *-----------------------------------------------------            00142021
                                                                        00143021
      *-----------------------------------------------------            00144021
       2000-PROCESSAR                             SECTION.              00145021
      *-----------------------------------------------------            00146021
             EXEC CICS RECEIVE                                          00150012
               MAPSET('F22CIM1')                                        00160001
               MAP('MAPA01')                                            00170001
               INTO(MAPA01I)                                            00180001
             END-EXEC.                                                  00190012
                                                                        00191033
            EVALUATE TRUE                                               00192041
             WHEN EIBAID = '"'                                          00193041
              PERFORM 2050-LIMPAR                                       00194041
             WHEN EIBAID = ':'                                          00195043
              PERFORM 2100-SAIR                                         00196041
             WHEN EIBAID = '5'                                          00197041
              PERFORM 2150-CONSULTAR                                    00198041
             WHEN EIBAID = '6'                                          00199041
              PERFORM 2200-INCLUIR                                      00199141
             WHEN EIBAID = '7'                                          00199241
              PERFORM 2250-EXCLUIR                                      00199341
             END-EVALUATE.                                              00199441
                                                                        00199541
             EXEC CICS SEND                                             00199741
               MAPSET('F22CIM1')                                        00199841
               MAP('MAPA01')                                            00199941
               DATAONLY                                                 00200041
               FROM(MAPA01O)                                            00200141
             END-EXEC.                                                  00200241
      *-----------------------------------------------------            00200341
       2000-99-FIM.                                  EXIT.              00200441
      *-----------------------------------------------------            00200541
                                                                        00200641
      *-----------------------------------------------------            00200741
       2050-LIMPAR                                SECTION.              00200841
      *-----------------------------------------------------            00200941
             EXEC CICS                                                  00201012
              RETURN TRANSID ('T221')                                   00201122
             END-EXEC.                                                  00201242
      *-----------------------------------------------------            00201341
       2050-99-FIM.                                  EXIT.              00201441
      *-----------------------------------------------------            00201541
                                                                        00201633
      *-----------------------------------------------------            00201741
       2100-SAIR                                  SECTION.              00201841
      *-----------------------------------------------------            00201941
             EXEC CICS                                                  00202022
              RETURN                                                    00202123
             END-EXEC.                                                  00202242
      *-----------------------------------------------------            00202341
       2100-99-FIM.                                  EXIT.              00202441
      *-----------------------------------------------------            00202541
                                                                        00202641
      *-----------------------------------------------------            00202741
       2150-CONSULTAR                             SECTION.              00202841
      *-----------------------------------------------------            00202941
             MOVE IDI TO DB2-ID.                                        00210042
              EXEC SQL                                                  00211021
               SELECT ID,NOME,SETOR,SALARIO,DATAADM,EMAIL               00212021
               INTO :DB2-ID,                                            00213021
                    :DB2-NOME,                                          00214021
                    :DB2-SETOR,                                         00215021
                    :DB2-SALARIO,                                       00216021
                    :DB2-DATAADM,                                       00217021
                    :DB2-EMAIL      :WRK-NULL-EMAIL                     00218021
               FROM IVAN.FUNC                                           00219021
                WHERE ID = :DB2-ID                                      00219121
                FETCH FIRST ROW ONLY                                    00219221
              END-EXEC.                                                 00219321
                                                                        00219421
             EVALUATE SQLCODE                                           00219521
              WHEN 0                                                    00219621
               MOVE DB2-NOME                  TO NOMEO                  00220021
               MOVE DB2-SETOR                 TO SETORO                 00230021
               MOVE DB2-SALARIO               TO SALARIOO               00240021
               MOVE DB2-DATAADM               TO WRK-DATAADM            00241022
               MOVE WRK-DATADIA               TO DATADIAO               00250024
               MOVE WRK-DATAMES               TO DATAMESO               00251024
               MOVE WRK-DATAANO               TO DATAANOO               00252024
              IF WRK-NULL-EMAIL EQUAL 0                                 00253021
               MOVE DB2-EMAIL                 TO EMAILO                 00260021
              END-IF                                                    00270021
               MOVE 'REGISTRO ENCONTRADO'     TO MSGO                   00271025
             WHEN 100                                                   00280021
              MOVE 'REGISTRO NAO ENCONTRADO'  TO MSGO                   00290021
             WHEN OTHER                                                 00291021
              MOVE SQLCODE                    TO WRK-SQLCODE            00291121
              MOVE 'ERRO: '                   TO MSGO                   00291221
              MOVE WRK-SQLCODE                TO MSGO(07:04)            00291321
            END-EVALUATE.                                               00292021
      *-----------------------------------------------------            00292141
       2150-99-FIM.                                  EXIT.              00292241
      *-----------------------------------------------------            00292341
                                                                        00292441
      *-----------------------------------------------------            00292541
       2200-INCLUIR                               SECTION.              00292641
      *-----------------------------------------------------            00292741
             MOVE 0 TO WRK-FLAG.                                        00302842
                                                                        00302933
             IF IDL GREATER 0                                           00303038
              MOVE IDI         TO DB2-ID                                00303143
             ELSE                                                       00303238
              MOVE 1 TO WRK-FLAG                                        00303338
             END-IF.                                                    00303442
                                                                        00303538
             IF NOMEL GREATER 0                                         00303638
              MOVE NOMEI       TO DB2-NOME                              00303837
             ELSE                                                       00303938
              MOVE 1 TO WRK-FLAG                                        00304038
             END-IF.                                                    00304142
                                                                        00304238
             IF SETORL GREATER 0                                        00304338
              MOVE SETORI      TO DB2-SETOR                             00304433
             ELSE                                                       00304538
              MOVE 1 TO WRK-FLAG                                        00304638
             END-IF.                                                    00304742
                                                                        00304838
             IF SALARIOL GREATER 0                                      00304938
              MOVE SALARIOI    TO DB2-SALARIO                           00305033
             ELSE                                                       00305138
              MOVE 1 TO WRK-FLAG                                        00305238
             END-IF.                                                    00305342
                                                                        00305438
             IF DATADIAL GREATER 0 AND DATAMESL GREATER 0 AND           00305538
                DATAANOL GREATER 0                                      00305638
              MOVE DATADIAI    TO WRK-DATADIA                           00305833
              MOVE DATAMESI    TO WRK-DATAMES                           00305938
              MOVE DATAANOI    TO WRK-DATAANO                           00306038
              MOVE WRK-DATAADM TO DB2-DATAADM                           00306138
             ELSE                                                       00306238
              MOVE 1 TO WRK-FLAG                                        00306338
             END-IF.                                                    00306442
                                                                        00307838
             IF EMAILL GREATER 0                                        00307938
              MOVE EMAILI      TO DB2-EMAIL                             00308033
             ELSE                                                       00308138
              MOVE 1 TO WRK-FLAG                                        00308238
             END-IF.                                                    00308342
                                                                        00308438
             IF WRK-FLAG EQUAL 1                                        00308538
              MOVE 'PREENCHA TODOS OS CAMPOS' TO MSGO                   00308638
             ELSE                                                       00308738
              EXEC SQL                                                  00309033
               INSERT INTO IVAN.FUNC(ID,NOME,SETOR,SALARIO,DATAADM,     00309127
                                     EMAIL)                             00309227
               VALUES (:DB2-ID,                                         00309327
                       :DB2-NOME,                                       00309427
                       :DB2-SETOR,                                      00309527
                       :DB2-SALARIO,                                    00309627
                       :DB2-DATAADM,                                    00309728
                       :DB2-EMAIL)                                      00309827
              END-EXEC                                                  00310042
                                                                        00310227
              EVALUATE SQLCODE                                          00310333
               WHEN 0                                                   00310427
                  EXEC SQL                                              00310527
                    COMMIT                                              00310627
                  END-EXEC                                              00310727
                  MOVE 'REGISTRO INSERIDO' TO MSGO                      00310827
               WHEN OTHER                                               00310927
                  MOVE SQLCODE TO WRK-SQLCODE                           00311027
                  MOVE WRK-SQLCODE TO MSGO                              00311127
              END-EVALUATE                                              00311233
             END-IF.                                                    00311342
      *-----------------------------------------------------            00311541
       2200-99-FIM.                                  EXIT.              00311641
      *-----------------------------------------------------            00311741
                                                                        00311841
      *-----------------------------------------------------            00311941
       2250-EXCLUIR                               SECTION.              00312041
      *-----------------------------------------------------            00312141
            MOVE IDI TO DB2-ID.                                         00312342
            EXEC SQL                                                    00312439
             DELETE FROM IVAN.FUNC WHERE ID = :DB2-ID                   00312539
            END-EXEC.                                                   00312642
            EVALUATE SQLCODE                                            00312739
             WHEN 0                                                     00312839
              MOVE 'REGISTRO APAGADO'        TO MSGO                    00312939
             WHEN 100                                                   00313039
              MOVE 'REGISTRO NAO ENCONTRADO' TO MSGO                    00313139
             WHEN OTHER                                                 00313239
              MOVE SQLCODE                   TO WRK-SQLCODE             00313339
              MOVE WRK-SQLCODE               TO MSGO                    00313439
            END-EVALUATE.                                               00313542
      *-----------------------------------------------------            00420021
       2250-99-FIM.                                  EXIT.              00430041
      *-----------------------------------------------------            00440021
                                                                        00450041
      *-----------------------------------------------------            00460041
       3000-FINALIZAR                             SECTION.              00470041
      *-----------------------------------------------------            00480041
            EXEC CICS                                                   00481041
             RETURN TRANSID ('T221')                                    00482041
            END-EXEC.                                                   00490041
      *-----------------------------------------------------            00500041
       3000-99-FIM.                                  EXIT.              00510041
      *-----------------------------------------------------            00520041

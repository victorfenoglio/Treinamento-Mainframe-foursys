      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22CB04.                                            00040000
      *===================================================*             00060000
      * AUTOR   : VICTOR LEAL                             *             00070000
      * EMPRESA : FOURSYS                                 *             00080000
      * OBJETIVO: UTILIZAR FILLER.                        *             00081000
      *===================================================*             00090000
                                                                        00090100
      *====================================================             00090200
       ENVIRONMENT                               DIVISION.              00090300
      *====================================================             00090400
                                                                        00090500
      *====================================================             00090600
       DATA                                      DIVISION.              00090700
      *====================================================             00090800
      *----------------------------------------------------             00090900
       WORKING-STORAGE                           SECTION.               00091000
      *----------------------------------------------------             00091100
       01 WRK-DADOS.                                                    00091200
          05 WRK-ID      PIC 9(05) VALUE ZEROS.                         00091300
          05 WRK-PRODUTO PIC X(30) VALUE SPACES.                        00091400
          05 FILLER      PIC X(10) VALUE SPACES.                        00091500
          05 WRK-PRECO   PIC 9(06) VALUE ZEROS.                         00091600
      *====================================================             00091700
       PROCEDURE                                 DIVISION.              00091800
      *====================================================             00091900
      *----------------------------------------------------             00092000
       0000-PRINCIPAL                             SECTION.              00092100
      *----------------------------------------------------             00092200
           PERFORM 1000-INICIALIZAR.                                    00092300
           PERFORM 2000-PROCESSAR.                                      00092400
           PERFORM 3000-FINALIZAR.                                      00092500
           STOP RUN.                                                    00092600
      *----------------------------------------------------             00092700
       0000-99-FIM.                                  EXIT.              00092800
      *----------------------------------------------------             00092900
                                                                        00093000
      *----------------------------------------------------             00093100
       1000-INICIALIZAR                           SECTION.              00093200
      *----------------------------------------------------             00093300
           ACCEPT WRK-DADOS FROM SYSIN.                                 00093400
      *----------------------------------------------------             00093500
       1000-99-FIM.                                  EXIT.              00093600
      *----------------------------------------------------             00093700
                                                                        00093800
      *----------------------------------------------------             00093900
       2000-PROCESSAR                             SECTION.              00094000
      *----------------------------------------------------             00094100
           DISPLAY 'ID:     '   WRK-ID.                                 00094200
           DISPLAY 'PRODUTO:'   WRK-PRODUTO.                            00094300
           DISPLAY 'PRECO:  '   WRK-PRECO.                              00095000
      *----------------------------------------------------             00096000
       2000-99-FIM.                                  EXIT.              00096100
      *----------------------------------------------------             00096200
                                                                        00096300
      *----------------------------------------------------             00096400
       3000-FINALIZAR                             SECTION.              00096500
      *----------------------------------------------------             00096600
           DISPLAY '----------FIM DO PROGRAMA-----------'.              00097000
      *----------------------------------------------------             00098000
       3000-99-FIM.                                  EXIT.              00099000
      *----------------------------------------------------             00100000

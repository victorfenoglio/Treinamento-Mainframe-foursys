      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22CB24.                                            00040000
                                                                        00050000
      *===================================================*             00060000
      * AUTOR   : VICTOR LEAL                             *             00070000
      * EMPRESA : FOURSYS                                 *             00080000
      * OBJETIVO: COMANDO DE REPETICAO PERFORM UNTIL.     *             00081000
      *===================================================*             00082000
                                                                        00083000
      *====================================================             00084000
       ENVIRONMENT                               DIVISION.              00085000
      *====================================================             00086000
       CONFIGURATION                              SECTION.              00087000
       SPECIAL-NAMES.                                                   00088000
           DECIMAL-POINT IS COMMA.                                      00089000
                                                                        00090000
      *====================================================             00090100
       DATA                                      DIVISION.              00090200
      *====================================================             00090300
      *----------------------------------------------------             00090400
       WORKING-STORAGE                            SECTION.              00090500
      *----------------------------------------------------             00090600
       01 WRK-DADOS.                                                    00090700
          05 WRK-CODIGO      PIC 9(03)    VALUE ZEROS.                  00090800
          05 WRK-ITEM        PIC X(30)    VALUE SPACES.                 00090900
          05 WRK-VALOR       PIC 9(05)V99 VALUE ZEROS.                  00091000
          05 WRK-QUANT       PIC 9(03)    VALUE ZEROS.                  00091100
      *----------------------------------------------------             00091200
       01 FILLER PIC X(48) VALUE                                        00091300
           '--------------VARIAVEIS ACUMULAR/CALCULAR----'.             00091400
      *----------------------------------------------------             00091500
       77 WRK-COUNT          PIC 9(02)    VALUE 0.                      00091600
       77 WRK-TOTAL          PIC 9(06)V99 VALUE ZEROS.                  00091700
       77 WRK-TOTALACUM      PIC 9(06)V99 VALUE ZEROS.                  00091800
      *----------------------------------------------------             00091900
       01 FILLER PIC X(48) VALUE                                        00092000
           '--------------VARIAVEIS PARA MASCARA---------'.             00092100
      *----------------------------------------------------             00092200
       77 WRK-VALOR-ED       PIC ZZZ.ZZ9,99.                            00092300
       77 WRK-TOTALACUM-ED   PIC ZZZ.ZZ9,99.                            00092400
      *====================================================             00092500
       PROCEDURE                                 DIVISION.              00092600
      *====================================================             00092700
      *----------------------------------------------------             00092800
       0000-PRINCIPAL                             SECTION.              00092900
      *----------------------------------------------------             00093000
           PERFORM 1000-INICIALIZAR.                                    00093100
           PERFORM 2000-PROCESSAR.                                      00093200
           PERFORM 3000-FINALIZAR.                                      00093300
           STOP RUN.                                                    00093400
      *----------------------------------------------------             00093500
       0000-99-FIM.                                  EXIT.              00093600
      *----------------------------------------------------             00093700
                                                                        00093800
      *----------------------------------------------------             00093900
       1000-INICIALIZAR                           SECTION.              00094000
      *----------------------------------------------------             00094100
           ACCEPT WRK-DADOS FROM SYSIN.                                 00094200
      *----------------------------------------------------             00094300
       1000-99-FIM.                                  EXIT.              00094400
      *----------------------------------------------------             00094500
                                                                        00094600
      *----------------------------------------------------             00094700
       2000-PROCESSAR                             SECTION.              00094800
      *----------------------------------------------------             00094900
           PERFORM UNTIL WRK-CODIGO EQUAL 999                           00095000
             DISPLAY 'CODIGO    : '     WRK-CODIGO                      00095100
             DISPLAY 'ITEM      : '     WRK-ITEM                        00095200
             MOVE WRK-VALOR TO WRK-VALOR-ED                             00095300
             DISPLAY 'VALOR     : '     WRK-VALOR-ED                    00095400
             ADD WRK-VALOR TO WRK-TOTALACUM                             00095500
             DISPLAY 'QUANTIDADE: '     WRK-QUANT                       00095600
             DISPLAY '------------------------------------'             00095700
             ACCEPT WRK-DADOS FROM SYSIN                                00095800
             ADD 1 TO WRK-COUNT                                         00095900
            END-PERFORM.                                                00096000
             MOVE WRK-TOTALACUM TO WRK-TOTALACUM-ED.                    00096100
             DISPLAY 'TOTAL ACUMULADO: ' WRK-TOTALACUM-ED.              00096200
             DISPLAY 'REGISTROS LIDOS: ' WRK-COUNT.                     00096300
      *----------------------------------------------------             00096400
       2000-99-FIM.                                  EXIT.              00096500
      *----------------------------------------------------             00096600
                                                                        00096700
      *----------------------------------------------------             00096800
       3000-FINALIZAR                             SECTION.              00096900
      *----------------------------------------------------             00097000
           DISPLAY '----------FIM DO PROGRAMA-----------'.              00097100
      *----------------------------------------------------             00097200
       3000-99-FIM.                                  EXIT.              00097300
      *----------------------------------------------------             00097400
                                                                        00097500
      *----------------------------------------------------             00097600
       9000-TRATA-ERROS                           SECTION.              00097700
      *----------------------------------------------------             00097800
      *----------------------------------------------------             00097900
       9000-99-FIM.                                  EXIT.              00098000
      *----------------------------------------------------             00099000

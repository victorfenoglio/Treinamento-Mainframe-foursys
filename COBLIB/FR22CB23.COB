      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22CB23.                                            00040000
                                                                        00050000
      *====================================================             00060000
      * AUTOR: VICTOR LEAL                                              00070000
      * EMPRESA: FOURSYS                                                00080000
      * OBJETIVO: COMANDO DE REPETICAO PERFORM UNTIL                    00081013
      *====================================================             00082000
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
       WORKING-STORAGE                            SECTION.              00090400
       01 WRK-DADOS.                                                    00090500
          05 WRK-NUMERO      PIC 9(02)    VALUE ZEROS.                  00090600
       77 WRK-COUNT          PIC 9(02)    VALUE 1.                      00090700
       77 WRK-TOTAL          PIC 9(02)    VALUE ZEROS.                  00090800
       77 WRK-TOTALACUM      PIC 9(03)    VALUE ZEROS.                  00090900
      *====================================================             00091000
       PROCEDURE                                 DIVISION.              00091100
      *====================================================             00091200
      *----------------------------------------------------             00091300
       0000-PRINCIPAL                             SECTION.              00091400
      *----------------------------------------------------             00091500
           PERFORM 1000-INICIALIZAR.                                    00091600
           PERFORM 2000-PROCESSAR.                                      00091707
           PERFORM 3000-FINALIZAR.                                      00091900
           STOP RUN.                                                    00092000
      *----------------------------------------------------             00092100
       0000-99-FIM.                                EXIT.                00092200
      *----------------------------------------------------             00092300
                                                                        00092400
      *----------------------------------------------------             00092500
       1000-INICIALIZAR                           SECTION.              00092600
      *----------------------------------------------------             00092700
           ACCEPT WRK-DADOS FROM SYSIN.                                 00092800
      *----------------------------------------------------             00092900
       1000-99-FIM.                                EXIT.                00093000
      *----------------------------------------------------             00093100
                                                                        00093200
      *----------------------------------------------------             00093300
       2000-PROCESSAR                             SECTION.              00093400
      *----------------------------------------------------             00093500
           PERFORM UNTIL WRK-COUNT EQUAL 11                             00093612
             COMPUTE WRK-TOTAL = WRK-NUMERO * WRK-COUNT                 00093708
             DISPLAY WRK-NUMERO '*' WRK-COUNT '=' WRK-TOTAL             00093808
             ADD 1 TO WRK-COUNT                                         00093908
             COMPUTE WRK-TOTALACUM = WRK-TOTALACUM + WRK-TOTAL          00094115
           END-PERFORM.                                                 00094208
             DISPLAY 'TOTAL ACUMULADO: ' WRK-TOTALACUM.                 00094308
      *----------------------------------------------------             00094400
       2000-99-FIM.                                EXIT.                00094500
      *----------------------------------------------------             00094600
                                                                        00094700
      *----------------------------------------------------             00095600
       3000-FINALIZAR                             SECTION.              00096000
      *----------------------------------------------------             00097000
            DISPLAY '----------FIM DO PROGRAMA-----------'.             00097100
      *----------------------------------------------------             00097200
       3000-99-FIM.                                EXIT.                00097300
      *----------------------------------------------------             00097400
                                                                        00097500
      *----------------------------------------------------             00097600
       9000-TRATA-ERROS                            SECTION.             00097700
      *----------------------------------------------------             00097800
      *----------------------------------------------------             00097900
       9000-99-FIM.                                EXIT.                00098000
      *----------------------------------------------------             00099000

      *====================================================             00010000
       IDENTIFICATION                            DIVISION.              00020000
      *====================================================             00030000
       PROGRAM-ID. FR22CB18.                                            00040000
                                                                        00050000
      *===================================================*             00060000
      * AUTOR   : VICTOR LEAL                             *             00070000
      * EMPRESA : FOURSYS                                 *             00080000
      * OBJETIVO: IFELSE, FRETE ESTADOS.                  *             00081000
      *===================================================*             00090000
                                                                        00090100
      *====================================================             00090200
       ENVIRONMENT                               DIVISION.              00090300
      *====================================================             00090400
       CONFIGURATION                              SECTION.              00090500
       SPECIAL-NAMES.                                                   00090600
           DECIMAL-POINT IS COMMA.                                      00090700
                                                                        00090800
      *====================================================             00090900
       DATA                                      DIVISION.              00091000
      *====================================================             00091100
      *----------------------------------------------------             00091200
       WORKING-STORAGE                           SECTION.               00091300
      *----------------------------------------------------             00091400
       01 WRK-DADOS.                                                    00091500
          05 WRK-CODIGO      PIC 9(05)       VALUE ZEROS.               00091600
          05 WRK-PRODUTO     PIC X(10)       VALUE SPACES.              00091700
          05 WRK-VALOR       PIC 9(06)V99    VALUE ZEROS.               00091800
          05 WRK-ESTADO      PIC X(02)       VALUE SPACES.              00091900
      *----------------------------------------------------             00092000
       01 FILLER PIC X(48) VALUE                                        00092100
           '--------------VARIAVEIS DE APOIO-------------'.             00092200
      *----------------------------------------------------             00092300
       77 WRK-FRETE          PIC 9(06)V99    VALUE ZEROS.               00092400
       77 WRK-FLAG           PIC 9           VALUE 0.                   00092500
      *-----------------MASCARAS---------------------------             00092600
       01 FILLER PIC X(48) VALUE                                        00092700
           '--------------VARIAVEIS PARA MASCARA---------'.             00092800
      *----------------------------------------------------             00092900
       77 WRK-VALOR-ED       PIC ZZZ.ZZ9,99.                            00093000
       77 WRK-FRETE-ED       PIC ZZZ.ZZ9,99.                            00093100
      *====================================================             00093200
       PROCEDURE                                 DIVISION.              00093300
      *====================================================             00093400
      *----------------------------------------------------             00093500
       0000-PRINCIPAL                             SECTION.              00093600
      *----------------------------------------------------             00093700
           PERFORM 1000-INICIALIZAR.                                    00093800
           PERFORM 2000-PROCESSAR.                                      00093900
           PERFORM 3000-FINALIZAR.                                      00094000
           STOP RUN.                                                    00094100
      *----------------------------------------------------             00094200
       0000-99-FIM.                                  EXIT.              00094300
      *----------------------------------------------------             00094400
                                                                        00094500
      *----------------------------------------------------             00094600
       1000-INICIALIZAR                           SECTION.              00094700
      *----------------------------------------------------             00094800
           ACCEPT WRK-DADOS FROM SYSIN.                                 00094900
      *----------------------------------------------------             00095000
       1000-99-FIM.                                  EXIT.              00095100
      *----------------------------------------------------             00095200
                                                                        00095300
      *----------------------------------------------------             00095400
       2000-PROCESSAR                             SECTION.              00095500
      *----------------------------------------------------             00095600
           IF WRK-VALOR LESS 1000                                       00095700
            DISPLAY ' RETIRAR O PRODUTO NA LOJA'                        00095800
           ELSE                                                         00095900
            IF  WRK-ESTADO EQUAL 'SP'                                   00096000
             COMPUTE WRK-FRETE = WRK-VALOR * 0,10                       00096100
             DISPLAY 'CODIGO:          '  WRK-CODIGO                    00096200
             DISPLAY 'PRODUTO:         '  WRK-PRODUTO                   00096300
             MOVE WRK-VALOR TO WRK-VALOR-ED                             00096400
             DISPLAY 'VALOR DO PRODUTO:'  WRK-VALOR-ED                  00096500
             DISPLAY 'O PRODUTO SERA ENTREGUE EM SUA CASA'              00096600
             MOVE WRK-FRETE TO WRK-FRETE-ED                             00096700
             DISPLAY 'VALOR DO FRETE: '   WRK-FRETE-ED                  00096800
              ADD 1 TO WRK-FLAG                                         00096900
            END-IF                                                      00097000
            IF WRK-ESTADO EQUAL 'RJ'                                    00097100
             COMPUTE WRK-FRETE = WRK-VALOR * 0,15                       00097200
             DISPLAY 'CODIGO:          '  WRK-CODIGO                    00097300
             DISPLAY 'PRODUTO:         '  WRK-PRODUTO                   00097400
             MOVE WRK-VALOR TO WRK-VALOR-ED                             00097500
             DISPLAY 'VALOR DO PRODUTO:'  WRK-VALOR-ED                  00097600
             DISPLAY 'O PRODUTO SERA ENTREGUE EM SUA CASA'              00097700
             MOVE WRK-FRETE TO WRK-FRETE-ED                             00097800
             DISPLAY 'VALOR DO FRETE: ' WRK-FRETE-ED                    00097900
              ADD 1 TO WRK-FLAG                                         00098000
            END-IF                                                      00098100
            IF  WRK-ESTADO EQUAL 'MG'                                   00098200
             COMPUTE WRK-FRETE = WRK-VALOR * 0,20                       00098300
             DISPLAY 'CODIGO:          '  WRK-CODIGO                    00098400
             DISPLAY 'PRODUTO:         '  WRK-PRODUTO                   00098500
             MOVE WRK-VALOR TO WRK-VALOR-ED                             00098600
             DISPLAY 'VALOR DO PRODUTO:'  WRK-VALOR-ED                  00098700
             DISPLAY 'O PRODUTO SERA ENTREGUE EM SUA CASA'              00098800
             MOVE WRK-FRETE TO WRK-FRETE-ED                             00098900
             DISPLAY 'VALOR DO FRETE: ' WRK-FRETE-ED                    00099000
              ADD 1 TO WRK-FLAG                                         00099100
            END-IF                                                      00099200
            IF WRK-FLAG EQUAL 0                                         00099300
             DISPLAY 'VOCE DEVE RETIRAR O PRODUTO NA LOJA'              00099400
            END-IF                                                      00099500
           END-IF.                                                      00099600
      *----------------------------------------------------             00099700
       2000-99-FIM.                                  EXIT.              00099800
      *----------------------------------------------------             00099900
                                                                        00100000
      *----------------------------------------------------             00101000
       3000-FINALIZAR                             SECTION.              00102000
      *----------------------------------------------------             00103000
           DISPLAY '----------FIM DO PROGRAMA-----------'.              00110000
      *----------------------------------------------------             00120000
       3000-99-FIM.                                  EXIT.              00130000
      *----------------------------------------------------             00140000

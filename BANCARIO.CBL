       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANCARIO.
       AUTHOR. HDK.
       INSTALLATION. CASA.
       DATE-WRITTEN. 29/05/2026.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-PC.
       OBJECT-COMPUTER. IBM-PC.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQIDX ASSIGN TO "ARQIDX"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS CPF-IN
               ALTERNATE KEY IS NOME-IN
                   WITH DUPLICATES
               FILE STATUS IS FS.

           SELECT RELATORIO ASSIGN TO "RELAT.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS FS-REL.

       DATA DIVISION.
       FILE SECTION.

       FD  ARQIDX.
       01  REGIDX.
           02 CPF-IN             PIC 9(11).
           02 NOME-IN            PIC X(20).
           02 CELULAR-IN         PIC 9(11).
           02 ENDERECO-IN        PIC X(30).
           02 CONTA-CORRENTE-IN  PIC X(10).
           02 SALDO              PIC S9(9)V99.
           02 AGENCIA            PIC X(5).

       FD  RELATORIO.
       01  LINHA-REL             PIC X(120).

       WORKING-STORAGE SECTION.
       77  FS                    PIC X(2) VALUE SPACES.
       77  FS-REL                PIC X(2) VALUE SPACES.
       77  NOME                  PIC X(20) VALUE SPACES.
       77  CPF                   PIC 9(11) VALUE 0.
       77  CELULAR               PIC 9(11) VALUE 0.
       77  ENDERECO              PIC X(30) VALUE SPACES.
       77  CONTA-CORRENTE        PIC X(10) VALUE SPACES.
       77  FLAG-OPCAO            PIC 9 VALUE 0.
       77  FLAG-GRAVAR           PIC X VALUE SPACES.
       77  TECLA-ENTER           PIC X VALUE SPACES.
       77  VALOR                 PIC 9(9)V99 VALUE 0.
       77  CONFIRMA              PIC X VALUE SPACES.
       77  CONT-LIN              PIC 99 VALUE 0.
       77  CONT-PAG              PIC 99 VALUE 0.
       77  TOTAL-CLIENTES        PIC 9(9) VALUE 0.
       77  TOTAL-SALDOS          PIC S9(11)V99 VALUE 0.
       77  FIM-ARQ               PIC X VALUE "N".

       01  DATA-SISTEMA.
           02 ANO                PIC 9(2).
           02 MES                PIC 9(2).
           02 DIA                PIC 9(2).

       01  DATA-EDITADA.
           02 DIA-E              PIC 99.
           02 FILLER             PIC X VALUE "/".
           02 MES-E              PIC 99.
           02 FILLER             PIC X VALUE "/".
           02 ANO-E              PIC 99.

       01  SALDO-EDITADO         PIC -ZZZ.ZZZ.ZZ9,99.
       01  TOTAL-EDITADO         PIC -ZZ.ZZZ.ZZZ.ZZ9,99.
       01  CLIENTES-EDITADO      PIC ZZZ.ZZZ.ZZ9.

       SCREEN SECTION.
       01  TELA-MENU.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 30 VALUE "MENU-BANCO DO COBOL ETERNO".
           02 LINE 04 COLUMN 10 VALUE "1 - ABRIR CONTA".
           02 LINE 05 COLUMN 10 VALUE "2 - ALTERAR DADOS DO CLIENTE".
           02 LINE 06 COLUMN 10 VALUE "3 - FAZER DEPOSITO".
           02 LINE 07 COLUMN 10 VALUE "4 - REALIZAR SAQUE".
           02 LINE 08 COLUMN 10 VALUE "5 - VER SALDO".
           02 LINE 09 COLUMN 10 VALUE "6 - ENCERRAR CONTA".
           02 LINE 10 COLUMN 10 VALUE "7 - GERAR RELATORIO DO BANCO".
           02 LINE 11 COLUMN 10 VALUE "8 - SAIR".
           02 LINE 12 COLUMN 10 VALUE "DIGITE A OPCAO DESEJADA: ( )".

       01  TELA-ABRIR-CONTA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 30 VALUE "ABRIR CONTA".
           02 LINE 03 COLUMN 10 VALUE "NOME:".
           02 LINE 04 COLUMN 10 VALUE "CPF:".
           02 LINE 05 COLUMN 10 VALUE "CELULAR:".
           02 LINE 06 COLUMN 10 VALUE "ENDERECO:".
           02 LINE 07 COLUMN 10 VALUE "NUMERO DA CONTA CORRENTE:".
           02 LINE 08 COLUMN 10 VALUE
              "DESEJA SALVAR OS DADOS DO CLIENTE: S OU N ( )".

       01  TELA-ALTERAR-DADOS.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 30 VALUE "ALTERAR DADOS DO CLIENTE".
           02 LINE 03 COLUMN 10 VALUE "INSIRA O CPF DO CLIENTE:".
           02 LINE 04 COLUMN 10 VALUE "NOME:".
           02 LINE 05 COLUMN 10 VALUE "CELULAR:".
           02 LINE 06 COLUMN 10 VALUE "ENDERECO:".
           02 LINE 07 COLUMN 10 VALUE
              "DESEJA SALVAR OS DADOS DO CLIENTE: S OU N ( )".

       01  TELA-ENCERRAR-CONTA.
           02 BLANK SCREEN.
           02 LINE 02 COLUMN 30 VALUE "ENCERRAR CONTA".
           02 LINE 03 COLUMN 10 VALUE "INSIRA O CPF DO CLIENTE:".
           02 LINE 04 COLUMN 10 VALUE "NOME:".
           02 LINE 05 COLUMN 10 VALUE
              "DESEJA MESMO ENCERRAR A CONTA".
           02 LINE 06 COLUMN 10 VALUE
              "E EXCLUIR OS DADOS DO CLIENTE: S OU N ( )".

       01  TELA-DEPOSITO.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 25 VALUE "FAZER DEPOSITO".
           05 LINE 05 COLUMN 10 VALUE "CPF DO CLIENTE: ".
           05 LINE 05 COLUMN 27 PIC 9(11) TO CPF.
           05 LINE 07 COLUMN 10 VALUE "VALOR DO DEPOSITO: ".
           05 LINE 07 COLUMN 30 PIC 9(9)V99 TO VALOR.
           05 LINE 08 COLUMN 10 VALUE
              "(11 DIGITOS: OS 2 ULTIMOS SAO OS CENTAVOS)".

       01  TELA-SAQUE.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 25 VALUE "REALIZAR SAQUE".
           05 LINE 05 COLUMN 10 VALUE "CPF DO CLIENTE: ".
           05 LINE 05 COLUMN 27 PIC 9(11) TO CPF.
           05 LINE 07 COLUMN 10 VALUE "VALOR DO SAQUE: ".
           05 LINE 07 COLUMN 27 PIC 9(9)V99 TO VALOR.
           05 LINE 08 COLUMN 10 VALUE
              "(11 DIGITOS: OS 2 ULTIMOS SAO OS CENTAVOS)".

       01  TELA-SALDO.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 25 VALUE "VER SALDO".
           05 LINE 05 COLUMN 10 VALUE "CPF DO CLIENTE: ".
           05 LINE 05 COLUMN 27 PIC 9(11) TO CPF.

       01  TELA-RELATORIO.
           05 BLANK SCREEN.
           05 LINE 02 COLUMN 25 VALUE "RELATORIO DO BANCO".

       01  TELA-MENSAGEM.
           05 LINE 20 COLUMN 10
              VALUE "PRESSIONE ENTER PARA CONTINUAR...".
           05 LINE 20 COLUMN 45 PIC X TO CONFIRMA.

       PROCEDURE DIVISION.

       SIST-MAIN.
           OPEN I-O ARQIDX
           IF FS = "35"
               OPEN OUTPUT ARQIDX
               CLOSE ARQIDX
               OPEN I-O ARQIDX
           END-IF
           IF FS NOT = "00"
               DISPLAY "ERRO AO ABRIR ARQUIVO. STATUS: " FS
               STOP RUN
           END-IF

           PERFORM PRINCIPAL UNTIL FLAG-OPCAO = 8

           DISPLAY "SAINDO..." AT 1410
           CLOSE ARQIDX
           STOP RUN.

       PRINCIPAL.
           DISPLAY TELA-MENU
           ACCEPT FLAG-OPCAO AT 1236

           EVALUATE FLAG-OPCAO
               WHEN 1
                   PERFORM ABRIR-CONTA
               WHEN 2
                   PERFORM ALTERAR-DADOS
               WHEN 3
                   PERFORM DEPOSITO
               WHEN 4
                   PERFORM SAQUE
               WHEN 5
                   PERFORM SALDO-ROT
               WHEN 6
                   PERFORM ENCERRAR-CONTA
               WHEN 7
                   PERFORM RELATORIO-ROT
               WHEN 8
                   CONTINUE
               WHEN OTHER
                   DISPLAY "OPCAO INVALIDA" AT 1410
                   PERFORM PAUSA
           END-EVALUATE.

       PAUSA.
           MOVE SPACES TO CONFIRMA
           DISPLAY TELA-MENSAGEM
           ACCEPT TELA-MENSAGEM.

       ABRIR-CONTA.
           MOVE SPACES TO NOME ENDERECO CONTA-CORRENTE FLAG-GRAVAR
           MOVE 0 TO CPF CELULAR
           DISPLAY TELA-ABRIR-CONTA
           ACCEPT NOME           AT 0316
           ACCEPT CPF            AT 0415
           ACCEPT CELULAR        AT 0519
           ACCEPT ENDERECO       AT 0620
           ACCEPT CONTA-CORRENTE AT 0736
           ACCEPT FLAG-GRAVAR    AT 0853

           IF FLAG-GRAVAR = "S" OR FLAG-GRAVAR = "s"
               PERFORM GRAVAR
           END-IF
           PERFORM PAUSA.

       GRAVAR.
           MOVE NOME TO NOME-IN
           MOVE CPF TO CPF-IN
           MOVE CELULAR TO CELULAR-IN
           MOVE ENDERECO TO ENDERECO-IN
           MOVE CONTA-CORRENTE TO CONTA-CORRENTE-IN
           MOVE 0 TO SALDO
           MOVE SPACES TO AGENCIA

           WRITE REGIDX
               INVALID KEY
                   IF FS = "22"
                       DISPLAY "CPF JA CADASTRADO"
                           AT 1010
                   ELSE
                       DISPLAY "ERRO NA GRAVACAO. STATUS: " FS
                           AT 1010
                   END-IF
               NOT INVALID KEY
                   DISPLAY "REGISTRO GRAVADO" AT 1010
           END-WRITE.

       ALTERAR-DADOS.
           MOVE SPACES TO NOME ENDERECO FLAG-GRAVAR
           MOVE 0 TO CPF CELULAR
           DISPLAY TELA-ALTERAR-DADOS
           ACCEPT CPF          AT 0335
           ACCEPT NOME         AT 0416
           ACCEPT CELULAR      AT 0519
           ACCEPT ENDERECO     AT 0620
           ACCEPT FLAG-GRAVAR  AT 0753

           IF FLAG-GRAVAR = "S" OR FLAG-GRAVAR = "s"
               PERFORM REGRAVAR
           END-IF
           PERFORM PAUSA.

       REGRAVAR.
           MOVE CPF TO CPF-IN

           READ ARQIDX
               KEY IS CPF-IN
               INVALID KEY
                   DISPLAY "CLIENTE NAO ENCONTRADO" AT 0910
               NOT INVALID KEY
                   MOVE NOME TO NOME-IN
                   MOVE CELULAR TO CELULAR-IN
                   MOVE ENDERECO TO ENDERECO-IN
                   REWRITE REGIDX
                       INVALID KEY
                           DISPLAY "ERRO AO ATUALIZAR. STATUS: " FS
                               AT 0910
                       NOT INVALID KEY
                           DISPLAY "DADOS ATUALIZADOS COM SUCESSO"
                               AT 0910
                   END-REWRITE
           END-READ.

       ENCERRAR-CONTA.
           MOVE SPACES TO FLAG-GRAVAR
           MOVE 0 TO CPF
           DISPLAY TELA-ENCERRAR-CONTA
           ACCEPT CPF          AT 0335
           MOVE CPF TO CPF-IN

           READ ARQIDX
               KEY IS CPF-IN
               INVALID KEY
                   DISPLAY "CLIENTE NAO ENCONTRADO" AT 0810
               NOT INVALID KEY
                   DISPLAY NOME-IN AT 0416
                   ACCEPT FLAG-GRAVAR AT 0649
                   IF FLAG-GRAVAR = "S" OR FLAG-GRAVAR = "s"
                       PERFORM EXCLUIR
                   END-IF
           END-READ
           PERFORM PAUSA.

       EXCLUIR.
           DELETE ARQIDX
               INVALID KEY
                   DISPLAY "ERRO AO EXCLUIR. STATUS: " FS
                       AT 0810
               NOT INVALID KEY
                   DISPLAY "REGISTRO EXCLUIDO" AT 0810
           END-DELETE.

       LER-CLIENTE.
           MOVE CPF TO CPF-IN

           READ ARQIDX
               KEY IS CPF-IN
               INVALID KEY
                   DISPLAY "CLIENTE NAO ENCONTRADO." AT 1010
           END-READ.

       DEPOSITO.
           MOVE 0 TO VALOR CPF
           DISPLAY TELA-DEPOSITO
           ACCEPT TELA-DEPOSITO

           PERFORM LER-CLIENTE

           IF FS = "00"
               IF VALOR > 0
                   ADD VALOR TO SALDO

                   REWRITE REGIDX
                       INVALID KEY
                           DISPLAY "ERRO AO ATUALIZAR DEPOSITO."
                               AT 1010
                       NOT INVALID KEY
                           DISPLAY "DEPOSITO REALIZADO COM SUCESSO."
                               AT 1010
                           MOVE SALDO TO SALDO-EDITADO
                           DISPLAY "SALDO ATUAL: " SALDO-EDITADO
                               AT 1110
                   END-REWRITE
               ELSE
                   DISPLAY "VALOR INVALIDO." AT 1010
               END-IF
           END-IF

           PERFORM PAUSA.

       SAQUE.
           MOVE 0 TO VALOR CPF
           DISPLAY TELA-SAQUE
           ACCEPT TELA-SAQUE

           PERFORM LER-CLIENTE

           IF FS = "00"
               IF VALOR <= 0
                   DISPLAY "VALOR INVALIDO." AT 1010
               ELSE
                   IF VALOR > SALDO
                       DISPLAY "SALDO INSUFICIENTE." AT 1010
                   ELSE
                       SUBTRACT VALOR FROM SALDO

                       REWRITE REGIDX
                           INVALID KEY
                               DISPLAY "ERRO AO ATUALIZAR SAQUE."
                                   AT 1010
                           NOT INVALID KEY
                               DISPLAY "SAQUE REALIZADO COM SUCESSO."
                                   AT 1010
                               MOVE SALDO TO SALDO-EDITADO
                               DISPLAY "SALDO ATUAL: " SALDO-EDITADO
                                   AT 1110
                       END-REWRITE
                   END-IF
               END-IF
           END-IF

           PERFORM PAUSA.

       SALDO-ROT.
           MOVE 0 TO CPF
           DISPLAY TELA-SALDO
           ACCEPT TELA-SALDO

           PERFORM LER-CLIENTE

           IF FS = "00"
               MOVE NOME-IN TO NOME
               MOVE CPF-IN TO CPF
               MOVE CONTA-CORRENTE-IN TO CONTA-CORRENTE

               MOVE SALDO TO SALDO-EDITADO
               DISPLAY "NOME: " NOME AT 0910
               DISPLAY "CPF: " CPF AT 1010
               DISPLAY "AGENCIA: " AGENCIA AT 1110
               DISPLAY "CONTA CORRENTE: " CONTA-CORRENTE AT 1210
               DISPLAY "SALDO: " SALDO-EDITADO AT 1310
           END-IF

           PERFORM PAUSA.

       RELATORIO-ROT.
           DISPLAY TELA-RELATORIO
           ACCEPT DATA-SISTEMA FROM DATE
           MOVE DIA TO DIA-E
           MOVE MES TO MES-E
           MOVE ANO TO ANO-E

           OPEN OUTPUT RELATORIO

           IF FS-REL NOT = "00"
               DISPLAY "ERRO AO CRIAR RELATORIO. STATUS: "
                   FS-REL AT 0510
           ELSE
               PERFORM GERAR-RELATORIO
               CLOSE RELATORIO
               DISPLAY "RELATORIO GERADO COM SUCESSO: RELAT.TXT"
                   AT 0510
               DISPLAY "ABRA NO WORD E SALVE COMO .DOCX." AT 0610
           END-IF

           PERFORM PAUSA.

       GERAR-RELATORIO.
           MOVE 0 TO CONT-LIN
           MOVE 0 TO CONT-PAG
           MOVE 0 TO TOTAL-CLIENTES
           MOVE 0 TO TOTAL-SALDOS
           MOVE "N" TO FIM-ARQ

           MOVE 0 TO CPF-IN

           START ARQIDX KEY IS NOT LESS THAN CPF-IN
               INVALID KEY
                   MOVE "S" TO FIM-ARQ
           END-START

           IF FIM-ARQ = "N"
               PERFORM CABECALHO
           END-IF

           PERFORM UNTIL FIM-ARQ = "S"
               READ ARQIDX NEXT RECORD
                   AT END
                       MOVE "S" TO FIM-ARQ
                   NOT AT END
                       PERFORM IMPRIMIR-DETALHE
               END-READ
           END-PERFORM

           IF TOTAL-CLIENTES = 0
               PERFORM CABECALHO
           END-IF

           PERFORM RODAPE-FINAL.

       CABECALHO.
           ADD 1 TO CONT-PAG
           MOVE 0 TO CONT-LIN

           MOVE SPACES TO LINHA-REL
           STRING
               "RELATORIO DO BANCO COBOL ETERNO PAG. "
                                       DELIMITED BY SIZE
               CONT-PAG                DELIMITED BY SIZE
               " DATA:"                DELIMITED BY SIZE
               DATA-EDITADA            DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL

           MOVE SPACES TO LINHA-REL
           WRITE LINHA-REL

           MOVE SPACES TO LINHA-REL
           MOVE
               "CPF          NOME                  CELULAR      SALDO"
               TO LINHA-REL
           WRITE LINHA-REL

           MOVE ALL "-" TO LINHA-REL
           WRITE LINHA-REL.

       IMPRIMIR-DETALHE.
           IF CONT-LIN = 20
               PERFORM CABECALHO
           END-IF

           MOVE CPF-IN TO CPF
           MOVE NOME-IN TO NOME
           MOVE CELULAR-IN TO CELULAR

           MOVE SALDO TO SALDO-EDITADO
           MOVE SPACES TO LINHA-REL

           STRING
               CPF            DELIMITED BY SIZE
               "  "           DELIMITED BY SIZE
               NOME           DELIMITED BY SIZE
               "  "           DELIMITED BY SIZE
               CELULAR        DELIMITED BY SIZE
               "  "           DELIMITED BY SIZE
               SALDO-EDITADO  DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING

           WRITE LINHA-REL

           ADD 1 TO CONT-LIN
           ADD 1 TO TOTAL-CLIENTES
           ADD SALDO TO TOTAL-SALDOS.

       RODAPE-FINAL.
           MOVE TOTAL-CLIENTES TO CLIENTES-EDITADO
           MOVE TOTAL-SALDOS TO TOTAL-EDITADO

           MOVE SPACES TO LINHA-REL
           WRITE LINHA-REL

           MOVE SPACES TO LINHA-REL
           STRING
               "TOTAL DE CLIENTES: "  DELIMITED BY SIZE
               CLIENTES-EDITADO       DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL

           MOVE SPACES TO LINHA-REL
           STRING
               "TOTAL DOS SALDOS: "   DELIMITED BY SIZE
               TOTAL-EDITADO          DELIMITED BY SIZE
               INTO LINHA-REL
           END-STRING
           WRITE LINHA-REL.

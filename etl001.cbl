      ******************************************************************
      * Author:    JOSE LANZOS GOMEZ
      * Date:      17/04/2022
      * Purpose:   ETL DEMO IN COBOL
      * Compiler:  gnucobol 3.2.1
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ETL001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
        SELECT CLIENTES ASSIGN TO "data/CLIENTES.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT CLIENTES-I ASSIGN TO "data/CLIENTES_I.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CLIENTE-I-ID.

        SELECT TELEFONOS ASSIGN TO "data/TELEFONOS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT TELEFONOS-I ASSIGN TO "data/TELEFONOS_I.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TELEFONO-I-NUM.

        SELECT TARIFAS ASSIGN TO "data/TARIFAS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT TARIFAS-I ASSIGN TO "data/TARIFAS_I.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TARIFA-I-ID.

        SELECT LLAMADAS ASSIGN TO "data/LLAMADAS.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT LLAMADAS-CLI ASSIGN TO "data/LLAMADAS_CLIENTES.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT LLAMADAS-CLI-S ASSIGN TO "data/LLAMADAS_CLI_SORT.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT WORK ASSIGN TO WRK
        ORGANIZATION IS LINE SEQUENTIAL.

        SELECT LLAMADAS-CLI-M ASSIGN TO "data/LLAMADAS_CLI_M.DAT"
        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENTES.
       01  CLIENTE.
           02  CLIENTE-ID     PIC X(5).
           02  CLIENTE-NOM    PIC X(16).
           02  CLIENTE-CUE    PIC X(30).
       FD  CLIENTES-I.
       01  CLIENTE-I.
           02  CLIENTE-I-ID     PIC X(5).
           02  CLIENTE-I-NOM    PIC X(16).
           02  CLIENTE-I-CUE    PIC X(30).

       FD  TELEFONOS.
       01  TELEFONO.
           02  TELEFONO-NUM      PIC X(16).
           02  TELEFONO-CLI      PIC X(5).
       FD  TELEFONOS-I.
       01  TELEFONO-I.
           02  TELEFONO-I-NUM      PIC X(16).
           02  TELEFONO-I-CLI      PIC X(5).

       FD  TARIFAS.
       01  TARIFA.
           02  TARIFA-ID      PIC X(5).
           02  TARIFA-TIP     PIC X(14).
           02  TARIFA-IMP     PIC 9V99.

       FD  TARIFAS-I.
       01  TARIFA-I.
           02  TARIFA-I-ID      PIC X(5).
           02  TARIFA-I-TIP     PIC X(14).
           02  TARIFA-I-IMP     PIC 9V99.

       FD  LLAMADAS.
       01  LLAMADA.
           02  TEL-ORIG    PIC X(16).
           02  TEL-DEST    PIC X(16).
           02  INICIO      PIC X(16).
           02  FIN         PIC X(16).

       FD  LLAMADAS-CLI.
       01  LLAMADA-CLI.
           02  LLC-CLIENTE-ID   PIC X(5).
           02  LLC-TEL-NUM      PIC X(16).
           02  LLC-TEL-DES      PIC X(16).
           02  LLC-INICIO       PIC X(16).
           02  LLC-FIN          PIC X(16).
           02  LLC-TIEMPO       PIC 9(8).
           02  LLC-SEP          PIC X.
           02  LLC-IMPORTE      PIC 9(8)V99.

       FD  LLAMADAS-CLI-S.
       01  LLAMADA-CLI-S.
           02  LLCS-CLIENTE-ID   PIC X(5).
           02  LLCS-TEL-NUM      PIC X(16).
           02  LLCS-TEL-DES      PIC X(16).
           02  LLCS-INICIO       PIC X(16).
           02  LLCS-FIN          PIC X(16).
           02  LLCS-TIEMPO       PIC 9(8).
           02  LLCS-SEP          PIC X.
           02  LLCS-IMPORTE      PIC 9(8)V99.

       SD  WORK.
       01  LLAMADA-CLI-W.
           02  LLCW-CLIENTE-ID   PIC X(5).
           02  LLCW-TEL-NUM      PIC X(16).
           02  LLCW-TEL-DES      PIC X(16).
           02  LLCW-INICIO       PIC X(16).
           02  LLCW-FIN          PIC X(16).
           02  LLCW-TIEMPO       PIC 9(8).
           02  LLCW-SEP          PIC X.
           02  LLCW-IMPORTE      PIC 9(8)V99.

       FD  LLAMADAS-CLI-M.
       01  LLAMADA-CLI-M.
           02  LLCM-CLIENTE-ID   PIC X(5).
           02  LLCM-CLIENTE-NOM  PIC X(16).
           02  LLCM-CUENTA       PIC X(30).
           02  LLCM-IMPORTE      PIC 9(8)V99.

       WORKING-STORAGE SECTION.
       01  WS-CLIENTE.
           02  WS-CLIENTE-ID     PIC X(5).
           02  WS-CLIENTE-NOM    PIC X(16).
           02  WS-CLIENTE-CUE    PIC X(30).
       01  WS-CLIENTE-I.
           02  WS-CLIENTE-I-ID   PIC X(5).
           02  WS-CLIENTE-I-NOM  PIC X(16).
           02  WS-CLIENTE-I-CUE  PIC X(30).
       01  WS-TELEFONO.
           02  WS-TELEFONO-NUM   PIC X(16).
           02  WS-TELEFONO-CLI   PIC X(5).
       01  WS-TELEFONO-I.
           02  WS-TELEFONO-I-NUM PIC X(16).
           02  WS-TELEFONO-I-CLI PIC X(5).
       01  WS-TARIFA.
           02  WS-TARIFA-ID      PIC X(5).
           02  WS-TARIFA-TIP     PIC X(14).
           02  WS-TARIFA-IMP     PIC 9V99.
       01  WS-TARIFA-I.
           02  WS-TARIFA-I-ID    PIC X(5).
           02  WS-TARIFA-I-TIP   PIC X(14).
           02  WS-TARIFA-I-IMP   PIC 9V99.
       01  WS-LLAMADA.
           02  WS-TEL-ORIG       PIC X(16).
           02  WS-TEL-DEST       PIC X(16).
           02  WS-INICIO.
               03 WS-INI-FECHA.
                  04 WS-INI-DD   PIC 9(2).
                  04 WS-INI-MM   PIC 9(2).
                  04 WS-INI-YYYY PIC 9(4).
               03 FILLER   PIC X.
               03 WS-INI-HORA.
                  04 WS-INI-HH   PIC 9(2).
                  04 WS-INI-NN   PIC 9(2).
                  04 WS-INI-SS   PIC 9(2).
               03 FILLER   PIC X.
           02  WS-FIN.
               03 WS-FIN-FECHA.
                  04 WS-FIN-DD   PIC 9(2).
                  04 WS-FIN-MM   PIC 9(2).
                  04 WS-FIN-YYYY PIC 9(4).
               03 FILLER   PIC X.
               03 WS-FIN-HORA.
                  04 WS-FIN-HH   PIC 9(2).
                  04 WS-FIN-NN   PIC 9(2).
                  04 WS-FIN-SS   PIC 9(2).
               03 FILLER   PIC X.
       01  WS-LLAMADA-CLI.
           02  WS-LLC-CLIENTE-ID   PIC X(5).
           02  WS-LLC-TEL-NUM      PIC X(16).
           02  WS-LLC-TEL-DES      PIC X(16).
           02  WS-LLC-INICIO       PIC X(16).
           02  WS-LLC-FIN          PIC X(16).
           02  WS-LLC-TIEMPO       PIC 9(8).
           02  WS-SEP              PIC X VALUE SPACE.
           02  WS-LLC-IMPORTE      PIC 9(8)V99.

       01  WS-LLAMADA-CLI-S.
           02  WS-LLCS-CLIENTE-ID   PIC X(5).
           02  WS-LLCS-TEL-NUM      PIC X(16).
           02  WS-LLCS-TEL-DES      PIC X(16).
           02  WS-LLCS-INICIO       PIC X(16).
           02  WS-LLCS-FIN          PIC X(16).
           02  WS-LLCS-TIEMPO       PIC 9(8).
           02  WS-LLCS-SEP          PIC X VALUE SPACE.
           02  WS-LLCS-IMPORTE       PIC 9(8)V99.

       01  WS-LLAMADA-CLI-M.
           02  WS-LLCM-CLIENTE-ID   PIC X(5).
           02  WS-LLCM-CLIENTE-NOM  PIC X(16).
           02  WS-LLCM-CUENTA       PIC X(30).
           02  WS-LLCM-IMPORTE      PIC 9(8)V99.

       01  WS-EOF     PIC A(1).
       01  WS-DATE0   PIC X(8).
       01  WS-DATE1   PIC X(8).
       01  WS-DATE00  PIC 9(8).
       01  WS-DATE01  PIC 9(8).
       01  WS-DAYS    PIC 9.
       01  WS-TIME0   PIC X(8).
       01  WS-TIME1   PIC X(8).
       01  WS-TIME00  PIC 9(8).
       01  WS-TIME01  PIC 9(8).
       01  WS-SECS    PIC 9(8).
       01  WS-TIEMPO  PIC 9(8).
       01  WS-IMP-TAR PIC 9V99.
       01  WS-IMPORTE PIC 9(8)V99.
       01  WS-CLI-ACT PIC X(5).
       01  WS-CLI-ANT PIC X(5).
       01  WS-CUENTA  PIC X(30) VALUE SPACES.
       01  WS-CLI-NOM PIC X(16) VALUE SPACES.
       01  WS-IMP-CLI PIC 9(8)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY "ETL Telco Demo"
           PERFORM 0100-READ-CLIENTES.
           PERFORM 0200-READ-TELEFONOS.
           PERFORM 0300-READ-TARIFAS.
           PERFORM 0400-READ-LLAMADAS.
           PERFORM 0800-SORT-LLAMADAS-CLI.
           PERFORM 0900-LLAMADAS-RES.
           STOP RUN.

       0100-READ-CLIENTES.
           DISPLAY "CLIENTES".
           OPEN INPUT CLIENTES.
           OPEN OUTPUT CLIENTES-I.
           PERFORM UNTIL WS-EOF='Y'
             READ CLIENTES INTO WS-CLIENTE
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-CLIENTE
                      WRITE CLIENTE-I FROM WS-CLIENTE
                      INVALID KEY
                        DISPLAY "RECORD ALREADY ON FILE"
             END-READ
           END-PERFORM.
           CLOSE CLIENTES.
           CLOSE CLIENTES-I.

       0200-READ-TELEFONOS.
           DISPLAY "TELEFONOS".
           MOVE 'N' TO WS-EOF.
           OPEN INPUT  TELEFONOS.
           OPEN OUTPUT TELEFONOS-I.
           PERFORM UNTIL WS-EOF='Y'
             READ TELEFONOS INTO WS-TELEFONO
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-TELEFONO
                          WRITE TELEFONO-I FROM WS-TELEFONO
                          INVALID KEY
                            DISPLAY "RECORD ALREADY ON FILE"
             END-READ
           END-PERFORM.
           CLOSE TELEFONOS.
           CLOSE TELEFONOS-I.

       0300-READ-TARIFAS.
           DISPLAY "TARIFAS".
           MOVE 'N' TO WS-EOF.
           OPEN INPUT  TARIFAS.
           OPEN OUTPUT TARIFAS-I.
           PERFORM UNTIL WS-EOF='Y'
             READ TARIFAS INTO WS-TARIFA
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END DISPLAY WS-TARIFA
                          WRITE TARIFA-I FROM WS-TARIFA
                          INVALID KEY
                            DISPLAY "RECORD ALREADY ON FILE"
             END-READ
           END-PERFORM.
           CLOSE TARIFAS.
           CLOSE TARIFAS-I.

       0400-READ-LLAMADAS.
           DISPLAY "LLAMADAS".
           MOVE 'N' TO WS-EOF.
           OPEN INPUT  LLAMADAS.
           OPEN OUTPUT LLAMADAS-CLI.
           OPEN INPUT  TELEFONOS-I.
           PERFORM UNTIL WS-EOF='Y'
             READ LLAMADAS INTO WS-LLAMADA
               AT END MOVE 'Y' TO WS-EOF
               NOT AT END PERFORM 0500-LLAMADAS-CLIENTE
             END-READ
           END-PERFORM.
           CLOSE LLAMADAS.
           CLOSE LLAMADAS-CLI.
           CLOSE TELEFONOS-I.

       0500-LLAMADAS-CLIENTE.
           MOVE WS-TEL-ORIG TO TELEFONO-I-NUM
           READ TELEFONOS-I INTO WS-TELEFONO-I
             KEY IS TELEFONO-I-NUM
             INVALID KEY
               DISPLAY 'INVALID KEY ' WS-TEL-ORIG "*"
             NOT INVALID KEY
               PERFORM 0600-CALC-TIME
               DISPLAY WS-TELEFONO-I-CLI " " WS-LLAMADA  " "
                       WS-SECS " " WS-IMPORTE
           END-READ.
           MOVE WS-TELEFONO-I-CLI TO LLC-CLIENTE-ID.
           MOVE WS-TEL-ORIG       TO LLC-TEL-NUM.
           MOVE WS-TEL-DEST       TO LLC-TEL-DES.
           MOVE WS-INICIO         TO LLC-INICIO.
           MOVE WS-FIN            TO LLC-FIN.
           MOVE WS-SECS           TO LLC-TIEMPO.
           MOVE WS-SEP            TO LLC-SEP.
           MOVE WS-IMPORTE        TO LLC-IMPORTE.
           WRITE LLAMADA-CLI.

       0600-CALC-TIME.
           STRING WS-INI-YYYY WS-INI-MM WS-INI-DD INTO WS-DATE0.
           MOVE   WS-DATE0 TO WS-DATE00.
           STRING WS-FIN-YYYY WS-FIN-MM WS-FIN-DD INTO WS-DATE1.
           MOVE   WS-DATE1 TO WS-DATE01.
           MOVE   WS-INI-HORA TO WS-TIME00.
           MOVE   WS-FIN-HORA TO WS-TIME01.
           COMPUTE WS-SECS = ( FUNCTION INTEGER-OF-DATE (WS-DATE01) -
                             FUNCTION INTEGER-OF-DATE (WS-DATE00))
                             *24*60*60.
           COMPUTE WS-SECS = WS-SECS +
                   ( WS-FIN-HH *60*60 + WS-FIN-NN * 60 ) -
                   ( WS-INI-HH *60*60 + WS-INI-NN * 60 ).
      *     DISPLAY WS-DATE0 " " WS-DATE1 " " WS-FIN-NN " "
      *             WS-INI-NN " " WS-SECS.
           PERFORM 0700-CALC-TARIFA
           COMPUTE WS-IMPORTE = WS-SECS * WS-IMP-TAR.

       0700-CALC-TARIFA.
           EVALUATE TRUE
             WHEN WS-TEL-DEST(6:3)="900"
               MOVE "T004" TO TARIFA-I-ID
             WHEN WS-TEL-DEST(1:4) NOT EQUAL "0034"
               MOVE "T005" TO TARIFA-I-ID
             WHEN OTHER
               MOVE "T002" TO TARIFA-I-ID
           END-EVALUATE.
           OPEN INPUT  TARIFAS-I.
           READ TARIFAS-I INTO WS-TARIFA-I
             KEY IS TARIFA-I-ID
             INVALID KEY
               DISPLAY 'INVALID KEY '
             NOT INVALID KEY
               MOVE WS-TARIFA-I-IMP TO WS-IMP-TAR
           END-READ.
           CLOSE TARIFAS-I.

       0800-SORT-LLAMADAS-CLI.
           SORT WORK ON ASCENDING KEY LLCS-CLIENTE-ID LLCS-TEL-NUM
           USING LLAMADAS-CLI GIVING LLAMADAS-CLI-S.
           DISPLAY 'LLAMADAS-CLI Sort Successful'.

       0900-LLAMADAS-RES.
           MOVE 'N' TO WS-EOF.
           OPEN INPUT  LLAMADAS-CLI-S.
           OPEN OUTPUT LLAMADAS-CLI-M.
           OPEN INPUT  CLIENTES-I.
           DISPLAY "--".
           MOVE "*" TO WS-CLI-ANT.
           MOVE 0.0 TO WS-IMP-CLI.
           PERFORM UNTIL WS-EOF='Y'
             READ LLAMADAS-CLI-S  INTO WS-LLAMADA-CLI-S
               AT END MOVE 'Y' TO WS-EOF
                  PERFORM 1000-CUENTA-CLIENTE
                  DISPLAY WS-CLI-ANT " " WS-IMP-CLI
                  MOVE WS-CLI-ANT TO WS-LLCM-CLIENTE-ID
                  MOVE WS-CUENTA  TO WS-LLCM-CUENTA
                  MOVE WS-CLI-NOM TO WS-LLCM-CLIENTE-NOM
                  MOVE WS-IMP-CLI TO WS-LLCM-IMPORTE
                  WRITE LLAMADA-CLI-M FROM WS-LLAMADA-CLI-M
               NOT AT END
                    IF WS-LLCS-CLIENTE-ID = WS-CLI-ANT
                       COMPUTE WS-IMP-CLI=WS-IMP-CLI + WS-LLCS-IMPORTE
                    ELSE
                       IF WS-CLI-ANT NOT EQUAL "*"
                         PERFORM 1000-CUENTA-CLIENTE
                         DISPLAY WS-CLI-ANT " " WS-IMP-CLI
                         MOVE WS-CLI-ANT TO WS-LLCM-CLIENTE-ID
                         MOVE WS-CUENTA  TO WS-LLCM-CUENTA
                         MOVE WS-CLI-NOM TO WS-LLCM-CLIENTE-NOM
                         MOVE WS-IMP-CLI TO WS-LLCM-IMPORTE
                         WRITE LLAMADA-CLI-M FROM WS-LLAMADA-CLI-M
                       END-IF
                       MOVE WS-LLCS-IMPORTE TO WS-IMP-CLI
                       MOVE WS-LLCS-CLIENTE-ID TO WS-CLI-ANT
                    END-IF
                    DISPLAY WS-LLAMADA-CLI-S
             END-READ
           END-PERFORM.
           CLOSE LLAMADAS-CLI-S.
           CLOSE LLAMADAS-CLI-M.
           CLOSE CLIENTES-I.

       1000-CUENTA-CLIENTE.
      *    DISPLAY WS-CLI-ANT " " WS-IMP-CLI.
           MOVE WS-CLI-ANT TO CLIENTE-I-ID
           READ CLIENTES-I INTO WS-CLIENTE-I
             KEY IS CLIENTE-I-ID
             INVALID KEY
               DISPLAY 'INVALID KEY '  WS-CLI-ANT "*"
             NOT INVALID KEY
               MOVE WS-CLIENTE-I-CUE TO WS-CUENTA
               MOVE WS-CLIENTE-I-NOM TO WS-CLI-NOM
           END-READ.

       END PROGRAM ETL001.

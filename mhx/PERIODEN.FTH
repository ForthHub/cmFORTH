\\   Sourcecode "Vierte Dimension" 2+3/88 S.24 ff
     PeriodenlÑnge von DezimalbrÅchen,
     100 Dezimalstellen nach dem Komma und mehr
     von Andreas Soeder, Darmstadt
     ( Ohne GewÑhr eingetippt von C. Krinninger )

     NÑhere Informationen bei:
        FORTH-Gesellschaft e.V.
        Postfach 1110
        D-8044 Unterschleiûheim
        Tel: 089/317 37 84

            Hinweise zur Anpassung ans volksFORTH:
 PRIMTAB als Feld mit Grî·e SIZE; Variablen ohne Defaultwert
 UM* ist U* ; UM/ ist U/ ; KEY? ist ?TERMINAL ; Zeichensatz?
 Vorletzte Zeile in Screen # 2 lîschen ;  --> ersetzt ;S
\ Primzahlen nach VD II/3, S. 5
HEX 8000 CONSTANT PRIMTAB     ( RAM-DISK-BEREICH )
    2000 CONSTANT SIZE        ( =8192 DEZIMAL, PRIMZ. BEREICH )
DECIMAL
: PRIMES        ( --- ANZAHL DER PRIMZAHLEN )
        PRIMTAB SIZE 01 FILL    ( PRIMZ.FELD, ANF.BEDINGUNG )
        0                       ( PRIMZAHL ZéHLER )
        SIZE 2
        DO PRIMTAB I + C@       ( PRIMZAHL ? )
           IF I 91 <            ( NUR BIS WURZEL SIZE PRöFEN )
              IF I PRIMTAB SIZE +
                 PRIMTAB I I + + ( VON SIZE BIS 2I )
                 DO 0 I C! DUP +LOOP  ( ALLE VIELFACHE V.I 0EN )
              DROP ENDIF        ( I VOM +LOOP WEGTUN )
              1+                ( SCHLEIFENINDEX ERHôHEN )
        ENDIF LOOP ; ;S
\ PERIODTAB-1 f. Period.lÑnge v. Dez.brÅchen
\ fÅr 6809-fig-FORTH + Erweiterung, hier PICK
\ Wenn primzahlen markiert werden sollen, muû PRIMES gelaufen
\ sein, oder man muû die Markierung ausschalten, dann sind
\ Zahlen von 0 bis 32768 annehmbar.

0 VARIABLE REST1        ( 1.REST vor dem ZÑhlen )
0 VARIABLE PERZAHL      ( PeriodenzÑhler        )
0 VARIABLE NCR          ( ZeilenzÑhler          )

: TABKOPF CR CR ." Zahl   " 10 0 DO I 6 .R LOOP CR ;

: CR1 CR 1 NCR +! NCR @ 60 > IF 12 EMIT TABKOPF 0 NCR ! ENDIF ;
( CR1 gibt nach 60 Zeilen 'Neue Seite' und Tabellenkopf )
HEX  FF C0 C! DECIMAL   ( EMIT Ñndern fÅr Grafikzeichen )
-->
\ PERIODENTAB-2, PERIOD? NREST
: NREST 10 U* 3 PICK U/ DROP ;  ( n1 n2 --- n1 10n2/n1 )

: PERIOD?       ( Zahl --- periodisch? periodenlÑnge )
  10000 15 0 DO NREST LOOP              ( SicherheitslÑufe )
        0 PERZAHL ! DUP REST1 !         ( Anfangsbedingung )
  BEGIN 1 PERZAHL +! NREST DUP 0=
        IF ."   ---" 1                  ( Rest=0 Ausgabe , f )
        ELSE DUP REST1 @ = DUP       ( DUP als flag f. UNTIL )
             IF PERZAHL @ 5 .R       ( Ausgabe: PeriodenlÑnge )
             ENDIF
        ENDIF ?TERMINAL OR
  UNTIL
  DROP DROP ;

-->
\ PERIODTAB-3 f. Period.lÑnge v. Dez.brÅchen

: PRIMZ?      ( PRIMES muû gelaufen sein im Feld 1 = Primzahl )
   ( ZAHL )   PRIMTAB + C@ ;

: PERIODENTAB   ( von bis --- Tabelle )
  TABKOPF ( drucken )  0 NCR !
  1+ SWAP DO  CR1 I I 6 .R ."    "
                10 0 DO DUP I +
                        DUP PERIOD?     ( periodisch ?)
                            PRIMZ?      ( Primzahl ?)
                            IF 194 EMIT ( Pfeil )
                            ELSE SPACE ENDIF
                      LOOP DROP
          10 +LOOP CR1 ;
;S

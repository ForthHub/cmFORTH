\\   Sourcecode "Vierte Dimension" 1/88 S.19 ff

     Die Reparatur des Interrupt im NC4000
        von Klaus Schleisiek, Hamburg

     ( Ohne Gew„hr eingetippt von C. Krinninger )


     N„here Informationen bei:

     FORTH-Gesellschaft e.V.
     Postfach 1110
     D-8044 Unterschleižheim

     Tel: 089/317 37 84

\ System Variables for serial I/O via INT           ks 03 mar 88
\ Variables below h20, 2 cycle access
\ accessed on almost every interrupt
VARIABLE TRICKLE     \ counter for 10ms intervals
VARIABLE OUTPHASE    \ counter for int. transmit routine
VARIABLE OUTCHAR     \ register for int. transmit routine
VARIABLE INPHASE     \ counter for int. receive routine
VARIABLE INCHAR      \ register for int. receive routine

\ Variables above h20, 3 cycle access
\ initialized
VARIABLE #TRICKLE    \ constant for 10ms interval trickle ctr.
VARIABLE RXED        \ result register rx-serial
\ non initialized
VARIABLE DTIMER      \ 10ms 32-bit Timer
VARIABLE TIMER       \ 10ms 16-bit Timer                ( --> )
\ receive serial via interrupt                      ks 03 mar 88

HEX

{ : rxchar }  INPHASE @ DUP IF  1 - INPHASE ! EXIT  THEN
        INCHAR @ IF   INCHAR @ 2/
                Xport I@ 010 AND IF  0100 OR  THEN
                DUP 1 AND IF  RXED !  0  THEN  INCHAR !
                2 INPHASE !  DROP EXIT
        THEN  Xport I@ 010 AND IF  DROP EXIT  THEN
        0100 INCHAR !  3 INPHASE ! DROP ;

: KEY?  ( -- f )        PAUSE RXED @ 1 AND ;

: KEY   ( -- char )
        BEGIN  KEY? UNTIL  RXED @ 2/  RXED OFF ;        ( --> )
\ send serial via interrupt                         ks 03 mar 88

HEX

{ : txchar } OUTPHASE @ DUP IF 1 - OUTPHASE ! EXIT  THEN
        OUTCHAR @ IF  Xmask I@  01E Xmask I!
                OUTCHAR @       DUP Xport I!
                2/ OUTCHAR !  Xmask I!  2 OUTPHASE !
        THEN  DROP ;

: EMIT  ( char -- )
        OFF AND 0100 XOR 2*     \ 1 stop bit
        BEGIN  PAUSE  OUTCHAR @ WHILE REPEAT  OUTCHAR ! ;


                                                        ( --> )
\ 10ms Timer via interrupt                          ks 03 mar 88

HEX

{ : tick }
        TRICKLE @ DUP IF  1 - TRICKLE ! EXIT  THEN
        DTIMER  1 @+ @  1 + SWAP 0+c  DTIMER 1 ! !+ !
        #TRICKLE @ TRICKLE !  DROP ;

{ : interrupt }
        R> DROP R> 1 - >R rxchar txchar tick [ \\ ] ;

\ interrupt routine to be copied to locatione h20




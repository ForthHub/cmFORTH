                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
( FORTHkit  1987 December)                                      
( Optimizing compiler)  4 LOAD  5 LOAD  6 LOAD                  
: 0<   \ 0<  \ NOP ;                                            
: END   \ RECURSIVE  POP DROP ;                                 
: REMEMBER;   CONTEXT 2 - 2@ , ,  \ END ;                       
   FORTH                                                        
: -MOD ( n n - n)   4 I!  MOD' ; ( 3)                           
                                                                
: THRU ( n n)   OVER - FOR  DUP LOAD  1 + NEXT DROP ;           
: EMPTY   FORGET REMEMBER;                                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Separated heads)                                           
         VARIABLE H'  HEX 2000 , ( relocation)                  
         : {   dA @  HERE  H' 2@ H !  dA !  H' 2! ;   : }   { ; 
COMPILER : }   H' @ ,A \\  PREVIOUS  8000 XOR  SWAP !  { ;      
   FORTH : forget   SMUDGE ;                                    
         : RECOVER   -1 ALLOT ;                                 
                                                                
: SCAN ( a - a)   @ BEGIN  DUP 1 2000 WITHIN WHILE  @ REPEAT ;  
: TRIM ( a a - a)   DUP PUSH  dA @ -  SWAP !  POP               
   DUP 1 +  DUP @  DFFF AND  OVER !                             
   DUP @ 200 \ F AND +  DUP @ FF7F AND  SWAP ! ;                
: CLIP ( a)   DUP BEGIN  DUP SCAN  DUP WHILE  TRIM  REPEAT      
   2025 XOR  dA @ -  SWAP !  @ , ;                              
: PRUNE   { CONTEXT 2 -  DUP CLIP  1 + CLIP {                   
   20 0 2025 2!  EMPTY ;                                        
                                                                
( cmFORTH)   EMPTY                                              
( Target compiler)  2 LOAD                                      
HEX 2000 800 0 FILL   2000 H' !                                 
: BOOT }   16  FFF FOR  0 @+  1 !+ NEXT   -1 @ ( reset) ;       
   0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  0 ,  0 ( TIB) ,     
   77C0 , 0 , 0 , 0 , 0 , 0 ,  1FF ( S0) ,  A ( BASE) ,         
   0 ( H) ,  DECIMAL 521 ( 5MHz 9600b/s) ,                      
   { : interrupt }   POP DROP ;   0 , 0 , 1 ( CONTEXT) ,        
                                                                
( Nucleus)  : #   POP DROP ;   7 11 THRU                        
( Interpreter)  12 22 THRU                                      
( Initialize)  23 24 THRU   ' reset dA @ -  HEX 2009 !  DECIMAL 
( Compiler)  25 30 THRU   } PRUNE                               
                                                                
: GO   FLUSH  [ HEX ] 2015 4 I!  15  6EA FOR                    
      4 I@!  1 @+  4 I@!  1 !+  NEXT  2009 PUSH ;               
( Optimizing compiler)   OCTAL                                  
: FORTH   1 CONTEXT ! ;                                         
: COMPILER   2 CONTEXT ! ;                                      
: uCODE ( n)   CREATE ,  DOES   R> 77777 AND  @ ,C ;            
                                                                
COMPILER : \   2 -' IF  DROP ABORT" ?"  THEN ,A ;               
         : !-   172700 SHORT ;                                  
         : I@!   157700 SHORT ;                                 
         100000 uCODE NOP         140000 uCODE TWO              
         154600 uCODE 0+c         102404 uCODE MOD'             
         177300 uCODE N!          147303 uCODE -1               
   FORTH : DUP?   HERE 2 - @  100120 = IF                       
            HERE 1 - @  7100 XOR  -2 ALLOT  ,C  THEN ;          
COMPILER : I!   157200 SHORT  DUP? ;                            
         : PUSH   157201 ,C  DUP? ;                             
                                                                
   ( Defining Words)   OCTAL                                    
   FORTH : PACK ( a n - a)   160257 AND  140201 XOR IF          
            40 SWAP +!  ELSE DROP  100040 ,  THEN  POP DROP ;   
COMPILER : EXIT   ?CODE @  DUP IF  \\  DUP @  DUP 0< IF         
         DUP 170000 AND  100000 = IF  PACK THEN                 
         DUP 170300 AND  140300 = IF  PACK THEN                 
         DUP 170000 AND  150000 = IF                            
            DUP 170600 AND  150000 XOR IF  PACK THEN  THEN DROP 
      ELSE  DUP HERE dA @ - XOR  170000 AND 0= IF               
         7777 AND  130000 XOR  SWAP !  EXIT THEN DROP  THEN     
   THEN DROP  100040 , ;                                        
                                                                
         : ;   \ RECURSIVE  POP DROP  \ EXIT ;                  
   FORTH : CONSTANT ( n)   CREATE  -1 ALLOT  \ LITERAL \ EXIT ; 
                                                                
                                                                
   ( Binary operators)   OCTAL                                  
: BINARY ( n n)   CREATE , ,  DOES   POP 77777 AND  2@          
   ?CODE @ DUP IF  @  DUP 117100 AND  107100 =                  
      OVER 177700 AND  157500 = OR IF ( v -!)                   
         DUP 107020 - IF  SWAP DROP XOR  DUP 700 AND  200 = IF  
            500 XOR  ELSE DUP 70000 AND 0= IF  20 XOR  THEN THEN
            ?CODE @ !  EXIT THEN                                
  THEN THEN DROP  ,C  DROP ;                                    
: SHIFT ( n n)   CREATE , ,  DOES   POP 77777 AND  2@           
   ?CODE @ ?DUP IF  @ AND  100000 = WHILE  ?CODE @ +!  EXIT THEN
   DROP THEN  100000 XOR ,C ;                                   
COMPILER 7100 107020 BINARY DROP                                
         4100 103020 BINARY OR      2100 105020 BINARY XOR      
         6100 101020 BINARY AND     3100 104020 BINARY +        
         5100 106020 BINARY -       1100 102020 BINARY SWAP-    
2 171003 SHIFT 2*      1 171003 SHIFT 2/      3 177003 SHIFT 0< 
( Nucleus)   OCTAL                                              
: ROT ( n n n - n n n)   PUSH SWAP  POP SWAP ; ( 5)             
                                                                
: 0= ( n - t)   IF  0 EXIT  THEN -1 ; ( 3)                      
: NOT ( n - t)   0= ; ( 4)                                      
: < ( n n - t)   - 0< ; ( 3)                                    
: > ( n n - t)   SWAP- 0< ; ( 3)                                
: = ( n n - t)   XOR 0= ; ( 5)                                  
: U< ( u u - t)   - 2/  0< ; ( 3)                               
                                                                
{ COMPILER                                                      
104411 uCODE *'      102411 uCODE *-                            
100012 uCODE D2*     100011 uCODE D2/                           
102416 uCODE /'      102414 uCODE /''                           
( 102412 uCODE *F      102616 uCODE S')  FORTH }                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Multiply, divide)                                          
: M/MOD ( l h u - q r)   4 I!  D2*  13 TIMES  /'  /'' ; ( 21)   
: M/ ( l h u - q r)   OVER 0< IF  DUP PUSH +  POP THEN          
   M/MOD DROP ; ( 27-30)                                        
: VNEGATE ( v - v)   NEGATE SWAP  NEGATE SWAP ; ( 5)            
: M* ( n n - d)   DUP 0< IF  VNEGATE  THEN 0 SWAP               
   4 I!  13 TIMES  *'  *- ; ( 26-31)                            
                                                                
: /MOD ( u u - r q)   0 SWAP  M/MOD SWAP ; ( 25)                
: MOD ( u u - r)   /MOD DROP ; ( 27)                            
                                                                
: U*+ ( u n u - l h)   4 I!  14 TIMES  *' ; ( 20)               
: */ ( n n u - n)   PUSH M*  POP M/ ; ( 64)                     
: * ( n n - n)   0 SWAP U*+ DROP ; ( 24)                        
: / ( n u - q)   PUSH  DUP 0<  POP M/ ; ( 35)                   
                                                                
   ( Memory reference operators)                                
: 2/MOD ( n - r q)   DUP 1 AND  SWAP 0 [ \\ ] + 2/ ; ( 6)       
                                                                
: +! ( n a)   0 @+ PUSH  +  POP ! ; ( 9)                        
: C! ( n b)   2/MOD  DUP PUSH @  SWAP IF  -256 AND              
   ELSE  255 AND  SWAP 6 TIMES 2*  THEN XOR  POP ! ; ( 20-29)   
: C@ ( b - n)   2/MOD @  SWAP 1 - IF  6 TIMES 2/  THEN 255 AND ;
   ( 10-20)                                                     
: 2@ ( a - d)   1 @+  @ SWAP ; ( 6)                             
: 2! ( d a)   1 !+  ! ; ( 6)                                    
                                                                
{ OCTAL COMPILER : -ZERO   1 +  \ BEGIN  130000 , ;  FORTH }    
: MOVE ( s d n)   PUSH  4 I!  BEGIN -ZERO                       
      1 @+  4 I@!  1 !+  4 I@!  THEN NEXT DROP ; ( 7* 5+)       
: FILL ( a n n)   4 I!  FOR -ZERO  4 I@ SWAP 1 !+  THEN NEXT    
   DROP ; ( 5* 5+)                                              
   ( Words)                                                     
: EXECUTE ( a)   PUSH ; ( 3)                                    
: CYCLES ( n)   TIMES ; ( 4 n+)                                 
                                                                
: ?DUP ( n - n n, 0)   DUP IF  DUP EXIT  THEN ; ( 4)            
: 2DUP ( d - d d)   OVER OVER ; ( 3)                            
: 2DROP ( d)   DROP DROP ; ( 3)                                 
                                                                
: WITHIN ( n l h - t)   OVER - PUSH  - POP U< ;                 
: ABS ( n - u)   DUP 0< IF  NEGATE EXIT  THEN ; ( 4)            
                                                                
: MAX ( n n - n)   OVER OVER - 0< IF BEGIN  SWAP DROP ;         
: MIN ( n n - n)   OVER OVER - 0< UNTIL  THEN DROP ; ( 6)       
                                                                
                                                                
                                                                
   ( RAM allocation)   OCTAL                                    
{ : ARRAY ( n)   CONSTANT  154462 USE ;                         
HEX 10 CONSTANT PREV        ( Last referenced buffer)           
    11 CONSTANT OLDEST      ( oldest loaded buffer)             
    12 ARRAY BUFFERS        ( Block in each buffer)  }          
 2 1 - CONSTANT NB          ( Number of buffers)                
                                                                
{ 14 CONSTANT CYLINDER } 15 CONSTANT TIB                        
                                                                
( Initialized)                                                  
16 CONSTANT SPAN       17 CONSTANT >IN      { 18 CONSTANT BLK } 
19 CONSTANT dA                                                  
1A CONSTANT ?CODE      1B CONSTANT CURSOR                       
{ 1C CONSTANT S0 }     1D CONSTANT BASE       1E CONSTANT H     
1F CONSTANT C/B        24 CONSTANT CONTEXT                      
                                                                
( ASCII terminal: 4X in, 0X out)  HEX                           
: EMIT ( n)   1E D I!  2*  S0 @ XOR                             
   9 FOR  DUP C I!  2/  C/B @ A - CYCLES  NEXT DROP ;           
: CR   D EMIT  A EMIT ;                                         
: TYPE ( a - a)   2*  DUP C@ 1 - FOR  1 +  DUP C@ EMIT  NEXT    
   2 + 2/ ;                                                     
                                                                
{ : RX ( - n) }   0 I@  10 AND ; ( 3)                           
: KEY ( - n)   0  BEGIN RX  10 XOR UNTIL  C/B @  DUP 2/ +       
   7 FOR  10 - CYCLES  2/  RX  2* 2* 2* OR  C/B @  NEXT         
   BEGIN RX UNTIL  DROP ;                                       
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Serial EXPECT)   HEX                                       
: SPACE   20 EMIT ;                                             
: SPACES ( n)   0 MAX  FOR -ZERO  SPACE THEN NEXT ;             
: HOLD ( ..# x n - ..# x)   SWAP PUSH  SWAP 1 +  POP ;          
                                                                
: EXPECT ( a #)   SWAP CURSOR !                                 
   1 - DUP FOR  KEY  DUP 8 XOR IF                               
         DUP D XOR IF  DUP  CURSOR @ 1 !+  CURSOR !  EMIT       
         ELSE  SPACE  DROP  POP - SPAN !  EXIT THEN             
      ELSE ( 8)  DROP  DUP I XOR [ OVER ] UNTIL                 
         CURSOR @ 1 -  CURSOR !  POP 2 + PUSH  8 EMIT           
      THEN NEXT 1 + SPAN ! ;                                    
                                                                
                                                                
                                                                
                                                                
   ( Numbers)                                                   
: DIGIT ( n - n)   DUP 9 >  7 AND +  48 + ;                     
: <# ( n - ..# n)   -1 SWAP ;                                   
: #> ( ..# n)   DROP FOR  EMIT NEXT ;                           
: SIGN ( ..# n n - ..# n)   0< IF  45 HOLD  THEN ;              
: # ( ..# n - ..# n)   BASE @ /MOD  SWAP DIGIT HOLD ;           
: #S ( ..# n - ..# 0)   BEGIN  #  DUP 0= UNTIL ;                
: (.) ( n - ..# n)   DUP PUSH ABS  <# #S  POP SIGN ;            
: . ( n)   (.) #> SPACE ;                                       
                                                                
: U.R ( u n)   PUSH  <# #S  OVER POP SWAP-  1 - SPACES  #> ;    
: U. ( u)   0 U.R  SPACE ;                                      
: DUMP ( a - a)   CR  DUP 5 U.R SPACE  7 FOR                    
      1 @+ SWAP  7 U.R  NEXT SPACE ;                            
                                                                
                                                                
   ( Strings)   HEX                                             
: HERE ( - a)   H @ ;                                           
                                                                
{ : abort" }   H @ TYPE  SPACE  POP 7FFF AND TYPE  2DROP        
   BLK @  ?DUP DROP  0 ( QUIT) ;                                
{ : dot" }   POP 7FFF AND TYPE  PUSH ;                          
                                                                
{ COMPILER : ABORT"   COMPILE abort"  22 STRING ;               
           : ."   COMPILE dot"  22 STRING ;                     
     FORTH }                                                    
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( 15-bit buffer manager)                                     
{ : ADDRESS ( n - a) }   30 + 8 TIMES 2* ;                      
{ : ABSENT ( n - n) }   NB FOR  DUP  I BUFFERS @ XOR  2* WHILE  
     NEXT EXIT THEN POP  PREV N!  POP DROP SWAP DROP  ADDRESS ; 
                                                                
{ : UPDATED ( - a n) }   OLDEST @  BEGIN  1 +  NB AND           
      DUP PREV @ XOR UNTIL  OLDEST N!  PREV N!                  
   DUP ADDRESS  SWAP BUFFERS  DUP @                             
   8192 ROT !  DUP 0< NOT IF  POP DROP DROP THEN ;              
                                                                
: UPDATE   PREV @ BUFFERS  0 @+ SWAP  32768 OR  SWAP ! ;        
{ : ESTABLISH ( n a - a) }   SWAP  OLDEST @  PREV N!            
   BUFFERS ! ;                                                  
: IDENTIFY ( n a - a)   SWAP  PREV @ BUFFERS ! ;                
                                                                
                                                                
   ( Disk read/write)                                           
{ : ## ( a n - a a #) }   0 EMIT  256 /MOD EMIT EMIT  DUP 1023 ;
                                                                
{ : buffer ( n - a) }   UPDATED                                 
   ## FOR  1 @+ SWAP  EMIT  NEXT  KEY 2DROP ;                   
: BUFFER ( n - a)   buffer ESTABLISH ;                          
                                                                
{ : block ( n a - n a) }  OVER  ## FOR  KEY  SWAP ]  !+         
      NEXT DROP ;                                               
: BLOCK ( n - a)   ABSENT buffer  block ESTABLISH ;             
                                                                
: FLUSH   NB FOR  8192 BUFFER DROP  NEXT ;                      
: EMPTY-BUFFERS   PREV [ NB 3 + ] LITERAL 0 FILL  FLUSH ;       
                                                                
                                                                
                                                                
( Interpreter)                                                  
{ : LETTER ( b a # - b a) }   FOR  DUP @  6 I@ XOR  WHILE       
         1 @+ PUSH  OVER C!  1 +  POP NEXT  EXIT THEN           
   >IN @  POP -  >IN ! ;                                        
{ : -LETTER ( b a # - b a) }  ?DUP IF                           
      1 - FOR  1 @+ SWAP  6 I@ XOR  0= WHILE NEXT  EXIT THEN    
      1 -  POP LETTER  THEN ;                                   
: WORD ( n - a)   PUSH  H @  DUP 2*  DUP 1 +  DUP  >IN @        
   BLK @ IF  BLK @ BLOCK + 1024  ELSE  TIB @ +  SPAN @  THEN    
   >IN @  OVER >IN !  -  POP 6 I!                               
   -LETTER DROP  32 OVER C!  SWAP-  SWAP C! ;                   
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Dictionary search)                                         
{ : SAME ( h a - h a f, a t) }   OVER 4 I!  DUP 1 +             
   6 I@ FOR  1 @+ SWAP  4 I@ 1 @+ 4 I!  - 2* IF                 
         POP DROP  0 AND  EXIT THEN                             
      NEXT  SWAP 1 + @  0< IF  @  THEN SWAP ;                   
                                                                
{ : COUNT ( n - n) }   7 TIMES 2/  15 AND ;                     
{ : HASH ( n - a) }   CONTEXT SWAP- ;                           
{ : -FIND ( h n - h t, a f) }   HASH  OVER @ COUNT  6 I!        
   BEGIN  @ DUP WHILE  SAME UNTIL  0 EXIT THEN  -1 XOR ;        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Number input)   HEX                                        
: -DIGIT ( n - n)   30 -  DUP 9 > IF  7 -  DUP A < OR  THEN     
   DUP BASE @ U< IF  EXIT THEN                                  
   2DROP  ABORT" ?" ;  RECOVER                                  
                                                                
{ : C@+ ( - n) }   6 I@ 1 +  DUP 6 I!  C@ ;                     
{ : 10*+ ( u n - u) }   -DIGIT  0E TIMES *'  DROP ;             
: NUMBER ( a - n)   BASE @ 4 I!  0  SWAP 2*  DUP 1 + C@  2D =   
   PUSH  DUP I - 6 I!  C@ I +  1 - FOR  C@+ 10*+  NEXT          
   POP IF NEGATE  THEN ;                                        
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Control)                                                   
: -' ( n - h t, a f)   32 WORD  SWAP -FIND ;                    
: ' ( - a)   CONTEXT @ -' IF  DROP ABORT" ?"  THEN ;  forget    
                                                                
: INTERPRET ( n n)   >IN 2!  BEGIN  1 -' IF  NUMBER             
      ELSE  EXECUTE  THEN AGAIN ;  RECOVER                      
                                                                
: QUIT   BEGIN CR  TIB @ 64 EXPECT                              
      0 0 INTERPRET ." ok"  AGAIN ;  RECOVER                    
                                                                
' QUIT  dA @ -  ' abort" 11 + !                                 
                                                                
                                                                
                                                                
                                                                
                                                                
   ( Initialize)   HEX                                          
: FORGET ( a)   POP 7FFF AND  DUP 2 + H !  2@ CONTEXT 2 - 2!    
   1 CONTEXT ! ;                                                
                                                                
{ : BPS }   4 BEGIN RX  10 XOR UNTIL  BEGIN 5 +  RX UNTIL       
   2/ C/B ! ;                                                   
{ : RS232 }   RX IF  EXIT THEN  200 S0 !  0B 0 I! ;             
                                                                
{ : reset }   0 ( RESET)                                        
   0 DUP 9 I!  DUP A I!  DUP 0B I!  DUP 8 I!  -1 A I!           
   DUP D I!  DUP E I!  F I!  1A C I!                            
   TIB 2@ XOR IF  EMPTY-BUFFERS  SPAN @ TIB !  THEN             
   RS232  F E I!  BPS  ." hi" QUIT ;                            
                                                                
                                                                
                                                                
   ( Words)                                                     
: SWAP   SWAP ;        : OVER   OVER ;                          
: DUP   DUP ;          : DROP   DROP ;                          
                                                                
: XOR   XOR ;          : AND   AND ;                            
: OR   OR ;                                                     
: +   + ;              : -   - ;                                
: 0<   0< ;            : NEGATE   NEGATE ;                      
                                                                
: @   @ ;              : !   ! ;                                
                                                                
: OCTAL   8 BASE ! ;  forget                                    
: DECIMAL   10 BASE ! ;  forget                                 
: HEX   16 BASE ! ;  forget                                     
: LOAD ( n)   >IN 2@ PUSH PUSH  0 INTERPRET  10 BASE !          
   POP POP >IN 2! ;   forget                                    
( Compiler)   OCTAL                                             
: \\   0 ?CODE ! ;                                              
: ALLOT ( n)   H +!  \\ ;                                       
: , ( n)   H @ !  1 H +! ;                                      
: ,C ( n)   H @ ?CODE !  , ;                                    
: ,A ( a)   dA @ -  ,C ;                                        
COMPILER : LITERAL ( n)   DUP -40 AND IF  147500 ,C  ,  EXIT    
   THEN  157500 XOR ,C ;                                        
: [   POP DROP ;                                                
                                                                
FORTH : ]   BEGIN 2 -'  IF  1 -FIND IF  NUMBER  \ LITERAL       
   ELSE DUP @                                                   
      DUP 140040 AND  140040 =  OVER 170377 AND  140342 XOR AND 
      SWAP 170040 AND  100040 = OR IF  @ 40 XOR  ,C             
      ELSE  ,A  THEN THEN                                       
   ELSE  EXECUTE  THEN AGAIN ;  RECOVER                         
   ( Compiler)   HEX                                            
: PREVIOUS ( - a n)   CONTEXT @ HASH  @ 1 +  0 @+ SWAP ;        
: USE ( a)   PREVIOUS  COUNT + 1 + ! ;                          
: DOES   POP 7FFF AND  USE ;                                    
: SMUDGE   PREVIOUS 2000 XOR  SWAP ! ;                          
: EXIT   POP DROP ;                                             
                                                                
: COMPILE   POP 7FFF AND  1 @+ PUSH  ,A ;                       
OCTAL                                                           
COMPILER : EXIT   100040 ,C ;   HEX                             
         : RECURSIVE   PREVIOUS DFFF AND  SWAP ! ;              
         : ;   \ RECURSIVE  POP DROP  \ EXIT ;  forget          
                                                                
                                                                
                                                                
                                                                
   ( Defining words)   OCTAL                                    
FORTH : CREATE   H @ 0 ,  40 WORD  CONTEXT @ HASH               
   2DUP @  SWAP 1 - !  SWAP @  COUNT 1 + ALLOT  !  147342 , ;   
                                                                
: :   CREATE  -1 ALLOT  SMUDGE  ] ;  forget                     
                                                                
: CONSTANT ( n)   CREATE  -1 ALLOT  \ LITERAL  \ EXIT ;         
: VARIABLE   CREATE  0 , ;                                      
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                
   ( uCODE)   OCTAL                                             
: -SHORT ( - t)   ?CODE @ @  177700 AND  157500 XOR ;           
: FIX ( n)   ?CODE @ @  77 AND OR  ?CODE @ ! ;                  
: SHORT ( n)   -SHORT IF  DROP ABORT" n?"  THEN FIX ;           
                                                                
COMPILER                                                        
: @   -SHORT IF  167100 ,C  ELSE  147100 FIX  THEN ;  forget    
: !   -SHORT IF  177000 ,C  ELSE  157000 FIX  THEN ;  forget    
: I@   147300 SHORT ;                                           
: I!   157200 SHORT ;                                           
: @+   164700 SHORT ;                                           
: !+   174700 SHORT ;                                           
                                                                
: R>   147321 ,C ;                                              
: POP   147321 ,C ;    : PUSH   157201 ,C ;                     
: I   147301 ,C ;      : TIMES   157221 ,C ;  forget            
   ( Structures)   OCTAL                                        
 FORTH { : OR, ( n n) }   \\  SWAP 7777 AND  OR , ;             
COMPILER : BEGIN ( - a)   H @  \\ ;                             
                                                                
: UNTIL ( a)   110000 OR, ;                                     
: AGAIN ( a)   130000 OR, ;                                     
: THEN ( a)   \ BEGIN  7777 AND  SWAP +! ;                      
: IF ( - a)   \ BEGIN  110000 , ;                               
: WHILE ( a - a a)   \ IF  SWAP ;                               
: REPEAT ( a a)   \ AGAIN  \ THEN ;                             
: ELSE ( a - a)   \ BEGIN  130000 ,  SWAP \ THEN ;              
                                                                
: FOR ( - a)   \ PUSH  \ BEGIN ;                                
: NEXT ( a)   120000 OR, ;                                      
                                                                
                                                                
   ( Strings)   HEX                                             
   FORTH : STRING ( n)   WORD @  7 TIMES 2/  1 + ALLOT ;        
                                                                
COMPILER : ABORT"   COMPILE abort"  22 STRING ;                 
         : ."   COMPILE dot"  22 STRING ;                       
         : (   29 WORD DROP ;                                   
                                                                
   FORTH : (   \ ( ;                                            
                                                                
: RESET   FORGET 0 ;  RECOVER   ' RESET dA @ -  ' reset !       
                                                                
                                                                
                                                                
                                                                
                                                                
                                                                

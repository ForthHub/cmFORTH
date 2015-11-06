                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
( FORTHkit  1987 December)                                       cmFORTH shadow blocks (1987 December).  Addresses are hex;      
( Optimizing compiler)  4 LOAD  5 LOAD  6 LOAD                      word timing in parentheses after  ; ( cycles) .              
: 0<   \ 0<  \ NOP ;                                                                                                             
: END   \ RECURSIVE  POP DROP ;                                  1 LOAD  compiles the compacting compiler (blocks 4-6).  Block 6 
: REMEMBER;   CONTEXT 2 - 2@ , ,  \ END ;                           exits in  COMPILER  vocabulary, anticipating additions.      
   FORTH                                                         0<  is redefined to resolve timing conflict.                    
: -MOD ( n n - n)   4 I!  MOD' ; ( 3)                            END  terminates a definition.                                   
                                                                 REMEMBER;  saves vocabulary heads (at compile time).            
: THRU ( n n)   OVER - FOR  DUP LOAD  1 + NEXT DROP ;                                                                            
: EMPTY   FORGET REMEMBER;                                       FORTH  puts following words in interpretive vocabulary.         
                                                                 -MOD  provides modular arithmetic.  It does a subtract if the   
                                                                    result is non-negative.                                      
                                                                                                                                 
                                                                 THRU  loads a sequence of blocks.                               
                                                                 EMPTY  empties the dictionary except for compacting compiler.   
                                                                                                                                 
   ( Separated heads)                                            H'  holds the next available address in the target dictionary.  
         VARIABLE H'  HEX 2000 , ( relocation)                   2000 relocates target addresses from RAM (2000) to PROM (0).    
         : {   dA @  HERE  H' 2@ H !  dA !  H' 2! ;   : }   { ;  {  }   switches between host and target dictionary by exchanging
COMPILER : }   H' @ ,A \\  PREVIOUS  8000 XOR  SWAP !  { ;          pointers and relocation offsets.                             
   FORTH : forget   SMUDGE ;                                     COMPILER  }  compiles an indirect reference for a headless word.
         : RECOVER   -1 ALLOT ;                                  forget  smudges a word that cannot execute in target dictionary.
                                                                                                                                 
: SCAN ( a - a)   @ BEGIN  DUP 1 2000 WITHIN WHILE  @ REPEAT ;   RECOVER  recovers a return (after  AGAIN ).                     
: TRIM ( a a - a)   DUP PUSH  dA @ -  SWAP !  POP                                                                                
   DUP 1 +  DUP @  DFFF AND  OVER !                              SCAN  finds the next word in target dictionary.                 
   DUP @ 200 \ F AND +  DUP @ FF7F AND  SWAP ! ;                 TRIM  relocates the vocabulary link and erases the smudge bit.  
: CLIP ( a)   DUP BEGIN  DUP SCAN  DUP WHILE  TRIM  REPEAT       CLIP  constructs a target vocabulary and stores its head.       
   2025 XOR  dA @ -  SWAP !  @ , ;                               PRUNE  relinks the target dictionary to produce a stand-alone   
: PRUNE   { CONTEXT 2 -  DUP CLIP  1 + CLIP {                      application (fixing the end-of-vocabulary word)               
   20 0 2025 2!  EMPTY ;                                           and restores the host dictionary.                             
                                                                                                                                 
( cmFORTH)   EMPTY                                               3 LOAD  recompiles cmFORTH.  EMPTY  clears dictionary for a new 
( Target compiler)  2 LOAD                                          application.                                                 
HEX 2000 800 0 FILL   2000 H' !                                  2 LOAD  compiles the target compiler.                           
: BOOT }   16  FFF FOR  0 @+  1 !+ NEXT   -1 @ ( reset) ;        Target is compiled at 2000 which is initialized to 0.           
   0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,  0 ,  0 ( TIB) ,      BOOT  copies PROM to RAM at power-up.  The reference to -1      
   77C0 , 0 , 0 , 0 , 0 , 0 ,  1FF ( S0) ,  A ( BASE) ,             disables PROM and enables RAM (setting A15 clocks 74).       
   0 ( H) ,  DECIMAL 521 ( 5MHz 9600b/s) ,                       Low RAM (16-24) is initialized (see block 12).                  
   { : interrupt }   POP DROP ;   0 , 0 , 1 ( CONTEXT) ,                                                                         
                                                                 #  is the bottom of the target dictionary.  PRUNE  changes its  
( Nucleus)  : #   POP DROP ;   7 11 THRU                            name to null and link to 0.  This version of  EXIT  marks the
( Interpreter)  12 22 THRU                                          end of both vocabulary chains.                               
( Initialize)  23 24 THRU   ' reset dA @ -  HEX 2009 !  DECIMAL  The address of  RESET  is relocated into the end of  BOOT .     
( Compiler)  25 30 THRU   } PRUNE                                The end of target program is stored into  TIB  and  HERE .      
                                                                    COMPILER  head is selected for  PRUNE .                      
: GO   FLUSH  [ HEX ] 2015 4 I!  15  6EA FOR                     GO  emulates  BOOT  for testing:  3 LOAD  GO                    
      4 I@!  1 @+  4 I@!  1 !+  NEXT  2009 PUSH ;                                                                                
( Optimizing compiler)   OCTAL                                   FORTH  sets interpretive vocabulary for both searches and       
: FORTH   1 CONTEXT ! ;                                            definitions.  Words are compiled in definitions.              
: COMPILER   2 CONTEXT ! ;                                       COMPILER  sets immediate vocabulary.  Words are executed in  : .
: uCODE ( n)   CREATE ,  DOES   R> 77777 AND  @ ,C ;             uCODE  names a NC4016 micro-coded instruction.  Compiled on use.
                                                                 \  compiles a following compiler directive (that would normally 
COMPILER : \   2 -' IF  DROP ABORT" ?"  THEN ,A ;                   be executed).  Named  [COMPILE]  in FORTH-83.                
         : !-   172700 SHORT ;                                                                                                   
         : I@!   157700 SHORT ;                                  4016 instructions:                                              
         100000 uCODE NOP         140000 uCODE TWO                  !-   stores and decrements.  I@!  exchanges stack&register.  
         154600 uCODE 0+c         102404 uCODE MOD'                 NOP  delays 1 cycle.         TWO  delays 2 cycles.           
         177300 uCODE N!          147303 uCODE -1                   0+c  Adds 0 with carry.     MOD'  conditionally subtracts R4.
   FORTH : DUP?   HERE 2 - @  100120 = IF                           N!   stores and saves data.   -1  fetches register 3         
            HERE 1 - @  7100 XOR  -2 ALLOT  ,C  THEN ;                                                                           
COMPILER : I!   157200 SHORT  DUP? ;                             DUP?  compacts preceeding  DUP  with current instruction.  Used 
         : PUSH   157201 ,C  DUP? ;                                 to redefine  I!  and  PUSH (previously  >R ).                
                                                                                                                                 
   ( Defining Words)   OCTAL                                     PACK  sets the return bit, if an instruction does not reference 
   FORTH : PACK ( a n - a)   160257 AND  140201 XOR IF             the Return stack.  Otherwise it compiles a return.  It exits  
            40 SWAP +!  ELSE DROP  100040 ,  THEN  POP DROP ;      from  EXIT  with  POP DROP .                                  
COMPILER : EXIT   ?CODE @  DUP IF  \\  DUP @  DUP 0< IF                                                                          
         DUP 170000 AND  100000 = IF  PACK THEN                  EXIT  optimizes return if permitted ( ?CODE  nonzero):          
         DUP 170300 AND  140300 = IF  PACK THEN                     For instructions (bit-15 = 1) it calls  PACK  except for jump
         DUP 170000 AND  150000 = IF                                   or 2-cycle instructions;                                  
            DUP 170600 AND  150000 XOR IF  PACK THEN  THEN DROP     for calls to the same 4K page, it substitutes a jump.        
      ELSE  DUP HERE dA @ - XOR  170000 AND 0= IF                                                                                
         7777 AND  130000 XOR  SWAP !  EXIT THEN DROP  THEN      ;  is redefined to use the new  EXIT .                          
   THEN DROP  100040 , ;                                                                                                         
                                                                 CONSTANT  is redefined to take advantage of the new  EXIT  for  
         : ;   \ RECURSIVE  POP DROP  \ EXIT ;                      5-bit literals.                                              
   FORTH : CONSTANT ( n)   CREATE  -1 ALLOT  \ LITERAL \ EXIT ;                                                                  
                                                                                                                                 
                                                                                                                                 
   ( Binary operators)   OCTAL                                   BINARY  defines and compacts ALU instructions.  If the previous 
: BINARY ( n n)   CREATE , ,  DOES   POP 77777 AND  2@              instruction was a fetch (ALU code 7) and not a store or  DROP
   ?CODE @ DUP IF  @  DUP 117100 AND  107100 =                      the ALU code is merged;  stack push is inhibited.  Otherwise 
      OVER 177700 AND  157500 = OR IF ( v -!)                       a new instruction is compiled.  ?CODE  holds address of      
         DUP 107020 - IF  SWAP DROP XOR  DUP 700 AND  200 = IF      candidate for compaction.                                    
            500 XOR  ELSE DUP 70000 AND 0= IF  20 XOR  THEN THEN                                                                 
            ?CODE @ !  EXIT THEN                                 SHIFT  defines and compacts shift instructions.  Shift left     
  THEN THEN DROP  ,C  DROP ;                                        ( 2* ) and right ( 2/) may be merged with an arithmetic      
: SHIFT ( n n)   CREATE , ,  DOES   POP 77777 AND  2@               instruction;  sign propagate ( 0< ) only with  DUP .         
   ?CODE @ ?DUP IF  @ AND  100000 = WHILE  ?CODE @ +!  EXIT THEN                                                                 
   DROP THEN  100000 XOR ,C ;                                    DROP  OR  XOR  AND  +  -  SWAP-  are redefined.                 
COMPILER 7100 107020 BINARY DROP                                 2*  2/  0<  likewise.                                           
         4100 103020 BINARY OR      2100 105020 BINARY XOR                                                                       
         6100 101020 BINARY AND     3100 104020 BINARY +                                                                         
         5100 106020 BINARY -       1100 102020 BINARY SWAP-                                                                     
2 171003 SHIFT 2*      1 171003 SHIFT 2/      3 177003 SHIFT 0<                                                                  
( Nucleus)   OCTAL                                               ROT  is a slow way to reference into the stack.                 
: ROT ( n n n - n n n)   PUSH SWAP  POP SWAP ; ( 5)                                                                              
                                                                 0=  returns false (0) if stack non-zero; otherwise true ( -1).  
: 0= ( n - t)   IF  0 EXIT  THEN -1 ; ( 3)                       NOT  same as 0=.  FORTH-83 wants one's complement.              
: NOT ( n - t)   0= ; ( 4)                                       <  >  subtract and test sign bit.  Range of difference limited  
: < ( n n - t)   - 0< ; ( 3)                                        to 15 bits (-20000 is not less-than 20000).                  
: > ( n n - t)   SWAP- 0< ; ( 3)                                 =  equality tested by XOR.                                      
: = ( n n - t)   XOR 0= ; ( 5)                                   U<  unsigned compare with 16-bit range (0 is less-than 40000).  
: U< ( u u - t)   - 2/  0< ; ( 3)                                                                                                
                                                                 { ... }  surround words defined into host dictionary.  Used     
{ COMPILER                                                          during compilation, they will not be in target dictionary.   
104411 uCODE *'      102411 uCODE *-                             4016 instructions:                                              
100012 uCODE D2*     100011 uCODE D2/                                *'  multiply step         *-  signed multiply step          
102416 uCODE /'      102414 uCODE /''                               D2*  32-bit left shift    D2/  32-bit right shift            
( 102412 uCODE *F      102616 uCODE S')  FORTH }                     /'  divide step          /''  final divide step             
                                                                     F*  fraction multiply     S'  square-root step              
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Multiply, divide)                                           M/MOD  30-bit dividend; 15-bit divisor, quotient, remainder.    
: M/MOD ( l h u - q r)   4 I!  D2*  13 TIMES  /'  /'' ; ( 21)    M/  signed dividend; 15-bit divisor, quotient.                  
: M/ ( l h u - q r)   OVER 0< IF  DUP PUSH +  POP THEN           VNEGATE  negates both multiplier and multiplicand.              
   M/MOD DROP ; ( 27-30)                                         M*  32-bit signed product; multiplier (on top) must be even.    
: VNEGATE ( v - v)   NEGATE SWAP  NEGATE SWAP ; ( 5)                                                                             
: M* ( n n - d)   DUP 0< IF  VNEGATE  THEN 0 SWAP                /MOD  15-bit dividend, divisor, quotient, remainder.            
   4 I!  13 TIMES  *'  *- ; ( 26-31)                             MOD  15-bit dividend, divisor, remainder.                       
                                                                                                                                 
: /MOD ( u u - r q)   0 SWAP  M/MOD SWAP ; ( 25)                 U*+  15-bit multiplier, multiplicand, addend; 30-bit product.   
: MOD ( u u - r)   /MOD DROP ; ( 27)                             */  signed multiplier, multiplicand, result; 15-bit divisor;    
                                                                    multiplier (in middle) must be even.                         
: U*+ ( u n u - l h)   4 I!  14 TIMES  *' ; ( 20)                *  signed product; multiplier (on top) must be even.            
: */ ( n n u - n)   PUSH M*  POP M/ ; ( 64)                      /  signed dividend, quotient; 15-bit divisor.                   
: * ( n n - n)   0 SWAP U*+ DROP ; ( 24)                                                                                         
: / ( n u - q)   PUSH  DUP 0<  POP M/ ; ( 35)                                                                                    
                                                                                                                                 
   ( Memory reference operators)                                 2/MOD  16-bit unsigned dividend; 15-bit quotient, remainder.    
: 2/MOD ( n - r q)   DUP 1 AND  SWAP 0 [ \\ ] + 2/ ; ( 6)           \\ (break compaction) used to combine  + 2/ ;                
                                                                 +!  adds to memory.                                             
: +! ( n a)   0 @+ PUSH  +  POP ! ; ( 9)                         Byte address is 2* cell address;  high byte is byte 0.  Range   
: C! ( n b)   2/MOD  DUP PUSH @  SWAP IF  -256 AND                  restricted to low RAM (0-7FFF).                              
   ELSE  255 AND  SWAP 6 TIMES 2*  THEN XOR  POP ! ; ( 20-29)    C!  stores 8-bit data into byte address; other byte unaffected. 
: C@ ( b - n)   2/MOD @  SWAP 1 - IF  6 TIMES 2/  THEN 255 AND ; C@  fetches 8-bits from byte address.                           
   ( 10-20)                                                                                                                      
: 2@ ( a - d)   1 @+  @ SWAP ; ( 6)                              2@  fetches 2 16-bit numbers;  lower address on top.            
: 2! ( d a)   1 !+  ! ; ( 6)                                     2!  stores 2 16-bit numbers.                                    
                                                                                                                                 
{ OCTAL COMPILER : -ZERO   1 +  \ BEGIN  130000 , ;  FORTH }     MOVE  the fastest move that does not stream to-from stack.      
: MOVE ( s d n)   PUSH  4 I!  BEGIN -ZERO                        FILL  fills RAM with constant.                                  
      1 @+  4 I@!  1 !+  4 I@!  THEN NEXT DROP ; ( 7* 5+)                                                                        
: FILL ( a n n)   4 I!  FOR -ZERO  4 I@ SWAP 1 !+  THEN NEXT                                                                     
   DROP ; ( 5* 5+)                                                                                                               
   ( Words)                                                      EXECUTE  executes code at an address by returning to it.        
: EXECUTE ( a)   PUSH ; ( 3)                                     CYCLES  delays n+4 cycles - count 'em.                          
: CYCLES ( n)   TIMES ; ( 4 n+)                                                                                                  
                                                                 ?DUP  copies stack if non-zero.                                 
: ?DUP ( n - n n, 0)   DUP IF  DUP EXIT  THEN ; ( 4)             2DUP  copies 32-bit (2 16-bit) stack item.                      
: 2DUP ( d - d d)   OVER OVER ; ( 3)                             2DROP  DROP DROP ;  is faster and usually no longer.            
: 2DROP ( d)   DROP DROP ; ( 3)                                                                                                  
                                                                 WITHIN  returns true if number within low (inclusive) and high  
: WITHIN ( n l h - t)   OVER - PUSH  - POP U< ;                     (non-inclusive) limits;  all numbers 16 bits or signed.      
: ABS ( n - u)   DUP 0< IF  NEGATE EXIT  THEN ; ( 4)             ABS  returns positive number (15-bits).                         
                                                                                                                                 
: MAX ( n n - n)   OVER OVER - 0< IF BEGIN  SWAP DROP ;          MAX  returns larger of pair; 15-bit range.                      
: MIN ( n n - n)   OVER OVER - 0< UNTIL  THEN DROP ; ( 6)        MIN  returns smaller.  Intertwining code saves 2 cells; left in 
                                                                    as illustration of obscure but efficient code.               
                                                                                                                                 
                                                                                                                                 
   ( RAM allocation)   OCTAL                                     ARRAY  defines an array that adds an index from stack in only   
{ : ARRAY ( n)   CONSTANT  154462 USE ;                             2 cycles.  Similar to  VARIABLE .                            
HEX 10 CONSTANT PREV        ( Last referenced buffer)                                                                            
    11 CONSTANT OLDEST      ( oldest loaded buffer)              These low-RAM variables are used by cmFORTH (0-F are unused).   
    12 ARRAY BUFFERS        ( Block in each buffer)  }              Change them cautiously!  In particular, make sure a variable 
 2 1 - CONSTANT NB          ( Number of buffers)                    is not used during compilation.  For example,  HEX  is       
                                                                    redefined to set  BASE .  It can be used if  BASE  has not   
{ 14 CONSTANT CYLINDER } 15 CONSTANT TIB                            moved;  otherwise it must be forgetted.                      
                                                                 Non-standard variables:                                         
( Initialized)                                                      ?CODE  address of last instruction compiled.  Zero indicates 
16 CONSTANT SPAN       17 CONSTANT >IN      { 18 CONSTANT BLK }        no compaction permitted (ie, after  THEN ).               
19 CONSTANT dA                                                      dA  offset to be added to compiled addresses.  Normally 0.   
1A CONSTANT ?CODE      1B CONSTANT CURSOR                              Relocated code cannot be executed!                        
{ 1C CONSTANT S0 }     1D CONSTANT BASE       1E CONSTANT H         CURSOR  tracks cursor (terminal dependent); used by  EXPECT .
1F CONSTANT C/B        24 CONSTANT CONTEXT                          S0  serial output polarity; 1FF or 200.                      
                                                                    C/B  cycles/bit for serial I/O.                              
( ASCII terminal: 4X in, 0X out)  HEX                            EMIT  sets Xmask to 1E so that only X0 can be changed.  Start/  
: EMIT ( n)   1E D I!  2*  S0 @ XOR                                 stop bits are added and polarity set.  I!  emits bits at C/B 
   9 FOR  DUP C I!  2/  C/B @ A - CYCLES  NEXT DROP ;               rate thru X0.                                                
: CR   D EMIT  A EMIT ;                                          CR  emits carriage-return and line-feed.                        
: TYPE ( a - a)   2*  DUP C@ 1 - FOR  1 +  DUP C@ EMIT  NEXT                                                                     
   2 + 2/ ;                                                      TYPE  types a string with prefixed count byte.  It returns an   
                                                                    incremented cell address.  This is not FORTH-83 standard.    
{ : RX ( - n) }   0 I@  10 AND ; ( 3)                                                                                            
: KEY ( - n)   0  BEGIN RX  10 XOR UNTIL  C/B @  DUP 2/ +        RX  reads a bit from pin X4.                                    
   7 FOR  10 - CYCLES  2/  RX  2* 2* 2* OR  C/B @  NEXT          KEY  reads 8-bits from X4.  It waits for a start bit, then      
   BEGIN RX UNTIL  DROP ;                                           delays until the middle of the first data bit.  Each bit is  
                                                                    sampled then ored into bit 7 of the accumulated byte.  It    
                                                                    does not exit until the stop bit (low) is detected.          
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Serial EXPECT)   HEX                                        SPACE  emits a space.                                           
: SPACE   20 EMIT ;                                              SPACES  emits  n>0  spaces.                                     
: SPACES ( n)   0 MAX  FOR -ZERO  SPACE THEN NEXT ;              HOLD  holds characters on the stack, maintaining a count.       
: HOLD ( ..# x n - ..# x)   SWAP PUSH  SWAP 1 +  POP ;              It reverses the digits resulting from number conversion.     
                                                                                                                                 
: EXPECT ( a #)   SWAP CURSOR !                                  EXPECT  accepts keystrokes and buffers them (at  TIB ).  An 8   
   1 - DUP FOR  KEY  DUP 8 XOR IF                                   will discard a character and emit a backspace;  a D will     
         DUP D XOR IF  DUP  CURSOR @ 1 !+  CURSOR !  EMIT           emit a space and exit; all other keys are stored and echoed  
         ELSE  SPACE  DROP  POP - SPAN !  EXIT THEN                 until the count is exhausted.  Actual count is in  SPAN .    
      ELSE ( 8)  DROP  DUP I XOR [ OVER ] UNTIL                                                                                  
         CURSOR @ 1 -  CURSOR !  POP 2 + PUSH  8 EMIT                                                                            
      THEN NEXT 1 + SPAN ! ;                                                                                                     
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Numbers)                                                    DIGIT  converts a digit (0-F) into an ASCII character.          
: DIGIT ( n - n)   DUP 9 >  7 AND +  48 + ;                      <#  starts conversion by tucking a count under the number.      
: <# ( n - ..# n)   -1 SWAP ;                                    #>  ends conversion by emitting the string of digits.           
: #> ( ..# n)   DROP FOR  EMIT NEXT ;                            SIGN  stacks a minus sign, if needed.                           
: SIGN ( ..# n n - ..# n)   0< IF  45 HOLD  THEN ;               #  converts the low-order digit of a 16-bit number.             
: # ( ..# n - ..# n)   BASE @ /MOD  SWAP DIGIT HOLD ;            #S  converts non-zero digits, at least one.                     
: #S ( ..# n - ..# 0)   BEGIN  #  DUP 0= UNTIL ;                 (.)  formats a signed number.                                   
: (.) ( n - ..# n)   DUP PUSH ABS  <# #S  POP SIGN ;             .  displays a 16-bit signed integer, followed by a space.       
: . ( n)   (.) #> SPACE ;                                                                                                        
                                                                 U.R  displays a right-justified 16-bit unsigned number.         
: U.R ( u n)   PUSH  <# #S  OVER POP SWAP-  1 - SPACES  #> ;     U.  displays an unsigned number.                                
: U. ( u)   0 U.R  SPACE ;                                       DUMP  displays an address and 8 numbers from memory.  It        
: DUMP ( a - a)   CR  DUP 5 U.R SPACE  7 FOR                        returns an incremented address for a subsequent  DUMP .      
      1 @+ SWAP  7 U.R  NEXT SPACE ;                                                                                             
                                                                                                                                 
                                                                                                                                 
   ( Strings)   HEX                                              HERE  returns next address in dictionary.                       
: HERE ( - a)   H @ ;                                                                                                            
                                                                 abort"  types the current word (at  HERE ) and an error message 
{ : abort" }   H @ TYPE  SPACE  POP 7FFF AND TYPE  2DROP            (at  I )  It also returns the current  BLK  to locate an     
   BLK @  ?DUP DROP  0 ( QUIT) ;                                    error during  LOAD .  It will end with  QUIT , when defined. 
{ : dot" }   POP 7FFF AND TYPE  PUSH ;                              It is a headless definition, referenced only by  ABORT" .    
                                                                 dot"  types a message whose address is pulled off the return    
{ COMPILER : ABORT"   COMPILE abort"  22 STRING ;                   stack, incremented and replaced.                             
           : ."   COMPILE dot"  22 STRING ;                                                                                      
     FORTH }                                                     ABORT"  compiles  abort"  and the following string.  This is a  
                                                                    host  COMPILER  definition.  The target definition is in     
                                                                    block 30.                                                    
                                                                 ."  compiles  dot"  and the following string.                   
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( 15-bit buffer manager)                                      BUFFERS  returns indexed address of buffer ID.                  
{ : ADDRESS ( n - a) }   30 + 8 TIMES 2* ;                       PREV  current buffer number (0-NB).                             
{ : ABSENT ( n - n) }   NB FOR  DUP  I BUFFERS @ XOR  2* WHILE   OLDEST  last buffer read.  Next buffer is  OLDEST @ 1 + .       
     NEXT EXIT THEN POP  PREV N!  POP DROP SWAP DROP  ADDRESS ;  ADDRESS  calculates a buffer address from buffer number.  NB is 
                                                                    1.  If increased,  ADDRESS  and  BUFFERS  must be also.      
{ : UPDATED ( - a n) }   OLDEST @  BEGIN  1 +  NB AND            ABSENT  returns the block number when the requested block isn't 
      DUP PREV @ XOR UNTIL  OLDEST N!  PREV N!                      already in RAM.  Otherwise it returns the buffer address and 
   DUP ADDRESS  SWAP BUFFERS  DUP @                                 exits from  BLOCK .                                          
   8192 ROT !  DUP 0< NOT IF  POP DROP DROP THEN ;               UPDATED  returns the buffer address and current block number if 
                                                                    the pending buffer has been  UPDATEd .  Otherwise it returns 
: UPDATE   PREV @ BUFFERS  0 @+ SWAP  32768 OR  SWAP ! ;            the buffer address and exits from the calling routine ( BLOCK
{ : ESTABLISH ( n a - a) }   SWAP  OLDEST @  PREV N!                or BUFFER ).  Pending means oldest but not just used.        
   BUFFERS ! ;                                                   UPDATE  marks the current buffer ( PREV ) to be rewritten.      
: IDENTIFY ( n a - a)   SWAP  PREV @ BUFFERS ! ;                 ESTABLISH  stores the block number of the current buffer.       
                                                                 IDENTIFY  stores a block number into the current buffer.  Used  
                                                                    to copy blocks.                                              
   ( Disk read/write)                                            ##  emits 3 bytes to host to start a block transfer;  0 followed
{ : ## ( a n - a a #) }   0 EMIT  256 /MOD EMIT EMIT  DUP 1023 ;    by block number.                                             
                                                                                                                                 
{ : buffer ( n - a) }   UPDATED                                  buffer  transmits an updated block and awaits acknowledgement.  
   ## FOR  1 @+ SWAP  EMIT  NEXT  KEY 2DROP ;                    BUFFER  returns address of an empty (but assigned) buffer.      
: BUFFER ( n - a)   buffer ESTABLISH ;                                                                                           
                                                                 block  reads a block.                                           
{ : block ( n a - n a) }  OVER  ## FOR  KEY  SWAP ]  !+          BLOCK  returns the buffer address of a specified block, writing 
      NEXT DROP ;                                                   and reading as necessary.                                    
: BLOCK ( n - a)   ABSENT buffer  block ESTABLISH ;                                                                              
                                                                 FLUSH  forces buffers to be written.                            
: FLUSH   NB FOR  8192 BUFFER DROP  NEXT ;                       EMPTY-BUFFERS  clears buffer IDs, without writing.              
: EMPTY-BUFFERS   PREV [ NB 3 + ] LITERAL 0 FILL  FLUSH ;                                                                        
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
( Interpreter)                                                   LETTER  moves a string of characters from cell address  a  to   
{ : LETTER ( b a # - b a) }   FOR  DUP @  6 I@ XOR  WHILE           byte address  b .  Terminated by count ( # ) or delimitor    
         1 @+ PUSH  OVER C!  1 +  POP NEXT  EXIT THEN               (register 6).  Input pointer  >IN  is advanced.              
   >IN @  POP -  >IN ! ;                                                                                                         
{ : -LETTER ( b a # - b a) }  ?DUP IF                            -LETTER  scans the source string for a non-delimiter.  If found,
      1 - FOR  1 @+ SWAP  6 I@ XOR  0= WHILE NEXT  EXIT THEN        calls  LETTER .                                              
      1 -  POP LETTER  THEN ;                                                                                                    
: WORD ( n - a)   PUSH  H @  DUP 2*  DUP 1 +  DUP  >IN @         WORD  locates text in either block buffer or  TIB  ( BLK  is 0).
   BLK @ IF  BLK @ BLOCK + 1024  ELSE  TIB @ +  SPAN @  THEN        Reads word into  HERE  prefixing count and suffixing a space 
   >IN @  OVER >IN !  -  POP 6 I!                                   (in case count even).                                        
   -LETTER DROP  32 OVER C!  SWAP-  SWAP C! ;                                                                                    
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Dictionary search)                                          SAME  compares the string at  HERE  with a name field.  Cell    
{ : SAME ( h a - h a f, a t) }   OVER 4 I!  DUP 1 +                 count is in register 6.  High bit of each cell is ignored.   
   6 I@ FOR  1 @+ SWAP  4 I@ 1 @+ 4 I!  - 2* IF                     Returns address of parameter field; requires indirect        
         POP DROP  0 AND  EXIT THEN                                 reference if high bit of count set (separated head).         
      NEXT  SWAP 1 + @  0< IF  @  THEN SWAP ;                                                                                    
                                                                 COUNT  extracts the cell count from the first word of a string. 
{ : COUNT ( n - n) }   7 TIMES 2/  15 AND ;                      HASH  returns the address of the head of a vocabulary.          
{ : HASH ( n - a) }   CONTEXT SWAP- ;                            -FIND  searches a vocabulary for match with  HERE .  Fails with 
{ : -FIND ( h n - h t, a f) }   HASH  OVER @ COUNT  6 I!            zero link field.                                             
   BEGIN  @ DUP WHILE  SAME UNTIL  0 EXIT THEN  -1 XOR ;                                                                         
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Number input)   HEX                                         -DIGIT  converts an ASCII character to a digit (0-Z).           
: -DIGIT ( n - n)   30 -  DUP 9 > IF  7 -  DUP A < OR  THEN         Failure generates an error message.                          
   DUP BASE @ U< IF  EXIT THEN                                                                                                   
   2DROP  ABORT" ?" ;  RECOVER                                   C@+  increments address in register 6 and fetches character.    
                                                                 10*+  multiplies number by  BASE  and adds digit.               
{ : C@+ ( - n) }   6 I@ 1 +  DUP 6 I!  C@ ;                      NUMBER  converts given string to binary;  stores  BASE  in R4;  
{ : 10*+ ( u n - u) }   -DIGIT  0E TIMES *'  DROP ;                 saves minus sign;  terminates on count;  applies sign        
: NUMBER ( a - n)   BASE @ 4 I!  0  SWAP 2*  DUP 1 + C@  2D =                                                                    
   PUSH  DUP I - 6 I!  C@ I +  1 - FOR  C@+ 10*+  NEXT                                                                           
   POP IF NEGATE  THEN ;                                                                                                         
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Control)                                                    -'  searches vocabulary for following word.                     
: -' ( n - h t, a f)   32 WORD  SWAP -FIND ;                     '  returns address of following word in current vocabulary      
: ' ( - a)   CONTEXT @ -' IF  DROP ABORT" ?"  THEN ;  forget        Error message on failure.  forget  to use host version.      
                                                                                                                                 
: INTERPRET ( n n)   >IN 2!  BEGIN  1 -' IF  NUMBER              INTERPRET  accepts block number and offset.  Searches  FORTH    
      ELSE  EXECUTE  THEN AGAIN ;  RECOVER                          and executes words found;  otherwise converts to binary.     
                                                                                                                                 
: QUIT   BEGIN CR  TIB @ 64 EXPECT                               QUIT  accepts a character string into the text input buffer,    
      0 0 INTERPRET ." ok"  AGAIN ;  RECOVER                        interprets and replies  ok  to signify success;  repeats.    
                                                                 The address of  QUIT  is relocated into the end of abort" .     
' QUIT  dA @ -  ' abort" 11 + !                                                                                                  
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Initialize)   HEX                                           FORGET  restores  HERE  and vocabulary heads to values saved at 
: FORGET ( a)   POP 7FFF AND  DUP 2 + H !  2@ CONTEXT 2 - 2!        compile time (by  REMEMBER; ).                               
   1 CONTEXT ! ;                                                                                                                 
                                                                 BPS  awaits a start bit, assumes only the first data bit is     
{ : BPS }   4 BEGIN RX  10 XOR UNTIL  BEGIN 5 +  RX UNTIL           zero and computes  C/B .  Type a  B  or other even letter.   
   2/ C/B ! ;                                                    RS232  examines the serial input line and inverts serial I/O if 
{ : RS232 }   RX IF  EXIT THEN  200 S0 !  0B 0 I! ;                 an inverting buffer is used (line rests low).                
                                                                                                                                 
{ : reset }   0 ( RESET)                                         reset  is executed at power-up or reset.                        
   0 DUP 9 I!  DUP A I!  DUP 0B I!  DUP 8 I!  -1 A I!              Carefully initializes I/O registers to avoid glitches.        
   DUP D I!  DUP E I!  F I!  1A C I!                               Empties buffers at power-up only ( TIB  contains garbage).    
   TIB 2@ XOR IF  EMPTY-BUFFERS  SPAN @ TIB !  THEN                Calibrates serial I/O.                                        
   RS232  F E I!  BPS  ." hi" QUIT ;                               Cheerful  hi  and awaits command.                             
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Words)                                                      This is the beginning of the compiler.  A turn-key application  
: SWAP   SWAP ;        : OVER   OVER ;                              might need only the code above.                              
: DUP   DUP ;          : DROP   DROP ;                                                                                           
                                                                 Common words are defined for both interpreter and compiler.     
: XOR   XOR ;          : AND   AND ;                                                                                             
: OR   OR ;                                                      Number base words defined together;  DECIMAL  required.         
: +   + ;              : -   - ;                                                                                                 
: 0<   0< ;            : NEGATE   NEGATE ;                       LOAD  saves current input pointers,  calls  INTERPRET , restores
                                                                    input pointers and returns to  DECIMAL .   >IN  and  BLK  are
: @   @ ;              : !   ! ;                                    treated as a 32-bit pointer.  forget  so that host  LOAD     
                                                                    will be used.                                                
: OCTAL   8 BASE ! ;  forget                                                                                                     
: DECIMAL   10 BASE ! ;  forget                                                                                                  
: HEX   16 BASE ! ;  forget                                                                                                      
: LOAD ( n)   >IN 2@ PUSH PUSH  0 INTERPRET  10 BASE !                                                                           
   POP POP >IN 2! ;   forget                                                                                                     
( Compiler)   OCTAL                                              \\  breaks code compaction.                                     
: \\   0 ?CODE ! ;                                               ALLOT  increments the dictionary pointer to allot memory.       
: ALLOT ( n)   H +!  \\ ;                                        ,  compiles a number into the dictionary.                       
: , ( n)   H @ !  1 H +! ;                                       ,C  compiles an instruction available for compaction.           
: ,C ( n)   H @ ?CODE !  , ;                                     ,A  compiles a address relocated by  dA .                       
: ,A ( a)   dA @ -  ,C ;                                         LITERAL  compiles a number as a short literal, if possible.     
COMPILER : LITERAL ( n)   DUP -40 AND IF  147500 ,C  ,  EXIT     [  stops compilation by popping the return stack, thus returning
   THEN  157500 XOR ,C ;                                            out of the infinite  ]  loop.                                
: [   POP DROP ;                                                                                                                 
                                                                 ]  unlike  INTERPRET , searches both vocabularies before falling
FORTH : ]   BEGIN 2 -'  IF  1 -FIND IF  NUMBER  \ LITERAL           into  NUMBER .  When a word is found in  COMPILER  it is     
   ELSE DUP @                                                       executed;  if found in  FORTH  it is compiled.  If it is a   
      DUP 140040 AND  140040 =  OVER 170377 AND  140342 XOR AND     single instruction, it is placed in-line;  otherwise its     
      SWAP 170040 AND  100040 = OR IF  @ 40 XOR  ,C                 address is compiled for a call.                              
      ELSE  ,A  THEN THEN                                                                                                        
   ELSE  EXECUTE  THEN AGAIN ;  RECOVER                                                                                          
   ( Compiler)   HEX                                             PREVIOUS  returns the address and count of the name field of    
: PREVIOUS ( - a n)   CONTEXT @ HASH  @ 1 +  0 @+ SWAP ;            the word just defined.                                       
: USE ( a)   PREVIOUS  COUNT + 1 + ! ;                           USE  assigns to the previous word a specified code field.       
: DOES   POP 7FFF AND  USE ;                                     DOES  provides a behavior for a newly defined word.  It is      
: SMUDGE   PREVIOUS 2000 XOR  SWAP ! ;                              executed when that word is defined.                          
: EXIT   POP DROP ;                                              SMUDGE  smudges the name field to avoid recursion.              
                                                                 EXIT  returns from a definition early ( FORTH  version).        
: COMPILE   POP 7FFF AND  1 @+ PUSH  ,A ;                                                                                        
OCTAL                                                            COMPILE  pops the address of the following word and compiles it.
COMPILER : EXIT   100040 ,C ;   HEX                                 7FFF AND  masks the carry bit from the return stack.         
         : RECURSIVE   PREVIOUS DFFF AND  SWAP ! ;               EXIT  compiles a return instruction ( COMPILER  version).       
         : ;   \ RECURSIVE  POP DROP  \ EXIT ;  forget           RECURSIVE  unsmudges the name field so a new word can be found. 
                                                                 ;  terminates a definition.  forget  permits more definitions.  
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( Defining words)   OCTAL                                     CREATE  creates an entry in the dictionary.  It saves space for 
FORTH : CREATE   H @ 0 ,  40 WORD  CONTEXT @ HASH                  the link field, then fetches a word terminated by space.  It  
   2DUP @  SWAP 1 - !  SWAP @  COUNT 1 + ALLOT  !  147342 , ;      links the word into the proper vocabulary, allots space for   
                                                                   the name field and compiles the return-next-address           
: :   CREATE  -1 ALLOT  SMUDGE  ] ;  forget                        instruction appropriate for a variable.                       
                                                                                                                                 
: CONSTANT ( n)   CREATE  -1 ALLOT  \ LITERAL  \ EXIT ;          :  creates a definition;  -1 ALLOT  recovers the instruction    
: VARIABLE   CREATE  0 , ;                                          compiled by  CREATE ;  ]  compiles the definition in its     
                                                                    place.  forget  permits more definitions.                    
                                                                                                                                 
                                                                 CONSTANT  names a number by compiling a literal.                
                                                                                                                                 
                                                                 VARIABLE  initializes its variable to zero.                     
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 
   ( uCODE)   OCTAL                                              -SHORT  checks if last instruction was a 5-bit literal.         
: -SHORT ( - t)   ?CODE @ @  177700 AND  157500 XOR ;            FIX  merges 5-bit literal with new instruction.                 
: FIX ( n)   ?CODE @ @  77 AND OR  ?CODE @ ! ;                   SHORT  requires 5-bit literal (register, address or increment)  
: SHORT ( n)   -SHORT IF  DROP ABORT" n?"  THEN FIX ;               for current instruction.  Error message.                     
                                                                                                                                 
COMPILER                                                         @  and  !  compile 5-bit or stack address instructions.         
: @   -SHORT IF  167100 ,C  ELSE  147100 FIX  THEN ;  forget     I@  and  I!  compile 5-bit register instuctions.                
: !   -SHORT IF  177000 ,C  ELSE  157000 FIX  THEN ;  forget     @+ and !+  compile 5-bit increment instructions.                
: I@   147300 SHORT ;                                                                                                            
: I!   157200 SHORT ;                                            PUSH  and  POP  push and pop the return stack.                  
: @+   164700 SHORT ;                                               They are usually designated  >R  and  R> .                   
: !+   174700 SHORT ;                                            I  copies the return stack onto the parameter stack.            
                                                                 TIMES  pushes the return stack to repeat the next instruction   
: R>   147321 ,C ;                                                  for  n + 2  cycles.                                          
: POP   147321 ,C ;    : PUSH   157201 ,C ;                                                                                      
: I   147301 ,C ;      : TIMES   157221 ,C ;  forget                                                                             
   ( Structures)   OCTAL                                         OR,  compiles a 12-bit address with a backward jump instruction.
 FORTH { : OR, ( n n) }   \\  SWAP 7777 AND  OR , ;              BEGIN  saves  HERE  for backward jumps.                         
COMPILER : BEGIN ( - a)   H @  \\ ;                                                                                              
                                                                 UNTIL  compiles a conditional backward jump.                    
: UNTIL ( a)   110000 OR, ;                                      AGAIN  compiles an unconditional backward jump.                 
: AGAIN ( a)   130000 OR, ;                                      THEN  adds 12-bit current address into forward jump.            
: THEN ( a)   \ BEGIN  7777 AND  SWAP +! ;                       IF  compiles a conditional forward jump.                        
: IF ( - a)   \ BEGIN  110000 , ;                                WHILE  compiles a conditional forward jump - out of structure.  
: WHILE ( a - a a)   \ IF  SWAP ;                                REPEAT  resolves a  BEGIN ... WHILE ...  loop.                  
: REPEAT ( a a)   \ AGAIN  \ THEN ;                              ELSE  inserts false clause in an  IF ... THEN  conditional.     
: ELSE ( a - a)   \ BEGIN  130000 ,  SWAP \ THEN ;                                                                               
                                                                 FOR  compiles return stack push for a down-counting loop.       
: FOR ( - a)   \ PUSH  \ BEGIN ;                                 NEXT  compiles a backward decrement-and-jump.                   
: NEXT ( a)   120000 OR, ;                                                                                                       
                                                                                                                                 
                                                                                                                                 
   ( Strings)   HEX                                              STRING  compiles a character string with a specified delimiter. 
   FORTH : STRING ( n)   WORD @  7 TIMES 2/  1 + ALLOT ;                                                                         
                                                                 ABORT"  DOT"  are target versions of previously-defined host    
COMPILER : ABORT"   COMPILE abort"  22 STRING ;                     words.                                                       
         : ."   COMPILE dot"  22 STRING ;                                                                                        
         : (   29 WORD DROP ;                                    (  skips over a comment.  It must be defined in both  FORTH  and
                                                                    COMPILER .                                                   
   FORTH : (   \ ( ;                                                                                                             
                                                                 RESET  restores dictionary to power-up status.  It must be the  
: RESET   FORGET 0 ;  RECOVER   ' RESET dA @ -  ' reset !           last word in the dictionary.  It is called by  reset .       
                                                                                                                                 
                                                                 Insert application code before this block, to avoid using these 
                                                                    common target words.  Alternatively,  forget  them.          
                                                                                                                                 
                                                                                                                                 
                                                                                                                                 

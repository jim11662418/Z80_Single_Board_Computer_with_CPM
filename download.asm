               PAGE 0                  ; suppress page headings in ASW listing file
               cpu z80            
;==================================================================================
; Contents of this file are copyright Grant Searle
; HEX routine from Joel Owens.
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http: //searle.hostei.com/grant/index.html
;
; eMail:  home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
; edited for Macro Assembler AS V1.42 http://john.ccac.rwth-aachen.de:8000/as/ syntax - jsl 9/14/2024
;==================================================================================

TPA               equ  100H                   
REBOOT            equ  0H                     
BDOS              equ  5H                     
CONIO             equ  6                      
CONINP            equ  1                      
CONOUT            equ  2                      
PSTRING           equ  9                      
MAKEF             equ  22                     
CLOSEF            equ  16                     
WRITES            equ  21                     
DELF              equ  19                     
SETUSR            equ  32                     

CR                equ  0DH                    
LF                equ  0AH                    

FCB               equ  05CH                   
BUFF              equ  080H                   

                  org  TPA


                  LD A,0
                  LD (buffPos),A
                  LD (checkSum),A
                  LD (byteCount),A
                  LD (printCount),A
                  LD HL,BUFF
                  LD (buffPtr),HL


WAITLT:           CALL GETCHR                 
                  CP 'U'
                  JP Z,SETUSER
                  CP ':'
                  JR NZ,WAITLT


                  LD C,DELF
                  LD DE,FCB
                  CALL BDOS

                  LD C,MAKEF
                  LD DE,FCB
                  CALL BDOS

GETHEX:
                  CALL GETCHR
                  CP '>'
                  JR Z,CLOSE
                  LD B,A
                  PUSH BC
                  CALL GETCHR
                  POP BC
                  LD C,A

                  CALL BCTOA

                  LD B,A
                  LD A,(checkSum)
                  ADD A,B
                  LD (checkSum),A
                  LD A,(byteCount)
                  INC A
                  LD (byteCount),A

                  LD A,B

                  LD HL,(buffPtr)

                  LD (HL),A
                  INC HL
                  LD (buffPtr),HL

                  LD A,(buffPos)
                  INC A
                  LD (buffPos),A
                  CP 80H

                  JR NZ,NOWRITE

                  LD C,WRITES
                  LD DE,FCB
                  CALL BDOS
                  LD A,'.'
                  CALL PUTCHR

                  ;New line every 8K (64 dots)
                  LD A,(printCount)
                  INC A
                  CP 64
                  JR NZ,noCRLF
                  LD (printCount),A
                  LD A,CR
                  CALL PUTCHR
                  LD A,LF
                  CALL PUTCHR
                  LD A,0
noCRLF:           LD (printCount),A           

                  LD HL,BUFF
                  LD (buffPtr),HL

                  LD A,0
                  LD (buffPos),A
NOWRITE:
                  JR GETHEX
                  

CLOSE:

                  LD A,(buffPos)
                  CP 0
                  JR Z,NOWRITE2

                  LD C,WRITES
                  LD DE,FCB
                  CALL BDOS
                  LD A,'.'
                  CALL PUTCHR

NOWRITE2:
                  LD C,CLOSEF
                  LD DE,FCB
                  CALL BDOS

; Byte count (lower 8 bits)
                  CALL GETCHR
                  LD B,A
                  PUSH BC
                  CALL GETCHR
                  POP BC
                  LD C,A

                  CALL BCTOA
                  LD B,A
                  LD A,(byteCount)
                  SUB B
                  CP 0
                  JR Z,byteCountOK

                  LD A,CR
                  CALL PUTCHR
                  LD A,LF
                  CALL PUTCHR

                  LD DE,countErrMess
                  LD C,PSTRING
                  CALL BDOS

                  ;Sink remaining 2 bytes
                  CALL GETCHR
                  CALL GETCHR

                  JR FINISH

byteCountOK:

; Checksum
                  CALL GETCHR
                  LD B,A
                  PUSH BC
                  CALL GETCHR
                  POP BC
                  LD C,A

                  CALL BCTOA
                  LD B,A
                  LD A,(checkSum)
                  SUB B
                  CP 0
                  JR Z,checksumOK

                  LD A,CR
                  CALL PUTCHR
                  LD A,LF
                  CALL PUTCHR

                  LD DE,chkErrMess
                  LD C,PSTRING
                  CALL BDOS
                  JR FINISH

checksumOK:
                  LD A,CR
                  CALL PUTCHR
                  LD A,LF
                  CALL PUTCHR

                  LD DE,OKMess
                  LD C,PSTRING
                  CALL BDOS
                  


FINISH:
                  LD C,SETUSR
                  LD E,0
                  CALL BDOS

                  JP REBOOT


SETUSER:
                  CALL GETCHR
                  CALL HEX2VAL
                  LD E,A
                  LD C,SETUSR
                  CALL BDOS
                  JP WAITLT

                  
; Get a char into A
;GETCHR:  LD C,CONINP
;   CALL BDOS
;   RET

; Wait for a char into A (no echo)
GETCHR:
                  LD E,0FFH
                  LD C,CONIO
                  CALL BDOS
                  CP 0
                  JR Z,GETCHR
                  RET

; Write A to output
PUTCHR:           LD C,CONOUT                 
                  LD E,A
                  CALL BDOS
                  RET


;------------------------------------------------------------------------------
; Convert ASCII characters in B C registers to a byte value in A
;------------------------------------------------------------------------------
BCTOA             LD A,B                      ; Move the hi order byte to A
                  SUB 30H                     ; Take it down from Ascii
                  CP 0AH                      ; Are we in the 0-9 range here?
                  JR C,BCTOA1                 ; If so, get the next nybble
                  SUB 07H                     ; But if A-F, take it down some more
BCTOA1            RLCA                        ; Rotate the nybble from low to high
                  RLCA                        ; One bit at a time
                  RLCA                        ; Until we
                  RLCA                        ; Get there with it
                  LD B,A                      ; Save the converted high nybble
                  LD A,C                      ; Now get the low order byte
                  SUB 30H                     ; Convert it down from Ascii
                  CP 0AH                      ; 0-9 at this point?
                  JR C,BCTOA2                 ; Good enough then, but
                  SUB 07H                     ; Take off 7 more if it's A-F
BCTOA2            ADD A,B                     ; Add in the high order nybble
                  RET

; Change Hex in A to actual value in A
HEX2VAL           SUB 30H                     
                  CP 0AH
                  RET C
                  SUB 07H
                  RET


buffPos           db  00H                      
buffPtr           dw  0000H                   
printCount        db  00H                      
checkSum          db  00H                      
byteCount         db  00H                      
OKMess            db  "OK$"                 
chkErrMess        db  "======Checksum Error======$"
countErrMess      db  "======File Length Error======$"
                  end

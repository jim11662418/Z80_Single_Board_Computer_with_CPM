                  PAGE 0               ; suppress page headings in ASW listing file
                  cpu z80

;==================================================================================
; Contents of this file are copyright Grant Searle
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myseLF.
;
; http://searle.hostei.com/grant/index.html
;
; modifications by Jim Loos 09/18/2024...
; 1. if, on startup, the jumper connects the SIO SYNCA input to +5V, bypass 
; the "Press space to start" message, use SIO channel A for the console and go
; directly to loading CP/M.
; 2. minor cosmetic changes
;
; edited for Macro Assembler AS V1.42 http://john.ccac.rwth-aachen.de:8000/as/
;==================================================================================
;
; Memory Map:
;
;  on initial boot:
;     0000-3FFFH  EPROM
;          0000H  Monitor
;          2000H  BASIC
;     4000-FFFFH  RAM
;
;  after CP/M is loaded:
;     0000-FFFFH  RAM
;          D000H  CP/M system
;          E600H  BIOS
;
;------------------------------------------------------------------------------
;
; Z80 Monitor Rom
;
;------------------------------------------------------------------------------
; General Equates
;------------------------------------------------------------------------------
LF                equ  0AH
CR                equ  0DH
ESC               equ  1BH

CLSCR             equ  "\e[2J\e[H"     ; VT100 escape sequence to to clear screen and home cursor

; CF registers
CF_DATA           equ  10H
CF_FEATURES       equ  11H
CF_ERROR          equ  11H
CF_SECCOUNT       equ  12H
CF_SECTOR         equ  13H
CF_CYL_LOW        equ  14H
CF_CYL_HI         equ  15H
CF_HEAD           equ  16H
CF_STATUS         equ  17H
CF_COMMand        equ  17H
CF_LBA0           equ  13H
CF_LBA1           equ  14H
CF_LBA2           equ  15H
CF_LBA3           equ  16H

;CF Features
CF_8bit           equ  1
CF_NOCACHE        equ  082H

;CF Commands
CF_READ_SEC       equ  020H
CF_WRITE_SEC      equ  030H
CF_SET_FEAT       equ  0EFH

loadaddr          equ  0D000H          ; CP/M load address
numSecs           equ  24              ; number of 512 sectors to be loaded

;BASIC cold and warm entry points
BASCLD            equ  2000H
BASWRM            equ  2003H

SER_BUFSIZE       equ  40H
SER_FULLSIZE      equ  30H
SER_EMPTYSIZE     equ  5

RTS_HIGH          equ  0E8H            ; pause transmission by the terminal 
RTS_LOW           equ  0EAH            ; resume transmission by the terminal

SIOA_D            equ  00H             ; Z80 SIO Channel A data port
SIOA_C            equ  02H             ; Z80 SIO Channel A control port
SIOB_D            equ  01H             ; Z80 SIO Channel B data port
SIOB_C            equ  03H             ; Z80 SIO Channel B control port

PORTA             equ  80H             ; 8255 PPI port A
PORTB             equ  81H             ; 8255 PPI port B
PORTC             equ  82H             ; 8255 PPI port C
CWR               equ  83H             ; 8255 PPI port Control Word Regsiter

                  org  4000H           ; start of RAM
serABuf           ds   SER_BUFSIZE
serAInPtr         ds   2
serARdPtr         ds   2
serABufUsed       ds   1
serBBuf           ds   SER_BUFSIZE
serBInPtr         ds   2
serBRdPtr         ds   2
serBBufUsed       ds   1
primaryIO         ds   1
secNo             ds   1
dmaAddr           ds   2
blinkflag         ds   1
keystate          ds   1
stackSpace        ds   32
STACKTOP          equ  $               ; stack top

;------------------------------------------------------------------------------
; Reset
;------------------------------------------------------------------------------
                  org 0000H            ; start of EPROM
rst00:            di
                  jp initialize        ; initialize hardware and go

;------------------------------------------------------------------------------
; send a character to the active console
;------------------------------------------------------------------------------
                  org 0008H
rst08:            jp conout

;------------------------------------------------------------------------------
; get a character from the active console
;------------------------------------------------------------------------------
                  org 0010H
rst10:            jp conin

;------------------------------------------------------------------------------
; Check input buffer status of the active console
;------------------------------------------------------------------------------
                  org 0018H
rst18:            jp ckinchar

;------------------------------------------------------------------------------
; print zero terminated string pointed to by DE to the active console
;------------------------------------------------------------------------------
                  org 0020H
rst20:            jp printstr

;------------------------------------------------------------------------------
; print the number in C to the active console as two hexadecimal digits
;------------------------------------------------------------------------------
                  org 0028H
rst28:            jp print2hex

;------------------------------------------------------------------------------
; jump to the start of the monitor
;------------------------------------------------------------------------------
                  org 0030H
rst30:            jp monitor

;------------------------------------------------------------------------------
; print CR and LF to the active console
;------------------------------------------------------------------------------
                  org 0038H
rst38:            jp newline

;------------------------------------------------------------------------------
; SIO Vector = 0x60
;------------------------------------------------------------------------------
                  org 0060H
                  dw serialInt

;------------------------------------------------------------------------------
; serial interrupt handler.
; this interrupt routing called if either of the SIO channels receives a character,
; so need to check the status of each SIO input.
;------------------------------------------------------------------------------
serialInt:        push AF
                  push HL

                  ; check if there is a character received by channel A
                  sub A
                  out (SIOA_C),A       ; select RR0
                  in A,(SIOA_C)        ; status byte: D2=TX buff empty, D0=RX char ready
                  rrca                 ; rotate RX status into Carry flag,
                  jr NC, serialIntB    ; jump if no character received by channel A
serialIntA:       ld HL,(serAInPtr)    ; else retrieve the input pointer to the buffer
                  inc HL               ; next location in buffer
                  ld A,L
                  cp (serABuf+SER_BUFSIZE)&0FFH
                  jr NZ,notAWrap       ; jump if no wrap-around
                  ld HL,serABuf        ; else, reset HL to the beginnning of the buffer
notAWrap:         ld (serAInPtr),HL    ; update the pointer
                  in A,(SIOA_D)        ; read the character from the SIO
                  ld (HL),A            ; store it in the buffer
                  ld A,(serABufUsed)
                  inc A                ; increment the amount of buffer space used
                  ld (serABufUsed),A
                  cp SER_FULLSIZE
                  jr C,rtsA0           ; jump if the buffer is not yet full
                  ld A,05H             
                  out (SIOA_C),A       ; else, select WR5
                  ld A,RTS_HIGH        
                  out (SIOA_C),A       ; the buffer is full, tell terminal to stop sending
rtsA0:            pop HL
                  pop AF
                  ei
                  reti
                  
                  ; check if there is a character received by channel B
serialIntB:       sub A
                  out (SIOB_C),A       ; select WR0
                  in A,(SIOB_C)        ; status byte: D2=TX buff empty, D0=RX char ready
                  rrca                 ; rotate RX status into Carry flag,
                  jr NC, rtsB0         ; jump if no character received by channel B either
                  ld HL,(serBInPtr)    ; else retrieve the input pointer to the buffer
                  inc HL               ; next location in buffer
                  ld A,L
                  cp (serBBuf+SER_BUFSIZE)&0FFH
                  jr NZ,notBWrap       ; jump if no wrap-around
                  ld HL,serBBuf        ; else, reset HL to the beginnning of the buffer
notBWrap:         ld (serBInPtr),HL
                  in A,(SIOB_D)        ; read the character from the SIO
                  ld (HL),A            ; store it in the buffer
                  ld A,(serBBufUsed)
                  inc A                ; increment the amount of buffer space used
                  ld (serBBufUsed),A
                  cp SER_FULLSIZE
                  jr C,rtsB0           ; jump if the buffer is not yet full
                  ld A,05H             
                  out (SIOB_C),A       ; else, select WR5
                  ld A,RTS_HIGH
                  out (SIOB_C),A       ; the buffer is full, tell terminal to stop sending
rtsB0:            pop HL
                  pop AF
                  ei
                  reti

;------------------------------------------------------------------------------
; console input routine. use the 'primaryIO' flag to determine which SIO channel is active
;------------------------------------------------------------------------------
conin:            push HL
                  ld A,(primaryIO)
                  cp 0
                  jr NZ,waitForCharB

waitForCharA:     ld A,(serABufUsed)
                  cp 0
                  jr NZ,charavailA
                  jr waitForCharA

charavailA:       ld HL,(serARdPtr)
                  inc HL
                  ld A,L
                  cp (serABuf+SER_BUFSIZE)&0FFH
                  jr NZ, notRdWrapA
                  ld HL,serABuf
notRdWrapA:       di
                  ld (serARdPtr),HL
                  ld A,(serABufUsed)
                  dec A
                  ld (serABufUsed),A
                  cp SER_EMPTYSIZE
                  jr NC,rtsA1
                  ld A,05H
                  out (SIOA_C),A       ; select WR5
                  ld A,RTS_LOW
                  out (SIOA_C),A       ; the buffer is empty, allow terminal to resume sending
rtsA1:            ld A,(HL)
                  ei
                  pop HL
                  ret

waitForCharB:     ld A,(serBBufUsed)
                  cp 0
                  jr NZ, charavailB
                  jr waitForCharB

charavailB:       ld HL,(serBRdPtr)
                  inc HL
                  ld A,L
                  cp (serBBuf+SER_BUFSIZE)&0FFH
                  jr NZ, notRdWrapB
                  ld HL,serBBuf
notRdWrapB:       di
                  ld (serBRdPtr),HL
                  ld A,(serBBufUsed)
                  dec A
                  ld (serBBufUsed),A
                  cp SER_EMPTYSIZE
                  jr NC,rtsB1
                  ld A,05H
                  out (SIOB_C),A       ; select WR5
                  ld A,RTS_LOW
                  out (SIOB_C),A       ; the buffer is empty, allow terminal to resume sending
rtsB1:            ld A,(HL)
                  ei
                  pop HL
                  ret

;------------------------------------------------------------------------------
; console output routine.
; use the 'primaryIO' flag to determine which SIO channel is active as console.
;------------------------------------------------------------------------------
conout:           push AF              ; Store character
                  ld A,(primaryIO)
                  cp 0
                  jr NZ,conoutB1
                  jr conoutA1

conoutA:          push AF
conoutA1:         call cksioA          ; See if SIO channel A is finished transmitting
                  jr Z,conoutA1        ; loop until SIO flag signals ready to transmit
                  pop AF               ; retrieve character
                  out (SIOA_D),A       ; output the character
                  ret

conoutB:          push AF
conoutB1:         call cksioB          ; See if SIO channel B is finished transmitting
                  jr Z,conoutB1        ; loop until SIO flag signals ready to transmit
                  pop AF               ; retrieve character
                  out (SIOB_D),A       ; output the character
                  ret

;------------------------------------------------------------------------------
; SIO channel A status check routine.
;------------------------------------------------------------------------------
cksioA:           sub A
                  out (SIOA_C),A       ; select RR0
                  in A,(SIOA_C)        ; status byte D2=TX Buff Empty, D0=RX char ready
                  rrca                 ; rotate RX status into Carry Flag,
                  bit 1,A              ; set Zero flag if still transmitting character
                  ret
;------------------------------------------------------------------------------
; SIO channel B status check routine.
;------------------------------------------------------------------------------
cksioB:           sub A
                  out (SIOB_C),A       ; select RR0
                  in A,(SIOB_C)        ; status byte D2=TX Buff Empty, D0=RX char ready
                  rrca                 ; rotate RX status into Carry Flag,
                  bit 1,A              ; set Zero flag if still transmitting character
                  ret

;------------------------------------------------------------------------------
; check if there is a character waiting in the input buffer. 
; returns with zero flag set if there are no characters.
; uses the 'primaryIO' flag to determine which SIO channel is active as console.
;------------------------------------------------------------------------------
ckinchar:         ld A,(primaryIO)
                  cp 0
                  jr NZ,ckincharB

ckincharA:        ld A,(serABufUsed)
                  cp 0
                  ret

ckincharB:        ld A,(serBBufUsed)
                  cp 0
                  ret

;------------------------------------------------------------------------------
; clear RAM, set the stack pointer, initialise PPI and SIO hardware and start monitor loop.
;------------------------------------------------------------------------------
initialize:       ld HL,0FFFFH
                  xor a
                  ld (keystate),A      ; initialize 'keystate'
                  ld (primaryIO),A     ; make SIO channel A the console channel
                  
                  ; clear RAM from FFFFH down to 0000H
initialize1:      ld (HL),A            
                  dec L
                  jr NZ,initialize1
                  dec H
                  jr NZ,initialize1
                  
                  ld SP,STACKTOP       ; Set the stack pointer
                  
                  ; initialize 8255 PPI
                  ld A,98H
                  out (CWR),A          ; port A and C upper inputs, port B and port C lower outputs
                  ld A,0FFH
                  out (PORTB),A        ; turn off all yellow LEDs
                  
                  ld (blinkflag),A     ; set flag to enable blinking orange LED                  

                  ; initialize serial buffers
                  ld HL,serABuf        ; beginning of the channel A receive buffer
                  ld (serAInPtr),HL
                  ld (serARdPtr),HL
                  ld HL,serBBuf
                  ld (serBInPtr),HL    ; beginning of the channel B receive buffer
                  ld (serBRdPtr),HL
                  xor a
                  ld (serABufUsed),A
                  ld (serBBufUsed),A

                  ; initialise SIO channel A
                  ld A,00
                  out (SIOA_C),A       ; select WR0
                  ld A,18H
                  out (SIOA_C),A       ; channel A reset
                  ld A,04H
                  out (SIOA_C),A       ; select WR4
                  ld A,0C4H
                  out (SIOA_C),A       ; X64 clock rate, 1 stop bit, no parity
                  ld A,01H
                  out (SIOA_C),A       ; select WR1
                  ld A,18H
                  out (SIOA_C),A       ; RX INT on all RX chars, no TX INT enable
                  ld A,03H
                  out (SIOA_C),A       ; select WR3
                  ld A,0E1H
                  out (SIOA_C),A       ; 8 bits/char, RX Enable
                  ld A,05H
                  out (SIOA_C),A       ; select WR5
                  ld A,RTS_LOW
                  out (SIOA_C),A       ; RTS = LOW to allow the terminal to send us data

                  ; initialise SIO channel B
                  ld A,00
                  out (SIOB_C),A       ; select WR0
                  ld A,18H
                  out (SIOB_C),A       ; channel B reset
                  ld A,04H
                  out (SIOB_C),A       ; select WR4
                  ld A,0C4H
                  out (SIOB_C),A       ; X64 clock rate, 1 stop bit, no parity
                  ld A,01H
                  out (SIOB_C),A       ; select WR1
                  ld A,18H
                  out (SIOB_C),A       ; RX INT on all RX chars, no TX INT enable
                  ld A,02H
                  out (SIOB_C),A       ; select WR2 as Interrupt Vector Register
                  ld A,60H
                  out (SIOB_C),A       ; interrupt vector address value
                  ld A,03H
                  out (SIOB_C),A       ; select WR3
                  ld A,0E1H
                  out (SIOB_C),A       ; 8 bits/char, RX Enable
                  ld A,05H
                  out (SIOB_C),A       ; select WR5
                  ld A,RTS_LOW
                  out (SIOB_C),A       ; RTS = LOW to allow the terminal to send us data

                  ld A,00
                  ld I,A               ; interrupt vector in page 0
                  im 2
                  ei

                  call clear_consoles  ; clear both consoles

; if the SYNC input (SIO pin 11) for channel A is HIGH, jump straight to loading CP/M.
                  out (SIOA_C),A       ; select RR0
                  in A,(SIOA_C)        ; D4 = SYNC/HUNT
                  rrca                 ; rotate D4 into carry
                  rrca
                  rrca
                  rrca
                  rrca
                  jr C,printbanner     ; jump if carry set (SYNC input is low)
                  jp cpmload           ; else, go load CP/M
 
printbanner:      ld DE,signontxt
                  call printstr

;------------------------------------------------------------------------------
; monitor command loop.
;------------------------------------------------------------------------------
monitor:          ld HL,monitor        ; save entry point for Monitor
                  push HL              ; this is the return address to return to the monitor loop
monitor1:         xor a
                  ld (keystate),A      ; reset 'keystate' back to zero
                  call newline
                  ld A,'>'
                  call conout          ; print '>' as a prompt
monitor2:         call ckinchar        ; see if there's input from the console
                  jr NZ,monitor3       ; jump if there's input from the console available
                  
                  ; else, flash the orange LED at about 1Hz while waiting for input from the console
                  ld A,(blinkflag)
                  or A
                  jr Z, monitor2       ; do not blink the LED if the flag is reset
                  dec E
                  jr NZ,monitor2
                  dec D
                  jr NZ,monitor2
                  in A,(PORTC)         ; read port C
                  cpl
                  and 01H              ; mask out everything except bit 0
                  out (CWR),A          ; toggle the orange LED connected to port C bit 0
                  jr monitor2          ; go back and check for input
                  
monitor3:         call conin           ; get a character from the console
                  cp ESC               ; is it the ESCape key?
                  jr NZ,monitor4       ; jump if not ESCape key
                  ld A,(keystate)
                  inc A
                  ld (keystate),A      ; else, move to the next 'keystate'
                  jr monitor2
                  
monitor4:         cp ' '
                  jr C,monitor2        ; if the input is a control character, go back for another character

                  cp ':'               ; is this ':' indicating the start of a hex file record download?
                  jp Z,hexdnload

                  cp '?'
                  jp Z,help

                  and 5FH              ; convert input command character to uppercase
                  cp 'B'
                  jp Z,basic

                  cp 'C'
                  jp Z,cpmload

                  cp 'D'
                  jp Z,dump

                  cp 'E'
                  jp Z,editmemory

                  cp 'G'
                  jp Z,gotocmd
                  
                  cp 'H'
                  jp Z,hexdnload

                  cp 'I'
                  jp Z,inputport

                  cp 'O'
                  jp Z,outputport

                  cp 'R'
                  jp Z,0000H

                  cp 'L'
                  jr NZ,monitor5
                  
                  ld A,(keystate)
                  cp 2                 ; has ESCape been pressed two times in succession?
                  ld A,0
                  ld (keystate),A      ; always reset 'keystate' back to zero
                  jp NZ,monitor2       ; jump if ESCape has not been pressed twice (keystate != 2)
                  
                  ; ESC, ESC 'L' has been pressed...
                  ld A,(blinkflag)
                  cpl
                  ld (blinkflag),A     ; toggle 'blinkflag'
                  ld A,01H
                  out (CWR),A          ; turn off the the orange LED
                  jp monitor2

monitor5:         ld A,'?'
                  call conout
                  jp monitor1

;------------------------------------------------------------------------------
; input from an I/O port.
;------------------------------------------------------------------------------
inputport:        ld DE,portaddresstxt
                  call printstr        ; prompt for port address
                  call get4hex         ; get the port address into HL
                  call newline
                  ret C                ; return if canceled
                  
                  ld C,L               ; transfer the port address to C
                  in A,(C)             ; input from the port
                  ld C,A
                  ld DE,portinputtxt
                  call printstr
                  call print2hex       ; print the input value in C
                  call newline
                  ret

;------------------------------------------------------------------------------
; output to an I/O port.
;------------------------------------------------------------------------------
outputport:       ld DE,portaddresstxt
                  call printstr        ; promt for port address
                  call get4hex         ; get the port address into HL
                  call newline
                  ret C                ; return if canceled
                  
                  ld C,L               ; transfer the port address to C
                  ld DE,portoutputtxt
                  call printstr        ; prompt for output port value
                  call get4hex         ; get the port value into HL
                  call newline
                  ret C                ; return if canceled
                  ld A,L               ; transfer the output value to A
                  out (C),A            ; else output the value to the port
                  ret

;------------------------------------------------------------------------------
; display/edit memory contents.
; <Enter> or <Space> increments to the next memory address.
; <Esc> exits back to the main monitor loop.
;------------------------------------------------------------------------------
editmemory:       ld DE,addresspromttxt
                  call printstr        ; prompt for address
                  call get4hex         ; get the memory address into HL
                  call newline
                  call newline
                  ret C                ; return if <Enter> or <Esc>
                  
                  ld DE,editmemorytxt   
                  call printstr
                  
editmemory1:      ld C,H
                  call print2hex       ; print the hi byte of the current memory address
                  ld C,L
                  call print2hex       ; print the lo byte of the current memory address
                  ld A,':'
                  call conout
                  ld A,' '
                  call conout
                  ld A,(HL)            ; retrieve the byte from memory
                  ld C,A
                  call print2hex       ; print the byte at the current memory address
                  ld A,' '
                  call conout
                  ld A,'-'
                  call conout
                  ld A,'>'
                  call conout
                  ld A,' '
                  call conout

editmemory2:      push HL
                  call get4hex         ; get the new memory value into HL
                  ld B,L               ; transfer the new memory value from L to B
                  pop HL
                  jr C,editmemory3     ; jump if the input from the 'get4hex' function is is Enter, Space or Escape
                  ld (HL),B            ; else, store the new value from the 'get4hex' function (saved in B) into memory
                  inc HL               ; next memory address
                  call newline
                  jr editmemory1       ; loop back

editmemory3:      cp ESC               ; Escape?
                  jr NZ,editmemory4    ; jump if not escape
                  call newline         ; next line
                  ret                  ; and return

editmemory4:      cp ' '               ; Space?
                  jr Z,editmemory5
                  cp CR                ; Enter?
                  jr NZ,editmemory2    ; if not Enter or Space, go back for more input
editmemory5:      ld C,(HL)            ; else, retrieve the value from the current memory location
                  call print2hex       ; print the value
                  call newline
                  inc HL               ; next memory location
                  jr editmemory1       ; loop back

;------------------------------------------------------------------------------
; clear screens on both A and B channels using VT100 escape sequence
;------------------------------------------------------------------------------
clear_consoles:   ld HL,clearscreen
clear_consoles1:  ld A,(HL)            ; get character
                  or A                 ; is it 00?
                  ret Z                ; if so, return
                  call conoutA         ; print to channel A
                  call conoutB         ; print to channel B
                  inc HL               ; next character
                  jr clear_consoles1   ; go back for the next character in the string

clearscreen       db CLSCR,0           ; VT100 escape sequence to clear screen and home cursor


;------------------------------------------------------------------------------
; print zero terminated string pointed to by DE to the active channel.
;------------------------------------------------------------------------------
printstr:         ld A,(DE)            ; get character
                  or A
                  ret Z                ; return if zero
                  call conout          ; else, print the character
                  inc DE               ; point to the next character
                  jr printstr          ; loop back for another character

;------------------------------------------------------------------------------
; print CR and LF to the active channel.
;------------------------------------------------------------------------------
newline:          push AF
                  ld A,CR              ; else, finish with carriage return
                  call conout
                  ld A,LF              ; and line feed
                  call conout
                  pop AF
                  ret

;------------------------------------------------------------------------------
; use the byte in A to turn on the LEDs connected to PPI port B (useful for de-bugging)
;------------------------------------------------------------------------------
setLEDs:          push AF
                  cpl
                  out (PORTB),A
                  pop AF
                  ret

resetLEDs:        push AF
                  ld A,0FFH
                  out (PORTB),A
                  pop AF
                  ret

;------------------------------------------------------------------------------
; get one hex digit (0-9,A-F) from the console in A. echo it. return with carry set if 
; <Space>, <Enter>, <Escape> or <^C>.
;------------------------------------------------------------------------------
get1hex:          call conin           ; get a character from the console
                  cp 03H
                  jr Z,get1hex1        ; jump if <^C>
                  cp CR
                  jr Z,get1hex1        ; jump if <Enter>
                  cp ESC
                  jr Z,get1hex1        ; jump if <Esc>
                  cp ' '
                  jr NZ,get1hex3       ; jump if not <SPACE>
get1hex1:         scf                  ; set the carry flag
                  ret                  ; and return

get1hex3:         cp '0'
                  jr C,get1hex         ; go back for another if the character is below '0'
                  cp '9'+1
                  jr C,get1hex         ; jump if the character is '0'-'9'
                  cp 'A'
                  jr C,get1hex         ; go back for another character if below 'A'
                  and 5FH              ; convert to upper case
                  cp 'F'+1
                  jr NC,get1hex        ; go back for another character if above 'F'
get1hex4:         call conout          ; else, echo the character to the active console if '0'-'9' or 'A'-'F'
                  or a                 ; clear carry flag
                  ret                  ; and return

;------------------------------------------------------------------------------
; get up to four hex digits from the console. echo valid hex digits (0-9,A-F). return the 
; binary value in HL. returns with carry set if <Enter>, <Esc> or <Space> is input as the first digit.
; if more than 4 digits are entered, uses the last four digits entered when <Enter> was pressed.
;------------------------------------------------------------------------------
get4hex:          ld HL,0
                  call conin           ; get a character from the console
                  cp CR
                  jr Z,get4hex1        ; jump if <Enter>
                  cp ESC
                  jr Z,get4hex1        ; jump if <Esc>
                  cp ' '
                  jr NZ,get4hex3       ; jump if not <SPACE>
get4hex1:         scf                  ; set the carry flag
                  ret                  ; and return to main program

get4hex2:         call conin           ; get the next digit
                  cp ESC               ; Escape?
                  jr Z,get4hex1        ; if so, return with carry set
                  cp CR                ; Enter?
                  jr NZ,get4hex3       ; jump if not Enter
                  or A                 ; else clear carry flag
                  ret                  ; and return

get4hex3:         cp '0'
                  jr C,get4hex2        ; go back for another if the character is below '0'
                  cp '9'+1
                  jr C,get4hex4        ; jump if the character is '0'-'9'
                  cp 'A'
                  jr C,get4hex2        ; go back for another character if below 'A'
                  and 5FH              ; convert to upper case
                  cp 'F'+1
                  jr NC,get4hex2       ; go back for another character if above 'F'
get4hex4:         call conout          ; else, echo the character to the active console if '0'-'9' or 'A'-'F'
                  add HL,HL            ; otherwise, rotate the previous low nibble to high
                  add HL,HL            ; rather slowly
                  add HL,HL            ; until we get to the top
                  add HL,HL            ; and then we can continue on.
                  sub 30H              ; convert ASCII to byte value
                  cp 0AH               ; are we in the 0-9 range?
                  jr C,get4hex5        ; then we just need to sub 30H, but if it is A-F
                  sub 07H              ; we need to take off 7 more to get the value down to
get4hex5:         and 0FH              ; to the right hex value
                  add A,L              ; add the high nibble to the low
                  ld L,A               ; move the byte back to A
                  jr get4hex2          ; and go back for next digit

;------------------------------------------------------------------------------
; GOTO command:
;------------------------------------------------------------------------------
gotocmd:          ld DE,addresspromttxt
                  call printstr        ; prompt for the goto address
                  call get4hex         ; get the address in HL
                  call newline
                  ret C                ; return if <Enter> or <Esc>
                  push HL              ; push the address from the 'get4hex' function
                  ret                  ; jump to the address

;------------------------------------------------------------------------------
; load one Intel HEX record from the console. jumps to the address contained in the last record.
; a record (line of text) consists of six fields that appear in order from left to right:
;   1. start code, one character, an ASCII colon ':'.
;   2. byte count, two hex digits, indicating the number of bytes in the data field.
;   3. address, four hex digits, representing the 16-bit beginning memory address offset of the data.
;   4. record type, two hex digits (00=data, 01=end of file), defining the meaning of the data field.
;   5. data, a sequence of n bytes of data, represented by 2n hex digits.
;   6. checksum, two hex digits, a computed value (starting with the byte count) used to verify record data.
;------------------------------------------------------------------------------
hexdnload:        ld B,0               ; clear the error count
                  cp ':'
                  jr NZ,hexdnload1
                  call newline
                  ld A,':'
                  jr hexdnload3
                  
hexdnload1:       ld DE,dnloadprompttxt 
                  call printstr        ; prompt for the hex download
                  
hexdnload2:       call conin
                  cp ':'
                  jr NZ,hexdnload2     ; loop here until the start of record character ':' is received

; get the byte count
hexdnload3:       call conout          ; echo the ':'
                  call load2hex        ; get the byte count...
                  ld E,A               ; use the byte count to initialize the checksum in E
                  ld D,A               ; store byte count in D

; get the record's starting address into HL...
                  call load2hex        ; get memory load address hi byte
                  ld H,A               ; put hi byte of the address in H
                  add A,E              ; update checksum
                  ld E,A
                  call load2hex        ; get memory load address lo byte
                  ld L,A               ; put lo byte of the address in L
                  add A,E              ; update checksum
                  ld E,A

; get the record's type...
                  call load2hex        ; get record type
                  ld C,A               ; save record type in C
                  add A,E              ; update checksum
                  ld E,A
                  ld A,C               ; recall the record type from C
                  cp 01H               ; record type 00=data, 01=end of file
                  jr Z,hexdnload7      ; jump if this is the end of file record type

; this is a normal data type record. get data bytes until the byte count in D is zero...
hexdnload4:       call load2hex        ; get the next data byte for this record
                  ld (HL),A            ; store the data byte in memory
                  inc HL               ; increment pointer to next memory location                  
                  add A,E
                  ld E,A               ; update checksum
                  dec D                ; decrement the byte count
                  jr NZ,hexdnload4     ; loop back for more data bytes until the byte count is zero

; all the data bytes for this record have been received, now get the checksum byte...
; since the record's checksum byte is the two's complement and therefore the additive inverse
; of the data checksum, the verification process can be reduced to summing all decoded byte
; values, including the record's checksum, and verifying that the LSB of the sum is zero.                  
hexdnload5:       call load2hex        ; get the checksum
                  add A,E              ; add the computed checksum in E to the record's checksum
                  ld C,A               ; save the sum of the computed checksum and the record's checksum
                  call conin           ; get the CR at the end of the record
                  call conout          ; echo the CR
                  ld A,C               ; restore the sum of the computed checksum and the record's checksum
                  and A                ; is it zero (is the checksum good)?
                  jr Z,hexdnload6      ; jump if the checksum is good
                  inc B                ; else, increment the checksum error count
hexdnload6:       jr hexdnload2        ; and go back for the next record
                  
; this is the end of file record, get the checksum byte...
hexdnload7:       call load2hex        ; get the checksum byte for the last record
                  ld C,A               ; save the last record's checksum byte in C
                  call conin           ; get the CR at the end of the last record
                  call conout          ; echo the CR 
                  ld A,C               ; recall the last record's checksum byte from C
                  add A,E              ; add the computed checksum in E to the record's checksum byte
                  and A                ; is it zero (is the checksum good)?
                  jr Z,hexdnload8      ; jump if checksum is good
                  inc B                ; else, increment the checksum error count

hexdnload8:       call newline
                  ld A,B               ; recall the number of checksum errors from B to A
                  call print8dec        ; print the number of checksum errors
                  ld DE,cksumerrortxt
                  call printstr
                  ld A,B               ; recall the number of checksum errors from B to A
                  or A
                  ret NZ               ; return to the monitor if there were checksum errors

                  ld A,H               ; else, load the hi byte of the last record's address into A
                  or L                 ; 'OR' the lo byte of the last record's with the hi byte
                  ret Z                ; return to the monitor if the address in HL is 0000

                  push HL              ; else, push the address from the last record onto the stack
                  ret                  ; jump to the address in the last record
                  
;------------------------------------------------------------------------------
; get two hex digits from the console, echo the digits
; return them as a byte in A.
; used by the hex download function above.
;------------------------------------------------------------------------------
load2hex:         push BC
                  call conin           ; get the most significant hex digit
                  call conout          ; echo it
                  ld B,A               ; save it in B
                  call conin           ; get the least significant hex digit
                  call conout          ; echo it
                  ld C,A               ; save it in C
                  ld A,B               ; recall the most significant nybble from B
                  sub 30H              ; take it down from ASCII
                  cp 0AH               ; are we in the 0-9 range here?
                  jr C,load2hex1       ; if so, get the next nybble
                  sub 07H              ; but if A-F, take it down some more
load2hex1:        RLCA                 ; rotate the nybble from low to high
                  RLCA                 ; one bit at a time
                  RLCA                 ; until we
                  RLCA                 ; get there with it
                  ld B,A               ; save the high nybble in B
                  ld A,C               ; recall the least significant nybble from C
                  sub 30H              ; convert it down from ASCII
                  cp 0AH               ; 0-9 at this point?
                  jr C,load2hex2       ; good enough then, but
                  sub 07H              ; take off 7 more if it's A-F
load2hex2:        add A,B              ; add the most significant nybble in B to the least significant nybble in A
                  pop BC
                  ret                  ; return with the binary value in A
                  
;-------------------------------------------------------------------------   
; print the 8 bit unsigned number in A as three decimal digits. 
; leading zeros are suppressed.                  
;-------------------------------------------------------------------------
print8dec:        push DE              ; save registers
                  push BC
                  ld E,0               ; E will be the suppress leading zeros flag
                  ld D,100             ; D will be the subtrahend
print8dec1:       ld C,'0'-1           ; C will be the ASCII digit for the hundreds and tens
print8dec2:       inc C
                  sub A,D              ; subtrahend (either 100 for the hundreds digit or 10 for the tens digit)
                  jr NC,print8dec2     ; repeat until the subtraction caused an underflow
                  add A,D              ; else, add the subtrahend back to reverse the underflow
                  ld B,A               ; move what remains from the subtraction into B
                  LD A,C               ; move the ASCII digit into A
                  cp '1'               ; is it zero?
                  jr NC,print8dec4     ; jump if the count is not zero
                  ld A,E               ; else, get the suppress leading zero flag
                  or A
                  ld A,C               ; restore the ASCII digit
                  jr Z,print8dec5      ; skip printing the zero if the leading zero flag is if zero
print8dec4:       call conout          ; else, print the ASCII digit for the hundreds or tens
                  ld E,0FFH            ; set the suppress leading zeros flag (all zeros from this point will be printed)
print8dec5:       ld A,D            
                  sub 90               ; reduce the subtrahend from 100 to 10
                  ld D,A               ; save it back in D
                  ld A,B               ; recall the remainder from B
                  jr NC,print8dec1     ; jump if the tens digit was not yet done
                  add A,'0'            ; convert from binary to ASCII
                  call conout          ; print the units digit
                  pop BC               ; restore registers
                  pop DE
                  ret
                  
;-------------------------------------------------------------------------   
; print the unsigned 16 bit number in HL as five decimal digits.
; leading zeros are suppressed.
;-------------------------------------------------------------------------   
print16dec:       ld B,0               ; clear suppress leading zero flag                  
                  ld DE,-10000
                  call printdigit      ; print the ten thousands digit
                  ld DE,-1000
                  call printdigit      ; print the thousands digit
                  ld DE,-100
                  call printdigit      ; print the hundreds digit
                  ld DE,-10             
                  call printdigit      ; print the tens digit
                  ld A,L               ; what remains in L is the units digit
                  add A,'0'            ; convert to ASCII
                  jp conout            ; print the units digit and return
                  
printdigit:       ld C,'0'-1           ; ASCII digit
printdigit1:      inc C
                  add HL,DE            ; add the negative number to HL (subtract DE from HL)
                  jr C,printdigit1     ; repeat until the subtraction causes an underflow
                  ld A,D               ; convert the negative number in DE into positive
                  cpl
                  ld D,A
                  ld A,E
                  cpl
                  ld E,A
                  inc DE
                  add HL,DE            ; add the positive number in DE back to HL to reverse the underflow
                  ld A,C               ; get the ASCII digit
                  cp '1'               ; is it less than '1'?
                  jr NC,printdigit2    ; jump if the ASCII digit is not zero
                  ld A,B               ; get the suppress leading zero flag from B into A
                  or A                 ; set flags
                  ld A,C               ; recall the count
                  ret Z                ; return if the suppress flag is zero
                  jp conout            ; else, print '0' and return
                  
printdigit2:      ld B,0FFH            ; set the suppress leading zero flag. all zeros are printed from now on
                  jp conout            ; print the digit '1'-'9' and return

                  
;-------------------------------------------------------------------------
; print the contents of one page of memory in hex and ASCII
;-------------------------------------------------------------------------
dump:             ld DE,addresspromttxt
                  call printstr        ; prompt for starting address
                  call get4hex         ; get the address into HL
                  call newline
                  ret C                ; return if <Enter> or <Esc>
                  
                  ld A,L
                  and 0F0H
                  ld L,A
dump1:            ld E,16              ; E is the line counter
                  push DE
                  ld DE,displaytxt
                  call printstr        ; print the header
                  pop DE

dump2:            ld C,H
                  call print2hex       ; print the hi byte of the address
                  ld C,L
                  call print2hex       ; print the lo byte of the address
                  ld A,' '
                  call conout          ; print a space

dump3:            ld C,(HL)            ; retrieve the byte from the address
                  call print2hex       ; print it in hexadecimal
                  inc HL               ; next address
                  ld A,L
                  and 0FH              ; 16 hex values printed?
                  jr Z,dump4           ; if so, go print the ASCII values
                  ld A,' '
                  call conout          ; print a space
                  jr dump3             ; loop back for the next byte

dump4:            ld A,' '
                  call conout          ; print a space between the hex and ASCII values
                  push DE
                  ld DE,-10H
                  add HL,DE            ; reset HL to the beginning of the line
                  pop DE
dump5:            ld A,(HL)            ; retrieve the byte
                  cp 07FH
                  jr NC,dump6          ; jump if the byte is 7FH or above
                  cp 21H
                  jr NC,dump7          ; jump if the byte is above 20H
dump6:            ld A,'.'             ; else, use '.' to represent the ASCII value
dump7:            call conout          ; print it
                  inc HL               ; next address
                  ld A,L
                  and 0FH              ; 16 ASCII values printed?
                  jr NZ,dump5          ; if not, loop back for the next byte
                  call newline
                  ld A,L
                  and 0FFH
                  jr Z,dump8           ; jump if 16 lines have been printed
                  jr dump2             ; else, loop back and do another line

dump8:            push DE
                  ld DE,displayprompttxt
                  call printstr        ; prompt for next page or exit
                  pop DE
dump9:            call conin
                  cp ' '               ; is the input SPACE?
                  jr Z,dump1           ; if so, print another page of memory
                  cp ESC               ; is the input ESC?
                  ret Z                ; if so, return
                  jr dump9             ; if not SPACE or ESC, go back for another input

;-------------------------------------------------------------------------
; print the 8 bit binary number in C as two hex digits.
;-------------------------------------------------------------------------
print2hex:        ld A,C
                  rra
                  rra
                  rra
                  rra
                  call print2hex1
                  ld A,C
print2hex1:       and 0FH
                  add A,'0'
                  cp '9'+1
                  jr C,print2hex2
                  add A,7
print2hex2:       call conout
                  ret

;------------------------------------------------------------------------------
; start BASIC command.
;------------------------------------------------------------------------------
basic:            ld DE,basictxt
                  call printstr
                  call conin
                  cp ESC
                  ret Z                ; cancel if ^C
                  
                  and 5FH              ; uppercase
                  cp 'C'
                  jp Z,BASCLD          ; BASIC cold start
                  cp 'W'
                  jp Z,BASWRM          ; BASIC warm start
                  ret

;------------------------------------------------------------------------------
; display Help command.
;------------------------------------------------------------------------------
help:             ld DE,menutxt         ; print the menu
                  call printstr
                  ret

;------------------------------------------------------------------------------
; CP/M load command.
;------------------------------------------------------------------------------
cpmload:          ld A,01H
                  out (CWR),A          ; turn off the LED connected to port C bit 0
                  call clear_consoles  ; clear screen on both consoles
                  ld DE,cpmloadingtxt
                  call printstr        ; print "Loading CP/M..."
                  call cfWait
                  ld A,CF_8bit         ; Set IDE to be 8-bit
                  out (CF_FEATURES),A
                  ld A,CF_SET_FEAT
                  out (CF_COMMand),A
                  call cfWait
                  ld A,CF_NOCACHE      ; no write cache
                  out (CF_FEATURES),A
                  ld A,CF_SET_FEAT
                  out (CF_COMMand),A

                  ld B,numSecs
                  ld A,0
                  ld (secNo),A
                  ld HL,loadaddr
                  ld (dmaAddr),HL

processSectors:   call cfWait
                  ld A,(secNo)
                  out (CF_LBA0),A
                  ld A,0
                  out (CF_LBA1),A
                  out (CF_LBA2),A
                  ld a,0E0H
                  out (CF_LBA3),A
                  ld A,1
                  out (CF_SECCOUNT),A
                  call readsec
                  ld DE,0200H
                  ld HL,(dmaAddr)
                  add  HL,DE
                  ld (dmaAddr),HL
                  ld A,(secNo)
                  inc A
                  ld (secNo),A
                  djnz processSectors

; Start CP/M using entry at top of BIOS
; The current active console stream ID is pushed onto the stack
; to allow the CBIOS to pick it up
; 0 = SIO A, 1 = SIO B
                  ld A,(primaryIO)
                  push AF
                  ld HL,(0FFFEH)
                  jp (HL)

;------------------------------------------------------------------------------
; read physical sector from host
readsec:          push AF
                  push BC
                  push HL
                  call cfWait
                  ld A,CF_READ_SEC
                  out (CF_COMMand),A
                  call cfWait
                  ld c,4
                  ld HL,(dmaAddr)
rd4secs:          ld b,128
rdByte:           nop
                  nop
                  in A,(CF_DATA)
                  ld (HL),A
                  inc HL
                  dec b
                  jr NZ, rdByte
                  dec c
                  jr NZ,rd4secs

                  pop HL
                  pop BC
                  pop AF
                  ret

; wait for disk to be ready (busy=0,ready=1)
cfWait:           push AF
cfWait1:          in A,(CF_STATUS)
                  and 080H
                  cp 080H
                  jr Z,cfWait1
                  pop AF
                  ret

;------------------------------------------------------------------------------
signontxt         db "Z80 SBC Boot ROM 1.2 by G. Searle\r\n"
                  db "Modifications by Jim Loos\r\n"
                  db "Assembled ",DATE," at ",TIME,"\r\n"
                  db "Type ? for menu\r\n",0

cpmloadingtxt     db "\r\nLoading CP/M...\r\n",0

basictxt          db "\r\nCold or Warm ?\r\n",0

dnloadprompttxt:  db "\nWaiting for hex download...\r\n",0
cksumerrortxt     db " Checksum errors\r\n",0
addresspromttxt   db "\r\nAddress: ",0
displaytxt        db "\r\n     00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\r\n",0
displayprompttxt: db "\r\n<SPACE> for next page. <ESC> to exit.\r\n",0
portaddresstxt    db "\r\nPort Address: ",0
portoutputtxt     db "Output Value: ",0
portinputtxt      db "Port Value: ",0
editmemorytxt:    db "      old   new\n",0
menutxt           db "\r\n\r\nB - BASIC in ROM\r\n"
                  db "C - boot CP/M\r\n"
                  db "D - Display memory contents\r\n"
                  db "E - Edit memory contents\r\n"
                  db "I - Input from I/O port\r\n"
                  db "G - Go to an address\r\n"
                  db "H - Intel Hex file download\r\n"
                  db "O - Output to an I/O port\r\n"
                  db "R - Reset\r\n"
                  db "? - display this menu\r\n",0

                  include "BASIC.inc"  ; ROM BASIC

                  end


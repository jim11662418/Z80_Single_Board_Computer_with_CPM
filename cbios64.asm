               PAGE 0                  ; suppress page headings in ASW listing file
               cpu z80            
;==================================================================================
; Contents of this file are copyright Grant Searle
; Blocking/unblocking routines are the published version by Digital Research
; (bugfixed, as found on the web)
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

ccp               equ  0D000h                 ; Base of CCP.
bdos              equ  ccp + 0806h            ; Base of BDOS.
bios              equ  ccp + 1600h            ; Base of BIOS.

; Set CP/M low memory datA, vector and buffer addresses.

iobyte            equ  03h                    ; Intel standard I/O definition byte.
userdrv           equ  04h                    ; Current user number and drive.
tpabuf            equ  80h                    ; Default I/O buffer and command line storage.


SER_BUFSIZE       equ  60                     
SER_FULLSIZE      equ  50                     
SER_EMPTYSIZE     equ  5                      


RTS_HIGH          equ  0E8H                   
RTS_LOW           equ  0EAH                   

SIOA_D            equ  00H                    
SIOA_C            equ  02H                    
SIOB_D            equ  01H                    
SIOB_C            equ  03H                    

int38             equ  38H                    
nmi               equ  66H                    

blksiz            equ  4096                   ; CP/M allocation size
hstsiz            equ  512                    ; host disk sector size
hstspt            equ  32                     ; host disk sectors/trk
hstblk            equ  hstsiz/128             ; CP/M sects/host buff
cpmspt            equ  hstblk * hstspt        ; CP/M sectors/track
secmsk            equ  hstblk-1               ; sector mask
                  ;compute sector mask
;secshf      equ    2      ;log2(hstblk)

wrall             equ  0                      ; write to allocated
wrdir             equ  1                      ; write to directory
wrual             equ  2                      ; write to unallocated



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
CF_COMMAND        equ  17H                    
CF_LBA0           equ  13H                    
CF_LBA1           equ  14H                    
CF_LBA2           equ  15H                    
CF_LBA3           equ  16H                    

;CF Features
CF_8BIT           equ  1                      
CF_NOCACHE        equ  082H                   
;CF Commands
CF_READ_SEC       equ  020H                   
CF_WRITE_SEC      equ  030H                   
CF_SET_FEAT       equ  0EFH                   

LF                equ  0AH                    ; line feed
FF                equ  0CH                    ; form feed
CR                equ  0DH                    ; carriage RETurn

;================================================================================================

                  org  bios                   ; BIOS origin.

;================================================================================================
; BIOS jump table.
;================================================================================================
                  JP boot                     ; 0 Initialize.
wboote:           JP wboot                    ; 1 Warm boot.
                  JP const                    ; 2 Console status.
                  JP conin                    ; 3 Console input.
                  JP conout                   ; 4 Console OUTput.
                  JP list                     ; 5 List OUTput.
                  JP punch                    ; 6 punch OUTput.
                  JP reader                   ; 7 Reader input.
                  JP home                     ; 8 Home disk.
                  JP seldsk                   ; 9 Select disk.
                  JP settrk                   ; 10 Select track.
                  JP setsec                   ; 11 Select sector.
                  JP setdma                   ; 12 Set DMA address.
                  JP read                     ; 13 Read 128 bytes.
                  JP write                    ; 14 Write 128 bytes.
                  JP listst                   ; 15 List status.
                  JP sectran                  ; 16 Sector translate.

;================================================================================================
; Disk parameter headers for disk 0 to 15
;================================================================================================
dpbase:
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb0,0000h,alv00
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv01
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv02
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv03
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv04
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv05
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpb,0000h,alv06
                  dw  0000h,0000h,0000h,0000h,dirbuf,dpbLast,0000h,alv07

; First drive has a reserved track for CP/M
dpb0:
                  dw  128                     ; SPT - sectors per track
                  db  5                       ; BSH - block shift factor
                  db  31                      ; BLM - block mask
                  db  1                       ; EXM - Extent mask
                  dw  2043                    ; (2047-4) DSM - Storage size (blocks - 1)
                  dw  511                     ; DRM - Number of directory entries - 1
                  db  240                     ; AL0 - 1 bit set per directory block
                  db  0                       ; AL1 -            "
                  dw  0                       ; CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
                  dw  1                       ; OFF - Reserved tracks

dpb:
                  dw  128                     ; SPT - sectors per track
                  db  5                       ; BSH - block shift factor
                  db  31                      ; BLM - block mask
                  db  1                       ; EXM - Extent mask
                  dw  2047                    ; DSM - Storage size (blocks - 1)
                  dw  511                     ; DRM - Number of directory entries - 1
                  db  240                     ; AL0 - 1 bit set per directory block
                  db  0                       ; AL1 -            "
                  dw  0                       ; CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
                  dw  0                       ; OFF - Reserved tracks

; Last drive is smaller because CF is never full 64MB or 128MB
dpbLast:
                  dw  128                     ; SPT - sectors per track
                  db  5                       ; BSH - block shift factor
                  db  31                      ; BLM - block mask
                  db  1                       ; EXM - Extent mask
                  dw  1279                    ; DSM - Storage size (blocks - 1)  ; 511 = 2MB (for 128MB card), 1279 = 5MB (for 64MB card)
                  dw  511                     ; DRM - Number of directory entries - 1
                  db  240                     ; AL0 - 1 bit set per directory block
                  db  0                       ; AL1 -            "
                  dw  0                       ; CKS - DIR check vector size (DRM+1)/4 (0=fixed disk)
                  dw  0                       ; OFF - Reserved tracks

;================================================================================================
; Cold boot
;================================================================================================

boot:
                  DI                          ; Disable interrupts.
                  LD SP,biosstack             ; Set default stack.

;      Turn off ROM

                  LD A,01H
                  OUT (38H),A

;   Initialise SIO

                  LD A,00
                  OUT (SIOA_C),A
                  LD A,18H
                  OUT (SIOA_C),A

                  LD A,04H
                  OUT (SIOA_C),A
                  LD A,0C4H
                  OUT (SIOA_C),A

                  LD A,01H
                  OUT (SIOA_C),A
                  LD A,18H
                  OUT (SIOA_C),A
                  
                  LD A,03H
                  OUT (SIOA_C),A
                  LD A,0E1H
                  OUT (SIOA_C),A

                  LD A,05H
                  OUT (SIOA_C),A
                  LD A,RTS_LOW
                  OUT (SIOA_C),A

                  LD A,00
                  OUT (SIOB_C),A
                  LD A,18h
                  OUT (SIOB_C),A

                  LD A,04H
                  OUT (SIOB_C),A
                  LD A,0C4H
                  OUT (SIOB_C),A

                  LD A,01H
                  OUT (SIOB_C),A
                  LD A,18H
                  OUT (SIOB_C),A

                  LD A,02H
                  OUT (SIOB_C),A
                  LD A,0E0H                    ; INTERRUPT VECTOR address
                  OUT (SIOB_C),A
                  
                  LD A,03H
                  OUT (SIOB_C),A
                  LD A,0E1H
                  OUT (SIOB_C),A

                  LD A,05H
                  OUT (SIOB_C),A
                  LD A,RTS_LOW
                  OUT (SIOB_C),A

                  ;Interrupt vector in page FF
                  LD A,0FFH
                  LD I,A

                  CALL printInline
                  db  FF
                  db  "Z80 CP/M BIOS 1.0 by G. Searle 2007-13"
                  db  CR,LF
                  db  CR,LF
                  db  "CP/M 2.2 "
                  db  "Copyright"
                  db  " 1979 (c) by Digital Research"
                  db  CR,LF,0


                  CALL cfWait
                  LD A,CF_8BIT                ; Set IDE to be 8bit
                  OUT (CF_FEATURES),A
                  LD A,CF_SET_FEAT
                  OUT (CF_COMMAND),A

                  CALL cfWait
                  LD A,CF_NOCACHE             ; No write cache
                  OUT (CF_FEATURES),A
                  LD A,CF_SET_FEAT
                  OUT (CF_COMMAND),A

                  XOR a                       ; Clear I/O & drive bytes.
                  LD (userdrv),A

                  LD (serABufUsed),A
                  LD (serBBufUsed),A
                  LD HL,serABuf
                  LD (serAInPtr),HL
                  LD (serARdPtr),HL

                  LD HL,serBBuf
                  LD (serBInPtr),HL
                  LD (serBRdPtr),HL

                  JP gocpm

;================================================================================================
; Warm boot
;================================================================================================
wboot:
                  DI                          ; Disable interrupts.
                  LD SP,biosstack             ; Set default stack.



                  ;Interrupt vector in page FF
                  LD A,0FFH
                  LD I,A


                  LD B,11                     ; Number of sectors to reload

                  LD A,0
                  LD (hstsec),A
                  LD HL,ccp
rdSectors:

                  CALL cfWait

                  LD A,(hstsec)
                  OUT (CF_LBA0),A
                  LD A,0
                  OUT (CF_LBA1),A
                  OUT (CF_LBA2),A
                  LD a,0E0H
                  OUT (CF_LBA3),A
                  LD A,1
                  OUT (CF_SECCOUNT),A

                  PUSH BC

                  CALL cfWait

                  LD A,CF_READ_SEC
                  OUT (CF_COMMAND),A

                  CALL cfWait

                  LD c,4
rd4secs512:
                  LD b,128
rdByte512:
                  in A,(CF_DATA)
                  LD (HL),A
                  iNC HL
                  dec b
                  JR NZ, rdByte512
                  dec c
                  JR NZ,rd4secs512

                  POP BC

                  LD A,(hstsec)
                  INC A
                  LD (hstsec),A

                  djnz rdSectors


;================================================================================================
; Common code for cold and warm boot
;================================================================================================

gocpm:
                  xor a                       ; 0 to accumulator
                  ld (hstact),a               ; host buffer inactive
                  ld (unacnt),a               ; clear unalloc count

                  LD HL,serialInt             ; address of serial interrupt.
                  LD (40H),HL

                  LD HL,tpabuf                ; address of BIOS DMA buffer.
                  LD (dmaAddr),HL
                  LD A,0C3H                   ; Opcode for 'JP'.
                  LD (00H),A                  ; Load at start of RAM.
                  LD HL,wboote                ; address of jump for a warm boot.
                  LD (01H),HL
                  LD (05H),A                  ; Opcode for 'JP'.
                  LD HL,bdos                  ; address of jump for the BDOS.
                  LD (06H),HL
                  LD A,(userdrv)              ; Save new drive number (0).
                  LD c,A                      ; Pass drive number in C.

                  IM 2
                  EI                          ; Enable interrupts

                  JP ccp                      ; Start CP/M by jumping to the CCP.

;================================================================================================
; Console I/O routines
;================================================================================================

serialInt:        PUSH AF                     
                  PUSH HL

                  ;Check if there is a char in channel A
                  ;If not, there is a char in channel B
                  SUB A
                  OUT (SIOA_C),A
                  IN A,(SIOA_C)               ; Status byte D2=TX Buff Empty, D0=RX char ready
                  RRCA                        ; Rotates RX status into Carry Flag,
                  JR NC, serialIntB

serialIntA:
                  LD HL,(serAInPtr)
                  INC HL
                  LD A,L
                  CP (serABuf+SER_BUFSIZE) & 0FFH
                  JR NZ, notAWrap
                  LD HL,serABuf
notAWrap:
                  LD (serAInPtr),HL
                  IN A,(SIOA_D)
                  LD (HL),A

                  LD A,(serABufUsed)
                  INC A
                  LD (serABufUsed),A
                  CP SER_FULLSIZE
                  JR C,rtsA0
                  LD A,05H
                  OUT (SIOA_C),A
                  LD A,RTS_HIGH
                  OUT (SIOA_C),A
rtsA0:
                  POP HL
                  POP AF
                  EI
                  RETI

serialIntB:
                  LD HL,(serBInPtr)
                  INC HL
                  LD A,L
                  CP (serBBuf+SER_BUFSIZE) & 0FFH
                  JR NZ, notBWrap
                  LD HL,serBBuf
notBWrap:
                  LD (serBInPtr),HL
                  IN A,(SIOB_D)
                  LD (HL),A

                  LD A,(serBBufUsed)
                  INC A
                  LD (serBBufUsed),A
                  CP SER_FULLSIZE
                  JR C,rtsB0
                  LD A,05H
                  OUT (SIOB_C),A
                  LD A,RTS_HIGH
                  OUT (SIOB_C),A
rtsB0:
                  POP HL
                  POP AF
                  EI
                  RETI

;------------------------------------------------------------------------------------------------
const:
                  LD A,(iobyte)
                  AND 00001011b               ; Mask off console and high bit of reader
                  CP 00001010b                ; redirected to reader on UR1/2 (Serial A)
                  JR Z,constA
                  CP 00000010b                ; redirected to reader on TTY/RDR (Serial B)
                  JR Z,constB

                  AND 03H                     ; remove the reader from the mask - only console bits then remain
                  CP 01H
                  JR NZ,constB
constA:
                  PUSH HL
                  LD A,(serABufUsed)
                  CP 00H
                  JR Z, dataAEmpty
                  LD A,0FFH
                  POP HL
                  RET
dataAEmpty:
                  LD A,0
                  POP HL
                  RET


constB:
                  PUSH HL
                  LD A,(serBBufUsed)
                  CP 00
                  JR Z, dataBEmpty
                  LD A,0FFH
                  POP HL
                  RET
dataBEmpty:
                  LD A,0
                  POP HL
                  RET

;------------------------------------------------------------------------------------------------
reader:
                  PUSH HL
                  PUSH AF
reader2:          LD A,(iobyte)               
                  AND 08H
                  CP 08H
                  JR NZ,coninB
                  JR coninA
;------------------------------------------------------------------------------------------------
conin:
                  PUSH HL
                  PUSH AF
                  LD A,(iobyte)
                  AND 03H
                  CP 02H
                  JR Z,reader2                ; "BAT: " redirect
                  CP 01H
                  JR NZ,coninB
                  

coninA:
                  POP AF
waitForCharA:
                  LD A,(serABufUsed)
                  CP 00H
                  JR Z, waitForCharA
                  LD HL,(serARdPtr)
                  INC HL
                  LD A,L
                  CP (serABuf+SER_BUFSIZE) & 0FFH
                  JR NZ, notRdWrapA
                  LD HL,serABuf
notRdWrapA:
                  DI
                  LD (serARdPtr),HL

                  LD A,(serABufUsed)
                  DEC A
                  LD (serABufUsed),A

                  CP SER_EMPTYSIZE
                  JR NC,rtsA1
                  LD A,05H
                  OUT (SIOA_C),A
                  LD A,RTS_LOW
                  OUT (SIOA_C),A
rtsA1:
                  LD A,(HL)
                  EI

                  POP HL

                  RET                         ; Char ready in A


coninB:
                  POP AF
waitForCharB:
                  LD A,(serBBufUsed)
                  CP 00H
                  JR Z, waitForCharB
                  LD HL,(serBRdPtr)
                  INC HL
                  LD A,L
                  CP (serBBuf+SER_BUFSIZE) & 0FFH
                  JR NZ, notRdWrapB
                  LD HL,serBBuf
notRdWrapB:
                  DI
                  LD (serBRdPtr),HL

                  LD A,(serBBufUsed)
                  DEC A
                  LD (serBBufUsed),A

                  CP SER_EMPTYSIZE
                  JR NC,rtsB1
                  LD A,05H
                  OUT (SIOB_C),A
                  LD A,RTS_LOW
                  OUT (SIOB_C),A
rtsB1:
                  LD A,(HL)
                  EI

                  POP HL

                  RET                         ; Char ready in A

;------------------------------------------------------------------------------------------------
list:             PUSH AF                     ; Store character
list2:            LD A,(iobyte)               
                  AND 0C0H
                  CP 40H
                  JR NZ,conoutB1
                  JR conoutA1

;------------------------------------------------------------------------------------------------
punch:            PUSH AF                     ; Store character
                  LD A,(iobyte)
                  AND 20H
                  CP 20H
                  JR NZ,conoutB1
                  JR conoutA1

;------------------------------------------------------------------------------------------------
conout:           PUSH AF                     ; Store character
                  LD A,(iobyte)
                  AND 03H
                  CP 02H
                  JR Z,list2                  ; "BAT: " redirect
                  CP 01H
                  JR NZ,conoutB1

conoutA1:         CALL CKSIOA                 ; See if SIO channel B is finished transmitting
                  JR Z,conoutA1               ; Loop until SIO flag signals ready
                  LD A,C
                  OUT (SIOA_D),A              ; OUTput the character
                  POP AF                      ; RETrieve character
                  RET

conoutB1:         CALL CKSIOB                 ; See if SIO channel B is finished transmitting
                  JR Z,conoutB1               ; Loop until SIO flag signals ready
                  LD A,C
                  OUT (SIOB_D),A              ; OUTput the character
                  POP AF                      ; RETrieve character
                  RET

;------------------------------------------------------------------------------------------------
CKSIOA
                  SUB A
                  OUT (SIOA_C),A
                  IN A,(SIOA_C)               ; Status byte D2=TX Buff Empty, D0=RX char ready
                  RRCA                        ; Rotates RX status into Carry Flag,
                  BIT 1,A                     ; Set Zero flag if still transmitting character
                  RET

CKSIOB
                  SUB A
                  OUT (SIOB_C),A
                  IN A,(SIOB_C)               ; Status byte D2=TX Buff Empty, D0=RX char ready
                  RRCA                        ; Rotates RX status into Carry Flag,
                  BIT 1,A                     ; Set Zero flag if still transmitting character
                  RET

;------------------------------------------------------------------------------------------------
listst:           LD A,0FFH                    ; Return list status of 0xFF (ready).
                  RET

;================================================================================================
; Disk processing entry points
;================================================================================================

seldsk:
                  LD HL,0000H
                  LD A,C
                  CP 8                        ; 16 for 128MB disk, 8 for 64MB disk
                  jr C,chgdsk                 ; if invalid drive will give BDOS error
                  LD A,(userdrv)              ; so set the drive back to a:
                  CP C                        ; If the default disk is not the same as the
                  RET NZ                      ; selected drive then return,
                  XOR A                       ; else reset default back to a:
                  LD (userdrv),A              ; otherwise will be stuck in a loop
                  LD (sekdsk),A
                  ret

chgdsk:           LD (sekdsk),A               
                  RLC a                       ; *2
                  RLC a                       ; *4
                  RLC a                       ; *8
                  RLC a                       ; *16
                  LD HL,dpbase
                  LD b,0
                  LD c,A
                  ADD HL,BC

                  RET

;------------------------------------------------------------------------------------------------
home:
                  ld a,(hstwrt)               ; check for pending write
                  or a
                  jr nz,homed
                  ld (hstact),a               ; clear host active flag
homed:
                  LD BC,0000h

;------------------------------------------------------------------------------------------------
settrk:           LD (sektrk),BC              ; Set track passed from BDOS in register BC.
                  RET

;------------------------------------------------------------------------------------------------
setsec:           LD (seksec),BC              ; Set sector passed from BDOS in register BC.
                  RET

;------------------------------------------------------------------------------------------------
setdma:           LD (dmaAddr),BC             ; Set DMA address given by registers BC.
                  RET

;------------------------------------------------------------------------------------------------
sectran:          PUSH BC                     
                  POP HL
                  RET

;------------------------------------------------------------------------------------------------
read:
                  ;read the selected CP/M sector
                  xor a
                  ld (unacnt),a
                  ld a,1
                  ld (readop),a               ; read operation
                  ld (rsflag),a               ; must read data
                  ld a,wrual
                  ld (wrtype),a               ; treat as unalloc
                  jp rwoper                   ; to perform the read


;------------------------------------------------------------------------------------------------
write:
                  ;write the selected CP/M sector
                  xor a                       ; 0 to accumulator
                  ld (readop),a               ; not a read operation
                  ld a,c                      ; write type in c
                  ld (wrtype),a
                  cp wrual                    ; write unallocated?
                  jr nz,chkuna                ; check for unalloc
;
;      write to unallocated, set parameters
                  ld a,blksiz/128             ; next unalloc recs
                  ld (unacnt),a
                  ld a,(sekdsk)               ; disk to seek
                  ld (unadsk),a               ; unadsk = sekdsk
                  ld hl,(sektrk)
                  ld (unatrk),hl              ; unatrk = sectrk
                  ld a,(seksec)
                  ld (unasec),a               ; unasec = seksec
;
chkuna:
;      check for write to unallocated sector
                  ld a,(unacnt)               ; any unalloc remain?
                  or a
                  jr z,alloc                  ; skip if not
;
;      more unallocated records remain
                  dec a                       ; unacnt = unacnt-1
                  ld (unacnt),a
                  ld a,(sekdsk)               ; same disk?
                  ld hl,unadsk
                  cp (hl)                     ; sekdsk = unadsk?
                  jp nz,alloc                 ; skip if not
;
;      disks are the same
                  ld hl,unatrk
                  call sektrkcmp              ; sektrk = unatrk?
                  jp nz,alloc                 ; skip if not
;
;      tracks are the same
                  ld a,(seksec)               ; same sector?
                  ld hl,unasec
                  cp (hl)                     ; seksec = unasec?
                  jp nz,alloc                 ; skip if not
;
;      match, move to next sector for future ref
                  inc (hl)                    ; unasec = unasec+1
                  ld a,(hl)                   ; end of track?
                  cp cpmspt                   ; count CP/M sectors
                  jr c,noovf                  ; skip if no overflow
;
;      overflow to next track
                  ld (hl),0                   ; unasec = 0
                  ld hl,(unatrk)
                  inc hl
                  ld (unatrk),hl              ; unatrk = unatrk+1
;
noovf:
                  ;match found, mark as unnecessary read
                  xor a                       ; 0 to accumulator
                  ld (rsflag),a               ; rsflag = 0
                  jr rwoper                   ; to perform the write
;
alloc:
                  ;not an unallocated record, requires pre-read
                  xor a                       ; 0 to accum
                  ld (unacnt),a               ; unacnt = 0
                  inc a                       ; 1 to accum
                  ld (rsflag),a               ; rsflag = 1

;------------------------------------------------------------------------------------------------
rwoper:
                  ;enter here to perform the read/write
                  xor a                       ; zero to accum
                  ld (erflag),a               ; no errors (yet)
                  ld a,(seksec)               ; compute host sector
                  or a                        ; carry = 0
                  rra                         ; shift right
                  or a                        ; carry = 0
                  rra                         ; shift right
                  ld (sekhst),a               ; host sector to seek
;
;      active host sector?
                  ld hl,hstact                ; host active flag
                  ld a,(hl)
                  ld (hl),1                   ; always becomes 1
                  or a                        ; was it already?
                  jr z,filhst                 ; fill host if not
;
;      host buffer active, same as seek buffer?
                  ld a,(sekdsk)
                  ld hl,hstdsk                ; same disk?
                  cp (hl)                     ; sekdsk = hstdsk?
                  jr nz,nomatch
;
;      same disk, same track?
                  ld hl,hsttrk
                  call sektrkcmp              ; sektrk = hsttrk?
                  jr nz,nomatch
;
;      same disk, same track, same buffer?
                  ld a,(sekhst)
                  ld hl,hstsec                ; sekhst = hstsec?
                  cp (hl)
                  jr z,match                  ; skip if match
;
nomatch:
                  ;proper disk, but not correct sector
                  ld a,(hstwrt)               ; host written?
                  or a
                  call nz,writehst            ; clear host buff
;
filhst:
                  ;may have to fill the host buffer
                  ld a,(sekdsk)
                  ld (hstdsk),a
                  ld hl,(sektrk)
                  ld (hsttrk),hl
                  ld a,(sekhst)
                  ld (hstsec),a
                  ld a,(rsflag)               ; need to read?
                  or a
                  call nz,readhst             ; yes, if 1
                  xor a                       ; 0 to accum
                  ld (hstwrt),a               ; no pending write
;
match:
                  ;copy data to or from buffer
                  ld a,(seksec)               ; mask buffer number
                  and secmsk                  ; least signif bits
                  ld l,a                      ; ready to shift
                  ld h,0                      ; double count
                  add hl,hl
                  add hl,hl
                  add hl,hl
                  add hl,hl
                  add hl,hl
                  add hl,hl
                  add hl,hl
;      hl has relative host buffer address
                  ld de,hstbuf
                  add hl,de                   ; hl = host address
                  ex de,hl                    ; now in DE
                  ld hl,(dmaAddr)             ; get/put CP/M data
                  ld c,128                    ; length of move
                  ld a,(readop)               ; which way?
                  or a
                  jr nz,rwmove                ; skip if read
;
;   write operation, mark and switch direction
                  ld a,1
                  ld (hstwrt),a               ; hstwrt = 1
                  ex de,hl                    ; source/dest swap
;
rwmove:
                  ;C initially 128, DE is source, HL is dest
                  ld a,(de)                   ; source character
                  inc de
                  ld (hl),a                   ; to dest
                  inc hl
                  dec c                       ; loop 128 times
                  jr nz,rwmove
;
;      data has been moved to/from host buffer
                  ld a,(wrtype)               ; write type
                  cp wrdir                    ; to directory?
                  ld a,(erflag)               ; in case of errors
                  ret nz                      ; no further processing
;
;      clear host buffer for directory write
                  or a                        ; errors?
                  ret nz                      ; skip if so
                  xor a                       ; 0 to accum
                  ld (hstwrt),a               ; buffer written
                  call writehst
                  ld a,(erflag)
                  ret

;------------------------------------------------------------------------------------------------
;Utility subroutine for 16-bit compare
sektrkcmp:
                  ;HL = .unatrk or .hsttrk, compare with sektrk
                  ex de,hl
                  ld hl,sektrk
                  ld a,(de)                   ; low byte compare
                  cp (HL)                     ; same?
                  ret nz                      ; return if not
;      low bytes equal, test high 1s
                  inc de
                  inc hl
                  ld a,(de)
                  cp (hl)                     ; sets flags
                  ret

;================================================================================================
; Convert track/head/sector into LBA for physical access to the disk
;================================================================================================
setLBAaddr:
                  LD HL,(hsttrk)
                  RLC L
                  RLC L
                  RLC L
                  RLC L
                  RLC L
                  LD A,L
                  AND 0E0H
                  LD L,A
                  LD A,(hstsec)
                  ADD A,L
                  LD (lba0),A

                  LD HL,(hsttrk)
                  RRC L
                  RRC L
                  RRC L
                  LD A,L
                  AND 01FH
                  LD L,A
                  RLC H
                  RLC H
                  RLC H
                  RLC H
                  RLC H
                  LD A,H
                  AND 020H
                  LD H,A
                  LD A,(hstdsk)
                  RLC a
                  RLC a
                  RLC a
                  RLC a
                  RLC a
                  RLC a
                  AND 0C0H
                  ADD A,H
                  ADD A,L
                  LD (lba1),A
                  

                  LD A,(hstdsk)
                  RRC A
                  RRC A
                  AND 03H
                  LD (lba2),A

; LBA Mode using drive 0 = E0
                  LD a,0E0H
                  LD (lba3),A


                  LD A,(lba0)
                  OUT (CF_LBA0),A

                  LD A,(lba1)
                  OUT (CF_LBA1),A

                  LD A,(lba2)
                  OUT (CF_LBA2),A

                  LD A,(lba3)
                  OUT (CF_LBA3),A

                  LD A,1
                  OUT (CF_SECCOUNT),A

                  RET

;================================================================================================
; Read physical sector from host
;================================================================================================

readhst:
                  PUSH AF
                  PUSH BC
                  PUSH HL

                  CALL cfWait

                  CALL setLBAaddr

                  LD A,CF_READ_SEC
                  OUT (CF_COMMAND),A

                  CALL cfWait

                  LD c,4
                  LD HL,hstbuf
rd4secs:
                  LD b,128
rdByte:
                  in A,(CF_DATA)
                  LD (HL),A
                  iNC HL
                  dec b
                  JR NZ, rdByte
                  dec c
                  JR NZ,rd4secs

                  POP HL
                  POP BC
                  POP AF

                  XOR a
                  ld (erflag),a
                  RET

;================================================================================================
; Write physical sector to host
;================================================================================================

writehst:
                  PUSH AF
                  PUSH BC
                  PUSH HL


                  CALL cfWait

                  CALL setLBAaddr

                  LD A,CF_WRITE_SEC
                  OUT (CF_COMMAND),A

                  CALL cfWait

                  LD c,4
                  LD HL,hstbuf
wr4secs:
                  LD b,128
wrByte:           LD A,(HL)                   
                  OUT (CF_DATA),A
                  iNC HL
                  dec b
                  JR NZ, wrByte

                  dec c
                  JR NZ,wr4secs

                  POP HL
                  POP BC
                  POP AF

                  XOR a
                  ld (erflag),a
                  RET

;================================================================================================
; Wait for disk to be ready (busy=0,ready=1)
;================================================================================================
cfWait:
                  PUSH AF
cfWait1:
                  in A,(CF_STATUS)
                  AND 080H
                  cp 080H
                  JR Z,cfWait1
                  POP AF
                  RET

;================================================================================================
; Utilities
;================================================================================================

printInline:
                  EX (SP),HL                  ; PUSH HL and put RET address into HL
                  PUSH AF
                  PUSH BC
nextILChar:       LD A,(HL)                   
                  CP 0
                  JR Z,endOfPrint
                  LD C,A
                  CALL conout                 ; Print to TTY
                  iNC HL
                  JR nextILChar
endOfPrint:       INC HL                      ; Get past "null" terminator
                  POP BC
                  POP AF
                  EX (SP),HL                  ; PUSH new RET address on stack and restore HL
                  RET

;================================================================================================
; Data storage
;================================================================================================

dirbuf:           ds  128                     ; scratch directory area
alv00:            ds  257                     ; allocation vector 0
alv01:            ds  257                     ; allocation vector 1
alv02:            ds  257                     ; allocation vector 2
alv03:            ds  257                     ; allocation vector 3
alv04:            ds  257                     ; allocation vector 4
alv05:            ds  257                     ; allocation vector 5
alv06:            ds  257                     ; allocation vector 6
alv07:            ds  257                     ; allocation vector 7

lba0              db  00h                     
lba1              db  00h                     
lba2              db  00h                     
lba3              db  00h                     

                  ds 020h                    ; Start of BIOS stack area.
biosstack:        equ  $                      

sekdsk:           ds  1                       ; seek disk number
sektrk:           ds  2                       ; seek track number
seksec:           ds  2                       ; seek sector number
;
hstdsk:           ds  1                       ; host disk number
hsttrk:           ds  2                       ; host track number
hstsec:           ds  1                       ; host sector number
;
sekhst:           ds  1                       ; seek shr secshf
hstact:           ds  1                       ; host active flag
hstwrt:           ds  1                       ; host written flag
;
unacnt:           ds  1                       ; unalloc rec cnt
unadsk:           ds  1                       ; last unalloc disk
unatrk:           ds  2                       ; last unalloc track
unasec:           ds  1                       ; last unalloc sector
;
erflag:           ds  1                       ; error reporting
rsflag:           ds  1                       ; read sector flag
readop:           ds  1                       ; 1 if read operation
wrtype:           ds  1                       ; write operation type
dmaAddr:          ds  2                       ; last dma address
hstbuf:           ds  512                     ; host buffer

hstBufEnd:        equ  $                      

serABuf:          ds  SER_BUFSIZE             ; SIO A Serial buffer
serAInPtr         dw  00h                     
serARdPtr         dw  00h                     
serABufUsed       db  00h                     
serBBuf:          ds  SER_BUFSIZE             ; SIO B Serial buffer
serBInPtr         dw  00h                     
serBRdPtr         dw  00h                     
serBBufUsed       db  00h                     

serialVarsEnd:    equ  $                      


biosEnd:          equ  $                      

; Disable the ROM, pop the active IO port from the stack (supplied by monitor),
; then start CP/M
popAndRun:
                  LD A,01H
                  OUT (38H),A

                  POP AF
                  CP 01H
                  JR Z,consoleAtB
                  LD A,01H                    ; (List is TTY: , Punch is TTY: , Reader is TTY: , Console is CRT: )
                  JR setIOByte
consoleAtB:       LD A,00H                    ; (List is TTY: , Punch is TTY: , Reader is TTY: , Console is TTY: )
setIOByte:        LD (iobyte),A               
                  JP bios

;   IM 2 lookup for serial interrupt

                  org  0FFE0H
                  dw  serialInt


;=================================================================================
; Relocate TPA area from 4100 to 0100 then start CP/M
; Used to manually transfer a loaded program after CP/M was previously loaded
;=================================================================================

                  org  0FFE8H
                  LD A,01H
                  OUT (38H),A

                  LD HL,4100H
                  LD DE,0100H
                  LD BC,8F00H
                  LDIR
                  JP bios

;=================================================================================
; Normal start CP/M vector
;=================================================================================

                  org  0FFFEH
                  dw  popAndRun

                  end

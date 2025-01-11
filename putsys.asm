               PAGE 0                  ; suppress page headings in ASW listing file
               cpu z80            
;==================================================================================
; Contents of this file are copyright Grant Searle
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

loadAddr          equ  0D000H                 
numSecs           equ  24                     ; Number of 512 sectors to be loaded


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

                  org  5000H                  ; Loader origin.

                  CALL printInline
                  db    "CP/M System Transfer by G. Searle 2012"
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

                  LD B,numSecs

                  LD A,0
                  LD (secNo),A
                  LD HL,loadAddr
                  LD (dmaAddr),HL

processSectors:   CALL cfWait
                  LD A,(secNo)
                  OUT (CF_LBA0),A
                  LD A,0
                  OUT (CF_LBA1),A
                  OUT (CF_LBA2),A
                  LD a,0E0H
                  OUT (CF_LBA3),A
                  LD A,1
                  OUT (CF_SECCOUNT),A
                  call write
                  LD DE,0200H
                  LD HL,(dmaAddr)
                  ADD HL,DE
                  LD (dmaAddr),HL
                  LD A,(secNo)
                  INC A
                  LD (secNo),A
                  djnz processSectors
                  CALL printInline
                  db  CR,LF
                  db  "System transfer complete"
                  db  CR,LF,0
                  RET

;================================================================================================
; Write physical sector to host
;================================================================================================
write:            PUSH AF
                  PUSH BC
                  PUSH HL
                  CALL cfWait
                  LD A,CF_WRITE_SEC
                  OUT (CF_COMMAND),A
                  CALL cfWait
                  LD c,4
                  LD HL,(dmaAddr)
wr4secs:          LD b,128
wrByte:           LD A,(HL)                   
                  nop
                  nop
                  OUT (CF_DATA),A
                  iNC HL
                  dec b
                  JR NZ, wrByte

                  dec c
                  JR NZ,wr4secs

                  POP HL
                  POP BC
                  POP AF
                  RET

;================================================================================================
; Wait for disk to be ready (busy=0,ready=1)
;================================================================================================
cfWait:           PUSH AF
cfWait1:          in A,(CF_STATUS)
                  AND 080H
                  cp 080H
                  JR Z,cfWait1
                  POP AF
                  RET

;================================================================================================
; Utilities
;================================================================================================
printInline:      EX (SP),HL                  ; PUSH HL and put RET ADDress into HL
                  PUSH AF
                  PUSH BC
nextILChar:       LD A,(HL)                   
                  CP 0
                  JR Z,endOfPrint
                  RST 08H
                  INC HL
                  JR nextILChar
endOfPrint:       INC HL                      ; Get past "null" terminator
                  POP BC
                  POP AF
                  EX (SP),HL                  ; PUSH new RET ADDress on stack and restore HL
                  RET

dmaAddr           dw 0                       
secNo             db 0                       

                  END 5000H

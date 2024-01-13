;==============================================================
; Variables (MEMORY MAP)
;==============================================================
; We need to store values in RAM
; There are a few ways to create a memory map,
; but the cleanest, simplest, and easiest to maintain method
; uses the assembler's "RS" keywords. RSSET begins a new table of
; offsets starting from any other offset (here we're starting at
; 0x00FF0000, the start of RAM), and allows us to add named entries
; of any size for the "variables". We can then read/write these
; variables using the offsets' labels.
;==============================================================
	RSSET $00FF0000			; Start a new offset table from beginning of RAM
Cursor_X rs.b 1
Cursor_Y rs.b 1

shiftDown rs.b 1 ; keep track of if shift is held down
doScroll rs.b 1 ; are we scrolling? 1 if yes!
vscroll_amount	rs.w 1 ; keep track of how much we scrolled

ScancodeBuffer rs.b 12 ; 12 nibbles stored in the lower nibble of the bytes (the more significant nibble is $0)
InputBuffer	rs.b 128

;  extra variables  ;
printStrAddr rs.l 1 ;
printStrLen  rs.w 1 ;
demoIndex	 rs.l 1 ;
demoTimer	 rs.l 1 ;

freeRAM		 rs.l 1
	RSRESET

;==============================================================
; Constants
;==============================================================

IoCtrl1      equ $A10009  ; I/O control port 1P
IoCtrl2      equ $A1000B  ; I/O control port 2P
IoCtrlExt    equ $A1000D  ; I/O control port modem
IoData1      equ $A10003  ; I/O data port 1P
IoData2      equ $A10005  ; I/O data port 2P
IoDataExt    equ $A10007  ; I/O data port modem


; VDP port addresses
VDP_data		equ	$C00000	; VDP data, R/W word or longword access only
VDP_control		equ	$C00004	; VDP control, word or longword writes only

; VDP commands
VDP_cmd_vram_write		equ $40000000
VDP_cmd_vram_read		equ $00000000
VDP_cmd_cram_write		equ $C0000000
VDP_cmd_cram_read		equ $00000020
vdp_cmd_vsram_write		equ $40000010	; Vertical Scroll RAM address

; VDP memory addresses
; according to VDP registers 0x2 and 0x4 (see table above)
vram_addr_tiles			equ $0000
vram_addr_plane_a		equ $C000
vram_addr_plane_b		equ $E000

; Screen width and height (in pixels)
vdp_screen_width		equ $0140
vdp_screen_height		equ $00F0

; The plane width and height (in tiles)
; according to VDP register 0x10 (see table above)
vdp_plane_width			equ $40
vdp_plane_height		equ $20

; The size of a word and longword
size_word				equ 2
size_long				equ 4

; The size of one palette (in bytes, words, and longwords)
size_palette_b			equ $20
size_palette_w			equ size_palette_b/size_word
size_palette_l			equ size_palette_b/size_long

; The size of one graphics tile (in bytes, words, and longwords)
size_tile_b				equ $20
size_tile_w				equ size_tile_b/size_word
size_tile_l				equ size_tile_b/size_long

; Some ascii codes
CH_ESC	 		equ $1B
CH_BACKSPACE	equ $08
CH_BACKSLASH 	equ $5C
CH_CR        	equ $0A
CH_SPACE		equ $20


; Hardware version address
hardware_ver_address	equ $00A10001

; TMSS
tmss_address			equ $00A14000
tmss_signature			equ 'SEGA'

;==============================================================
; VRAM WRITE MACROS
;==============================================================
; Some utility macros to help generate addresses and commands for
; writing data to video memory, since they're tricky (and
; error prone) to calculate manually.
; The resulting command and address is written to the VDP's
; control port, ready to accept data in the data port.
;==============================================================
	
; Set the VRAM (video RAM) address to write to next
	macro SetVRAMWrite
		move.l #(vdp_cmd_vram_write)|((\1)&$3FFF)<<16|(\1)>>14,vdp_control
	endm
; Set the CRAM (colour RAM) address to write to next
	macro SetCRAMWrite
		move.l #(vdp_cmd_cram_write)|((\1)&$3FFF)<<16|(\1)>>14,vdp_control
	endm
; Set the VSRAM (vertical scroll RAM) address to write to next
	macro SetVSRAMWrite
		move.l #(vdp_cmd_vsram_write)|((\1)&$3FFF)<<16|(\1)>>14,vdp_control
	endm

;==============================================================
; Z80 MACROS
;==============================================================
; We can't access Z80 RAM while it's running, but we can request
; its bus (which will pause the Z80). Note that depending what
; the Z80 is doing, it may take a bit until the bus is free.
; So once you write $100 to Z80BusReq, you need to keep
; reading back from it until it also returns $100
; (at which point the Z80 has let go of the bus).
;==============================================================
; We are not using the z80
Z80Ram     equ $A00000  ; Where Z80 RAM starts
Z80BusReq  equ $A11100  ; Z80 bus request line
Z80Reset   equ $A11200  ; Z80 reset line

	macro PauseZ80
		move.w  #$100,(Z80BusReq)
	endm
	macro WaitZ80
		btst    #0,(Z80BusReq)
		bne.s   WaitZ80
    endm
	macro FastPauseZ80
    	move.w  #$100,(Z80BusReq)
    endm
	macro ResumeZ80
    	move.w  #$000,(Z80BusReq)
    endm

ROM_Start:

;==============================================================
; CPU VECTOR TABLE
;==============================================================
; A table of addresses that the CPU needs to know about -
; things like stack address, "main()" function address,
; vertical/horizontal interrupt addresses, etc.
;==============================================================
; For any interrupts we don't want to handle in this demo,
; we specify INT_Null (an interrupt at the bottom of the
; file that doesn't do anything).
;==============================================================
; This must be the very first thing in the ROM, since the CPU
; reads it from $0000 on bootup.
;==============================================================
	dc.l   $00FFE000			; Initial stack pointer value
	dc.l   CPU_EntryPoint		; Start of program
	dc.l   CPU_Exception 		; Bus error
	dc.l   CPU_Exception 		; Address error
	dc.l   CPU_Exception 		; Illegal instruction
	dc.l   CPU_Exception 		; Division by zero
	dc.l   CPU_Exception 		; CHK CPU_Exception
	dc.l   CPU_Exception 		; TRAPV CPU_Exception
	dc.l   CPU_Exception 		; Privilege violation
	dc.l   INT_Null				; TRACE exception
	dc.l   INT_Null				; Line-A emulator
	dc.l   INT_Null				; Line-F emulator
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Spurious exception
	dc.l   INT_Null				; IRQ level 1
	dc.l   INT_Null				; IRQ level 2
	dc.l   INT_Null				; IRQ level 3
	dc.l   INT_HInterrupt		; IRQ level 4 (horizontal retrace interrupt)
	dc.l   INT_Null  			; IRQ level 5
	dc.l   INT_VInterrupt		; IRQ level 6 (vertical retrace interrupt)
	dc.l   INT_Null				; IRQ level 7
	dc.l   INT_Null				; TRAP #00 exception
	dc.l   INT_Null				; TRAP #01 exception
	dc.l   INT_Null				; TRAP #02 exception
	dc.l   INT_Null				; TRAP #03 exception
	dc.l   INT_Null				; TRAP #04 exception
	dc.l   INT_Null				; TRAP #05 exception
	dc.l   INT_Null				; TRAP #06 exception
	dc.l   INT_Null				; TRAP #07 exception
	dc.l   INT_Null				; TRAP #08 exception
	dc.l   INT_Null				; TRAP #09 exception
	dc.l   INT_Null				; TRAP #10 exception
	dc.l   INT_Null				; TRAP #11 exception
	dc.l   INT_Null				; TRAP #12 exception
	dc.l   INT_Null				; TRAP #13 exception
	dc.l   INT_Null				; TRAP #14 exception
	dc.l   INT_Null				; TRAP #15 exception
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	dc.l   INT_Null				; Unused (reserved)
	
;==============================================================
; SEGA MEGA DRIVE ROM HEADER
;==============================================================
; A structure that specifies some metadata about the ROM, like
; its name, author, version number, release date, region,
; and any special peripherals used.
; Note that the Mega Drive console itself doesn't read any of
; this, it's more a convenience for the programmer, but
; most emulators will read the title and region.
;==============================================================
; If your emulator doesn't show the correct ROM name, then this
; table is in the wrong place or in the wrong format.
;==============================================================
	dc.b "SEGA MEGA DRIVE "                                 ; Console name
	dc.b "eythore         "                                 ; Copyright holder and release date
	dc.b "WozMon          "	; $120 48 bytes ; Game title (domestic)
	dc.b "                "
	dc.b "                " 
	dc.b "WozMon          "	; $150 48 bytes ; Game title (overseas)
	dc.b "                "
	dc.b "                " 
	dc.b "GM XXXXXXXX-XX"                                   ; Version number
	dc.w $0000                                             ; Checksum
	dc.b "J               "                                 ; I/O support
	dc.l ROM_Start                                          ; Start address of ROM
	dc.l ROM_End-1                                          ; End address of ROM
	dc.l $00FF0000                                         ; Start address of RAM
	dc.l $00FF0000+$0000FFFF                              ; End address of RAM
	dc.l $00000000                                         ; SRAM enabled
	dc.l $00000000                                         ; Unused
	dc.l $00000000                                         ; Start address of SRAM
	dc.l $00000000                                         ; End address of SRAM
	dc.l $00000000                                         ; Unused
	dc.l $00000000                                         ; Unused
	dc.b "                                        "         ; Notes (unused)
	dc.b "  E             "                                 ; Country codes
	
;==============================================================
; INITIAL VDP REGISTER VALUES
;==============================================================
; 24 register values to be copied to the VDP during initialisation.
; These specify things like initial width/height of the planes,
; addresses within VRAM to find scroll/sprite data, the
; background palette/colour index, whether or not the display
; is on, and clears initial values for things like DMA.
;==============================================================
VDPRegisters:
	dc.b $14 ; $00:  H interrupt on, palettes on
	dc.b $74 ; $01:  V interrupt on, display on, DMA on, Genesis mode on
	dc.b $30 ; $02:  Pattern table for Scroll Plane A at VRAM $C000 (bits 3-5 = bits 13-15)
	dc.b $00 ; $03:  Pattern table for Window Plane at VRAM $0000 (disabled) (bits 1-5 = bits 11-15)
	dc.b $07 ; $04:  Pattern table for Scroll Plane B at VRAM $E000 (bits 0-2 = bits 11-15)
	dc.b $78 ; $05:  Sprite table at VRAM $F000 (bits 0-6 = bits 9-15)
	dc.b $00 ; $06:  Unused
	dc.b $00 ; $07:  Background colour: bits 0-3 = colour, bits 4-5 = palette
	dc.b $00 ; $08:  Unused
	dc.b $00 ; $09:  Unused
	dc.b $08 ; $0A: Frequency of Horiz. interrupt in Rasters (number of lines travelled by the beam)
	dc.b $00 ; $0B: External interrupts off, V scroll fullscreen, H scroll fullscreen
	dc.b $81 ; $0C: Shadows and highlights off, interlace off, H40 mode (320 x 224 screen res)
	dc.b $3F ; $0D: Horiz. scroll table at VRAM $FC00 (bits 0-5)
	dc.b $00 ; $0E: Unused
	dc.b $02 ; $0F: Autoincrement 2 bytes
	dc.b $01 ; $10: Scroll plane size: 64x32 tiles
	dc.b $00 ; $11: Window Plane X pos 0 left (pos in bits 0-4, left/right in bit 7)
	dc.b $00 ; $12: Window Plane Y pos 0 up (pos in bits 0-4, up/down in bit 7)
	dc.b $FF ; $13: DMA length lo byte
	dc.b $FF ; $14: DMA length hi byte
	dc.b $00 ; $15: DMA source address lo byte
	dc.b $00 ; $16: DMA source address mid byte
	dc.b $80 ; $17: DMA source address hi byte, memory-to-VRAM mode (bits 6-7)
	
	even

;==============================================================
; Initialize TMSS (Trademark Security System)
;==============================================================
CPU_EntryPoint:
	;==============================================================
	; Initialise the Mega Drive
	;==============================================================

	; Write the TMSS signature (if a model 1+ Mega Drive)
	jsr    VDP_WriteTMSS

	; Load the initial VDP registers
	jsr    VDP_LoadRegisters

	;==============================================================
	; Clear VRAM (video memory)
	;==============================================================

	; Setup the VDP to write to VRAM address $0000 (start of VRAM)
	SetVRAMWrite $0000

	; Write 0's across all of VRAM
	move.w #($00010000/size_word)-1,d0	; Loop counter = 64kb,in words (-1 for DBRA loop)
	@ClrVramLp:								; Start of loop
	move.w #$0,vdp_data					; Write a $0000 (word size) to VRAM
	dbra   d0,@ClrVramLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	
	;==============================================================
	; Initialise status register and set interrupt level.
	; This begins firing vertical and horizontal interrupts.
	;==============================================================
	move.w #$2300,sr

	;==============================================================
	;  Write color pallete to cram
	;==============================================================
	jsr VDP_writePallete

	;==============================================================
	; Write the font tiles to VRAM
	;==============================================================
	jsr FontINIT

	;==============================================================
	; Initialize keyboard
	;==============================================================
	; https://www.plutiedev.com/saturn-keyboard
	; blastem 6.2 has saturn keyboard emulation,
	; it should be connected to controller port 2
	; press right ctrl for blastem to capture keystrokes

	; The z80 doesn't need to be running, yet.
	FastPauseZ80
    move.b  #$60,(IoCtrl2)
    move.b  #$60,(IoData2)
    ResumeZ80

	;==============================================================
	; Extras to play with
	; this section can be removed in full
	;==============================================================

	; initialize extra_demo timer and index
	move.l #1,(demoTimer) ; start of counter, keypress when x lsb are 0
	move.l #0,(demoIndex)

	; begin with free ram in printStrAddr
	move.l #freeRAM,(printStrAddr)
	move.w #$0000,(printStrLen)

	jmp WozMon
	include extra.asm
	; nothing can be changed in the ROM in or above extra
	; for the addresses to stay consistent for extra_demo
	include extra_demo.asm ; Comment this out to remove the starting demo

	;==============================================================
	; WozMon
	;==============================================================
WozMon:
	ifd extraInfo
		jsr extraInfo
	endif
; d0.b: current_char
; d1.b: text_index
; d2.l: hex_parsing
; d3.b: YSAV text_index save, used to see if hex value is given
; d4.b: MODE, $00=XAM, $7F=STOR, $AE=BLOCK XAM
; d5.b: tmp/loop incrementor
; d6.b: tmp/loop incrementor

; a0: inputBuffer
; a1: XAM, last opened location
; a2: ST, store address

RST:
				clr d1					; Clear text_index
				lea     InputBuffer,a0	; Permanently load address of InputBuffer into a0
                move.b  #CH_ESC,d0      ; Begin with escape in current_char

NOTCR:
                cmp.b   #CH_BACKSPACE,d0
                beq     BACKSPACE 		; Current char is backspace
                cmp.b   #CH_ESC,d0
                beq     ESCAPE          ; Current char is escape
                addq.b  #1,d1           ; Advance text index.
                bpl.b   NEXTCHAR        ; Auto ESC if line longer than 127.

ESCAPE: 
                move.b  #CH_BACKSLASH,d0
                jsr     printChar       ; Output "\".

GETLINE:		
                move.b	#CH_CR,d0           
                JSR     printChar		; Print CR
                move.b  #$01,d1         ; Initialize text index.
				move.b  #$01,(Cursor_X)

BACKSPACE:      
				sub.b   #1,(Cursor_X)
				sub.b   #$01,d1
				bge     cursor_positive
				move.b  #0,(Cursor_X)   ; set cursor to start of line
				move.b  #0,d1
cursor_positive:
				move    #CH_SPACE,d0
				jsr     printChar       ; clear character from screen
				sub.b   #1,(Cursor_X)

NEXTCHAR:
				moveM.l d1/d4/d5/a0/a1/a4,-(sp) ; Read keyboard trashes some registers
				clr     d0
				jsr     readKey
				moveM.l (sp)+,d1/d4/d5/a0/a1/a4
				cmp.b   #0,d0	      ; if 0 then key was not ready
				beq     NEXTCHAR      ; Loop until ready.

				; Check if character is lowercase (between 'a' and 'z')
				CMP.B   #97,D0        ; Compare with ASCII of 'a'
				BLT     NotLowercase  ; If less than 'a', it's not lowercase
				CMP.B   #122,D0       ; Compare with ASCII of 'z'
				BGT     NotLowercase  ; If greater than 'z', it's not lowercase
				
				SUB.B   #32,D0        ; Convert to uppercase (subtract 32)
NotLowercase:			
				move.b  d0,(a0,d1)    ; Add to text buffer.
                JSR     printChar          ; Display character.
                CMP.b   #CH_CR,d0     ; CR?
                BNE     NOTCR         ; No.

                move    #$FF,d1       ; Reset text index.
                clr.b   d0            ; For XAM mode.
                clr.l   d2            ; hex_parsing = 0.
SETBLOCK:
                asl.b   #1,d0
SETSTOR:
                asl.b   #1,d0         ; Leaves $7B if setting STOR mode.
                move.b  d0,d4         ; $00 = XAM, $74 = STOR, $B8 = BLOK XAM.
BLSKIP:
                add.b   #1,d1         ; Advance text index.
NEXTITEM:
                move.b  (a0,d1),d0    ; Get character.
                CMP.b   #CH_CR,d0     ; CR?
                BEQ     GETLINE       ; Yes, done this line.
                CMP.b   #$2E,d0       ; "."?
				BEQ     SETBLOCK      ; Set BLOCK XAM mode.
                BLT     BLSKIP        ; Skip delimiter.
                CMP.b   #$3A,d0 	  ; ":"?
                BEQ     SETSTOR       ; Yes, set STOR mode.
                CMP.b   #$52,d0       ; "R"?
                BEQ     RUN           ; Yes, run user program.
				clr.l   d2			  ; hex_parsing = 0
                move.b  d1,d3         ; Save Y for comparison                    

NEXTHEX:
                move.b  (a0,d1),d0    ; Get character for hex test.
                EOR.b   #$30,d0       ; Map digits to $0-9.
                CMP.b   #$0A,d0       ; Digit?
                Blt     DIG           ; Yes.
                add.b   #$89,d0       ; Map letter "A"-"F" to $FA-FF.
                CMP.b   #$FA,d0       ; Hex letter?
                Blt     NOTHEX        ; No, character not hex.
                CMP.b   #$FF,d0       ; Not in wozmon, it used some trick for this check
                Bgt     NOTHEX        ; No, character not hex.
DIG:
				and.b   #$0F,D0       ; Mask nibble
HEXSHIFT:
				lsl.l   #4,d2         ; Shift nibble from d0 into d2
				or.b    d0,d2         ; Fill nibble with d0
                addq    #1,d1         ; Advance text index.
                bra     NEXTHEX       ; Always taken. Check next character for hex.
NOTHEX:
                cmp.b   d3,d1         ; Check if YSAV empty (no hex digits).
                BEQ     ESCAPE        ; Yes, generate ESC sequence.
                btst    #6,d4         ; Test bit 6 of MODE byte.
                beq     NOTSTOR       ; B6=0 is STOR, 1 is XAM and BLOCK XAM.

				; store in STL
				move.b  d2,(a2)       ; move a single byte into STORE
				add.l   #1,a2         ; increment STORE
TONEXTITEM:    
				bra     NEXTITEM
RUN:
				; in original WozMon RUN was simply "JMP (address)"
				; Now, it is a call to subroutine
				move.b  #CH_CR,d0
				jsr     printChar
				jsr     (a1)
				jmp     RST
NOTSTOR:
				btst 	#7,d4         ; branch if bit is set
                bne     XAMNEXT       ; B7 = 0 for XAM, 1 for BLOCK XAM.
SETADR:
				move.l  d2,a1
				move.l  d2,a2
				move.b  #0,d0         ; # there is an address to print (set the zero flag)
NXTPRNT:
                BNE     PRDATA        ; NE means no address to print.
                move.b  #CH_CR,d0     ; CR.
                JSR     printChar          ; Output it.

				move.l  a1,d0 	      ; move address to print to d0
				move.l  #4,d6         ; 4 iterations
				bra     _prAddress
prAddress:
				rol.l   #8,d0
                JSR     prbyte        ; Output it in hex format.
_prAddress:		dbra    d6,prAddress

                move.b  #$3A,d0       ; ":".
                JSR     printChar          ; Output it.
PRDATA:
                move.b  #$20,d0       ; Blank.
                JSR     printChar          ; Output it.
                move.b  (a1),d0       ; Get data byte at 'examine index'.
                jsr     PRBYTE 
				
XAMNEXT:
		        clr.b   d4            ; 0 -> MODE (XAM mode).
				cmp.l 	a1,d2         ; check if last thing we parsed is greater than a1
				ble     TONEXTITEM    ; d2<=a1, so no more data to output.
                add.l   #1,a1         ; More data to print, increment 'examine index'
                move.l  a1,d5		  ; Check low-order 'examine index' byte
                AND.l   #$07,d5       ; For MOD 8 = 0, if address is mod8 print the address in a1
                bra     NXTPRNT       ; Always taken.

PRBYTE:
                move.b  d0,d5         ; Save A for LSD.
                LSR.b   #4,D0	      ; MSD to LSD position.
                JSR     PRHEX         ; Output hex digit.
                move.b  d5,d0         ; Restore A.
PRHEX:
                AND.b   #$0F,d0       ; Mask LSD for hex print.
                OR.b    #$30,d0       ; Add "0".
                CMP.b   #$39,d0       ; Digit?
                Ble     printChar          ; Yes, output it.
                ADD.b   #$07,d0       ; Add offset for letter.
; ECHO:
	;==============================================================
	; End of WozMon port
	;==============================================================
	;==============================================================
	; Print characters and handle screen scroll
	;	note: jsr PRBYTE trashes d0,d5
	;==============================================================
PrintChar:
	moveM.l d0/d1/d2/d4/d5,-(sp)
		and #$FF,d0
		cmp.b #$0A,d0 ; carrage return
		beq _newline
		; 32 is space, 33 first printable(!), 127 is the last(<3)
		cmp.b #127,d0 ; is it higher than the highest printable character
		bgt skip_printchar
		sub #32,d0
		bmi skip_printchar ; less than space
PrintCharAlt:				
		Move.L  #$40000003,d5
		Move.L #0,d4
		Move.B (Cursor_Y),D4
		rol.L #8,D4
		rol.L #8,D4
		rol.L #7,D4
		add.L D4,D5
		
		Move.B (Cursor_X),D4
		rol.L #8,D4
		rol.L #8,D4
		rol.L #1,D4
		add.L D4,D5
		
		MOVE.L	D5,(VDP_control)
		MOVE.W	D0,(VDP_data)

		addq.b #1,(Cursor_X)
		move.b (Cursor_X),d0
		cmp.b #39,d0
		ble skip_printchar
_newline:		; newline (28 lines on monitor at a time)
		move.b #0,(Cursor_X)
		addq.b #1,(Cursor_Y)
		and.b #%00011111,(Cursor_Y) ; mod 32, there are 32 rows in VRAM

		jsr clearLine

		cmp.b #1,(doScroll) ; Are we scrolling the page after each newline
		beq scrollPage

		cmp.b #28,(Cursor_Y) ; Should we start scrolling
		Blt skip_printchar ; No
		move.b #1,(doScroll)
		
scrollPage:
		move.w (vscroll_amount),d5
		add.w #8,d5 ; scroll single line, 8 pixels
		move.w d5,(vscroll_amount)

		; Write Plane A's V-scroll value to vertical scroll memory (VSRAM).
		; Plane A's vertical page scroll value is at VSRAM $0000,Plane B's is at $0002.
		SetVSRAMWrite $0000
		move.w d5,(vdp_data)
skip_printchar:
	moveM.l (sp)+,d0/d1/d2/d4/d5
	rts

NewLine:
	; I think it is obvious now but, comment, save trashed registers, avoid debugging
	moveM.l d0/d1/d2/d4/d5,-(sp)
	bra _newline

clearLine:
	; trashes d1,d2
	; SetVRAMWrite: move.l #(vdp_cmd_vram_write)|((\1)&$3FFF)<<16|(\1)>>14,vdp_control
	; SetVRAMWrite vram_addr_plane_a+(((y*vdp_plane_width)+x)*size_word)
	; vram_addr_plane_a	equ $C000
	; vdp_plane_width   equ $0040
	; size_word         equ $0002
	clr d1
	move.b (Cursor_Y),d1
	; mulu.w #vdp_plane_width*size_word,d1 ; equivalent to shifting left 7 places
	lsl.w #7,d1
	; add.l #vram_addr_plane_a,d1 ; equivalent to
	or.w #vram_addr_plane_a,d1

	move.l d1,d2
	;lsr.l #14,d2 ; keep a15 and a14 in lowest two bits
	lsr.l #7,d2
	lsr.l #7,d2 ;
	;lsl.l #16,d1 ; move a13-a0 to b29-b16
	lsl.l #7,d1
	lsl.l #7,d1
	lsl.l #2,d1 ;
	and.l #$3FFF0000,d1 ; keep only b29-b16
	or.l d2,d1 ; combine
	or.l #VDP_cmd_vram_write,d1 ; set bit 30, CD0=1 (write)
	move.l d1,(VDP_control)		; set write next character to VRAM with address
	
	move.b #20,d1 ; iterator, we are writing two words (.l) at a time, there are 40 words (characters) per row
	bra clrChar
_clrChar:
	move.l #0,(VDP_data) ; 0 is the empty tile
clrChar: dbmi d1,_clrChar
	rts

;==============================================================
; Read keyboard
;==============================================================
; Read a keyboard key into d0.b, $00 if no character
; trashes d1,d4,d5 a0,a1,a4;
	ifd demo
readKeyReal:
	else
readKey:
	endif
	jsr ReadKeyboard
	; check if succeded
	cmp #0,d0
	beq keyboardSuccess

	; failure
	move.l #0,d0
	rts

keyboardSuccess:
	move.b (ScancodeBuffer+7),d4 ; nibble 7 of the scancode contains bits: make 1 1 break ($E make, $7 break)
	; assumes it is either make or break scancode
	; could also just do not break, then no shift is needed
	and.b #%1000,d4 ; extract make bit
	lsr #3,d4 ; shift it right to the 1 position
	; now d4 should be 1 if make, else 0

	; load scancode into d5.b
	clr.l d5 ; d5 is used to index (must be zero)
	move.b (ScancodeBuffer+8),d5 ; upper nibble of scancode
	rol.b #4,d5
	or.b (ScancodeBuffer+9),d5; lower nibble of scancode

	; shift
	cmp.b #$12,d5 ;left_shift
	beq set_shift
	cmp.b #$59,d5 ;right_shift
	beq set_shift

	bra not_shift
set_shift:
	move.b d4,(shiftDown) ; store it in address shiftDown
	rts
not_shift:
	cmp.b #0,d4 ; test if make bit is set
	bne _keyMake ; branch if make bit is set
	rts ; else return
_keyMake:
	;shifted (+$FF)
	lea (scancode_ascii_table),a4
	cmp.b #0,(shiftDown)
	beq shiftUp
	add #$0100,a4
shiftUp:
	; the character we want is offset by the value of d5(ascii)
	move.b (a4,d5),d0 ; move the ascii to d0
	rts


; Routine for reading a keyboard packet
; Assumes it's in the second player port
;
; out d0.w = 0 on success
;          = -1 on failure
; trashes d1, a0, a1

ReadKeyboard:
    lea     (IoData2),a0
    lea     (ScancodeBuffer),a1
    
    ; Pause Z80 while we access the
    ; I/O ports to avoid glitches
    FastPauseZ80
    
    ; Initial step, also check
    ; that it's indeed a keyboard
    move.b  #$20,(a0)
    moveq   #$0F,d0
    and.b   (a0),d0
    cmp.b   #$01,d0
    bne     @Error
    
    ; Now try reading every nibble
    moveq   #(12/2)-1,d0
@Loop:
    ; Read a nibble
    move.b  #$00,(a0)
    moveq   #$7F,d1
@Wait1:
    btst    #4,(a0)
    beq.s   @DataOk1
    dbf     d1,@Wait1
    bra     @Error
@DataOk1:
    moveq   #$0F,d1
    and.b   (a0),d1
    move.b  d1,(a1)+
    
    ; Read another nibble
    move.b  #$20,(a0)
    moveq   #$7F,d1
@Wait2:
    btst    #4,(a0)
    bne.s   @DataOk2
    dbf     d1,@Wait2
    bra     @Error
@DataOk2:
    moveq   #$0F,d1
    and.b   (a0),d1
    move.b  d1,(a1)+
    
    ; Onto next pair
    dbf     d0,@Loop
    
    ; Let keyboard and Z80 idle
    move.b  #$60,(a0)
    ResumeZ80
    ; Return success!
    moveq   #0,d0
    rts
    
@Error:
    ; Let keyboard and Z80 idle
    move.b  #$60,(a0)
    ResumeZ80
    ; Return failure...
    moveq   #-1,d0
    rts

;==============================================================
; Initilization routines
;==============================================================

VDP_WriteTMSS:

	; The TMSS (Trademark Security System) locks up the VDP if we don't
	; write the string 'SEGA' to a special address. This was to discourage
	; unlicensed developers, since doing this displays the "LICENSED BY SEGA
	; ENTERPRISES LTD" message to screen (on Mega Drive models 1 and higher).
	;
	; First, we need to check if we're running on a model 1+, then write
	; 'SEGA' to hardware address $A14000.

	move.b hardware_ver_address,d0			; Move Megadrive hardware version to d0
	andi.b #$0F,d0						; The version is stored in last four bits,so mask it with 0F
	beq    @SkipTMSS						; If version is equal to 0, skip TMSS signature
	move.l #tmss_signature,tmss_address	; Move the string "SEGA" to $A14000
	@SkipTMSS:

	; Check VDP
	move.w vdp_control,d0					; Read VDP status register (hangs if no access)
	
	rts


FontINIT:
	move.l 	#(96*8)-1,d2 ;
	lea Font,a1
	SetVRAMWrite vram_addr_tiles ; Start writes to address zero
NextFont:
	move.b	(a1)+,d0 ; load the first byte of the font
	clr.l d1         ; we will have to convert bits to bytes (%xxxxxxx to $xxxxxxxx)

	; rol the first bit into d1
	roxl.b	#1,d0
	roxl.L	#1,d1

	; have to move the lsbit 3 places left 1000, then rol a new bit in 0001 0001
	move.l  #7-1,d3  ; do that 7 time
	bra rolin
rolin:
	rol.L   #3,d1  ; have to move the bit 3 places left 1000 then rol a new bit in 0001 0001
	roxl.B	#1,d0
	roxl.L	#1,d1
	dbra d3,rolin

	move.l	d1,(VDP_Data)	; write long to VDP
	dbra	d2,NextFont		; loop until done
	rts

;==============================================================
; Write the palette to CRAM (colour memory)
;==============================================================
;send a command to the vdp with the address we want to write toþ
;then loop through the pallet and write it to the cram
;may be something wrong with the macro, however theses are equivalent
;move.l #VDP_cmd_cram_write,vdp_control
VDP_writePallete:
	SetCRAMWrite $0000
	lea    Palette,a0				; Move palette address to a0
	move.w #size_palette_w-1,d0	; Loop counter = 8 words in palette (-1 for DBRA loop)
	@PalLp:							; Start of loop
	move.w (a0)+,vdp_data			; Write palette entry, post-increment address
	dbra d0,@PalLp					; Decrement d0 and loop until finished (when d0 reaches -1)
	rts

VDP_LoadRegisters:

	; To initialise the VDP, we write all of its initial register values from
	; the table at the top of the file, using a loop.
	;
	; To write a register, we write a word to the control port.
	; The top bit must be set to 1 (so $8000), bits 8-12 specify the register
	; number to write to, and the bottom byte is the value to set.
	;
	; In binary:
	;   10$ XXXX YYYY YYYY
	;   X = register number
	;   Y = value to write

	; Set VDP registers
	lea    VDPRegisters,a0		; Load address of register table into a0
	move.w #$18-1,d0			; 24 registers to write (-1 for loop counter)
	move.w #$8000,d1			; 'Set register 0' command to d1

	@CopyRegLp:
	move.b (a0)+,d1			; Move register value from table to lower byte of d1 (and post-increment the table address for next time)
	move.w d1,vdp_control		; Write command and value to VDP control port
	addi.w #$0100,d1			; Increment register #
	dbra   d0,@CopyRegLp		; Decrement d0,and jump back to top of loop if d0 is still >= 0
	
	rts

; ;==============================================================
; ; INITIAL VDP REGISTER VALUES
; ;==============================================================
; ; 24 register values to be copied to the VDP during initialisation.
; ; These specify things like initial width/height of the planes,
; ; addresses within VRAM to find scroll/sprite data, the
; ; background palette/colour index, whether or not the display
; ; is on, and clears initial values for things like DMA.
; ;==============================================================
; 	;==============================================================
; 	;Set VDP mode to enable display
; 	;==============================================================
; 	;7 6 5 | 4 3 2 1 0 | lower_byte 
; 	;1 0 0 | [RS4-RS0] | [d7-d0 data]
; 	; $01 - Mode Register 2 data = %01000100(0x44) unblank display
; 	; VRAM	DE	IE0	M1	M2	M5	0	0
; 	; 0 - VRAM allows use of an additional 64 KB of external VRAM. [2]
; 	; 1 - DE decides if the display is enabled (1) or disabled (0).
; 	; 0 - IE0 enables the vertical blank interrupt, or VBI. This is a Level 6 interrupt to the M68k.
; 	; 0 - M1 allows DMA to be performed when set, while it disables any sort of DMA when it is not set. This bit also masks CD5 in the VDP control word if cleared.
; 	; 0 - M2 sets the vertical display mode/resolution. When set to 1, 30 cell (240 pixel) mode is enabled, which is exclusive to PAL. When set to 0, 28 cell (224 pixel) mode is enabled, which is always the case on NTSC. Enabling 30 cell mode on an NTSC Mega Drive will cause the image to roll upwards and be unstable, as well as causing vertical interrupts to happen at 30 Hz, and no vertical sync pulse to be output on the video connector.
; 	; 1 - M5 selects between Master System (Mode 4) and Mega Drive (Mode 5) video modes. Should always be set to 1 when programming Mega Drive software.
; 	; MOVE.W	#$8144,(VDP_Control)
; VDPSettings:
; 		dc.b $14 ; 0x00:  H interrupt on, palettes on
; 		dc.b $74 ; 0x01:  V interrupt on, display on, DMA on, Genesis mode on
; 		; DC.B $04 ; 0 mode register 1											---H-1M-
; 		; DC.B $04 ; 1 mode register 2											-DVdP---
; 		DC.B $30 ; 2 name table base for scroll A (A=top 3 bits)				--AAA--- = $C000
; 		DC.B $3C ; 3 name table base for window (A=top 4 bits / 5 in H40 Mode)	--AAAAA- = $F000
; 		DC.B $07 ; 4 name table base for scroll B (A=top 3 bits)				-----AAA = $E000
; 		DC.B $6C ; 5 sprite attribute table base (A=top 7 bits / 6 in H40)		-AAAAAAA = $D800
; 		DC.B $00 ; 6 unused register											--------
; 		DC.B $00 ; 7 background color (P=Palette C=Color)						--PPCCCC
; 		DC.B $00 ; 8 unused register											--------
; 		DC.B $00 ; 9 unused register											--------
; 		DC.B $FF ;10 H interrupt register (L=Number of lines)					LLLLLLLL
; 		DC.B $00 ;11 mode register 3											----IVHL
; 		DC.B $81 ;12 mode register 4 (C bits both1 = H40 Cell)					C---SIIC
; 		DC.B $37 ;13 H scroll table base (A=Top 6 bits)							--AAAAAA = $FC00
; 		DC.B $00 ;14 unused register											--------
; 		DC.B $02 ;15 auto increment (After each Read/Write)						NNNNNNNN
; 		DC.B $01 ;16 scroll size (Horiz & Vert size of ScrollA & B)				--VV--HH = 64x32 tiles
; 		DC.B $00 ;17 window H position (D=Direction C=Cells)					D--CCCCC
; 		DC.B $00 ;18 window V position (D=Direction C=Cells)					D--CCCCC
; 		DC.B $FF ;19 DMA length count low										LLLLLLLL
; 		DC.B $FF ;20 DMA length count high										HHHHHHHH
; 		DC.B $00 ;21 DMA source address low										LLLLLLLL
; 		DC.B $00 ;22 DMA source address mid										MMMMMMMM
; 		DC.B $80 ;23 DMA source address high (C=CMD)							CCHHHHHH
; 	even
;==============================================================
; INTERRUPT ROUTINES
;==============================================================
; The interrupt routines, as specified in the vector table at
; the top of the file.
; Note that we use RTE to return from an interrupt, not
; RTS like a subroutine.
;==============================================================

; Vertical interrupt - run once per frame
INT_VInterrupt:
	; Doesn't do anything in this demo
	rte

; Horizontal interrupt - run once per N scanlines (N = specified in VDP register $A)
INT_HInterrupt:
	; Doesn't do anything in this demo
	rte

; NULL interrupt - for interrupts we don't care about
INT_Null:
	rte

; Exception interrupt - called if an error has occured
CPU_Exception:
	; Just halt the CPU if an error occurred. Later on, you may want to write
	; an exception handler to draw the current state of the machine to screen
	; (registers, stack, error type, etc) to help debug the problem.
	stop   #$2700
	rte

;==============================================================
; PALETTE
;==============================================================
; A single colour palette (16 colours) we'll be using to draw text.
; Colour #0 is always transparent, no matter what colour value
; you specify.
; We only use white (colour 2) and transparent (colour 0) in this
; demo, the rest are just examples.
;==============================================================
; Each colour is in binary format 0000 BBB0 GGG0 RRR0
; The bottom bit is discarded, can only use even values
; (0,2,4,6,8,A,C,E). So 0x0000 is black, 0x0EEE is white
; 0x000E is red, 0x00E0 is green, and 0x0E00 is blue.
;==============================================================
Palette:
	dc.w $0000	; Colour 0 = Transparent
	dc.w $0EEE	; Colour 1 = White
	dc.w $0000	; Colour 2 = Black
	dc.w $000E	; Colour 3 = Red
	dc.w $00E0	; Colour 4 = Blue
	dc.w $0E00	; Colour 5 = Green
	dc.w $0E0E	; Colour 6 = Pink
	dc.w $0000	; Leave the rest black...
	dc.w $0000
	dc.w $0000
	dc.w $0000
	dc.w $0000
	dc.w $0000
	dc.w $0000
	dc.w $0000
	dc.w $0000

Font:
	incbin "data/PetMe.FNT"
scancode_ascii_table:
	incbin "data/saturn_ascii_table.blob"

ROM_End:

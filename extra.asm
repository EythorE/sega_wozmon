	;==============================================================
	; Subroutines to call from wozmon
	;==============================================================

	; Print a line with string followed by an address
	macro writeStrAddressLine ;str,address
		move.l \1,a0
		jsr printStringZTa0
		move.l \2,d0
		jsr printD0
		move.b #CH_CR,d0
		jsr printChar
	endm
	

strExtraInfo: 		dc.b 	"Extra:        ",0
strPrintStrZt:		dc.b	"printStrZT:   ",0
strWriteln:			dc.b	"writeStrZT:   ",0
strPrintString:		dc.b	"printString:  ",0
strPrintStrAddr:	dc.b	"        Addr.l: ",0
strPrintStrLen:		dc.b	"        Len.w:  ",0
strVdpReadVram:	  	dc.b	"VdpReadVRAM:  ",0
strVdpWriteVram:	dc.b	"VdpWriteVRAM: ",0
strVdpWriteCram:	dc.b	"VdpWriteCRAM: ",0
strVdpReadCram: 	dc.b	"VdpReadCRAM:  ",0
	even

extraInfo:
	writeStrAddressLine #strExtraInfo,#extraInfo
	writeStrAddressLine #strPrintStrZT,#printStringZT
	writeStrAddressLine #strWriteln,#writeln
	writeStrAddressLine #strPrintString,#printString

	;writeStrAddressLine #strPrintStrAddr,#printStrAddr
	move.l #strPrintStrAddr,a0
	jsr printStringZTa0
	move.l #printStrAddr,d0
	jsr printD0
	move.b #$20,d0
	jsr printChar
	move.b #'(',d0
	jsr printChar
	move.l printStrAddr,d0
	jsr printD0
	move.b #')',d0
	jsr printChar
	move.b #CH_CR,d0
	jsr printChar

	;writeStrAddressLine #strPrintStrLen,#printStrLen
	move.l #strPrintStrLen,a0
	jsr printStringZTa0
	move.l #printStrLen,d0
	jsr printD0
	move.b #$20,d0
	jsr printChar
	move.b #'(',d0
	jsr printChar
	move.w (printStrLen),d1
	rol.w #8,d1
	move.b d1,d0
	jsr prbyte
	rol.w #8,d1
	move.b d1,d0
	jsr prbyte
	move.b #')',d0
	jsr printChar
	move.b #CH_CR,d0
	jsr printChar
	jsr printChar

	writeStrAddressLine #strVdpWriteVram,#vdpWriteVram
	writeStrAddressLine #strVdpWriteCram,#vdpWriteCram
	writeStrAddressLine #strVdpReadVram,#vdpReadVram
	writeStrAddressLine #strVdpReadCram,#vdpReadCram
	rts


; print zero terminated string at (printStrAddr)
; trashes d0, a0
printStringZT:
	move.l (printStrAddr),a0 ; load address stored in ram PrintStrAddr
printStringZTa0:
	move.b (a0)+,d0
	cmp.b #0,d0
	beq _zt
	jsr printChar
	bra printStringZTa0
_zt:
	rts

; print string at (printStrAddr) of length (printStrLen)
; trashes d0,d1, a0
printString:
	move.w (printStrLen),d1
	move.l (printStrAddr),a0 ; load address stored in ram PrintStrAddr
	bra _printStringChar
printStringChar:
	move.b (a0)+,d0
	jsr printChar
_printStringChar: dbra.w d1,printStringChar
	rts

; print byte in d0
PrintD0Byte:
	moveM.l d0/d5,-(sp)
	jsr PRBYTE
	moveM.l (sp)+,d0/d5
	rts
; print word in d0
printD0Word:
    movem.l d0/d1/d5/a0,-(sp)
	swap d0
	move.l #2,d1 ; 2 bytes
	bra _printD0bytes ; long is now
; print long in d0
printD0:
    movem.l d0/d1/d5/a0,-(sp)
	move.l #4,d1 ; 4 iterations
	bra _printD0bytes
printD0bytes:
	rol.l #8,d0
    jsr prbyte
_printD0bytes: dbra.w d1,printD0bytes
	movem.l (sp)+,d0/d1/d5/a0
	rts

; Write a line of ascii characters to (printStrAddr)
; 	ESC for end of text
; trashes d0,d1,d4,d5 a0,a1,a4;
writeln:
	move.l (printStrAddr),a0 ; load address into a0
_read_key_loop:
	move.l a0,-(sp) ; Read keyboard trashes some registers
	jsr readKey
	move.l (sp)+,a0
	cmp.b #0,d0	 ; if 0 then key was not ready
	beq _read_key_loop ; Loop until ready.
	cmp.b #CH_BACKSPACE,d0
	bne not_backspace
    ; backspace
	cmp.b #0,(Cursor_X)
	beq _read_key_loop ; already at start of line (avoidng messing with the scroll and Cursor_Y)
	subq.b #1,(Cursor_X) ; move cursor back
	subq.l #1,a0 ; move address back
	move.b #$20,d0 ; empty tile
	jsr printChar ; clear tile
	subq.b #1,(Cursor_X) ; go back to cleared tile
	bra _read_key_loop ; cleared the character
not_backspace:
	cmp.b #CH_ESC,d0 ; Esc?
	beq done_write   ; yes
	move.b d0,(a0)+ ; Add to text buffer.
	jsr printChar         ; Display character.
	bra _read_key_loop
done_write:
	move.b #0,(a0) ; zero terminate
	; also write the strings length
	move.l a0,d0
	sub.l (printStrAddr),d0
	move.w d0,(printStrLen)
	rts

; VDP stuff
vdpWriteVram:
	move.l #vdp_cmd_vram_write,d2
	; SetVRAMWrite vram_addr_plane_a+(((y*vdp_plane_width)+x)*size_word)
	bra vdpWrite
vdpWriteCram:
	move.l #vdp_cmd_cram_write,d2
vdpWrite:
	; Modify a vdp address into the format expected by vdp_control
	; Set d0=(strAddr)&$3FFF)<<16|(strAddr)>>14
	;          byte 3 | byte 2 | byte 1 | byte 0
	; CD1 CD0 A13 A12 | A11-A0 |  $00   | CD5-CD2 0 0 A15 A14
	move.l (printStrAddr),d0
	move.w d0,d1
	rol.w #2,d1  ; keep a15 and a14 in lowest two bits
	swap d0      ; swap a15-a0, a32-a16
	move.w d1,d0 ; copy a15,a14 to lower two bits of d0, will mask other bits in w
	and.l #$3FFF0003,d0 ; keep only b29-b16, b1-b0
	
	or.l d0,d2 ; set CD bits
	move.l d2,(vdp_control)
	move.w (printStrLen),(vdp_data)
	rts

vdpReadVram:
	; read (strLen) longwords from vram (strAddr) into freeRAM and print it out.
	; There are 4 bytes per row and 8 rows per tile; begin with free ram in printStrAddr
	; the last tile is
	;	move.l #95*4*8,(printStrAddr)
	;	move.w #8,(printStrLen)
	;   jsr vdpReadVram

	; modify the vdp address into the format expected by vdp_control
	; Set d0=(strAddr)&$3FFF)<<16|(strAddr)>>14
	;          byte 3 | byte 2 | byte 1 | byte 0
	; CD1 CD0 A13 A12 | A11-A0 |  $00   | CD5-CD2 0 0 A15 A14
	move.l (printStrAddr),d0 ; address in vram
	move.w d0,d1
	rol.w #2,d1  ; keep a15 and a14 in lowest two bits
	swap d0      ; swap a15-a0, a32-a16
	move.w d1,d0 ; copy a15,a14 to lower two bits of d0, will mask other bits in w
	and.l #$3FFF0003,d0 ; keep only b29-b16, b1-b0
	move.l d0,(vdp_control) ; all CD bits are zero for vram read
	
	lea freeRAM,a0
	move.w (printStrLen),d2 ; 8 longs per tile
	bra readVramLong
_readVramLong:
	move.l (vdp_data),(a0)+
readVramLong: dbra.w d2,_readVramLong

	; printChar sets vdp_control so have this loop seperatly
	lea freeRAM,a0
	move.w (printStrLen),d2 ; 8 longs per tile
	bra printVramlong
_printVramLong:
	move.l (a0)+,d0
	jsr printD0
	move.b #CH_CR,d0
	jsr printChar
printVramlong: dbra.w d2,_printVramLong
	rts


vdpReadCram:
 	; read (strLen) words from CRAM (strAddr) into freeRAM and print it out.

 	; modify the vdp address into the format expected by vdp_control
	; only a single byte of memory indexes for CRAM
	move.l (printStrAddr),d0
	and.l #$FFFFFFFE,d0 ; mod 2, cram colors are words in even bytes
	move.l d0,(printStrAddr) ; replace Addr with mod 2 version
	move.l d0,d1 ; address in vram
	swap d0      ; swap a15-a0, a32-a16
	and.l #$00FF0000,d0 ; keep only b23-b16 (a7-a0)
	
	or.l #vdp_cmd_cram_read,d0 ; set CD bits
	move.l d0,(vdp_control)
	
	lea freeRAM,a0
	move.w (printStrLen),d2 ; word per cram color
	bra readCramWord
_readCramWord:
	move.w (vdp_data),(a0)+
readCramWord: dbra.w d2,_readCramWord

	; printChar sets vdp_control so have this loop seperatly
	lea freeRAM,a0
	move.w (printStrLen),d5
	bra printCramWord


_printCramWord:
	move.l d1,d0 ; d1 is address in cram 
	and.b #$0F,d0
	bne skipCaddr ; if mod 16 print it out
	move.l d1,d0
	jsr printD0byte
	move.l #':',d0
	jsr printChar
	move.b #CH_CR,d0
	jsr printChar
skipCaddr:
	move.w (a0)+,d0
	jsr printD0Word

	jsr showColorInPallet0 ; lot of code just to display color tile
	addq.w #2,d1 ; next address

	move.b #CH_CR,d0
	jsr printChar
printCramWord: dbra.w d5,_printCramWord
	rts


showColorInPallet0:
	; create a tile with color in palette 0, and display that tile at the current cursor position
	; address in cram in d1
	cmp.w #$20,d1 ; check if the address is in pallete 0, 4 palletes in cram, 16 words ($20 bytes) each starting at 0
	blt createCTile
	rts ; if not
createCTile:
	; Write to a free tile address, the font is 96 tiles * (4 * 8) bytes/tiles in vram ($0C00 is the first free)
	; modify the vdp address into the format expected by vdp_control
	move.l d1,d0
	lsl.w #4,d0 ; multiply address by 2*8 = 32 (index*4*8)
	add.w #$0C00,d0 ; start of new tiles at $0C00
	swap d0      ; swap a15-a0, a32-a16
	and.l #$3FFF0000,d0 ; keep only b23-b16 (a13-a0), (a15,a14 = 0)
	or.l #vdp_cmd_vram_write,d0
	move.l d0,(vdp_control)

	; Create tile
	; set nibbles to index for tile
	move.l d1,d0
	lsr.l #1,d0 ; index into cram
	move.l #7,d6 ; 7 nibbles to set
	clr.l d2
	bra setCnibble
_setCnibble
	lsl.l #4,d2
	or.l d0,d2 ; set nibble to first nibble of index (no overflow check)
setCnibble: dbra d6,_setCnibble
	; created #$0ccccccc in d2

	; 7 rows (longs) to set for tile
	move.l #7,d3
	bra setVramCRow
_setVramCRow:
	move.l d2,(vdp_data) ; move.l #$0ccccccc,d2
setVramCRow: dbra d3,_setVramCRow
	move.l #0,(vdp_data) ; last row is 0

	; Write the tile to plane_a at cursor
	clr d2
	clr d3
	clr d4

	; compute address for where we want to display the tile to
	; vram_addr_plane_a+(((y*vdp_plane_width)+x)*size_word)
	move.w #vdp_plane_width,d2
	move.b (Cursor_Y),d3
	move.b (Cursor_X),d4
	mulu.w d3,d2 ; (y*vdp_plane_width)
	add.l d4,d2  ; +x
	lsl.l #1,d2  ; *2(size_word)
	add.l #vram_addr_plane_a,d2 ; vram_addr_plane_a ($C000)
	
	; Mundle the bits into correct format
	move.w d2,d3
	rol.w #2,d3  ; keep a15 and a14 in lowest two bits
	swap d2      ; swap a15-a0, a32-a16
	move.w d3,d2 ; copy a15,a14 to lower two bits of d0, will mask other bits in w
	and.l #$3FFF0003,d2 ; keep only b29-b16, b1-b0
	or.l #vdp_cmd_vram_write,d2 ; set CD bits
	move.l d2,(vdp_control)

	; we want to show tile 96+index
	move.w d1,d0 ; tile index into d0
	lsr.w #1,d0 ; index into cram
	add.w #96,d0
	move.w d0,(vdp_data)
	rts


; adapted from https://www.chibiakumas.com/68000/sources.7z
; Print register values to screen
Registers:
	moveM.l d0-d7/a0-a7,-(sp)
	move.l sp,a0
	move.w #'0',d5
ShowNextReg:
	move.w #'D',d0
	bsr PrintChar
	move.w d5,d0
	bsr PrintChar
	move.w #':',d0
	bsr PrintChar
	move.l (a0),d0
	bsr printD0
	
	move.w #' ',d0
	bsr PrintChar
	bsr PrintChar
	bsr PrintChar
	
	add #8*4,a0
	
	move.w #'A',d0
	bsr PrintChar
	move.w d5,d0
	bsr PrintChar
	move.w #':',d0
	bsr PrintChar
	move.l (a0),d0
	bsr printD0
	
	sub #8*4,a0
	add #4,a0
	
	bsr NewLine
	
	add #1,d5
	cmp #'8',d5
	bne ShowNextReg
	
	add #8*4,a0
	move.w #'P',d0
	bsr PrintChar
	move.w #'C',d0
	bsr PrintChar
	move.w #':',d0
	
	bsr PrintChar
	move.l (a0),d0
	bsr printD0
	bsr NewLine	
	moveM.l (sp)+,d0-d7/a0-a7
	rts

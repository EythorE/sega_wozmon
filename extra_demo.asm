	org $0000FF00
demo_instruct:
		dc.b CH_CR,'If you are using BlasEm:',CH_CR
		dc.b 'press ESC |Settings| -> |System| and set'
		dc.b '|IO port 2 Device| to "Saturn Keyboard".'
		dc.b ' -Right CTRL toggles keyboard capture.',CH_CR,0
demo:
		dc.b '51A R',CH_CR ; writeStr
		dc.b 'Hello',CH_CR
		dc.b '    World!',CH_CR,CH_ESC
		dc.b '4B4 R',CH_CR ; printStr
		dc.b 'FF0092: 00 00 00 00 00 08',CH_CR ; set CRAM addr and data to Addr,Len
		dc.b '606 R',CH_CR ; readCram
		dc.b 'FF0096: 0E 55',CH_CR ; store a blueish color in Len
		dc.b '582 R',CH_CR ; write to Addr in CRAM
		
		dc.b 'FF0092: 40 00 EC 2E 20 5F',CH_CR ; set Addr to write to $plane_A...EC2E: color palette 2, E5 is the last character <3 
		dc.b '57A R',CH_CR ; write to VRAM
		dc.b 'FF0095: AC',CH_CR  ; and to ECAC (+$80 row -$2 bytes)
		dc.b '57A R',CH_CR ; write to VRAM
		dc.b 'FF0092: 00 00 00 22 00 0E',CH_CR ; change colour 1 (byte $2) in palette 2(byte $20) to $000E
		dc.b '582 R',CH_CR ; write CRAM
		dc.b 'FF0094: FF 00',CH_CR
		dc.b '334 R',CH_CR ; print subroutine addresses
		dc.b '4B4 R',CH_CR ; printStr
		dc.b CH_ESC,CH_ESC,CH_ESC ; printStr
	    dc.b 0
	even

readKey:
	lea demo,a0
	move.l (demoIndex),d1
	move.b (a0,d1),d1
	cmp.l #0,d1
	beq readKeyReal

	
	move.l (demoTimer),d0
	add.l #1,d0
	move.l d0,(demoTimer)
	and.l #$1FFF,d0 ; branch if last bits are 0
	beq emulateKeyPress
	clr.l d0
	rts
emulateKeyPress:
    add.l #1,(demoIndex)
	move.b d1,d0
	rts
    
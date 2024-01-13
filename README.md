# Port of WozMon for the SEGA Mega Drive
## Description
WozMon is a machine code monitor written by Steve Wozniak in 6502 assembly for the apple 1 [1,2].

This is a port written in motorolla 68000 assembly for the Sega Mega Drive/Genesis.  
Additionally it contains subroutines to interact with the VDP (Video Display Processor).

The program does run on original Sega Mega Drive II hardware, tested with a [Mega Everdrive Pro](https://krikzz.com/our-products/cartridges/mega-everdrive-pro.html).  
Unfortunately I do not have a [Saturn Keyboard](https://www.plutiedev.com/saturn-keyboard) to test if it works correctly.  
Fortunately some emulators such as BlastEm do have keyboard support.  
Running the code on linux should be as easy as:

### Compile and Emulate
```bash
git clone https://github.com/EythorE/sega_wozmon.git
cd sega_wozmon

# On linux this does all the manual labour and boots the rom in an emulator
make run
```

## Manual labour
Get a Sega Mega Drive emulator with Saturn Keyboard support
```bash
wget https://www.retrodev.com/blastem/blastem64-0.6.2.tar.gz
tar -xf blastem64-0.6.2.tar.gz
```

Get a compatible assembler
```bash
wget http://sun.hasenbraten.de/vasm/release/vasm.tar.gze
tar -xf vasm.tar.gz
# compile vasmm68k_mot
cd vasm
make CPU=m68k SYNTAX=mot
cd ..
```

Assemble the ROM
```bash
./vasm/vasmm68k_mot -chklabels -nocase -Fbin main.asm -L wozmon.l -o wozmon.gen
```

Emulate
```bash
# From the menu go into 'Settings' -> 'System' and change 'IO port 2 Device' to 'Saturn Keyboard'.
# To enable keyboard capture; press the right CTRL.
# load wozmon.gen
./blastem64-0.6.2/blastem wozmon.gen
```
 
## Resources
Compiler:  
http://sun.hasenbraten.de/vasm/

Great emulator with debuging capabilities and support for keyboards:  
Select saturn keyboard as the input device on port 2 and  
press right ctrl to capture keyboard when the code is running.  
https://www.retrodev.com/blastem/

Wozmon:  
[1] https://www.sbprojects.net/projects/apple1/wozmon.php  
Explained by Ben Eater:  
[2] https://youtu.be/SpG8rgI7Hec?si=jlzcbweJRtWATUog  
    https://eater.net/6502

Mega Drive tutorials and code:  
https://github.com/BigEvilCorporation/megadrive_sampless ; Most of the supporting code  
https://www.chibiakumas.com/68000/genesis.php ; More code, including good video tutorials  

Saturn keyboard reference and code:  
https://www.plutiedev.com/saturn-keyboard

Mega Drive architecture:  
https://www.copetti.org/writings/consoles/mega-drive-genesis/

References for the VDP:  
https://segaretro.org/Sega_Mega_Drive/VDP_general_usage  
https://md.railgun.works/index.php?title=VDP

References for Motorola 68000 assembly:  
https://mrjester.hapisan.com/04_MC68/Index.html  
Full reference documentation: http://wpage.unina.it/rcanonic/didattica/ce1/docs/68000.pdf  
Cheatsheet: https://www.chibiakumas.com/68000/CheatSheet.pdf  

Awesome links:  
https://github.com/And-0/awesome-megadrive
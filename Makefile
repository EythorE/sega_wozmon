ASM=bin/vasmm68k_mot
ASMFLAGS= -chklabels -nocase -Fbin
emulator=bin/blastem

wozmon.gen: $(ASM) main.asm extra.asm extra_demo.asm
	$(ASM) $(ASMFLAGS) main.asm -o $@

clean:
	rm -f wozmon.gen

run: $(emulator) wozmon.gen
	$(emulator) wozmon.gen

# Convenient
bin/vasm.tar.gz:
	mkdir -p bin
	wget -P bin/ http://sun.hasenbraten.de/vasm/release/vasm.tar.gz
$(ASM): bin/vasm.tar.gz
	cd bin && \
	tar -xf vasm.tar.gz && \
	cd vasm && \
	make CPU=m68k SYNTAX=mot && \
	mv vasmm68k_mot ..


bin/blastem64-0.6.2.tar.gz:
	mkdir -p bin
	wget -P bin/ https://www.retrodev.com/blastem/blastem64-0.6.2.tar.gz
$(emulator): bin/blastem64-0.6.2.tar.gz
	cd bin && \
	tar -xf blastem64-0.6.2.tar.gz && \
	ln -fs blastem64-0.6.2/blastem blastem

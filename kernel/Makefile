pre:
	mkdir out
	make all

all: main.o kernel
	ld -m elf_i386 -Ttext 0xc0001500 -e main -o kernel.bin \
			main.o out/*.o
clean:
	rm -rf ./out/
	rm -rf *.o
	rm -rf kernel.bin
	
main.o: main.c
	gcc -I lib/kernel/ -c -m32 -o main.o main.c -masm=intel

kernel: ./lib/kernel/
	make -C lib/kernel/

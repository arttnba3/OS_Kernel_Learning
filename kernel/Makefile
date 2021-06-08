all: main.o
	ld -m elf_i386 main.o -Ttext 0xc0001500 -e main -o kernel.bin
clean:
	rm *.o
	
main.o: main.c
	gcc -c -m32 -o main.o main.c -masm=intel

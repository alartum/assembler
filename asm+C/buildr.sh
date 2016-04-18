# Builds asm/c program and runs it

nasm -f elf64 func.asm
g++ -Wall -Wextra main.c -c -o main.o
g++ main.o func.o
./a.out

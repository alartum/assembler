; Declare external function
        extern printf  ; the C function, to be called

        SECTION .data

    fmt:    db "a=%ld",  10, 0 ; The printf format, "\n",'0'


        SECTION .text

        global print  ; the standard gcc entry point
print:    ; the program label for the entry point
    push    rbp  ; set up stack frame
    mov  rbp, rsp
 
 mov rdi,fmt  ; format for printf
 mov esi,[rbp+8*3+4]         ; first parameter for printf
 mov rax,0  ; no xmm registers
    call    printf  ; Call C function

    mov rsp, rbp
 pop rbp  ; restore 
 mov rax,0  ; normal, no error, return value
 ret   ; return

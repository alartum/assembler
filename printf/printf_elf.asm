;------------------------------
;        SYSCALL CODES
;Breaf info: 
;    (0) Syscall code goes to RAX
;    (1) RDI->RSI->RDX->RCX->R8->R9
;    (2) R10 and R11 will be broken
;    (3) Others will be saved 
;    (4) Result is in RAX:RDX
;       (if 64 bits isn't enough)
;------------------------------
sys_exit    equ    60
sys_read    equ    0
sys_write   equ    1
;------------------------------
;   STANDARD FILE DESCRIPTORS
;       (POSIX granted)
;------------------------------
stdin       equ    0
stdout      equ    1
stderr      equ    2

;------------------------------
;    CONTROL CHARACTERS
;------------------------------
BEL_    equ    0x07     ;beeps;
BS_     equ    0x08     ;backspaces one column (but not past the beginning of the line);
HT_     equ    0x09     ;goes to the next tab stop or to the end of the line if there 
                        ;is no earlier tab stop;
LF_     equ    0x0A     ;linefeed;
CR_     equ    0x0D     ;gives a carriage return;
SO_     equ    0x0E     ;activates the G1 character set;
SI_     equ    0x0F     ;activates the G0 character set;
CAN_    equ    0x18     ;interrupt escape sequences;
ESC_    equ    0x1B     ;starts an escape sequence;
DEL_    equ    0x7F     ;is ignored;
CSI_    equ    0x9B     ;is equivalent to ESC [.
;------------------------------

;------------------------------
;     PROGRAM DESCRIPTION
;------------------------------
;________________________________________
;(*)Main info
;Apparently, Just Another Printf.
;
;This program is used to explain that creating
;bicycles is great evil. I'd better take a walk
;than adding one more clone to the legion of
;printfs. Of course, it has educational purposes 
;only and will be forgotten by others quite soon.
;Nevertheless, it will live in my heart for long
;ages as the first love that I've found in asm.
;________________________________________
;(*)Key chars:
;    b = binary
;    o = octal
;    d = decimal
;    h = hexagonal    
;________________________________________
;(*)Examples:
;In:   d256h
;Out:  0x100
;
;In:   h25b
;Out:  100101
;________________________________________
;------------------------------
;    END PROGRAM DESCRIPTION
;------------------------------
section .data
    ; Can't touch this!
    sym            db    '0123456789ABCDEF'  ;Symbols to display digits
    spec_error     db    '[Error: Printf wrong format!]', LF_
    spec_error_len equ   $-spec_error
    arg_error      db    '[Error: Printf wrong arguments number!]', LF_
    arg_error_len  equ   $-arg_error
    error          db    'Something bad has happened!', LF_
    error_len      equ   $-error
    ; Secret message we want to announce
    msg          db    "Dumb as %d, hot as %x, brave as %b, obvious as %o, strong as %s, creepy as %c!", LF_, 0 
    d            equ   'd'
    h            equ   'h'
    b            equ   'b'
    o            equ   'o'
    little_msg   db    "String", 0
    c            equ   64
section .bss
    output       resb  19       ; Storage for parameters we want to print
    output_len   equ   $-output ; Its size
section .text
global _start
_start:
    mov rbp, rsp; for correct debugging
    ;write your code here
    xor rax, rax
    
    ;Secret preparations for the ritual
    push qword c
    push little_msg
    push qword o
    push qword b
    push qword h
    push qword d
    push msg
    ;|-(top)-|
    ;|--msg--|
    ;|---d---|
    ;|---h---|
    ;| (down)|
    
    call BicyclePrintf ;Okay, I must admit that it's one more version of printf
    
    ; Clearing the stack. Why not to add some hack protection?
    lea rsp, [rsp - 7*0x8]
    times 7 push qword 0x0
    lea rsp, [rsp - 7*0x8]
    
    cmp  rax, 0
    je   .Ok
    call ExitError
.Ok:
    call Exit

;-----------------------------
;Info:     Gets parameters from stack (see printf syntax) and prints them
;In:       MSG_ADDR<-ARG_1<-ARG_2<-...
;Out:      RAX (error code if any)
;Local:    R13 (block pointer), R8 (char address), R12 (number of arguments),
;          RDX (number of symbols passed), RAX (current char), R9 (local storage),
;          RDI (data for printing), RSI (pointer to the printing data), RSP and RBP (standard)
;-----------------------------
BicyclePrintf:
    mov  r13, rbp ; For cathing out of border errors
    mov  rbp, rsp ; Saving our block pointer comfortable work
    
    mov  r8, [rbp + 0x8] ; Loading string address [return address]
    mov  r12, 2 ; Number of args passed. We need to pass ret. addr. and msg pointer 
    
.GetPart: ; Getting part of the message until the end or argument
        xor rdx, rdx
        mov rsi, r8    ; Saving part begin position
        .Next:
            inc r8   ; Moving forward
            inc rdx    ; Increasing count of symbols passed
            mov al, [r8] ; Saving current symbol
            cmp al, 0    ; Is it the end?
            je  .PrintPart ; Print it, if so
            cmp al, '%' ; Is it argument?
            je  .PrintPart ; Print, if so
            jmp .Next ; Proceed
            
        .PrintPart:    ; Printing the part of the message
        mov r9b, al
        call DisplayText ; Printing [RSI is pointing to the start]
        cmp r9b, 0  ; If we've reached the end, nothing to do here
        je .End    ; Leaving
        
        .PrepareArg:    ;If we're not at the end
        lea r9, [rbp + r12*8] ; Getting current parameter address
        mov rdi, [r9]  ; Getting the parameter

        cmp r9, r13 ; If the argument is out of border, exit
        jnb .EndArgError ; Exit
        
        inc r12    ; Increasing number of arguments passed
        inc r8 ; Moving to the key char
        mov r9b, [r8] ; Getting it
        inc r8 ; Moving to the next part
        
        .cCheck:    
        cmp r9b, 'c' ; Printing symbol
        jne .sCheck
        mov rsi, output
        mov [rsi], dil
        mov rdx, 1
        jmp .PrintArg
        
        .sCheck:
        cmp r9b, 's'
        jne .numCheck
        mov rsi, rdi ; Get the address of string
        call StrLen ; Count the length (store in RDX)
        mov rsi, rdi ; Restore the address
        jmp .PrintArg
        
        .numCheck:
        mov rsi, output + output_len - 1 ; Preparing the storage
                      
        .dCheck:
        cmp r9b, 'd'
        jne .bCheck
        call PutDec ; Count the length (store in RDX)
        jmp .PrintArg
        
        .bCheck:
        cmp r9b, 'b'
        jne .oCheck
        call PutBin 
        jmp .PrintArg 
        
        .oCheck:
        cmp r9b, 'o'
        jne .xCheck
        call PutOct 
        jmp .PrintArg 
        
        .xCheck:
        cmp r9b, 'x'
        jne .EndError
        call PutHex 
        jmp .PrintArg 
        
        .PrintArg:
        call DisplayText
        jmp .GetPart
;GetPart END
.EndArgError:
    mov rsi, arg_error
    mov rdx, arg_error_len
    call DisplayText
    mov rsp, rbp ; Equal to leave
    mov rbp, r13 ; Repair the block pointer
    mov rax, 2
    ret
.EndError:
    mov rsi, spec_error
    mov rdx, spec_error_len
    call DisplayText
    mov rsp, rbp ; Equal to leave
    mov rbp, r13 ; Repair the block pointer
    mov rax, 1
    ret
.End:
    mov rsp, rbp ; Equal to leave
    mov rbp, r13 ; Repair the block pointer
    xor rax, rax ; No error
    ret

;-----------------------------
;Info:     Prints the string from the given address
;In:       RSI (pointer to string)
;Out:      -
;Local:    RAX (store address here)
;-----------------------------
PrintString:
    mov  rax, rsi
    call StrLen
    mov  rsi, rax
    call DisplayText
    
    ret

;-----------------------------
;Info:     Counts number of characters in string (until 0 symbol)
;In:       RSI* (pointer to string)
;Out:      RDX (amount of characters)
;Local:    RBX (current symbol)
;-----------------------------
StrLen:
    xor rdx, rdx
    .Repeat:
        mov bl, [rsi]
        cmp bl, 0
        je  .End
        inc rsi
        inc rdx
    jmp .Repeat
.End:
    ret
;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in hex form. Number is stored like: 0000'1''2''3''4'
;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
;Out:      RSI (address of last byte written [first numbers byte]), RDX (amount of symbols in number)
;Local:    RCX (last bits of the number), RAX (symbols)
;-----------------------------
PutHex:
    xor  rdx, rdx
    xor  rcx, rcx
    
    test rdi, rdi
    jns  .GetDigit
    mov  bl, '-'
    neg  rdi
.GetDigit:
    mov  cl, dil    ; [1b]->[1b]<-
    and  cl, 0xF    
    shr  rdi, 4   ; Get rid of last byte
    
    mov  rax, sym
    add  rax, rcx
    mov  cl, [rax]    ; Load the digit
    mov  [rsi], cl  ; Put the digit
    dec  rsi
    inc  rdx ; Chars amount is increased by 1
    cmp  rdi, 0  ; If dividend is 0, we must exit
jne  .GetDigit

    mov  [rsi], byte 'x'
    inc  rdx
    dec  rsi
    mov  [rsi], byte '0'
    inc  rdx
    cmp bl, '-'
    jne .End

.WriteMinus:
    dec rsi
    mov [rsi], bl ; WTF?! stosb is not working
    inc rdx
    
.End:    
    ret


;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in octal form. Number is stored like: 0000'1''2''3''4'
;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
;Local:    RCX (last bits of the number)
;-----------------------------
PutOct:
    xor  rdx, rdx
    xor  rcx, rcx
    
    test rdi, rdi
    jns  .GetDigit
    mov  bl, '-'
    neg  rdi
.GetDigit:
    mov  cl, dil    ; [1b]->[1b]<-
    and  cl, 0x7    ; 
    shr  rdi, 3   ; Get rid of last 3 bits
    
    add  cl, '0'
    mov  [rsi], cl
    dec  rsi
    inc  rdx

    cmp  rdi, 0  ; If dividend is 0, we must exit
    jne  .GetDigit
    
cmp bl, '-'
jne .End
.WriteMinus:
    mov [rsi], bl ; WTF?! stosb is not working
    dec rsi
    inc rdx
.End:
    inc rsi
    
    ret

;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in binary form. Number is stored like: 0000'1''2''3''4'
;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
;Local:    RCX (last bits of the number), RAX (symbols)
;-----------------------------
PutBin:
    xor  rdx, rdx
    xor  rcx, rcx
    
    test rdi, rdi
    jns  .GetDigit
    mov  bl, '-'
    neg  rdi
.GetDigit:
    mov  cl, dil    ; [1b]->[1b]<-
    shr  rdi, 1   ; Get rid of last byte
    
    and  cl, 1
    mov  rax, '0'
    add  rax, rcx
    mov  [rsi], al
    dec  rsi
    inc  rdx

    cmp  rdi, 0  ; If dividend is 0, we must exit
    jne  .GetDigit

cmp bl, '-'
jne .End
.WriteMinus:
    mov [rsi], bl ; WTF?! stosb is not working
    dec rsi
    inc rdx
.End:
    inc rsi
    
    ret

;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in decimal form. Number is stored like: 0000'1''2''3''4'
;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
;Local:    RAX (arithmetics), RCX (arithmetics)
;-----------------------------
PutDec:
    mov  rax, rdi ;dividend
    mov  rdi, 10
    xor  rcx, rcx
    
    test rax, rax ; test if negative (look up first bit)
    jns  .GetDigit
    mov  bl, '-'
    neg  rax

.GetDigit:
    xor  rdx, rdx    ;remainder
    div  rdi   ;Dividing by qword value
    xchg rax, rdx ;Our number is in rdx now. Reminder is in al
    add  al, '0'    ; Get char number
    mov  [rsi], al ; WTF?! stosb is not working
    dec  rsi
    inc  rcx
    mov  rax, rdx ; Move dividend back to rax
    cmp  rax, 0  ; If dividend is 0, we must exit
    jne  .GetDigit

cmp bl, '-'
jne .End
.WriteMinus:
    mov [rsi], bl ; WTF?! stosb is not working
    dec rsi
    inc rcx
.End:
    inc rsi
    mov rdx, rcx
    
    ret
;-----------------------------
;Info:     Saves data from standard input to somewhere else
;In:       RSI (pointer to storage), RDX (storage capacity)
;Out:      -
;Local:    RAX (syscall code), RDI (file descriptor[input]) 
;-----------------------------
SaveData:
    mov  rax, sys_read ;We are going to read data
    mov  rdi, stdin    ;From stdin 
    syscall
    ret

;-----------------------------
;Info:     Prints the text
;In:       RSI (pointer to message), RDX (message length)
;Out:      -
;Local:    RAX (syscall code), RDI (file descriptor) 
;-----------------------------
DisplayText:
    mov  rax, sys_write
    mov  rdi, stdout
    syscall
    ret
;-----------------------------
;Info:     Exits the program
;In:       -
;Out:      -
;Local:    RAX (syscall code), RDI (error code) 
;-----------------------------
Exit:
    mov  rax, sys_exit
    mov  rdi, 0
    syscall
    ret
    ;Must return to the parent process here
;-----------------------------
;Info:     Exits the program with error
;In:       -
;Out:      -
;Local:    RAX (syscall code), RDI (error code) 
;-----------------------------
ExitError:
    mov  rsi, error; Write msg address to rdi
    mov  rdx, error_len; We'll need the number of symbols too
    call DisplayText
    
    mov  rax, sys_exit
    mov  rdi, 1
    syscall
    ret

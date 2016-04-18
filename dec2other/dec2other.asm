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
;This program is used to transfer numbers
;between different count systems (for example, 
;decimal to octal). Input is provided in 
;x<NUMBER>y format, where 'x' stands for the
;input and 'y' for the output system.
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
    sym        db    '0123456789ABCDEF'     ;Symbols to display digits
    msg        db    'Input something:', LF_,'>', 0;Sample string
    msg_len    equ   $-msg                  ;String length
    err        db    'Input is broken', 0, LF_
    err_len    equ   $-err  
    line       db    LF_; Just linefeed

section .bss
    data       resb  19       ; Max size is len(2^64)+1=21
    data_len   equ   $-data   ; The size of that something
    result     resb  19       ; Bla-bla-bla
    result_len equ   $-result ; Bla-bla
                              ; Bla
section .text
global main
main:
    mov  rbp, rsp; for correct debugging
    
    mov  rsi, msg; Write msg address to rdi
    mov  rdx, msg_len; We'll need the number of symbols too
    call DisplayText
    
    mov  rsi, data
    mov  rdx, data_len
    call SaveData
    
    call ScanNumber
    
    cmp  rax, 1
    je   .Err
    call Exit
.Err:
    call ExitError
;-----------------------------
;Info:     Gets string from RSI and parses it in A<NUM>B, where A is input specificator
;          and B is output specificator. Possible specificators: 
;                                        d = decimal, h = hex, o = octal, b = binary
;In:       RDI (string size), RSI (pointer to the string to parse)
;Out:      RAX (error code if any)
;Local:    -
;-----------------------------
ScanNumber:
    
    ; Loading the key char
    mov al, [rsi]
    ;Moving forward
    inc rsi
    dec rdi
  
;.bCheck  
    cmp  al, 'b'
    jne  .dCheck
    call GetBin
    jmp  .EndCheck
    
.dCheck:
    cmp  al, 'd'
    jne  .hCheck 
    call GetDec
    jmp  .EndCheck
    
.hCheck:
    cmp  al, 'h'
    jne  .oCheck 
    call GetHex
    jmp  .EndCheck

.oCheck:
    cmp  al, 'o'
    jne  .ErrorEnd
    call GetOct
    ;jmp .endCheck

.EndCheck:
    ; Number is in RAX now, RSI is pointing to first NaN
    mov rdi, rax ; Load our number to convert to string
    mov al, [rsi] ; Put the convert option char in al
    mov rsi, result+result_len-1; Set writing position
    
    ; Lets analyze the key value in al
;.bPut:
    cmp  al, 'b'
    jne  .dPut
    call PutBin
    jmp  .EndPut
    
.dPut:
    cmp  al, 'd'
    jne  .hPut
    call PutDec
    jmp  .EndPut

.hPut:
    cmp   al, 'h'
    jne  .oPut
    call PutHex
    jmp  .EndPut

.oPut:
    cmp  al, 'o'
    jne  .ErrorEnd
    call PutOct
    ;jmp .EndPut
    
.EndPut:
    mov  rsi, rax  ; Save last byte written
    mov  rdx, rcx ; Count of characters in our number
    call DisplayText

    ret
.ErrorEnd:
    mov  rax, 1
    ret
    
;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in hex form. Number is stored like: 0000'1''2''3''4'
;In:       RDI (number to convert), RSI (pointer to the storage's _END_)
;Out:      RAX (address of last byte written [first numbers byte]), RCX (amount of symbols in number)
;Local:    RDX (last bits of the number)
;-----------------------------
PutHex:
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    mov  dl, dil    ; [1b]->[1b]<-
    and  dl, 0xF    
    shr  rdi, 4   ; Get rid of last byte
    
    mov  rax, sym
    add  rax, rdx
    mov  dl, [rax]    ; Load the digit
    mov  [rsi], dl  ; Put the digit
    dec  rsi
    inc  rcx ; Chars amount is increased by 1

    cmp  rdi, 0  ; If dividend is 0, we must exit
    jne  .GetDigit
    
.End: 
    mov  rax, rsi
    mov  [rax], byte 'x'
    inc  rcx
    dec  rax
    mov  [rax], byte '0'
    inc  rcx
    
    ret


;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in octal form. Number is stored like: 0000'1''2''3''4'
;In:       RDI (number to convert), RSI (pointer to the storage's _END_)
;Out:      RAX (address of last byte written), RCX (amount of symbols in number)
;Local:    RDX (last bits of the number)
;-----------------------------
PutOct:
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    mov  dl, dil    ; [1b]->[1b]<-
    and  dl, 0x7    ; 
    shr  rdi, 3   ; Get rid of last 3 bits
    
    add  dl, '0'
    mov  [rsi], dl
    dec  rsi
    inc  rcx

    cmp  rdi, 0  ; If dividend is 0, we must exit
    jne  .GetDigit
    
.End: 
    mov  rax, rsi
    inc  rax
    
    ret

;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in binary form. Number is stored like: 0000'1''2''3''4'
;In:       RDI (number to convert), RSI (pointer to the storage's _END_)
;Out:      RAX (address of last byte written), RCX (amount of symbols in number)
;Local:    RDX (last bits of the number)
;-----------------------------
PutBin:
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    mov  dl, dil    ; [1b]->[1b]<-
    shr  rdi, 1   ; Get rid of last byte
    
    and  dl, 1
    mov  rax, '0'
    add  rax, rdx
    mov  [rsi], al
    dec  rsi
    inc  rcx

    cmp  rdi, 0  ; If dividend is 0, we must exit
    jne  .GetDigit
    
.End: 
    mov  rax, rsi
    inc  rax
    mov  rsi, r10
    
    ret

;-----------------------------
;Info:     Gets number from RDI and writes it in RSI in decimal form. Number is stored like: 0000'1''2''3''4'
;In:       RDI (number to convert), RSI (pointer to the storage's _END_)
;Out:      RAX (address of last byte written), RCX (amount of symbols in number)
;Local:    R8 (stores 10 here)
;-----------------------------
PutDec:
    mov  r8, 10
    mov  rax, rdi ;dividend
    xor  rcx, rcx
.GetDigit:
    xor  rdx, rdx    ;remainder
    div  r8    ;Dividing by qword value
    xchg rax, rdx ;Our number is in rdx now. Reminder is in al
    add  al, '0'    ; Get char number
    mov  [rsi], al ; WTF?! stosb is not working
    dec  rsi
    inc  rcx
    mov  rax, rdx ; Move dividend back to rax
    cmp  rax, 0  ; If dividend is 0, we must exit
    jne  .GetDigit
    
.End: 
    mov  rax, rsi
    inc  rax
    
    ret
;-----------------------------
;Info:     Gets decimal number from memory and stores it in RAX
;In:       RDI (number of chars in string), RSI (pointer to string)
;Out:      RAX (decimal number encoded), RSI (first NaN symbol)
;Local:    RCX (current char counter), RDX (number buffer), R8B (save flags)
;-----------------------------
GetDec:
    lahf           
    mov  r8w, ax  
    cld
    
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    xor  rax, rax
    lodsb ; Load [rsi + k] into al
    inc  rcx
    cmp  al, '0'
    jb   .End
    cmp  al, '9'
    ja   .End
    sub  al, '0'
    imul rdx, 10
    add  rax, rdx
    mov  rdx, rax
    cmp  rcx, rdi
    jne  .GetDigit
.End:
    mov  ax, r8w    
    dec  rsi
    mov  rax, rdx
    ret
 
;-----------------------------
;Info:     Gets binary number from memory and stores it in RAX
;In:       RDI (number of chars in string), RSI (pointer to string)
;Out:      RAX (binary number encoded), RSI (first NaN symbol)
;Local:    RCX (current char counter), RDX (number buffer), R8B (save flags)
;Flags:    DF -> 0
;-----------------------------
GetBin:
    lahf           
    mov  r8w, ax  
    cld
    
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    xor  rax, rax
    lodsb ; Load [rsi + k] into al
    inc  rcx
    cmp  al, '0'
    jb   .End
    cmp  al, '7'
    ja   .End
    sub  al, '0'
    shl  rdx, 1 ; !!!!!!!!!<----BE CAREFUL! VERY COMPLICATED OPTIMISATION HERE!
    add  rax, rdx
    mov  rdx, rax
    cmp  rcx, rdi
    jne  .GetDigit
.End:
    mov  ax, r8w    
    sahf
    dec  rsi
    mov  rax, rdx
    ret

          
;-----------------------------
;Info:     Gets octal number from memory and stores it in RAX
;In:       RDI (number of chars in string), RSI (pointer to string)
;Out:      RAX (octal number encoded), RSI (first NaN symbol)
;Local:    RCX (current char counter), RDX (number buffer), R8B (save flags)
;Flags:    DF -> 0
;-----------------------------
GetOct:
    lahf           
    mov  r8w, ax  
    cld
    
    xor  rdx, rdx
    xor  rcx, rcx
.GetDigit:
    xor  rax, rax
    lodsb ; Load [rsi + k] into al
    inc  rcx
    cmp  al, '0'
    jb  .End
    cmp  al, '7'
    ja  .End
    sub  al, '0'
    shl  rdx, 3 ; !!!!!!!!!<----BE CAREFUL! VERY COMPLICATED OPTIMISATION HERE!
    add  rax, rdx
    mov  rdx, rax
    cmp  rcx, rdi
    jne  .GetDigit
.End:
    mov  ax, r8w    
    sahf
    dec  rsi
    mov  rax, rdx
    ret

;-----------------------------
;Info:     Gets hex number from memory and stores it in RAX
;In:       RDI (number of chars in string), RSI (pointer to string)
;Out:      RAX (hex number encoded), RSI (first NaN symbol)
;Local:    RCX (current char counter), RDX (number buffer), R8B (save flags)
;Flags:    DF -> 0
;-----------------------------
GetHex:
    inc  rsi   ; Testing if the format is 0x[NUM]
    cmp  [rsi], byte 'x'
    jne  ExitError ; Writing error if input is wrong
    inc  rsi
    
    lahf           ; Saving flags
    mov  r8w, ax  
    cld
    
    xor  rdx, rdx
    xor  rcx, rcx
    
.GetDigit:
    xor  rax, rax
    lodsb ; Load [rsi + k] into al
    inc  rcx
    cmp  al, '0'
    jb   .End
    cmp  al, 'F'
    ja   .End
    sub  al, '0'
    cmp  al, 9
    jna  .Pass
    sub  al, 7 ; ASCII is bad for programmers
.Pass:
    shl  rdx, 4 ; !!!!!!!!!<----BE CAREFUL! NON-TRIVIAL OPTIMISATION HERE!
    add  rax, rdx
    mov  rdx, rax
    cmp  rcx, rdi
    jne  .GetDigit
.End:
    mov  ax, r8w    ; Loading flags
    sahf
    dec  rsi
    mov  rax, rdx
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
    mov  rsi, line
    mov  rdx, 1
    call DisplayText
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
    mov  rsi, err; Write msg address to rdi
    mov  rdx, err_len; We'll need the number of symbols too
    call DisplayText
    
    mov  rax, sys_exit
    mov  rdi, 1
    syscall
    ret
    ; Yep, it universary 500 line! Let's celebrate!

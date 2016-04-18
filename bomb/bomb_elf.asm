;------------------------------
;        SYSCALL CODES
;Brief info: 
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

section .data
    ; The overflow we will allow to use
    mistake    equ   16 ; Just enough to rewrite rbp and return address
    
    ; Welcome message
    input_msg  db    "Tell me the password:", LF_
    input_len  equ   $-input_msg
    ; Success message
    succ_msg   db    "Access granted!", LF_
    succ_len   equ   $-succ_msg
    ; Failure message
    fail_msg   db    "Access denied!", LF_
    fail_len   equ   $-fail_msg
    
    help1      db    "*it smells like there is a memory leak nearby*"
    iter       dq    5 ; Number of iterations in calc
    key        dq    144 ; Key/2: we'll double it at CALC_SIN
                         ; Keyword that opens the box: B_O_M_B
section .bss
    pass_len   equ   10
    pass_msg   resb  pass_len
    temp       resq  1
section .text
global _start
_start:
    mov rbp, rsp; for correct debugging
    ; Invites user to the input
    mov rsi, input_msg
    mov rdx, input_len
    call DisplayText
    
    ; Unsafe scanf: we can return to Success by changing the return address
    ; Allocating space for password
    ; Reading it
    call READ_PASS
    
    xor  rdx, rdx
    xor  rax, rax
    mov  dl,  [pass_msg]
    add  rax, rdx
    mov  dl,  [pass_msg+2]
    add  rax, rdx
    mov  dl,  [pass_msg+4]
    add  rax, rdx
    mov  dl,  [pass_msg+6]
    add  rax, rdx

    mov  [temp], rax
    call CALC_SIN
    
    mov  rax, [temp] ; Let's take the result from CALC_SIN
    call HANDLER
    
FAILURE: ;Jump here on failure
    call DENY
SUCCESS: ;Jump here on success
    call GRANT


;Access handler 
HANDLER:
    mov bl, 5
    mul bl
    mov rbx, [rsp]
    add rbx, rax
    mov [rsp], rbx
    ret
;-----------------------------
;Info:     Calculates hash value depending on key
;In:       qword [RSP]
;Out:      -
;Local:    RCX (loop counter), fpu registers
;------------------------------
CALC_SIN:
    pow  equ 20  ; Power to rise in. Makes the difference in levels more clear
   
    enter 0,0
    mov   rcx, [iter]
    jrcxz .END
    
    finit
    ; Load 8 bytes from stack
    fild  qword [temp] ; st0 = x (x = [temp])
    fild  qword [key] ; st0=key, st1 = x
    fadd  st0 ; Double it. <--- VERY IMPORTANT
    fdivr st1 ; st0 = x/key, st1 = 2pi*x, st2 = x
    fldpi ; st0 = pi, st1 = x/key
    fmul  st1 ; std0 = pi*x/key, st1 = x/key
    fadd  st0 ; st0 = 2pi*x/key, st1 = x/key
    
    fldz  ; st0 = 0, st1 = 2pi*x/key, st2 = 2pi*x
    fst   st3 ; st0 = 0, st1 = 2pi*x/key, st2 = 2pi*x, st3 = 0
    
.SUM:
    fadd  st1 ; st0 = k* 2pi*x/key
    fld   st0 ; st0 = k*2pi*x/key, st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = summ
    fcos  ; st0 = cos (k*2pi*x/key), st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = summ
    fadd  st4 ; st0 = new_summ, st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = summ
    fstp  st4 ; st0 = k*2pi*x/key, st1 = 2pi*x/key, st2 = new_summ
loop .SUM

    fild  qword [iter] ; st0 = new_summ, st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = new_summ
    fdivr st4 ; st0 = new_summ/iter, st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = new_summ
    fst   st3 ; st0 = new_summ/iter, st1 = k*2pi*x/key, st2 = 2pi*x/key, st3 = new_summ/iter
    mov rcx, pow
    fld1 ; st0=1
    fst   st5 ; st3=1
    fadd  st0, st0 ; st0=2
    fdivr st5 ; st0=1/2
    fxch  st5 ; 
    ffreep st0
.POW:
    fmul  st3 ; st0 = (new_summ)^k
    fabs  ; st0 = |st0|
    fcomi  st4 ; Write flags directly to CPU
    fcmovb st0, st4 ; Move st2 to st0 if st0 <= 0.5
loop .POW
.END_LOOP:
    frndint
    fistp  qword [temp] ; [temp] = st0
.END:
    leave
    ret
    
;-----------------------------
;Info:     Gets password from standard input
;In:       -
;Out:      -
;Local:    RSI, RDX
;------------------------------
READ_PASS:
    ; Allocate space for input, save sp, bp. Nesting level is 0
    enter pass_len, 0 ; push rbp/ mov rbp, rsp/ sub rsp, 0
    mov   rsi, rsp
    mov   rdx, pass_len + mistake ; Hmm, strange!
    call  SaveData ; (!) Heap is possibly broken here (!)
    
    ; Copy it to 
    mov   rsi, rsp
    mov   rdi, pass_msg
    mov   rdx, pass_len
    call  MEM_CPY
    
    leave ; mov rsp, rbp /pop rbp
    ret

;-----------------------------
;Info:     Copies str_len bytes from str_source to str_dest
;In:       RSI (str_source), RDI (str_dest), RDX (str_len)
;Out:      -
;Local:    RCX (loop counter)
;-----------------------------
MEM_CPY:
    cld ; Clear direction flag: moving forward
    push  rcx ; Save it
    mov   rcx, rdx
    jrcxz .END
.TOP:
    lodsb              ;char in al 
    stosb              ;store it in password
    loop   .TOP         ;loop until done
    
.END:
    pop rcx ; Restore it

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
;Info:     Denies access
;In:       -
;Out:      -
;Local:    RAX (syscall code), RDI (error code) 
;-----------------------------
DENY:
    mov  rsi, fail_msg; Write msg address to rdi
    mov  rdx, fail_len; We'll need the number of symbols too
    call DisplayText
    
    mov  rax, sys_exit
    mov  rdi, 1
    syscall
    ret
    
;-----------------------------
;Info:     Grants access
;In:       -
;Out:      -
;Local:    RAX (syscall code), RDI (error code) 
;-----------------------------
GRANT:
    mov  rsi, succ_msg; Write msg address to rdi
    mov  rdx, succ_len; We'll need the number of symbols too
    call DisplayText
    
    mov  rax, sys_exit
    mov  rdi, 0
    syscall
    ret

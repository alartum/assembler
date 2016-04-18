     1                                  ;------------------------------
     2                                  ;        SYSCALL CODES
     3                                  ;Breaf info: 
     4                                  ;    (0) Syscall code goes to RAX
     5                                  ;    (1) RDI->RSI->RDX->RCX->R8->R9
     6                                  ;    (2) R10 and R11 will be broken
     7                                  ;    (3) Others will be saved 
     8                                  ;    (4) Result is in RAX:RDX
     9                                  ;       (if 64 bits isn't enough)
    10                                  ;------------------------------
    11                                  sys_exit    equ    60
    12                                  sys_read    equ    0
    13                                  sys_write   equ    1
    14                                  ;------------------------------
    15                                  ;   STANDARD FILE DESCRIPTORS
    16                                  ;       (POSIX granted)
    17                                  ;------------------------------
    18                                  stdin       equ    0
    19                                  stdout      equ    1
    20                                  stderr      equ    2
    21                                  
    22                                  ;------------------------------
    23                                  ;    CONTROL CHARACTERS
    24                                  ;------------------------------
    25                                  BEL_    equ    0x07     ;beeps;
    26                                  BS_     equ    0x08     ;backspaces one column (but not past the beginning of the line);
    27                                  HT_     equ    0x09     ;goes to the next tab stop or to the end of the line if there 
    28                                                          ;is no earlier tab stop;
    29                                  LF_     equ    0x0A     ;linefeed;
    30                                  CR_     equ    0x0D     ;gives a carriage return;
    31                                  SO_     equ    0x0E     ;activates the G1 character set;
    32                                  SI_     equ    0x0F     ;activates the G0 character set;
    33                                  CAN_    equ    0x18     ;interrupt escape sequences;
    34                                  ESC_    equ    0x1B     ;starts an escape sequence;
    35                                  DEL_    equ    0x7F     ;is ignored;
    36                                  CSI_    equ    0x9B     ;is equivalent to ESC [.
    37                                  ;------------------------------
    38                                  
    39                                  ;------------------------------
    40                                  ;     PROGRAM DESCRIPTION
    41                                  ;------------------------------
    42                                  ;________________________________________
    43                                  ;(*)Main info
    44                                  ;Apparently, Just Another Printf.
    45                                  ;
    46                                  ;This program is used to explain that creating
    47                                  ;bicycles is great evil. I'd better take a walk
    48                                  ;than adding one more clone to the legion of
    49                                  ;printfs. Of course, it has educational purposes 
    50                                  ;only and will be forgotten by others quite soon.
    51                                  ;Nevertheless, it will live in my heart for long
    52                                  ;ages as the first love that I've found in asm.
    53                                  ;________________________________________
    54                                  ;(*)Key chars:
    55                                  ;    b = binary
    56                                  ;    o = octal
    57                                  ;    d = decimal
    58                                  ;    h = hexagonal    
    59                                  ;________________________________________
    60                                  ;(*)Examples:
    61                                  ;In:   d256h
    62                                  ;Out:  0x100
    63                                  ;
    64                                  ;In:   h25b
    65                                  ;Out:  100101
    66                                  ;________________________________________
    67                                  ;------------------------------
    68                                  ;    END PROGRAM DESCRIPTION
    69                                  ;------------------------------
    70                                  section .data
    71                                      ; Can't touch this!
    72 00000000 303132333435363738-         sym            db    '0123456789ABCDEF'  ;Symbols to display digits
    73 00000009 39414243444546     
    74 00000010 5B4572726F723A2050-         spec_error     db    '[Error: Printf wrong format!]', LF_
    75 00000019 72696E74662077726F-
    76 00000022 6E6720666F726D6174-
    77 0000002B 215D0A             
    78                                      spec_error_len equ   $-spec_error
    79 0000002E 5B4572726F723A2050-         arg_error      db    '[Error: Printf wrong arguments number!]', LF_
    80 00000037 72696E74662077726F-
    81 00000040 6E6720617267756D65-
    82 00000049 6E7473206E756D6265-
    83 00000052 72215D0A           
    84                                      arg_error_len  equ   $-arg_error
    85 00000056 536F6D657468696E67-         error          db    'Something bad has happened!', LF_
    86 0000005F 206261642068617320-
    87 00000068 68617070656E656421-
    88 00000071 0A                 
    89                                      error_len      equ   $-error
    90                                      ; Secret message we want to announce
    91 00000072 44756D622061732025-         msg          db    "Dumb as %d, hot as %x, brave as %b, obvious as %o, strong as %s, creepy as %c!", LF_, 0 
    92 0000007B 642C20686F74206173-
    93 00000084 2025782C2062726176-
    94 0000008D 652061732025622C20-
    95 00000096 6F6276696F75732061-
    96 0000009F 7320256F2C20737472-
    97 000000A8 6F6E67206173202573-
    98 000000B1 2C2063726565707920-
    99 000000BA 6173202563210A00   
   100                                      d            equ   'd'
   101                                      h            equ   'h'
   102                                      b            equ   'b'
   103                                      o            equ   'o'
   104 000000C2 537472696E6700              little_msg   db    "String", 0
   105                                      c            equ   64
   106                                  section .bss
   107 00000000 <res 00000013>              output       resb  19       ; Storage for parameters we want to print
   108                                      output_len   equ   $-output ; Its size
   109                                  section .text
   110                                  global _start
   111                                  _start:
   112 00000000 4889E5                      mov rbp, rsp; for correct debugging
   113                                      ;write your code here
   114 00000003 4831C0                      xor rax, rax
   115                                      
   116                                      ;Secret preparations for the ritual
   117 00000006 6A40                        push qword c
   118 00000008 68[C2000000]                push little_msg
   119 0000000D 6A6F                        push qword o
   120 0000000F 6A62                        push qword b
   121 00000011 6A68                        push qword h
   122 00000013 6A64                        push qword d
   123 00000015 68[72000000]                push msg
   124                                      ;|-(top)-|
   125                                      ;|--msg--|
   126                                      ;|---d---|
   127                                      ;|---h---|
   128                                      ;| (down)|
   129                                      
   130 0000001A E828000000                  call BicyclePrintf ;Okay, I must admit that it's one more version of printf
   131                                      
   132                                      ; Clearing the stack. Why not to add some hack protection?
   133 0000001F 488D6424C8                  lea rsp, [rsp - 7*0x8]
   134 00000024 6A00<rept>                  times 7 push qword 0x0
   135 00000032 488D6424C8                  lea rsp, [rsp - 7*0x8]
   136                                      
   137 00000037 4883F800                    cmp  rax, 0
   138 0000003B 7405                        je   .Ok
   139 0000003D E87B020000                  call ExitError
   140                                  .Ok:
   141 00000042 E869020000                  call Exit
   142                                  
   143                                  ;-----------------------------
   144                                  ;Info:     Gets parameters from stack (see printf syntax) and prints them
   145                                  ;In:       MSG_ADDR<-ARG_1<-ARG_2<-...
   146                                  ;Out:      RAX (error code if any)
   147                                  ;Local:    R13 (block pointer), R8 (char address), R12 (number of arguments),
   148                                  ;          RDX (number of symbols passed), RAX (current char), R9 (local storage),
   149                                  ;          RDI (data for printing), RSI (pointer to the printing data), RSP and RBP (standard)
   150                                  ;-----------------------------
   151                                  BicyclePrintf:
   152 00000047 4989ED                      mov  r13, rbp ; For cathing out of border errors
   153 0000004A 4889E5                      mov  rbp, rsp ; Saving our block pointer comfortable work
   154                                      
   155 0000004D 4C8B4508                    mov  r8, [rbp + 0x8] ; Loading string address [return address]
   156 00000051 41BC02000000                mov  r12, 2 ; Number of args passed. We need to pass ret. addr. and msg pointer 
   157                                      
   158                                  .GetPart: ; Getting part of the message until the end or argument
   159 00000057 4831D2                          xor rdx, rdx
   160 0000005A 4C89C6                          mov rsi, r8    ; Saving part begin position
   161                                          .Next:
   162 0000005D 49FFC0                              inc r8   ; Moving forward
   163 00000060 48FFC2                              inc rdx    ; Increasing count of symbols passed
   164 00000063 418A00                              mov al, [r8] ; Saving current symbol
   165 00000066 3C00                                cmp al, 0    ; Is it the end?
   166 00000068 7406                                je  .PrintPart ; Print it, if so
   167 0000006A 3C25                                cmp al, '%' ; Is it argument?
   168 0000006C 7402                                je  .PrintPart ; Print, if so
   169 0000006E EBED                                jmp .Next ; Proceed
   170                                              
   171                                          .PrintPart:    ; Printing the part of the message
   172 00000070 4188C1                          mov r9b, al
   173 00000073 E82B020000                      call DisplayText ; Printing [RSI is pointing to the start]
   174 00000078 4180F900                        cmp r9b, 0  ; If we've reached the end, nothing to do here
   175 0000007C 0F84D2000000                    je .End    ; Leaving
   176                                          
   177                                          .PrepareArg:    ;If we're not at the end
   178 00000082 4E8D4CE500                      lea r9, [rbp + r12*8] ; Getting current parameter address
   179 00000087 498B39                          mov rdi, [r9]  ; Getting the parameter
   180                                  
   181 0000008A 4D39E9                          cmp r9, r13 ; If the argument is out of border, exit
   182 0000008D 0F8381000000                    jnb .EndArgError ; Exit
   183                                          
   184 00000093 49FFC4                          inc r12    ; Increasing number of arguments passed
   185 00000096 49FFC0                          inc r8 ; Moving to the key char
   186 00000099 458A08                          mov r9b, [r8] ; Getting it
   187 0000009C 49FFC0                          inc r8 ; Moving to the next part
   188                                          
   189                                          .cCheck:    
   190 0000009F 4180F963                        cmp r9b, 'c' ; Printing symbol
   191 000000A3 7514                            jne .sCheck
   192 000000A5 48BE-                           mov rsi, output
   193 000000A7 [0000000000000000] 
   194 000000AF 40883E                          mov [rsi], dil
   195 000000B2 BA01000000                      mov rdx, 1
   196 000000B7 EB51                            jmp .PrintArg
   197                                          
   198                                          .sCheck:
   199 000000B9 4180F973                        cmp r9b, 's'
   200 000000BD 750D                            jne .numCheck
   201 000000BF 4889FE                          mov rsi, rdi ; Get the address of string
   202 000000C2 E8A8000000                      call StrLen ; Count the length (store in RDX)
   203 000000C7 4889FE                          mov rsi, rdi ; Restore the address
   204 000000CA EB3E                            jmp .PrintArg
   205                                          
   206                                          .numCheck:
   207 000000CC 48BE-                           mov rsi, output + output_len - 1 ; Preparing the storage
   208 000000CE [1200000000000000] 
   209                                                        
   210                                          .dCheck:
   211 000000D6 4180F964                        cmp r9b, 'd'
   212 000000DA 7507                            jne .bCheck
   213 000000DC E871010000                      call PutDec ; Count the length (store in RDX)
   214 000000E1 EB27                            jmp .PrintArg
   215                                          
   216                                          .bCheck:
   217 000000E3 4180F962                        cmp r9b, 'b'
   218 000000E7 7507                            jne .oCheck
   219 000000E9 E824010000                      call PutBin 
   220 000000EE EB1A                            jmp .PrintArg 
   221                                          
   222                                          .oCheck:
   223 000000F0 4180F96F                        cmp r9b, 'o'
   224 000000F4 7507                            jne .xCheck
   225 000000F6 E8DB000000                      call PutOct 
   226 000000FB EB0D                            jmp .PrintArg 
   227                                          
   228                                          .xCheck:
   229 000000FD 4180F978                        cmp r9b, 'x'
   230 00000101 7531                            jne .EndError
   231 00000103 E87A000000                      call PutHex 
   232 00000108 EB00                            jmp .PrintArg 
   233                                          
   234                                          .PrintArg:
   235 0000010A E894010000                      call DisplayText
   236 0000010F E943FFFFFF                      jmp .GetPart
   237                                  ;GetPart END
   238                                  .EndArgError:
   239 00000114 48BE-                       mov rsi, arg_error
   240 00000116 [2E00000000000000] 
   241 0000011E BA28000000                  mov rdx, arg_error_len
   242 00000123 E87B010000                  call DisplayText
   243 00000128 4889EC                      mov rsp, rbp ; Equal to leave
   244 0000012B 4C89ED                      mov rbp, r13 ; Repair the block pointer
   245 0000012E B802000000                  mov rax, 2
   246 00000133 C3                          ret
   247                                  .EndError:
   248 00000134 48BE-                       mov rsi, spec_error
   249 00000136 [1000000000000000] 
   250 0000013E BA1E000000                  mov rdx, spec_error_len
   251 00000143 E85B010000                  call DisplayText
   252 00000148 4889EC                      mov rsp, rbp ; Equal to leave
   253 0000014B 4C89ED                      mov rbp, r13 ; Repair the block pointer
   254 0000014E B801000000                  mov rax, 1
   255 00000153 C3                          ret
   256                                  .End:
   257 00000154 4889EC                      mov rsp, rbp ; Equal to leave
   258 00000157 4C89ED                      mov rbp, r13 ; Repair the block pointer
   259 0000015A 4831C0                      xor rax, rax ; No error
   260 0000015D C3                          ret
   261                                  
   262                                  ;-----------------------------
   263                                  ;Info:     Prints the string from the given address
   264                                  ;In:       RSI (pointer to string)
   265                                  ;Out:      -
   266                                  ;Local:    RAX (store address here)
   267                                  ;-----------------------------
   268                                  PrintString:
   269 0000015E 4889F0                      mov  rax, rsi
   270 00000161 E809000000                  call StrLen
   271 00000166 4889C6                      mov  rsi, rax
   272 00000169 E835010000                  call DisplayText
   273                                      
   274 0000016E C3                          ret
   275                                  
   276                                  ;-----------------------------
   277                                  ;Info:     Counts number of characters in string (until 0 symbol)
   278                                  ;In:       RSI* (pointer to string)
   279                                  ;Out:      RDX (amount of characters)
   280                                  ;Local:    RBX (current symbol)
   281                                  ;-----------------------------
   282                                  StrLen:
   283 0000016F 4831D2                      xor rdx, rdx
   284                                      .Repeat:
   285 00000172 8A1E                            mov bl, [rsi]
   286 00000174 80FB00                          cmp bl, 0
   287 00000177 7408                            je  .End
   288 00000179 48FFC6                          inc rsi
   289 0000017C 48FFC2                          inc rdx
   290 0000017F EBF1                        jmp .Repeat
   291                                  .End:
   292 00000181 C3                          ret
   293                                  ;-----------------------------
   294                                  ;Info:     Gets number from RDI and writes it in RSI in hex form. Number is stored like: 0000'1''2''3''4'
   295                                  ;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
   296                                  ;Out:      RSI (address of last byte written [first numbers byte]), RDX (amount of symbols in number)
   297                                  ;Local:    RCX (last bits of the number), RAX (symbols)
   298                                  ;-----------------------------
   299                                  PutHex:
   300 00000182 4831D2                      xor  rdx, rdx
   301 00000185 4831C9                      xor  rcx, rcx
   302                                      
   303 00000188 4885FF                      test rdi, rdi
   304 0000018B 7905                        jns  .GetDigit
   305 0000018D B32D                        mov  bl, '-'
   306 0000018F 48F7DF                      neg  rdi
   307                                  .GetDigit:
   308 00000192 4088F9                      mov  cl, dil    ; [1b]->[1b]<-
   309 00000195 80E10F                      and  cl, 0xF    
   310 00000198 48C1EF04                    shr  rdi, 4   ; Get rid of last byte
   311                                      
   312 0000019C 48B8-                       mov  rax, sym
   313 0000019E [0000000000000000] 
   314 000001A6 4801C8                      add  rax, rcx
   315 000001A9 8A08                        mov  cl, [rax]    ; Load the digit
   316 000001AB 880E                        mov  [rsi], cl  ; Put the digit
   317 000001AD 48FFCE                      dec  rsi
   318 000001B0 48FFC2                      inc  rdx ; Chars amount is increased by 1
   319 000001B3 4883FF00                    cmp  rdi, 0  ; If dividend is 0, we must exit
   320 000001B7 75D9                    jne  .GetDigit
   321                                  
   322 000001B9 C60678                      mov  [rsi], byte 'x'
   323 000001BC 48FFC2                      inc  rdx
   324 000001BF 48FFCE                      dec  rsi
   325 000001C2 C60630                      mov  [rsi], byte '0'
   326 000001C5 48FFC2                      inc  rdx
   327 000001C8 80FB2D                      cmp bl, '-'
   328 000001CB 7508                        jne .End
   329                                  
   330                                  .WriteMinus:
   331 000001CD 48FFCE                      dec rsi
   332 000001D0 881E                        mov [rsi], bl ; WTF?! stosb is not working
   333 000001D2 48FFC2                      inc rdx
   334                                      
   335                                  .End:    
   336 000001D5 C3                          ret
   337                                  
   338                                  
   339                                  ;-----------------------------
   340                                  ;Info:     Gets number from RDI and writes it in RSI in octal form. Number is stored like: 0000'1''2''3''4'
   341                                  ;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
   342                                  ;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
   343                                  ;Local:    RCX (last bits of the number)
   344                                  ;-----------------------------
   345                                  PutOct:
   346 000001D6 4831D2                      xor  rdx, rdx
   347 000001D9 4831C9                      xor  rcx, rcx
   348                                      
   349 000001DC 4885FF                      test rdi, rdi
   350 000001DF 7905                        jns  .GetDigit
   351 000001E1 B32D                        mov  bl, '-'
   352 000001E3 48F7DF                      neg  rdi
   353                                  .GetDigit:
   354 000001E6 4088F9                      mov  cl, dil    ; [1b]->[1b]<-
   355 000001E9 80E107                      and  cl, 0x7    ; 
   356 000001EC 48C1EF03                    shr  rdi, 3   ; Get rid of last 3 bits
   357                                      
   358 000001F0 80C130                      add  cl, '0'
   359 000001F3 880E                        mov  [rsi], cl
   360 000001F5 48FFCE                      dec  rsi
   361 000001F8 48FFC2                      inc  rdx
   362                                  
   363 000001FB 4883FF00                    cmp  rdi, 0  ; If dividend is 0, we must exit
   364 000001FF 75E5                        jne  .GetDigit
   365                                      
   366 00000201 80FB2D                  cmp bl, '-'
   367 00000204 7508                    jne .End
   368                                  .WriteMinus:
   369 00000206 881E                        mov [rsi], bl ; WTF?! stosb is not working
   370 00000208 48FFCE                      dec rsi
   371 0000020B 48FFC2                      inc rdx
   372                                  .End:
   373 0000020E 48FFC6                      inc rsi
   374                                      
   375 00000211 C3                          ret
   376                                  
   377                                  ;-----------------------------
   378                                  ;Info:     Gets number from RDI and writes it in RSI in binary form. Number is stored like: 0000'1''2''3''4'
   379                                  ;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
   380                                  ;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
   381                                  ;Local:    RCX (last bits of the number), RAX (symbols)
   382                                  ;-----------------------------
   383                                  PutBin:
   384 00000212 4831D2                      xor  rdx, rdx
   385 00000215 4831C9                      xor  rcx, rcx
   386                                      
   387 00000218 4885FF                      test rdi, rdi
   388 0000021B 7905                        jns  .GetDigit
   389 0000021D B32D                        mov  bl, '-'
   390 0000021F 48F7DF                      neg  rdi
   391                                  .GetDigit:
   392 00000222 4088F9                      mov  cl, dil    ; [1b]->[1b]<-
   393 00000225 48D1EF                      shr  rdi, 1   ; Get rid of last byte
   394                                      
   395 00000228 80E101                      and  cl, 1
   396 0000022B B830000000                  mov  rax, '0'
   397 00000230 4801C8                      add  rax, rcx
   398 00000233 8806                        mov  [rsi], al
   399 00000235 48FFCE                      dec  rsi
   400 00000238 48FFC2                      inc  rdx
   401                                  
   402 0000023B 4883FF00                    cmp  rdi, 0  ; If dividend is 0, we must exit
   403 0000023F 75E1                        jne  .GetDigit
   404                                  
   405 00000241 80FB2D                  cmp bl, '-'
   406 00000244 7508                    jne .End
   407                                  .WriteMinus:
   408 00000246 881E                        mov [rsi], bl ; WTF?! stosb is not working
   409 00000248 48FFCE                      dec rsi
   410 0000024B 48FFC2                      inc rdx
   411                                  .End:
   412 0000024E 48FFC6                      inc rsi
   413                                      
   414 00000251 C3                          ret
   415                                  
   416                                  ;-----------------------------
   417                                  ;Info:     Gets number from RDI and writes it in RSI in decimal form. Number is stored like: 0000'1''2''3''4'
   418                                  ;In:       RDI* (number to convert), RSI* (pointer to the storage's _END_)
   419                                  ;Out:      RSI (address of last byte written), RDX (amount of symbols in number)
   420                                  ;Local:    RAX (arithmetics), RCX (arithmetics)
   421                                  ;-----------------------------
   422                                  PutDec:
   423 00000252 4889F8                      mov  rax, rdi ;dividend
   424 00000255 BF0A000000                  mov  rdi, 10
   425 0000025A 4831C9                      xor  rcx, rcx
   426                                      
   427 0000025D 4885C0                      test rax, rax ; test if negative (look up first bit)
   428 00000260 7905                        jns  .GetDigit
   429 00000262 B32D                        mov  bl, '-'
   430 00000264 48F7D8                      neg  rax
   431                                  
   432                                  .GetDigit:
   433 00000267 4831D2                      xor  rdx, rdx    ;remainder
   434 0000026A 48F7F7                      div  rdi   ;Dividing by qword value
   435 0000026D 4892                        xchg rax, rdx ;Our number is in rdx now. Reminder is in al
   436 0000026F 0430                        add  al, '0'    ; Get char number
   437 00000271 8806                        mov  [rsi], al ; WTF?! stosb is not working
   438 00000273 48FFCE                      dec  rsi
   439 00000276 48FFC1                      inc  rcx
   440 00000279 4889D0                      mov  rax, rdx ; Move dividend back to rax
   441 0000027C 4883F800                    cmp  rax, 0  ; If dividend is 0, we must exit
   442 00000280 75E5                        jne  .GetDigit
   443                                  
   444 00000282 80FB2D                  cmp bl, '-'
   445 00000285 7508                    jne .End
   446                                  .WriteMinus:
   447 00000287 881E                        mov [rsi], bl ; WTF?! stosb is not working
   448 00000289 48FFCE                      dec rsi
   449 0000028C 48FFC1                      inc rcx
   450                                  .End:
   451 0000028F 48FFC6                      inc rsi
   452 00000292 4889CA                      mov rdx, rcx
   453                                      
   454 00000295 C3                          ret
   455                                  ;-----------------------------
   456                                  ;Info:     Saves data from standard input to somewhere else
   457                                  ;In:       RSI (pointer to storage), RDX (storage capacity)
   458                                  ;Out:      -
   459                                  ;Local:    RAX (syscall code), RDI (file descriptor[input]) 
   460                                  ;-----------------------------
   461                                  SaveData:
   462 00000296 B800000000                  mov  rax, sys_read ;We are going to read data
   463 0000029B BF00000000                  mov  rdi, stdin    ;From stdin 
   464 000002A0 0F05                        syscall
   465 000002A2 C3                          ret
   466                                  
   467                                  ;-----------------------------
   468                                  ;Info:     Prints the text
   469                                  ;In:       RSI (pointer to message), RDX (message length)
   470                                  ;Out:      -
   471                                  ;Local:    RAX (syscall code), RDI (file descriptor) 
   472                                  ;-----------------------------
   473                                  DisplayText:
   474 000002A3 B801000000                  mov  rax, sys_write
   475 000002A8 BF01000000                  mov  rdi, stdout
   476 000002AD 0F05                        syscall
   477 000002AF C3                          ret
   478                                  ;-----------------------------
   479                                  ;Info:     Exits the program
   480                                  ;In:       -
   481                                  ;Out:      -
   482                                  ;Local:    RAX (syscall code), RDI (error code) 
   483                                  ;-----------------------------
   484                                  Exit:
   485 000002B0 B83C000000                  mov  rax, sys_exit
   486 000002B5 BF00000000                  mov  rdi, 0
   487 000002BA 0F05                        syscall
   488 000002BC C3                          ret
   489                                      ;Must return to the parent process here
   490                                  ;-----------------------------
   491                                  ;Info:     Exits the program with error
   492                                  ;In:       -
   493                                  ;Out:      -
   494                                  ;Local:    RAX (syscall code), RDI (error code) 
   495                                  ;-----------------------------
   496                                  ExitError:
   497 000002BD 48BE-                       mov  rsi, error; Write msg address to rdi
   498 000002BF [5600000000000000] 
   499 000002C7 BA1C000000                  mov  rdx, error_len; We'll need the number of symbols too
   500 000002CC E8D2FFFFFF                  call DisplayText
   501                                      
   502 000002D1 B83C000000                  mov  rax, sys_exit
   503 000002D6 BF01000000                  mov  rdi, 1
   504 000002DB 0F05                        syscall
   505 000002DD C3                          ret

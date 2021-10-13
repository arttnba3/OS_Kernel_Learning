%include "../../../boot.inc"

SELECTOR_VIDEO equ (0x0003 << 3) + TI_GDT + RPL_RING0

[bits 32]
section .text

;------------- [FUNCTION] put_char -------------
global put_char
put_char:
    pushad                                      ; save the environment
    mov ax, SELECTOR_VIDEO                      ; set the gs value
    mov gs, ax

;------------- Get the location of the cursor -------------
;; get the high 8 bit
    mov dx, 0x03d4                              ; addr of the input port
    mov al, 0x0e                                ; op code
    out dx, al                                  ; write to the port
    mov dx, 0x03d5                              ; addr of the output port
    in al, dx                                   ; read from the port
    mov ah, al

;; get the low 8 bit
    mov dx, 0x03d4                              ; addr of the input port
    mov al, 0x0f                                ; op code
    out dx, al                                  ; write to the port
    mov dx, 0x03d5                              ; addr of the output port
    in al, dx                                   ; read from the port

;; save the location in bx
    mov bx, ax

;------------- Print the char -------------
    mov ecx, [esp + 36]                         ; pushad for 8 regs => 8*4, ret addr for 4 bytes
    cmp cl, 0xd                                 ; \r
    jz .is_carrage_return
    cmp cl, 0xa                                 ; \n
    jz .is_line_feed

    cmp cl, 0x8                                 ; \b
    jz .is_backspace
    jmp .put_other

;; for backspace, a blankspace to replace it
.is_backspace:
    dec bx
    shl bx, 1                                   ; bx *= 2
    mov byte [gs:bx], 0x20
    inc bx
    mov byte [gs:bx], 0x7                       ; mode of display
    shr bx, 1                                   ; recover the bx
    jmp .set_cursor

;; \r, set cursor back to start of current line
.is_carrage_return:
    xor dx, dx
    mov ax, bx
    mov si, 80                                  ; a line in 80 *25 is 80 len
    div si                                      ; dx_ax / si => value of cursor / 80
    sub bx, dx                                  ; dx for the left, bx -dx to back to the start of a line

    jmp .set_cursor

;; usual char
.put_other:
    shl bx, 1
    mov byte [gs:bx], cl
    inc bx
    mov byte [gs:bx], 0x7
    shr bx, 1
    inc bx                                      ; cursor location += 2
    cmp bx, 2000                                ; if out of 80 * 25, scroll up a new line
    jl .set_cursor

;; \n, set cursor to a new line
.is_line_feed:
    xor dx, dx
    mov ax, bx
    mov si, 80                                  ; a line in 80 *25 is 80 len
    div si                                      ; dx_ax / si => value of cursor / 80
    sub bx, dx                                  ; dx for the left, bx -dx to back to the start of a line

    add bx, 80
    cmp bx, 2000
    jl .set_cursor

;; mov line 1~24 to 0~23
.roll_screen:
    cld
    mov ecx, 960                                ; 24 * 80 chr => *2 bytes totally => 4 bytes each mov
    mov esi, 0xc00b80a0                         ; addr of line 1
    mov edi, 0xc00b8000                         ; addr of line 0, our graphic mem start
    rep movsd

;; clear line 24
    mov ebx, 3840
    mov ecx, 80
.clear_line_24:
    mov byte [gs:ebx], 0x20                     ; ' '
    mov byte [gs:ebx + 1], 0x07
    add ebx, 2
    loop .clear_line_24
    mov bx, 1920                                ; start of line 24 (cursor location)

;; set the cursor
.set_cursor:
;;; high 8 bit
    mov dx, 0x03d4
    mov al, 0x0e
    out dx, al
    mov dx, 0x03d5
    mov al, bh
    out dx, al

;;; low 8 bit
    mov dx, 0x03d4
    mov al, 0x0f
    out dx, al
    mov dx, 0x03d5
    mov al, bl
    out dx, al

;------------- Return -------------
    popad                                       ; recover the regs
    ret

;; function to get value of (cursor back to start of current line)
.get_line_start_func:
    xor dx, dx
    mov ax, bx
    mov si, 80                                  ; a line in 80 *25 is 80 len
    div si                                      ; dx_ax / si => value of cursor / 80
    sub bx, dx                                  ; dx for the left, bx -dx to back to the start of a line
    ret

;------------- [FUNCTION] put_str -------------
global put_str
put_str:
    push ebx
    push ecx
    xor ecx, ecx
    mov ebx, [esp + 12]                         ; addr of str
.str_on:
    mov cl, [ebx]
    cmp cl, 0                                   ; judge whether it's \0 or not
    jz .str_over
    push ecx
    call put_char
    add esp, 4                                  ; cdecl clear the stack
    inc ebx                                     ; mov to next char
    jmp .str_on
.str_over:
    pop ecx
    pop ebx
    ret

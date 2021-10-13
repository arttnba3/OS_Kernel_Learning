; Basic loader
; Original from the book "Caozuoxitongzhenxianghuanyuan" by Zheng Gang, 2016
; Recode by arttnba3, 2021.4

%include "boot.inc"
section loader vstart=LOADER_BASE_ADDR
LOADER_STACK_TOP equ LOADER_BASE_ADDR
jmp loader_start


;------------- Global Descriptor Table -------------
;; the first one shall be NULL, not available
    GDT_BASE:           dd 0x00000000
                        dd 0x00000000

    CODE_DESC:          dd 0x0000FFFF               ; base 0x0000, limit 0xffff
                        dd MACRO_DESC_CODE

    DATA_STACK_DESC:    dd 0x0000FFFF
                        dd MACRO_DESC_DATA

    VIDEO_DESC:         dd 0x80000007               ; (0xbffff-0xb8000) / 4k = 0x7, for text mode(0xb8000~0xbffff)
                        dd MACRO_DESC_VIDEO

    GDT_SIZE equ $ - GDT_BASE
    GDT_LIMIT equ GDT_SIZE - 1
    times 60 dq 0                                   ; reserve for 60 descriptors

;------------- something for GDT and selectors -------------
    SELECTOR_CODE equ (0x0001 << 3) + TI_GDT + RPL_RING0  ; index, ti, rpl
    SELECTOR_DATA equ (0x0002 << 3) + TI_GDT + RPL_RING0
    SELECTOR_VIDEO equ (0x0003 << 3) + TI_GDT + RPL_RING0

    total_mem dd 0

    GDT_PTR     dw GDT_LIMIT
                dd GDT_BASE                         ; little endian

    ards_array times 244 db 0
    ards_num dw 0x0

    loader_msg  db 'arttnba3', 0x0

;------------- loader programme -------------
loader_start:
;------------- Get the info of the memory -------------
;; 0xe820 func of int 0x15
    xor ebx, ebx                                ; set the ebx to 0
    mov edx, 0x534d4150
    mov di, ards_array
.get_ards_loop:
    mov eax, 0x0000e820
    mov ecx, 20
    int 0x15
    jc .get_mem_e801                            ; cf=1:failed, try another way
    add edi, ecx
    inc word [ards_num]
    cmp ebx, 0
    jnz .get_ards_loop

;;;find the biggest ards for our os to use
    mov ecx, [ards_num]
    mov ebx, ards_array
    xor edx, edx                                ; to store the biggest one
.find_max:
    mov eax, [ebx]
    add eax, [ebx+8]                            ; base_addr_low + length_low
    add ebx, 20
    cmp edx, eax
    jge .next_ards
    mov edx, eax
.next_ards:
    loop .find_max

    mov [total_mem], edx
    jmp .protected_mode

.get_mem_e801:
    mov eax, 0x0000e801
    int 0x15
    jc .get_mem_88                              ; failed, try the 0x88 func

    mov ecx, 0x00000400
    mul ecx
    add eax, 0x100000
    mov esi, eax
    xor eax, eax
    mov eax, ebx
    mov ecx, 0x10000
    mul ecx
    add esi, eax
    mov [total_mem], esi
    jmp .protected_mode

.get_mem_88:
    mov ah, 0x88
    int 0x15
    jc .get_mem_failed
    and eax, 0x0000FFFF
    mov cx, 0x400
    mul cx
    shr edx, 16
    or edx, eax
    add edx, 0x100000
    mov dword [total_mem], edx
    jmp .protected_mode

.get_mem_failed:                                ; we finally failed, hang it on
    mov byte [gs:0x0+0],'F'
    mov byte [gs:0x0+2],'A'
    mov byte [gs:0x0+4],'I'
    mov byte [gs:0x0+6],'L'
    hlt

;;------------- Step into the protected mode -------------
.protected_mode:
;;;------------- print some words -------------
    mov byte [gs:0x160+0],'A'
    mov byte [gs:0x160+2],'3'
    mov byte [gs:0x160+4],'O'
    mov byte [gs:0x160+6],'S'
    mov sp, LOADER_BASE_ADDR
    mov bp, loader_msg                          ; es:bp for the addr of msg (es is 0 now) 
    mov cx, 8                                   ; length of loader_msg
    mov ax, 0x1301                              ; ah=0x13 for print, al for attribute(01:characters only, change the cursor(00 not change))
                                                ; other attributes in bl(00,01), or in msg(10,11)
    mov bx, 0x001f                              ; bh:number of page, bl:attribute(1f blue bg, pink char)
    mov dx, 0x1800                              ; (0,24)
    int 0x10

;;; open the A20, addr line expand to 24
    in al, 0x92
    or al, 0000_0010b
    out 0x92, al

;;; load the GDT into GDT register(48bit)
    lgdt [GDT_PTR]

;;; set the cr0 no.0 bit to 1
    mov eax, cr0
    or eax, 0x0000_0001
    mov cr0, eax
    jmp dword SELECTOR_CODE:p_mode_start        ; code selector is 0 now

[bits 32]
p_mode_start:
    mov ax, SELECTOR_DATA
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov esp, LOADER_STACK_TOP
    mov ax, SELECTOR_VIDEO
    mov gs, ax

    mov byte [gs:160], 'A'

    mov eax, KERNEL_START_SECTOR
    mov ebx, KERNEL_BIN_BASE_ADDR
    mov ecx, 400
    call read_disk

    call setup_page
    
    sgdt [GDT_PTR]                              ; save the original GDT value

    mov ebx, [GDT_PTR + 2]                      ; GDT_BASE
    or dword [ebx + 0x18 + 4], 0xc0000000       ; reset the video segment descriptor to virtual addr

    add dword [GDT_PTR + 2], 0xc0000000         ; pre-reset the GDT_BASE
    add esp, 0xc0000000                         ; reset the stack to the kernel space(virtual addr)

;; set the cr3 register
    mov eax, PAGE_DIR_TABLE_POS
    mov cr3, eax

;; turning on paging
    mov eax, cr0
    or eax, 0x80000000
    mov cr0, eax                                ; from now on, all addr are delt with the paging
                                                ; for example, 0xc0000000 -> Page DIrectory Table no.768 PDE
                                                ; no.768 PDE -> Page Table no.0 PTE -> exact page 

;; reset the GDT to virtual addr   
    lgdt [GDT_PTR]
    mov byte [gs:162], '3'
    jmp SELECTOR_CODE:enter_kernel
enter_kernel:
    call kernel_init
    mov esp, 0xc009f000

;; I DON'T KNOW WHY BUT IF WE DON'T WAIT FOR A WHILE THERE THE BOOT WILL FAILED
;; hope someone can explain this for me
;; arttnba3 2021.10.13
;; the problem DISAPPEAR AFTER I DELETE SOME OUTPUT SENTENCES, BUT I DON'T KNOW WHY
;; arttnba3 2021.10.13


    jmp KERNEL_ENTRY_POINT

;;------------- prepare the PDE and PTE -------------
;;; clear the space for the page directory table
setup_page:
    mov ecx, 4096
    mov esi, 0
.clear_page_dir:
    mov byte [PAGE_DIR_TABLE_POS + esi], 0 
    inc esi
    loop .clear_page_dir

;;; generate the PDE
.create_pde:
    mov eax, PAGE_DIR_TABLE_POS
    add eax, 0x1000                             ; the addr of the first Page Table
    mov ebx, eax

    or eax, PG_US_U | PG_RW_W | PG_P            ; user page, writable, present(exist)
    mov [PAGE_DIR_TABLE_POS + 0x0], eax         ; the first Page Table restored in the no.0 PDE and no.768 PDE
                                                ; the first PDE for 0~0x3fffff(default), including the 0 ~ 0xfffff(we're using it now)
    mov [PAGE_DIR_TABLE_POS + 0xc00], eax       ; at there we divided memory into two parts: user and kernel
                                                ; 0xc00 is the no.768 page directory entry, (virtual) higher are all for kernel
                                                ; our kernel will be mapping on the physical mem start from 0x100000
    sub eax, 0x1000
    mov [PAGE_DIR_TABLE_POS + 4092], eax        ; get the last PDE pointing to the Page Directory Table itself

;;; generate PTE for the lower 1M memory
    mov ecx, 256                                ; at the very beginning, it's okay for us to just initialize PTE for 1MB
                                                ; each page is 4096(4K), for 1M memory, there're 256 PTE available
                                                ; each pte only needs 4B in a page table, the first one page table is enough to use
    mov esi, 0
    mov edx, PG_US_U | PG_RW_W | PG_P           ; user page, writable, present(exist)
.create_pte:
    mov [ebx + esi * 4], edx
    add edx, 4096
    inc esi
    loop .create_pte

;;; create PDE for other page table in kernel
    mov eax, PAGE_DIR_TABLE_POS
    add eax, 0x2000                             ; the addr of the second page table
    or eax, PG_US_U | PG_RW_W | PG_P            ; user page, writable, present(exist)
    mov ebx, PAGE_DIR_TABLE_POS

    mov ecx, 254                                ; no.769 ~ 1022 PDE, 1023 pointing to itself
    mov esi, 769
.create_kernel_pte:
    mov [ebx + esi * 4], eax
    inc esi
    add eax, 0x1000
    loop .create_kernel_pte
    
    ret

;;------------- load the kernel -------------
kernel_init:
    xor eax, eax
    xor ebx, ebx                                ; addr of each Program Header Entry
    xor ecx, ecx                                ; nums of Program Header Entry
    xor edx, edx                                ; size of a Program Header Entry

    mov dx, [KERNEL_BIN_BASE_ADDR + 42]         ; e_phentsize, size of each entry in program Header Table
    mov ebx, [KERNEL_BIN_BASE_ADDR + 28]        ; e_phoff, offset of Program Header Table in the ELF file
    add ebx, KERNEL_BIN_BASE_ADDR
    mov cx, [KERNEL_BIN_BASE_ADDR + 44]         ; e_phnum, amount of entries in Program Header Table

.each_segment:
    cmp byte [ebx + 0], PT_NULL                          ; check the p_type
    je .PT_IS_NULL                             ; PT_NULL means not used
    ;cmp dword [ebx + 4], 0                      ; p_offset is 0, seems useless now?
    ;je .PT_IS_NULL

    push dword [ebx + 16]                       ; p_filesz, count
    
    mov eax, [ebx + 4]                          ; p_offset
    add eax, KERNEL_BIN_BASE_ADDR               ; src
    push eax

    push dword [ebx + 8]                        ; dst, p_vaddr
    call memcpy
    
    add esp, 12                                 ; clear the parameters

.PT_IS_NULL:
    add ebx, edx
    loop .each_segment
    ret

;;------------- basic memcpy -------------
;;; parameters passed by stack
memcpy:
    push ebp
    mov ebp, esp
    push ecx

    mov edi, [ebp + 8]                          ; dst
    mov esi, [ebp + 12]                         ; src
    mov ecx, [ebp + 16]                         ; count
    cld
    rep movsb

    pop ecx
    pop ebp
    ret

;;------------- Read data from the disk -------------
;;; data to disk shall be pass by ax/al, dx for the port
read_disk:
    mov esi, eax
    mov di, cx
    mov dx, 0x1f2                   ; amount of sectors to read
    mov al, cl
    out dx, al

    mov eax, esi                    ; Logical Block Address, which means that 1 for a block(usually 512B), not 1 B
                                    ; we use LBA28 there
    mov dx, 0x1f3                   ; LBA low
    out dx, al                      ; LBA 0~7 bit

    mov cl, 8
    shr eax, cl                     ; LBA 8~15 bit
    mov dx, 0x1f4                   ; LBA mid
    out dx, al

    shr eax, cl                     ; LBA 16~23 bit
    mov dx, 0x1f5                   ; LBA high
    out dx, al

    shr eax, cl
    and al, 0x0f                    ; LBA 24~27 bit
    or al, 0xe0                     ; 4(0) for main disk, 6(1) for LBA(0 for CHS), 5(1) and 7(1) are MBS bit
    mov dx, 0x1f6                   ; device
    out dx, al

    mov dx, 0x1f7                   ; command for the disk
    mov al, 0x20                    ; 0x20: to read from the disk, 0x30: to write to the fisk
    out dx, al

;; check whether the disk is ready
.not_ready:
    nop
    in al, dx
    and al, 0x88                    ; (bit) 0x80:busy, 0x08:ready
    cmp al, 0x8                     
    jnz .not_ready

;; calculate the total times of reading
    mov ax, di                      ; amount of sectors to read
    mov dx, 256                     ; we can read 2 B each time, 256 times for a sector
    mul dx
    mov cx, ax

; data from disk shall be get from dx, port shall be set forward
    mov dx, 0x1f0

.go_on_read:
    in ax, dx
    mov [ebx], ax
    add ebx, 2
    loop .go_on_read
    ret
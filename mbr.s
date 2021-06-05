; Basic MBR to read loader from disks
; Original from the book "Caozuoxitongzhenxianghuanyuan" by Zheng Gang, 2016
; Recode by arttnba3, 2021.4

%include "boot.inc"
SECTION MBR vstart=0x7c00
    mov ax, cx
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov ss, ax
    mov sp, 0x7f00                  ; stack frame pointer
    mov ax, 0xb800                   
    mov gs, ax                      ; 0xb800 for GPU to set the text mode, base on gs register

    mov ax, 0x0600                  ; ah=0x06, means to volumn lines; al for nums of lines, 0 for all
    mov bx, 0x0700                  ; bh for the attribute of lines
    mov cx, 0                       ; (0, 0)
    mov dx, 0x184f                  ; (80, 25):0~24, 0~79
    int 0x10                        ; clear the screen

    mov eax, LOADER_START_SECTOR	; address of the sector on the disk
    mov ebx, LOADER_BASE_ADDR		; destination address

    mov cx, 8                       ; amount of sectors to read
    call read_disk

    jmp LOADER_BASE_ADDR

; data to disk shall be pass by ax/al, dx for the port
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
    mov [bx], ax
    add bx, 2
    loop .go_on_read
    ret

    times 510 - ($ - $$) db 0       ; totally 512B, 0 for padding
    db 0x55, 0xaa                   ; the final 2 B shall always be this

; In this file we will set some useful macros
; Original from the book "Caozuoxitongzhenxianghuanyuan" by Zheng Gang, 2016
; Recode by arttnba3, 2021.4

;------------- load base -------------
LOADER_BASE_ADDR equ 0x900
LOADER_START_SECTOR equ 0x2
PAGE_DIR_TABLE_POS equ 0x100000
KERNEL_BIN_BASE_ADDR equ 0x70000
KERNEL_START_SECTOR equ 9
KERNEL_ENTRY_POINT equ 0xc0001500

;------------- attributes for GDT descriptor (high 32 bit) -------------
DESC_G_4K equ 1000_0000_00000000_00000000b      ; G flag, 1 for the 4k size of granularity of the sector boundaries, 0 for 1 B
DESC_D_32 equ 100_0000_00000000_00000000b       ; D/B flag, D(1) for the available addr to 32 bit, B(0) for the 16 bit
DESC_L_32  equ 00_0000_00000000_00000000b       ; L flag for 32 bit
DESC_L_64  equ 10_0000_00000000_00000000b       ; L flag for 64 bit
DESC_AVL equ 0_0000_00000000_00000000b          ; AVL flag, temporary useless

DESC_LIMIT_CODE equ 1111_00000000_00000000b     ; the 19~16 bit of the segment limit
                                                ; for a larger boundary of address, it shall always be 0xFFFFF
DESC_LIMIT_DATA equ 1111_00000000_00000000b     ; 
DESC_LIMIT_VIDEO equ 0000_00000000_00000000b    ; unknown, maybe for 16 bit?

DESC_P equ 1000_0000_00000000b                  ; P flag, 1 for the existance of the segment

DESC_DPL_RING_0 equ 00_0_0000_00000000b         ; DPL flag, ring 0
DESC_DPL_RING_1 equ 01_0_0000_00000000b         ; DPL flag, ring 1
DESC_DPL_RING_2 equ 10_0_0000_00000000b         ; DPL flag, ring 2
DESC_DPL_RING_3 equ 11_0_0000_00000000b         ; DPL flag, ring 3

DESC_S_CODE equ 1_0000_00000000b                ; s flag, 1 for data(including code) segment
DESC_S_DATA equ 1_0000_00000000b                ;
DESC_S_SYS equ 0_0000_00000000b                 ; s flag, 0 for system segment

DESC_TYPE_CODE_X equ 1000_00000000b             ; type flag for code segment, 
                                                ; eXecutable
DESC_TYPE_DATA_W equ 0010_00000000b             ; type flag for data segment, 
                                                ; Writable

;;------------- package macros for GDT descriptor (high 32 bit) -------------
MACRO_DESC_CODE equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L_32 + DESC_AVL + DESC_LIMIT_CODE + DESC_P + DESC_DPL_RING_0 + DESC_S_CODE + DESC_TYPE_CODE_X + 0x00

MACRO_DESC_DATA equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L_32 + DESC_AVL + DESC_LIMIT_DATA + DESC_P + DESC_DPL_RING_0 + DESC_S_DATA + DESC_TYPE_DATA_W + 0x00

MACRO_DESC_VIDEO equ (0x00 << 24) + DESC_G_4K + DESC_D_32 + DESC_L_32 + DESC_AVL + DESC_LIMIT_VIDEO + DESC_P + DESC_DPL_RING_0 + DESC_S_DATA + DESC_TYPE_DATA_W + 0x0b

;------------- attributes for segment selector -------------
RPL_RING0 equ 00b
RPL_RING1 equ 01b
RPL_RING2 equ 10b
RPL_RING3 equ 11b
TI_GDT equ 000b
TI_LDT equ 100b

;------------- attributes for page table -------------
PG_P equ 1b
PG_RW_R equ 00b
PG_RW_W equ 10b
PG_US_S equ 000b
PG_US_U equ 100b

;------------- attributes for ELF -------------
PT_NULL equ 0

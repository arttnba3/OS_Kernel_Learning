#!/bin/bash
./bochs/bin/bximage -mode=create -hd=60M -imgmode="flat" -q ./img.img
dd if=./mbr.bin \
        of=./img.img \
        bs=512 count=1 conv=notrunc
dd if=./loader.bin \
        of=./img.img \
        bs=512 count=8 seek=2 conv=notrunc
dd if=./kernel/kernel.bin \
        of=./img.img \
        bs=512 count=200 seek=9 conv=notrunc

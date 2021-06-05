int main (void)
{   
    while (1)
    {
        asm(
        "mov byte ptr [gs:170], 'W';"
        "mov byte ptr [gs:172], 'E';"
        "mov byte ptr [gs:174], 'L';"
        "mov byte ptr [gs:176], 'C';"
        "mov byte ptr [gs:178], 'O';"
        "mov byte ptr [gs:180], 'M';"
        "mov byte ptr [gs:182], 'E';"
        "mov byte ptr [gs:186], 'T';"
        "mov byte ptr [gs:188], 'O';"
        "mov byte ptr [gs:192], 'A';"
        "mov byte ptr [gs:194], '3';"
        "mov byte ptr [gs:196], 'O';"
        "mov byte ptr [gs:198], 'S';"
        "mov byte ptr [gs:200], '!';");
    }
}
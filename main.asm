[org 0x0100]

section .text

jmp start


clrscn:
mov ax, 0xA000     ; Set the segment for video memory (Mode 13h)
mov es, ax         ; ES points to video memory
xor di, di         ; DI = 0, start at the beginning of video memory
mov di, 47000
mov al, 0          ; Set the color to black (0, can change to other color)

mov cx, 17000  ; Number of pixels on screen (320 * 200 = 64000)
rep stosb          ; Fill the screen with the value in AL (black)
ret


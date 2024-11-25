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


clr2:
pusha 
mov ax, 0xA000     ; Set the segment for video memory (Mode 13h)
mov es, ax         ; ES points to video memory
xor di, di         ; DI = 0, start at the beginning of video memory

mov al, 0          ; Set the color to black (0, can change to other color)

mov cx, 47000   ; Number of pixels on screen (320 * 200 = 64000)
rep stosb      


popa

ret 


delay:
	pusha
	pushf

	mov cx, [speed]
	mydelay:
	mov bx, 8000     ;; increase this number if you want to add more delay, and decrease this number if you want to reduce delay.
	mydelay1:
	dec bx
	jnz mydelay1
	loop mydelay

	popf
	popa
ret	 



randG:
   push bp
   mov bp, sp
   pusha
   cmp word [rand], 0
   jne next

  MOV     AH, 00h   ; interrupt to get system timer in CX:DX 
  INT     1AH
  inc word [rand]
  mov     [randnum], dx
  jmp next1

  next:
  mov     ax, 25173          ; LCG Multiplier
  mul     word  [randnum]     ; DX:AX = LCG multiplier * seed
  add     ax, 13849          ; Add LCG increment value
  ; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
  mov     [randnum], ax          ; Update seed = return value

 next1:xor dx, dx
 mov ax, [randnum]
 mov cx, [bp+4]
 inc cx
 div cx
 
 mov [bp+6], dx
 popa
 pop bp
 ret 2




printchar:

push bp
mov bp,sp

pushA



mov ax, 256
mov bx, [bp+4]
mul bx
 
mov bx,sprite_data
add bx,ax  ; bx has starting address of alphabet

mov cx, [bp+8] ; starting x             
mov dx, [bp+6]  ; starting y

loop_rows:
    
    push cx            
       

loop_columns:
    
              
   mov al,[bx]
   inc bx   
    cmp al, 1          

    jne skip_pixel    

    mov ah, 0x0C        
    mov al, [fore]         ; White color
    int 0x10
    jmp next_pixel

skip_pixel:
;    mov ah, 0x0C  
 ;   mov al, [back]
  ; int 0x10

next_pixel:
    inc cx      
    mov si,[bp+8]       
    add si, 16
    cmp cx,si        
    jnz loop_columns     

    pop cx              
    add dx, 1          
   mov si,[bp+6]       
    add si, 16
    cmp dx,si            ; End of sprite?
    jne loop_rows        ; Continue with next row

end:

popA
pop bp

ret 6



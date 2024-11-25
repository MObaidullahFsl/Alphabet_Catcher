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





movchar:

pushA

mov cx,[totalchars]
mov di,0


sub sp,2
push 255
call randG
pop ax 

mov [fore],ax


mov word [back],0

mover:

sub sp,2
push 266
call randG
pop ax

add ax,38

mov [helper_x+di],ax


sub sp,2
push 30
call randG
pop bx

mov [helper_y+di],bx


push 25
call randG
pop dx



mov [helper_num+di],dx

push ax 
push bx
push dx 


call printchar

add di,2


loop mover

call delay


mov di,0

mov cx ,[totalchars]

call clr2


mov di,0

mov cx, [totalchars]

mov si,[lines]

mov dx,173


godown:

cmp word [epilepsy],1
jne normal


sub sp,2
push 255
call randG
pop ax 

mov [fore],ax

sub sp,2
push 255
call randG
pop ax 

mov [back],ax


normal: 

mov bx ,[helper_y+di]
add bx,si

add bx,16 
cmp bx,173

je skip

push word [helper_x+di]

push word bx

push word [helper_num+di]

call printchar


skip:

push word [helper_x+di]
mov word bx, [helper_x +di] 
add bx,16
push bx 

call collision

pop ax 

cmp ax,0 
je flagn



flagn:

cmp 


add di,2

loop godown

mov di,0
mov cx, [totalchars]

call delay


call clr2
call scoreboard



add si,[lines]

mov di, 0

dec dx
mov cx, [totalchars]
cmp dx,0



jne godown


popA


ret  



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



blank:
mov al,0
jmp flagj

clearprev:

push bp
mov bp,sp
pusha 



mov dx,[bp+4]
mov bx,dx
push dx
xor dx,dx

mov ax,320 
mul bx
add ax, [bp+6]

pop dx 

cmp dx, 180
jg flagf

mov si, ax 

mov cx, [bp+6]

mov ah, 0x0c

mov bx, cx
add bx, 16

mov di, dx
add di,16 

flage:

cmp word [bgflag], 0

je blank
mov al, [sprite_buffer+si]


call colorchecker
flagj:
int 10h 

inc si 
inc cx 

cmp cx, bx 
jl flage

add si,304
mov cx, [bp+6]
inc dx 
cmp dx,di
jl flage 


flagf:

popa
pop bp 



ret 2




file_error: 

jmp flagg
    read_err:
    jmp flagg

colorchecker:

 cmp al,0
    je col1 
cmp al,1
     je col2
cmp al,2
     je col3 
cmp al,3
     je col4 
cmp al,4
     je col5 
cmp al,5
     je col6 
cmp al,6
    je col7
     


col1:
mov al,1011b
jmp flagd
col2:
mov al,1110b
jmp flagd
col3:
mov al,15
jmp flagd
col4:
mov al,0110b
jmp flagd
col5:
mov al,0111b
jmp flagd
col6:
mov al,1000b
jmp flagd
col7:
mov al,1010b

flagd:



ret


background_maker:

pusha


 mov ah, 0x3D        ; DOS function to open file
    mov al, 0           ; Read-only mode
    mov dx, file_name ; Pointer to file name
    int 0x21            ; DOS interrupt
    jc file_error       ; Jump if error
    mov [file_handle], ax          ; File handle in BX



    ; Read the file
    mov ah, 0x3F        ; DOS function to read file

    mov dx, sprite_buffer ; Load the address of sprite_buffer into DX

    mov bx,[file_handle]
    mov cx, 56000    ; Number of bytes to read
    int 0x21            ; DOS interrupt
    jc read_err       ; Jump if error

    ; Close the file
    mov ah, 0x3E        ; DOS function to close file
    mov bx, [file_handle]    ; Load file handle
    int 0x21




mov si,0
mov dx, 0            ; Starting row (Y)
mov cx, 0            ; Starting column (X)


draw_sprite:
    mov al, [sprite_buffer+si]     ; Load pixel from sprite buffer 

call colorchecker
 

    mov ah, 0x0C     ; Put Pixel service
    int 3h
    int 0x10         ; Draw the pixel
    inc si           ; Move to next byte in buffer
    inc cx           ; Increment column
    cmp cx, 320      ; Check if end of row
    jl continue_draw
    inc dx           ; Move to next row
    xor cx, cx       ; Reset column to 0

continue_draw:
    cmp dx, 200      ; Check if end of sprite
    jl draw_sprite

mov word [bgflag],1

popa

ret


scorer:
pushA

cmp word [scorex],34
jge flagk


flagl:
push word [scorex]
push word [scorey]
push word 9
call printchar

add word [scorex], 17

popa
ret


flagk:
mov word [scorex],0
add word [scorey],1

push word [scorex]
push word [scorey]
push word 9
call printchar
popa
ret



updatedkb:
pusha
;read from keyboard
in al,0x60


;compares if the right arrow key is pressed or not
cmp al,0x4D
;cmp al,0x36
je righter

;compares if the left arrow key is pressed or not
cmp al,0x4B
;cmp al,0x2a
je lefter

jmp endofi ;if none have been pressed this will jump to the end of the interupt

righter: ;moves the basket to the right,clears the screen and prints it again
mov ax,[basketx+30*4]
add ax,12         ;makes adjustments so that we get the rightmost pixel
cmp ax,320    ;compares if the basket has gone out of bounds on the right
ja endofi
call moveright
call clrscn
call bosketer
jmp endofi

lefter:  ;moves the basket to the left,clears the screen and prints it again
mov ax,[basketx]
sub ax,10    ;makes adjustments so that we get the leftmost pixel
cmp ax,38  ;compares if the basket has gone out of bounds on the left
jb endofi
call moveleft
call clrscn
call bosketer


endofi:

mov al, 0x20
out 0x20, al 
popA
iret 


;moves the basket left by decrementing the value of x for the pixels in the string
moveleft:
push si

mov si,0

subbr:
sub word [basketx+si],1   ;add a greater number to move right faster
add si,4
cmp word [basketx+si],'$'
jne subbr

mov si,0

subbl:
sub word [blake+si],1    ;add a greater number to move right faster
add si,4
cmp word [blake+si],'$'
jne subbl

pop si
ret




;moves the basket right by incrementing the value of x for the pixels in the string
moveright:
push si

mov si,0

addbr:
add word [basketx+si],1   ;add a greater number to move right faster
add si,4
cmp word [basketx+si],'$'
jne addbr

mov si,0

addbl:
add word [blake+si],1    ;add a greater number to move right faster
add si,4
cmp word [blake+si],'$'
jne addbl


pop si
ret


blackenator:    ;adds the hollow part of the basket same as bosketer but with string and colour changed
push ax
push bx
push cx
push dx
push si


xor ax,ax
xor bx,bx
xor cx,cx
xor dx,dx
mov si,0

mov ah,0ch ;write a pixel
mov al,00000000b ; colour
mov bh,00h ; select page always zero


;llop work basically iterates through the string to get coords of pixels to change
lop2:
mov cx,[blake+si] ; y
mov dx,[blake+si+2]
int 10h

;compares if all the pixels have ended
add si,4
cmp word [blake+si],'$'
jne lop2



pop si
pop dx
pop cx
pop bx
pop ax
ret









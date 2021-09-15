Extrn Player1:BYTE
Extrn Player2:BYTE
Extrn MainMenuCheck:BYTE
Public OL2OF


;3-macros

fill MACRO playerx ,playerSize
    local fillo
	pusha
    fillo:                                                                      
    add playerx[BX],CX
    add BX,2
    inc cx
    cmp cx,playerSize         
    jnz fillo
	popa
endm fill

DrawPlayer MACRO playerx, playery, playerSize
    local back
	pusha
    mov SI,offset playerx
    mov DI,offset playery
    mov Bl,playerSize
    
    mov al,color ;Pixel color
    mov ah,0ch ;Draw Pixel Command
    back: 
    mov cx,[SI] ;Column
    mov dx,[DI] ;Row      
    int 10h
    add SI,2
    add DI,2
    dec bl
    jnz back
	popa
endm DrawPlayer

EGGFIRE MACRO y,pos , col
local loop1,loop2
  pusha
INC NoFired
;set color of currently shot egg
;in its position in array EGGCOLOR
mov di,offset EGGCOLOR
add di,NoFired
mov ah,col
mov [di],ah 
;set X pos of currently shot egg
;in its position in array EGGX 
mov di,offset EGGX
add di,NoFired
add di,NoFired
mov si,pos
mov [di],si
;set Y pos of currently shot egg
;in its position in array EGGX
mov di, offset EGGY
add di,NoFired
add di,NoFired
mov cx,y
mov [di],cx
mov dx,[di] ;put EGGY in dx to display
mov bx,EGGSIZE  ;set loop1 counter
;prepare for printing
mov di,offset EGGCOLOR
add di,NoFired
MOV AL, [di]   
MOV AH,0CH 
;;;;;;;
loop1: ;;Display an EGG
 mov di,offset EGGX
 add di,NoFired
 add di,NoFired
 mov cx,[di] ;put EGGX in cx to display                 
 mov si,EGGSIZE ;set loop2 counter   
      loop2:  ;;Display one row 	
 	INT 10h ;display one pixel
 	dec si
 	inc cx
 	cmp si,0
      jnz loop2
  inc dx  ;next row
  dec bx
  cmp bx,0
 jnz loop1
   popa
ENDM EGGFIRE 


EGGERASE MACRO y,pos , col
local loop6,loop7
  inc FirstFired ;one egg has already reached bottom
  pusha

mov bx,EGGSIZE  ;set loop6 counter
MOV AL, col ;set color   
MOV AH,0CH
add cx,5 
loop6: ;;Erase an Egg             
 mov si,EGGSIZE ;set loop2 counter
 sub cx,5   
      loop7:  ;;ERASE one row 	
 	INT 10h ;display one pixel
 	dec si
 	inc cx
 	cmp si,0
      jnz loop7
  inc dx  ;next row
  dec bx
  cmp bx,0
 jnz loop6
   popa
ENDM EGGERASE


.model small
.386
.stack 64

.data
EGGX dw 15 dup(-1)
EGGY dw 15 dup(-1)
EGGCOLOR db 15 dup (-1)
EGGSIZE equ 5
NoFired dw -1
FirstFired dw 0
CrashingEGG db ?

speed1 equ 3
speed2 equ 09ffh

color equ 0fh
player1Size equ 50
player2Size equ 50

player2y DW player1Size dup(150)
player2x DW player1Size dup(160)
player1y DW player1Size dup(10)
player1x DW player1Size dup(160)
 
checktest  db 0
MouseDelay dW 100
leftarrow  equ 75
rightarrow equ 77
leftclick  equ 1
rightclick equ 2
mainmenu equ 19h

Red equ 1Eh
Green equ 1Fh
Blue equ 20h
MaxCountRed db 5,'$'
MaxCountGreen db 5,'$'
MaxCountBlue db 5,'$'
CheckCountRed db 2,'$'
CheckCountGreen db 3,'$'
CheckCountBlue db 1,'$'
CrashedEggs db 0,'$'
PauseChecker db 1
Winner db 'IS THE WINNER !!!! $'
StatusBar db 'PRESS P TO PAUSE $'

;Phase#2:
PlayerPos db 3  ;Flag to know which player ,,1 for the upper ,, 2 for the lower
command db 0
Receiver db 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


.CODE

OL2OF PROC FAR
    ;(
		call Initialization
		;check users input
        CHECK: 
           ;(   
		        call delay
				call CheckMaster
				cmp MainMenuCheck,0
				jz MainMenuMark
                call CheckSlave
				cont:
                
                mov di,FirstFired
			    cmp di,NoFired
			    JG noeggsavailable
			    call EGGFREEFALL
			    noeggsavailable:				
                
           ;) 
        cmp MaxCountRed,0
        jnz	CHECK
        cmp MaxCountBlue,0
        jnz	CHECK
        cmp MaxCountGreen,0
		jnz	CHECK
		cmp CrashedEggs,15
		jnz CHECK
		
		
cmp CheckCountBlue,0
jnz player1_win
cmp CheckCountGreen,0
jnz player1_win
cmp CheckCountRed,0
jnz player1_win
call winnerplayer2
jmp term		
player1_win: call winnerplayer1          
term: call termii    

MainMenuMark:
ret		
		
OL2OF endp
;;;;;;;;;;;;;;;;;;
Initialization proc 

        mov ax,@data
        mov DS,ax
        
        mov dx,3fbh    ; Line Control Register
		mov al,10000000b  ;Set Divisor Latch Access Bit
		out dx,al    ;Out it
		;set the divisor:
		;first byte
		mov dx,3f8h
		mov al,0ch
		out dx,al 
		;second byte
		mov dx,3f9h
		mov al,00h
		out dx,al 
		
		;set the rest of the initialization
		mov dx,3fbh
		mov al,00011011b ;
		out dx,al
		
		
        ;grapics mode
        mov ah,0
        mov al,13h
        int 10h
		cmp PauseChecker,0
        jz drawplayers 
		;fill players
        mov CX,0
        mov BX,0
        fill player1x,player1size
        mov CX,0
        mov BX,0
        fill player2x,player2size
       drawplayers:     
        ;draw players
        DrawPlayer player1x, player1y, player1Size
        DrawPlayer player2x, player2y, player2Size
		
		call Score

ret
Initialization endp
;;;;;;;;;;;;;;;;;;
CheckKeyboard proc 
pusha
  
        mov ah,1
        int 16h
		
        jnz Set_Command
		jmp EXITKEY
		
		Set_Command:
		mov Command,ah;the scan code of the pressed key
        
        ; eatbuffer:
            ; mov ah,0
            ; int 16h

EXITKEY:			  
popa
ret
CheckKeyboard endp
;;;;;;;;;;;;;;;;;
CheckMouse proc 
pusha
	 
                
        mov ax,3
        int 33h
		mov cx,bx
		cmp bx,0
		jz EXITMOUSE	
		
		click:
            ;(   
                mov checktest,1
                
                cmp cx,leftclick
				jnz RCheck
                call MoveLeft
				jmp EXITMOUSE
                RCheck:
                cmp cx,rightclick 
                call MoveRight
                
                ;jmp checkkey (xxxxxxxxxxxxxxxxxxxxx)
            ;)			
EXITMOUSE:
popa
ret
CheckMOuse endp
;;;;;;;;;;;;;;;;
CheckMaster proc
pusha
	call CheckKeyboard
	
	;check keypressed:
	cmp Command,0;no keypressed
	jz EndMaster ;انا قلقان من الحتة دي
	
	;cmp MasterCommand,3Eh;F4 command
	;jz 
	
	call Execute
	call Send
	
EndMaster:
popa
ret
CheckMaster endp
;;;;;;;;;;;;;;;;
CheckSlave proc 
	pusha
	call Receive
	;jump لسه مش عارفينه
	;check
	call Execute
popa
ret
CheckSlave endp
;;;;;;;;;;;;;;;;;
Send proc
	;(
pusha	   
	CheckAgain:	
		;Check that Transmitter Holding Register is Empty
		mov dx , 3FDH  ; Line Status Register
		In al , dx    ;Read Line Status
		test al , 00100000b
		JZ  CheckAgain        ;Not empty 
		;jz RetrunSend
		
		;If empty put the VALUE in Transmit data register
		mov dx , 3F8H  ; Transmit data register
		mov  al,Command
		out dx , al 
		
		cmp al,ResetButton ;if user presses (esc)
		;je Finish    ;انظر هنا 
		
		;end the program for both users
		
	;)
RetrunSend:
popa
ret
Send endp
;;;;;;;;;;;;;;;
Receive proc
	;(
	pusha
		;Check that Data is Ready
		mov dx , 3FDH  ; Line Status Register
		in al , dx  
		test al , 1
		JZ RetrunReceived            ;Not Ready 
		
		 ;If Ready read the VALUE in Receive data register
		 mov dx , 03F8H     
		 in al , dx      
		 mov command , al ;slave command
		
		 ;if the user presses esc:
		 cmp al,ResetButton
		 ;je Finish  ; أنظر هنا ايضا
		 ;end the program for both users
		 
	;)
RetrunReceived:
popa
ret
Receive endp
;;;;;;;;;;;;
Execute proc 
pusha
	key:
            ;( 
               ; mov checktest,0;أمر ملوش لازمة
               
			   FirsArrowCheck:	
				
                cmp Command,leftarrow 
                jnz OtherArrowCheck
				call MoveLeft
				jmp 	EXITKEY_1
				
               OtherArrowCheck:
                cmp Command,rightarrow
				jnz Checkmain
                call MoveRight
				jmp EXITKEY_1			
				
			   Checkmain:
				cmp Command,Pausebutton
				jnz CheckRed
				mov PauseChecker,0    ; he pressed p
				mov MainMenuCheck,0   
                jmp eatbuffer
                
                
			   
				CheckRed:
				cmp PlayerPos, 1
			    jne FirsArrowCheck
			    
			    	
				cmp Command,Red
				jnz CheckGreen
				cmp MaxCountRed,0
				jz eatbuffer
				dec MaxCountRed
				CALL Score_Count
				mov cl,04h
				jmp Fire
				
				
				
			CheckGreen:
				cmp Command,Green
				jnz CheckBlue
				cmp MaxCountGreen,0
				jz eatbuffer
				dec MaxCountGreen
				CALL Score_Count
				mov cl,02h
				jmp Fire
				
				
				
			CheckBlue:
				cmp Command,Blue
				jnz eatbuffer
				cmp MaxCountBlue,0
				jz eatbuffer
				dec MaxCountBlue
				CALL Score_Count
				mov cl,01h
				
			
			Fire:
				mov bx,sprite_X
				add bx,20;before mid			
				EGGFIRE 41,bx,cl
			
				
				
			
	EXITKEY_1:			
				
				
			eatbuffer:
            mov ah,0
            int 16h
			

	mov Command,0
	
popa
ret
Execute endp


MoveLeft proc 
pusha

		cmp cx,1
        jz playerlm
		cmp cx,3
		jz exitleft
                
        ;keyboard player
        mov SI,offset player1x
        mov DI,offset player1y 
        ;set the speed
        mov bx,speed1
        ;eat the buffer
        mov ah,0
        int 16h  
         
        jmp startleft
        
        ;mouse player                
        playerlm:
        mov SI,offset player2x
        mov DI,offset player2y
        mov bx,1
                 
        startleft:
                 
        ;if End of the Windowleft donot move
        ;(
            mov cx,[SI]
            cmp cx,0
            CHECK1:;;;;;;;;;;;;;
            JE exitleft
         ;)
            
        ;black pixel
        ;(
            mov al,0 ;Pixel color
            mov ah,0ch ;Draw Pixel Command 
            mov cx,[SI+player1Size*2-2] ;Column
            mov dx,[DI] ;Row      
            int 10h
        ;)
            
        
        ;decreasing index of all x
        ;(
        push bx
            mov bx,SI
            mov cx,player1Size
            loop2: mov ax,[bx]
                   dec ax
                   mov [bx],ax
                   add bx,2
                   loop loop2 
        pop bx
        ;)                             
        
        ;white pixel
        ;(
            mov al,color ;Pixel color
            mov ah,0ch ;Draw Pixel Command 
            mov cx,[SI] ;Column
            mov dx,[DI] ;Row      
            int 10h
        ;)
        
        ;for the keyboard player speed    
        dec bx
        cmp bx,0 
        
        jnz startleft
                
            ;)   
exitleft:
popa
ret
MoveLeft endp
;;;;;;;;;;;;;;
MoveRight proc 
pusha

;(
     cmp cx,2
     jz playermr
	 cmp cx,3
	jz exitright
		
     
     ;keyboard player 
     mov SI,offset player1x
     mov DI,offset player1y
     ;set the speed
     mov bx,speed1
     ;eat the buffer
     mov ah,0
     int 16h
      
     jmp startright
     
     ;mouse player
     playermr:
     mov SI,offset player2x
     mov DI,offset player2y
     mov bx,1
     
     startright:
     
     ;if End of the Windowright donot move
     mov cx,[SI+player1Size*2-2]
     cmp cx,319
     JE exitright
     
     
     ;black pixel
     ;(
         mov al,0 ;Pixel color
         mov ah,0ch ;Draw Pixel Command 
         mov cx,[SI] ;Column
         mov dx,[DI] ;Row      
         int 10h
     ;)    
     
     ;increasing index of all x
     ;(
     push bx
         mov bx,SI
         mov cx,player1Size
         loop1: mov ax,[bx]
                inc ax
                mov [bx],ax
                add bx,2
                loop loop1
     pop bx
     ;)
             
     ;white pixel
     ;(
         mov al,color ;Pixel color
         mov ah,0ch ;Draw Pixel Command 
         mov cx,[SI+player1Size*2-2] ;Column
         mov dx,[DI] ;Row      
         int 10h
     ;)
     
     ;for keyboard player speed    
     dec bx
     cmp bx,0
     jnz startright
     
     
exitright:
popa
ret
MoveRight endp
;;;;;;;;;;;;;;
EGGFREEFALL PROC near
	 pusha
	mov bx,FirstFired
    
	 loop5: ; loop on all currently falling eggs
	mov si,EGGSIZE ;counter
	;set X pos of currently shot egg
    ;in its position in array EGGX
    mov di,offset EGGX
    add di,bx
	add di,bx
	mov cx,[di]
	;set Y pos of currently shot egg
    ;in its position in array EGGY 
	mov di,offset EGGY
    add di,bx
	add di,bx
	mov dx,[di]
	;set color of currently shot egg
    ;in its position in array EGGCOLOR
	mov di,offset EGGCOLOR
    add di,bx
	MOV Ax,[di]
	mov ah,0
	cmp dx,145 ;the row before receiver
    je checker ;if egg reached bottom erase it and
	           ;check if it touched the basket
	
	
	;prepare to display
	MOV AL, 0       ;PIXEL COLOR
    MOV AH,0CH
	loop3: ;Clear First Row
	INT 10h  ;clear one pixel
    inc cx
	dec si
    cmp si,0
    jnz loop3
	

	mov si ,EGGSIZE  ;counter
	;set X pos of currently shot egg
    ;in its position in array EGGX
	mov di,offset EGGX
    add di,bx
	add di,bx
	mov cx ,[di]
	add dx ,EGGSIZE
	;set color of currently shot egg
    ;in its position in array EGGCOLOR
	mov di,offset EGGCOLOR
    add di,bx
	MOV Ax,[di]
	mov ah,0ch
	loop4: ;Draw Shifted Row 
	INT 10h
    inc cx
	dec si
    cmp si,0
    jnz loop4
	;set Y pos of next egg
    ;by its position in array EGGY 
	mov di,offset EGGY
    add di,bx
	add di,bx
	mov si,[di]
	inc si
    mov [di],si
	go_back:
       inc bx
	   cmp bx,NoFired
	   jbe loop5
	 popa
	 ret
	checker:
    mov CrashingEGG,al
	pusha
	push cx
	push dx
	EGGERASE dx,cx,0 ;ERASE THAT EGG
	Inc CrashedEggs
	pop dx
	pop cx
	add dx,5 ;GET POSITION OF NEXT COLOUMN
	mov bh,0
	mov ah,0dh
	int 10h ;GET COLOR OF PIXEL AT THE bottom of left EDGE OF EGG
	cmp al,0fh
	je hit ;if its color is .... then it hits the basket
	add cx,4
	int 10h ;GET COLOR OF PIXEL AT THE bottom of right EDGE OF EGG
	cmp al,0fh
	je hit ;if its color is .... then it hits the basket
	popa
	jmp go_back ;contine looping on the rest of eggs
	hit:
	cmp CrashingEGG,4
	jnz check_green
    cmp CheckCountRed,0
    jz Player1_win0
	dec CheckCountRed
	CALL Score_Count
	jmp prepareGoBack
	check_green:
    cmp CrashingEGG,2
	jnz check_blue
    cmp CheckCountGreen,0
    jz Player1_win0
	dec CheckCountGreen
	CALL Score_Count
    jmp prepareGoBack
	check_blue:
	cmp CrashingEGG,1
	jnz prepareGoBack
    cmp CheckCountBlue,0
    jz Player1_win0
	dec CheckCountBlue
	CALL Score_Count
	jmp prepareGoBack
	Player1_win0:
	call winnerplayer1
	prepareGoBack:
	 popa
	 jmp go_back
	EGGFREEFALL ENDP

delay proc
 pusha
  mov cx, 0h      ;HIGH WORD.
  mov dx, 01BB8h ;LOW WORD.
  mov ah, 86h    ;WAIT.
  int 15h
 popa
  ret
delay endp

winnerplayer1 proc
call ClearScreen
mov ah,9
mov dx,offset Player1+2
int 21h
mov ah,2
mov dl,10
int 21h
mov dl,13
int 21h
mov ah,9
mov dx,offset Winner
int 21h
call termii
ret
winnerplayer1 endp

winnerplayer2 proc
call ClearScreen
mov ah,9
mov dx,offset Player2+2
int 21h
mov ah,2
mov dl,10
int 21h
mov dl,13
int 21h
mov ah,9
mov dx,offset Winner
int 21h
call termii
ret
winnerplayer2 endp


ClearScreen proc 
	;(
		pusha
		
		mov ah,0
		mov al,3
		
		
		int 10h
		
		
		popa
	;)
	ret
ClearScreen endp

termii proc 
  mov ah,4Ch
  int 21h
termii endp

Score Proc

	mov ah,2
	mov dx,1400h
	int 10h
	
	mov ah, 9
	mov dx, offset Player1+2
	int 21h

	mov ah,2
	mov dx,1418h
	int 10h
	
	mov ah, 9
	mov dx, offset Player2+2
	int 21h
	
	
	mov ah,2
	mov dx,1700h
	int 10h
	
	mov ah, 9
	mov dx, offset StatusBar
	int 21h
	
	
	
	
	MOV DX,158
	MOV CX,0
	MOV AH,0ch
	MOV AL,0fh
	
	line:
		
		INT 10h
		inc cx 
		cmp CX,320
		jne line


; FIRST PLAYER SCORE

	MOV DX,170
	MOV CX,0
	MOV AH,0ch
	MOV AL,04h	

	Red_EGG:

	INT 10h
	inc cx 
	cmp CX,5
	jne Red_EGG
	
	inc dx
	MOV CX,0
	cmp dx,175
	jne Red_EGG
	
	
	mov ah,2
	mov dx,1501h
	int 10h
	
	ADD MaxCountRed,30H
	mov ah, 9
	mov dx, offset MaxCountRed
	int 21h
	Sub MaxCountRed,30H
	
	MOV DX,170
	MOV CX,18
	MOV AH,0ch
	MOV AL,02h
	
	Green_EGG:
	
	INT 10h
	inc cx 
	cmp CX,23
	jne Green_EGG
	
	inc dx
	MOV CX,18
	cmp dx,175
	jne Green_EGG
	
	
	
	mov ah,2
	mov dx,1503h
	int 10h
	
	ADD MaxCountGreen,30H
	mov ah, 9
	mov dx, offset MaxCountGreen
	int 21h
	Sub MaxCountGreen,30H
	
	
	
	MOV DX,170
	MOV CX,32
	MOV AH,0ch
	MOV AL,01h
	
	Blue_EGG:
	
	INT 10h
	inc cx 
	cmp CX,37
	jne Blue_EGG
	
	inc dx
	MOV CX,32
	cmp dx,175
	jne Blue_EGG
	
	mov ah,2
	mov dx,1505h
	int 10h
	
	ADD MaxCountBlue,30H
	mov ah, 9
	mov dx, offset MaxCountBlue
	int 21h
	Sub MaxCountBlue,30H
	
	
	
; SECOND PLAYER SCORE

	MOV DX,170
	MOV CX,192
	MOV AH,0ch
	MOV AL,04h	

	Red_EGG2:

	INT 10h
	inc cx 
	cmp CX,197
	jne Red_EGG2
	
	inc dx
	MOV CX,192
	cmp dx,175
	jne Red_EGG2
	
	
	mov ah,2
	mov dx,1519h
	int 10h
	
	ADD CheckCountRed,30H
	mov ah, 9
	mov dx, offset CheckCountRed
	int 21h
	Sub CheckCountRed,30H
	
	MOV DX,170
	MOV CX,208
	MOV AH,0ch
	MOV AL,02h
	
	Green_EGG2:
	
	INT 10h
	inc cx 
	cmp CX,213
	jne Green_EGG2
	
	inc dx
	MOV CX,208
	cmp dx,175
	jne Green_EGG2
	
	
	
	mov ah,2
	mov dx,151Bh
	int 10h
	
	ADD CheckCountGreen,30H
	mov ah, 9
	mov dx, offset CheckCountGreen
	int 21h
	Sub CheckCountGreen,30H
	
	
	
	MOV DX,170
	MOV CX,224
	MOV AH,0ch
	MOV AL,01h
	
	Blue_EGG2:
	
	INT 10h
	inc cx 
	cmp CX,229
	jne Blue_EGG2
	
	inc dx
	MOV CX,224
	cmp dx,175
	jne Blue_EGG2
	
	mov ah,2
	mov dx,151Dh
	int 10h
	
	ADD CheckCountBlue,30H
	mov ah, 9
	mov dx, offset CheckCountBlue
	int 21h
	Sub CheckCountBlue,30H
	Ret

Score ENDP


Score_Count Proc
;First Player

;Red
	mov ah,2
	mov dx,1501h
	int 10h
	
	ADD MaxCountRed,30H
	mov ah, 9
	mov dx, offset MaxCountRed
	int 21h
	Sub MaxCountRed,30H
;GREEN
	mov ah,2
	mov dx,1503h
	int 10h
	
	ADD MaxCountGreen,30H
	mov ah, 9
	mov dx, offset MaxCountGreen
	int 21h
	Sub MaxCountGreen,30H
;BLUE
	mov ah,2
	mov dx,1505h
	int 10h
	
	ADD MaxCountBlue,30H
	mov ah, 9
	mov dx, offset MaxCountBlue
	int 21h
	Sub MaxCountBlue,30H
	
;SECOND PLAYER
;RED
	mov ah,2
	mov dx,1519h
	int 10h
	
	ADD CheckCountRed,30H
	mov ah, 9
	mov dx, offset CheckCountRed
	int 21h
	Sub CheckCountRed,30H
	
;GREEN	
	mov ah,2
	mov dx,151Bh
	int 10h
	
	ADD CheckCountGreen,30H
	mov ah, 9
	mov dx, offset CheckCountGreen
	int 21h
	Sub CheckCountGreen,30H
	
;BLUE
	mov ah,2
	mov dx,151Dh
	int 10h
	
	ADD CheckCountBlue,30H
	mov ah, 9
	mov dx, offset CheckCountBlue
	int 21h
	Sub CheckCountBlue,30H

RET



Score_Count ENDP



END 
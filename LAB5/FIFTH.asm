.model tiny

.data 
cmd_length db ?
cmd_line db 127 dup('$')
count_of_cmd_word db 3

;file                       
;file_name db 'file.txt', 0 
file_name db 50 dup('$')   
file_id dw 0000h 
file_size dw 0000h


new_file_name db "newfile.txt", 0    
new_file_id dw 0000h 

msgFileError db 10,13,"No such file in directory$",10,13,'$'
msgFileOpened db 10,13,"File is opened!$",10,13,'$'  
msgFileClosed db 10,13,"File is closed!$",10,13,'$'
msgCMD_Error db 10,13,"There are less arguments in command line...",10,13,'$'
msgWordFind db 10,13,"Word is found!$",10,13,'$'
msgGettingCMD db 10,13,"Parsing CMD line...$",10,13,'$'
msgErrorMoving db 10,13,"Error moving pointer...$",10,13,'$'
space db 10,13,'$' 

;countOfReadBites dw 96h     ;150 bytes 
countOfReadBites dw 33h  

maxWordSize equ 50
cmdBufSize db 52 dup(0)
cmdBUFFER db 52 dup(0)

startPosBuf dw 0000h
endPosBuf dw 0000h
file_buffer db 202 dup ('$')
bufSize dw 0000h 
new_bufSize dw 0000h
count_bufSize dw 0000h

delem db " .,!",09H,0Dh,0Ah

chgWord db 50 dup ('$') 
wordSize dw 0
newWord db 50 dup ('$')                       
new_wordSize dw 0 
;chgWord db "robot"
;wordSize dw 5
;newWord db "1111"                       
;new_wordSize dw 4

processed_bytes_l dw 0000h 
processed_bytes_h dw 0000h 
new_processed_bytes_l dw 0000h 
new_processed_bytes_h dw 0000h 
read_bytes dw 0000h 

max_CMD_lenght dw 126
argc db 00h

.code
print_str macro out_str 
    pusha
    mov ah,09h
    mov dx,offset out_str
    int 21h 
    popa
endm 

get_word macro inp_str
    pusha
    mov ah, 0Ah
    lea dx, inp_str
    int 21h
    popa
endm

check_fseek macro proc_bytes_l,proc_bytes_h
    pusha
    mov ax,proc_bytes_h
    cmp ax,max_processed_bytes
    jb @@end_check_fseek
    add proc_bytes_l
    mov ax,max_processed_bytes
    sub proc_bytes_h,ax 
@@end_check_fseek:    
    popa
    ret 
endm    


start:
;
    mov ax,@data
    mov es,ax
    
    xor cx,cx
    mov cl,ds:[80h]
    mov cmd_length,cl
    mov si,82h
    lea di,cmd_line
    rep movsb           ;get cmd line
  
    call cmd_parse
    
;    print_str cmd_line
;    print_str space
;    print_str file_name
;    print_str space
;    print_str chgWord
;    print_str space
;    print_str newWord

     
;    xor cx,cx
;    mov cl,chgWord+1
;    mov wordSize,cx
;    print_str space
;    get_word newWord
;    xor cx,cx
;    mov cl,newWord+1
;    mov new_wordSize,cx 
    
    call open_file 
    mov file_id, ax             ;get file id from ax  
    call get_size_of_file
    call create_new_file
next_part:
    
    cmp file_size,0000h
    je close_and_exit    
    call read_file       
    call make_normal_file_buffer
    print_str file_buffer  
    
    call check_buf
     
    print_str file_buffer
     
    call write_into_new_file
    
    mov ax,read_bytes
    sub file_size,ax
    
    
    cmp file_size,0000h
    jne next_part
     
close_and_exit: 
    call close_file 
    
    mov bx, new_file_id ;
    xor ax, ax      ; 
    mov ah, 3Eh     ; close file
    int 21h
exit:    
    mov ah,4Ch
    mov al,00h
    int 21h 
;////////////////////////////////////////////////////////////
;===================PROC===================================== 

cmd_parse proc
    pusha
    cld

    xor cx,cx
    xor ax,ax
    mov cl,cmd_length
    
    
    lea si,cmd_line
    
    lea di,file_name
    call get_cmd_word 
    
    
    lea di,chgWord
    call get_cmd_word
    cmp chgWord,'$'
    je error_cmd
    push si
    lea si,chgWord
    call get_cmd_wordSize
    mov wordSize,ax
    pop si 
    
    lea di,newWord
    call get_cmd_word
    cmp newWord,'$'
    je error_cmd 
    lea si,newWord 
    call get_cmd_wordSize
    mov new_wordSize,ax
    popa
    ret
error_cmd:
    print_str msgCMD_Error
    jmp exit     
            
    
cmd_parse endp 

get_cmd_word proc
    push ax
    push cx
    push di 
    xor ax,ax
loop_getting:
    mov al,[si]
    
    cmp al,' '
    je end_getting
    cmp al,09h
    je end_getting
    cmp al,0Ah
    je end_getting
    cmp al,0Dh
    je end_getting
    cmp al,00h
    je end_getting
    
    mov [di],al
    
    inc si
    inc di
    
    loop loop_getting
end_getting:
    mov [di],0
    inc si
            
    pop di
    pop cx
    pop ax
    ret
get_cmd_word endp

get_cmd_wordSize proc                     
	push bx                     
	push si                   
	                            
	xor ax, ax                  
                                
    startCalc:                  
	    mov bl, [si]         
	    cmp bl, 0            
	    je endCalc             
                               
	    inc si                  
	    inc ax                                                                      
	    jmp startCalc           
	                            
    endCalc:                   
	pop si                      
	pop bx                     
	ret                         
get_cmd_wordSize endp                          
    
open_file proc
    push dx
    mov dx,offset file_name 
    mov ax, 3D02h  ;3D - open file, 02 - for reading and recording  
    int 21h       
    jc error_opening
    jmp is_opened
error_opening:
    print_str msgFileError
    pop dx
    jmp exit
is_opened:
        
    print_str msgFileOpened  

    pop dx
    ret
open_file endp   

 
read_file proc 
    push bx
    push cx
    push dx
    
    call move_pointer_by_processed_bytes
    
    mov bx, file_id              
    mov ah, 3Fh                                             
    mov cx,countOfReadBites         
    mov dx, offset file_buffer    
    int 21h   
    
    mov read_bytes, ax
    dec ax
    mov bufSize,ax
    mov new_bufSize,ax
    mov count_bufSize,ax 
    

     
;=============CHECK if > 65536=============    
    mov ax,countOfReadBites  
    clc
    add processed_bytes_h,ax 
    jc add_l_bytes
    jmp end_reading_file 
    
add_l_bytes:
    inc processed_bytes_l

end_reading_file:    
    
    pop dx
    pop cx
    pop bx
    ret
read_file endp

move_pointer_by_processed_bytes proc 
    pusha
      
    mov bx, file_id
    mov al, 00h                    
    xor cx, cx                        
    mov dx, processed_bytes_h         
    mov cx, processed_bytes_l        
    mov ah, 42h 
   
    int 21h
    
    jc er_
    jmp normal_
    
er_:
    print_str msgErrorMoving 
    jmp exit

normal_:    
     
    popa    
    ret
move_pointer_by_processed_bytes endp

close_file proc
    pusha
    mov bx, file_id 
    xor ax, ax       
    mov ah, 3Eh    
    int 21h          
    print_str msgFileClosed
    popa
    ret
close_file endp  

make_normal_file_buffer proc
    pusha
    lea si,file_buffer
    add si,countOfReadBites
    dec si
    loop_normilize:
    cmp [si],'$' 
    je its_normal
    cmp [si],00h 
    je its_normal
    cmp [si],' '
    je its_normal
    mov [si],'$'
    dec si
    dec processed_bytes_h
    dec read_bytes
    jmp loop_normilize 
its_normal:    
    popa
    ret
make_normal_file_buffer endp    

get_size_of_file proc
    pusha    
    xor cx,cx
    xor dx,dx
    mov ah,42h
    mov al,02h
    mov bx,file_id
    int 21h 
    mov file_size,ax
    popa 
    ret
get_size_of_file endp 
   

check_buf proc
    pusha
    lea si,file_buffer

    loop1:

        mov startPosBuf,si
        ;lea di,chgWord+2 
        lea di,chgWord
        mov cx,wordSize  
        REPE cmpsb
        cmp cx,0000h
        je find 
 
        sub cx,wordSize
        not cx
        inc cx
        sub new_bufSize,cx 
        sub count_bufSize,cx
        ;sub file_size,cx 
        cmp file_size,0000h
        je ch_buf_endp 

        lea di,delem
        loop2:             
            cmpsb 
            je loop1
            dec si
            cmp [si],'$'
            je ch_buf_endp
            cmp [si],0
            je ch_buf_endp
            cmp [di],0Ah
            je loop1
            jne loop2
    find:
    print_str msgWordFind
    ;mov ax,wordSize 
    ;sub file_size,ax
    
    call get_buf_lenght
    
    call change_word 
    cmp file_size,0000h
    je ch_buf_endp
    jne loop1
ch_buf_endp:
    dec si
    cmp [si],00h
    je end_ 
    jne all_is_clear 
end_:    
    mov file_size,0000h
    mov cx,wordSize
    sub cx,new_wordSize 
    sub si,cx
    dec si
all_is_clear:   
    mov [si],00h
    inc si
    loop all_is_clear              
    popa
    ret
check_buf endp

get_output_bufSize proc
    pusha
    lea si,file_buffer
    mov bufSize,000h
    loop_get_out_size:
    inc si
    inc bufSize
    cmp [si],00h
     
    jne check_$ 
    jmp end_check_counting
check_$:    
    cmp [si],'$'
    jne loop_get_out_size
    mov [si],00h
    jmp end_check_counting
end_check_counting:    
    popa
    ret
get_output_bufSize endp    
        
change_word proc
    ;lea di,newWord+2
    mov si,startPosBuf
    mov di,si
    mov cx,new_wordSize    
check_prob:
    cmp [si],' '
    je word_is_changed 
    cmp [si],'!'
    je word_is_changed
    cmp [si],09H
    je word_is_changed
    get_pos_to_ch:
    cmp [si],0Dh            ;enter
    je enter_in_buf
    inc si
    dec count_bufSize  
    cmp [si],00h
    je is_checked
    cmp [si],'!'
    je is_checked
    cmp [si],09H 
    je is_checked
    cmp [si],','
    je is_checked
    cmp [si],' '
    jne get_pos_to_ch 
    
is_checked:      
    mov ax, wordSize
    cmp new_wordSize,ax
    ja add_to_right
    cld
    jmp add_to_left
add_to_right: 
    mov count_bufSize,0000h
    loop_count:
    inc si
    inc count_bufSize
    cmp [si],00
    jne loop_count
    mov cx,count_bufSize
    inc cx
    mov di,si
    add di,new_wordSize
    sub di,wordSize
    
    std  
    jmp xchg_word_to_pos
add_to_left:
    cmp [si],00h 
    je xchg_word_to_pos 
    push si
    mov count_bufSize,0000h
    loop_count_bufS:
    inc si
    inc count_bufSize
    cmp [si],00
    jne loop_count_bufS
    mov cx,count_bufSize
    pop si
    add di,new_wordSize
xchg_word_to_pos:    
    repe movsb
    mov ax,bufSize
    add ax,new_wordSize
    sub ax,wordSize
    mov bufSize,ax
    ;/////////////////////////
    cld
    ;lea si,newWord+2
    lea si,newWord
    mov di,startPosBuf
    mov cx,new_wordSize
    repe movsb
    xchg si,di     
    ;/////////////////////////
    jmp word_is_changed
to_right:
        
enter_in_buf:
    ;add si,2
    ;sub count_bufSize,2
    inc si
    dec count_bufSize
    jmp get_pos_to_ch    
end_of_buf:
    movsb
    
word_is_changed:        
    ret
change_word endp  

create_new_file proc
    pusha
        mov ah, 3Ch
        mov cx,0000h 
        lea dx, new_file_name
        int 21h
        mov new_file_id, ax  
              
    popa
    ret
create_new_file endp 

open_new_file proc
    pusha
    mov dx,offset new_file_name 
    mov ax, 3D02h  ;3D - open file, 02 - for reading and recording  
    int 21h 
    popa
    ret
open_new_file endp 

get_buf_lenght proc
    pusha
    mov ax,new_bufSize
    sub ax,wordSize
    add ax,new_wordSize
    mov new_bufSize,ax
    popa
    ret
get_buf_lenght endp 

get_end_pos_buf proc
    pusha
    mov ax,startPosBuf
    add ax,bufSize
    mov endPosBuf,ax
    popa
    ret
get_end_pos_buf endp    

write_into_new_file proc
    pusha 
    
    cmp new_processed_bytes_h,0000h ;start of file
    je write
    ;sub new_processed_bytes_h,2  
    ;dec new_processed_bytes_h
write:    
    mov bx, file_id
    mov al, 00h                     ;start position in file
    xor cx, cx                      ;the beginning of file
    
    mov dx, new_processed_bytes_h         ; - 
    mov cx, new_processed_bytes_l         ; - amount of bytes
    mov ah, 42h 
   
    int 21h
    mov ah,40h
    mov bx,new_file_id 
    call get_output_bufSize
    mov cx,bufSize
    inc cx
    lea dx,file_buffer
    int 21h
    call init_buffer_to_start_pos
     
;=============CHECK if > 65536=============    
    clc
    add new_processed_bytes_h,cx 

    jc add_to_l_bytes
    jmp end_writing
add_to_l_bytes:
    inc new_processed_bytes_l
    mov new_processed_bytes_h,0000h     
    
end_writing:    
    popa
    ret
write_into_new_file endp

init_buffer_to_start_pos proc
    pusha
    lea si,file_buffer
    mov cx,202
    init_$_buf:
    mov [si],'$'
    inc si
    loop init_$_buf
    mov bufSize,0000h 
    popa
    ret
init_buffer_to_start_pos endp    


end start 


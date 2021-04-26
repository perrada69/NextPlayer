
              DEVICE ZXSPECTRUMNEXT
			  ;OPT reset --zxnext --syntax=abfw              
              slot 3
			  
              org #6000
CHARS         equ  15616-256              
mystak        equ  24575         ;ar bi trary value picked to be be low
                                ;BFE0h and above 4000h
staksto       equ  24575         ;some where to put BA SIC's stack
                                ;pointer
bankm         equ  #5B5C         ;sys tem vari able that holds the
                                ;last value out put to 7FFDh
port1         equ  #7FFD         ;ad dress of ROM/RAM switch ing port
                                ;in I/O map
catbuff       equ  #8000         ;some where for DOS to put its catalog
dos_catalog   equ  #011E         ;the DOS routine to call
filename 	  ds   15
typ 		  equ $-4
pocetpolozek	equ 231
virtmem			defb 0
maxlen			equ 128
maxline 		equ 37
save_allfiles	defw 0		

ReadNextReg:
    ; reads nextreg in A into A (does modify currently selected NextReg on I/O port)
    push    bc
    ld      bc,#243B
    out     (c),a
    inc     b       ; bc = TBBLUE_REGISTER_ACCESS_P_253B
    in      a,(c)   ; read desired NextReg state
    pop     bc
    ret
		
INPUT 	ei
		ld (INPOS+1),hl ;ulož adresu začátku pro další použití
		ld hl,23296 ;do HL adresa editační oblasti
		ld b,hx ;do B délka editační oblasti
IN1 	ld (hl),32 ;a nyní celou editační
		inc hl ;zónu vyplníme mezerami
		djnz IN1 ;na konec editační zóny
		ld (hl),b ;přijde 0
		res 5,(iy+1) ;signál není stisknuta klávesa
		xor a ;nastav kurzor
		ld (CURSOR+1),a ;na začátek editační zóny
IN2 	ld b,hx ;nyní celou editační zónu
INPOS 	ld hl,0 ;vytiskneme, nastav
		ld (PPOS+1),hl ;tiskovou pozici
		ld hl,23296 ;začínáme od začátku
CURSOR 	ld c,0 ;do C polohu kurzoru
IN3 	ld a,l ;testuj spodní byte adresy
		cp c ;v případe rovnosti
		ld a,">" ;dej do A kód kurzoru
		call z,CHAR ;a vytiskni ho
		ld a,(hl) ;vytiskni znak
		call CHAR ;z editační zóny
		inc hl ;a posun se pro další
		djnz IN3 ;opakuj se všemi znaky
		ld a,l ;kurzor také může
		cp c ;být až za posledním
		ld a,"<"+128 ;znakem, pak bude
		call z,CHAR ;na řádku vypadat jinak
		call INKEY ;přečti si kód klávesy
		cp 7 ;testuj EDIT (Caps Shift + 1)
		ret z ;a případně se vrať zpátky
		cp 13 ;testuj ENTER a případné odskoč
		ret z  ;jp z,INPCLEAR ;na smazání řádku z obrazovky
		
		ld hl,IN2 ;na zásobník ulož adresu IN2, sem
		push hl ;se bude nyní program vracet
		ld hl,CURSOR+1 ;do HL vlož adresu pozice kurzoru
		cp 8 ;testuj kurzor doleva
		jr z,CURSLEFT ;odskoč
		cp 9 ;kurzor doprava
		jr z,CURSRGHT
		cp 12 ;delete (správně BACKSPACE)
		jr z,BCKSPACE
		cp 199 ;znak <= (funkce DELETE)
		jr z,DELETE 
		cp 32 ;nyní zbývají
		ret c ;obyčejné znaky,
		cp 128 ;odfiltruj
		ret nc ;netisknutelné znaky
		ex af,af' ;a kód přesuň do A'
		ld a,(hl) ;testuj, zda není kurzor
		cp hx ;na konci řádku,
		ret nc ;když ano, tak se vrať
		inc (hl) ;posuň kurzor doprava
		ld l,(hl) ;do HL vlož adresu,
		dec l ;na kterou bude znak
		ld h,23296/256 ;uložen
INS 	ld a,(hl) ;přečti původní znak
		or a ;a testuj konec řádku
		ret z ;případně se vrať
		ex af,af' ;přehoď původní a nový znak
		ld (hl),a ;a nový zapiš, pro další znak
		inc hl ;bude novým znakem předchozí
		jr INS ;znak, opakuj posun znaku až do konce
CURSLEFT ld a,(hl) ;přečti polohu kurzoru
		or a ;a v případe, že je na levém okraji
		ret z ;tak se vrať a nic nedělej
		dec (hl) ;posuň kurzor doleva a vrať se na IN2
		ret
CURSRGHT ld a,(hl) ;přečti polohu kurzoru
		cp hx ;a když je na konci řádku
		ret nc ;tak se vrať
		inc (hl) ;jinak posuň kurzor doprava a vrať se
		ret ;vrať se na IN2
DELETE 	ld a,(hl) ;na konci
		cp hx ;editační zóny
		ret z ;DELETE nepracuje
		inc a ;jinak uprav polohu
		jr BCK2 ;a pokračuj společnou částí
BCKSPACE ld a,(hl) ;BACKSPACE naopak
		or a ;nepracuje na začátku
		ret z ;editační zóny
		dec (hl) ;posuň kurzor vlevo
BCK2 	ld l,a ;společná část,
		ld h,23296/256 ;která zajišťuje
		ld e,l ;přesunutí následujících
		ld d,h ;znaků na uvolněné místo
		dec e ;po smazaném znaku 

DEL2 	ld a,(hl) ;vlastní přesun
		ldi ;je prováděn instrukcí
		or a ;LDI dokud není přenesena 0,
		jr nz,DEL2 ;která signalizuje konec zóny
		ex de,hl ;na poslední pozici,
		dec hl ;která se nyní uvolnila,
		ld (hl)," " ;je zapsána mezera
		ret ;návrat na IN2
delka_souboru	defw 0
INKEY 	halt 
		xor  a				           
         ld   (aLAST_KEY+1),a		 
		ei
		 halt

ahl0		 
		 call KEYSCAN			       

		 ld   a,e
         inc  a
         jr   z,INKEY
         ld   a,d
         ld   hl,SYMTAB
         cp   $18
         jr   z,aHLSM2
         ld   hl,CAPSTAB
         cp   $27
         jr   z,aHLSM2
         ld   hl,NORMTAB
aHLSM2    ld   d,0
         add  hl,de
         ld   a,(hl)
         or   a
         jr   z,INKEY
         
aLAST_KEY ld   b,0
         cp   b
         jr   z,aSEDI_KEY
         
         ld   b,3
aLOOP_LST halt 
         djnz aLOOP_LST
aSEDI_KEY 
         ld   (aLAST_KEY+1),a
		push af
		call beepk
		pop af
		ret 		
		
INPCLEAR ld de,(INPOS+1) ;do DE pixelová pozice
		ld c,8 ;16 pixelových řádků na výšku
INPC2 	ld b,hx ;do B délka řádku
		inc b ;plus 1 za kurzor
		xor a ;vynuluj A
		push de ;ulož adresu začátku řádku
INPC3 	ld (de),a ;vymaž byty
		inc de ;v pixelovém
		djnz INPC3 ;řádku
		pop de ;obnov adresu počátku

		call DOWNDE ;a posuň se na další řádek
		dec c ;odečti jedničku
		jr nz,INPC2 ;a zbývají-li řádky, opakuj
		ret 

CHAR 	exx
		add a,a 
		ld l,a 
		sbc a,a 
		ld c,a 
		ld h,15 
		add hl,hl 
		add hl,hl 
PPOS 	ld de,16384
		push de
		ld b,8
CHAR2 	ld a,(hl) 
		;rrca 
		or (hl) 
		xor c 
		ld (de),a 
		call DOWNDE 
		inc hl 
		djnz CHAR2
		pop de 
		inc e 
		ld a,e 
		and 31 
		jr nz,CHAR3 
		dec e 
		ld a,e ; posun na další tiskovou pozici
		and %11100000 
		ld e,a 
		ld b,16 
CHAR4 	call DOWNDE 
		djnz CHAR4 
CHAR3 	ld (PPOS+1),de 
		exx
		ret 

; DE .... vstup hledaneho retezce
; HL .... vstup textu,kde budeme hledat
; Výstup:
		; Z ... nalezeno
		; C .. nenalezeno
search		ld (search0+1),de	
search1		
			
			ld a,(de)
			cp "a"
			jr c,search_next
			cp "z"
			jr nc,search_next
			or a
			sbc a,32
			ld (de),a
search_next
			ld a,(hl)
			cp "a"
			jr c,search_next2
			cp "z"
			jr nc,search_next2
			or a
			sbc a,32
			ld (hl),a
search_next2			
			
			ld a,(de)
			xor 32
			ret z		;konec, nasli jsme retezec

			ld a,(hl)
			or a
			jr z,konec_textu
					
			ld a,(de)
			; res 5,a
			; res 5,(hl)
			xor (hl)
			jr z,search_jo
search0		ld de,0						
			inc hl
			jr search1
search_jo	inc hl
			inc de
			jr search1

konec_textu ld a,1		;nastav nz - nic jsme nenasli
			or a
			ret

SF
searchfile
			ld (ssearch0+1),de
			ld a,(de)
			or a
			ret z		;konec, nasli jsme retezec

			ld a,(hl)
			or a
			jr z,skonec_textu
					
			ld a,(de)
			xor (hl)
			jr z,ssearch_jo
ssearch0		ld de,0						
			inc hl
			jr searchfile
ssearch_jo	inc hl
			inc de
			jr searchfile
			

skonec_textu ld a,1		;nastav nz - nic jsme nenasli
			or a
			ret









; Porovnava dva reteze o delce 3 byty.
; Vstup:
			; HL ... adresa prvniho retezce
			; DE ... adresa druheho retezce
; Vystup: 
			; Z ... souhlasí
			; NZ .. nesouhlasí
compare
			ld a,(de)
			res 7,(hl)
			xor (hl)
			ret nz
			inc hl
			inc de
			ld a,(de)
			res 7,(hl)
			xor (hl)
			ret nz
			inc hl
			inc de
			ld a,(de)
			res 7,(hl)
			xor (hl)
			ret

CountMemory
			ld de,13*(pocetpolozek-1)
Count11		ld hl,catbuff
			add hl,de
			ld (Count11+1),hl
	
			ret

savehl		defw 0
saveix		defw 0		
ESXDOS      MACRO service? : push hl : pop ix : rst $08 : db service? : ENDM    ; copies HL into IX

; Read NextReg into A (does modify A, and NextReg selected on the I/O port)
; is not optimized for speed + restores BC
    MACRO NEXTREG2A register?
        ld     a,register?
        call   ReadNextReg
    ENDM

txtWait		defb	"Please wait:",0
txtShrek	defb	"Main program: Shrek/MB Maniax",0
txtBorik	defb 	"AY play rutines: mborik/SinDiKat",0
txtThanks	defb	"Big thanks: ped7g, z00m, Logout",0
txtVersion	defb 	"0.6",0
demo:       di      

F_OPEN                      equ $9A
FA_READ                     equ $01
F_READ                      equ $9D
F_CLOSE                     equ $9B
M_GETERR                    equ $93 
next_register_select equ $243b
nxr_peripheral2 equ $06


			ld hl,22528
            ld de,22529
			ld a,7
            ld (hl),a
            ld bc,767
            ldir
			ld hl,screen
			ld de,16384
			ld bc,2048
			ldir
			ld de,18464+2
			ld hl,txtWait
			call print
			ld hl, 18496+2
			ld   bc,4*256+28
			ld   a,%00000101
			call RAMECEK

			ld de,18528+4
			ld hl,txtStart
			call print

			ld de,20544+3
			ld hl,txtShrek
			call print
			ld de,20576+3
			ld hl,txtBorik
			call print
			ld de,20608+3
			ld hl,txtThanks
			call print

			ld de,16480+28
			ld hl,txtVersion
			call print
			
			ld hl,0
			ld de,18528+24
			ld (NUMPOS+1),de
			call DEC16
			defw 20*256

			ld hl,22528+13 + 32*18
			ld b,15
podbarvi	ld (hl),%01000111
			inc hl
			djnz podbarvi
			
			ld hl,22528+15 + 32*19
			ld b,15
podbarvi1	ld (hl),%01000111
			inc hl
			djnz podbarvi1
			
			ld hl,22528+11 + 32*20
			ld b,15
podbarvi2	ld (hl),%01000111
			inc hl
			djnz podbarvi2
			
			ld hl,22528
			ld b,32*4
podbarvi3	ld (hl),%01000111
			inc hl
			djnz podbarvi3
			
			ld bc,$123b
            ;nextreg #03,1
			nextreg #07,3             ;28MHz - jen kvůli načítání (rychlost)
			
			call BUFF83
			
            ld (staksto),sp   ;save BASIC's stack pointer
            ld bc,port1       ;the horizontal ROM switch/RAM
                                ;switch I/O ad dress
            ld a,(bankm)      ;sys tem vari able that holds cur rent
                                ;switch state
            res 4,a           ;move right to left in hor i zon tal
                                ;ROM switch (3 to 2)
                                
              or 7              ;switch in RAM page 7
              ld (bankm),a      ;must keep sys tem vari able up to
              	                  ;date (very im por tant)
              out (c),a         ;make the switch

			  ld sp,mystak      ;make sure stack is above 4000h and
                                ;be low BFE0h
              ei                ;in ter rupts can now be en abled

              ld hl,catbuff     ;some where for DOS to put the cata
                                ;log
              ld de,catbuff+1   ;
              ld bc,1024        ;max i mum (for +3DOS) is ac tu ally
                                ;64x13+13 = 845
              ld (hl),0
              ldir              ;make sure at least first en try is
              ld de,catbuff     ;the lo ca tion to be filled with the
NextDirItem
              ld b,pocetpolozek ;the num ber of en tries in the
                                ;buffer
              ld c,%100            ;include sys tem files in the cata
              ld hl,stardstar   ;the file name ("*.*")
              call dos_catalog  ;call the DOS en try
			  
              ld (savehl),hl
			  ld (saveix),ix
			  ld a,b
			  cp pocetpolozek
			  push af
			  
			  ld hl,(ALLFILES)
			  ld e,b
			  ld d,0
			  add hl,de
			  dec hl
			  ld (ALLFILES),hl

			push hl
			push de
			push bc
			
			
			ld hl,(dirNum)
			add	hl,de
			dec hl
			ld 	(dirNum),hl
			ld de,18528+24
			ld (NUMPOS+1),de
			halt
			call DEC16
			defw 20*256
			pop bc
			pop de
			pop hl

			  
			  ld a,(FILES)
			  add a,b
			  dec a
			  ld (FILES),a
			  pop af
			  jr c,cont
nezapisuj			  
			  ld a,b
			  or a
			  jr z,cont

			  ld a,(virtmem)
			  cp 4
			  jr z,cont

			  call CountMemory
			  ex de,hl
			  ld hl,virtmem
			  inc (hl)
			  jr NextDirItem
cont			  
              push af           ;save flags and pos si ble er ror num
                                ;ber re turned by DOS
				
			
			ld hl,(dirNum)
			ld de,18528+24
			ld (NUMPOS+1),de
			halt
			call DEC16
			defw 20*256


              pop hl
              ld (dosret),hl    ;put it where it can be seen from
                                ; NextBASIC
              ld c,b            ;move num ber of files in cat a log to
                                ;low byte of BC
              ld b,0            ;this will be re turned in NextBASIC
                  
              di                ;about to ROM/RAM switch so be
                                ;care ful
              push bc           ;save num ber of files
              ld bc,port1       ;I/O ad dress of hor i zon tal ROM/RAM
                                ;switch
              ld a,(bankm)      ;get cur rent switch state
              
              
              set 4,a           ;move left to right (ROM 2 to ROM
                                ;3)
              and #F8           ;also want RAM page 0
              ld (bankm),a      ;up date the sys tem vari able (very
                                ;im por tant)
              out (c),a         ;make the switch
              pop bc            ;get back the saved num ber of files
              dec bc
              
              ld sp,(staksto)   ;put NextBASIC's stack back
			
			 call BUFF83
			 ld hl,catbuff+13
			 ld de,(ALLFILES)
testovani	 push de
			 push hl
			 pop ix
			 push ix
			 bit 7,(ix+7)

			 jr nz,souhlasi

			 ld de,8
			 add hl,de		;v HL je začátek přípony souboru

			 push hl
			 ld de,PT2
			 call compare
			 pop hl
			 jr z,souhlasi

			 push hl
			 ld de,PT3
			 call compare
			 pop hl
			 jr z,souhlasi

			 ld de,SQT
			 push hl
			 call compare
			 pop hl
			 jr z,souhlasi

			 ld de,STC
			 push hl
			 call compare
			 pop hl
			 jr z,souhlasi

 			 ld de,STP
			 push hl
			 call compare
			 pop hl
			 jr z,souhlasi
			 
			 pop de
			 push de		;v de mame zacatek jmena souboru
			 ld h,d
			 ld l,e
			 add hl,13		;vynechana jedna polozka
			 ld bc,1260*13
			 ldir
			 
			 ld hl,(ALLFILES)
			 dec hl
			 ld (ALLFILES),hl
			 pop hl
			 pop de
			 dec de
			 ld a,d
			 or e
			 jr nz,testovani
			 jr skon
souhlasi			 
			 pop hl
			 ld de,13
			 add hl,de
			 pop de
			 dec de
			 ld a,d
			 or e
			 jr nz,testovani
skon
			
			ld hl,(ALLFILES)
			ld (save_allfiles),hl
			 ld de,18560+4
			 ld hl,txtLFNinfo
			 call print
			 call getAllLFN	
			 call GETDIR
			 ld hl,16384
			 ld de,16385
			 ld bc,6143
			 xor a
			 ld (hl),a
			 ldir
			
			 call NOBUFF83
             call kresli
             call INIROLL

			 ld hl,catbuff+#d              
             call WRFILES      ;vypis soubory
			 ld de,18464+15
			 ld hl,txtcurrentfile
			 call print
			
			 ld de,18432+15
			 ld hl,txtallfiles
			 call print

			call LFNWINDOW
			call LFNPRINT
			ld hl,LFNNAME + maxline
			ld (hl),0
			ld hl,LFNNAME
			ld de,20672+2
			call print

			
			ld de,20544+15
			ld hl,lfndir
			call print
			ld de,20544+15+3
			ld hl,NAZEVADRESARE
			call print
			
            nextreg #07,0             ;3,5MHz        
			
			nextreg $57,24		;Nastrankuj od C000 player
HLSMYC0  
		ld hl,(apos_all)
		inc hl
		ld de,18464+15+11
		ld (NUMPOS+1),de
		call DEC16
		defw 20*256
		
		ld hl,(ALLFILES)
		ld de,18432+15+11
		ld (NUMPOS+1),de
		call DEC16
		defw 20*256
										
         xor  a				           
         ld   (LAST_KEY+1),a		 
		 ei
		 halt
		 
		 ld a,(endsong)
		 or a
		 jr z,hl0
		 ld (LAST_KEY+1),a
		 jr SEDI_KEY
hl0		 
		 call KEYSCAN			       

		 ld   a,e
         inc  a
         jr   z,HLSMYC0
         ld   a,d
         ld   hl,SYMTAB
         cp   $18
         jr   z,HLSM2
         ld   hl,CAPSTAB
         cp   $27
         jr   z,HLSM2
         ld   hl,NORMTAB
HLSM2    ld   d,0
         add  hl,de
         ld   a,(hl)
         or   a
         jr   z,HLSMYC0
         
LAST_KEY ld   b,0
         cp   b
         jr   z,SEDI_KEY
         
         ld   b,1
LOOP_LST halt 
         djnz LOOP_LST
         
SEDI_KEY 
         ld   (LAST_KEY+1),a
				
				
	     ld (KEY),a
			  
		call beepk
		call CH_FILE
		jr   HLSMYC0

         ei
konec    jr konec     


         
         ret               
                                

NUM      ds 11				
DEC16    ld   c,' '
DEC16X   ld   iy,NUM
         ld   de,-10000
         call BN1
         ld   de,-1000
         call BN1
DEC8_3   ld   de,-100
         call BN1
         ld   e,-10
         call BN1
         ld   e,-1
         ld   c,'0'
         call BN1
DEC32SP  ld   (iy+0),0
         ex   (sp),hl
         ld   e,(hl)
         inc  hl
         ld   d,(hl)
         inc  hl
         ex   (sp),hl
         ex   de,hl
         ld   hl,NUM
NUMPOS	 ld   de,18562+15
         jp   print
         
BN1      ld   a,'0'-1
BN4      add  hl,de
         inc  a
         jr   c,BN4
BN2      sbc  hl,de
         cp   '0'
         jr   z,BN3
         db 1,'0'
BN3      ld   a,c
         or   a
         ret  z
         ld   (iy+0),a
         inc  iy
         ret  

dirNum	 defw 0

LFNWINDOW
		ld hl, 20640
		ld   bc,3*256+32
        ld   a,%00000111
		call RAMECEK
		ret

setspace
		ld (hl),32
		ret



;Nastránkuj buffer s daty adresáře
BUFF83	
			nextreg $54,20
			nextreg $55,21
			nextreg $56,22
			nextreg $57,23
			ret

NOBUFF83	
			nextreg $54,04
			nextreg $55,05
			nextreg $56,00
			nextreg $57,01
			ret


; Input: HL = Dividend, C = Divisor, A = 0
; Output: HL = Quotient, A = Remainder
deleno		xor a
			ld b,16
de			
			add	hl,hl	
			rla			
			cp	c		
			jr	c,de1	
			sub	c		
			inc	l		
de1			djnz de			
			ret

addrlfn		dw 0

FINDLFN
			or a
			ld de,63
			sbc hl,de
			add hl,de
			jr c,prvni
			ld c,63
			xor a
			call deleno
		    jr oddeleno
prvni		ld a,l
			ld l,0
			
oddeleno	push af
			ld a,24
			add a,l
			nextreg $57,a
			pop bc
			ld de,maxlen
			ld a,b
			or a
			ld hl,$e000		
			jr z,prvnizaznam
lll			
			add hl,de
			djnz lll
			
prvnizaznam 
			ld (addrlfn),hl
			ld de,LFNNAME
			ld b,maxlen
popop			
			ld a,(hl)
			cp 255
			jr z,kon
			ld (de),a
			inc hl
			inc de
			djnz popop
kon			
			ld hl,LFNNAME
			ld b,40
F22222
			ld a,(hl)
			cp 255
			call z,setspace	
			inc hl
			djnz F22222
lfnend		ld hl,LFNNAME
lfn_1		ld hl,28 + 20672+2
			ld (maxpos),hl
lfn_2		ld hl,63 + 20672+2
			ld (maxpos2),hl
		
lfnat		ld de,20672+2
			ld hl,LFNNAME
			ret

LFNPRINT 	
			ld hl,LFNNAME
			ld de,LFNNAME + 1
			ld a,32
			ld (hl),a
			ld bc,164
			ldir
stop
			ld hl,(apos_all)

			call FINDLFN


			;call p6b
			nextreg $57,01
			ret

by1			defb  %00000000
			defb  %01111110
			defb  %01101010
			defb  %01010110
			defb  %01101010
			defb  %01010110
			defb  %01111110
			defb  %00000000
			
			defb  %00000000
			defb  %01111110
			defb  %01101010
			defb  %01010110
			defb  %01101010
			defb  %01010110
			defb  %01111110
			defb  %00000000
	
			defb  %00000000
			defb  %01111110
			defb  %01101010
			defb  %01010110
			defb  %01101010
			defb  %01010110
			defb  %01111110
			defb  %00000000

pozadi		  push de
			  ld b,8
kr1			  push bc
			  push de
			  ld bc,16
			  ld hl,byte16
			  ldir
			  pop hl
			  call downhl
			  ex de,hl
			  
			  pop bc
			  djnz  kr1
			  
			  pop de
			  ld b,8
kr2			  push bc
			  push de
			  ld bc,16
			  ld hl,abyte16
			  lddr
			  pop hl
			  call downhl
			  ex de,hl
			  
			  pop bc
			  djnz  kr2
			  ret



byte16	defb	%11111111
byte15	defb	%11111110
byte14	defb	%11111100
byte13	defb	%11111000
byte12	defb	%11110000
byte11	defb	%11100001
byte10	defb	%11000011
byte9	defb	%10000111
byte8	defb	%00011111
byte7	defb	%00001111
byte6	defb	%00000111
byte5	defb	%00000111
byte4	defb	%00000011
byte3	defb	%00000001
byte2	defb	%00000001
byte1	defb	%00000000

abyte1	defb	%00000000
abyte2	defb	%00000001
abyte3	defb	%00000001
abyte4	defb	%00000011
abyte5	defb	%00000111
abyte6	defb	%00000111
abyte7	defb	%00001111
abyte8	defb	%00011111
abyte9	defb	%10000111
abyte10	defb	%11000011
abyte11	defb	%11100001
abyte12	defb	%11110000
abyte13	defb	%11111000
abyte14	defb	%11111100
abyte15	defb	%11111110
abyte16	defb	%11111111
outputdir	defb 0
stardstar:
              defb "*.???",#FF  
dosret:
              defw 0          

KEY      defb 0
CH_FILE
		 
         ld   a,(KEY)
         cp   "q"
         jp   z,UP
         cp   11
         jp   z,UP
		 cp "s"
		 jp z,hledej
		 cp "r"
		 jp z,reload
         cp   "a"
         jp   z,DOWN
         cp   10
         jp   z,DOWN
		 cp 9
		 jp z,right
		 cp 8
		 jp z,left
         cp   13
         jp   z,FIRE
		 cp 32
		 jp z,nextsong
         ret
txtFilter defb "Search:           ",0		 
savepoc defw 0
actadr	defw 0		 
h1		defw 0
txtFind defb "Search songs.",0
txtFind2 defb "Only chars, without spaces.",0
txtFinded defb "Found:",0
txtLeft defb "Left: ",0
hledej	call savescr
		ld hl,0
		ld (nalezeno+1),hl
		ld hl,0
		ld (pozice+1),hl
		ld hl,(ALLFILES)
		ld (savepoc),hl
		ld hl,catbuff+#d
		ld (actadr),hl
		
		ld   hl,18464
        ld   bc,8*256+32
        ld   a,%00000111
        call RAMECEK		
		
		ld de,18496+1
		ld hl,txtFind
		call print

		ld de,18528+1
		ld hl,txtFind2
		call print

		ld hl,0
		ld (pozice+1),hl
		ld (h1),hl
		
		ld hl,13*32+22528+1
		ld b,29
po		ld (hl),01101000b
		inc hl
		djnz po
		
		ld hx,28 ;délka vstupu
		ld hl,18592+1 ;adresa na obrazovce
		call INPUT ;volej vstupní podprogram
		cp 7 ;testuj EDIT
		jp z,end_hledej
		ld hl,23296
		ld de,txtFilter + 8
		ld bc,10
		ldir
		ld de,20576+15
		ld hl,txtFilter
		call print

		
		ld de,18624+1
		ld hl,txtFinded
		call print

		ld de,18656+1
		ld hl,txtLeft
		call print

ssss
			nextreg #07,3             ;28MHz - jen kvůli načítání (rychlost)
			call BUFF83
			ld hl,catbuff+13; + 26 ;vynechej první dvě položky 
			
			ld de,(ALLFILES)
htestovani	push de
			push hl
			
			push hl
			push de

nalezeno	ld hl,0			
			ld de,18624+5	;pozice ve VideoRam
			ld (NUMPOS+1),de

			call DEC16		;vypiš číso HL - jaký soubor právě spracováváme
			defw 20*256

			ld hl,(h1)
			inc hl
			ld (h1),hl
			
			ld hl,(ALLFILES)
			ld de,18656+5	;pozice ve VideoRam
			ld (NUMPOS+1),de
			call DEC16		;vypiš číso HL - jaký soubor právě spracováváme
			defw 20*256
			

			ld hl,LFNNAME
			ld de,LFNNAME+1
			xor  a
			ld (hl),a
			ld bc,128
			ldir
			pop de
			pop hl
pozice		ld hl,0
			push hl
			call FINDLFN
			pop hl
			inc hl
			ld (pozice+1),hl
			ld de,23296
			ld hl,LFNNAME
			call search
			jr z,hsouhlasi
			; pop de
			; push de		;v de mame zacatek jmena souboru

			; ld h,d
			; ld l,e
			; add hl,13		;vynechana jedna polozka
			
			; ld hl,(actadr)
			; ex de,hl
			
			; ld bc,13	;1260*13
			; ldir
			 
			ld hl,(ALLFILES)
			dec hl				
			ld (ALLFILES),hl
			pop hl
			ld de,13
			add hl,de
			pop de
			dec de
			ld a,d
			or e
			jr nz,htestovani
			jr hkon
hsouhlasi	ld hl,(nalezeno+1)
			inc hl
			ld (nalezeno+1),hl
			
			ld hl,(actadr)
			ld de,vymenafile
			ld bc,13
			ldir				;ulož soubor, co bude vymazany
			
			ld hl,(actadr)				
			pop de
			push de
			ex de,hl
			ld bc,13
			ldir			;nahrad
			
			pop de
			push de
			ld hl,vymenafile
			ld bc,13
			ldir
			
			ld hl,(actadr)
			ld de,13
			add hl,de
			ld (actadr),hl
			
			 pop hl
			 ld de,13
			 add hl,de
			 pop de
			 dec de
			 ld a,d
			 or e
			 jp nz,htestovani
hkon		 
			 call GETDIR
			 ld hl,(ALLFILES)

			 ld a,h
			 or l
			 jp z,nula
			 ld hl,18624+5
			 ld (lfnpos+1),hl
			 call getAllLFN
			 ld hl,18560+24
			 ld (lfnpos+1),hl
			 
			 ld hl,0
			 ld (apos_all),hl
			 xor a
			 ld (POS_F + 1),a
			 
			
			 ld hl,catbuff+#d              
		     ld (WRBUFF+1),hl
			 call kresli


			;call loadscr
		    ld   hl,16416
		    ld   bc,20*256+14
		    ld   a,%00001111
		    call RAMECEK
			 call BUFF83
			call WRFILES
			ld de,18464+15
			ld hl,txtcurrentfile
			call print
			
			ld de,18432+15
			ld hl,txtallfiles
			call print
			ld de,20576+15
			ld hl,txtFilter
			call print
TISK		
			ld de,20544+15
			ld hl,lfndir
			call print
			ld de,20544+15+3
			ld hl,NAZEVADRESARE
			call print

			call LFNWINDOW
			call LFNPRINT
			ld hl,LFNNAME + maxline
			ld (hl),0
			ld hl,LFNNAME
			ld de,20672+2
			call print


 			 nextreg #07,0			;3,5 MHz

			 ret
vymenafile	ds 13
nula		
        ld   hl,18496
        ld   bc,6*256+32
        ld   a,%00000111
        call RAMECEK


		ld   de,18562+8-32
     	ld hl,nulatxt
        call print

		; ld   de,18560+7
     	; ld hl,reloadtxt
        ; call print

		ld   de,18624+6
     	ld hl,presstxt
        call print

		ld hl,(savepoc)
		ld (ALLFILES),hl
nula1	halt
		XOR A ;test keyboard
		IN A,(#FE)
		CPL
		AND 15
		JR Z,nula1
		XOR A ;test keyboard
		IN A,(#FE)
		CPL
		AND 15
		JR Z,nula1

		call loadscr
		nextreg #07,0			;3,5 MHz
		ret
		jp reload
		
nulatxt defb "Nothing found!",0			
reloadtxt defb "I must reload directory!",0
presstxt defb "Press any key to continue.",0
end_hledej 	 call loadscr		;návrat pomocí klávesy EDIT
			 ret
templfn		ds	129
;Posunutí o stránku výše - šipka doleva
left
		ld hl,(apos_all)
		ld a,h
		or l
		ret z  			;skončí, pokud jsme na nultém souboru
		
		ld a,(POS_F+1)
		or a
		jr z,next_page_left
		
		ld a,(POS_F+1)
        ld  c,%00001111
        call KURZOR			;smaž kurzor
		
		ld a,(POS_F+1)
		ld e,a
		ld d,0
		ld hl,(apos_all)
		or a
		sbc hl,de
		ld (apos_all),hl
		xor a
		ld (POS_F+1),a
        ld   c,%00010111
        call KURZOR
		call LFNPRINT	
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

		ret


next_page_left

		ld hl,(apos_all)
		ld de,17
		or a
		sbc hl,de
		jr c,next_left1		;méně než 17
		ld (apos_all),hl
		ld b,13
		call mull
		ld de,catbuff+13
		add hl,de
		ld (WRBUFF+1),hl
		call WRFILES
		call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

		
		ret
next_left1
		ld hl,0
		ld (apos_all),hl
		ld hl,catbuff+13
		ld (WRBUFF+1),hl
		call WRFILES
		call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print
		
		ret

; Posunutí o stránku níž - šipka doprava
right				
		ld hl,(ALLFILES)
		dec hl
		
		ld e,17
		ld d,0
		or a
		sbc hl,de
		add hl,de
		jp c,posledni

		ld a,(POS_F+1)
		cp 17
		jp z,next_page
		
		ld  a,(POS_F+1)		;na konec stránky
		push af
        ld  c,%00001111
        call KURZOR
		ld a,17
		ld (POS_F+1),a
		ld hl,(apos_all)
		pop bc
		push af
		ld a,17
		or a
		sbc a,b
		ld e,a
		ld d,0
		add hl,de
		ld (apos_all),hl
        ld   c,%00010111
		pop af
        call KURZOR
		call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print
		
		ret 


reload_dir
			
			di
			call BUFF83
			ld hl,#8000
			ld de,#8001
			ld bc,1024
			xor a
			ld (hl),a
			ldir
            
			ld bc,port1       ;the horizontal ROM switch/RAM
                                ;switch I/O ad dress
            ld a,(bankm)      ;sys tem vari able that holds cur rent
                                ;switch state
            res 4,a           ;move right to left in hor i zon tal
                                ;ROM switch (3 to 2)
                                
              or 7              ;switch in RAM page 7
              ld (bankm),a      ;must keep sys tem vari able up to
              	                  ;date (very im por tant)
              out (c),a         ;make the switch

                               ;be low BFE0h
              ei                ;in ter rupts can now be en abled

              ld hl,catbuff     ;some where for DOS to put the cata
                                ;log
              ld de,catbuff+1   ;
              ld bc,1024        ;max i mum (for +3DOS) is ac tu ally
                                ;64x13+13 = 845
              ld (hl),0
              ldir              ;make sure at least first en try is
              ld de,catbuff     ;the lo ca tion to be filled with the
aNextDirItem
              ld b,pocetpolozek ;the num ber of en tries in the
                                ;buffer
              ld c,%100            ;include sys tem files in the cata
              ld hl,stardstar   ;the file name ("*.*")
              call dos_catalog  ;call the DOS en try
NEXT0			  
              ld (savehl),hl
			  ld (saveix),ix
			  ld a,b
			  cp pocetpolozek
			  push af
			  
			  ld hl,(ALLFILES)
			  ld e,b
			  ld d,0
			  add hl,de
			  dec hl
			  ld (ALLFILES),hl

			push hl
			push de
			push bc
			
			
			ld hl,(dirNum)
			add	hl,de
			dec hl
			ld 	(dirNum),hl
			ld de,18528+24
			ld (NUMPOS+1),de
			
			call DEC16
			defw 20*256
			pop bc
			pop de
			pop hl

			  
			  ld a,(FILES)
			  add a,b
			  dec a
			  ld (FILES),a
			  pop af
			  jr c,acont

			  ld a,b
			  or a
			  jr z,acont

			  ld a,(virtmem)
			  cp 4
			  jr z,acont

			  call CountMemory
			  ex de,hl
			ld hl,virtmem
			inc (hl)
			jr aNextDirItem
acont			  
            push af
			
			ld hl,(dirNum)
			ld de,18528+24
			ld (NUMPOS+1),de
			halt
			call DEC16
			defw 20*256

              pop hl
              ld (dosret),hl    ;put it where it can be seen from
                                ; NextBASIC
              ld c,b            ;move num ber of files in cat a log to
                                ;low byte of BC
              ld b,0            ;this will be re turned in NextBASIC
                  
              di                ;about to ROM/RAM switch so be
                                ;care ful
              push bc           ;save num ber of files
              ld bc,port1       ;I/O ad dress of hor i zon tal ROM/RAM
                                ;switch
              ld a,(bankm)      ;get cur rent switch state
              
              
              set 4,a           ;move left to right (ROM 2 to ROM
                                ;3)
              and #F8           ;also want RAM page 0
              ld (bankm),a      ;up date the sys tem vari able (very
                                ;im por tant)
              out (c),a         ;make the switch
              pop bc            ;get back the saved num ber of files
              dec bc
              
              

			 call BUFF83
JKJK			 
			 ld hl,catbuff+13
			 ld de,(ALLFILES)
			 ld (save_allfiles),de
;			 dec de
atestovani	 push de
			 push hl
			 pop ix
			 push ix
			 bit 7,(ix+7)
;			 pop hl
			 jr nz,asouhlasi

			 ld de,8
			 add hl,de		;v HL je začátek přípony souboru
			 push hl
			 ld de,PT2
			 call compare
			 pop hl
			 jr z,asouhlasi
			 push hl
			 ld de,PT3
			 call compare
			 pop hl
			 jr z,asouhlasi
			 ld de,SQT
			 push hl
			 call compare
			 pop hl
			 jr z,asouhlasi
			 
			 ld de,STC
			 push hl
			 call compare
			 pop hl
			 jr z,asouhlasi

 			 ld de,STP
			 push hl
			 call compare
			 pop hl
			 jr z,asouhlasi

			 pop de
			 push de		;v de mame zacatek jmena souboru
			 ld h,d
			 ld l,e
			 add hl,13		;vynechana jedna polozka
			 ld bc,910*13
			 ldir
			 
			 ld hl,(ALLFILES)
			 dec hl
			 ld (ALLFILES),hl
			 pop hl
			 pop de
			 dec de
			 ld a,d
			 or e
			 jr nz,atestovani
			 jr askon
asouhlasi			 
			 pop hl
			 ld de,13
			 add hl,de
			 pop de
			 dec de
			 ld a,d
			 or e
			 jr nz,atestovani
askon		ei

			 ld de,18560+4
			 ld hl,txtLFNinfo
			 call print
			 call getAllLFN	
			call GETDIR
			 ld hl,16384
			 ld de,16385
			 ld bc,6143
			 xor a
			 ld (hl),a
			 ldir
			
			 call NOBUFF83
			
             call kresli

			 ld hl,catbuff+#d              
		     ld (WRBUFF+1),hl

             call WRFILES      ;vypis soubory
			 ld de,18464+15
			 ld hl,txtcurrentfile
			 call print
			
			 ld de,18432+15
			 ld hl,txtallfiles
			 call print


			ld de,20544+15
			ld hl,lfndir
			call print
			ld de,20544+15+3
			ld hl,NAZEVADRESARE
			call print


			call LFNWINDOW
			call LFNPRINT
			ld hl,LFNNAME + maxline
			ld (hl),0
			ld hl,LFNNAME
			ld de,20672+2
			call print
        
		ret

		
next_page
		ld hl,(apos_all)
		inc hl
		;test jestli už není víc souborů
		
		ex de,hl			;de ... aktualni pozice začátku výpisu
		ld hl,(ALLFILES)	;hl ... pocet souboru
		push de
		ld de,18			;odečti počet na jednu stránku
		or a
		sbc hl,de			;hl ... počet souborů - 18
		pop de				;de ... aktuální pozice
		or a
		sbc hl,de
		jr nc, next2
		ld hl,(ALLFILES)
		or a
		ld de,17
		sbc hl,de
		ld a,1
		ld (maxfile),a
		jr next3
next2
		ld hl,(apos_all)
		inc hl
		xor a
		ld (maxfile),a
next3
		ld b,13
		call mull
		ld de,catbuff ;+13
		add hl,de
		ld (WRBUFF+1),hl
		call WRFILES
		ld a,(maxfile)
		or a
		jr nz,next4
		ld hl,(apos_all)
		ld de,17
		add hl,de
		ld (apos_all),hl
		jr next5
next4	ld hl,(ALLFILES)
		dec hl
		ld (apos_all),hl
next5	call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

		ret
maxfile	db 0
posledni					;méně souborů než 17
	     ld  a,(POS_F+1)	
         ld  c,%00001111
         call KURZOR
		 ld hl,(ALLFILES)
		 dec hl
		 ld a,l
		 ld (POS_F+1),a
		 
		 ld (apos_all),hl
         ld   c,%00010111
         call KURZOR
		 call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print
		 
		 ret


right1


		ret

txtLFNinfo defb "Getting LFN information:",0
txtStart   defb "Reading files in directory:",0
;Nacte LFN vsech souboru nahranych v pameti

numLoop		defw 0



; Násobení HL x B
; Vysledek HL
mull		ld d,l		;vynásob spodní byty
			ld e,b		;
			mul d,e		;vysledek je v de
			ex de,hl	;vysledek je v hl
			ld e,b		;násobitel do e
			mul d,e		;vynásob
			ld a,e		;do akumulátoru hoď výsledek (spodní byte)
			add a,h
			ld h,a		;konečný výsledek je v HL
			ret

; Tato rutina naplní banky ZX Next LFN jmény souborů,
; o délce 128bytů. 
getAllLFN	ei
			ld hl,0
			ld (numLoop),hl
			ld hl,#e000
			ld (InBuff+1),hl
			ld a,24
			ld (Page+1),a
			call savescr
			
			call BUFF83
			ld hl,catbuff+#d
			ld de,bufftmp
			ld bc,13
			ldir
			call NOBUFF83
			
			ld hl,catbuff+#d
			ld (tmpname),hl
			ld bc,(ALLFILES)
LFN1		push bc
			ei
			
			ld hl,(numLoop)
			inc hl
			ld 	(numLoop),hl
lfnpos			ld de,18560+24	;pozice ve VideoRam
			ld (NUMPOS+1),de
			call DEC16		;vypiš číso HL - jaký soubor právě spracováváme
			defw 20*256
			di				;zakaž přerušení, jinak bude problém, nemáme nastránkovanou ZX Rom

			exx
BFN
BufferName	ld de,bufftmp			;jmeno souboru
			ld hl,stardstar
			ld ix,(savehl)
			ld bc,LFNNAME
			exx
			ld c,7
			ld de,$01b7  
			rst 8			;služba EsxDosu, která volá službu +3Dosu (zjištění LFN konkrétního souboru)
			defb M_P3DOS	
Page		ld a,24
			nextreg $57,a	;nastrankuj stranku s volnymi daty

InBuff		ld de,#e000		;ukazatel na pamet v bufferu			
			ld hl,LFNNAME	;buffer pro LFN
			ld bc,maxlen		;ulož 64 bytů
			ldir

			call BUFF83
			ld hl,(tmpname)	;přesuň se na další položku v adresáři
			ld de,13
			add hl,de
			ld (tmpname),hl	;a ulož adresu, kde se nachází
			ld de,bufftmp
			ld bc,13
			ldir

			call NOBUFF83

			ld a,(Page+1)
			nextreg $57,a
			
			ld hl,(InBuff+1)		;vypočti další adresu v bance ZX Next, kam budu ukládat
			ld de,maxlen
			add hl,de
			ld (InBuff+1),hl
			
			ld de,#FFFF-128
			or a
			sbc hl,de
			jr c,contin
			ld hl,Page+1
			inc (hl)
			ld hl,#e000
			ld (InBuff+1),hl
contin		pop bc				;zopakuj to pro všechny soubory, které máme načtené... 
			dec bc
			ld a,b
			or c
			jr nz,LFN1

			nextreg $57,1			;vráť zpátky stránku, kde se nachází data pro player
			call loadscr			;obnov obrazovku
			ret
tmpname		ds 2
BFT
bufftmp		ds 15		 
M_P3DOS	equ $94		 
reload
		nextreg #07,3             ;28MHz - jen kvůli načítání (rychlost)
		ld hl, 18496+2
		ld   bc,4*256+28
		ld   a,%00000101
		call RAMECEK

		ld de,18528+4
		ld hl,txtStart
		call print

		ld hl,0
		ld (ALLFILES),hl
		ld (dirNum),hl
		ld (apos_all),hl
		xor a
		ld (POS_F+1),a
		ld (virtmem),a
		ld hl,catbuff
		ld (Count11+1),hl
		; ld hl,(save_allfiles)
		; ld (ALLFILES),hl
		call reload_dir
		; ld de,18560+4
			 ; ld hl,txtLFNinfo
			 ; call print
			 ; call getAllLFN	

			 ; ld hl,16384
			 ; ld de,16385
			 ; ld bc,6143
			 ; xor a
			 ; ld (hl),a
			 ; ldir
						

			; call NOBUFF83
             ; call kresli

			 ; ld hl,catbuff+#d              
		     ; ld (WRBUFF+1),hl

             ; call WRFILES      ;vypis soubory
			 ; ld de,18464+15
			 ; ld hl,txtcurrentfile
			 ; call print
			
			 ; ld de,18432+15
			 ; ld hl,txtallfiles
			 ; call print

			; call LFNWINDOW
			; call LFNPRINT
			; ld hl,LFNNAME + maxline
			; ld (hl),0
			; ld hl,LFNNAME
			; ld de,20672+2
			; call print
		
		nextreg #07,0             ;3,5 Mhz
		ret
DIRNAME	defs 13
		defb 255
DIRTMP  defb "C:",255
		ds 10
NAZEVADRESARE	ds 20		
GETDIR
		ld hl,DIRTMP
		ld de,modstart
		ld bc,4
		ldir

		ld bc,port1
        ld a,(bankm)      ;sys tem vari able that holds cur rent
        res 4,a           ;move right to left in hor i zon tal
        or 7              ;switch in RAM page 7
        ld (bankm),a      ;must keep sys tem vari able up to
        out (c),a         ;make the switch
              
		ld a,1				;get path
		ld hl,modstart
		call $01b1
		
		jr c,ok1
EEEE	ld a,2
		out (254),a

ok1		ld bc,port1       ;I/O ad dress of hor i zon tal ROM/RAM
        ld a,(bankm)      ;get cur rent switch state
        set 4,a           ;move left to right (ROM 2 to ROM
        and #F8           ;also want RAM page 0
        ld (bankm),a      ;up date the sys tem vari able (very
        out (c),a         ;make the switch

		ld hl,modstart
		ld bc,1024
		ld a,#ff
		cpir
AAA
		dec hl
		dec hl
		dec hl
		
		push hl
		ld hl,DIRBUFF
		ld de,DIRBUFF+1
		ld bc,13
		ld a,32
		ld (hl),a
		ldir
		pop hl
		
		
pr1		ld a,(hl)
		cp "/"
		jr z,pr0
		cp ":"
		jp z,root

		dec hl
		jr pr1
		
		
pr0		inc hl

		ld de,DIRBUFF
		ld b,8
presun
		ld a,(hl)
		cp "/"
		jr z,prk
		ld (de),a
		inc de
		inc hl
		djnz presun
		
prk		

		ld hl,DIRBUFF
		ld bc,7
		ld a,"."

		cpir
		jr nz,PRK2
		push hl
		ld de,DIRBUFF+8
		ld bc,4
		ldir
PRK0		
		pop hl				;adresa s teckou v HL
		dec hl
		push hl
		ex de,hl			;je v DE
		ld hl,DIRBUFF+7		;az kam
		or a				
		sbc hl,de
		push hl
		pop bc				;počet bytu
		pop hl
		ld d,h
		ld e,l
		inc de
		ld a,32
		ld (hl),a
		ldir
		
PRK2
		xor a
		ld hl,DIRBUFF+11
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		ld hl,DIRBUFF+7
		set 7,(hl)

		ld hl,parrent
		ld (goto+1),hl
		;call gotodir

		ld bc,port1
        ld a,(bankm) 
        res 4,a      
        or 7         
        ld (bankm),a 
        out (c),a   

		xor a 		
		ld hl,parrent
		call $01b1				;skoc do nadrizeneho adresare

		nextreg $54,22
		nextreg $55,23
		ld hl,32768
		ld (FF+1),hl
		xor a
		ld (virtmem),a

        
        
FF		ld de,32768
        ld b,pocetpolozek 
        ld hl,stardstar   
		ld c,%100         
		call dos_catalog  
		ld a,pocetpolozek
		xor b
		jr nz,PRK4
		ld hl,(FF+1)
		ld de,pocetpolozek*13
		add hl,de
		ld (FF+1),hl
		ld hl,virtmem
		inc (hl)
		ld a,(hl)
		xor 4
		jr z,PRK4
		jr FF
		
PRK4		
		ld de,DIRBUFF			;jmeno souboru
		ld hl,stardstar
		ld ix,(savehl)
		ld bc,LFNNAME
		call	$01b7  

						;skoc zpatky do adresare
		ld hl,DIRBUFF+7
		res 7,(hl)
		ld a,255
		ld (DIRBUFF+11),a
		ld hl,DIRBUFF
		xor a	
		call $01b1
		
		; ld hl,32768
		; ld (FF2+1),hl

;FF2	ld de,32768
        ; ld b,pocetpolozek 
        ; ld hl,stardstar   
		; ld c,%101         
        ; call dos_catalog  ;call the DOS en try
		; ld a,pocetpolozek
		; xor b
		; jr nz,PRK40
		; ld hl,(FF+1)
		; ld de,pocetpolozek*13
		; add hl,de
		; ld (FF2+1),hl
		; jr FF2
; PRK40

		ld bc,port1  
        ld a,(bankm)      
        set 4,a           
        and #F8           
        ld (bankm),a      
        out (c),a   
PRK5		
		ld hl,LFNNAME
		ld bc,128
		ld a,#ff
		cpir
		dec hl
		ld (hl),0
		xor a
		ld (LFNNAME+15),a
		ld de,20544+15
		ld hl,lfndir
		call print
		ld a,32
		ld (LFNNAME-1),a
		ld de,20544+15+3
		ld hl,LFNNAME-1
		;call print
		
		ld hl,LFNNAME-1
		ld de,NAZEVADRESARE
		ld bc,20
		ldir
		
		ld hl,DIRBUFF
		ld (goto+1),hl
		ld a,255
		ld (DIRBUFF+11),a
		ld hl,DIRBUFF+7
		res 7,(hl)
;		call gotodir

		ret
GOTODIR
gotodir
		ld bc,port1
        ld a,(bankm) 
        res 4,a      
        or 7         
        ld (bankm),a 
        out (c),a    
              
		xor a 		
goto	ld hl,parrent
		call $01b1
		
		jr c,gok
		ld a,2
		out (254),a
gok		ld bc,port1  
        ld a,(bankm)      
        set 4,a           
        and #F8           
        ld (bankm),a      
        out (c),a   
		ret

testpolozky defb "A~1    ",#a0,"POP",0,0

parrent 	defb 	"..",#ff

lfndir		defb "Dir:", 0
root	
		ld a,32
		ld (LFNNAME),a
		ld a,"/"
		ld (LFNNAME+1),a
		xor a
		ld (LFNNAME+2),a
		ld de,20544+15
		ld hl,lfndir
		call print
		ld hl,LFNNAME
		ld de,NAZEVADRESARE
		ld bc,20
		ldir
		ret
DIRBUFF	ds 15
CHANGEDIR
		nextreg #07,3             ;28MHz - jen kvůli načítání (rychlost)	
		

		push hl
		ld hl, 18496+2
		ld   bc,4*256+28
		ld   a,%00000101
		call RAMECEK

		 ld de,18528+4
		 ld hl,txtStart
		 call print

		ld hl,0
		ld (ALLFILES),hl
		ld (dirNum),hl
		ld (apos_all),hl
		xor a
		ld (POS_F+1),a
		ld (virtmem),a
		ld hl,catbuff
		ld (Count11+1),hl
		ld hl,0
		ld (ALLFILES),hl		
		
		pop hl
		call BUFF83
		ld ix,DIRNAME
		ld b,13
chngdir1 ld a,(hl)
		res 7,a
		or a
		jr nz,chng
		ld a,32
chng	ld (ix+0),a
		inc hl
		inc ix
		djnz chngdir1
STOP
		
		ld hl,DIRNAME+11
chng2	ld a,(hl)
		cp 32
		jr nz,zap
		dec hl
		jr chng2
zap		ld a,255
		inc hl
		ld (hl),a
		ld bc,port1
        ld a,(bankm)      ;sys tem vari able that holds cur rent
        res 4,a           ;move right to left in hor i zon tal
        or 7              ;switch in RAM page 7
        ld (bankm),a      ;must keep sys tem vari able up to
        out (c),a         ;make the switch
              
		xor a 			;change path
		ld hl,DIRNAME
AAAA
		call $01b1
		
		jr c,ok
ERROR	ld a,2
		out (254),a

ok			ld bc,port1       ;I/O ad dress of hor i zon tal ROM/RAM
            ld a,(bankm)      ;get cur rent switch state
            set 4,a           ;move left to right (ROM 2 to ROM
            and #F8           ;also want RAM page 0
            ld (bankm),a      ;up date the sys tem vari able (very
            out (c),a         ;make the switch
			call reload_dir
;			call GETDIR
			nextreg #07,0
			ret
txtsetup	defb "Configuration",0			
txtstereo	defb "Stereo:      ACB  /  ABC",0
txtchip		defb "Sound chip:  AY   /   YM",0

txthelp0	defb "Controls:",0
txthelp1	defb "Break: stop play | Space: next song",0
txthelp2	defb "  A: AY | Y: YM | C: ACB | B: ABC  ",0

vyrazni		equ 01000111b

showsetup	
			NEXTREG2A 06
			bit 0,a 		;AY
			jr nz,sAY


			ld a,vyrazni
			ld hl,22528+32*4 + 21
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			
			ld   a,%00000001
			ld hl,22528+32*4 + 15
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			
			jr stereo
sAY			
			ld a,%00000001
			ld hl,22528+32*4 + 21
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			
			ld   a,vyrazni
			ld hl,22528+32*4 + 15
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
stereo

			NEXTREG2A 08
			bit 5,a
			jr nz,sACB
			ld a,vyrazni
			ld hl,22528+32*3 + 21
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			
			ld   a,%00000001
			ld hl,22528+32*3 + 15
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			ret

sACB
			ld a,%00000001
			ld hl,22528+32*3 + 21
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a
			
			ld   a,vyrazni
			ld hl,22528+32*3 + 15
			ld (hl),a
			inc hl
			ld (hl),a
			inc hl
			ld (hl),a

			ret

;Skok na další skladbu		 
nextsong
		 call DOWN

;Stisk klávesy ENTER. 
FIRE
		 nextreg #07,3   
		 call savescr 
		 ld a,(POS_F+1)
		 ld e,a
		 ld d,0
		ld hl,(apos_all)
		add hl,de
		ld a,l
		or h
 	    ld hl,catbuff+#D
		jr z,F1
		
		ld hl,(apos_all)
		ld b,h
		ld c,l
		
		ld de,13
  	    ld hl,catbuff+#D
F0 		add hl,de
		dec bc
		ld a,c
		or b
		jr nz,F0
F1		 
		push hl
		pop ix
		call BUFF83
		bit 7,(ix+7)

		jp nz,CHANGEDIR
		push hl
        ld   hl,18496
        ld   bc,7*256+32
        ld   a,%00000111
        call RAMECEK
		ld   hl,20480
        ld   bc,4*256+32
        ld   a,%00000101
        call RAMECEK
		
		ld   hl,16416+5
        ld   bc,5*256+22
        ld   a,%00000111
        call RAMECEK

		ld   hl,16576+1
        ld   bc,4*256+30
        ld   a,%00000101
        call RAMECEK

		; ld de,16576+3
		; ld hl,txthelp0
		; call print
		ld de,16608+3
		ld hl,txthelp1
		call print
		ld de,18432+3
		ld hl,txthelp2
		call print
		
		
		
		ld hl,22528 + 32*7 + 2
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		ld hl,22528 + 32*7 + 17
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		
		ld hl,22528 + 32*8 + 4
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a
		
		ld hl,22528 + 32*8 + 10
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a
		
		ld hl,22528 + 32*8 + 16
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a

		ld hl,22528 + 32*8 + 22
		ld a,7
		ld (hl),a
		inc hl
		ld (hl),a
		

		ld de,16448+6
		ld hl,txtsetup
		call print
		ld hl,22528+32*2 + 6
		ld a,101b
		;ld a,255
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		inc hl
		ld (hl),a
		ld de,16480+6
		ld hl,txtstereo
		call print

		ld de,16512+6
		ld hl,txtchip
		call print

		call showsetup


		ld hl,22528+32*18+1
		ld d,h
		ld e,l
		inc de
		ld a,7
		ld (hl),a
		ld bc, 28
		ldir 
		

        ; ld   hl,18592+2
		; ld (lfnat+1),hl
		; ld hl,28 + 18592+2
		; ld (lfn_1 + 1),hl
		; ld hl,28 + 18624+21
		; ld (lfn_2 + 1),hl
		; ld hl,LFNNAME+128
		; ld (lfnend+1),hl
		; ld hl,4
		; ld (odskok+1),hl
		
		
		call LFNPRINT

		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,LFNNAME
		ld de,tmp
		ld bc,maxline
		ldir

		ld de,18560+2
		ld hl,tmp
		call print
;**************************
		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,LFNNAME+maxline
		ld de,tmp
		ld bc,maxline
		ldir

		ld de,18592+2
		ld hl,tmp
		call print
;****************************
		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,LFNNAME+maxline+maxline
		ld de,tmp
		ld bc,maxline
		ldir

		ld de,18624+2
		ld hl,tmp
		call print

		
		ld hl,12*32+22528+1
		ld de,12*32+22528+2
		ld bc,29
		ld (hl),%01000111
		ldir
		ld hl,13*32+22528+1
		ld de,13*32+22528+2
		ld bc,29
		ld (hl),%01000111
		ldir
		
		ld hl,14*32+22528+1
		ld de,14*32+22528+2
		ld bc,29
		ld (hl),%01000111
		ldir
		
		
		
		ld de,20672+2	 
		ld (lfnat+1),de
		ld hl,28 + 20672+2
		ld (lfn_1 + 1),hl
		ld hl,33 + 20672+2
		ld (lfn_2 + 1),hl
		ld hl,LFNNAME+37
		ld (lfnend+1),hl
		pop de				; v DE je nazev souboru		
        call NOWRMOD
		 
		ld   de,18560+8-32+3
     	ld hl,text
		 
        call print
		   
		call LOADFile

		nextreg $57,1		
		
		ld hl,filename+9
		ld de,PT2
		call compare
		jp z,playmusic
		
		ld hl,filename+9
		ld de,PT3
		call compare
		jp z,playmusic
		
		ld hl,filename+9
		ld de,SQT
		call compare
		jp z,SQTPLAY
		
		ld hl,filename+9
		ld de,STC
		call compare
		jp z,STCPLAY
		
		ld hl,filename+9
		ld de,STP
		call compare
		jp z,STPPLAY
		
		
		
		
		ld a,32
		ld hl,endsong
		ld (hl),a
;		jp playmusic
navrat
		halt
		call loadscr
        ret
tmp     ds 49
PT2 	defb "PT2"
PT3		defb "PT3"
SQT		defb "SQT"
STC		defb "STC"
STP		defb "STP"
endsong	defb 0
Loading defb "Plase wait, loading file...",0
; Nahrání souboru pomocí EsxDos služeb. Rutina nemá žádný vstup,
; vše si zjišťuje z proměnných (na adrese filename je již připravený název souboru)
LOADFile
		nextreg #07,3             ;28MHz - jen kvůli načítání (rychlost)
		ld hl, 20640
		ld   bc,3*256+32
        ld   a,%00000010
		call RAMECEK

		ld de,20672+2
		ld hl,Loading
		call print
		ld      b,FA_READ
        ld      a,'*'
        ld      hl,filename
        ESXDOS  F_OPEN
		ld (fread+1),a

		ld hl,modstart
		ld de,modstart+1
		ld bc,22048
		ld a,255
		ld (hl),a
		ldir

		

        ld      hl,modstart
        ld      bc,22048
        call    fread
		call 	fclose


		ld hl,modstart + 17000
	
hledej_konec_souboru
		ld a,(hl)
		cp 255
		jr nz,nasel
		dec hl
		jr hledej_konec_souboru

nasel	ld (delka_souboru),hl

		nextreg #07,0             ;28MHz - jen kvůli načítání (rychlost)
		ret
fclose
        ld      a,(fread+1)
        ESXDOS  F_CLOSE
        ret
fread:
        ld a,1              
        ESXDOS  F_READ
        ret  nc


fileError:                  
        push    af
        and     7
        out     (254),a     
        pop     af
        ld      b,1         
        ld      de,esxError
        ESXDOS  M_GETERR
        ld      hl,esxError
        jp      customErrorToBasic


switchLayer2Off:           
        ret


;Uloží obrazovku do banky 19 ZX Next. Ukládají se spodní dvě třetiny obrazovky,
;a celé atributy. Banka 19 se stránkuje od adresy $e000
savescr	
		nextreg $57,19				;Stránka na uložení VideoRam
		ld hl,16384
		ld de,bufscr
		ld bc,6912
		ldir
		nextreg $57,1				;Nastránkuj zpátky
		ret

;Obnovení spodních dvou třetin obrazovky z 19 stárnky ZX Next.		
loadscr	
		nextreg $57,19
		ld hl,bufscr
		ld de,16384
		ld bc,6912
		ldir
		nextreg $57,1
		ret
		
customErrorToBasic: ; HL = message with |80 last char
		ld a,1
		out (254),a
		ret
keysound db 0				;key sound 0= yes,1= no, klavesnicove echo
;KeyScan od Busyho z MRSu
KEYSCAN  ld   l,47			;testovani klavesnice
         ld   de,65535
         ld   bc,65278
KEYLINE  in   a,(c)
         cpl  
         and  31
         jr   z,KEYDONE
         ld   h,a
         ld   a,l
KEY3KEYS inc  d
         ret  nz
KEYBITS  sub  8
         srl  h
         jr   nc,KEYBITS
         ld   d,e
         ld   e,a
         jr   nz,KEY3KEYS
KEYDONE  dec  l
         rlc  b
         jr   c,KEYLINE
         ld   a,d
         inc  a
         ret  z
         cp   40
         ret  z
         cp   25
         ret  z
         ld   a,e
         ld   e,d
         ld   d,a
         cp   24
         ret  
         
SYMTAB   db "*^[&%>}/"
         db ",-]'$<{?"
         db ".+($"
         db 200
         db '/',' '
         db 0
         db "=;)@"
         db 201
         db "|:"
         db 32,13,34
         db "_!"
         db 199
         db "~",0
         
CAPSTAB  db "BHY"
         db 10,8
         db "TGV"
         db "NJU"
         db 11,5
         db "RFC"
         db "MKI"
         db 9,4
         db "EDX"
         db 2
         db "LO"
         db 15,6
         db "WSZ"
         db 1,13,"P"
         db 12,7
         db "QA"
         
NORMTAB  db "bhy65tgv"
         db "nju74rfc"
         db "mki83edx"
         db 0
         db "lo92wsz"
         db 32,13
         db "p01qa"
         db 0

beepk		ld a,(keysound)		;Busyho nahradni rutina,kratsi
		or a
		ret nz
		ld a,(BORDER)
		ld e,a
		ld b,$10
		add a,b
;		ld a,$10+border
		out ($fe),a
		ld b,$1c
beepk1		djnz beepk1
		ld a,$08
		add a,e
;		ld a,$08+border
		out ($fe),a
		ret
BORDER   db 0				;okraj

;Pohyb nahoru - na pozici kurzoru se používa 16bitové číslo,
;takže maximální počet souboru je omezen, ale prakticky se k 
;němu ani nepřiblížíme. 
;Při scrolování se vypisuje jen spodní řádek, jinak se volá 
;rutinka ROLLDOWN, která provede scroll o 8 pixelů dolů. Je to rychlejší,
;než vypisovat všechny soubory znova.
UP
POS_F    ld a,0 
		 or a
		 jp z,UP1
		 ld hl,(apos_all)
		 ld a,h
         or   l
         jp   z,UP1
         ld   c,%00001111
		 ld a,(POS_F+1)
         call KURZOR
		 ld hl,(apos_all)
         dec  hl
         ld   (apos_all),hl

		 dec a
		 ld (POS_F+1),a
         ld   c,%00010111
         call KURZOR
		 call LFNPRINT
 		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

         ret

UP1
POS_ALL  ld   a,0
         ;or   a
         ;ret  z
         ;dec  a
         ;ld   (POS_ALL+1),a
		 ld hl,(apos_all)
		 ld a,l
		 or h
		 ret z
		 dec hl
		 ld (apos_all),hl
         push af
         call ROLLDOWN
         pop  af
         ld   hl,16448+1
         call PRN_F
		 call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

         ret
DOWN
		ld hl,(ALLFILES)
		dec hl
		
		ld e,17
		ld d,0
		or a
		sbc hl,de
		ld c,l
		jp c,D1
		ld c,17
		
D1
         ld   a,(POS_F+1)
         cp   c
         jp   z,DOWN1
		 ld e,a
		 ld d,0
		 ld hl,(ALLFILES)
		 dec hl
		 or a
		 sbc hl,de
		 ret z
         ld   c,%00001111
         call KURZOR

         inc  a
         ld   (POS_F+1),a
		 ld hl,(apos_all)
		 inc hl
		 ld (apos_all),hl
         ld   c,%00010111
         call KURZOR
		 
		 call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print
		 
         ret
		 
apos_all defw 0
DOWN1
       	ld hl,(apos_all)
		;dec hl
		ex de,hl
		ld hl,(ALLFILES)
		dec hl
		ex de,hl
		or a
		sbc hl,de
		ret z			;maximum souboru
  
        ld hl,(apos_all)
		;dec hl
		ex de,hl
		ld hl,(ALLFILES)
		dec hl
		ex de,hl
		or a
		sbc hl,de
		
		ex de,hl
		ld hl,(apos_all)
		
		or a
		sbc hl,de
		ret z

         ld   a,(POS_ALL+1)
         inc  a
         ld   (POS_ALL+1),a
		 ld hl,(apos_all)
		 inc hl
		 ld (apos_all),hl
         push af
         call ROLLUP
         pop  af
         ld   c,a
         ld   a,(POS_F+1)
         add  a,c
         ld   hl,20576+1
         call PRN_F
		 call LFNPRINT
		ld hl,LFNNAME + maxline
		ld (hl),0
		ld hl,LFNNAME
		ld de,20672+2
		call print

         ret

;Font na vykreslování rámečků
SPECFNT
         defw 32512
         defw 30079
         defw 28768
         defw 28768

         defw 65280
         defw 22015
         defw 0
         defw 0

         defw 65024
         defw 22270
         defw 1550
         defw 1550

         defw 28768
         defw 28768
         defw 28768
         defw 28768

         defw 0
         defw 0
         defw 0
         defw 0

         defw 1550
         defw 1550
         defw 1550
         defw 1550

         defw 28768
         defw 32618
         defw 30079
         defw 28768

         defw 0
         defw 65450
         defw 22015
         defw 0

         defw 1550
         defw 65198
         defw 22270
         defw 1550

         defw 28768
         defw 28768
         defw 32618
         defw 127

         defw 0
         defw 0
         defw 65450
         defw 255

         defw 1550
         defw 1550
         defw 65198
         defw 254

         defb 0,85,0,85,0,85,0
         defb 0

DOWNHL
         inc  h
         ld   a,h
         and  7
         ret  nz
         ld   a,l
         add  a,32
         ld   l,a
         ld   a,h
         ret  c
         sub  8
         ld   h,a
         ret


DOWNH
         ld   a,l
         add  a,32
         ld   l,a
         ld   a,h
         ret  nc
         add  a,2048/256
         ld   h,a
         ret

;Rutinka na vypsání jednoho znaku 8bitovým fontem.
; Vstup: 
			; A ... znak
			; Pozice se určuje návěstím P_AT
PRINT
         exx
         ld   l,a
         ld   h,0
         add  hl,hl
         add  hl,hl
         add  hl,hl
FNT      ld   bc,CHARS
         add  hl,bc
P_AT     ld   de,16384

         push de
         ld   a,(hl)
         
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         inc  hl
         inc  d
         ld   a,(hl)
         ld   (de),a
         pop  de

         inc  e
         jr   nz,CHAR1B
         ld   a,d
         add  a,8
         ld   d,a
         cp   88
         jr   c,CHAR1B
         ld   d,64
CHAR1B   ld   (P_AT+1),de
         exx
         ret

; Vykreslení rámečku
; Vstup: 
				;hl=adrvrm
				;bc,vyska * 256 + sirka
				;a=barva
RAMECEK
         push af
         push hl
         push bc
         ld   (P_AT+1),hl

         dec  c
         dec  c
         push hl
         ld   hl,SPECFNT
         ld   (FNT+1),hl
         push bc

         ld   a,0
         call PRINT
         ld   b,c
RAM1     ld   a,1
         call PRINT
         djnz RAM1
         ld   a,2
         call PRINT
         pop  bc
         pop  hl
         dec  b
         dec  b
         call DOWNH
         ld   (P_AT+1),hl
RAM3
         push hl
         push bc
         ld   a,3
         call PRINT
         ld   b,c
RAM2     ld   a,4
         call PRINT
         djnz RAM2
         ld   a,5
         call PRINT
         pop  bc
         pop  hl
         call DOWNH
         ld   (P_AT+1),hl
         djnz RAM3

         ld   a,9
         call PRINT
         ld   b,c
RAM4     ld   a,10
         call PRINT
         djnz RAM4
         ld   a,11
         call PRINT

         ld   hl,CHARS
         ld   (FNT+1),hl

         pop  bc
         pop  hl
         ld   a,h
         sub  64
         rrca
         rrca
         rrca
         and  3
         add  a,88
         ld   h,a

         dec  c

         pop  af


ATRR     push bc
         push hl
         ld   d,h
         ld   e,l
         inc  e
         ld   b,0
         ld   (hl),a
         ldir
         pop  hl
         ld   de,32
         add  hl,de
         pop  bc
         djnz ATRR

         ret
		 
tcursor defb "Cursors: move",0		 
tenter defb "Enter: play song",0		 
tspace defb "Space: play next",0		 
tsearch defb "S: search",0		 
treload defb "R: reload directory",0		 
		 
; Nakreslí všechny okna potřebné pro NextPlayer
kresli:
		  

		  ld   hl,16416
		  ld   bc,20*256+14
		  ld   a,%00001111
		  call RAMECEK
 
		  ld   hl,16608+14
		  ld   bc,14*256+18
		  ld   a,%00001111
		  call RAMECEK
 
		  ld   hl,16416+14
		  ld   bc,6*256+18
		  ld   a,%00000111
		  call RAMECEK
		  
		  
		  
		  
		  ld   hl,16384+1
		  ld   de,TEXT1
		  call TEXT

		  ld   de,16384+ 14
		  ld   hl,nnext
		  call print
		ld hl,22528+14
		ld a,5
		ld b,17
kk		ld (hl),a
		inc hl
		djnz kk

         ld   hl,16448+15
         ld   de,Versionn
		 ex de,hl
         call print
         ld   hl,16480+15
         ld   de,Cursors
         ex de,hl
		 call print
         ld   hl,16512+15
         ld   de,Play
         ex de,hl
		 call print
         ld   hl,16544+15
         ld   de,NextPlay
         ex de,hl
		 call print

         ld   hl,18528+15
         ld   de,tcursor
         ex de,hl
		 call print
         ld   hl,18560+15
         ld   de,tenter
         ex de,hl
		 call print
         ld   hl,18592+15
         ld   de,tspace
         ex de,hl
		 call print
         ld   hl,18624+15
         ld   de,tsearch
         ex de,hl
		 call print
         ld   hl,18656+15
         ld   de,treload
         ex de,hl
		 call print
		ld a,00001101b

		ld hl,11*32+22528 + 15
		ld b,14
		ld (hl),a
		inc hl
		djnz $-2

		ld hl,12*32+22528 + 15
		ld b,14
		ld (hl),a
		inc hl
		djnz $-2

		ld hl,13*32+22528 + 15
		ld b,14
		ld (hl),a
		inc hl
		djnz $-2

		ld hl,14*32+22528 + 15
		ld b,14
		ld (hl),a
		inc hl
		djnz $-2

		ld hl,15*32+22528 + 15
		ld b,15
		ld (hl),a
		inc hl
		djnz $-2


              ret

Versionn  defb "Version: 0.6",0
Cursors defb " ",0
Play    defb "Main program: Shrek",0 
NextPlay defb "AY routines: mborik",0
TEXT1    defb "NextPlayer",0
         
nnext    defb "  for ZX Spectrum Next",0		 



WRFILES	 
		 call BUFF83
         ld   hl,(ALLFILES)
		 ld a,l
		 ld de,18
         or a
		 sbc hl,de
         jr   c,WRF4
         ld   a,18
WRF4     ld   b,a
         ld   hl,16448+1
WRBUFF   ld   de,catbuff+#D

WRF2     push bc
         push hl
         ld   (P_AT+1),hl

         ld   a,(de)
         cp   32
         jr   z,ENDP
         call WRMOD

         ld   hl,POC
         inc  (hl)
         pop  hl
         call DOWNH
         pop  bc
         djnz WRF2
E2
         ld   a,(POS_F+1)
         ld   c,%00010111
         call KURZOR
		 call NOBUFF83
         ret

ENDP     pop  hl
         pop  bc
         ld   hl,POC
         ld   a,(hl)
         ld   (FILES),a
         jr   E2
dir		 defb 0
WRMOD	 
		 
		 
		 
		 ;původni WRMOD
		 push de
		 ld hl,7
		 add hl,de
		 bit 7,(hl)
		 jr z,neni_adr
		 ld a,1
		 ld (dir),a
		 jr je_dir
neni_adr xor a
		 ld (dir),a
je_dir		 
		 pop de
		 ld ix,filename
         ld   b,8
WRF1     ld   a,(de)
         or   a
         jr   z,WWR
		 ld (ix+0),a
		 inc ix
		 res 7,a
         call PRINT
         inc  de
         djnz WRF1
		 ld a,(dir)
		 or a
		 jr nz,wwrdir
		 ld a,"."
		 ld (ix+0),a
		 inc ix
		 call PRINT
		 ld b,3
wrf2	 ld a,(de)
		 res 7,a
		 ld (ix+0),a
		 inc ix

		 call PRINT
		 inc de
		 djnz wrf2
		 inc de
		 inc de
         ret
WWR
         inc  de
         ret
		 
wwrdir	
		push de
		ld a,32
		call PRINT
		ld a,"D"
		call PRINT
		ld a,"I"
		call PRINT
		ld a,"R"
		call PRINT
		pop de

		inc de
		inc de
		inc de
		inc de
		;inc de
		inc de
		ret

NOWRMOD	 call BUFF83
		 ld ix,filename
         ld   b,8
aWRF1     ld   a,(de)
         or   a
         jr   z,aWWR
		 ld (ix+0),a
		 inc ix
         ;call PRINT
         inc  de
         djnz aWRF1
		 ld a,"."
		 ld (ix+0),a
		 inc ix

;		 call PRINT
		 ld b,3
awrf2	 ld a,(de)
		 res 7,a
		 ld (ix+0),a
		 inc ix

;		 call PRINT
		 inc de
		 djnz awrf2
		 inc de
		 inc de
         call NOBUFF83
		 ret
aWWR
         inc  de
		 call NOBUFF83
         ret


S_TEXT
         ld   bc,SPECFNT
         ld   (FNT+1),bc
         call TEXT
         ld   hl,CHARS
         ld   (FNT+1),hl
         ret
TEXT
         ld   (P_AT+1),hl
         ex   de,hl
TEX1
         ld   a,(hl)
         or   a
         ret  z
		 bit 7,a
		 res 7,a
		 jp nz,PRINT
         call PRINT
         inc  hl
         jr   TEX1
         ret
FILES    defb 0
ALLFILES defb 0,0
POC      defb 0

KURZOR
         push af
         add  a,a
         add  a,a
         add  a,a
         ld   l,a
         ld   h,0
         add  hl,hl
         add  hl,hl
         ld   de,22528+64+1
         add  hl,de
         ld   e,l
         ld   d,h
         inc  de
         ld   (hl),c
         ld   bc,11
         ldir
         pop  af
         ret

PRN_F
		 call BUFF83
         ld   (P_AT+1),hl
		 ld hl,(apos_all)
		 
		 ld a,l
		 or h
		 ex de,hl
		 ld hl,catbuff+#d
		 jr z,F12
		 ld hl,catbuff
		 ld b,d
		 ld c,e
		 ld de,13
F02      add hl,de
		 
		 ld a,b
		 or c
		 dec bc
		 jr nz,F02
F12		 
         ex   de,hl
         call WRMOD
		 call NOBUFF83
         ret
         

INIROLL  nextreg $57,18	
         ld   b,17*8
         ld   hl,UPDATA
IR1
INI1     ld   de,16480+1
         ld   (hl),e
         inc  hl
         ld   (hl),d
         inc  hl
         call DOWNDE
         ld   (INI1+1),de

INI2     ld   de,16448+1
         ld   (hl),e
         inc  hl
         ld   (hl),d
         inc  hl
         call DOWNDE
         ld   (INI2+1),de
         djnz IR1

         ld   b,17*8
         ld   hl,UPDATA-1
IR2
INI12    ld   de,16480+1
         ld   (hl),d
         dec  hl
         ld   (hl),e
         dec  hl
         call DOWNDE
         ld   (INI12+1),de

INI22    ld   de,16448+1
         ld   (hl),d
         dec  hl
         ld   (hl),e
         dec  hl
         call DOWNDE
         ld   (INI22+1),de
         djnz IR2

ROLLUP
		 
         ld   hl,UPDATA
         jp   ROLL
ROLLDOWN 
         ld   hl,DOWNDATA
ROLL	 nextreg $57,18	
         ld   (SP_ST+1),sp
         ld   sp,hl
         ld   a,8*17
ROLL1
         pop  hl
         pop  de

         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi
         ldi

         dec  a
         jr   nz,ROLL1

SP_ST    ld   sp,0
		 nextreg $57,1				;Stránka na uložení VideoRam
         ret

DOWNDE
         inc  d
         ld   a,d
         and  7
         ret  nz
         ld   a,e
         add  a,32
         ld   e,a
         ld   a,d
         ret  c
         sub  8
         ld   d,a
         ret
maxpos	defw 0
maxpos2 defw 0
pp		defb 0
pp_a	defb 0
print:	push hl
		push de
		ld hl,55
		add hl,de
		
		ld (maxpos),hl
		ld de,33
		add hl,de
		ld (maxpos2),hl
		pop de
		pop hl
;		ld	de,printpos
p6b		ld	(pos),de
		xor	a
		ld	(roll),a	
 
print1: 
		push hl
		ld hl,(maxpos)
        or a
		ld de,(pos)
		sbc hl,de
		jr nz,pophl
odskok	ld hl,8
		add hl,de
		ld (pos),hl
		
		
pophl		
		ld hl,(pos)
		ld de,(maxpos2)
		or a
		sbc hl,de
		jr nz, pophl2
		pop hl
		ret
pophl2		
		pop hl
		push	hl
		ld	a,(hl)
		and	127
		or a
		jr nz,ppp1
	
		ld a,32
ppp1		
		call	ascii
		ld	a,(roll)
		inc	a
		cp	1
		call	z,print2
		cp	2
		call	z,print2
		cp	4
		jr	nz,print3
		ld	a,0
		call	print2
print3:
		ld	(roll),a
		pop	hl
		push hl
		ld a,(hl)
		or a
		pop hl
		inc	hl
		ret	z
		jr	print1
 
print2:
		ld	hl,(pos)
		inc	hl
		ld	(pos),hl
		ret
 
ascii:
		ld	bc,font
		push	bc
		sub	32
		ld	e,a
		ld	d,0
		ld	b,8
		ld	hl,0
x8:
		add	hl,de
		djnz	x8
		pop	bc
		add	hl,bc
		ld	de,(pos)
		ex	de,hl
asci0:
		ld	a,(roll)
		cp	3
		jr	z,roll3
		cp	2
		jr	z,roll2
		cp	1
		jr	z,roll1
roll0:
		call	asci1
		ret
 
roll1:
		ld	b,8
rol1_1:
		ld	a,(de)
		sla	a
		ld	c,0
		rl	c
		sla	a
		rl	c
		push	bc
		call	asci2
		pop	bc
		inc	de
		djnz	rol1_1
		ret
 
roll2:
		ld	b,8
rol2_1:
		ld	a,(de)
		sla	a
		ld	c,0
		rl	c
		sla	a
		rl	c
		sla	a
		rl	c
		sla	a
		rl	c
		push	bc
		call	asci2
		pop	bc
		inc	de
		djnz	rol2_1
		ret
 
roll3:
		ld	b,8
rol3_1:
		ld	a,(de)
		srl	a
		srl	a
		push	bc
		call	asci3
		pop	bc
		inc	de
		djnz	rol3_1
		ret
 
asci1:
		ld	b,8
asc1:
		ld	a,(de)
		ld	(hl),a
		inc	de
		call	downhl
		djnz	asc1
		ret
 
asci2:
		ld	(hl),a
		ld	a,c
		dec	hl
		or	(hl)
		ld	(hl),a
		inc	hl
		call	downhl
		ret
 
asci3:
		or	(hl)
		ld	(hl),a
 
downhl:
		inc	h
		ld	a,h
		and	7
		ret	nz
		ld	a,l
		add	a,32
		ld	l,a
		ld	a,h
		jr	c,down2
		sub	8
		ld	h,a
down2:
		cp	88
		ret	c
		ld	hl,64
		ret
 
;5-bit font
font:
		db	$00, $00, $00, $00, $00, $00, $00, $00
		db	$00, $20, $20, $20, $20, $00, $20, $00
		db	$00, $50, $50, $00, $00, $00, $00, $00
		db	$00, $50, $F8, $50, $50, $F8, $50, $00
		db	$00, $20, $F8, $A0, $F8, $28, $F8, $20
		db	$00, $40, $A8, $50, $20, $50, $A8, $10
		db	$00, $20, $50, $20, $68, $90, $68, $00
		db	$00, $10, $20, $00, $00, $00, $00, $00
		db	$00, $08, $10, $10, $10, $10, $08, $00
		db	$00, $40, $20, $20, $20, $20, $40, $00
		db	$00, $00, $50, $20, $F8, $20, $50, $00
		db	$00, $00, $20, $20, $F8, $20, $20, $00
		db	$00, $00, $00, $00, $00, $10, $10, $20
		db	$00, $00, $00, $00, $78, $00, $00, $00
		db	$00, $00, $00, $00, $00, $30, $30, $00
		db	$00, $00, $08, $10, $20, $40, $80, $00
		db	$00, $70, $98, $A8, $A8, $C8, $70, $00
		db	$00, $60, $A0, $20, $20, $20, $F8, $00
		db	$00, $70, $88, $08, $70, $80, $F8, $00
		db	$00, $70, $88, $30, $08, $88, $70, $00
		db	$00, $10, $30, $50, $90, $F8, $10, $00
		db	$00, $F8, $80, $F0, $08, $88, $70, $00
		db	$00, $70, $80, $F0, $88, $88, $70, $00
		db	$00, $F8, $08, $10, $20, $40, $40, $00
		db	$00, $70, $88, $70, $88, $88, $70, $00
		db	$00, $70, $88, $88, $78, $08, $70, $00
		db	$00, $00, $00, $20, $00, $00, $20, $00
		db	$00, $00, $20, $00, $00, $20, $20, $40
		db	$00, $00, $08, $10, $20, $10, $08, $00
		db	$00, $00, $00, $78, $00, $78, $00, $00
		db	$00, $00, $20, $10, $08, $10, $20, $00
		db	$00, $70, $88, $10, $20, $00, $20, $00
		db	$00, $70, $08, $68, $A8, $A8, $70, $00
		db	$00, $70, $88, $88, $F8, $88, $88, $00
		db	$00, $F0, $88, $F0, $88, $88, $F0, $00
		db	$00, $70, $88, $80, $80, $88, $70, $00
		db	$00, $E0, $90, $88, $88, $90, $E0, $00
		db	$00, $F8, $80, $F0, $80, $80, $F8, $00
		db	$00, $F8, $80, $F0, $80, $80, $80, $00
		db	$00, $70, $88, $80, $B8, $88, $70, $00
		db	$00, $88, $88, $F8, $88, $88, $88, $00
		db	$00, $F8, $20, $20, $20, $20, $F8, $00
		db	$00, $08, $08, $08, $88, $88, $70, $00
		db	$00, $90, $A0, $C0, $A0, $90, $88, $00
		db	$00, $80, $80, $80, $80, $80, $F8, $00
		db	$00, $88, $D8, $A8, $88, $88, $88, $00
		db	$00, $88, $C8, $A8, $98, $88, $88, $00
		db	$00, $70, $88, $88, $88, $88, $70, $00
		db	$00, $F0, $88, $88, $F0, $80, $80, $00
		db	$00, $70, $88, $88, $A8, $98, $70, $00
		db	$00, $F0, $88, $88, $F0, $90, $88, $00
		db	$00, $70, $80, $70, $08, $88, $70, $00
		db	$00, $F8, $20, $20, $20, $20, $20, $00
		db	$00, $88, $88, $88, $88, $88, $70, $00
		db	$00, $88, $88, $88, $88, $50, $20, $00
		db	$00, $88, $88, $88, $88, $A8, $50, $00
		db	$00, $88, $50, $20, $20, $50, $88, $00
		db	$00, $88, $50, $20, $20, $20, $20, $00
		db	$00, $F8, $08, $30, $40, $80, $F8, $00
		db	$00, $38, $20, $20, $20, $20, $38, $00
		db	$00, $00, $80, $40, $20, $10, $08, $00
		db	$00, $70, $10, $10, $10, $10, $70, $00
		db	$00, $20, $70, $A8, $20, $20, $20, $00
		db	$00, $00, $00, $00, $00, $00, $00, $FC
		db	$00, $30, $48, $E0, $40, $40, $F8, $00
		db	$00, $00, $70, $08, $78, $88, $78, $00
		db	$00, $80, $80, $F0, $88, $88, $F0, $00
		db	$00, $00, $38, $40, $40, $40, $38, $00
		db	$00, $08, $08, $78, $88, $88, $78, $00
		db	$00, $00, $70, $88, $F0, $80, $78, $00
		db	$00, $30, $40, $60, $40, $40, $40, $00
		db	$00, $00, $78, $88, $88, $78, $08, $70
		db	$00, $80, $80, $F0, $88, $88, $88, $00
		db	$00, $20, $00, $60, $20, $20, $70, $00
		db	$00, $08, $00, $08, $08, $08, $48, $30
		db	$00, $40, $50, $60, $60, $50, $48, $00
		db	$00, $40, $40, $40, $40, $40, $30, $00
		db	$00, $00, $D0, $A8, $A8, $A8, $A8, $00
		db	$00, $00, $F0, $88, $88, $88, $88, $00
		db	$00, $00, $70, $88, $88, $88, $70, $00
		db	$00, $00, $F0, $88, $88, $F0, $80, $80
		db	$00, $00, $78, $88, $88, $78, $08, $08
		db	$00, $00, $38, $40, $40, $40, $40, $00
		db	$00, $00, $70, $80, $70, $08, $F0, $00
		db	$00, $20, $70, $20, $20, $20, $18, $00
		db	$00, $00, $88, $88, $88, $88, $70, $00
		db	$00, $00, $88, $88, $50, $50, $20, $00
		db	$00, $00, $88, $A8, $A8, $A8, $50, $00
		db	$00, $00, $88, $50, $20, $50, $88, $00
		db	$00, $00, $88, $88, $88, $78, $08, $70
		db	$00, $00, $F8, $10, $20, $40, $F8, $00
		db	$00, $38, $20, $C0, $20, $20, $38, $00
		db	$00, $10, $10, $10, $10, $10, $10, $00
		db	$00, $E0, $20, $18, $20, $20, $E0, $00
		db	$00, $28, $50, $00, $00, $00, $00, $00
		db	$00, $78, $84, $B4, $B4, $84, $78, $00
 
pos:		dw	0
roll:		db	0
txtbuf:		ds	44
 
text:
		db	"Now playing: ",0
txtallfiles db "All files:",0         
txtcurrentfile db "Position:",0

DOWNDATA equ $e000
UPDATA   equ DOWNDATA + 4*8*17
bufscr equ $e000
KKK
		
			org #9000
txtbuff    ds 100

			module key
			
check 	
ahl0	
		 call KEYSCAN			       

		 ld   a,e
         inc  a
         ret z
         ld   a,d
         ld   hl,SYMTAB
         cp   $18
         jr   z,aHLSM2
         ld   hl,CAPSTAB
         cp   $27
         jr   z,aHLSM2
         ld   hl,NORMTAB
aHLSM2    ld   d,0
         add  hl,de
         ld   a,(hl)
         or   a
         ret z
         
aLAST_KEY ld   b,0
         cp   b
         jr   z,aSEDI_KEY

aSEDI_KEY 
         ld   (aLAST_KEY+1),a
		push af
		;call beepk
		pop af
		ret 				
			endmodule


infosong
			push hl
			ld hl,txtbuff
			ld de,txtbuff+1
			ld bc,99
			ld a,32
			ld (hl),a
			ldir
			pop hl

			ld de,txtbuff
			ld b,90
i1			ld a,(hl)			
			cp 32
			ret c
			cp 128
			ret nc
			ld (de),a
			inc de
			inc hl
			djnz i1
			ret
		
pocitadlo	defb 0
ACBhlas		defb 0
		
STCPLAY	di

			ld hl,20640
			ld de,by1
			ld b,8*3
sappp        push de
			push bc
			push hl
			ld b,32
sappp0		ld a,(de)			
			ld (hl),a
			inc hl
			djnz sappp0
			pop hl
			pop bc
			pop de
			
			call downhl
			inc de
			djnz sappp


			; ld de,18464+15
			; ld hl,txtcurrentfile
			; call print
			
			; ld de,18432+15
			; ld hl,txtallfiles
			; call print

			
		; ld hl,(apos_all)
		; inc hl
		; ld de,18464+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		
		; ld hl,(ALLFILES)
		; ld de,18432+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		ld a,0
		ld hl,endsong
		ld (hl),a	



		ld hl,modstart + 7
		call infosong
		ld hl,txtbuff
		xor a
		ld (txtbuff+18),a
        ld de,20512+1
		call print


		call	stc.music_init
.sloop	ei
		halt
				
		push hl
		push de 
		push bc
		
		ld hl,ACBhlas
		xor a
		ld (hl),a
		
		 ld   hl,23200;Vymazání
         ld   de,23201;předešlého
         ld   bc,96   ;zobrazení.
         ld   (hl),b
         ldir

         ld   a,8     ;A teď nové
         ld   hl,23216;zobrazení.
         ld   de,23215;Kanál A.
         call ATR

         ld   a,10    ;Kanál C.
         ld   hl,23248
         ld   de,23247
         call ATR

         ld   a,9     ;Kanál B.
         ld   hl,23280
         ld   de,23279
         call ATR
		 
		 
		 ld a,(ACBhlas)
		 or a
		 jr z,.s1
		 ld hl,pocitadlo
		 xor a
		 ld (hl),a
		 jr .s2
.s1		 ld hl,pocitadlo
		 inc (hl)
.s2		 

		call keysetup
		pop bc
		pop de
		pop hl
		
		call	stc.music_play
		xor a
        ld a,(klavesa)
        cp 1
		jr z,sakoneca
		cp 32
		jr z,.s3
		ld a,(pocitadlo)
		xor 100
		jr z,.s3
		ld a,(music_setup)
		bit 7,a
		jr z,.sloop
		
.s3		ld a,32
		ld hl,endsong
		ld (hl),a		
sakoneca	push af	

		call music_mute
		pop af
		jp navrat



STPPLAY		di

			ld hl,20640
			ld de,by1
			ld b,8*3
stappp        push de
			push bc
			push hl
			ld b,32
stappp0		ld a,(de)			
			ld (hl),a
			inc hl
			djnz stappp0
			pop hl
			pop bc
			pop de
			
			call downhl
			inc de
			djnz stappp


			; ld de,18464+15
			; ld hl,txtcurrentfile
			; call print
			
			; ld de,18432+15
			; ld hl,txtallfiles
			; call print

			
		; ld hl,(apos_all)
		; inc hl
		; ld de,18464+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		
		; ld hl,(ALLFILES)
		; ld de,18432+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		ld a,0
		ld hl,endsong
		ld (hl),a	



		ld hl,modstart + 11 + 27
		call infosong
		ld hl,txtbuff
		xor a
		ld (txtbuff+80),a

		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,txtbuff
		ld de,tmp
		ld bc,25
		ldir
		ld hl,tmp
        ld de,20512+1
		call print


		
		

		call	stp.music_init
.astloop	ei
		halt
				
		push hl
		push de 
		push bc
		
		ld hl,ACBhlas
		xor a
		ld (hl),a
		
		 ld   hl,23200;Vymazání
         ld   de,23201;předešlého
         ld   bc,96   ;zobrazení.
         ld   (hl),b
         ldir

         ld   a,8     ;A teď nové
         ld   hl,23216;zobrazení.
         ld   de,23215;Kanál A.
         call ATR

         ld   a,10    ;Kanál C.
         ld   hl,23248
         ld   de,23247
         call ATR

         ld   a,9     ;Kanál B.
         ld   hl,23280
         ld   de,23279
         call ATR
 		 call keysetup
		 
		 ld a,(ACBhlas)
		 or a
		 jr z,.st1
		 ld hl,pocitadlo
		 xor a
		 ld (hl),a
		 jr .st2
.st1		 ld hl,pocitadlo
		 inc (hl)
.st2		 
		pop bc
		pop de
		pop hl
		
		call	stp.music_play
		xor a
        ld a,(klavesa)
        cp	1
		jr z,stakoneca
		cp 32
		jr z,.st3
		
		ld a,(pocitadlo)
		xor 100
		jr z,.st3
		ld a,(music_setup)
		bit 7,a
		jr z,.astloop
		
.st3		ld a,32
		ld hl,endsong
		ld (hl),a		
stakoneca	push af	

		call stc.music_mute
		pop af
		jp navrat


sqt_txt	defb "SQ Tracker", 0
SQTPLAY	di

			ld hl,20640
			ld de,by1
			ld b,8*3
appp        push de
			push bc
			push hl
			ld b,32
appp0		ld a,(de)			
			ld (hl),a
			inc hl
			djnz appp0
			pop hl
			pop bc
			pop de
			
			call downhl
			inc de
			djnz appp


			; ld de,18464+15
			; ld hl,txtcurrentfile
			; call print
			
			; ld de,18432+15
			; ld hl,txtallfiles
			; call print

			
		; ld hl,(apos_all)
		; inc hl
		; ld de,18464+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		
		; ld hl,(ALLFILES)
		; ld de,18432+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		ld a,0
		ld hl,endsong
		ld (hl),a	

		ld hl,sqt_txt
        ld de,20512+1
		call print


		call	SQ_INIT
.loop	ei
		halt
				
		push hl
		push de 
		push bc
		 ld   hl,23200;Vymazání
         ld   de,23201;předešlého
         ld   bc,96   ;zobrazení.
         ld   (hl),b
         ldir


		 xor a
		 ld hl,ACBhlas
		 ld (hl),a
		 
         ld   a,8     ;A teď nové
         ld   hl,23216;zobrazení.
         ld   de,23215;Kanál A.
         call ATR

         ld   a,10    ;Kanál C.
         ld   hl,23248
         ld   de,23247
         call ATR

         ld   a,9     ;Kanál B.
         ld   hl,23280
         ld   de,23279
         call ATR
		 
		 
		 ld a,(ACBhlas)
		 or a
		 jr z,.as1
		 ld hl,pocitadlo
		 xor a
		 ld (hl),a
		 jr .as2
.as1	 ld hl,pocitadlo
		 inc (hl)
.as2		 
		 
 		call keysetup
		pop bc
		pop de
		pop hl
		
		call	SQ_PLAY
		xor a
        ld a,(klavesa)
		cp 1
		jr z,akoneca
		cp 32
		jr z,.as3
		
		ld a,(pocitadlo)
		xor 100
		jr z,.as3
		ld a,(SQ_STATUS)
		bit 7,a
		jr z,.loop
.as3	ld a,32
		ld hl,endsong
		ld (hl),a		
akoneca	push af	

		call music_mute
		pop af
		jp navrat
		;jr	.loop


; Povodny player generovany SQ-Linkerom doplneny o GLOBVOL,
; tj. moznost globalneho utlmenia ~ © 2009 mborik/RM-TEAM.
; Upravene na prehravanie SQT suborov, ktorym je potrebne
; na zaciatku relokovat absolutne adresy, dalej bol doplneny
; stavovy bajt podobny aky ma moj PT3 player, cize je mozne
; song neopakovat, alebo fadeoutovat © 2020 mborik/SinDiKat.

; Stavovy bajt prehravaca:
; bit.0 nastav ak nechces ziaden looping
; bit.1 nastav ak chces, aby nastal postupny fadeout po par sekundach
; bit.7 sa nastavi vzdy, po odohrati vsetkych pozicii alebo po fadeoute
; ...v jednoduchosti:	2 - fadeout after loop
;			1 - no loop
;			0 - loop forever
SQ_STATUS:	db	2

SQ_INIT: 
		ld	de,(RELOC_BASE)		; nacitame prvy offset, z ktoreho
		ld	hl,BUFFER+10		; po odpocitani skutocnej adresy dat
		xor	a			; bez 10 bajtov ziskavame offset,
		sbc	hl,de			; ktorym musime relokovat vsetky
		push	hl			; absolutne ukazovatele v datach muziky
		ld	de,(RELOC_ENDPTR)
		add	hl,de
		pop	bc
RELOCATOR:	dec	hl
		ld	d,(hl)
		dec	hl
		ld	e,(hl)
		ex	de,hl
		add	hl,bc
		ex	de,hl
		ld	(hl),e
		inc	hl
		ld	(hl),d
		dec	hl
		ld	a,l
		sub	RELOC_BASE % 256
		jr	nz,RELOCATOR
 	    ld      (CHKFADEOUT+1),a
        ld      (GLOBVOL+1),a
		dec	a
		ld	(SQ_REST+1),a		; resetujem detektor konca skladby
		ld a,2
		ld (SQ_STATUS),a
		ld	a,8			; inicializacia prehravaca
		ld	(CHNZ1),a
		ld	(CHNZ2),a
		ld	(CHNZ3),a
		ld	bc,$0101
		call	SQ_REND1
CHN_INIT: 	ld	hl,(I_POSITIONS)
		ld	ix,CHNZ1
		call	SQ_I9
		call	SQ_I
		call	SQ_I
SQ_STOP:	ld	de,#073F		; AY registre 7-12 nastav na uplne ticho
CHN_INILOOP:	call	OUT1
		ld	e,0
		inc	d
		ld	a,d
		cp	12
		jr	nz,CHN_INILOOP
		ret

SQ_DEADEND:	inc	(hl)			; prehravanie skoncilo, SQ_PLAY je mrtve
		jr	SQ_STOP

SQ_PLAY:	ld	hl,SQ_SYS
		dec	(hl)
		jr	nz,CHKFADEOUT
SQ_PLAYSPEED:	ld	(hl),0
		inc	hl
		dec	(hl)
		ld	a,(hl)
		or	a
		call	z,SQ_REST
		cp	4
		call	c,SQ_I

SQ_PP		ld	ix,CHNZ1
		ld	c,36
		call	SQ_P
		ld	ix,CHNZ2
		ld	c,18
		call	SQ_P
		ld	ix,CHNZ3
		ld	c,9
		call	SQ_P

CHKFADEOUT:	ld	a,0			; kontrola, ci fadeoutujeme
		or	a
		jr	z,SQ_C
		ld	a,(SQ_STATUS)
		bit	7,a
		jr	nz,SQ_DEADEND

COUNTFADEOUT:	ld	a,0
		dec	a
		ld	(COUNTFADEOUT+1),a
		jr	nz,GLOBVOL
		ld	a,(GLOBVOL+1)
		inc	a
		cp	16
		jr	z,RESETFADEOUT
		ld	(GLOBVOL+1),a
DIVFADEOUT:	ld	a,0
		srl	a
		ld	l,a
		srl	l
		add	a,l
		jr	nz,DIVFADEOUT1
		inc	a
DIVFADEOUT1:	ld	(COUNTFADEOUT+1),a
		ld	(DIVFADEOUT+1),a

GLOBVOL:	ld	a,0			; global attenuation
		ld	(CHNZ1+11),a		; pre vsetky kanaly
		ld	(CHNZ2+11),a
		ld	(CHNZ3+11),a
		bit	3,a			; dosiahla hodnota attenuation
		jr	z,SQ_C			; cislo vacsie ako 8?
		ld	hl,CHNZ1		; tak je nutne upravit sq-flagy,
		ld	de,CHZL 		; aby sa prestali prehravat hw obalky
		ld	c,3			; vo vsetkych troch kanaloch
GLOBVOL1:	res	0,(hl)			; vynulovanim nulteho bytu
		add	hl,de
		dec	c
		jr	nz,GLOBVOL1

SQ_C:		xor	a			; vynuluje mixer register
		ld	l,a
		ld	h,a
		ld	(SQ_N+1),hl
		ld	ix,CHNZ1		; postupne prechadza kanalmi
		call	SQ_R
		call	SQ_R
		call	SQ_R

SQ_N:		ld	bc,0			; nastavenie mixer registra
		ld	a,b			; B = sumove generatory pre ABC
		rla				; C = tonove generatory pre ABC
		rla
		rla
		or	c
		cpl				; potom sa bity komplementuju
SQ_OFF: 	or	0
		ld	e,a
		ld	d,7			; a posielaju na register 7 AY
OUT1:		ld	bc,$FFFD
		out	(c),d
		ld	b,$BF
		out	(c),e
		ret

RESETFADEOUT:	ld	hl,-1
		ld	(SQ_SYS),hl
		ld	hl,SQ_STATUS
		set	7,(hl)
		xor	a
		db	1 ; ld bc,NN namiesto ld a,N

ENABLEFADE:	ld	a,48
FORCEFADE:	ld	hl,CHKFADEOUT+1
		inc	(hl)
		ld	(COUNTFADEOUT+1),a
		ld	(DIVFADEOUT+1),a
		cpl
		ld	(SQ_REST+1),a
		ret

SQ_I:		ld	hl,0
SQ_I1:		ld	ix,CHNZ1
SQ_I9:		ld	a,(hl)
		or	a
		jr	nz,SQ_I3
		ld	(SQ_REST+1),a		; oznac, ze pri najblizsom patterne
		ld	hl,(I_REPEAT)		; budeme testovat fadeout alebo noloop
SQ_I3:		ld	b,(hl)
		rl	b
		res	5,(ix+0)
		jr	nc,SQ_I4
		set	5,(ix+0)
SQ_I4:		inc	hl
		ld	a,(hl)
		and	15
		ld	(ix+26),a
		ld	a,(hl)
		and	240
		rra
		rra
		rra
		rra
		cp	9
		jr	c,ZBR
		sub	9
		cpl
ZBR:		ld	(ix+24),a
		inc	hl
		ld	(SQ_I+1),hl
		ld	l,b
		ld	h,0
		ld	de,(I_PATTERNS)
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	de
		ld	(ix+22),e
		ld	(ix+23),d
		ld	de,CHZL
		add	ix,de
		ld	(SQ_I1+2),ix
		ret

SQ_NOFADE:	call	RESETFADEOUT
		pop	af
		ret

SQ_REST:	ld	a,-1
		or	a
		jr	nz,SQ_REST0
		ld	a,(SQ_STATUS)
		rrca			; bit 0. nastaveny = koniec
		jr	c,SQ_NOFADE
		rrca			; bit 1. nastaveny = fadeout
		jr	nc,SQ_REST0
		call	ENABLEFADE
SQ_REST0:	ld	a,(CHNZ1+26)
		ld	(CHNZ1+11),a
		ld	a,(CHNZ2+26)
		ld	(CHNZ2+11),a
		ld	a,(CHNZ3+26)
		ld	(CHNZ3+11),a
		ld	hl,(CHNZ1+22)
		dec	hl
		ld	b,(hl)
		inc	hl
		ld	(CHNZ1+18),hl
		ld	hl,(CHNZ2+22)
		ld	(CHNZ2+18),hl
		ld	hl,(CHNZ3+22)
		ld	(CHNZ3+18),hl
		ld	hl,(CHNZ1+24)
		ld	(CHNZ1+20),hl
		ld	hl,(CHNZ2+24)
		ld	(CHNZ2+20),hl
		ld	hl,(CHNZ3+24)
		ld	(CHNZ3+20),hl
		ld	hl,(SQ_I+1)
		ld	c,(hl)
		inc	hl
		ld	(SQ_I+1),hl
		ld	hl,CHNZ1
		ld	(SQ_I1+2),hl
		ld	a,3
		ld	d,0
SQ_REST1:	res	4,(hl)
		bit	5,(hl)
		jr	z,SQ_REST2
		set	4,(hl)
SQ_REST2:	ld	e,21
		add	hl,de
		ld	(hl),d
		ld	e,CHZL-21
		add	hl,de
		dec	a
		jr	nz,SQ_REST1
SQ_REND1:	ld	(SQ_SYS),bc
		ld	a,c
SQ_REND2:	ld	(SQ_PLAYSPEED+1),a
		ld	a,b
		ret

SQ_P:		ld	a,(ix+21)
		or	a
		jr	z,Y01
		dec	(ix+21)
		bit	7,(ix+0)
		jr	nz,Y33
		ret

Y01:		ld	e,(ix+18)
		ld	d,(ix+19)
		set	6,(ix+0)
		res	7,(ix+0)
		ld	a,(de)
		inc	de
		bit	7,a
		jr	z,Y02

Y05:		ld	(ix+18),e
		ld	(ix+19),d
		ld	b,a
		bit	6,a
		jr	z,Y60

		dec	de
		ld	(ix+27),e
		ld	(ix+28),d
Y34:		and	31
		jp	SQ_SMP

Y60:		bit	5,a
		jr	nz,Y06

		and	15
		bit	4,b
		jr	z,Y07
		neg
Y07:		add	a,(ix+12)
		ld	(ix+12),a
Y33:		ld	e,(ix+27)
		ld	d,(ix+28)
		res	6,(ix+0)
		ld	a,(de)
		bit	7,a
		jr	nz,Y34
		inc	de
		jp	SMP_ORN

Y06:		and	15
		ld	(ix+21),a
		bit	4,b
		ret	z
		or	a
		jr	z,Y33
		set	7,(ix+0)
		jr	Y33

Y02:		cp	96
		jp	c,Y03
		sub	96
		cp	15
		jr	c,Y04

		ld	hl,SQ_OFF+1
		ld	b,a
		ld	a,(hl)
		or	c
		ld	(hl),a
		set	3,(ix+0)
		ld	a,b
		sub	15
		jp	z,Z26

Y04:		dec	a
		ex	de,hl
		ld	c,(hl)
		inc	hl
		bit	6,(ix+0)
		jr	z,Y69
		ld	(ix+18),l
		ld	(ix+19),h
		res	6,(ix+0)
Y69:		cp	8
		jr	c,Z38
		set	0,(ix+0)
		ld	l,c
		ld	e,a
		ld	d,13
		call	OUT1
		ld	d,11
		ld	e,l
		jp	OUT1

Z38:		cp	6			; channel volume set
		jr	nc,Z36
		bit	4,(ix+0)
		ret	z
		or	a
		jr	nz,Z31
		ld	a,c
SQ_V:		and	15
		ld	(ix+11),a
		ret

Z31:		dec	a			; channel volume slide
		jr	nz,Z32
		ld	a,c
		add	a,(ix+11)
		jr	SQ_V

Z32:		dec	a			; global volume set
		jr	nz,Z33
		ld	a,c
		ld	(CHNZ1+11),a
		ld	(CHNZ2+11),a
		ld	(CHNZ3+11),a
		ret

Z33:		dec	a			; global volume slide
		jr	nz,Z34
		ld	b,3
		ld	de,CHZL
		ld	hl,CHNZ1+11
Z33_2:		ld	a,(hl)
		add	a,c
		and	15
		ld	(hl),a
		add	hl,de
		djnz	Z33_2
		ret

Z34:		ld	hl,SQ_SYS		; speed set
		dec	a
		jr	nz,Z35
		ld	a,c
SQ_S:		and	31
		jr	nz,SQ_Z
		ld	a,32
SQ_Z:		ld	(hl),a
		jp	SQ_REND2

Z35:		ld	a,(hl)			; speed slide
		add	a,c
		jr	SQ_S

Z36:		sub	6
		ld	b,0
		ld	a,c
		ld	c,b
		jr	nz,Z37
		dec	b
		neg
Z37:		set	2,(ix+0)
		ld	(ix+13),c
		ld	(ix+14),c
		ld	(ix+15),a
		ld	(ix+16),b
		ret

Y03:		ld	(ix+12),a
		dec	de
		ld	(ix+27),e
		ld	(ix+28),d
		inc	de
		call	SMP_ORN
		bit	6,(ix+0)
		ret	z
Z26:		ld	(ix+18),e
		ld	(ix+19),d
		ret

SMP_ORN:	ld	a,(de)
		inc	de
		bit	7,a
		jr	z,SMP_ORN9
		ld	b,a
		rra
		and	31
		call	nz,SQ_SMP
		bit	6,b
		ret	z
		ld	a,(de)
		and	240
		rr	b
		rra
		rra
		rra
		srl	a
		call	nz,SQ_ORN
		ld	a,(de)
		inc	de
		and	15
		ret	z
SMP_ORN9:	jp	Y04

SQ_SMP: 	push	bc
		add	a,a
		ld	c,a
		ld	b,0
		ld	a,(ix+0)
		and	%11110000
		ld	(ix+0),a
		ld	hl,(I_SAMPLES)
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		push	ix
		pop	hl
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	bc
		inc	bc
		inc	hl
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ld	(hl),32
		inc	hl
		ld	(SQ_NXT+1),hl
		pop	bc
		ld	hl,SQ_OFF+1
		ld	a,(hl)
		or	c
		xor	c
		ld	(hl),a
		ret

SQ_ORN: 	add	a,a
		ld	c,a
		ld	b,0
		ld	hl,(I_ORNAMENTS)
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
SQ_NXT: 	ld	hl,0
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		inc	bc
		inc	bc
		ld	(hl),c
		inc	hl
		ld	(hl),b
		inc	hl
		ld	(hl),32
		set	1,(ix+0)
		ret

OUT2:		ld	hl,SQ_N+1		; podla carry nastavi v SQ_N
		rl	(hl)			; tonovy generator, sumovy off
		inc	hl			; potom vytiahne cislo kanalu
		rl	(hl)
		ld	a,(ix+17)
		add	a,8			; pripocita k nemu 8
		out	(c),a			; tj. volume registre AY
		ld	b,$BF
		out	(c),e			; a posle hodnotu v E
		jp	SQ_ZCH

SQ_R:		ld	l,(ix+3)
		ld	h,(ix+4)
		ld	bc,$FFFD
		ld	d,(ix+0)
		ld	e,0
		bit	3,d			; ak sa nema nic hrat, umlcat
		jr	nz,OUT2
		ld	a,(hl)
		and	15
		jp	nz,SQ_R1
		bit	0,d			; ak sa ma hrat obalka, nastav
		jr	z,SQ_R2
		ld	e,16
		jp	SQ_R2

SQ_R1:		sub	(ix+11) 		; od hlasitosti z sa odpocita
		jr	c,SQ_R2 		; global volume nastavenie
		ld	e,a

SQ_R2:		ld	a,(ix+17)		; vytiahne sa cislo kanalu
		add	a,8			; pripocita sa 8 cim sa dostnem
		out	(c),a			; na registre hlasitosi AY
		ld	b,$BF
		out	(c),e
		ld	a,(hl)
		inc	hl
		and	240			; vytiahujem sumove data
		rra
		rra
		rra
		ld	d,6
		ld	e,(hl)
		rl	e
		bit	5,(hl)			; zistujem, ci budeme sumiet
		jr	z,SQ_ZNN
		adc	a,0
		ld	b,$FF
		out	(c),d
		ld	b,$BF
		out	(c),a
SQ_ZNN:		ld	a,e
		rla
		ex	de,hl
		ld	hl,SQ_N+1		; nastavime v SQ_N stavy oboch
		rl	(hl)			; generatorov (sum/ton) pre ch.
		inc	hl
		rla
		rl	(hl)
		ex	de,hl
		ld	a,(hl)			; vypocitavanie frekvencie...
		and	31
		ld	d,a
		inc	hl
		ld	e,(hl)
		inc	hl
		push	de
		ld	d,0
		dec	(ix+5)
		jp	nz,FQ_2
		ld	l,(ix+1)
		ld	h,(ix+2)
		ld	a,(hl)
		inc	hl
		cp	32
		ld	c,(hl)
		inc	hl
		jr	nz,FQ_1
		set	3,(ix+0)
		res	1,(ix+0)
FQ_1:		ld	b,a
		add	a,a
		add	a,b
		ld	e,a
		add	hl,de
		ld	(ix+5),c
FQ_2:		ld	(ix+3),l
		ld	(ix+4),h
		ld	a,(ix+12)
		bit	1,(ix+0)
		jr	z,FQ_5
		ld	l,(ix+8)
		ld	h,(ix+9)
		add	a,(hl)
		inc	hl
		dec	(ix+10)
		jp	nz,FQ_4
		ex	af,af'
		ld	l,(ix+6)
		ld	h,(ix+7)
		ld	a,(hl)
		inc	hl
		cp	32
		ld	e,b
		jr	z,FQ_3
		ld	c,(hl)
		ld	e,a
FQ_3:		inc	hl
		add	hl,de
		ld	(ix+10),c
		ex	af,af'
FQ_4:		ld	(ix+8),l
		ld	(ix+9),h
FQ_5:		add	a,(ix+20)
		cp	45
		jr	nc,FQ_6
		add	a,a
		ld	e,a
		ld	hl,FRQ2
		add	hl,de
		ld	d,(hl)
		inc	hl
		jp	FQ_7

FQ_6:		ld	hl,FRQ1-45
		ld	e,a
		add	hl,de
FQ_7:		ld	e,(hl)
		ex	de,hl
		pop	de			; ...frekvenciu mame,
		bit	4,d			; bude este fine-tuning?
		res	4,d
		jr	z,FQ_9
		add	hl,de
		db	1 ; ld bc,NN namiesto sbc hl,de
FQ_9:		sbc	hl,de
		bit	2,(ix+0)
		jr	z,OUT9
		ld	c,(ix+13)
		ld	b,(ix+14)
		add	hl,bc
		ex	de,hl
		ld	l,(ix+15)
		ld	h,(ix+16)
		add	hl,bc
		ld	(ix+13),l
		ld	(ix+14),h
		ex	de,hl

OUT9:		ld	a,(ix+17)		; vytiahi cislo kanalu
		add	a,a			; vynasob dvoma
		ld	bc,$FFFD		; a naprogramuj freq tonu
		out	(c),a
		ld	b,$BF
		out	(c),l
		inc	a
		ld	b,$FF
		out	(c),a
		ld	b,$BF
		out	(c),h
SQ_ZCH: 	ld	de,CHZL 		; prejdi s IX na dalsi kanal
		add	ix,de
		ret

FRQ2:		db	13,93,12,156
		db	11,231,11,60
		db	10,155,10,2,9,115
		db	8,235,8,107,7,242
		db	7,128,7,20,6,174
		db	6,78,5,244,5,158
		db	5,79,5,1,4,185
		db	4,117,4,53,3,249
		db	3,192,3,138,3,87
		db	3,39,2,250,2,207
		db	2,167,2,129,2,93
		db	2,59,2,27,1,252
		db	1,224,1,197,1,172
		db	1,148,1,125,1,104
		db	1,83,1,64
		db	1,46,1,29,1,13

FRQ1:		db	254,240,226,214
		db	202,190,180,170
		db	160,151,143,135
		db	127,120,113,107
		db	101,95,90,85,80
		db	76,71,67,64,60,57
		db	53,50,48,45,42,40
		db	38,36,34,32,30,28
		db	27,25,24,22,21,20
		db	19,18,17,16,15,14

CHNZ1:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	2,0,0,0,0,0
CHNZ2:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	1,0,0,0,0,0
CHNZ3:	db	0
		dw	0,0,0,0,0,0,0,0
		dw	0,0,0,0,0,0

SQ_SYS: 	dw	#0101

CHZL:		equ	CHNZ2-CHNZ1	; offset medzi kanalmi


;universal pt2 and pt3 player for zx spectrum and msx
;(c)2004-2005 s.v.bulba <vorobey@mail.khstu.ru>
;http://bulba.at.kz
 
;release number
release equ "0"
 
;conditional assembly
;1) version of rout (zx or msx standards)
zx equ 1
msx equ 0
;2) current position counter at (start+11)
curposcounter equ 1
;3) allow channels allocation bits at (start+10)
acbbac equ 1
;4) allow loop checking and disabling
loopchecker equ 1
;5) insert official identificator
id equ 1

ATR
         ld   bc,AY   ;Přečtení hlasitosti
         out  (c),a   ;z AY
         in   a,(c)
		 push hl
		 push af
		 ld hl,ACBhlas
		 or (hl)
		 ld (hl),a
		 pop af
		 pop hl
		;	out (254),a
         cp   16      ;Test překročení
         jr  c,ATR3      ;maximální hlast.
		 ld a,15
ATR3     or   a
         ret  z
         ld   b,a     ;Uschování hlast.
         or a
         rra          ;Výpočet barvy
         jr   c,ATR2  ;atributu.
         set  6,a     ;Nastavení jasu.
ATR2
         ld   (hl),a  ;Vykreslení
         ld   (de),a  ;atributové čáry.
         inc  hl      ;Opakuje se
         dec  de      ;tolikrát, kolik
         djnz ATR2    ;je hlasitost.
         ret

AY       equ  65533	


keysetup
		call key.check

		ld (klavesa),a
		cp "a"
		call z,setAY
		cp "y"
		call z,setYM
		cp "b"
		call z,ABC
		cp "c"
		call z,ACB
		call showsetup
		ret
 
playmusic
			ld hl,20640
			ld de,by1
			ld b,8*3
ppp         push de
			push bc
			push hl
			ld b,32
ppp0		ld a,(de)			
			ld (hl),a
			inc hl
			djnz ppp0
			pop hl
			pop bc
			pop de
			
			call downhl
			inc de
			djnz ppp



			; ld de,18464+15
			; ld hl,txtcurrentfile
			; call print
			
			; ld de,18432+15
			; ld hl,txtallfiles
			; call print

			
		; ld hl,(apos_all)
		; inc hl
		; ld de,18464+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256
		
		; ld hl,(ALLFILES)
		; ld de,18432+15+11
		; ld (NUMPOS+1),de
		; call DEC16
		; defw 20*256



		xor a
		ld (endsong),a
		ld a,(typ)
		cp "3"
		jr nz,pp0
		ld a,%00000001
		ld (VTPL.SETUP),a
        ld a,1 ;pt2,abc,looped
	    jr pp1
pp0
		ld a,%00000011
		ld (VTPL.SETUP),a
		ld a,2
pp1	   
		
		push af
		cp 2
		jr nz,pt3
		ld hl,modstart + 101
		jr pt2
pt3		ld hl,modstart + 30
pt2		
		call infosong
		ld hl,txtbuff
		xor a
		ld (txtbuff+80),a

		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,txtbuff
		ld de,tmp
		ld bc,maxline-4
		ldir
		ld hl,tmp
        ld de,20512+1
		call print

		ld hl,tmp
		ld de,tmp+1
		xor a
		ld (hl),a
		ld bc,48
		ldir
		
		ld hl,txtbuff + maxline-4
		ld de,tmp
		ld bc,maxline
		ldir
		ld hl,tmp
        ld de,20512+1 + 32
		call print
		pop af

		ld hl,(delka_souboru)
DELKA		
		push hl
		ld a,(hl)
		cp "S"
		jr nz,normalni_pt3
		dec hl
		ld a,(hl)
		cp "T"
		jr nz,normalni_pt3
		dec hl
		ld a,(hl)
		cp "2"
		jr nz,normalni_pt3
		dec hl
		ld a,(hl)
		cp "0"
		jr nz,normalni_pt3
		ld a,(VTPL.SETUP)
		or %00010000
		ld (VTPL.SETUP),a
		jr pokrvdet


normalni_pt3
		;ld (VTPL.SETUP),a
		jr pokrvdet
pokrvdet
		pop hl
		or a
		ld de,11
		sbc hl,de

		ld a,(hl)
		inc hl
		ld h,(hl)
		ld l,a
		
		ld de,songdata
		add hl,de
		ex de,hl		;v DE máme druhou skladbu
		
		;CALL music_init

		ld hl,songdata
		call VTPL.INIT

		EI
LOOP	HALT
		

		push hl
		push de 
		push bc
		
		 ld   hl,23200;Vymazání
         ld   de,23201;předešlého
         ld   bc,96   ;zobrazení.
         ld   (hl),b
         ldir
		 
		 xor a
		 ld hl,ACBhlas
		 ld (hl),a

         ld   a,8     ;A teď nové
         ld   hl,23216;zobrazení.
         ld   de,23215;Kanál A.
         call ATR

         ld   a,10    ;Kanál C.
         ld   hl,23248
         ld   de,23247
         call ATR

         ld   a,9     ;Kanál B.
         ld   hl,23280
         ld   de,23279
         call ATR
		
		 ld a,(ACBhlas)
		 or a
		 jr z,.s1
		 ld hl,pocitadlo
		 xor a
		 ld (hl),a
		 jr .s2
.s1		 ld hl,pocitadlo
		 inc (hl)
.s2		 
	
		call keysetup
		pop bc
		pop de
		pop hl
		
		CALL VTPL.PLAY
		ld a,(klavesa)
		cp 1
		jr z,koneca
		cp 32
		jr z,.s3
		ld a,(pocitadlo)
		xor 100
		jr z,.s3


		ld a,(music_setup)
		bit 7,a
		JR z,LOOP
.s3		ld a,32
		ld hl,endsong
		ld (hl),a		
koneca	push af	

		call VTPL.MUTE
		pop af
		jp navrat


ACB
		NEXTREG2A 08
		or #20
		nextreg 08,a

		ret
ABC
		NEXTREG2A 08
		and 0xDF
		nextreg 08,a
		
		ret
		
setAY
		NEXTREG2A 06
		and #FC
		or #1
		nextreg 06,a
		ret
		
setYM	
		NEXTREG2A 06
		and #FC
		nextreg 06,a
		ret

KLAVESA
klavesa defb 0
tona    equ 0
tonb    equ 2
tonc    equ 4
noise   equ 6
mixer   equ 7
ampla   equ 8
amplb   equ 9
amplc   equ 10
env     equ 11
envtp   equ 13
 
;       struct  chp
;reset group
chp_psinor      equ 0
chp_psinsm      equ 1
chp_cramsl      equ 2
chp_crnssl      equ 3
chp_crensl      equ 4
chp_tslcnt      equ 5
chp_crtnsl      equ 6
chp_tnacc       equ 8
chp_conoff      equ 10
;reset group
 
chp_onoffd      equ 11
 
;ix for ptdecod here (+12)
chp_offond      equ 12
chp_ornptr      equ 13
chp_samptr      equ 15
chp_nntskp      equ 17
chp_note        equ 18
chp_sltont      equ 19
chp_env_en      equ 20
chp_flags       equ 21
 ;enabled - 0,simplegliss - 2
chp_tnsldl      equ 22
chp_tslstp      equ 23
chp_tndelt      equ 25
chp_ntskcn      equ 27
chp_volume      equ 28
;       ends
chp     equ 29
 
;entry and other points
;start initialize playing of module at mdladdr
;start+3 initialization with module address in hl
;start+5 play one quark
;start+8 mute
;start+10 setup and status flags
;start+11 current position value (byte) (optional)
;first 12 values of tone tables (packed)
 

music_init:	ld	hl,MDLADDR
music_init0:	push	hl
		ld	bc,#40			; look for "by" identifier
		add	hl,bc
		ld	a,(hl)
		cp	'y'
		dec	hl
		ld	a,(hl)
		pop	hl
		jr	z,pt3fmt_init
		cp	'b'
		jr	z,pt3fmt_init
		ld	a,(hl)			; and first byte of PT2 is delay
		cp	c				; compared to PT3 text identifier
		jp	c,pt2fmt_init

pt3fmt_init	call	setmodaddr
		push	hl
		ld	de,100
		add	hl,de
		ld	a,(hl)
		ld	(delay),a
		push	hl
		pop	ix
		add	hl,de
		ld	(CrPsPtr+1),hl
		ld	e,(ix+2)
		add	hl,de
		inc	hl
		ld	(LPosPtr+1),hl
		pop	de
		ld	l,(ix+3)
		ld	h,(ix+4)
		add	hl,de
		ld	(PatsPtr+1),hl
		ld	hl,#A9
		add	hl,de
		ld	(OrnPtrs+1),hl
		ld	hl,#69
		add	hl,de
		ld	(SamPtrs+1),hl

		ld	a,(ix-87)		; extract version number
		sub	'0'
		jr	c,l20
		cp	10
		jr	c,l21
l20:		ld	a,6
l21:		ld	(Version+1),a
		push	af
		cp	4
		ld	a,(ix-1) ; tone table number
		rla
		and	7
		push	af

		ld	hl,#18 + ((smp_-SamCnv-2) << 8)	; jr smp_
		ld	(SamCnv),hl
		ld	a,#BA				; cp d
		ld	(OrnCP),a
		ld	(SamCP),a
		ld	a,#7B				; ld a,e
		ld	(OrnLD),a
		ld	(SamLD),a
		ld	a,#87				; add a,a
		ld	(SamClc2),a
		ld	hl,0
		ld	de,PT3_EmptySmpOrn
		ld	a,pt3pattdc-ptdecode-2
		jr	ptx_initcommon

pt2fmt_init:	ld	(delay),a
		push	hl
		push	hl
		push	hl
		inc	hl
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	(SamPtrs+1),hl
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		pop	hl
		and	a
		sbc	hl,de
		call	setmodaddr
		pop	hl
		ld	de,67
		add	hl,de
		ld	(OrnPtrs+1),hl
		ld	e,32
		add	hl,de
		ld	c,(hl)
		inc	hl
		ld	b,(hl)
		ld	e,30
		add	hl,de
		ld	(CrPsPtr+1),hl
		ld	e,a
		inc	hl

		add	hl,de
		ld	(LPosPtr+1),hl
		pop	hl
		add	hl,bc
		ld	(PatsPtr+1),hl
		ld	a,5
		ld	(Version+1),a
		push	af
		ld	a,2
		push	af
		ld	hl,$51CB			; bit 2,c
		ld	(SamCnv),hl
		ld	a,$BB				; cp e
		ld	(OrnCP),a
		ld	(SamCP),a
		ld	a,$7A				; ld a,d
		ld	(OrnLD),a
		ld	(SamLD),a
		ld	a,$80				; add a,b
		ld	(SamClc2),a
		ld	hl,$8687			; add a,a,(hl)
		ld	de,PT2_EmptySmpOrn
		ld	a,pt2pattdc-ptdecode-2

ptx_initcommon:	ld	(PsCalc),hl
		ld	(ptdecode+1),a
		push	de

; note table data depacker
		ld	de,T_PACK
		ld	bc,T1_+(2*49)-1
tp_0:		ld	a,(de)
		inc	de
		cp	15*2
		jr	nc,tp_1
		ld	h,a
		ld	a,(de)
		ld	l,a
		inc	de
		jr	tp_2
tp_1:		push	de
		ld	d,0
		ld	e,a
		add	hl,de
		add	hl,de
		pop	de
tp_2:		ld	a,h
		ld	(bc),a
		dec	bc
		ld	a,l
		ld	(bc),a
		dec	bc
		sub	low (#F8*2)
		jr	nz,tp_0

		ld	(GlobalAttn+1),a
		ld	(chkfadeout+1),a
		ld	hl,music_setup
		res	7,(hl)

		ld	hl,VARS
		ld	(hl),a
		ld	de,VARS+1
		ld	bc,VAR0END-VARS-1
		ldir
		inc	hl
		ld	(AdInPtA+1),hl		; ptr to zero
		inc	a
		ld	(DelyCnt),a
		ld	hl,#F001	; H - CHP.Volume, L - CHP.NtSkCn
		ld	(ChanA+CHP.NtSkCn),hl
		ld	(ChanB+CHP.NtSkCn),hl
		ld	(ChanC+CHP.NtSkCn),hl
		pop	hl
		ld	(ChanA+CHP.OrnPtr),hl	; ornament 0 is "0,1,0"
		ld	(ChanB+CHP.OrnPtr),hl	; in all versions from
		ld	(ChanC+CHP.OrnPtr),hl	; 3.xx to 3.6x and VTII
		pop	af

;NoteTableCreator (c) Ivan Roshin
;A - NoteTableNumber*2+VersionForNoteTable
;	xx1b - 3.xx..3.4r
;	xx0b - 3.4x..3.6x - VortexTracker II

		ld	hl,nt_data
		push	de
		ld	d,b
		add	a,a
		ld	e,a
		add	hl,de
		ld	e,(hl)
		inc	hl
		srl	e
		sbc	a,a
		and	#A7	; #00 (NOP) or #A7 (AND A)
		ld	(l3),a
		ex	de,hl
		pop	bc	; bc = T1_
		add	hl,bc
		ld	a,(de)
		add	a,low T_
		ld	c,a
		adc	a,high T_
		sub	c
		ld	b,a
		push	bc
		ld	de,NT_
		push	de
		ld	b,12
l1:		push	bc
		ld	c,(hl)
		inc	hl
		push	hl
		ld	b,(hl)
		push	de
		ex	de,hl
		ld	de,#17
		ld	xh,8
l2:		srl	b
		rr	c
l3:		add	hl,de	; will be replaced by AND A or NOP apparently
		ld	a,c
		adc	a,d
		ld	(hl),a
		inc	hl
		ld	a,b
		adc	a,d
		ld	(hl),a
		add	hl,de
		dec	xh
		jr	nz,l2

		pop	de
		inc	de
		inc	de
		pop	hl
		inc	hl
		pop	bc
		djnz	l1

		pop	hl
		pop	de
		ld	a,e
		cp	low TCOLD_1
		jr	nz,corr_1
		ld	a,#FD
		ld	(NT_+#002E),a
corr_1:		ld	a,(de)
		and	a
		jr	z,tc_exit
		rra
		push	af
		add	a,a
		ld	c,a
		add	hl,bc
		pop	af
		jr	nc,corr_2
		dec	(hl)
		dec	(hl)
corr_2:		inc	(hl)
		and	a
		sbc	hl,bc
		inc	de
		jr	corr_1
tc_exit:	pop	af

; VolTableCreator (c) Ivan Roshin
; A - VersionForVolumeTable
;	(0..4 - v3.xx .. v3.4x
;	(5..  - v2.x, v3.5x .. v3.6x (VT2) .. v3.7)

		cp	5
		ld	hl,17
		ld	d,h
		ld	e,h
		ld	a,#17
		jr	nc,m1
		dec	l
		ld	e,l
		xor	a
m1:		ld	(m2),a
		ld	ix,VT_+16
		ld	c,16
initloop2:	push	hl
		add	hl,de
		ex	de,hl
		sbc	hl,hl
initloop1:	ld	a,l
m2:		ld	a,l
		ld	a,h
		adc	a,0
		ld	(ix+0),a
		inc	ix
		add	hl,de
		inc	c
		ld	a,c
		and	15
		jr	nz,initloop1
		pop	hl
		ld	a,e
		cp	#77
		jr	nz,m3
		inc	e
m3:		ld	a,c
		and	a
		jr	nz,initloop2
		jp	rout_a0

setmodaddr:	ld	(modaddr+1),hl
		ld	(mdaddr1+1),hl
		ld	(mdaddr2+1),hl
		ret

; PT2 pattern decoder ---------------------------------------------------------
pd2_sam		call	setsam
		jr	pd2_loop

pd2_eoff	ld	(ix-12+CHP.Env_En),a
		jr	pd2_loop

pd2_env		ld	(ix-12+CHP.Env_En),16
		ld	(AYREGS+AR_EnvTp),a
		ld	a,(bc)
		inc	bc
		ld	l,a
		ld	a,(bc)
		inc	bc
		ld	h,a
		ld	(EnvBase),hl
		jr	pd2_loop

pd2_orn		call	setorn
		jr	pd2_loop

pd2_skip	inc	a
		ld	(ix-12+CHP.NNtSkp),a
		jr	pd2_loop

pd2_vol		rrca
		rrca
		rrca
		rrca
		ld	(ix-12+CHP.Volume),a
		jr	pd2_loop

pd2_del		call	c_delay
		jr	pd2_loop

pd2_glis	set	2,(ix-12+CHP.Flags)
		inc	a
		ld	(ix-12+CHP.TnSlDl),a
		ld	(ix-12+CHP.TSlCnt),a
		ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.TSlStp),a
		add	a,a
		sbc	a,a
		ld	(ix-12+CHP.TSlStp+1),a
		scf
		jr	pd2_loop2

pd2_port:	res	2,(ix-12+CHP.Flags)
		ld	a,(bc)
		inc	bc
		inc	bc		; ignoring precalc delta to right sound
		inc	bc
		scf
		jr	pd2_loop2

pd2_stop:	ld	(ix-12+CHP.TSlCnt),a
		jr	pd2_loop

pt2pattdc:	and	a
pd2_loop2:	ex	af,af'
pd2_loop:	ld	a,(bc)
		inc	bc
		add	a,$20
		jr	z,pd2_rel
		jr	c,pd2_sam
		add	a,96
		jr	c,pd2_note
		inc	a
		jr	z,pd2_eoff
		add	a,15
		jp	z,pd_fin
		jr	c,pd2_env
		add	a,$10
		jr	c,pd2_orn
		add	a,$40
		jr	c,pd2_skip
		add	a,$10
		jr	c,pd2_vol
		inc	a
		jr	z,pd2_del
		inc	a
		jr	z,pd2_glis
		inc	a
		jr	z,pd2_port
		inc	a
		jr	z,pd2_stop
		ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.CrNsSl),a
		jr	pd2_loop

pd2_rel:	ld	(ix-12+CHP.Flags),a
		jr	pd2_exit

pd2_note:	ld	l,a
		ld	a,(ix-12+CHP.Note)
		ld	(PrNote+1),a
		ld	(ix-12+CHP.Note),l
		xor	a
		ld	(ix-12+CHP.TSlCnt),a
		set	0,(ix-12+CHP.Flags)
		ex	af,af'
		jr	nc,pd2_noglis
		bit	2,(ix-12+CHP.Flags)
		jr	nz,pd2_noport
		ld	(LoStep+1),a
		add	a,a
		sbc	a,a
		ex	af,af'
		ld	h,a
		ld	l,a
		inc	a
		call	setport
pd2_noport:	ld	(ix-12+CHP.TSlCnt),1
pd2_noglis:	xor	a

pd2_exit:	ld	(ix-12+CHP.PsInSm),a
		ld	(ix-12+CHP.PsInOr),a
		ld	(ix-12+CHP.CrTnSl),a
		ld	(ix-12+CHP.CrTnSl+1),a
		jp	pd_fin


ptdecode:	jr	pt3pattdc		; patter decoder fork

; PT3 pattern decoder ---------------------------------------------------------
PD_OrSm:	ld	(ix-12+CHP.Env_En),0
		call	setorn
pd_sam_:	ld	a,(bc)
		inc	bc
		rrca
pd_sam:		call	setsam
		jr	pd_loop

pd_vol:		rrca
		rrca
		rrca
		rrca
		ld	(ix-12+CHP.Volume),a
		jr	pd_lp2

pd_eoff:	ld	(ix-12+CHP.Env_En),a
		ld	(ix-12+CHP.PsInOr),a
		jr	pd_lp2

pd_SorE:	dec	a
		jr	nz,pd_env
		ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.NNtSkp),a
		jr	pd_lp2

pd_env:		call	setenv
		jr	pd_lp2

pd_orn:		call	setorn
		jr	pd_loop

pd_esam:	ld	(ix-12+CHP.Env_En),a
		ld	(ix-12+CHP.PsInOr),a
		call	nz,setenv
		jr	pd_sam_

pt3pattdc:	ld	a,(ix-12+CHP.Note)
		ld	(PrNote+1),a
		ld	l,(ix-12+CHP.CrTnSl)
		ld	h,(ix-12+CHP.CrTnSl+1)
		ld	(PrSlide+1),hl

pd_loop:	ld	de,#2010
pd_lp2:		ld	a,(bc)
		inc	bc
		add	a,e
		jr	c,PD_OrSm
		add	a,d
		jr	z,pd_fin
		jr	c,pd_sam
		add	a,e
		jr	z,pd_rel
		jr	c,pd_vol
		add	a,e
		jr	z,pd_eoff
		jr	c,pd_SorE
		add	a,#60
		jr	c,pd_note
		add	a,e
		jr	c,pd_orn
		add	a,d
		jr	c,pd_nois
		add	a,e
		jr	c,pd_esam
		add	a,a
		ld	e,a
		ld	hl,spccoms-#20E0
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		push	de
		jr	pd_loop

pd_nois:	ld	(Ns_base),a
		jr	pd_lp2

pd_rel:		res	0,(ix-12+CHP.Flags)
		jr	pd_res

pd_note:	ld	(ix-12+CHP.Note),a
		set	0,(ix-12+CHP.Flags)
		xor	a
pd_res:		ld	(pdsp_+1),sp
		ld	sp,ix
		ld	h,a
		ld	l,a
		push	hl
		push	hl
		push	hl
		push	hl
		push	hl
		push	hl
pdsp_:		ld	sp,#3131
pd_fin:		ld	a,(ix-12+CHP.NNtSkp)
		ld	(ix-12+CHP.NtSkCn),a
		ret

c_portm:	ld	a,(bc)
		inc	bc
		inc	bc	; skip precalculated tone delta
		inc	bc	; (because cannot be right after pt3 compilation)
		ex	af,af'
		ld	a,(bc)	; signed tone step
		inc	bc
		ld	(LoStep+1),a
		ld	a,(bc)
		inc	bc
		and	a
		ex	af,af'
		ld	l,(ix-12+CHP.CrTnSl)
		ld	h,(ix-12+CHP.CrTnSl+1)

; Set portamento variables
; A - Delay; A' - Hi(Step); ZF' - (A'=0); HL - CrTnSl
setport		res	2,(ix-12+CHP.Flags)
		ld	(ix-12+CHP.TnSlDl),a
		ld	(ix-12+CHP.TSlCnt),a
		push	hl
		ld	de,NT_
		ld	a,(ix-12+CHP.Note)
		ld	(ix-12+CHP.SlToNt),a
		add	a,a
		ld	l,a
		ld	h,0
		add	hl,de
		ld	a,(hl)
		inc	hl
		ld	h,(hl)
		ld	l,a
		push	hl
PrNote:		ld	a,#3E
		ld	(ix-12+CHP.Note),a
		add	a,a
		ld	l,a
		ld	h,0
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		pop	hl
		sbc	hl,de
		ld	(ix-12+CHP.TnDelt),l
		ld	(ix-12+CHP.TnDelt+1),h
		pop	de
Version:	ld	a,#3E
		cp	6
		jr	c,LoStep	; old 3xxx for PT v3.5-
PrSlide:	ld	de,#1111
		ld	(ix-12+CHP.CrTnSl),e
		ld	(ix-12+CHP.CrTnSl+1),d
LoStep:		ld	a,0		; signed tone step
		ex	af,af'
		jr	z,nosig
		ex	de,hl
nosig:		sbc	hl,de
		jp	p,set_stp
		cpl
		ex	af,af'
		neg
		ex	af,af'
set_stp:	ld	(ix-12+CHP.TSlStp+1),a
		ex	af,af'
		ld	(ix-12+CHP.TSlStp),a
		ld	(ix-12+CHP.COnOff),0
		ret

c_gliss:	set	2,(ix-12+CHP.Flags)
		ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.TnSlDl),a
		and	a
		jr	nz,gl36
		ld	a,(Version+1) ;AlCo PT3.7+
		cp	7
		sbc	a,a
		inc	a
gl36:		ld	(ix-12+CHP.TSlCnt),a
		ld	a,(bc)
		inc	bc
		ex	af,af'
		ld	a,(bc)
		inc	bc
		jr	set_stp

c_smpos:	ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.PsInSm),a
		ret

c_orpos:	ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.PsInOr),a
		ret

c_vibrt:	ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.OnOffD),a
		ld	(ix-12+CHP.COnOff),a
		ld	a,(bc)
		inc	bc
		ld	(ix-12+CHP.OffOnD),a
		xor	a
		ld	(ix-12+CHP.TSlCnt),a
		ld	(ix-12+CHP.CrTnSl),a
		ld	(ix-12+CHP.CrTnSl+1),a
		ret

c_engls:	ld	a,(bc)
		inc	bc
		ld	(Env_Del+1),a
		ld	(CurEDel),a
		ld	a,(bc)
		inc	bc
		ld	l,a
		ld	a,(bc)
		inc	bc
		ld	h,a
		ld	(ESldAdd+1),hl
		ret

c_delay:	ld	a,(bc)
		inc	bc
		ld	(delay),a
		ld	(DelyCnt),a ; bugfix by Lee_dC
		ret

setenv:		ld	(ix-12+CHP.Env_En),e
		ld	(AYREGS+AR_EnvTp),a
		ld	a,(bc)
		inc	bc
		ld	h,a
		ld	a,(bc)
		inc	bc
		ld	l,a
		ld	(EnvBase),hl
		xor	a
		ld	(ix-12+CHP.PsInOr),a
		ld	(CurEDel),a
		ld	h,a
		ld	l,a
		ld	(CurESld),hl
c_nop:		ret

setorn:		add	a,a
		ld	e,a
		ld	d,0
		ld	(ix-12+CHP.PsInOr),d
OrnPtrs:	ld	hl,#2121
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
mdaddr2:	ld	hl,#2121
		add	hl,de
		ld	(ix-12+CHP.OrnPtr),l
		ld	(ix-12+CHP.OrnPtr+1),h
		ret

setsam:		add	a,a
		ld	e,a
		ld	d,0
SamPtrs:	ld	hl,#2121
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
mdaddr1:	ld	hl,#2121
		add	hl,de
		ld	(ix-12+CHP.SamPtr),l
		ld	(ix-12+CHP.SamPtr+1),h
		ret

; all 16 addresses filled to protect from broken pt3 modules
spccoms:	dw	c_nop
		dw	c_gliss
		dw	c_portm
		dw	c_smpos
		dw	c_orpos
		dw	c_vibrt
		dw	c_nop
		dw	c_nop
		dw	c_engls
		dw	c_delay
		dw	c_nop
		dw	c_nop
		dw	c_nop
		dw	c_nop
		dw	c_nop
		dw	c_nop

chregs:		xor	a
		ld	(Ampl),a
		bit	0,(ix+CHP.Flags)
		push	hl
		jp	z,ch_exit
		ld	(csp_+1),sp
		ld	l,(ix+CHP.OrnPtr)
		ld	h,(ix+CHP.OrnPtr+1)
		ld	sp,hl
		pop	de
		ld	h,a
		ld	a,(ix+CHP.PsInOr)
		ld	l,a
		add	hl,sp
		inc	a			; PT2	| PT3
OrnCP:		cp	d			; cp e	| cp d
		jr	c,ch_orps
OrnLD:		ld	a,e			; a,d	| a,e
ch_orps:	ld	(ix+CHP.PsInOr),a
		ld	a,(ix+CHP.Note)
		add	a,(hl)
		jp	p,ch_ntp
		xor	a
ch_ntp:		cp	96
		jr	c,ch_nok
		ld	a,95
ch_nok:		add	a,a
		ex	af,af'
		ld	l,(ix+CHP.SamPtr)
		ld	h,(ix+CHP.SamPtr+1)
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,(ix+CHP.PsInSm)
		ld	b,a
		add	a,a			; PT2	| PT3
SamClc2:	add	a,a			; a,b	| a,a
		ld	l,a
		add	hl,sp
		ld	sp,hl
		ld	a,b
		inc	a
SamCP:		cp	d			; cp e	| cp d
		jr	c,ch_smps
SamLD:		ld	a,e			; a,d	| a,e
ch_smps:	ld	(ix+CHP.PsInSm),a
		pop	bc
		pop	hl
						; PT2     | PT3
SamCnv:		jr	smp_			; bit 2,c | jr smp_

		ld	h,b			; Convert PT2 sample to PT3
		jr	nz,SamCnv1
		ex	de,hl
		and	a
		sbc	hl,hl
		sbc	hl,de
SamCnv1:	ld	d,c
		rr	c
		sbc	a,a
		cpl
		and	#3E
		rr	c
		rr	b
		and	c
		ld	c,a
		ld	a,b
		rra
		rra
		rr	d
		rra
		and	#9F
		ld	b,a

smp_:		ld	e,(ix+CHP.TnAcc)
		ld	d,(ix+CHP.TnAcc+1)
		add	hl,de
		bit	6,b
		jr	z,ch_noac
		ld	(ix+CHP.TnAcc),l
		ld	(ix+CHP.TnAcc+1),h
ch_noac:	ex	de,hl
		ex	af,af'
		ld	l,a
		ld	h,0
		ld	sp,NT_
		add	hl,sp
		ld	sp,hl
		pop	hl
		add	hl,de
		ld	e,(ix+CHP.CrTnSl)
		ld	d,(ix+CHP.CrTnSl+1)
		add	hl,de
csp_:		ld	sp,#3131
		ex	(sp),hl
		xor	a
		or	(ix+CHP.TSlCnt)
		jr	z,ch_amp
		dec	(ix+CHP.TSlCnt)
		jr	nz,ch_amp
		ld	a,(ix+CHP.TnSlDl)
		ld	(ix+CHP.TSlCnt),a
		ld	l,(ix+CHP.TSlStp)
		ld	h,(ix+CHP.TSlStp+1)
		ld	a,h
		add	hl,de
		ld	(ix+CHP.CrTnSl),l
		ld	(ix+CHP.CrTnSl+1),h
		bit	2,(ix+CHP.Flags)
		jr	nz,ch_amp
		ld	e,(ix+CHP.TnDelt)
		ld	d,(ix+CHP.TnDelt+1)
		and	a
		jr	z,ch_stpp
		ex	de,hl
ch_stpp:	sbc	hl,de
		jp	m,ch_amp
		ld	a,(ix+CHP.SlToNt)
		ld	(ix+CHP.Note),a
		xor	a
		ld	(ix+CHP.TSlCnt),a
		ld	(ix+CHP.CrTnSl),a
		ld	(ix+CHP.CrTnSl+1),a
ch_amp:		ld	a,(ix+CHP.CrAmSl)
		bit	7,c
		jr	z,ch_noam
		bit	6,c
		jr	z,ch_amin
		cp	15
		jr	z,ch_noam
		inc	a
		jr	ch_svam
ch_amin:	cp	-15
		jr	z,ch_noam
		dec	a
ch_svam:	ld	(ix+CHP.CrAmSl),a
ch_noam:	ld	l,a
		ld	a,b
		and	15
		add	a,l
		jp	p,ch_apos
		xor	a
ch_apos:	cp	16
		jr	c,ch_vol
		ld	a,15
ch_vol:		or	(ix+CHP.Volume)
		ld	l,a
		ld	h,0
		ld	de,VT_
		add	hl,de
		ld	e,(hl)
GlobalAttn:	ld	a,0		; GLOBAL ATTENUATION
		cp	8		; if value of attenuation is higher
		jr	c,ch_globvol	; then this value we will stop
		set	0,c		; also envelopes (bit.0 in flags)...
ch_globvol:	sub	e
		jr	c,ch_env
		xor	a
ch_env:		neg
		bit	0,c
		jr	nz,ch_noen
		or	(ix+CHP.Env_En)
ch_noen:	ld	(Ampl),a
		bit	7,b
		ld	a,c
		jr	z,no_ensl
		rla
		rla
		sra	a
		sra	a
		sra	a
		add	a,(ix+CHP.CrEnSl)
		bit	5,b
		jr	z,no_enac
		ld	(ix+CHP.CrEnSl),a
no_enac:	ld	hl,AddToEn+1
		add	a,(hl)
		ld	(hl),a
		jr	ch_mix

no_ensl:	rra
		add	a,(ix+CHP.CrNsSl)
		ld	(AddToNs),a
		bit	5,b
		jr	z,ch_mix
		ld	(ix+CHP.CrNsSl),a
ch_mix:		ld	a,b
		rra
		and	#48
ch_exit:	ld	hl,AYREGS+AR_Mixer
		or	(hl)
		rrca
		ld	(hl),a
		pop	hl
		xor	a
		or	(ix+CHP.COnOff)
		ret	z
		dec	(ix+CHP.COnOff)
		ret	nz
		xor	(ix+CHP.Flags)
		ld	(ix+CHP.Flags),a
		rra
		ld	a,(ix+CHP.OnOffD)
		jr	c,ch_ondl
		ld	a,(ix+CHP.OffOnD)
ch_ondl:	ld	(ix+CHP.COnOff),a
		ret

;------------------------------------------------------------------------------
music_play:	xor	a
		ld	(AddToEn+1),a
		ld	(AYREGS+AR_Mixer),a
		dec	a
		ld	(AYREGS+AR_EnvTp),a
		ld	hl,DelyCnt
		dec	(hl)
		jp	nz,pl2
		call	chkfadeout
		ld	hl,ChanA+CHP.NtSkCn
		dec	(hl)
		jr	nz,pl1b
AdInPtA:	ld	bc,#0101
		ld	a,(bc)
		and	a
		jr	nz,pl1a
		ld	d,a
		ld	(Ns_base),a
CrPsPtr:	ld	hl,#2121
		inc	hl
		ld	a,(hl)
		inc	a
		jr	nz,plnlp
		call	checklp
LPosPtr:	ld	hl,#2121
		ld	a,(hl)
		inc	a
plnlp:		ld	(CrPsPtr+1),hl
		dec	a			; PT2		PT3
PsCalc:		nop				; add a,a	nop
		nop				; add a,(hl)	nop
		add	a,a
		ld	e,a
		rl	d
PatsPtr:	ld	hl,#2121
		add	hl,de
modaddr:	ld	de,#1111
		ld	(psp_+1),sp
		ld	sp,hl
		pop	hl
		add	hl,de
		ld	b,h
		ld	c,l
		pop	hl
		add	hl,de
		ld	(AdInPtB+1),hl
		pop	hl
		add	hl,de
		ld	(AdInPtC+1),hl
psp_:		ld	sp,#3131
pl1a:		ld	ix,ChanA+12
		call	ptdecode
		ld	(AdInPtA+1),bc

pl1b:		ld	hl,ChanB+CHP.NtSkCn
		dec	(hl)
		jr	nz,pl1c
		ld	ix,ChanB+12
AdInPtB:	ld	bc,#0101
		call	ptdecode
		ld	(AdInPtB+1),bc

pl1c:		ld	hl,ChanC+CHP.NtSkCn
		dec	(hl)
		jr	nz,pl1d
		ld	ix,ChanC+12
AdInPtC:	ld	bc,#0101
		call	ptdecode
		ld	(AdInPtC+1),bc
delay:		equ	$+1
pl1d:		ld	a,#3E
		ld	(DelyCnt),a
pl2:		ld	ix,ChanA
		ld	hl,(AYREGS+AR_TonA)
		call	chregs
		ld	(AYREGS+AR_TonA),hl

		ld	a,(Ampl)
		ld	(AYREGS+AR_AmplA),a
		ld	ix,ChanB
		ld	hl,(AYREGS+AR_TonB)
		call	chregs
		ld	(AYREGS+AR_TonB),hl

		ld	a,(Ampl)
		ld	(AYREGS+AR_AmplB),a
		ld	ix,ChanC
		ld	hl,(AYREGS+AR_TonC)
		call	chregs
		ld	(AYREGS+AR_TonC),hl

		ld	hl,(Ns_base) ; read together with AddToNs
		ld	a,h
		add	a,l
		ld	(AYREGS+AR_Noise),a

AddToEn:	ld	a,#3E
		ld	e,a
		add	a,a
		sbc	a,a
		ld	d,a
		ld	hl,(EnvBase)
		add	hl,de
		ld	de,(CurESld)
		add	hl,de
		ld	(AYREGS+AR_Env),hl

		xor	a
		ld	hl,CurEDel
		or	(hl)
		jr	z,rout_a0
		dec	(hl)
		jr	nz,rout
Env_Del:	ld	a,#3E
		ld	(hl),a
ESldAdd:	ld	hl,#2121
		add	hl,de
		ld	(CurESld),hl

rout:		xor	a
rout_a0:	ld	de,#FFBF
		ld	bc,#FFFD
		ld	hl,AYREGS
lout:		out	(c),a
		ld	b,e
		outi
		ld	b,d
		inc	a
		cp	#0D
		jr	nz,lout
		out	(c),a
		ld	a,(hl)
		and	a
		ret	m
		ld	b,e
		out	(c),a
		ret

checklp:	ld	hl,music_setup
		bit	1,(hl)
		jr	nz,enablefade
		bit	0,(hl)
		ret	z

noloop:		set	7,(hl)
deadend:	pop	hl
		ld	hl,DelyCnt
		inc	(hl)
		ld	hl,ChanA+CHP.NtSkCn
		inc	(hl)

music_mute:	xor	a
		ld	h,a
		ld	l,a
		ld	(AYREGS+AR_AmplA),a
		ld	(AYREGS+AR_AmplB),hl
		jr	rout_a0

enablefade:	ld	a,48
forcefade:	ld	hl,chkfadeout+1
		inc	(hl)
		ld	hl,(CrPsPtr+1)
		ld	(resetfadeout+1),hl
		ld	(resetfadeout1+1),bc
		ld	(countfadeout+1),a
		ld	(divfadeout+1),a

chkfadeout:	ld	a,0
		or	a
		ret	z
		ld	a,(music_setup)
		rlca
		jr	c,deadend
countfadeout:	ld	a,#3E
		dec	a
		ld	(countfadeout+1),a
		ret	nz
		ld	a,(GlobalAttn+1)
		inc	a
		cp	16
		jr	z,resetfadeout
		ld	(GlobalAttn+1),a
divfadeout:	ld	a,#3E
		srl	a
		ld	l,a
		srl	l
		add	a,l
		jr	nz,divfadeout1
		inc	a
divfadeout1:	ld	(countfadeout+1),a
		ld	(divfadeout+1),a
		ret

resetfadeout:	ld	hl,#2121
		ld	(CrPsPtr+1),hl
resetfadeout1:	ld	hl,#2121
		ld	(AdInPtA+1),hl
		xor	a
		ld	(ChanA+CHP.NtSkCn),a
		ld	hl,music_setup
		jr	noloop


; bit.0 if you want to play without looping
; bit.1 if you play looped but fadeout after few seconds
; bit.7 is set each time when loop is passed
; ...or simplified:	2 - fadeout after loop
;			1 - no loop
;			0 - loop forever
music_setup:	db	2


;------------------------------------------------------------------------------
; note table data
nt_data:	db	(T_NEW_0-T1_)*2
		db	TCNEW_0-T_
		db	(T_OLD_0-T1_)*2+1
		db	TCOLD_0-T_
		db	(T_NEW_1-T1_)*2+1
		db	TCNEW_1-T_
		db	(T_OLD_1-T1_)*2+1
		db	TCOLD_1-T_
		db	(T_NEW_2-T1_)*2
		db	TCNEW_2-T_
		db	(T_OLD_2-T1_)*2
		db	TCOLD_2-T_
		db	(T_NEW_3-T1_)*2
		db	TCNEW_3-T_
		db	(T_OLD_3-T1_)*2
		db	TCOLD_3-T_
T_

TCOLD_0		db	#00+1,#04+1,#08+1,#0A+1,#0C+1,#0E+1,#12+1,#14+1
		db	#18+1,#24+1,#3C+1,0
TCOLD_1		db	#5C+1,0
TCOLD_2		db	#30+1,#36+1,#4C+1,#52+1,#5E+1,#70+1,#82,#8C,#9C
		db	#9E,#A0,#A6,#A8,#AA,#AC,#AE,#AE,0
TCNEW_3		db	#56+1
TCOLD_3		db	#1E+1,#22+1,#24+1,#28+1,#2C+1,#2E+1,#32+1,#BE+1,0
TCNEW_0		db	#1C+1,#20+1,#22+1,#26+1,#2A+1,#2C+1,#30+1,#54+1
		db	#BC+1,#BE+1,0
TCNEW_1		equ	TCOLD_1
TCNEW_2		db	#1A+1,#20+1,#24+1,#28+1,#2A+1,#3A+1,#4C+1,#5E+1
		db	#BA+1,#BC+1,#BE+1,0

PT3_EmptySmpOrn	equ	$-1
		db	1,0 ;#90 ; delete #90 if you don't need default sample

PT2_EmptySmpOrn	equ	VT_+31 ; 1,0,0 sequence

; first 12 values of tone tables (packed)
T_PACK		db	high (#06EC*2), low (#06EC*2)
		db	#0755-#06EC
		db	#07C5-#0755
		db	#083B-#07C5
		db	#08B8-#083B
		db	#093D-#08B8
		db	#09CA-#093D
		db	#0A5F-#09CA
		db	#0AFC-#0A5F
		db	#0BA4-#0AFC
		db	#0C55-#0BA4
		db	#0D10-#0C55
		db	high (#066D*2), low (#066D*2)
		db	#06CF-#066D
		db	#0737-#06CF
		db	#07A4-#0737
		db	#0819-#07A4
		db	#0894-#0819
		db	#0917-#0894
		db	#09A1-#0917
		db	#0A33-#09A1
		db	#0ACF-#0A33
		db	#0B73-#0ACF
		db	#0C22-#0B73
		db	#0CDA-#0C22
		db	high (#0704*2), low (#0704*2)
		db	#076E-#0704
		db	#07E0-#076E
		db	#0858-#07E0
		db	#08D6-#0858
		db	#095C-#08D6
		db	#09EC-#095C
		db	#0A82-#09EC
		db	#0B22-#0A82
		db	#0BCC-#0B22
		db	#0C80-#0BCC
		db	#0D3E-#0C80
		db	high (#07E0*2), low (#07E0*2)
		db	#0858-#07E0
		db	#08E0-#0858
		db	#0960-#08E0
		db	#09F0-#0960
		db	#0A88-#09F0
		db	#0B28-#0A88
		db	#0BD8-#0B28
		db	#0C80-#0BD8
		db	#0D60-#0C80
		db	#0E10-#0D60
		db	#0EF8-#0E10

; channel data offsets
	struct CHP
PsInOr		byte
PsInSm		byte
CrAmSl		byte
CrNsSl		byte
CrEnSl		byte
TSlCnt		byte
CrTnSl		word
TnAcc		word
COnOff		byte
OnOffD		byte
OffOnD		byte	; IX for PTDECOD here (+12)
OrnPtr		word
SamPtr		word
NNtSkp		byte
Note		byte
SlToNt		byte
Env_En		byte
Flags		byte
TnSlDl		byte	; Enabled - 0, SimpleGliss - 2
TSlStp		word
TnDelt		word
NtSkCn		byte
Volume		byte
	ends

AR_TonA		equ	0	; word 1
AR_TonB		equ	2	; word 1
AR_TonC		equ	4	; word 1
AR_Noise	equ	6	; byte 1
AR_Mixer	equ	7	; byte 1
AR_AmplA	equ	8	; byte 1
AR_AmplB	equ	9	; byte 1
AR_AmplC	equ	10	; byte 1
AR_Env		equ	11	; word 1
AR_EnvTp	equ	13	; byte 1
AR_Size		equ	14


;------------------------------------------------------------------------------
; variables are 541 bytes long
VARS
ChanA		ds	29
ChanB		ds	29
ChanC		ds	29


; GlobalVars
DelyCnt		db	0
CurESld		dw	0
CurEDel		db	0
Ns_base		db	0
AddToNs		db	0

AYREGS		equ	$
Ampl		equ	AYREGS+AR_AmplC

VT_		ds	256		; Volume Table
NT_		ds	192		; Note Table

EnvBase		equ	VT_+14
T1_		equ	VT_+16		; Tone tables data depacked here
T_OLD_1		equ	T1_
T_OLD_2		equ	T_OLD_1+24
T_OLD_3		equ	T_OLD_2+24
T_OLD_0		equ	T_OLD_3+2
T_NEW_0		equ	T_OLD_0
T_NEW_1		equ	T_OLD_1
T_NEW_2		equ	T_NEW_0+24
T_NEW_3		equ	T_OLD_3

VAR0END		equ	T1_		; init zeroes from VARS to VAR0END-1
VARSEND		equ	$



		module stc

music_init:	ld	hl,songdata
music_init0:	ld	a,(hl)
		ld	(speed),a
		ld	(module_addr+1),hl
		inc	hl
		call	add_offset
		ld	a,(de)
		inc	de
		inc	a
		ld	(song_length),a
		ld	(positions),de
		call	add_offset
		ld	(ornaments),de
		push	de
		call	add_offset
		ld	(patterns),de
		ld	hl,HEADLEN
		call	module_addr
		ex	de,hl
		ld	(samples),hl
		ld	hl,empty_pattern
		ld	(chn1_pattptr),hl
		inc	hl
		ld	de,CHN1+1
		ld	bc,VARSLEN
		ld	(hl),b
		ldir
		pop	hl
		ld	bc,ORNALEN
		xor	a
		call	scan_until
		ld	(apply_volenv+1),a
		ld	(chkfadeout+1),a
		dec	a
		ld	(CHN1+CHP.SmpCnt),a
		ld	(CHN2+CHP.SmpCnt),a
		ld	(CHN3+CHP.SmpCnt),a
		ld	a,1
		ld	(tempo_cnt),a
		inc	hl
		ld	(CHN1+CHP.OrnPtr),hl
		ld	(CHN2+CHP.OrnPtr),hl
		ld	(CHN3+CHP.OrnPtr),hl
		ld	hl,music_setup
		res	7,(hl)
		jp	music_mute

scan_until:	cp	(hl)
		ret	z
		add	hl,bc
		jr	scan_until

add_offset:	ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl
module_addr:	ld	bc,0
		add	hl,bc
		ex	de,hl
		ret

new_position:	ld	a,(current_pos)
		ld	c,a
		ld	hl,song_length
		cp	(hl)
		jr	c,.continue
		ld	hl,music_setup
		bit	1,(hl)
		call	nz,enablefade
		bit	0,(hl)
		jp	nz,noloop
		xor	a
		ld	c,a
.continue:	inc	a
		ld	(current_pos),a
		ld	l,c
		ld	h,0
		add	hl,hl
		ld	de,(positions)
		add	hl,de
		ld	c,(hl)
		inc	hl
		ld	a,(hl)
		ld	(get_frequency+1),a
		ld	a,c
		ld	hl,(patterns)
		ld	bc,PATTLEN
		call	scan_until
		inc	hl
		call	add_offset
		ld	(chn1_pattptr),de
		call	add_offset
		ld	(chn2_pattptr),de
		call	add_offset
		ld	(chn3_pattptr),de
		ret

adv_patt_step:	dec	(ix+CHP.PatStpCnt)
		ret	p
		ld	a,(ix+CHP.PatStep)
		ld	(ix+CHP.PatStpCnt),a
		ret

;------------------------------------------------------------------------------
music_play:	ld	a,(tempo_cnt)
		dec	a
		ld	(tempo_cnt),a
		jr	nz,playback
		ld	a,(speed)
		ld	(tempo_cnt),a

.chn1:		ld	ix,CHN1
		call	adv_patt_step
		jp	p,.chn2
		ld	hl,(chn1_pattptr)
		ld	a,(hl)
		inc	a
		call	z,new_position
		ld	hl,(chn1_pattptr)
		call	read_pattern
		ld	(chn1_pattptr),hl

.chn2:		ld	ix,CHN2
		call	adv_patt_step
		jp	p,.chn3
		ld	hl,(chn2_pattptr)
		call	read_pattern
		ld	(chn2_pattptr),hl

.chn3:		ld	ix,CHN3
		call	adv_patt_step
		jp	p,playback
		ld	hl,(chn3_pattptr)
		call	read_pattern
		ld	(chn3_pattptr),hl

;------------------------------------------------------------------------------
playback:	call	chkfadeout
		ld	ix,CHN1
		call	adv_sample
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,c
		or	b
		rrca
		ld	(AYREGS+AR.Mixer),a
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.chn2
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonA),hl

.chn2:		ld	hl,AYREGS+AR.AmplA
		call	apply_volenv
		ld	ix,CHN2
		call	adv_sample
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.chn3
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,(AYREGS+AR.Mixer)
		or	c
		or	b
		ld	(AYREGS+AR.Mixer),a
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonB),hl

.chn3:		ld	hl,AYREGS+AR.AmplB
		call	apply_volenv
		ld	ix,CHN3
		call	adv_sample
		ld	a,(ix+CHP.SmpCnt)
		inc	a
		jr	z,.finish
		ld	a,c
		ld	(sample_index+1),a
		call	read_sample
		ld	a,(AYREGS+AR.Mixer)
		rlc	c
		rlc	b
		or	b
		or	c
		ld	(AYREGS+AR.Mixer),a
		call	set_noise
		call	get_tone
		ld	(AYREGS+AR.TonC),hl

.finish:	ld	hl,AYREGS+AR.AmplC
		call	apply_volenv

outay:		ld	hl,AYREGS+AR.EnvSh
		xor	a
		or	(hl)
		ld	a,AR.EnvSh
		jr	nz,.fillregs
		sub	3
		dec	hl
		dec	hl
		dec	hl
.fillregs:	ld	bc,#FFFD
.loop:		out	(c),a
		res	6,b
		outd
		set	6,b
		dec	a
		ret	m
		jr	.loop

music_mute:	ld	hl,AYREGS
		ld	de,AYREGS+1
		ld	bc,AR.Mixer
		ld	(hl),b
		ldir
		dec	(hl)
		ld	l,low AYREGS
		ld	c,AR.EnvSh-AR.Mixer-1
		ldir
		ex	hl,de
		ld	a,AR.EnvSh
		jr	outay.fillregs

c_note:		ld	(ix+CHP.Note),a
		ld	(ix+CHP.SmpIndex),0
		ld	(ix+CHP.SmpCnt),#20
c_empty:	inc	hl
		ret

c_sample:	sub	#60
		push	hl
		ld	bc,SAMPLEN
		ld	hl,(samples)
		call	scan_until
		inc	hl
		ld	(ix+CHP.SmpPtr),l
		ld	(ix+CHP.SmpPtr+1),h
		pop	hl
		inc	hl
		jr	read_pattern

c_rest:		inc	hl
c_norept:	ld	(ix+CHP.SmpCnt),-1
		ret

read_pattern:	ld	a,(hl)
		cp	#60		; note
		jr	c,c_note
		cp	#70		; sample
		jr	c,c_sample
		cp	#80		; ornament
		jr	c,c_ornament
		jr	z,c_rest	; rest
		cp	#81		; empty
		jr	z,c_empty
		cp	#82		; ornament off
		jr	z,c_ornoff
		cp	#8F		; envelope
		jr	c,c_envelope
		sub	#A1		; speed
		ld	(ix+CHP.PatStpCnt),a
		ld	(ix+CHP.PatStep),a
		inc	hl
		jr	read_pattern

c_ornoff:	xor	a
		db	1 ; ld bc,* (skip next instruction)
c_ornament:	sub	#70
c_ornament0:	push	hl
		ld	bc,ORNALEN
		ld	hl,(ornaments)
		call	scan_until
		inc	hl
		ld	(ix+CHP.OrnPtr),l
		ld	(ix+CHP.OrnPtr+1),h
		ld	(ix+CHP.EnvState),0
		pop	hl
		inc	hl
		jr	read_pattern

c_envelope:	sub	#80
		ld	(AYREGS+AR.EnvSh),a
		inc	hl
		ld	a,(hl)
		inc	hl
		ld	(AYREGS+AR.Env),a
		ld	(ix+CHP.EnvState),1
		push	hl
		xor	a
		ld	bc,ORNALEN
		ld	hl,(ornaments)
		call	scan_until
		inc	hl
		ld	(ix+CHP.OrnPtr),l
		ld	(ix+CHP.OrnPtr+1),h
		pop	hl
		jr	read_pattern

adv_sample:	ld	a,(ix+CHP.SmpCnt)
		inc	a
		ret	z
		dec	a
		dec	a
		ld	(ix+CHP.SmpCnt),a
		push	af
		ld	a,(ix+CHP.SmpIndex)
		ld	c,a
		inc	a
		and	#1F
		ld	(ix+CHP.SmpIndex),a
		pop	af
		ret	nz
		ld	e,(ix+CHP.SmpPtr)
		ld	d,(ix+CHP.SmpPtr+1)
		ld	hl,#60	; offset to sample metadata
		add	hl,de
		ld	a,(hl)
		dec	a
		jp	m,c_norept
		ld	c,a	; repeat sample
		inc	a
		and	#1F
		ld	(ix+CHP.SmpIndex),a
		inc	hl
		ld	a,(hl)
		inc	a
		ld	(ix+CHP.SmpCnt),a
		ret

read_sample:	ld	d,0
		ld	e,a
		add	a,a
		add	a,e
		ld	e,a
		ld	l,(ix+CHP.SmpPtr)
		ld	h,(ix+CHP.SmpPtr+1)
		add	hl,de
		inc	hl
		ld	a,(hl)
		bit	7,a
		ld	c,#10
		jr	nz,.no_noise
		ld	c,d
.no_noise:	bit	6,a
		ld	b,2
		jr	nz,.no_tone
		ld	b,d
.no_tone:	inc	hl
		ld	e,(hl)
		dec	hl
		dec	hl
		ld	d,(hl)
		ld	l,a
		and	#1F
		ld	h,a
		ld	a,d
		push	af
		and	#F0
		rrca
		rrca
		rrca
		rrca
		ld	d,a
		pop	af
		and	#0F
		bit	5,l
		ld	l,a
		ret	z
		set	4,d
		ret

set_noise:	ld	a,c
		or	a
		ret	nz
		ld	a,h
		ld	(AYREGS+AR.Noise),a
		ret

apply_volenv:	ld	e,0		; GLOBAL ATTENUATION
		sub	e
		jr	nc,.set
		xor	a
.set		ld	(hl),a		; if value of attenuation is higher
		bit	3,a		; then 8 we will stop also envelopes...
		ret	z

		ld	a,(ix+CHP.SmpCnt)
		inc	a
		ret	z
		ld	a,(ix+CHP.EnvState)
		or	a
		ret	z
		dec	a
		jr	nz,.resetshape
		ld	(ix+CHP.EnvState),2
		jr	.setenv

.resetshape:	xor	a
		ld	(AYREGS+AR.EnvSh),a
.setenv:	set	4,(hl)
		ret

get_tone:	ld	a,l
		push	af
		push	de
		ld	l,(ix+CHP.OrnPtr)
		ld	h,(ix+CHP.OrnPtr+1)
sample_index:	ld	de,0
		add	hl,de
		ld	a,(ix+CHP.Note)
		add	a,(hl)

get_frequency:	add	a,0
		add	a,a
		ld	e,a
		ld	d,0
		ld	hl,freqtable
		add	hl,de
		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		ex	de,hl
		pop	de
		pop	af
		bit	4,d
		jr	z,.negative
		res	4,d
		add	hl,de
		ret

.negative:	and	a
		sbc	hl,de
		ret

;------------------------------------------------------------------------------
resetfadeout:	ld	hl,music_setup
noloop:		set	7,(hl)
		call	music_mute
deadend:	pop	hl
		xor	a
		ld	(tempo_cnt),a
		dec	a
		ld	(CHN1+CHP.SmpCnt),a
		ld	(CHN2+CHP.SmpCnt),a
		ld	(CHN3+CHP.SmpCnt),a
		ld	(chkfadeout+1),a
		ret

enablefade:	ld	a,48
forcefade:	ld	(chkfadeout+1),a
		ld	(countfadeout+1),a
		ld	(divfadeout+1),a

chkfadeout:	ld	a,0
		or	a
		ret	z
		ld	a,(music_setup)
		rlca
		jr	c,deadend
countfadeout:	ld	a,0
		dec	a
		ld	(countfadeout+1),a
		ret	nz
		ld	a,(apply_volenv+1)
		inc	a
		cp	16
		jr	z,resetfadeout
		ld	(apply_volenv+1),a
divfadeout:	ld	a,0
		srl	a
		ld	e,a
		srl	e
		add	a,e
		jr	nz,divfadeout1
		inc	a
divfadeout1:	ld	(countfadeout+1),a
		ld	(divfadeout+1),a
		ret

;------------------------------------------------------------------------------
positions:	dw	0
ornaments:	dw	0
patterns:	dw	0
samples:	dw	0
speed:		db	0
tempo_cnt:	db	0
song_length:	db	0

chn1_pattptr:	dw	empty_pattern
chn2_pattptr:	dw	empty_pattern
chn3_pattptr:	dw	empty_pattern
empty_pattern:	db	#FF

VARS
CHN1:		ds	10
CHN2:		ds	10
CHN3:		ds	10

current_pos:	db	0
@VARSLEN = $ - VARS

AYREGS:		ds	14

;------------------------------------------------------------------------------
freqtable:	dw	#0EF8, #0E10, #0D60, #0C80, #0BD8, #0B28, #0A88, #09F0
		dw	#0960, #08E0, #0858, #07E0, #077C, #0708, #06B0, #0640
		dw	#05EC, #0594, #0544, #04F8, #04B0, #0470, #042C, #03F0
		dw	#03BE, #0384, #0358, #0320, #02F6, #02CA, #02A2, #027C
		dw	#0258, #0238, #0216, #01F8, #01DF, #01C2, #01AC, #0190
		dw	#017B, #0165, #0151, #013E, #012C, #011C, #010B, #00FC
		dw	#00EF, #00E1, #00D6, #00C8, #00BD, #00B2, #00A8, #009F
		dw	#0096, #008E, #0085, #007E, #0077, #0070, #006B, #0064
		dw	#005E, #0059, #0054, #004F, #004B, #0047, #0042, #003F
		dw	#003B, #0038, #0035, #0032, #002F, #002C, #002A, #0027
		dw	#0025, #0023, #0021, #001F, #001D, #001C, #001A, #0019
		dw	#0017, #0016, #0015, #0013, #0012, #0011, #0010, #000F


;------------------------------------------------------------------------------
	struct AR	; AY Registers structure
TonA		word	; 0
TonB		word	; 2
TonC		word	; 4
Noise		byte	; 6
Mixer		byte	; 7
AmplA		byte	; 8
AmplB		byte	; 9
AmplC		byte	; 10
Env		word	; 11
EnvSh		byte	; 13
	ends

	struct CHP	; channel parameters structure
EnvState	byte
PatStep		byte
SmpIndex	byte
Note		byte
PatStpCnt	byte
SmpPtr		word
OrnPtr		word
SmpCnt		byte
	ends

@HEADLEN = 27	; file header length
@SAMPLEN = 99	; single sample length
@ORNALEN = 33	; single ornament length
@PATTLEN = 7	; single pattern record length



			endmodule
			

;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;Universal PT2'n'PT3 Turbo Sound player for ZX Spectrum
;(c)2004-2007 S.V.Bulba <vorobey@mail.khstu.ru>
;Specially for AlCo
;http://bulba.untergrund.net/ (http://bulba.at.kz/)
	MODULE VTPL
;Release number
Release EQU "0"
;Conditional assembly
;1) Current position counters at (Vars1+0) and (Vars2+0)
CurPosCounter=0
;2) Allow channels allocation bits at (START+10)
ACBBAC=0
;3) Allow loop checking and disabling
LoopChecker=1
;4) Insert official identificator
Id=0
;5) Set IY for correct return to ZX Basic
Basic=1

;Features
;--------
;-Can be compiled at any address (i.e. no need rounding ORG
; address).
;-Variables (VARS) can be located at any address (not only after
; code block).
;-INIT subprogram checks PT3-module version and rightly
; generates both note and volume tables outside of code block
; (in VARS).
;-Two portamento (spc. command 3xxx) algorithms (depending of
; PT3 module version).
;-New 1.XX and 2.XX special command behaviour (only for PT v3.7
; and higher).
;-Any Tempo value are accepted (including Tempo=1 and Tempo=2).
;-TS modes: 2xPT3, 2xPT2 and PT v3.7 TS standard.
;-Fully compatible with Ay_Emul PT3 and PT2 players codes.
;-See also notes at the end of this source code.

;Limitations
;-----------
;-Can run in RAM only (self-modified code is used).
;-PT2 position list must be end by #FF marker only.

;Warning!!! PLAY subprogram can crash if no module are loaded
;into RAM or INIT subprogram was not called before.

;Call MUTE or INIT one more time to mute sound after stopping
;playing 

;Test codes (commented)
;	LD A,32 ;SinglePT3(TS if TSPT3.7),ABC,Looped
;	LD (START+10),A
;	LD HL,#8000 ;Mod1
;	LD DE,#A000 ;Mod2 (optional)
;	CALL START+3
;	EI
;_LP	HALT
;	CALL START+5
;	XOR A
;	IN A,(#FE)
;	CPL
;	AND 15
;	JR Z,_LP
;	JR START+8

TonA	EQU 0
TonB	EQU 2
TonC	EQU 4
Noise	EQU 6
Mixer	EQU 7
AmplA	EQU 8
AmplB	EQU 9
AmplC	EQU 10
Env	EQU 11
EnvTp	EQU 13

;Entry and other points
;START initialize playing of modules at MDLADDR (single module)
;START+3 initialization with module address in HL and DE (TS)
;START+5 play one quark
;START+8 mute
;START+10 setup and status flags

START:
	LD HL,songdata ;DE - address of 2nd module for TS
	JR INIT
	JP PLAY
	JR MUTE
SETUP	DB %00010000 ;set bit0, if you want to play without looping
	     ;(optional);
	     ;set bit1 for PT2 and reset for PT3 before
	     ;calling INIT;
	     ;bits2-3: %00-ABC, %01-ACB, %10-BAC (optional);
	     ;bits4-5: %00-no TS, %01-2 modules TS, %10-
	     ;autodetect PT3 TS-format by AlCo (PT 3.7+);
	     ;Remark: old PT3 TS-format by AlCo (PT 3.6) is not
	     ;documented and must be converted to new standard.
	     ;bit6 is set each time, when loop point of 2nd TS
	     ;module is passed (optional).
	     ;bit7 is set each time, when loop point of 1st TS
	     ;or of single module is passed (optional).

;Identifier
	IF Id
	DB "=UniPT2/PT3/TS-Player r.",Release,"="
	ENDIF

	IF LoopChecker
CHECKLP	LD HL,SETUP
	BIT 0,(IY-100+VRS.ModNum)
	JR Z,CHL1
	SET 6,(HL)
	JR CHL2
CHL1	SET 7,(HL)
CHL2	BIT 0,(HL)
	RET Z
	POP HL
	INC (IY-100+VRS.DelyCnt)
	INC (IY-100+VRS.ChanA+CHP.NtSkCn)
	XOR A
	LD (IY-100+VRS.AYREGS+AmplA),A
	LD (IY-100+VRS.AYREGS+AmplB),A
	LD (IY-100+VRS.AYREGS+AmplC),A
	RET
	ENDIF

MUTE: XOR A
	LD H,A
	LD L,A
	LD (VARS1+VRS.AYREGS+AmplA),A
	LD (VARS1+VRS.AYREGS+AmplB),HL
	LD (VARS2+VRS.AYREGS+AmplA),A
	LD (VARS2+VRS.AYREGS+AmplB),HL
	JP ROUT

INIT:
;HL - AddressOfModule
;DE - AddresOf2ndModule
	PUSH DE
	PUSH HL
	LD HL,VARS
	LD (HL),0
	LD DE,VARS+1
	LD BC,VAR0END-VARS-1
	LDIR
	INC HL
	LD (VARS1+VRS.AdInPtA),HL ;ptr to zero
	LD (VARS2+VRS.AdInPtA),HL

	POP HL
	LD IY,VARS1+100
	LD A,(START+10)
	AND 2
	JP NZ,I_PT2

	CALL INITPT3
	LD HL,(e_-SamCnv-2)*256+#18
	LD (SamCnv),HL
	LD A,#BA
	LD (OrnCP),A
	LD (SamCP),A
	LD A,#7B
	LD (OrnLD),A
	LD (SamLD),A
	LD A,#87
	LD (SamClc2),A
	POP HL
	;Use version and ton table of 1st module
	LD A,(IX+13-100) ;EXTRACT VERSION NUMBER
	SUB #30
	JR C,L20
	CP 10
	JR C,L21
L20	LD A,6
L21	LD (Version),A
	PUSH AF ;VolTable version
	CP 4
	LD A,(IX+99-100) ;TONE TABLE NUMBER
	RLA
	AND 7
	PUSH AF ;NoteTable number

	LD IY,VARS2+100
	LD A,(START+10)
	AND 48
	JR Z,NOTS
	CP 16
	JR Z,TwoPT3s
	LD A,(Version)
	CP 7
	JR C,NOTS
	LD A,(IX+98-100) ;ALCO TS MARKER
	CP #20
	JR Z,NOTS
	LD HL,VARS1
	LD DE,VARS2
	LD BC,VRS
	LDIR
	SET 1,(IY-100+VRS.ModNum)
	LD C,A
	ADD A,A
	ADD A,C
	SUB 2
	LD (TSSub),A
	JR AlCoTS_
TwoPT3s	CALL INITPT3
AlCoTS_	LD A,1
	LD (is_ts),A
	SET 0,(IY-100+VRS.ModNum)

NOTS	LD BC,PT3PD
	LD HL,0
	LD DE,PT3EMPTYORN
	JR INITCOMMON

I_PT2	CALL INITPT2
	LD HL,#51CB
	LD (SamCnv),HL
	LD A,#BB
	LD (OrnCP),A
	LD (SamCP),A
	LD A,#7A
	LD (OrnLD),A
	LD (SamLD),A
	LD A,#80
	LD (SamClc2),A
	POP HL
	LD A,5
	LD (Version),A
	PUSH AF
	LD A,2
	PUSH AF

	LD A,(START+10)
	AND 48
	JR Z,NOTS2

	LD IY,VARS2+100
	LD A,1
	LD (is_ts),A
	SET 0,(IY-100+VRS.ModNum)
	CALL INITPT2

NOTS2	LD BC,PT2PD
	LD HL,#8687
	LD DE,PT2EMPTYORN

INITCOMMON

	IF Basic
	LD IY,#5C3A
	ENDIF

	LD (PTDEC),BC
	LD (PsCalc),HL
	PUSH DE

;note table data depacker
;(c) Ivan Roshin
	LD DE,T_PACK
	LD BC,T1_+(2*49)-1
TP_0	LD A,(DE)
	INC DE
	CP 15*2
	JR NC,TP_1
	LD H,A
	LD A,(DE)
	LD L,A
	INC DE
	JR TP_2
TP_1	PUSH DE
	LD D,0
	LD E,A
	ADD HL,DE
	ADD HL,DE
	POP DE
TP_2	LD A,H
	LD (BC),A
	DEC BC
	LD A,L
	LD (BC),A
	DEC BC
	SUB #F8*2
	JR NZ,TP_0

	INC A
	LD (VARS1+VRS.DelyCnt),A
	LD (VARS2+VRS.DelyCnt),A
	LD HL,#F001 ;H - CHP.Volume, L - CHP.NtSkCn
	LD (VARS1+VRS.ChanA+CHP.NtSkCn),HL
	LD (VARS1+VRS.ChanB+CHP.NtSkCn),HL
	LD (VARS1+VRS.ChanC+CHP.NtSkCn),HL
	LD (VARS2+VRS.ChanA+CHP.NtSkCn),HL
	LD (VARS2+VRS.ChanB+CHP.NtSkCn),HL
	LD (VARS2+VRS.ChanC+CHP.NtSkCn),HL
	POP HL
	LD (VARS1+VRS.ChanA+CHP.OrnPtr),HL
	LD (VARS1+VRS.ChanB+CHP.OrnPtr),HL
	LD (VARS1+VRS.ChanC+CHP.OrnPtr),HL
	LD (VARS2+VRS.ChanA+CHP.OrnPtr),HL
	LD (VARS2+VRS.ChanB+CHP.OrnPtr),HL
	LD (VARS2+VRS.ChanC+CHP.OrnPtr),HL

	POP AF

;NoteTableCreator (c) Ivan Roshin
;A - NoteTableNumber*2+VersionForNoteTable
;(xx1b - 3.xx..3.4r, xx0b - 3.4x..3.6x..VTII1.0)

	LD HL,NT_DATA
	LD D,0
	ADD A,A
	LD E,A
	ADD HL,DE
	LD E,(HL)
	INC HL
	SRL E
	SBC A,A
	AND #A7 ;#00 (NOP) or #A7 (AND A)
	LD (L3),A
	EX DE,HL
	LD BC,T1_
	ADD HL,BC

	LD A,(DE)
	ADD A,T_
	LD C,A
	ADC A,T_/256
	SUB C
	LD B,A
	PUSH BC
	LD DE,NT_
	PUSH DE

	LD B,12
L1	PUSH BC
	LD C,(HL)
	INC HL
	PUSH HL
	LD B,(HL)

	PUSH DE
	EX DE,HL
	LD DE,23
	LD IXH,8

L2	SRL B
	RR C
L3	DB #19	;AND A or NOP
	LD A,C
	ADC A,D	;=ADC 0
	LD (HL),A
	INC HL
	LD A,B
	ADC A,D
	LD (HL),A
	ADD HL,DE
	DEC IXH
	JR NZ,L2

	POP DE
	INC DE
	INC DE
	POP HL
	INC HL
	POP BC
	DJNZ L1

	POP HL
	POP DE

	LD A,E
	CP TCOLD_1
	JR NZ,CORR_1
	LD A,#FD
	LD (NT_+#2E),A

CORR_1	LD A,(DE)
	AND A
	JR Z,TC_EXIT
	RRA
	PUSH AF
	ADD A,A
	LD C,A
	ADD HL,BC
	POP AF
	JR NC,CORR_2
	DEC (HL)
	DEC (HL)
CORR_2	INC (HL)
	AND A
	SBC HL,BC
	INC DE
	JR CORR_1

TC_EXIT

	POP AF

;VolTableCreator (c) Ivan Roshin
;A - VersionForVolumeTable (0..4 - 3.xx..3.4x;
			   ;5.. - 2.x,3.5x..3.6x..VTII1.0)

	CP 5
	LD HL,#11
	LD D,H
	LD E,H
	LD A,#17
	JR NC,M1
	DEC L
	LD E,L
	XOR A
M1      LD (M2),A

	LD IX,VT_+16

	LD C,#F
INITV2  PUSH HL

	ADD HL,DE
	EX DE,HL
	SBC HL,HL

	LD B,#10
INITV1  LD A,L
M2      DB #7D
	LD A,H
	ADC A,0
	LD (IX),A
	INC IX
	ADD HL,DE
	DJNZ INITV1

	POP HL
	LD A,E
	CP #77
	JR NZ,M3
	INC E
M3      DEC C
	JR NZ,INITV2

	JP ROUT

INITPT3	CALL SETMDAD
	PUSH HL
	LD DE,100
	ADD HL,DE
	LD A,(HL)
	LD (IY-100+VRS.Delay),A
	PUSH HL
	POP IX
	ADD HL,DE
	CALL SETCPPT
	LD E,(IX+102-100)
	INC HL

	IF CurPosCounter
	LD (IY-100+VRS.PosSub),L
	ENDIF

	ADD HL,DE
	CALL SETLPPT
	POP DE
	LD L,(IX+103-100)
	LD H,(IX+104-100)
	ADD HL,DE
	CALL SETPTPT
	LD HL,169
	ADD HL,DE
	CALL SETORPT
	LD HL,105
	ADD HL,DE

SETSMPT LD (IY-100+VRS.SamPtrs),L
	LD (IY-100+VRS.SamPtrs+1),H
	RET

INITPT2	LD A,(HL)
	LD (IY-100+VRS.Delay),A
	PUSH HL
	PUSH HL
	PUSH HL
	INC HL
	INC HL
	LD A,(HL)
	INC HL
	CALL SETSMPT
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	AND A
	SBC HL,DE
	CALL SETMDAD
	POP HL
	LD DE,67
	ADD HL,DE
	CALL SETORPT
	LD E,32
	ADD HL,DE
	LD C,(HL)
	INC HL
	LD B,(HL)
	LD E,30
	ADD HL,DE
	CALL SETCPPT
	LD E,A
	INC HL

	IF CurPosCounter
	LD (IY-100+VRS.PosSub),L
	ENDIF

	ADD HL,DE
	CALL SETLPPT
	POP HL
	ADD HL,BC

SETPTPT	LD (IY-100+VRS.PatsPtr),L
	LD (IY-100+VRS.PatsPtr+1),H
	RET

SETMDAD	LD (IY-100+VRS.MODADDR),L
	LD (IY-100+VRS.MODADDR+1),H
	RET

SETORPT	LD (IY-100+VRS.OrnPtrs),L
	LD (IY-100+VRS.OrnPtrs+1),H
	RET

SETCPPT	LD (IY-100+VRS.CrPsPtr),L
	LD (IY-100+VRS.CrPsPtr+1),H
	RET

SETLPPT	LD (IY-100+VRS.LPosPtr),L
	LD (IY-100+VRS.LPosPtr+1),H
	RET

SETENBS	LD (IY-100+VRS.EnvBase),L
	LD (IY-100+VRS.EnvBase+1),H
	RET

SETESLD	LD (IY-100+VRS.CurESld),L
	LD (IY-100+VRS.CurESld+1),H
	RET

GETIX	PUSH IY
	POP IX
	ADD IX,DE
	RET

PTDECOD CALL GETIX
PTDEC	EQU $+1
	JP #C3C3

;PT2 pattern decoder
PD2_SAM	CALL SETSAM
	JR PD2_LOOP

PD2_EOff LD (IX-12+CHP.Env_En),A
	JR PD2_LOOP

PD2_ENV	LD (IX-12+CHP.Env_En),16
	LD (IY-100+VRS.AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	CALL SETENBS
	JR PD2_LOOP

PD2_ORN	CALL SETORN
	JR PD2_LOOP

PD2_SKIP INC A
	LD (IX-12+CHP.NNtSkp),A
	JR PD2_LOOP

PD2_VOL	RRCA
	RRCA
	RRCA
	RRCA
	LD (IX-12+CHP.Volume),A
	JR PD2_LOOP

PD2_DEL	CALL C_DELAY
	JR PD2_LOOP

PD2_GLIS SET 2,(IX-12+CHP.Flags)
	INC A
	LD (IX-12+CHP.TnSlDl),A
	LD (IX-12+CHP.TSlCnt),A
	LD A,(BC)
	INC BC
        LD (IX-12+CHP.TSlStp),A
	ADD A,A
	SBC A,A
        LD (IX-12+CHP.TSlStp+1),A
	SCF
	JR PD2_LP2

PT2PD	AND A

PD2_LP2	EX AF,AF'

PD2_LOOP LD A,(BC)
	INC BC
	ADD A,#20
	JR Z,PD2_REL
	JR C,PD2_SAM
	ADD A,96
	JR C,PD2_NOTE
	INC A
	JR Z,PD2_EOff
	ADD A,15
	JP Z,PD_FIN
	JR C,PD2_ENV
	ADD A,#10
	JR C,PD2_ORN
	ADD A,#40
	JR C,PD2_SKIP
	ADD A,#10
	JR C,PD2_VOL
	INC A
	JR Z,PD2_DEL
	INC A
	JR Z,PD2_GLIS
	INC A
	JR Z,PD2_PORT
	INC A
	JR Z,PD2_STOP
	LD A,(BC)
	INC BC
	LD (IX-12+CHP.CrNsSl),A
	JR PD2_LOOP

PD2_PORT RES 2,(IX-12+CHP.Flags)
	LD A,(BC)
	INC BC
	INC BC ;ignoring precalc delta to right sound
	INC BC
	SCF
	JR PD2_LP2

PD2_STOP LD (IX-12+CHP.TSlCnt),A
	JR PD2_LOOP

PD2_REL	LD (IX-12+CHP.Flags),A
	JR PD2_EXIT

PD2_NOTE LD L,A
	LD A,(IX-12+CHP.Note)
	LD (PrNote+1),A
	LD (IX-12+CHP.Note),L
	XOR A
	LD (IX-12+CHP.TSlCnt),A
	SET 0,(IX-12+CHP.Flags)
	EX AF,AF'
	JR NC,NOGLIS2
	BIT 2,(IX-12+CHP.Flags)
	JR NZ,NOPORT2
	LD (LoStep),A
	ADD A,A
	SBC A,A
	EX AF,AF'
	LD H,A
	LD L,A
	INC A
	CALL SETPORT
NOPORT2	LD (IX-12+CHP.TSlCnt),1
NOGLIS2	XOR A


PD2_EXIT LD (IX-12+CHP.PsInSm),A
	LD (IX-12+CHP.PsInOr),A
	LD (IX-12+CHP.CrTnSl),A
	LD (IX-12+CHP.CrTnSl+1),A
	JP PD_FIN

;PT3 pattern decoder
PD_OrSm	LD (IX-12+CHP.Env_En),0
	CALL SETORN
PD_SAM_	LD A,(BC)
	INC BC
	RRCA

PD_SAM	CALL SETSAM
	JR PD_LOOP

PD_VOL	RRCA
	RRCA
	RRCA
	RRCA
	LD (IX-12+CHP.Volume),A
	JR PD_LP2
	
PD_EOff	LD (IX-12+CHP.Env_En),A
	LD (IX-12+CHP.PsInOr),A
	JR PD_LP2

PD_SorE	DEC A
	JR NZ,PD_ENV
	LD A,(BC)
	INC BC
	LD (IX-12+CHP.NNtSkp),A
	JR PD_LP2

PD_ENV	CALL SETENV
	JR PD_LP2

PD_ORN	CALL SETORN
	JR PD_LOOP

PD_ESAM	LD (IX-12+CHP.Env_En),A
	LD (IX-12+CHP.PsInOr),A
	CALL NZ,SETENV
	JR PD_SAM_

PT3PD	LD A,(IX-12+CHP.Note)
	LD (PrNote+1),A
	LD L,(IX-12+CHP.CrTnSl)
	LD H,(IX-12+CHP.CrTnSl+1)
	LD (PrSlide+1),HL

PD_LOOP	LD DE,#2010
PD_LP2	LD A,(BC)
	INC BC
	ADD A,E
	JR C,PD_OrSm
	ADD A,D
	JR Z,PD_FIN
	JR C,PD_SAM
	ADD A,E
	JR Z,PD_REL
	JR C,PD_VOL
	ADD A,E
	JR Z,PD_EOff
	JR C,PD_SorE
	ADD A,96
	JR C,PD_NOTE
	ADD A,E
	JR C,PD_ORN
	ADD A,D
	JR C,PD_NOIS
	ADD A,E
	JR C,PD_ESAM
	ADD A,A
	LD E,A
	LD HL,SPCCOMS+#FF20-#2000
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	PUSH DE
	JR PD_LOOP

PD_NOIS	LD (IY-100+VRS.Ns_Base),A
	JR PD_LP2

PD_REL	RES 0,(IX-12+CHP.Flags)
	JR PD_RES

PD_NOTE	LD (IX-12+CHP.Note),A
	SET 0,(IX-12+CHP.Flags)
	XOR A

PD_RES	LD (PDSP_+1),SP
	LD SP,IX
	LD H,A
	LD L,A
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
	PUSH HL
PDSP_	LD SP,#3131

PD_FIN	LD A,(IX-12+CHP.NNtSkp)
	LD (IX-12+CHP.NtSkCn),A
	RET

C_PORTM LD A,(BC)
	INC BC
;SKIP PRECALCULATED TONE DELTA (BECAUSE
;CANNOT BE RIGHT AFTER PT3 COMPILATION)
	INC BC
	INC BC
	EX AF,AF'
	LD A,(BC) ;SIGNED TONE STEP
	INC BC
	LD (LoStep),A
	LD A,(BC)
	INC BC
	AND A
	EX AF,AF'
	LD L,(IX-12+CHP.CrTnSl)
	LD H,(IX-12+CHP.CrTnSl+1)

;Set portamento variables
;A - Delay; A' - Hi(Step); ZF' - (A'=0); HL - CrTnSl

SETPORT	RES 2,(IX-12+CHP.Flags)
	LD (IX-12+CHP.TnSlDl),A
	LD (IX-12+CHP.TSlCnt),A
	PUSH HL
	LD DE,NT_
	LD A,(IX-12+CHP.Note)
	LD (IX-12+CHP.SlToNt),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD A,(HL)
	INC HL
	LD H,(HL)
	LD L,A
	PUSH HL
PrNote	LD A,#3E
	LD (IX-12+CHP.Note),A
	ADD A,A
	LD L,A
	LD H,0
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	POP HL
	SBC HL,DE
	LD (IX-12+CHP.TnDelt),L
	LD (IX-12+CHP.TnDelt+1),H
	POP DE
Version EQU $+1
	LD A,#3E
	CP 6
	JR C,OLDPRTM ;Old 3xxx for PT v3.5-
PrSlide	LD DE,#1111
	LD (IX-12+CHP.CrTnSl),E
	LD (IX-12+CHP.CrTnSl+1),D
LoStep	EQU $+1
OLDPRTM	LD A,#3E
	EX AF,AF'
	JR Z,NOSIG
	EX DE,HL
NOSIG	SBC HL,DE
	JP P,SET_STP
	CPL
	EX AF,AF'
	NEG
	EX AF,AF'
SET_STP	LD (IX-12+CHP.TSlStp+1),A
	EX AF,AF'
	LD (IX-12+CHP.TSlStp),A
	LD (IX-12+CHP.COnOff),0
	RET

C_GLISS	SET 2,(IX-12+CHP.Flags)
	LD A,(BC)
	INC BC
	LD (IX-12+CHP.TnSlDl),A
	AND A
	JR NZ,GL36
	LD A,(Version) ;AlCo PT3.7+
	CP 7
	SBC A,A
	INC A
GL36	LD (IX-12+CHP.TSlCnt),A
	LD A,(BC)
	INC BC
	EX AF,AF'
	LD A,(BC)
	INC BC
	JR SET_STP

C_SMPOS	LD A,(BC)
	INC BC
	LD (IX-12+CHP.PsInSm),A
	RET

C_ORPOS	LD A,(BC)
	INC BC
	LD (IX-12+CHP.PsInOr),A
	RET

C_VIBRT	LD A,(BC)
	INC BC
	LD (IX-12+CHP.OnOffD),A
	LD (IX-12+CHP.COnOff),A
	LD A,(BC)
	INC BC
	LD (IX-12+CHP.OffOnD),A
	XOR A
	LD (IX-12+CHP.TSlCnt),A
	LD (IX-12+CHP.CrTnSl),A
	LD (IX-12+CHP.CrTnSl+1),A
	RET

C_ENGLS	LD A,(BC)
	INC BC
	LD (IY-100+VRS.Env_Del),A
	LD (IY-100+VRS.CurEDel),A
	LD A,(BC)
	INC BC
	LD L,A
	LD A,(BC)
	INC BC
	LD H,A
	LD (IY-100+VRS.ESldAdd),L
	LD (IY-100+VRS.ESldAdd+1),H
	RET

C_DELAY	LD A,(BC)
	INC BC
	LD (IY-100+VRS.Delay),A
	LD HL,VARS2+VRS.ModNum ;if AlCo_TS
	BIT 1,(HL)
	RET Z
	LD (VARS1+VRS.Delay),A
	LD (VARS1+VRS.DelyCnt),A
	LD (VARS2+VRS.Delay),A
	RET
	
SETENV	LD (IX-12+CHP.Env_En),E
	LD (IY-100+VRS.AYREGS+EnvTp),A
	LD A,(BC)
	INC BC
	LD H,A
	LD A,(BC)
	INC BC
	LD L,A
	CALL SETENBS
	XOR A
	LD (IX-12+CHP.PsInOr),A
	LD (IY-100+VRS.CurEDel),A
	LD H,A
	LD L,A
	JP SETESLD

SETORN	ADD A,A
	LD E,A
	LD D,0
	LD (IX-12+CHP.PsInOr),D
	LD L,(IY-100+VRS.OrnPtrs)
	LD H,(IY-100+VRS.OrnPtrs+1)
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	LD L,(IY-100+VRS.MODADDR)
	LD H,(IY-100+VRS.MODADDR+1)
	ADD HL,DE
	LD (IX-12+CHP.OrnPtr),L
	LD (IX-12+CHP.OrnPtr+1),H
C_NOP	RET

SETSAM	ADD A,A
	LD E,A
	LD D,0
	LD L,(IY-100+VRS.SamPtrs);
	LD H,(IY-100+VRS.SamPtrs+1);
	ADD HL,DE
	LD E,(HL)
	INC HL
	LD D,(HL)
	LD L,(IY-100+VRS.MODADDR)
	LD H,(IY-100+VRS.MODADDR+1)
	ADD HL,DE
	LD (IX-12+CHP.SamPtr),L
	LD (IX-12+CHP.SamPtr+1),H
	RET

;ALL 16 ADDRESSES TO PROTECT FROM BROKEN PT3 MODULES
SPCCOMS DW C_NOP
	DW C_GLISS
	DW C_PORTM
	DW C_SMPOS
	DW C_ORPOS
	DW C_VIBRT
	DW C_NOP
	DW C_NOP
	DW C_ENGLS
	DW C_DELAY
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP
	DW C_NOP

CHREGS	CALL GETIX
	XOR A
	LD (Ampl),A
	BIT 0,(IX+CHP.Flags)
	PUSH HL
	JP Z,CH_EXIT
	LD (CSP_+1),SP
	LD L,(IX+CHP.OrnPtr)
	LD H,(IX+CHP.OrnPtr+1)
	LD SP,HL
	POP DE
	LD H,A
	LD A,(IX+CHP.PsInOr)
	LD L,A
	ADD HL,SP
	INC A
		;PT2	PT3
OrnCP	INC A	;CP E	CP D
	JR C,CH_ORPS
OrnLD	DB 1	;LD A,D	LD A,E
CH_ORPS	LD (IX+CHP.PsInOr),A
	LD A,(IX+CHP.Note)
	ADD A,(HL)
	JP P,CH_NTP
	XOR A
CH_NTP	CP 96
	JR C,CH_NOK
	LD A,95
CH_NOK	ADD A,A
	EX AF,AF'
	LD L,(IX+CHP.SamPtr)
	LD H,(IX+CHP.SamPtr+1)
	LD SP,HL
	POP DE
	LD H,0
	LD A,(IX+CHP.PsInSm)
	LD B,A
	ADD A,A
SamClc2	ADD A,A ;or ADD A,B for PT2
	LD L,A
	ADD HL,SP
	LD SP,HL
	LD A,B
	INC A
		;PT2	PT3
SamCP	INC A	;CP E	CP D
	JR C,CH_SMPS
SamLD	DB 1	;LD A,D	LD A,E
CH_SMPS	LD (IX+CHP.PsInSm),A
	POP BC
	POP HL

;Convert PT2 sample to PT3
		;PT2		PT3
SamCnv	POP HL  ;BIT 2,C	JR e_
	POP HL	
	LD H,B
	JR NZ,$+8
	EX DE,HL
	AND A
	SBC HL,HL
	SBC HL,DE
	LD D,C
	RR C
	SBC A,A
	CPL
	AND #3E
	RR C
	RR B
	AND C
	LD C,A
	LD A,B
	RRA
	RRA
	RR D
	RRA
	AND #9F
	LD B,A

e_	LD E,(IX+CHP.TnAcc)
	LD D,(IX+CHP.TnAcc+1)
	ADD HL,DE
	BIT 6,B
	JR Z,CH_NOAC
	LD (IX+CHP.TnAcc),L
	LD (IX+CHP.TnAcc+1),H
CH_NOAC EX DE,HL
	EX AF,AF'
	ADD A,NT_
	LD L,A
	ADC A,NT_/256
	SUB L
	LD H,A
	LD SP,HL
	POP HL
	ADD HL,DE
	LD E,(IX+CHP.CrTnSl)
	LD D,(IX+CHP.CrTnSl+1)
	ADD HL,DE
CSP_	LD SP,#3131
	EX (SP),HL
	XOR A
	OR (IX+CHP.TSlCnt)
	JR Z,CH_AMP
	DEC (IX+CHP.TSlCnt)
	JR NZ,CH_AMP
	LD A,(IX+CHP.TnSlDl)
	LD (IX+CHP.TSlCnt),A
	LD L,(IX+CHP.TSlStp)
	LD H,(IX+CHP.TSlStp+1)
	LD A,H
	ADD HL,DE
	LD (IX+CHP.CrTnSl),L
	LD (IX+CHP.CrTnSl+1),H
	BIT 2,(IX+CHP.Flags)
	JR NZ,CH_AMP
	LD E,(IX+CHP.TnDelt)
	LD D,(IX+CHP.TnDelt+1)
	AND A
	JR Z,CH_STPP
	EX DE,HL
CH_STPP SBC HL,DE
	JP M,CH_AMP
	LD A,(IX+CHP.SlToNt)
	LD (IX+CHP.Note),A
	XOR A
	LD (IX+CHP.TSlCnt),A
	LD (IX+CHP.CrTnSl),A
	LD (IX+CHP.CrTnSl+1),A
CH_AMP	LD A,(IX+CHP.CrAmSl)
	BIT 7,C
	JR Z,CH_NOAM
	BIT 6,C
	JR Z,CH_AMIN
	CP 15
	JR Z,CH_NOAM
	INC A
	JR CH_SVAM
CH_AMIN	CP -15
	JR Z,CH_NOAM
	DEC A
CH_SVAM	LD (IX+CHP.CrAmSl),A
CH_NOAM	LD L,A
	LD A,B
	AND 15
	ADD A,L
	JP P,CH_APOS
	XOR A
CH_APOS	CP 16
	JR C,CH_VOL
	LD A,15
CH_VOL	OR (IX+CHP.Volume)
	ADD A,VT_
	LD L,A
	ADC A,VT_/256
	SUB L
	LD H,A
	LD A,(HL)
CH_ENV	BIT 0,C
	JR NZ,CH_NOEN
	OR (IX+CHP.Env_En)
CH_NOEN	LD (Ampl),A
	BIT 7,B
	LD A,C
	JR Z,NO_ENSL
	RLA
	RLA
	SRA A
	SRA A
	SRA A
	ADD A,(IX+CHP.CrEnSl) ;SEE COMMENT BELOW
	BIT 5,B
	JR Z,NO_ENAC
	LD (IX+CHP.CrEnSl),A
NO_ENAC	ADD A,(IY-100+VRS.AddToEn) ;BUG IN PT3 - NEED WORD HERE
	LD (IY-100+VRS.AddToEn),A
	JR CH_MIX
NO_ENSL RRA
	ADD A,(IX+CHP.CrNsSl)
	LD (IY-100+VRS.AddToNs),A
	BIT 5,B
	JR Z,CH_MIX
	LD (IX+CHP.CrNsSl),A
CH_MIX	LD A,B
	RRA
	AND #48
CH_EXIT	OR (IY-100+VRS.AYREGS+Mixer)
	RRCA
	LD (IY-100+VRS.AYREGS+Mixer),A
	POP HL
	XOR A
	OR (IX+CHP.COnOff)
	RET Z
	DEC (IX+CHP.COnOff)
	RET NZ
	XOR (IX+CHP.Flags)
	LD (IX+CHP.Flags),A
	RRA
	LD A,(IX+CHP.OnOffD)
	JR C,CH_ONDL
	LD A,(IX+CHP.OffOnD)
CH_ONDL	LD (IX+CHP.COnOff),A
	RET

PLAY_	XOR A
	LD (IY-100+VRS.AddToEn),A
	LD (IY-100+VRS.AYREGS+Mixer),A
	DEC A
	LD (IY-100+VRS.AYREGS+EnvTp),A
	DEC (IY-100+VRS.DelyCnt)
	JP NZ,PL2
	DEC (IY-100+VRS.ChanA+CHP.NtSkCn)
	JR NZ,PL1B
	LD C,(IY-100+VRS.AdInPtA)
	LD B,(IY-100+VRS.AdInPtA+1)
	LD A,(BC)
	AND A
	JR NZ,PL1A
	LD D,A
	LD (IY-100+VRS.Ns_Base),A
	LD L,(IY-100+VRS.CrPsPtr)
	LD H,(IY-100+VRS.CrPsPtr+1)
	INC HL
	LD A,(HL)
	INC A
	JR NZ,PLNLP

	IF LoopChecker
	CALL CHECKLP
	ENDIF

	LD L,(IY-100+VRS.LPosPtr)
	LD H,(IY-100+VRS.LPosPtr+1)
	LD A,(HL)
	INC A
PLNLP	CALL SETCPPT
	DEC A
	BIT 1,(IY-100+VRS.ModNum)
	JR Z,NoAlCo
TSSub	EQU $+1
	SUB #D6
	CPL
NoAlCo
		;PT2		PT3
PsCalc	DEC A	;ADD A,A	NOP
	DEC A	;ADD A,(HL)	NOP
	ADD A,A
	LD E,A
	RL D

	IF CurPosCounter
	LD A,L
	SUB (IY-100+VRS.PosSub)
	LD (IY-100+VRS.CurPos),A
	ENDIF

	LD L,(IY-100+VRS.PatsPtr)
	LD H,(IY-100+VRS.PatsPtr+1)
	ADD HL,DE
	LD E,(IY-100+VRS.MODADDR)
	LD D,(IY-100+VRS.MODADDR+1)
	LD (PSP_+1),SP
	LD SP,HL
	POP HL
	ADD HL,DE
	LD B,H
	LD C,L
	POP HL
	ADD HL,DE
	LD (IY-100+VRS.AdInPtB),L
	LD (IY-100+VRS.AdInPtB+1),H
	POP HL
	ADD HL,DE
	LD (IY-100+VRS.AdInPtC),L
	LD (IY-100+VRS.AdInPtC+1),H
PSP_	LD SP,#3131
PL1A	LD DE,VRS.ChanA+12-100
	CALL PTDECOD
	LD (IY-100+VRS.AdInPtA),C
	LD (IY-100+VRS.AdInPtA+1),B

PL1B	DEC (IY-100+VRS.ChanB+CHP.NtSkCn)
	JR NZ,PL1C
	LD DE,VRS.ChanB+12-100
	LD C,(IY-100+VRS.AdInPtB)
	LD B,(IY-100+VRS.AdInPtB+1)
	CALL PTDECOD
	LD (IY-100+VRS.AdInPtB),C
	LD (IY-100+VRS.AdInPtB+1),B

PL1C	DEC (IY-100+VRS.ChanC+CHP.NtSkCn)
	JR NZ,PL1D
	LD DE,VRS.ChanC+12-100
	LD C,(IY-100+VRS.AdInPtC)
	LD B,(IY-100+VRS.AdInPtC+1)
	CALL PTDECOD
	LD (IY-100+VRS.AdInPtC),C
	LD (IY-100+VRS.AdInPtC+1),B

PL1D	LD A,(IY-100+VRS.Delay)
	LD (IY-100+VRS.DelyCnt),A

PL2	LD DE,VRS.ChanA-100
	LD L,(IY-100+VRS.AYREGS+TonA)
	LD H,(IY-100+VRS.AYREGS+TonA+1)
	CALL CHREGS
	LD (IY-100+VRS.AYREGS+TonA),L
	LD (IY-100+VRS.AYREGS+TonA+1),H
Ampl	EQU $+1
	LD A,#3E
	LD (IY-100+VRS.AYREGS+AmplA),A
	LD DE,VRS.ChanB-100
	LD L,(IY-100+VRS.AYREGS+TonB)
	LD H,(IY-100+VRS.AYREGS+TonB+1)
	CALL CHREGS
	LD (IY-100+VRS.AYREGS+TonB),L
	LD (IY-100+VRS.AYREGS+TonB+1),H
	LD A,(Ampl)
	LD (IY-100+VRS.AYREGS+AmplB),A
	LD DE,VRS.ChanC-100
	LD L,(IY-100+VRS.AYREGS+TonC)
	LD H,(IY-100+VRS.AYREGS+TonC+1)
	CALL CHREGS
	LD (IY-100+VRS.AYREGS+TonC),L
	LD (IY-100+VRS.AYREGS+TonC+1),H
	LD A,(Ampl)
	LD (IY-100+VRS.AYREGS+AmplC),A

	LD A,(IY-100+VRS.Ns_Base)
	ADD (IY-100+VRS.AddToNs)
	LD (IY-100+VRS.AYREGS+Noise),A

	LD A,(IY-100+VRS.AddToEn)
	LD E,A
	ADD A,A
	SBC A,A
	LD D,A
	LD L,(IY-100+VRS.EnvBase)
	LD H,(IY-100+VRS.EnvBase+1)
	ADD HL,DE
	LD E,(IY-100+VRS.CurESld)
	LD D,(IY-100+VRS.CurESld+1)
	ADD HL,DE
	LD (IY-100+VRS.AYREGS+Env),L
	LD (IY-100+VRS.AYREGS+Env+1),H

	XOR A
	OR (IY-100+VRS.CurEDel)
	RET Z
	DEC (IY-100+VRS.CurEDel)
	RET NZ
	LD A,(IY-100+VRS.Env_Del)
	LD (IY-100+VRS.CurEDel),A
	LD L,(IY-100+VRS.ESldAdd)
	LD H,(IY-100+VRS.ESldAdd+1)
	ADD HL,DE
	JP SETESLD

PLAY    LD IY,VARS1+100
	CALL PLAY_
	LD A,(is_ts)
	AND A
	JR Z,PL_nts
	LD IY,VARS2+100
	CALL PLAY_
PL_nts
	IF Basic
	LD IY,#5C3A
	ENDIF

ROUT	LD BC,#FFFD
	LD A,(is_ts)
	AND A
	JR Z,r_nts ;keep old standard
	OUT (C),B
r_nts	EX AF,AF'

	IF ACBBAC
	LD IX,VARS1+VRS.AYREGS
	ELSE
	LD HL,VARS1+VRS.AYREGS
	ENDIF

	CALL ROUT_
	EX AF,AF'
	RET Z
	LD B,D
	CPL
	OUT (C),A

	IF ACBBAC
	LD IX,VARS2+VRS.AYREGS
	ELSE
	LD HL,VARS2+VRS.AYREGS
	ENDIF

ROUT_
	IF ACBBAC
	LD A,(SETUP)
	AND 12
	JR Z,ABC
	ADD A,CHTABLE
	LD E,A
	ADC A,CHTABLE/256
	SUB E
	LD D,A
	LD B,0
	PUSH IX
	POP HL
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	LD A,(IX+TonB)
	LD C,(HL)
	LD (IX+TonB),C
	LD (HL),A
	INC HL
	LD A,(IX+TonB+1)
	LD C,(HL)
	LD (IX+TonB+1),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD C,A
	ADD HL,BC
	LD A,(IX+AmplB)
	LD C,(HL)
	LD (IX+AmplB),C
	LD (HL),A
	LD A,(DE)
	INC DE
	LD (RxCA1),A
	XOR 8
	LD (RxCA2),A
	LD A,(DE)
	AND (IX+Mixer)
	LD E,A
	LD A,(IX+Mixer)
RxCA1	DB #E6
	AND %010010
	OR E
	LD E,A
	LD A,(IX+Mixer)
	AND %010010
RxCA2	OR E
	OR E
	LD (IX+Mixer),A
ABC
	ENDIF

	XOR A
	LD DE,#FFBF

	IF ACBBAC
	LD BC,#FFFD
	PUSH IX
	POP HL
	ENDIF

LOUT	OUT (C),A
	LD B,E
	OUTI 
	LD B,D
	INC A
	CP 13
	JR NZ,LOUT
	OUT (C),A
	LD A,(HL)
	AND A
	RET M
	LD B,E
	OUT (C),A
	RET

	IF ACBBAC
CHTABLE	EQU $-4
	DB 4,5,15,%001001,0,7,7,%100100
	ENDIF

NT_DATA	DB (T_NEW_0-T1_)*2
	DB TCNEW_0-T_
	DB (T_OLD_0-T1_)*2+1
	DB TCOLD_0-T_
	DB (T_NEW_1-T1_)*2+1
	DB TCNEW_1-T_
	DB (T_OLD_1-T1_)*2+1
	DB TCOLD_1-T_
	DB (T_NEW_2-T1_)*2
	DB TCNEW_2-T_
	DB (T_OLD_2-T1_)*2
	DB TCOLD_2-T_
	DB (T_NEW_3-T1_)*2
	DB TCNEW_3-T_
	DB (T_OLD_3-T1_)*2
	DB TCOLD_3-T_

T_

TCOLD_0	DB #00+1,#04+1,#08+1,#0A+1,#0C+1,#0E+1,#12+1,#14+1
	DB #18+1,#24+1,#3C+1,0
TCOLD_1	DB #5C+1,0
TCOLD_2	DB #30+1,#36+1,#4C+1,#52+1,#5E+1,#70+1,#82,#8C,#9C
	DB #9E,#A0,#A6,#A8,#AA,#AC,#AE,#AE,0
TCNEW_3	DB #56+1
TCOLD_3	DB #1E+1,#22+1,#24+1,#28+1,#2C+1,#2E+1,#32+1,#BE+1,0
TCNEW_0	DB #1C+1,#20+1,#22+1,#26+1,#2A+1,#2C+1,#30+1,#54+1
	DB #BC+1,#BE+1,0
TCNEW_1 EQU TCOLD_1
TCNEW_2	DB #1A+1,#20+1,#24+1,#28+1,#2A+1,#3A+1,#4C+1,#5E+1
	DB #BA+1,#BC+1,#BE+1,0

PT3EMPTYORN EQU $-1
	DB 1,0

;first 12 values of tone tables (packed)

T_PACK	DB #06EC*2/256,#06EC*2
	DB #0755-#06EC
	DB #07C5-#0755
	DB #083B-#07C5
	DB #08B8-#083B
	DB #093D-#08B8
	DB #09CA-#093D
	DB #0A5F-#09CA
	DB #0AFC-#0A5F
	DB #0BA4-#0AFC
	DB #0C55-#0BA4
	DB #0D10-#0C55
	DB #066D*2/256,#066D*2
	DB #06CF-#066D
	DB #0737-#06CF
	DB #07A4-#0737
	DB #0819-#07A4
	DB #0894-#0819
	DB #0917-#0894
	DB #09A1-#0917
	DB #0A33-#09A1
	DB #0ACF-#0A33
	DB #0B73-#0ACF
	DB #0C22-#0B73
	DB #0CDA-#0C22
	DB #0704*2/256,#0704*2
	DB #076E-#0704
	DB #07E0-#076E
	DB #0858-#07E0
	DB #08D6-#0858
	DB #095C-#08D6
	DB #09EC-#095C
	DB #0A82-#09EC
	DB #0B22-#0A82
	DB #0BCC-#0B22
	DB #0C80-#0BCC
	DB #0D3E-#0C80
	DB #07E0*2/256,#07E0*2
	DB #0858-#07E0
	DB #08E0-#0858
	DB #0960-#08E0
	DB #09F0-#0960
	DB #0A88-#09F0
	DB #0B28-#0A88
	DB #0BD8-#0B28
	DB #0C80-#0BD8
	DB #0D60-#0C80
	DB #0E10-#0D60
	DB #0EF8-#0E10

;vars from here can be stripped
;you can move VARS to any other address

VARS

is_ts	DB 0

;ChannelsVars
	STRUCT	CHP
;reset group
PsInOr	DB 0
PsInSm	DB 0
CrAmSl	DB 0
CrNsSl	DB 0
CrEnSl	DB 0
TSlCnt	DB 0
CrTnSl	DW 0
TnAcc	DW 0
COnOff	DB 0
;reset group

OnOffD	DB 0

;IX for PTDECOD here (+12)
OffOnD	DB 0
OrnPtr	DW 0
SamPtr	DW 0
NNtSkp	DB 0
Note	DB 0
SlToNt	DB 0
Env_En	DB 0
Flags	DB 0
 ;Enabled - 0, SimpleGliss - 2
TnSlDl	DB 0
TSlStp	DW 0
TnDelt	DW 0
NtSkCn	DB 0
Volume	DB 0
	ENDS

	STRUCT	VRS

;IF not works in STRUCT in SjASM :(
;	IF CurPosCounter
CurPos	DB 0
PosSub	DB 0
;	ENDIF

ModNum	DB 0 ;bit0: ChipNum
	     ;bit1: 1-reversed patterns order (AlCo TS)

ChanA	DS CHP
ChanB	DS CHP
ChanC	DS CHP

;GlobalVars
MODADDR	DW 0
OrnPtrs	DW 0
SamPtrs	DW 0
PatsPtr	DW 0
AdInPtA	DW 0
AdInPtB	DW 0
AdInPtC	DW 0
CrPsPtr	DW 0
LPosPtr	DW 0
Delay	DB 0
DelyCnt	DB 0
ESldAdd	DW 0
CurESld	DW 0
Env_Del	DB 0
CurEDel	DB 0
Ns_Base	DB 0
AddToNs	DB 0
AddToEn	DB 0
EnvBase	DW 0
AYREGS	DS 14
	ENDS

VARS1	DS VRS
VARS2	DS VRS

VT_	EQU $-16
	DS 256-16 ;CreatedVolumeTableAddress

T1_	EQU VT_+16 ;Tone tables data depacked here

T_OLD_1	EQU T1_
T_OLD_2	EQU T_OLD_1+24
T_OLD_3	EQU T_OLD_2+24
T_OLD_0	EQU T_OLD_3+2
T_NEW_0	EQU T_OLD_0
T_NEW_1	EQU T_OLD_1
T_NEW_2	EQU T_NEW_0+24
T_NEW_3	EQU T_OLD_3

PT2EMPTYORN EQU VT_+31 ;1,0,0 sequence

NT_	DS 192 ;CreatedNoteTableAddress

VAR0END	EQU VT_+16 ;INIT zeroes from VARS to VAR0END-1

VARSEND EQU $

MDLADDR EQU songdata

;Release 0 steps:
;04/21/2007
;Works start (PTxPlay adaptation); first beta.
;04/22/2007
;Job finished; beta-testing.
;04/23/2007
;PT v3.7 TS mode corrected (after AlCo remarks).
;04/29/2007
;Added 1.XX and 2.XX special commands interpretation for PT3
;modules of v3.7+.

;Size (minimal build for ZX Spectrum):
;Code block #908 bytes
;Variables #2BF bytes (can be stripped)
;Total size #908+#2BF=#BC7 (3015) bytes
	ENDMODULE
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************
;***************************


			module stp
@NULL = 0
;------------------------------------------------------------------------------
music_init:		ld	hl,songdata
music_init0:	ld	(module_addr+1),hl
		ld	a,#FC
		ld	(ChA.smppos+1),a
		ld	(ChB.smppos+1),a
		ld	(ChC.smppos+1),a
		ld	a,(hl)
		inc	hl
		ld	(speed+1),a
		call	add_offset.de
		ld	a,(hl)
		ld	(song_length+1),a
		inc	hl
		ld	a,(hl)
		ld	(loop_position+1),a
		inc	hl
		ld	(positions+1),hl
		call	add_offset
		ld	(patterns+1),hl
		push	hl
		call	add_offset
		ld	(ChA.ornoffs+1),hl
		ld	(ChB.ornoffs+1),hl
		ld	(ChC.ornoffs+1),hl
		call	add_offset
		ld	(ChA.smpoffs+1),hl
		ld	(ChB.smpoffs+1),hl
		ld	(ChC.smpoffs+1),hl
		ex	de,hl
		ld	a,(hl)
		ld	(hl),0
		pop	hl
		or	a
		jr	z,.noreloc

.relocator:	call	add_offset.de
		ex	de,hl
		dec	hl
		ld	(hl),d
		dec	hl
		ld	(hl),e
		inc	hl
		inc	hl
		dec	a
		jr	nz,.relocator

.noreloc:	ld	hl,empty_pattern
		ld	c,3
		ld	(ChA.datptr+1),hl
		ld	(ChB.datptr+1),hl
		ld	(ChC.datptr+1),hl
		ld	h,a
		ld	l,a
		ld	(ChA.Xportamnt+1),hl
		ld	(ChB.Xportamnt+1),hl
		ld	(ChA.Xpitch+1),hl
		ld	(ChB.Xpitch+1),hl
		ld	(ChC.playback+2),a

		ld	(chkfadeout+1),a
		ld	(apply_volume+1),a
		ld	(ChA.attn+1),a
		ld	(ChB.attn+1),a
		ld	(ChC.attn+1),a
		dec	a
		ld	(current_pos+1),a
		ld	hl,music_setup
		res	7,(hl)

music_mute:	xor	a
		ld	(AYREG_EnvSh+1),a
		ld	hl,AYREG_TonA
		ld	b,#0D
.loopregs:	ld	(hl),a
		inc	hl
		djnz	.loopregs
		ld	a,c
		ld	(tempocnt_0),a
		ld	bc,#FFFD
		ld	a,#0C
		out	(c),a
		xor	a
		ld	b,#BF
		out	(c),a
		jp	outay

add_offset	ex	de,hl
.de		ld	e,(hl)
		inc	hl
		ld	d,(hl)
		inc	hl
		ex	de,hl
module_addr:	ld	bc,0
		add	hl,bc
		ret

; ---------------------------------------------------------------------------
resetfadeout:	ld	hl,music_setup
noloop:		set	7,(hl)
		xor	a
		ld	(AYREG_AmplA),a
		ld	(AYREG_AmplB),a
		ld	(AYREG_AmplC),a
		ld	(AYREG_EnvSh+1),a
		dec	a
		ld	(tempocnt_0),a
		ld	(chkfadeout+1),a
		ld	xl,#3F
		jp	retsp

enablefade:	ld	a,48
forcefade:	ld	(chkfadeout+1),a
		ld	(countfadeout+1),a
		ld	(divfadeout+1),a
		xor	a
		ld	(apply_volume+1),a
		jr	loop_position

; ---------------------------------------------------------------------------
new_position:	ld	hl,(ChB.datptr+1)
		or	(hl)
		jp	nz,chkfadeout
		ld	hl,(ChC.datptr+1)
		or	(hl)
		jp	nz,chkfadeout
		ld	b,a
current_pos:	ld	a,0
		inc	a
song_length:	cp	0
		jr	nz,song_continue
		ld	hl,music_setup
		bit	1,(hl)
		jr	nz,enablefade
		bit	0,(hl)
		jr	nz,noloop
loop_position:	ld	a,0
song_continue:	ld	(current_pos+1),a
		ld	c,a

positions:	ld	hl,NULL
		add	hl,bc
		add	hl,bc
		ld	c,(hl)
		inc	hl
		ld	a,(hl)

patterns:	ld	hl,NULL
		add	hl,bc
		ld	sp,hl
		pop	hl
		ld	(ChA.datptr+1),hl
		pop	hl
		ld	(ChB.datptr+1),hl
		pop	hl
		ld	(ChC.datptr+1),hl
		add	a,#60
		ld	(ChC.playback+2),a
		ld	b,a
		ld	c,#F0
		jp	ChA.datptr

; ----------------------------------------------------------------------------
music_play:	di
		ld	(retsp+1),sp
		ld	d,0
		exx
ChC.playback:	ld	bc,#60F0
		ld	hl,tempocnt_0
		dec	(hl)
		jp	nz,ChB.playback
@speed:		ld	(hl),0
		inc	hl
		dec	(hl)
		jp	p,ChB.Xsmpptr
ChC.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,ChB.Xsmpptr
ChC.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChC.nocmd
		sub	c
		jr	nc,ChC.setvol
		sub	c
		jp	nc,ChC.nop
		sub	c
		jr	nc,ChC.setrst
		sub	c
		jp	ChC.setenv

ChC.nocmd:	sub	#80
		jr	nc,ChC.setspd
		sub	c
		jp	nc,ChC.setorn
		sbc	a,c
		jr	nc,ChC.setsmp
		add	a,b
		ld	(ChC.note+1),a
		ld	(ChC.datptr+1),hl
		xor	a
		ld	(ChC.smppos+1),a
		ld	(ChC.ornpos+1),a
		ld	l,a
		ld	h,a
		ld	(ChC.pitch+1),hl
		jp	ChC.exit

ChC.setport:	ld	a,(hl)
		inc	hl
		ld	(ChC.portamnt+1),a
		rla
		sbc	a,a
		ld	(ChC.portamnt+2),a
		jr	ChC.readpatt

ChC.setvol:	jr	z,ChC.setport
		dec	a
		ld	(ChC.volume+1),a
		jr	ChC.readpatt

ChC.setrst:	ld	a,#FC
		ld	(ChC.smppos+1),a
		ld	(ChC.datptr+1),hl
		jp	ChC.exit

ChC.setsmp:	add	a,a
		exx
		ld	e,a
ChC.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChC.smploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChC.smplen+1),a
		ld	(ChC.smpptr+1),hl
		exx
		jp	ChC.readpatt

ChC.setspd:	ld	(ChC.exit+1),a
		jp	ChC.readpatt

ChC.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChC.ampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChC.ornptr+1),de
		xor	a
		ld	(ChC.ornloop+1),a
		ld	(ChC.portamnt+1),a
		ld	(ChC.portamnt+2),a
		inc	a
		ld	(ChC.ornlen+1),a
		jp	ChC.readpatt

ChC.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChC.ampl+1),a
ChC.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChC.ornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChC.ornlen+1),a
		inc	hl
		ld	(ChC.ornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChC.portamnt+1),hl
		exx
		jp	ChC.readpatt

; ---------------------------------------------------------------------------
ChB.playback:	ld	a,(hl)
		dec	a
		jp	nz,ChA.playback
		ld	hl,tempocnt_2
		dec	(hl)
		jp	p,chkfadeout
ChB.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,chkfadeout
ChB.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChB.nocmd
		sub	c
		jr	nc,ChB.setvol
		sub	c
		jr	nc,ChB.nop
		sub	c
		jr	nc,ChB.setrst
		sub	c
		jr	ChB.setenv

ChB.nocmd:	sub	#80
		jr	nc,ChB.setspd
		sub	c
		jp	nc,ChB.setorn
		sbc	a,c
		jr	nc,ChB.setsmp
		add	a,b
		ld	(ChB.Xnote+1),a
		ld	(ChB.datptr+1),hl
		xor	a
		ld	(ChB.Xsmppos+1),a
		ld	l,a
		ld	h,a
		ld	(ChB.Xpitch+1),hl
		ld	a,#22
		ld	(ChB.Xinst1),a
		ld	a,#32
		ld	(ChB.Xinst2),a
		jr	ChB.exit

ChB.setvol:	jr	z,ChB.setport
		dec	a
		ld	(ChB.Xvolume+1),a
		jr	ChB.readpatt

ChB.nop:	ld	(ChB.datptr+1),hl
ChB.exit:	ld	a,0
		ld	(tempocnt_2),a
		jp	chkfadeout

ChB.setrst:	ld	a,#FC
		ld	(ChB.Xsmppos+1),a
		ld	(ChB.datptr+1),hl
		jr	ChB.exit

ChB.setsmp:	add	a,a
		exx
		ld	e,a
ChB.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChB.Xsmploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChB.Xsmplen+1),a
		ld	(ChB.Xsmpptr+1),hl
		exx
		jr	ChB.readpatt

ChB.setspd:	ld	(ChB.exit+1),a
		jr	ChB.readpatt

ChB.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChB.Xampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChB.Xornptr+1),de
		ld	a,#22
		ld	(ChB.Xinst3),a
		xor	a
		ld	(ChB.Xornloop+1),a
		ld	(ChB.Xportamnt+1),a
		ld	(ChB.Xportamnt+2),a
		inc	a
		ld	(ChB.Xornlen+1),a
		jp	ChB.readpatt

ChB.setport:	ld	a,(hl)
		inc	hl
		ld	(ChB.Xportamnt+1),a
		rla
		sbc	a,a
		ld	(ChB.Xportamnt+2),a
		jp	ChB.readpatt

ChB.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChB.Xampl+1),a
ChB.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChB.Xornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChB.Xornlen+1),a
		inc	hl
		ld	(ChB.Xornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChB.Xportamnt+1),hl
		ld	a,#22
		ld	(ChB.Xinst3),a
		exx
		jp	ChB.readpatt

; ---------------------------------------------------------------------------
ChA.playback:	dec	a
		jp	nz,chkfadeout
		dec	hl
		dec	(hl)
		jp	p,chkfadeout
ChA.datptr:	ld	hl,NULL
		or	(hl)
		jp	z,new_position
ChA.readpatt:	ld	a,(hl)
		inc	hl
		cp	#C0
		jr	c,ChA.nocmd
		sub	c
		jr	nc,ChA.setvol
		sub	c
		jr	nc,ChA.nop
		sub	c
		jr	nc,ChA.setrst
		sub	c
		jr	ChA.setenv

ChA.nocmd:	sub	#80
		jr	nc,ChA.setspd
		sub	c
		jp	nc,ChA.setorn
		sbc	a,c
		jr	nc,ChA.setsmp
		add	a,b
		ld	(ChA.Xnote+1),a
		ld	(ChA.datptr+1),hl
		xor	a
		ld	(ChA.Xsmppos+1),a
		ld	l,a
		ld	h,a
		ld	(ChA.Xpitch+1),hl
		ld	a,#22
		ld	(ChA.Xinst1),a
		ld	a,#32
		ld	(ChA.Xinst2),a
		jr	ChA.exit

ChA.setvol:	jr	z,ChA.setport
		dec	a
		ld	(ChA.Xvolume+1),a
		jr	ChA.readpatt

ChA.nop:	ld	(ChA.datptr+1),hl
ChA.exit:	ld	a,0
		ld	(tempocnt_1),a
		jp	chkfadeout

ChA.setrst:	ld	a,#FC
		ld	(ChA.Xsmppos+1),a
		ld	(ChA.datptr+1),hl
		jr	ChA.exit

ChA.setsmp:	add	a,a
		exx
		ld	e,a
ChA.smpoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChA.Xsmploop+1),a
		inc	hl
		ld	a,(hl)
		add	a,a
		add	a,a
		ld	(ChA.Xsmplen+1),a
		ld	(ChA.Xsmpptr+1),hl
		exx
		jr	ChA.readpatt

ChA.setspd:	ld	(ChA.exit+1),a
		jr	ChA.readpatt

ChA.setenv:	ld	(env.shape+1),a
		ld	a,#1F
		ld	(ChA.Xampl+1),a
		ld	a,(hl)
		inc	hl
		ld	(env.period+1),a
		ld	de,empty_orn
		ld	(ChA.Xornptr+1),de
		ld	a,#22
		ld	(ChA.Xinst3),a
		xor	a
		ld	(ChA.Xornloop+1),a
		ld	(ChA.Xportamnt+1),a
		ld	(ChA.Xportamnt+2),a
		inc	a
		ld	(ChA.Xornlen+1),a
		jp	ChA.readpatt

ChA.setport:	ld	a,(hl)
		inc	hl
		ld	(ChA.Xportamnt+1),a
		rla
		sbc	a,a
		ld	(ChA.Xportamnt+2),a
		jp	ChA.readpatt

ChA.setorn:	add	a,a
		exx
		ld	e,a
		ld	a,#0F
		ld	(ChA.Xampl+1),a
ChA.ornoffs:	ld	hl,NULL
		add	hl,de
		ld	sp,hl
		pop	hl
		ld	a,(hl)
		ld	(ChA.Xornloop+1),a
		inc	hl
		ld	a,(hl)
		ld	(ChA.Xornlen+1),a
		inc	hl
		ld	(ChA.Xornptr+1),hl
		ld	l,d
		ld	h,d
		ld	(ChA.Xportamnt+1),hl
		ld	a,#22
		ld	(ChA.Xinst3),a
		exx
		jp	ChA.readpatt

; ---------------------------------------------------------------------------
ChA.mute:	xor	a
		ld	(AYREG_AmplA),a
		ld	xl,9
		jp	ChB.ornpos

ChB.mute:	xor	a
		ld	(AYREG_AmplB),a
		ld	a,xl
		or	#12
		ld	xl,a
		jp	ChC.ornpos

ChC.mute:	xor	a
		ld	(AYREG_AmplC),a
		ld	a,xl
		or	#24
		ld	xl,a
		jp	retsp

; ---------------------------------------------------------------------------
ChC.nop:	ld	(ChC.datptr+1),hl
ChC.exit:	ld	a,0
		ld	(pattstep_cnt),a
ChB.Xsmpptr:	ld	hl,NULL
		ld	(ChB.smpptr+1),hl
ChB.Xornptr:	ld	hl,NULL
		ld	(ChB.ornptr+1),hl
ChB.Xpitch:	ld	hl,NULL
ChB.Xinst1:	ld	(ChB.pitch+1),hl
ChB.Xportamnt:	ld	hl,NULL
ChB.Xinst3:	ld	(ChB.portamnt+1),hl
ChB.Xornlen:	ld	a,0
		ld	(ChB.ornlen+1),a
ChB.Xsmplen:	ld	a,0
		ld	(ChB.smplen+1),a
ChB.Xornloop:	ld	a,0
		ld	(ChB.ornloop+1),a
ChB.Xsmploop:	ld	a,0
		ld	(ChB.smploop+1),a
ChB.Xvolume:	ld	a,0
		ld	(ChB.volume+1),a
ChB.Xampl:	ld	a,#0F
		ld	(ChB.ampl+1),a
ChB.Xnote:	ld	a,0
		ld	(ChB.note+1),a
ChB.Xsmppos:	ld	a,0
		cp	1
		jr	z,ChA.Xsmpptr
		ld	(ChB.smppos+1),a
ChA.Xsmpptr:	ld	hl,NULL
		ld	(ChA.smpptr+1),hl
ChA.Xornptr:	ld	hl,NULL
		ld	(ChA.ornptr+1),hl
ChA.Xpitch:	ld	hl,NULL
ChA.Xinst1:	ld	(ChA.pitch+1),hl
ChA.Xportamnt:	ld	hl,NULL
ChA.Xinst3:	ld	(ChA.portamnt+1),hl
ChA.Xornlen:	ld	a,0
		ld	(ChA.ornlen+1),a
ChA.Xsmplen:	ld	a,0
		ld	(ChA.smplen+1),a
ChA.Xornloop:	ld	a,0
		ld	(ChA.ornloop+1),a
ChA.Xsmploop:	ld	a,0
		ld	(ChA.smploop+1),a
ChA.Xvolume:	ld	a,0
		ld	(ChA.volume+1),a
ChA.Xampl:	ld	a,#0F
		ld	(ChA.ampl+1),a
ChA.Xnote:	ld	a,0
		ld	(ChA.note+1),a
ChA.Xsmppos:	ld	a,0
		cp	1
		jr	z,env.shape
		ld	(ChA.smppos+1),a
env.shape:	ld	a,0
		ld	(AYREG_EnvSh+1),a
env.period:	ld	a,0
		ld	(AYREG_Env+1),a
		xor	a
		ld	(env.shape+1),a
ChA.Xinst2:	ld	(ChA.ornpos+1),a
ChB.Xinst2:	ld	(ChB.ornpos+1),a
		inc	a
		ld	(ChA.Xsmppos+1),a
		ld	(ChA.Xinst2),a
		ld	(ChB.Xsmppos+1),a
		ld	(ChB.Xinst2),a
		ld	(ChA.Xinst1),a
		ld	(ChB.Xinst1),a

; ---------------------------------------------------------------------------
chkfadeout:	ld	a,0
		or	a
		jr	z,ChA.ornpos
		ld	a,(music_setup)
		rlca
		jp	c,retsp
countfadeout:	ld	a,0
		dec	a
		ld	(countfadeout+1),a
		jr	nz,ChA.ornpos
		ld	e,16
apply_volume:	ld	a,0
		inc	a
		cp	e
		jp	z,resetfadeout
		ld	(apply_volume+1),a
		ld	(ChA.attn+1),a
		ld	(ChB.attn+1),a
		ld	(ChC.attn+1),a
		bit	3,a			; if value of attenuation
		jr	z,divfadeout		; hits the 8 we turn off
		xor	a			; also hw envelopes
		ld	(env.shape+1),a
		ld	(AYREG_EnvSh+1),a
		ld	a,e
		dec	a
		ld	(ChA.ampl+1),a
		ld	(ChB.ampl+1),a
		ld	(ChC.ampl+1),a
divfadeout:	ld	a,0
		srl	a
		ld	e,a
		srl	e
		add	a,e
		jr	nz,divfadeout1
		inc	a
divfadeout1:	ld	(divfadeout+1),a
		add	a,a
		ld	(countfadeout+1),a

ChA.ornpos:	ld	de,0
ChA.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChA.ornlen:	cp	0
		jr	nz,ChA.readorn
ChA.ornloop:	ld	a,0
ChA.readorn:	ld	(ChA.ornpos+1),a
ChA.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChA.smppos:	ld	a,0
ChA.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChA.mute
		ld	e,a
		add	hl,de
		add	a,3
ChA.smplen:	cp	0
		jr	nz,ChA.readsmp
ChA.smploop:	ld	a,0
ChA.readsmp:	ld	(ChA.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChA.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChA.attn
		set	4,h
ChA.attn:	sub	0
		jr	nc,ChA.attnn
		xor	a
ChA.attnn:	or	h
ChA.ampl:	and	0
		ld	(AYREG_AmplA),a
		ld	a,e
		rlca
		jr	c,ChA.pitch
		ld	xh,d
ChA.pitch:	ld	hl,NULL
ChA.portamnt:	ld	de,0
		add	hl,de
		ld	(ChA.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		rlca
		rlca
		rlca
		and	#0F
		ld	xl,a
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonA),hl

ChB.ornpos:	ld	de,0
ChB.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChB.ornlen:	cp	0
		jr	nz,ChB.readorn
ChB.ornloop:	ld	a,0
ChB.readorn:	ld	(ChB.ornpos+1),a
ChB.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChB.smppos:	ld	a,NULL
ChB.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChB.mute
		ld	e,a
		add	hl,de
		add	a,3
ChB.smplen:	cp	0
		jr	nz,ChB.readsmp
ChB.smploop:	ld	a,0
ChB.readsmp:	ld	(ChB.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChB.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChB.attn
		set	4,h
ChB.attn:	sub	0
		jr	nc,ChB.attnn
		xor	a
ChB.attnn:	or	h
ChB.ampl:	and	0
		ld	(AYREG_AmplB),a
		ld	a,e
		rrca
		rrca
		rrca
		and	#1E
		or	xl
		ld	xl,a
		and	#10
		jr	nz,ChB.pitch
		ld	xh,d
ChB.pitch:	ld	hl,NULL
ChB.portamnt:	ld	de,0
		add	hl,de
		ld	(ChB.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonB),hl

ChC.ornpos:	ld	de,0
ChC.ornptr:	ld	hl,NULL
		add	hl,de
		inc	e
		ld	a,e
ChC.ornlen:	cp	0
		jr	nz,ChC.readorn
ChC.ornloop:	ld	a,0
ChC.readorn:	ld	(ChC.ornpos+1),a
ChC.note:	ld	a,0
		add	a,(hl)
		add	a,a
		ld	e,a
		ld	hl,freqtable
		add	hl,de
		ld	sp,hl
ChC.smppos:	ld	a,0
ChC.smpptr:	ld	hl,NULL
		inc	a
		jp	m,ChC.mute
		ld	e,a
		add	hl,de
		add	a,3
ChC.smplen:	cp	0
		jr	nz,ChC.readsmp
ChC.smploop:	ld	a,0
ChC.readsmp:	ld	(ChC.smppos+1),a
		pop	bc
		ld	sp,hl
		pop	de
		ld	h,0
		ld	a,e
		and	#0F
ChC.volume:	sub	0
		ld	l,a
		ccf
		sbc	a,a
		and	l
		srl	d
		jr	nc,ChC.attn
		set	4,h
ChC.attn:	sub	0
		jr	nc,ChC.attnn
		xor	a
ChC.attnn:	or	h
ChC.ampl:	and	0
		ld	(AYREG_AmplC),a
		ld	a,e
		rrca
		rrca
		and	#3C
		or	xl
		ld	xl,a
		and	#20
		jr	nz,ChC.pitch
		ld	xh,d
ChC.pitch:	ld	hl,NULL
ChC.portamnt:	ld	de,0
		add	hl,de
		ld	(ChC.pitch+1),hl
		add	hl,bc
		pop	bc
		add	hl,bc
		ld	a,h
		and	#0F
		ld	h,a
		ld	(AYREG_TonC),hl
@retsp:		ld	sp,0

outay:		ld	hl,AYREG_AmplC
		ld	de,#FFBF
		ld	bc,#FFFD
		ld	a,#0D
		out	(c),a
AYREG_EnvSh:	ld	a,0
		or	a
		jr	z,noenvelopes
		ld	b,e
		out	(c),a

		ld	a,#0B
		ld	b,d
		out	(c),a
		ld	b,e
AYREG_Env:	ld	a,0
		out	(c),a
		ld	b,d

noenvelopes:	ld	a,#0A
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		ld	a,xl
		out	(c),a
		ld	a,6
		ld	b,d
		out	(c),a
		ld	b,e
		ld	a,xh
		out	(c),a
		ld	a,5
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		dec	a
		ld	b,d
		out	(c),a
		ld	b,e
		outd
		ld	(AYREG_EnvSh+1),a
		ei
		ret

;------------------------------------------------------------------------------
freqtable	dw	#0EF8, #0E10, #0D60, #0C80, #0BD8, #0B28, #0A88, #09F0
		dw	#0960, #08E0, #0858, #07E0, #077C, #0708, #06B0, #0640
		dw	#05EC, #0594, #0544, #04F8, #04B0, #0470, #042C, #03F0
		dw	#03BE, #0384, #0358, #0320, #02F6, #02CA, #02A2, #027C
		dw	#0258, #0238, #0216, #01F8, #01DF, #01C2, #01AC, #0190
		dw	#017B, #0165, #0151, #013E, #012C, #011C, #010B, #00FC
		dw	#00EF, #00E1, #00D6, #00C8, #00BD, #00B2, #00A8, #009F
		dw	#0096, #008E, #0085, #007E, #0077, #0070, #006B, #0064
		dw	#005E, #0059, #0054, #004F, #004B, #0047, #0042, #003F
		dw	#003B, #0038, #0035, #0032, #002F, #002C, #002A, #0027
		dw	#0025, #0023, #0021, #001F, #001D, #001C, #001A, #0019
		dw	#0017, #0016, #0015, #0013, #0012, #0011, #0010, #000F

empty_orn:	db	#00, #00, #00
empty_pattern:	db	#70, #80, #F1, #D0, #00

AYREG_TonA:	dw	0
AYREG_TonB:	dw	0
AYREG_TonC:	dw	0
AYREG_AmplA:	db	0
AYREG_AmplB:	db	0
AYREG_AmplC:	db	0
tempocnt_2:	db	0
tempocnt_1:	db	0
tempocnt_0:	db	0
pattstep_cnt:	db	0

;-----------------------------------------------------------------------------			
			
			endmodule
			
			
songdata
MDLADDR
modstart 
BUFFER:			ds	2		; dlzka songu
RELOC_BASE:
I_SAMPLES:		ds	2		; adresa lookup tabulky samplov
I_ORNAMENTS:	ds	2		; adresa lookup tabulky ornamentov
I_PATTERNS:		ds	2		; adresa lookup tabulky patternov
I_POSITIONS:	ds	2		; adresa definicii pozicii
I_REPEAT:		ds	2		; ukazovatel na repeat poziciu
RELOC_ENDPTR:	ds	2		; tu je uz ukazovat na prvy sampel
LFNNAME ds 262
screen 			incbin	logo.bin


esxError        ds      34
			  CSPECTMAP player.map

              savenex open "NextPlayer.nex",demo,24575
			  
              savenex core 2,0,0
              ;SAVENEX SCREEN BMP "screen2.bmp"
			  savenex auto
              savenex close
                                    
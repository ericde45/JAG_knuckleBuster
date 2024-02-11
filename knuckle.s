; knucklebuster sur Jaguar
;
; "its not Zoids or Knucklebuster so ..... "
;			Cyrano Jones 10 november 2023, 11:22

; Colour Mapping 	RGB 3/3/3 [512 colour] 	RBG 5/5/6 [65536 colour]

; appel àa la routine de son ?
;			- init : metre = $4e75 (rts) en $108ae puis appel à 10000
;			- replay ?
;			- stop 



; - ré-écrire le scrolling
	; - convertir la fonte 1 plan vers une bloc en .b // $32 caractere // fonte en $2160c , fin=$2160c+($34*64) // format idéal de la fonte : 32 pixels * 16 lignes
	; - copier au blitter
	; - inserer un caractère a gauche
	; - lire le texte du scrolling , table de conversion en $10698
; OK - scrolling dans une autre zone / swap de zone de scrolling // le GPU doit attendre le scrolling en vbl // arranger le tout
; OK - convertir les blocs graphiques du batteur en .B 256c + table
; OK - routine pour chaque animation
; OK - supprimer les ecritures dans SR
; OK - executer le scrolling en VBL
; OK - copier la zone du scrolling par le GPU ( source = $77080 )
; OK - timer 1 au dsp
; OK - I2S au dsp
; OK - executer musique en main 68000 suivant flag timer 1 dsp
; OK - frequence + élevée, formule de recalcule des increments : ( frequence timer C / frequence jaguar ) * increment					 // 9906/DSP_Audio_frequence

increment_scrolling=4
TEMPS_GPU=0
copie_du_scrolling_au_GPU=1
message_CJ=1
scrolling_TEX=0
avancer_le_scrolling=1
numero_couleur_scrolling=128

; - ecran de 320x200 en 256c
; - object list à gerer
; - 

; - convertir le graph de $70000 en 256 couleurs jaguar
; - logo dans une zone de couleur +32
; - convertir zone scrolling 1 plan en zone 1 plan



AHX_nb_bits_virgule_increment_period		.equ			16		; mini freq = 16000 // freq amiga = 32000 // 32000/16000=2 => 2 bits // 128=>7 bits // 9 bits + signe : entiere = 10 bits
DSP_Audio_frequence					.equ			30000				; real hardware needs lower sample frequencies than emulators 




.text

			.68000


	include	"jaguar.inc"
GPU_STACK_SIZE	equ		4	; long words
GPU_USP			equ		(G_ENDRAM-(4*GPU_STACK_SIZE))
GPU_ISP			equ		(GPU_USP-(4*GPU_STACK_SIZE))	

; --------------------- DSP
DSP_STACK_SIZE	equ		64	; long words
DSP_USP			equ		(D_ENDRAM-(4*DSP_STACK_SIZE))
DSP_ISP			equ		(DSP_USP-(4*DSP_STACK_SIZE))
; --------------------- DSP

ob_list_1				equ		(ENDRAM-52000)				; address of read list =  
ob_list_2				equ		(ENDRAM-104000)				; address of read list =  
nb_octets_par_ligne			equ		320
nb_lignes								equ		200


zone3D_POSX = 0		; (16+16)		; 16+46
zone3D_POSY = 58+10-16			; (58+(91*2))
hauteur_zone_3D = 200-20

.opt "~Oall"

.text

			.68000




	move.l		#$70007,G_END
	move.l		#$70007,D_END
	move.l		#INITSTACK-128, sp	

	;move.w		#%001011000001,VMODE			; 9-10-11=001 / CRY   640*256
	
	move.w		#%0000011011000001, VMODE			; 320x256 / CRY / $6C7
	
	move.w		#$100,JOYSTICK


	move.w		#801,VI			; stop VI

; clear BSS
	lea			DEBUT_BSS,a0
	lea			ob_list_2,a1
	moveq		#0,d0
	
boucle_clean_BSS:
	move.l		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS
; clear stack
	lea			INITSTACK-100,a0
	lea			INITSTACK,a1
	moveq		#0,d0
	
boucle_clean_BSS2:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS2


; test
;	lea			buffer_double_scrolling1+50,a0
;	lea			buffer_double_scrolling2,a2
;	lea			zone_scrolling1+50,a1
;	move.w	#16-1,d0
;.un:
;	
;	move.b		#numero_couleur_scrolling,(a0)
;	move.b		#numero_couleur_scrolling+1,1(a0)
;	move.b		#numero_couleur_scrolling,2(a0)
;	move.b		#numero_couleur_scrolling+1,3(a0)
;	move.b		#numero_couleur_scrolling,5(a0)
;	move.b		#numero_couleur_scrolling+1,6(a0)
;	;move.b		#numero_couleur_scrolling,(a1)
;	lea				640(a0),a0
;	lea				320(a1),a1
;	dbf				d0,.un


;------------------- original
	;bsr				relocate_bloc_knuckle
	
	lea				$10000,a1
	lea				bloc_knuckle,a0
.copy:
	move.l		(a0)+,(a1)+
	cmp.l			#fin_bloc_knuckle,a0
	blt.s			.copy
	
	move.l		#$4e714e71,$107e2		; suppression move to SR
	move.w		#$4e75,$108ae				; = boucle principale
	move.w		#$4e75,$1080c				; = retour init
	;move.l		#$4e714e71,$107da						; vire appel init $60000 => casse tout :)
	
	;move.w			#$77,BORD1
; toutes les inits	
	jsr				$10000
	
; morceaux du batteur
		;moveq			#2+1,d0
		;jsr				$1093e
		;moveq			#0+1,d0
		;jsr				$10968
		;moveq			#4+1,d0
		;jsr				$10968
		;moveq			#6+1,d0
		;jsr				$10916
;------------------- original

	;move.w			#$77,BORD1


	bsr			copie_logo_dans_ecran_JAG
	lea		palette_logo_ST,a0	
	lea		FIN_palette_logo_ST,a1
	bsr			convert_CLUT_ST
	lea		palette_personnage_ST,a0	
	lea		FIN_palette_personnage_ST,a1
	bsr			convert_CLUT_ST


; wait for gpu to stop
	lea			G_CTRL,A0
wait_STOP_GPU:
	move.l		(a0),d0
	btst		#0,d0
	bne.s		wait_STOP_GPU


; copie du code GPU
	move.l	#0,G_CTRL
; copie du code GPU dans la RAM GPU
	lea		GPU_debut,A0
	lea		G_RAM,A1
	move.l	#GPU_fin-GPU_base_memoire,d0
	lsr.l	#2,d0
	sub.l	#1,D0
boucle_copie_bloc_GPU:
	move.l	(A0)+,(A1)+
	dbf		D0,boucle_copie_bloc_GPU


	move.l	#0,D_CTRL
; copie du code DSP dans la RAM DSP

	lea		code_DSP_debut,A0
	lea		D_RAM,A1
	move.l	#code_DSP_fin-DSP_base_memoire,d0
	lsr.l	#2,d0
	sub.l	#1,D0
boucle_copie_bloc_DSP:
	move.l	(A0)+,(A1)+
	dbf		D0,boucle_copie_bloc_DSP
	

	
	bsr		RAPTOR_InitVideo

; creer les object listes
	lea		ob_list_1,a6
	bsr			preparation_OL
	lea		ob_list_2,a6
	bsr			preparation_OL

;; ici CLUT
; Colour Mapping 	RGB 3/3/3 [512 colour] 	RBG 5/5/6 [65536 colour]
; ST : xRRRxGGGxBBB
	lea			CLUT+(32*2),a1
	lea			palette_logo_ST,a0
	move.w		#16-1,d7
clut1:	
	move.w		(a0)+,(a1)+
	dbf				d7,clut1

	lea			CLUT,a1
	lea			palette_personnage_ST,a0
	move.w		#16-1,d7
clut2:	
	move.w		(a0)+,(a1)+
	dbf				d7,clut2

; couleur scrolling
	move.w		#$00,CLUT+(numero_couleur_scrolling*2)
	move.w		#$FFFF,CLUT+((numero_couleur_scrolling+1)*2)

; launch GPU
	move.l	#REGPAGE,G_FLAGS
	move.l	#GPU_init,G_PC
	move.l  #RISCGO,G_CTRL	; START GPU

	bsr				convertir_cymballes
	bsr				convertir_bouches
	bsr				convertir_caisses
	bsr				convertir_cgauche
	
	bsr				conversion_fonte_1plan_scrolling
	bsr				conversion_texte_scrolling

	.if				message_CJ=1
; attends 20 secondes
	move.w			#10*60,d7
attends20sec1:
	move.l			vbl_counter_GPU,d0
attends20sec2:	
	move.l			vbl_counter_GPU,d1
	cmp.l			d0,d1
	beq.s			attends20sec2
	dbf				d7,attends20sec1
	.endif


; cls rapide
 .wait:
    move.l   B_CMD,d0             ;; wait for blitter to finish
    ror.w    #1,d0                ;; Check if blitter is idle
    bcc.b    .wait                ;; bit was clear -> busy
 
    move.l   image_CJ,A1_BASE       ;; point to destination
    move.l   #$0,A1_PIXEL         ;; start in front
    move.l   #PIXEL16|XADDPHR|PITCH1,A1_FLAGS ;; use 16bit pixels/phrasemode
    move.l   #0,B_PATD            ;; our value to draw
 
    move.w   #1,d0                ;; we wanna draw 1 'line'
    swap     d0                   ;; in upper word
    move.w   #((320*77)/2)/4,d0          ;; and 256 bytes == 128 16-bit-pixels
    move.l   d0,B_COUNT           ;; == 32 phrases
 
    move.l   #PATDSEL,B_CMD       ;; use B_PATD to draw and GO


	move.l			#1,GPU_ecran_de_presentation



	move.w		#%0000011011000111, VMODE			; 320x256 / RGB / $6C7


	.if				scrolling_TEX=1
; mise en place VBL
	move.w  	#%0,INT1                 	; Disable video interrupts 11101
	lea				VBL,a0
	move.l  		a0,LEVEL0     	; Install 68K LEVEL0 handler
	move.w  	a_vde,d0                	; Must be ODD
	sub.w   		#16,d0
	;move.w		#40,d0			; forcé
	ori.w   		#1,d0
	move.w  	d0,VI
	move.w  	#%01,INT1                 	; Enable video interrupts 11101
	;and.w   #%1111100011111111,sr				; 1111100011111111 => bits 8/9/10 = 0
	and.w   	#$f8ff,sr
	.endif

;init music
	jsr			$12fe6

; replay music
				jsr			$1248a
				jsr			$125be


; launch DSP
	move.l	#REGPAGE,D_FLAGS
	move.l	#DSP_routine_init_DSP,D_PC
	move.l	#DSPGO,D_CTRL

	
	; valeurs dans le code				
;sample1 = ($12e98)									(lire 1310c.w) etendu en .l +1310c
;table volume1 = (lire 13138.w) etendu en .l +13138;
;offset = 0
;increment1 = (lire 13110.w) etendu en .l 				

	
	
PLAY_ON=1
main:
				cmp.l			#1,AHX_DSP_flag_timer1
				bne.s			main
				
				;move.w		#$FFFF,BG
				
				.if				PLAY_ON=1
				
				jsr			$1248a
				jsr			$125be

				move.l		#0,AHX_DSP_flag_timer1

	
				moveq		#0,d0
				add.w			$10878,d0
				cmp.w		old_1,d0
				beq.s			main_pas_de_changement1
				move.w		d0,old_1
				bsr				copie_cymballe
	main_pas_de_changement1:			
		
				moveq		#0,d0
				add.w			$10880,d0
				cmp.w		old_2,d0
				beq.s			main_pas_de_changement2
				move.w		d0,old_2
				bsr				copie_bouche
main_pas_de_changement2:					
				
				moveq		#0,d0
				add.w			$1087c,d0
				cmp.w		old_3,d0
				beq.s			main_pas_de_changement3				
				move.w		d0,old_3
				bsr				copie_48pixels_caisse
main_pas_de_changement3:					
		
				moveq		#0,d0
				add.w			$10874,d0
				cmp.w		old_4,d0
				beq.s			main_pas_de_changement4
				move.w		d0,old_4				
				bsr				copie_48pixels_cgauche
main_pas_de_changement4:			
	
				

; valeurs dans le code				
;sample1 = ($12e98)									(lire 1310c.w) etendu en .l +1310c
;table volume1 = (lire 13138.w) etendu en .l +13138;
;offset = 0
;increment1 = (lire 13110.w) etendu en .l 				
				;move.l		$12e98,d0			; sample1
				;move.w		$13138,d1			; DSP_pointeur_sur_table_volume_voie1
				;ext.l			d1
				;add.l			#$13138,d1
				;moveq		#0,d2						; DSP_pointeur_sur_offset_voie1
				;move.l		$13110,d3			; DSP_pointeur_sur_increment1_16_16
				;move.l		$12e98+6,d4			; sample1
				;movem.l	d0-d4,DSP_pointeur_sur_sample_voie1
;
				;move.l		$12efc,d0			; sample2
				;move.w		$1316a+2,d1			; DSP_pointeur_sur_table_volume_voie2
				;ext.l			d1
				;add.l			#$1316a+2,d1
				;moveq		#0,d2						; DSP_pointeur_sur_offset_voie2
				;move.l		$13142+2,d3			; DSP_pointeur_sur_increment2_16_16
				;move.l		$12efc+6,d4			; sample2
				;movem.l	d0-d4,DSP_pointeur_sur_sample_voie2
				;
				;move.l		$12f60,d0			; sample3
				;move.w		$131a0+2,d1			; DSP_pointeur_sur_table_volume_voie3
				;ext.l			d1
				;add.l			#$131a0+2,d1
				;moveq		#0,d2						; DSP_pointeur_sur_offset_voie2
				;move.l		$13178+2,d3			; DSP_pointeur_sur_increment3_16_16
				;move.l		$12f60+6,d4			; sample3
				;movem.l	d0-d4,DSP_pointeur_sur_sample_voie3

				.endif
				
				;move.w		#$0,BG


				bra		main

old_1:		dc.w				0
old_2:		dc.w				0
old_3:		dc.w				0
old_4:		dc.w				0

	.if				scrolling_TEX=1
; VBL
VBL:
				;move.w			#$0FFF,BORD1
                movem.l d0-d7/a0-a6,-(a7)
				jsr				$10004						; appel scrolling 1 plan
				move.l		vbl_counter,d0
				addq.l			#1,d0
				move.l		d0,vbl_counter
                movem.l (a7)+,d0-d7/a0-a6
				move.l			#1,GPU_flag_scrolling_fait
				;move.w			#$0FF,BORD1
                move.w  	#$101,INT1              	; Signal we're done
				move.w  	#$0,INT2
				rte

		.endif

copie_logo_dans_ecran_JAG:
; copie un graph ST vers un ecran JAG en 256 couleurs chunky
	lea				$70000,a0
	lea				ecran_logo,a1
	move.w		#180-1,d0							; 57 lignes = logo
copie_logo_dans_ecran_JAG__ligne:
	move.w		#(320/16)-1,d1
copie_logo_dans_ecran_JAG__16pixels:
	moveq		#15,d7
copie_logo_dans_ecran_JAG__1pixel:

	move.w		(a0),d2
	move.w		2(a0),d3
	move.w		4(a0),d4
	move.w		6(a0),d5
	
	lsr.w			d7,d2
	lsr.w			d7,d3
	lsr.w			d7,d4
	lsr.w			d7,d5
	and.w			#%1,d2
	and.w			#%1,d3
	and.w			#%1,d4
	and.w			#%1,d5
	
	lsl.w			#1,d3
	lsl.w			#2,d4
	lsl.w			#3,d5
	
	
	add.w			d2,d3
	add.w			d3,d4
	add.w			d4,d5
	move.b		d5,(a1)+
	subq.w		#1,d7
	cmp.w		#0,d7
	bge.s			copie_logo_dans_ecran_JAG__1pixel
	lea				8(a0),a0
	dbf				d1,copie_logo_dans_ecran_JAG__16pixels
	dbf				d0,copie_logo_dans_ecran_JAG__ligne

; +32 sur les 57 premiers lignes	
	lea				ecran_logo,a1
	move.w		#(57*320)-1,d7
plus_32_pour_logo:	
	move.b		(a1),d0
	add.b			#32,d0
	move.b			d0,(a1)+	
	dbf				d7,plus_32_pour_logo
	
	rts


conversion_texte_scrolling:
	lea				$1015c,a0
	lea				$10698,a1
	moveq		#0,d0
conversion_texte_scrolling__boucle:	
	move.b		(a0),d0
	cmp.b		#$ff,d0
	beq.s			.sortie
	move.b		(a1,d0.w),d0
	move.b		d0,(a0)+
	bra.s			conversion_texte_scrolling__boucle
.sortie:
	rts
	

;-------------------------------------
; des couleurs sont au format ST
; Bits [0-5] are green, bits [6-10] are blue and bits [11-15] are red.
reglage_fin_palette=0
convert_CLUT_ST:
convert_CLUT_ST_boucle:
	moveq	#0,d6						; d6 = couleur JAGUAR RGB 16
	move.w	(a0),d1						; d1=$0777
; R
	move.w	d1,d2
	and.w	#$0700,d2						; D2 = R / 3 bits
	lsr.w	#8,d2
	lsl.w	#8,d2
	lsl.w	#13-8-reglage_fin_palette,d2							; bits 13 14 15
	move.w	d2,d6
; G
	move.w	d1,d2
	and.w	#$0070,d2						; D2 = G / 3 bits
	lsr.w	#4,d2
	lsl.w	#3-reglage_fin_palette,d2							; bits 3 4 5
	or.w	d2,d6
;B
	move.w	d1,d2
	and.w	#$0007,d2						; D2 = B / 3 bits
	;lsr.w	#4,d2
	lsl.w	#8-reglage_fin_palette,d2							; bits 8 9 10
	or.w	d2,d6

	move.w	d6,(a0)+
	cmp.l	a1,a0
	bne.s	convert_CLUT_ST_boucle

	rts

;------------------------------
convertir_cymballes:
; cymballe1
	lea				$113ea,a0
	lea				bloc_cymballe1,a1
	bsr				convertir_cymballes__sous_routine
; cymballe2
	lea				$11792,a0
	lea				bloc_cymballe2,a1
	bsr				convertir_cymballes__sous_routine
	rts
	
convertir_cymballes__sous_routine:
	move.w		#39-1,d7
convertir_cymballes_boucle1:
	lea				8(a0),a0
	move.w		#2-1,d5
convertir_cymballes_boucle2:
	move.w		#15,d6
convertir_cymballes_boucle1_16pixels:
	move.w		(a0),d1
	move.w		2(a0),d2
	move.w		4(a0),d3
	move.w		6(a0),d4
	lsr.w			d6,d1
	lsr.w			d6,d2
	lsr.w			d6,d3
	lsr.w			d6,d4
	and.w			#%1,d1
	and.w			#%1,d2
	and.w			#%1,d3
	and.w			#%1,d4
	lsl.w			#1,d2
	lsl.w			#2,d3
	lsl.w			#3,d4
	add.w			d2,d1
	add.w			d3,d1
	add.w			d4,d1
	move.b		d1,(a1)+
	subq.w		#1,d6
	cmp.w		#0,d6
	bge.s			convertir_cymballes_boucle1_16pixels
	lea				8(a0),a0
	dbf				d5,convertir_cymballes_boucle2
	dbf				d7,convertir_cymballes_boucle1
	rts

copie_cymballe:
	lea					bloc_cymballe1,a0
	move.w			#39-1,d7
	lea					ecran_logo+(72*320)+192,a1
	cmp.w			#0,d0
	beq.s				.cymballe1
	lea					bloc_cymballe2,a0
.cymballe1:
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,16(a1)
	lea					320(a1),a1
	dbf					d7,.cymballe1
	rts

;------------------------------
convertir_bouches:
; bouche1
	lea				$1210a,a0
	lea				bloc_bouche1,a1
	bsr				convertir_bouche__sous_routine
; bouche2
	lea				$122aa,a0
	lea				bloc_bouche2,a1
	bsr				convertir_bouche__sous_routine
	rts

convertir_bouche__sous_routine:
	move.w		#26-1,d7
convertir_bouche_boucle1:
	move.w		#2-1,d5
convertir_bouche_boucle2:
	move.w		#15,d6
convertir_bouche_boucle1_16pixels:
	move.w		(a0),d1
	move.w		2(a0),d2
	move.w		4(a0),d3
	move.w		6(a0),d4
	lsr.w			d6,d1
	lsr.w			d6,d2
	lsr.w			d6,d3
	lsr.w			d6,d4
	and.w			#%1,d1
	and.w			#%1,d2
	and.w			#%1,d3
	and.w			#%1,d4
	lsl.w			#1,d2
	lsl.w			#2,d3
	lsl.w			#3,d4
	add.w			d2,d1
	add.w			d3,d1
	add.w			d4,d1
	move.b		d1,(a1)+
	subq.w		#1,d6
	cmp.w		#0,d6
	bge.s			convertir_bouche_boucle1_16pixels
	lea				8(a0),a0
	dbf				d5,convertir_bouche_boucle2
	dbf				d7,convertir_bouche_boucle1
	rts


copie_bouche:
	lea					bloc_bouche1,a0
	move.w			#26-1,d7
	lea					ecran_logo+(57*320)+160,a1
	cmp.w			#0,d0
	beq.s				.bouche1
	lea					ecran_logo+(56*320)+160,a1
	lea					bloc_bouche2,a0
.bouche1:
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,16(a1)
	lea					320(a1),a1
	dbf					d7,.bouche1
	rts


convertir_caisses:
	lea				$11b3a,a0
	lea				bloc_caisse1,a1
	move.w		#31-1,d7
	bsr				convertir_caisse__sous_routine

	lea				$11e22,a0
	lea				bloc_caisse2,a1
	move.w		#31-1,d7
	bsr				convertir_caisse__sous_routine
	rts
	
convertir_cgauche:
	lea				$10c0a,a0
	lea				bloc_cgauche1,a1
	move.w		#42-1,d7
	bsr				convertir_caisse__sous_routine

	lea				$10ffa,a0
	lea				bloc_cgauche2,a1
	move.w		#42-1,d7
	bsr				convertir_caisse__sous_routine
	rts
	

convertir_caisse__sous_routine:
convertir_caisse_boucle1:
	move.w		#3-1,d5
convertir_caisse_boucle2:
	move.w		#15,d6
convertir_caisse_boucle1_16pixels:
	move.w		(a0),d1
	move.w		2(a0),d2
	move.w		4(a0),d3
	move.w		6(a0),d4
	lsr.w			d6,d1
	lsr.w			d6,d2
	lsr.w			d6,d3
	lsr.w			d6,d4
	and.w			#%1,d1
	and.w			#%1,d2
	and.w			#%1,d3
	and.w			#%1,d4
	lsl.w			#1,d2
	lsl.w			#2,d3
	lsl.w			#3,d4
	add.w			d2,d1
	add.w			d3,d1
	add.w			d4,d1
	move.b		d1,(a1)+
	subq.w		#1,d6
	cmp.w		#0,d6
	bge.s			convertir_caisse_boucle1_16pixels
	lea				8(a0),a0
	dbf				d5,convertir_caisse_boucle2
	dbf				d7,convertir_caisse_boucle1
	rts


copie_48pixels_caisse:
	lea					bloc_caisse1,a0
	move.w			#31-1,d7
	lea					ecran_logo+(116*320)+144,a1
	cmp.w			#0,d0
	beq.s				.caisse1
	lea					bloc_caisse2,a0
.caisse1:
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,16(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,32(a1)
	lea					320(a1),a1
	dbf					d7,.caisse1
	rts

copie_48pixels_cgauche:
	lea					bloc_cgauche1,a0
	move.w			#42-1,d7
	lea					ecran_logo+(71*320)+112,a1
	cmp.w			#0,d0
	beq.s				.cgauche1
	lea					bloc_cgauche2,a0
.cgauche1:
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,16(a1)
	movem.l		(a0)+,d0-d3
	movem.l		d0-d3,32(a1)
	lea					320(a1),a1
	dbf					d7,.cgauche1
	rts
	

conversion_fonte_1plan_scrolling:
; etape intermediaire on enleve les colonnes de 16 pixels
	lea			$2160c,a0
	lea			buffer_temporatire_fonte1P,a1
	move.w		#$34-1,d7
conversion_fonte_1plan_scrolling__inter:	
	move.w		#16-1,d6
	move.l		a0,a2
conversion_fonte_1plan_scrolling__inter_1lettre:	
	move.w		(a0),d0
	move.w		16*2(a0),d1
	move.w		d0,(a1)+
	move.w		d1,(a1)+
	lea				2(a0),a0
	dbf				d6,conversion_fonte_1plan_scrolling__inter_1lettre
	lea				16*2(a0),a0
	dbf				d7,conversion_fonte_1plan_scrolling__inter


; couleurs 128 et 129
	; - convertir la fonte 1 plan vers une bloc en .b // $32 caractere // fonte en $2160c , fin=$2160c+($32*64) = $2228c
	lea			buffer_temporatire_fonte1P,a0
	lea			fonte1P_256c,a1
	move.w		#$34-1,d7				; $34 caracteres
conversion_fonte_1plan_scrolling__boucle_caractere:	
	move.w		#16-1,d4		; lignes
conversion_fonte_1plan_scrolling__boucle_ligne:
	move.w		#2-1,d5
conversion_fonte_1plan_scrolling__boucle_32pixels:
		move.w		#15,d6
	conversion_fonte_1plan_scrolling__boucle_16pixels:		
			move.w		(a0),d1
			lsr.w			d6,d1
			and.w			#%1,d1
			add.w			#numero_couleur_scrolling,d1
			move.b		d1,(a1)+
			subq.w		#1,d6
			cmp.w		#0,d6
			bge.s			conversion_fonte_1plan_scrolling__boucle_16pixels			; A1+16
		lea				2(a0),a0			; ligne suivante
		dbf				d5,conversion_fonte_1plan_scrolling__boucle_32pixels			; A1+(16*16)
	dbf				d4,conversion_fonte_1plan_scrolling__boucle_ligne
	dbf				d7,conversion_fonte_1plan_scrolling__boucle_caractere
	rts




;-----------------------------------------------------------------------------------
; preparation de l'Objects list
;   Condition codes (CC):
;
;       Values     Comparison/Branch
;     --------------------------------------------------
;        000       Branch on equal            (VCnt==VC)
;        001       Branch on less than        (VCnt>VC)
;        010       Branch on greater than     (VCnt<VC)
;        011       Branch if OP flag is set
; input A6=adresse object list 
preparation_OL:
	move.l	a6,a1
	;lea		ob_list_1,a1

;
; ============== insertion de Branch if YPOS < 0 a X+16

	move.l		#$00000003,d0					; branch
	or.l		#%0100000000000000,d0			; <
	move.l		GPU_premiere_ligne,d3
	;add.l		d3,d3							; *2 : half line
	lsl.l		#3,d3
	or.l		d3,d0							; Ymax	

	move.l		a1,d1
	add.l		#16,d1
	lsr.l		#3,d1							
	move.l		d1,d2
	lsl.l		#8,d1							; <<24 : 8 bits
	lsl.l		#8,d1
	lsl.l		#8,d1
	or.l		d1,d0
	lsr.l		#8,d2
	move.l		d2,(a1)+
	move.l		d0,(a1)+

; ============== insertion de Branch if YPOS < Ymax à X+16

	move.l		#$00000003,d0					; branch
	or.l		#%0100000000000000,d0			; <
	;move.l		#derniere_ligne,d3
	;add.l		d3,d3							; *2 : half line
	;moveq		#0,d3
	move.l		GPU_derniere_ligne,d3
	;add.l		d3,d3							; *2 : half line
	lsl.l		#3,d3
	or.l		d3,d0							; Ymax	
	move.l		a1,d1
	add.l		#16,d1
	lsr.l		#3,d1							
	move.l		d1,d2
	lsl.l		#8,d1							; <<24 : 8 bits
	lsl.l		#8,d1
	lsl.l		#8,d1
	or.l		d1,d0
	lsr.l		#8,d2
	move.l		d2,(a1)+
	move.l		d0,(a1)+

; ============== insertion de STOP
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#4,d0
	move.l		d0,(a1)+
	
	lea			48(a6),a2

; insertion d'un bra < GPU_derniere_ligne
	move.l		#$00000003,d0					; branch
	or.l		#%0100000000000000,d0			; <
	move.l		GPU_derniere_ligne,d4
	subq.l		#2,d4
	;add.l		d4,d4							; (*2 : half line)
	lsl.l		#3,d4
	or.l		d4,d0							; VC
	move.l		a2,d1
	lsr.l		#3,d1							
	move.l		d1,d2
	lsl.l		#8,d1							; <<24 : 8 bits
	lsl.l		#8,d1
	lsl.l		#8,d1
	or.l		d1,d0
	lsr.l		#8,d2
	move.l		d2,(a1)+
	move.l		d0,(a1)+

; insertion GPU object
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#$3FFA,d0				; $3FFA
	move.l		d0,(a1)+
	
; insertion de STOP
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#4,d0
	move.l		d0,(a1)+

; insertion de STOP
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#4,d0
	move.l		d0,(a1)+
; insertion de STOP
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#4,d0
	move.l		d0,(a1)+

; insertion de STOP
	moveq		#0,d0
	move.l		d0,(a1)+
	move.l		#4,d0
	move.l		d0,(a1)+


; insertion de STOP
	moveq		#0,d0
	move.l		d0,(a2)+
	move.l		#4,d0
	move.l		d0,(a2)+

	rts




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Procedure: InitVideo 
;;
RAPTOR_InitVideo:
            movem.l d0-d6,-(sp)
            move.w  CONFIG,d0                            ; Also is joystick register
            andi.w  #VIDTYPE,d0                          ; 0 = PAL, 1 = NTSC
            beq.s    .palvals
            
.ntscvals:    move.w  #RAPTOR_NTSC_HMID,d2
            move.w  #RAPTOR_NTSC_WIDTH,d0
            move.w  #RAPTOR_NTSC_VMID,d6
            move.w  #RAPTOR_NTSC_HEIGHT,d4
            ;move.w    #8,raptor_topclip_val

		move.l	#26-26+8,GPU_premiere_ligne
		move.l	#508-26+8,GPU_derniere_ligne
		move.l	#60,_50ou60hertz	

            bra.s     .calc_vals
.palvals:    move.w     #RAPTOR_PAL_HMID,d2
            move.w     #RAPTOR_PAL_WIDTH,d0
            move.w     #RAPTOR_PAL_VMID+30,d6                        ; +30  322
            move.w     #RAPTOR_PAL_HEIGHT,d4

		move.l	#26+16+16,GPU_premiere_ligne
		move.l	#(256*2)+26+16+16,GPU_derniere_ligne
		move.l	#60,_50ou60hertz	


            ;move.w    #8,raptor_topclip_val
.calc_vals:    ;move.w  d0,raptor_width
            ;move.w  d4,raptor_height
            move.w  d0,d1
            asr     #1,d1                               ; Width/2
            sub.w   d1,d2                               ; Mid - Width/2
            add.w   #4,d2                               ; (Mid - Width/2)+4
            sub.w   #1,d1                               ; Width/2 - 1
            ori.w   #$400,d1                            ; (Width/2 - 1)|$400
            ;move.w  d1,raptor_a_hde
	        move.w  d1,a_hde
            move.w  d1,HDE
            ;move.w  d2,raptor_a_hdb
			move.w  d2,a_hdb
            move.w  d2,HDB1
            move.w  d2,HDB2
            move.w  d6,d5
            sub.w   d4,d5
            add.w   #16,d5
			move.w  d5,a_vdb
            ;move.w  d5,raptor_a_vdb
            add.w   d4,d6
			move.w  d6,a_vde
            ;move.w  d6,raptor_a_vde
            move.w  d5,VDB
            move.w  #$ffff,VDE
            ; move.w  #couleur_de_fond_BORD1,BORD1                            ; Black border
            ; move.w  #couleur_de_fond_BORD2,BORD2                            ; Black border
            ; move.w  #couleur_de_fond,BG                               ; Init line buffer to black
			
			; force ntsc pour pal
			
            movem.l (sp)+,d0-d6
            rts
			
			
;-------------------------------------------
;      VIDEO INITIALIZATION CONSTANTS
;-------------------------------------------

;NTSC_WIDTH      EQU     1409            ; Width of screen in pixel clocks
;NTSC_HMID       EQU     823             ; Middle of screen in pixel clocks
;NTSC_HEIGHT     EQU     241             ; Height of screen in scanlines
;NTSC_VMID       EQU     266             ; Middle of screen in halflines
;PAL_WIDTH       EQU     1381            ; Same as above for PAL...
;PAL_HMID        EQU     843
;PAL_HEIGHT      EQU     287
;PAL_VMID        EQU     322


; 320 PIXELS WIDE
RAPTOR_NTSC_WIDTH      EQU     1229               ; Width of screen in pixel clocks
RAPTOR_NTSC_HMID       EQU     787                ; Middle of screen in pixel clocks
RAPTOR_NTSC_HEIGHT     EQU     241				; 225             ; Height of screen in scanlines
RAPTOR_NTSC_VMID       EQU     266             ; Middle of screen in halflines

; 3.923295454545455
RAPTOR_PAL_WIDTH       EQU     1255            ; Same as above for PAL...
RAPTOR_PAL_HMID        EQU     821
RAPTOR_PAL_HEIGHT      EQU     256				; 225
RAPTOR_PAL_VMID        EQU     322






	


relocate_bloc_knuckle:
	move.l	label_eac,a0
	move.l	label_eb0,a1
	move.l	-(a1),a5
	add.l	a0,a5
	move.l	a5,$eb0
	move.l	-(a1),d1
	cmp.w	#0,-(a1)
	bne	label_e9c
label_e6e:
	dbra	d1,label_e74
	rts
label_e74:
	move.w	-(a1),d0
	move.l	-(a1),a3
	move.l	-(a1),a2
	add.l	a0,a2
	add.l	a0,a3
	cmp.l	a3,a5
	beq	label_e8c
label_e84:
	move.w	-(a1),-(a5)
	cmp.l	a5,a3
	bne	label_e84
label_e8c:
	move.w	d0,-(a5)
	cmp.l	a5,a2
	bne	label_e8c
	cmp.l	a5,a0
	bne	label_e6e
	rts
label_e9c:
	move.l		-(a1),a2
	add.l			a0,a2
label_ea0:
	move.w		-(a1),-(a5)
	cmp.l			a2,a5
	bne				label_ea0
	bra				label_e6e
label_eac:
	dc.l		bloc_knuckle
label_eb0:
	dc.l		bloc_knuckle+$badc
	
	.even



;------------------------------------------------------------
;-------------------------------------
;
;     DSP
;
;-------------------------------------
;------------------------------------------------------------





	.phrase

code_DSP_debut:
	.dsp
	.org	D_RAM


DSP_base_memoire:

AHX__I2S__source_sample_voie1			.equr			R0
AHX__I2S__pointeur_table_vol1				.equr			R1
AHX__I2S__source_offset1							.equr			R2
AHX__I2S__valeur_offset1							.equr			R3
AHX__I2S__increment1									.equr			R4

AHX__I2S__source_sample_voie2			.equr			R5
AHX__I2S__pointeur_table_vol2				.equr			R6
AHX__I2S__source_offset2							.equr			R7
AHX__I2S__valeur_offset2							.equr			R8
AHX__I2S__increment2									.equr			R9

AHX__I2S__source_sample_voie3			.equr			R10
AHX__I2S__pointeur_table_vol3				.equr			R11
AHX__I2S__source_offset3							.equr			R12
AHX__I2S__valeur_offset3							.equr			R13
AHX__I2S__increment3									.equr			R14

AHX__I2S__sample1										.equr			R15
AHX__I2S__sample2										.equr			R16
AHX__I2S__sample3										.equr			R17
AHX__I2S__valeur80										.equr			R18
AHX__I2S__multiplie										.equr			R19

AHX__I2S__pointeur_source_sample_voie1			.equr			R20
AHX__I2S__pointeur_source_sample_voie2			.equr			R21
AHX__I2S__pointeur_source_sample_voie3			.equr			R22
AHX__I2S__debug								.equr			R23				; <<===========
AHX__I2S__tmp_1								.equr			R24
AHX__I2S__tmp_2								.equr			R25
AHX__I2S__tmp_3								.equr			R26

AHX_I2S_pointeur_DAC_voie_G		.equr			R27
AHX_I2S_pointeur_DAC_voie_D		.equr			R28
AHX__I2S__save_flags						.equr			R29
; R30 utilisé
; R31 utilisé



; CPU interrupt
	.rept	8
		nop
	.endr
; I2S interrupt
	movei	#AHX_I2S_N_voies,AHX__I2S__tmp_1						; 6 octets
	movei	#D_FLAGS,r30											; 6 octets
	jump		(AHX__I2S__tmp_1)													; 2 octets
	load		(r30),AHX__I2S__save_flags	; read flags								; 2 octets = 16 octets
	nop
	nop
; Timer 1 interrupt
	movei	#DSP_LSP_routine_interruption_Timer1,AHX__I2S__tmp_1						; 6 octets
	movei	#D_FLAGS,r30											; 6 octets
	jump	(AHX__I2S__tmp_1)													; 2 octets
	load	(r30),AHX__I2S__save_flags	; read flags								; 2 octets = 16 octets
; Timer 2 interrupt	
;	.rept	8
;		nop
;	.endr
; External 0 interrupt
;	.rept	8
;		nop
;	.endr
; External 1 interrupt
;	.rept	8
;		nop
;	.endr


; I2S:
; version sans adaptation du volume, avec SAT
AHX_I2S_N_voies:
; pointeur sur le sample de la voie
; pointeur sur la table de volume de la voie
; pointeur sur l'increment de la voie
	movei			#$14556,AHX__I2S__debug

	movei			#DSP_pointeur_sur_sample_voie2,AHX__I2S__pointeur_source_sample_voie2
	movei			#DSP_pointeur_sur_sample_voie1,AHX__I2S__pointeur_source_sample_voie1
	load				(AHX__I2S__pointeur_source_sample_voie2),AHX__I2S__source_sample_voie2
	movei			#DSP_pointeur_sur_sample_voie3,AHX__I2S__pointeur_source_sample_voie3
	load				(AHX__I2S__pointeur_source_sample_voie1),AHX__I2S__source_sample_voie1
	movei			#$80,AHX__I2S__valeur80
	load				(AHX__I2S__pointeur_source_sample_voie3),AHX__I2S__source_sample_voie3

	movei			#DSP_pointeur_sur_table_volume_voie1,AHX__I2S__tmp_1
	movei			#DSP_pointeur_sur_table_volume_voie2,AHX__I2S__tmp_2
	load				(AHX__I2S__tmp_1),AHX__I2S__pointeur_table_vol1
	movei			#DSP_pointeur_sur_table_volume_voie3,AHX__I2S__tmp_3
	load				(AHX__I2S__tmp_2),AHX__I2S__pointeur_table_vol2
	movei			#-6,AHX__I2S__multiplie
	load				(AHX__I2S__tmp_3),AHX__I2S__pointeur_table_vol3

	movei			#DSP_pointeur_sur_offset_voie1,AHX__I2S__source_offset1
	movei			#DSP_pointeur_sur_offset_voie2,AHX__I2S__source_offset2
	load				(AHX__I2S__source_offset1),AHX__I2S__valeur_offset1
	movei			#DSP_pointeur_sur_offset_voie3,AHX__I2S__source_offset3
	load				(AHX__I2S__source_offset2),AHX__I2S__valeur_offset2
	load				(AHX__I2S__source_offset3),AHX__I2S__valeur_offset3

	movei			#DSP_pointeur_sur_increment1_16_16,AHX__I2S__tmp_1
	movei			#DSP_pointeur_sur_increment2_16_16,AHX__I2S__tmp_2
	load				(AHX__I2S__tmp_1),AHX__I2S__increment1
	movei			#DSP_pointeur_sur_increment3_16_16,AHX__I2S__tmp_3
	load				(AHX__I2S__tmp_2),AHX__I2S__increment2
	load				(AHX__I2S__tmp_3),AHX__I2S__increment3

	add					AHX__I2S__increment1,AHX__I2S__valeur_offset1
	add					AHX__I2S__increment2,AHX__I2S__valeur_offset2
	add					AHX__I2S__increment3,AHX__I2S__valeur_offset3

	store				AHX__I2S__valeur_offset1,(AHX__I2S__source_offset1)
	store				AHX__I2S__valeur_offset2,(AHX__I2S__source_offset2)
	store				AHX__I2S__valeur_offset3,(AHX__I2S__source_offset3)

	shrq				#16,AHX__I2S__valeur_offset1
	shrq				#16,AHX__I2S__valeur_offset2
	shrq				#16,AHX__I2S__valeur_offset3

	add					AHX__I2S__source_sample_voie1,AHX__I2S__valeur_offset1
	add					AHX__I2S__source_sample_voie2,AHX__I2S__valeur_offset2
	add					AHX__I2S__source_sample_voie3,AHX__I2S__valeur_offset3

	loadb				(AHX__I2S__valeur_offset1),AHX__I2S__sample1			; sample sans volume
	loadb				(AHX__I2S__valeur_offset2),AHX__I2S__sample2
	loadb				(AHX__I2S__valeur_offset3),AHX__I2S__sample3

;  test bouclage 1		
	movei		#.cont_v1,AHX__I2S__tmp_2
	cmpq			#0,AHX__I2S__sample1
	jump			ne,(AHX__I2S__tmp_2)
	nop
	movei			#DSP_pointeur_sur_bouclage1,AHX__I2S__tmp_1
	moveq			#0,AHX__I2S__valeur_offset1
	load				(AHX__I2S__tmp_1),AHX__I2S__source_sample_voie1						; adresse de bouclage
	store				AHX__I2S__valeur_offset1,(AHX__I2S__source_offset1)
	cmpq				#0,AHX__I2S__source_sample_voie1
	jr						ne,.pas_bouclage_a_zero1
	nop
	movei			#$13652,AHX__I2S__source_sample_voie1
	moveq			#0,AHX__I2S__increment1
	movei			#DSP_pointeur_sur_increment1_16_16,AHX__I2S__tmp_1
	movei			#$7F,AHX__I2S__sample1
	store				AHX__I2S__increment1,(AHX__I2S__tmp_1)
	jr						.cont_v1
; va executer ci dessous 
.pas_bouclage_a_zero1:	
	store				AHX__I2S__source_sample_voie1,(AHX__I2S__pointeur_source_sample_voie1)
	loadb				(AHX__I2S__source_sample_voie1),AHX__I2S__sample1				; sample sans volume
.cont_v1:	
	add						AHX__I2S__sample1,AHX__I2S__pointeur_table_vol1


;  test bouclage 2
	cmp			AHX__I2S__debug,AHX__I2S__valeur_offset2
	jr					ne,.debugedz
	nop
	nop
.debugedz:	


	movei		#.cont_v2,AHX__I2S__tmp_2
	cmpq			#0,AHX__I2S__sample2
	jump			ne,(AHX__I2S__tmp_2)
	nop
	movei			#DSP_pointeur_sur_bouclage2,AHX__I2S__tmp_1
	moveq			#0,AHX__I2S__valeur_offset2
	load				(AHX__I2S__tmp_1),AHX__I2S__source_sample_voie2						; adresse de bouclage
	store				AHX__I2S__valeur_offset2,(AHX__I2S__source_offset2)
	cmpq				#0,AHX__I2S__source_sample_voie2
	jr						ne,.pas_bouclage_a_zero2
	nop
	movei			#$13652,AHX__I2S__source_sample_voie2
	moveq			#0,AHX__I2S__increment2
	movei			#DSP_pointeur_sur_increment2_16_16,AHX__I2S__tmp_1
	movei			#$7F,AHX__I2S__sample1
	store				AHX__I2S__increment2,(AHX__I2S__tmp_1)
	jr						.cont_v2
; va executer ci dessous 
.pas_bouclage_a_zero2:	
	store				AHX__I2S__source_sample_voie2,(AHX__I2S__pointeur_source_sample_voie2)
	loadb				(AHX__I2S__source_sample_voie2),AHX__I2S__sample2				; sample sans volume
.cont_v2:	
	add						AHX__I2S__sample2,AHX__I2S__pointeur_table_vol2

;  test bouclage 3
	movei		#.cont_v3,AHX__I2S__tmp_2
	cmpq			#0,AHX__I2S__sample3
	jump			ne,(AHX__I2S__tmp_2)
	nop
	movei			#DSP_pointeur_sur_bouclage3,AHX__I2S__tmp_1
	moveq			#0,AHX__I2S__valeur_offset3
	load				(AHX__I2S__tmp_1),AHX__I2S__source_sample_voie3						; adresse de bouclage
	store				AHX__I2S__valeur_offset3,(AHX__I2S__source_offset3)
	cmpq				#0,AHX__I2S__source_sample_voie3
	jr						ne,.pas_bouclage_a_zero3
	nop
	movei			#$13652,AHX__I2S__source_sample_voie3
	moveq			#0,AHX__I2S__increment3
	movei			#DSP_pointeur_sur_increment3_16_16,AHX__I2S__tmp_1
	movei			#$7F,AHX__I2S__sample3
	store				AHX__I2S__increment3,(AHX__I2S__tmp_1)
	jr						.cont_v3
; va executer ci dessous 
.pas_bouclage_a_zero3:	
	store				AHX__I2S__source_sample_voie3,(AHX__I2S__pointeur_source_sample_voie3)
	loadb				(AHX__I2S__source_sample_voie3),AHX__I2S__sample3				; sample sans volume
.cont_v3:	
	add						AHX__I2S__sample3,AHX__I2S__pointeur_table_vol3

	loadb					(AHX__I2S__pointeur_table_vol1),AHX__I2S__sample1			; sample reel non signé
	loadb					(AHX__I2S__pointeur_table_vol2),AHX__I2S__sample2
	loadb					(AHX__I2S__pointeur_table_vol3),AHX__I2S__sample3
	
	sub						AHX__I2S__valeur80,AHX__I2S__sample1
	sub						AHX__I2S__valeur80,AHX__I2S__sample2
	sub						AHX__I2S__valeur80,AHX__I2S__sample3

	add						AHX__I2S__sample1,AHX__I2S__sample3
	add						AHX__I2S__sample2,AHX__I2S__sample3
		
	sha						AHX__I2S__multiplie,AHX__I2S__sample3
	
	sat16s				AHX__I2S__sample3

; 8 bits +8 +8 = 9,5 bits
	store				AHX__I2S__sample3,(AHX_I2S_pointeur_DAC_voie_G)
	store				AHX__I2S__sample3,(AHX_I2S_pointeur_DAC_voie_D)
	
		

; return from interrupt I2S
		load	(r31),AHX__I2S__tmp_1					; return address
		bclr	#3,AHX__I2S__save_flags						; clear IMASK
		bset	#10,AHX__I2S__save_flags						; clear latch 1 = I2S
		addq	#4,r31										; pop from stack
		addqt	#2,AHX__I2S__tmp_1						; next instruction
		jump	(AHX__I2S__tmp_1)						; return
		store	AHX__I2S__save_flags,(R30)		; restore flags
;------------------------------------	



;--------------------------------------------
; Timer 1
DSP_LSP_routine_interruption_Timer1:

			movei			#DSP_ratio_timerC_I2S_JAG,R8
			load				(R8),R7

; ---voie 1---
				movei		#$12e98,R0				; *4=OK
				moveq		#0,R6
				load			(R0),R2			; R2 = adresse sample
				movei		#DSP_pointeur_sur_sample_voie1,R14
; nouveau sample ?
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie1_pas_de_nouveau_sample				; il faut juste modifier table volume et increment
				nop
				store			R6,(R0)			; met adresse sample source à 0
				store			R2,(R14)		; met a jour adresse sample
				store			R6,(R14+2)	; offset=0
DSP_timer1_voie1_pas_de_nouveau_sample:
; nouveau bouclage ?
				addq			#6,R0
				loadw			(R0),R2				
				addq			#2,R0
				shlq			#16,R2
				loadw			(R0),R4
				or				R4,R2
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie1_pas_de_nouveau_bouclage
				nop
				storew		R6,(R0)
				subq			#2,R0
				store			R2,(R14+4)		; bouclage, qui doit etre identique à adresse sample
				storew		R6,(R0)
DSP_timer1_voie1_pas_de_nouveau_bouclage:
; table de volume
				movei		#($13136+2),R1			
				loadw			(R1),R3
				add				R1,R3
				store			R3,(R14+1)	; table volume
; increment
				movei		#$13110,R1		; *4=OK
				load			(R1),R3
				
				or				R3,R3
				shrq			#8,R3
				mult			R7,R3					; * ratio<<8
				
				store			R3,(R14+3)		; increment


; ---voie 2---
				movei		#$12efc,R0			; *4=OK
				moveq		#0,R6
				load			(R0),R2			; R2 = adresse sample
				movei		#DSP_pointeur_sur_sample_voie2,R14
; nouveau sample ?
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie2_pas_de_nouveau_sample				; il faut juste modifier table volume et increment
				nop
				store			R6,(R0)			; met adresse sample source à 0
				store			R2,(R14)		; met a jour adresse sample
				store			R6,(R14+2)	; offset=0
DSP_timer1_voie2_pas_de_nouveau_sample:
; nouveau bouclage ?
				addq			#6,R0
				loadw			(R0),R2				
				addq			#2,R0
				shlq			#16,R2
				loadw			(R0),R4
				or				R4,R2
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie2_pas_de_nouveau_bouclage
				nop
				storew		R6,(R0)
				subq			#2,R0
				store			R2,(R14+4)		; bouclage, qui doit etre identique à adresse sample
				storew		R6,(R0)
DSP_timer1_voie2_pas_de_nouveau_bouclage:
; table de volume
				movei		#($1316a+2),R1
				loadw			(R1),R3
				add				R1,R3
				store			R3,(R14+1)	; table volume
; increment
				movei		#($13142+2),R1		; *4=OK
				load			(R1),R3

				or				R3,R3
				shrq			#8,R3
				mult			R7,R3					; * ratio<<8

				
				store			R3,(R14+3)		; increment

; ---voie 3---
				movei		#$12f60,R0			; *4=OK
				moveq		#0,R6
				load			(R0),R2			; R2 = adresse sample
				movei		#DSP_pointeur_sur_sample_voie3,R14
; nouveau sample ?
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie3_pas_de_nouveau_sample				; il faut juste modifier table volume et increment
				nop
				store			R6,(R0)			; met adresse sample source à 0
				store			R2,(R14)		; met a jour adresse sample
				store			R6,(R14+2)	; offset=0
DSP_timer1_voie3_pas_de_nouveau_sample:
; nouveau bouclage ?
				addq			#6,R0
				loadw			(R0),R2				
				addq			#2,R0
				shlq			#16,R2
				loadw			(R0),R4
				or				R4,R2
				cmpq			#0,R2
				jr					eq,DSP_timer1_voie3_pas_de_nouveau_bouclage
				nop
				storew		R6,(R0)
				subq			#2,R0
				store			R2,(R14+4)		; bouclage, qui doit etre identique à adresse sample
				storew		R6,(R0)
DSP_timer1_voie3_pas_de_nouveau_bouclage:
; table de volume
				movei		#($131a0+2),R1
				loadw			(R1),R3
				add				R1,R3
				store			R3,(R14+1)	; table volume
; increment
				movei		#($13178+2),R1		; *4= KOKOKO
				loadw			(R1),R3
				addq			#2,R1
				shlq			#16,R3
				loadw			(R1),R5			
				or				R5,R3

				or				R3,R3
				shrq			#8,R3
				mult			R7,R3					; * ratio<<8

				
				store			R3,(R14+3)		; increment

				
				



	

; return from interrupt Timer 1
	;movei	#D_FLAGS,r11											; 6 octets
	movei		#AHX_DSP_flag_timer1,AHX__I2S__tmp_1
	moveq		#1,AHX__I2S__tmp_2
		load		(r31),AHX__I2S__tmp_3					; return address
	store		AHX__I2S__tmp_2,(AHX__I2S__tmp_1)
		bclr	#3,AHX__I2S__save_flags						; clear IMASK
		bset	#11,AHX__I2S__save_flags						; clear latch 1 = timer 1
		addq	#4,r31										; pop from stack
		addqt	#2,AHX__I2S__tmp_3						; next instruction
		jump	(AHX__I2S__tmp_3)						; return
		store	AHX__I2S__save_flags,(R30)		; restore flags
;------------------------------------	





;------------------------------------------
;------------------------------------------
; ------------- main DSP ------------------
;------------------------------------------
;------------------------------------------



DSP_routine_init_DSP:


	


; assume run from bank 1
	movei	#DSP_ISP+(DSP_STACK_SIZE*4),r31			; init isp
	moveq	#0,r1
	moveta	r31,r31									; ISP (bank 0)
	nop
	movei	#DSP_USP+(DSP_STACK_SIZE*4),r31			; init usp




; calculs des frequences deplacé dans DSP
; sclk I2S
	movei		#JOYBUTS,r0
	loadw		(r0),r3
	btst		#4,r3
	movei	#415530<<8,r1	;frequence_Video_Clock_divisee*128
	jr			eq,initPAL
	nop
	movei	#415483<<8,r1	;frequence_Video_Clock_divisee*128
initPAL:
    movei    #DSP_Audio_frequence,R0
    div     	 R0,R1
	or			R1,R1
    movei 	   #128,R2
    add      	R2,R1		; +128 = +0.5
    shrq     	#8,R1
    subq     	#1,R1
    movei    #DSP_parametre_de_frequence_I2S,r2
    store   	 R1,(R2)
;calcul inverse
    addq    #1,R1
    add     R1,R1		; *2
    add     R1,R1		; *2
    shlq    #4,R1	; *16

	btst	#4,r3
	movei	#26593900,r0	;frequence_Video_Clock
	jr	eq,initPAL2
	nop
	movei	#26590906,r0	;frequence_Video_Clock
initPAL2:
    div      	R1,R0
	or			R0,R0
    movei    #DSP_frequence_de_replay_reelle_I2S,R2
    store    R0,(R2)

; calcul constants ratio Amiga/Jaguar
	movei	#3546895<<9,R1
	movei	#DSP_ratio_Amiga_Jaguar__a_virgule_9_bits,R2
	div		R0,R1
	or		R1,R1
	shlq		#AHX_nb_bits_virgule_increment_period-9,R1		; maxi = 31 bits : 31-15 = 16 - ( 2+5) = 9
	store	R1,(R2)

; calcul du ratio timer C sur ST et replay sur Jaguar
; - frequence + élevée, formule de recalcule des increments : ( frequence timer C / frequence jaguar ) * increment					 // 9906/DSP_Audio_frequence
	movei		#DSP_frequence_de_replay_reelle_I2S,R0
	movei		#9906<<8,R2			; freq timer C
	load			(R0),R1
	movei		#DSP_ratio_timerC_I2S_JAG,R3
	div				R1,R2
	or				R2,R2
	store			R2,(R3)
	



	; moveta, constantes I2S
;AHX_I2S_pointeur_DAC_voie_G														.equr		R18
;AHX_I2S_pointeur_DAC_voie_D														.equr		R19
	movei		#L_I2S+4,R0
	movei		#L_I2S,R1
	moveta		R0,AHX_I2S_pointeur_DAC_voie_G
	moveta		R1,AHX_I2S_pointeur_DAC_voie_D
	

	
; init I2S
	movei	#SCLK,r10
	movei	#SMODE,r11
	movei	#DSP_parametre_de_frequence_I2S,r12
	movei	#%001101,r13			; SMODE bascule sur RISING
	load	(r12),r12				; SCLK
	store	r12,(r10)
	store	r13,(r11)


; init Timer 1 = 50 HZ

; init Timer 1 = 50 HZ

	movei	#3643,R13					; valeur pour 50 Hz
	;load		(R2),R0							; = plyPSpeed / If SPD=0, the mod plays at 50Hz //  SPD=1, 100Hz. SPD=2, 150Hz. SPD=3, 200Hz
		moveq	#0,R0
	addq		#1,R0
	div			R0,R13					; divise pour avoir la bonne vitesse de replay
	or			R13,R13
		
	subq		#1,R13					; -1 pour parametrage du timer 1
	
; 26593900 / 50 = 531 878 => 2 × 73 × 3643 => 146*3643
	movei	#JPIT1,r10				; F10000
	;movei	#JPIT2,r11				; F10002
	movei	#145*65536,r12				; Timer 1 Pre-scaler
	;shlq	#16,r12
	or		R13,R12
	store	r12,(r10)				; JPIT1 & JPIT2




; enable interrupts
	movei	#D_FLAGS,r24
; prod version
;	movei	#D_I2SENA|D_TIM1ENA|D_TIM2ENA|REGPAGE|D_CPUENA,r29			; I2S+Timer 1+timer 2+CPU
	movei	#D_I2SENA|D_TIM1ENA|REGPAGE,r29			; I2S+Timer 1
	;movei	#D_I2SENA|REGPAGE,r29					; I2S only
	;movei	#D_TIM1ENA|REGPAGE,r29					; Timer 1 only
	;movei	#D_TIM2ENA|REGPAGE,r29					; Timer 2 only
			; demarre les timers
	store	r29,(r24)
	nop
	nop


;------------------------------------------------------
;
; boucle centrale
;
;------------------------------------------------------

DSP_boucle_centrale:
; ------------------------------
; retour boucle principale
; bouclage final
	;movei	#AHX_DSP_flag_timer1,R2
	;moveq	#3,R3
	movei	#DSP_boucle_centrale,R26
	jump	(R26)
	;store	R3,(R2)

	.phrase
	
DSP_frequence_de_replay_reelle_I2S:					dc.l				0
DSP_ratio_Amiga_Jaguar__a_virgule_9_bits:		dc.l				0
DSP_parametre_de_frequence_I2S:								dc.l				0
AHX_DSP_flag_timer1:					dc.l						0


DSP_pointeur_sur_sample_voie1:					dc.l				$13652
DSP_pointeur_sur_table_volume_voie1:		dc.l				$13656
DSP_pointeur_sur_offset_voie1:						dc.l				0
DSP_pointeur_sur_increment1_16_16:			dc.l				0
DSP_pointeur_sur_bouclage1:							dc.l				0

DSP_pointeur_sur_sample_voie2:					dc.l				$13652
DSP_pointeur_sur_table_volume_voie2:		dc.l				$13656
DSP_pointeur_sur_offset_voie2:						dc.l				0
DSP_pointeur_sur_increment2_16_16:			dc.l				0
DSP_pointeur_sur_bouclage2:							dc.l				0

DSP_pointeur_sur_sample_voie3:					dc.l				$13652
DSP_pointeur_sur_table_volume_voie3:		dc.l				$13656
DSP_pointeur_sur_offset_voie3:						dc.l				0
DSP_pointeur_sur_increment3_16_16:			dc.l				0
DSP_pointeur_sur_bouclage3:							dc.l				0

DSP_ratio_timerC_I2S_JAG:								dc.l				0

;---------------------
; FIN DE LA RAM DSP
code_DSP_fin:
;---------------------


SOUND_DRIVER_SIZE			.equ			code_DSP_fin-DSP_base_memoire
	.print	"; ------------------------------------------------------------------------------------------------"
	.print	"--- Sound driver code size (DSP): ", /u SOUND_DRIVER_SIZE, " bytes / 8192 ---"
	.print	"; ------------------------------------------------------------------------------------------------"


	.68000




;--------------------------------------------- GPU ------------------------------------------


	.dphrase
GPU_debut:

	.gpu
	.org	G_RAM
GPU_base_memoire:
; CPU interrupt
	.rept	8				; 3
		nop
	.endr
; DSP interrupt, the interrupt output from Jerry
	.rept	8
		nop
	.endr
; Timing generator
	.rept	8
		nop
	.endr
; Object Processor
	jump	(R27)
	nop
	.rept	6
		nop
	.endr
; Blitter
	.rept	8
		nop
	.endr



GPU_init:
	movei	#GPU_ISP+(GPU_STACK_SIZE*4),r31			; init isp				6
	moveq	#0,r1										;						2
	moveta	r31,r31									; ISP (bank 0)		2
	nop													;						2
	movei	#GPU_USP+(GPU_STACK_SIZE*4),r31			; init usp				6

	moveq	#$0,R0										; 2
	moveta	R0,R26							; compteur	  2
	movei	#interrupt_OP,R1							; 6
	moveta	R1,R27										; 2

	movei	#OBF,R0									; 6
	moveta	R0,R22										; 2

	movei	#G_FLAGS,R1											; GPU flags
	moveta	R1,R28

	movei	#G_FLAGS,r30

	movei	#G_OPENA|REGPAGE,r29			; object list interrupt
	nop
	nop
	store	r29,(r30)
	nop
	nop

; swap les pointeurs d'OL
		movei	#GPU_pointeur_object_list_a_modifier,R0
		movei	#GPU_pointeur_object_list_a_afficher,R1
		load	(R0),R2
		load	(R1),R3
		store	R2,(R1)
		movei	#OLP,R4
		;moveta	R3,R3
		rorq	#16,R2
		store	R3,(R0)
		store	R2,(R4)


; -------------------------- LOOP ------------------------
GPU_loop:
; ----------------- object list --------------------
	.if			TEMPS_GPU=1
	movei		#BORD1,R0
	movei		#$FFFF,R1
	storew		R1,(R0)
	.endif


; inserer zone motif
	movei		#GPU_premiere_ligne,R20
	movei		#GPU_pointeur_object_list_a_modifier,R10
	load		(R20),R4
	;movei		#pointeur_zone_scrolling_a_modifier,R17
	load		(R10),R11
	shlq		#3,R4

	addq		#32,R11
	addq		#16,R11


	movei		#GPU_ecran_de_presentation,R0
	movei		#GPU_KNUKLE,R27
	load			(R0),R0
	cmpq			#0,R0
	jump			ne,(R27)
	nop
	
;inserer ecran debut
; inserer zone 3D
	movei		#((zone3D_POSY+50)<<3)+(77<<14),R1				; YPOS<<3 + HEIGHT<<14
	movei		#image_CJ,R0
	add			R4,R1
	movei		#(zone3D_POSX)+(4<<12)+(1<<15)+(80<<18)+(80<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		1 plan=0<<12 / 256c : 3<<12  // 05<<18 // 05<<28
	;movei		#(zone3D_POSX)+(4<<12)+(1<<15)+(80<<18)+(80<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		CRY
	;movei		#(%0101)+(1<<15),R3			; IWIDTH+TRANS			CRY
	movei		#(%0101),R3			; IWIDTH+TRANS // %0 pour 1 plan
	
	move		R11,R12				; R12 = link
	sharq		#3,R0
	addq		#16,R12				; R12 = link 
	shlq		#11,R0
	sharq		#3,R12				; aligné sur une phrase
	
	move		R12,R13
	shlq		#24,R12				; link partie 1
	sharq		#8,R13				; R13 = 2eme partie du link
	or			R0,R13				; DATA+LINK
	or			R12,R1
	store		R13,(R11)
	addq		#4,R11
	store		R1,(R11)
	addq		#4,R11
	store		R3,(R11)
	addq		#4,R11
	store		R2,(R11)
	addq		#4,R11

; insert un stop
		moveq	#4,R16
		moveq	#0,R13
		store	R13,(R11)		; 0000
		addq	#4,R11
		store	R16,(R11)		; 0004

	movei		#GPU_KNUKLE__NO,R27
	jump			(R27)
	nop


GPU_KNUKLE:
; inserer zone logo+batteur
	movei		#((zone3D_POSY)<<3)+(hauteur_zone_3D<<14),R1				; YPOS<<3 + HEIGHT<<14
	movei		#ecran_logo,R0
	add			R4,R1
	movei		#(zone3D_POSX)+(3<<12)+(1<<15)+(40<<18)+(40<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		1 plan=0<<12 / 256c : 3<<12  // 05<<18 // 05<<28
	;movei		#(zone3D_POSX)+(4<<12)+(1<<15)+(80<<18)+(80<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		CRY
	;movei		#(%0101)+(1<<15),R3			; IWIDTH+TRANS			CRY
	movei		#(%0010)+(1<<15),R3			; IWIDTH+TRANS // %0 pour 1 plan
	
	move		R11,R12				; R12 = link
	sharq		#3,R0
	addq		#16,R12				; R12 = link 
	shlq		#11,R0
	sharq		#3,R12				; aligné sur une phrase
	
	move		R12,R13
	shlq		#24,R12				; link partie 1
	sharq		#8,R13				; R13 = 2eme partie du link
	or			R0,R13				; DATA+LINK
	or			R12,R1
	store		R13,(R11)
	addq		#4,R11
	store		R1,(R11)
	addq		#4,R11
	store		R3,(R11)
	addq		#4,R11
	store		R2,(R11)
	addq		#4,R11


; inserer zone scrolling
	movei		#GPU_pointeur_zone_scrolling_a_modifier,R0
	movei		#((zone3D_POSY+(180*2))<<3)+(16<<14),R1				; YPOS<<3 + HEIGHT<<14
	load			(R0),R0
	add			R4,R1
	movei		#(zone3D_POSX)+(3<<12)+(1<<15)+(40<<18)+(40<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		1 plan=0<<12 / 256c : 3<<12  // 05<<18 // 05<<28

	;movei		#(zone3D_POSX)+(4<<12)+(1<<15)+(80<<18)+(80<<28),R2				; XPOS+DEPTH+PITCH+DWIDTH+IWIDTH		CRY
	;movei		#(%0101)+(1<<15),R3			; IWIDTH+TRANS			CRY
	movei		#(%0010),R3			; IWIDTH+TRANS // %0 pour 1 plan
	
	move		R11,R12				; R12 = link
	sharq		#3,R0
	addq		#16,R12				; R12 = link 
	shlq		#11,R0
	sharq		#3,R12				; aligné sur une phrase
	
	move		R12,R13
	shlq		#24,R12				; link partie 1
	sharq		#8,R13				; R13 = 2eme partie du link
	or			R0,R13				; DATA+LINK
	or			R12,R1
	store		R13,(R11)
	addq		#4,R11
	store		R1,(R11)
	addq		#4,R11
	store		R3,(R11)
	addq		#4,R11
	store		R2,(R11)
	addq		#4,R11

	
; insert un stop
		moveq	#4,R16
		moveq	#0,R13
		store	R13,(R11)		; 0000
		addq	#4,R11
		store	R16,(R11)		; 0004


; ----------------- fin object liste ------------------


		.if				1=1
; ------------ scrolling -------------
; il faut inserer une colonne de pixels à offset_zone_scrolling-1 et  offset_zone_scrolling+319
; dans  zone_scrolling_dessus_256c_640pixels
; EDZ 0x00F031B6

		movei		#GPU_pointeur_zone_double_scrolling_a_modifier,R24
		movei		#offset_zone_scrolling,R10
		load		(R24),R20
		load		(R10),R1
		movei		#scrolling_pointeur_actuel_sur_lettre,R11		; = source
		add			R1,R20											; = dest
		load		(R11),R12
		movei		#320,R22				; ecart entre les lignes dans la fonte = 32
		move		R20,R21
		movei		#640,R23
		add			R22,R21					; + une demi ligne du buffer
		movei		#16,R19				; hauteur 
		movei		#32,R22				; ecart entre les lignes dans la fonte = 32
		
copie_colonne_de_pixels_de_la_fonte:		
		load			(R12),r0
;edz
		;moveq		#0,R0

		store		R0,(R20)			; a gauche
		add			R22,R12				; fonte +32
		add			R23,R20				; buffer+640
		store		R0,(R21)			; a droite
		add			R23,R21				; buffer+640
		subq		#1,R19
		jr			ne,copie_colonne_de_pixels_de_la_fonte
		nop
		
; avance dans la lettre
		movei		#GPU__scrolling_pas_nouvelle_lettre,R29
		movei		#scrolling__pixel_actuel_dans_lettre,R10
		movei		#32,R9
		load		(R10),R1
		movei		#scrolling_pointeur_actuel_sur_lettre,R11
		.if			avancer_le_scrolling=1
		addq		#increment_scrolling,R1
		.endif
		load		(R11),R2
		cmp			R9,R1
		jump		ne,(R29)
		addqt		#increment_scrolling,R2
; il faut gerer une nouvelle lettre ici

		movei		#scrolling__pointeur_actuel_texte_scrolling,R12
		moveq		#0,R1
		load		(R12),R3			; pointeur sur le texte
		addq		#1,R3
		loadb		(R3),R2				; pointeur sur la nouvelle lettre
		movei		#$FF,R9
		cmp		R9,R2
		jr 			ne,GPU__scrolling_pas_fin_du_texte
		nop
; boucle le texte		
		movei		#$1015c,R3
		loadb		(R3),R2				; pointeur sur la nouvelle lettre
GPU__scrolling_pas_fin_du_texte:
		movei	#fonte1P_256c,R8
		shlq		#9,R2					; 16*32=512 = numero caractere * 512
		add			R8,R2
		store		R3,(R12)		
GPU__scrolling_pas_nouvelle_lettre:
		store		R1,(R10)
		store		R2,(R11)
		

		.if			avancer_le_scrolling=1
; avance scrolling offset
		movei		#offset_zone_scrolling,R10
		movei		#320,R12
		load		(R10),R11
		addq		#increment_scrolling,R11
		cmp			R12,R11
		jr			ne,pas_fin_offset_scrolling
		nop
		moveq		#0,R11
pas_fin_offset_scrolling:
		store		R11,(r10)
		.endif

		.endif
		
		
		
		
		
		

	.if		copie_du_scrolling_au_GPU=1

	movei	#320,R22
	movei	#GPU_pointeur_zone_double_scrolling_a_modifier,R24
	movei	#offset_zone_scrolling,R13
	load	(R24),R10		; source=zone_scrolling_dessus_256c_640pixels
	load	(R13),R16								; entre 0 et 319
	movei	#GPU_pointeur_zone_scrolling_a_modifier,R25
	add		R16,R10
	movei	#GPU_copie_scrolling_au_GPU_pixels,R29
	load	(R25),R12			; R12 = dest multiple de 4
	movei	#GPU_copie_scrolling_au_GPU_ligne,R28
	movei	#16,R18
	
GPU_copie_scrolling_au_GPU_ligne:
	movei	#(320/4),R19
GPU_copie_scrolling_au_GPU_pixels:	
	loadb	(R10),R3
	addq	#1,R10
	shlq	#8,R3
	loadb	(R10),R0
	addq	#1,R10
	or		R0,R3
	shlq	#8,R3
	loadb	(R10),R0
	addq	#1,R10
	or		R0,R3
	shlq	#8,R3
	loadb	(R10),R0
	addq	#1,R10
	or		R0,R3
	store	R3,(R12)
	subq	#1,R19				; nb colonnes de 4 pixels
	jump	ne,(R29)
	addqt	#4,R12
; ligne suivante

	add		R22,R10					; +320

	subq	#1,R18
	jump	ne,(R28)
	nop

	.endif
	



	.if				copie_du_scrolling_au_GPU=0
;----------------------------------------------------
; copie blitter pour le scrolling
;
; source = A2 = zone de 640 pixels de 16 lignes
; dest = A1 = zone de 320 pixels de 16 lignes
	movei		#A1_BASE,R14				; base du blitter
	movei		#A2_BASE,R15

	movei		#GPU_pointeur_zone_double_scrolling_a_modifier,R24				; =A1 = source
	movei	#offset_zone_scrolling,R13
	load		(R24),R1		; source=GPU_pointeur_zone_double_scrolling_a_modifier   =A2
	load	(R13),R16								; entre 0 et 319
	moveq		#0,R0
	move	R16,R17
	movei		#PIXEL8|XADDPIX|PITCH1|WID640,R2
	shrq	#3,R17
	shlq	#32-3,R16			; numero pixel sur 8
	shlq	#3,R17
	shrq	#32-3,R16
	movei		#(1<<16)+($FEC0),R3				; 1 ligne <<16 + -320
	add		R17,R1				;	pointeur debut source
	moveq		#1,R4

; source=A2
; A2_BASE=R1 / A2_FLAGS=PIXEL8|XADDPIX|WID640|PITCH1 / A2_PIXEL=R16 / A2_STEP=(1<<16)+-320


;A2_BASE	= 2224
	store		R1,(R15)
;A2_PIXEL	= 2230
	store		R16,(R15+3)
;A2_FLAGS	= 2228
	store		R2,(R15+1)
;A2_STEP 
	store		R3,(R15+((A2_STEP-A2_BASE)/4))						; _BLIT_A2_STEP)

; dest = A1
; A1_BASE=R5 / A1_FLAGS / A1_PIXEL / A1_STEP  

	movei		#GPU_pointeur_zone_scrolling_a_modifier,R24
	movei		#PIXEL8|XADDPIX|PITCH1|WID320,R6
	load		(R24),R5		; DEST = GPU_pointeur_zone_scrolling_a_modifier
	;movei		#(1<<16)+($FEC0),R3				; 1 ligne <<16 + -320

;A1_FLAGS
	store		R6,(R14+1)
;A1_BASE	
	store		R5,(R14)
;A1_pixel=0 Dest
	store		R0,(R14+3)
; A1_STEP
	store		R3,(R14+3)
;A1_FSTEP	= 2214
	store		R0,(R14+5)
;A1_FPIXEL	= $2218
	store		R0,(R14+6)
;B_PATD		EQU	BASE + $2268
	store		R0,(R14+$1A)
	store		R0,(R14+$1A+1)


; B_COUNT/ B_CMD
	movei		#$00100140,R7							; 16 lignes, 320 pixels
	movei		#SRCEN|UPDA1|UPDA2|LFU_REPLACE,R8
	
	store		R7,(R14+((B_COUNT-A1_BASE)/4))				; B_COUNT								F0223C				+0F
	store		R8,(R14+((B_CMD-A1_BASE)/4))				; B_CMD			
	


GPU_wait_blitter_copie_motif:
	load 	(R14+($38/4)),R0				; Command Register=$38
	shrq 	#1,R0
	jr 		cc,GPU_wait_blitter_copie_motif

	.endif
;-------------------- scrolling original TEX--------------------

	;movei		#BORD1,R10
;	movei		#$FFFF,R0
;	storew		R0,(R10)


	.if				scrolling_TEX=1
; attendre scrolling fait
	movei		#GPU_flag_scrolling_fait,R10
GPU__wait_for_scrolling_vbl:	
	load			(R10),R0
	cmpq			#1,R0
	jr					ne,GPU__wait_for_scrolling_vbl
	nop

; copier $77080 dans ecran_logo+(180*320) sur 1 seul plan
	movei		#GPU_pointeur_zone_scrolling_a_modifier,R11
	movei		#$77080+6,R10
	load			(R11),R11
	;movei		#ecran_logo+(180*320),R11				; sur plan 3
	movei		#(16*20),R9				; 16 lignes
	move			R11,R14
	movei		#64,R16			; couleur de base = 64
	movei		#GPU_decode_bloc,R27
	addq			#1,R14

GPU_decode_bloc:	
	movei		#%1,R12
	moveq		#15,R13
	loadw			(R10),R0			; 16 bits sur 1 plan
	moveq		#14,R17
GPU_decode16pixels:
	move			R0,R1
	move			R0,R2
	sh				R13,R1
	sh				R17,R2
	and				R12,R1
	subq			#2,R17
	and				R12,R2
	add				R16,R1				; + couleur de base
	add				R16,R2
	storeb		R1,(R11)
	storeb		R2,(R14)
	addq			#2,R11
	subq			#2,R13
	jr					cc,GPU_decode16pixels
	addqt			#2,R14
	
	subq			#1,R9
	jump				ne,(R27)
	addqt			#8,R10


	movei		#GPU_flag_scrolling_fait,R10
	moveq		#0,R0
	store			R0,(R10)

	.endif

GPU_KNUKLE__NO:
	
;----------------------------------------------
; incremente compteur de VBL au GPU
		movei	#vbl_counter_GPU,R0
		load	(R0),R1
		addq	#1,R1
		store	R1,(R0)


	.if			TEMPS_GPU=1
	movei		#BORD1,R0
	movei		#$0,R1
	storew		R1,(R0)
	.endif



;-------------------------------------
; synchro avec l'interrupt object list
		movefa	R26,R26
		
GPU_boucle_wait_vsync:
		movefa	R26,R25
		cmp		R25,R26
		jr		eq,GPU_boucle_wait_vsync
		nop


	
; swap les pointeurs d'OL
		movei	#GPU_pointeur_object_list_a_modifier,R0
		movei	#GPU_pointeur_object_list_a_afficher,R1
		load	(R0),R2
		load	(R1),R3				; R3 = pointeur sur l'object list a modifier prochaine frame
		store	R2,(R1)
		movei	#OLP,R4
		;moveta	R3,R3
		rorq	#16,R2
		store	R3,(R0)

		store	R2,(R4)

;swap les pointeur de zones scrolling
		movei			#GPU_pointeur_zone_scrolling_a_modifier,R0
		movei			#GPU_pointeur_zone_scrolling_a_afficher,R1
		load	(R0),R2
		load	(R1),R3
		store	R2,(R1)
		store	R3,(R0)



; boucle globale/centrale
		movei	#GPU_loop,R20
		;or		R20,R20
		jump	(R20)
		nop

;--------------------------------------------------------
;
; interruption object processor
;	- libere l'OP
;	- incremente R26
; utilises : R0/R22/R26/R28/R29/R30/R31
;
;--------------------------------------------------------
interrupt_OP:
		storew		R0,(r22)					; R22 = OBF

		load     (R28),r29
		addq     #1,r26							; incremente R26
		load     (R31),r30
		bclr     #3,r29
		addq     #2,r30
		addq     #4,r31
		bset     #12,r29
		jump     (r30)
		store    r29,(r28)



		.phrase
GPU_premiere_ligne:				dc.l		0				; lus 2 fois
GPU_derniere_ligne:				dc.l		0
vbl_counter_GPU:								dc.l		0
GPU_pointeur_object_list_a_modifier:			dc.l			ob_list_1
GPU_pointeur_object_list_a_afficher:			dc.l			ob_list_2

GPU_pointeur_zone_scrolling_a_modifier:			dc.l				zone_scrolling1
GPU_pointeur_zone_scrolling_a_afficher:			dc.l				zone_scrolling2
GPU_pointeur_zone_double_scrolling_a_modifier:			dc.l				buffer_double_scrolling1
GPU_pointeur_zone_double_scrolling_a_afficher:			dc.l				buffer_double_scrolling2
offset_zone_scrolling:			dc.l			0
scrolling_pointeur_actuel_sur_lettre:				dc.l		fonte1P_256c	+((32*16)*$24)				; espace ?? = $24 ?
scrolling__pixel_actuel_dans_lettre:					dc.l			0
scrolling__pointeur_actuel_texte_scrolling:				dc.l			$1015c



GPU_ecran_de_presentation:			dc.l					0

	.if				scrolling_TEX=1
GPU_flag_scrolling_fait:					dc.l					1
	.endif
; FIN DE LA RAM GPU
GPU_fin:
;---------------------	

GPU_DRIVER_SIZE			.equ			GPU_fin-GPU_base_memoire
	.print	"---------------------------------------------------------------"
	.print	"--- GPU code size : ", /u GPU_DRIVER_SIZE, " bytes / 4096 ---"
	.if GPU_DRIVER_SIZE > 4088
		.print		""
		.print		""
		.print		""
		.print	"---------------------------------------------------------------"
		.print	"          GPU code too large !!!!!!!!!!!!!!!!!! "
		.print	"---------------------------------------------------------------"
		.print		""
		.print		""
		.print		""
		
	.endif



	.68000
	.dphrase
	.text

.phrase
;silence:				dc.l				$80808080,0

palette_logo_ST:
		dc.w			$000,$742,$731,$720,$710,$600,$500,$400,$007,$006,$005,$004,$003,$002,$404,$233
FIN_palette_logo_ST:

palette_personnage_ST:
		dc.w			$000,$773,$760,$750,$640,$530,$727,$607,$506,$405,$304,$420,$700,$455,$344,$233
FIN_palette_personnage_ST:

	.phrase
image_CJ:
; 320*77
		.incbin		"remark_CJ.png_JAG_CRY"
	
	.phrase
bloc_knuckle:
		.incbin	"C:/Jaguar/WoD/knucklebuster_10000_relocated.bin"
fin_bloc_knuckle:


		;ds.b				100*320

		.BSS
DEBUT_BSS:
	.phrase
ecran_logo:				ds.b				320*180

buffer_temporatire_fonte1P:		ds.b				$34*4*16
fonte1P_256c:		ds.b				(32*16)*$34
; buffer tinermediaire = 52 * (4*16)

.phrase
zone_scrolling1:	ds.b				320*16
.phrase
zone_scrolling2:	ds.b				320*16

bloc_cymballe1:	ds.b				39*32
bloc_cymballe2:	ds.b				39*32
bloc_bouche1:		ds.b				26*32
bloc_bouche2:		ds.b				26*32
bloc_caisse1:			ds.b				31*48
bloc_caisse2:			ds.b				31*48
bloc_cgauche1:			ds.b				42*48
bloc_cgauche2:			ds.b				42*48

.phrase
buffer_double_scrolling1:		ds.b				320*16*2
.phrase
buffer_double_scrolling2:		ds.b				320*16*2

_50ou60hertz:	ds.l	1
ntsc_flag:				ds.w		1
a_hdb:          		ds.w		1
a_hde:          		ds.w		1
a_vdb:          		ds.w		1
a_vde:          		ds.w		1
width:          		ds.w		1
height:         		ds.w		1
vbl_counter:			ds.l		1
	even	


	.phrase


FIN_RAM:
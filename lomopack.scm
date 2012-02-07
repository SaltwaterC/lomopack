; GIMP Lomopack v0.5.1
; Simula l'effetto di una "lomografia".
; 
; Copyright (C) 2012  Milo Martini
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 2 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>. 

; -----------------------------------------------------------------------
; Funzioni Condivise
; -----------------------------------------------------------------------

; -----------------------------------------------------------------------
; (lomo-border azione immagine livello intensità)
; Aggiunge ai bordi una sfocatura o una vignettatura
; 
; azione: 0 = sfocatura, 1 = vignettatura
;
(define (lomo-border theAction inImage inBackground theStrength theSize theSoftness)
	(let* 
		(
			(inBlur (car (gimp-layer-copy inBackground TRUE)))
			; Imposta un raggio proporzionale per la sfocatura
			(theWidth (car (gimp-drawable-width inBlur)))
			(theHeight (car (gimp-drawable-height inBlur)))
			;(theBlursize (/ (/ (+ theHeight theWidth) 2) 100))
			(theBlursize (/ (* (/ (+ theHeight theWidth) 20) theStrength) 100))
			(theDrawable 0)
			(theE theSize)
		)

		(gimp-image-add-layer inImage inBlur -1)
			
		(cond 
			((= theAction 0)
				(plug-in-gauss-rle RUN-NONINTERACTIVE inImage inBlur theBlursize TRUE TRUE)
			)
			((= theAction 1)
				(gimp-levels inBlur HISTOGRAM-VALUE 0 255 1.00 0 (- 255 (/ (* theStrength 255) 100)))
			)
		)
			
		; Crea la maschera per il livello della sfocatura
		(let
			( (inMask (car (gimp-layer-create-mask inBlur ADD-WHITE-MASK))) )
			(gimp-layer-add-mask inBlur inMask)
		)
			
		; Disegna un'ellisse nella maschera
		(set! theDrawable (car (gimp-image-get-active-drawable inImage)))
		(gimp-ellipse-select inImage (/ theWidth theE) (/ theHeight theE) (- theWidth(* (/ theWidth theE) 2)) (- theHeight (* (/ theHeight theE) 2)) 2 TRUE FALSE 0)
		(gimp-invert theDrawable)
		(gimp-selection-none inImage)
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE inImage theDrawable (/ (* (/ theWidth 2) theSoftness) 100) (/ (* (/ theHeight 2) theSoftness) 100))
			
		; Fonde il livello della sfocatura
		(gimp-image-merge-down inImage inBlur 1)
		; Attiva il livello modificato
		(set! inBackground (car (gimp-image-get-active-drawable inImage)))
		; inBackground
	)
)

; -----------------------------------------------------------------------
; Filtri del pacchetto Lomo
; -----------------------------------------------------------------------

; -----------------------------------------------------------------------
; Lomo Burn
; 
; Applica all'immagine un effetto di bruciatura tipico delle macchine
; con esposimetro difettoso.
;
(define (script-fu-lomo-burn inImage inBackground theMerge)

	; Cronologia, inizio
	(gimp-image-undo-group-start inImage)

	(let*
		(
			; Copia il livello per il rosso
		 	(inRed (car (gimp-layer-copy inBackground TRUE)))
			; Definisce le dimensioni dell'immagine
		 	(theWidth (car (gimp-drawable-width inBackground)))
			(theHeight (car (gimp-drawable-height inBackground)))
			(theDrawable 0)
		)

		; Inserisce il livello rosso
		(gimp-image-add-layer inImage inRed -1)
		; Riflette il livello in orizzontale
		(gimp-drawable-transform-flip-simple inRed 0 TRUE 0.0 TRUE)
		; Applica sfocatura e colore
		(gimp-levels inRed HISTOGRAM-GREEN 0 255 1.00 0 0)
		(gimp-levels inRed HISTOGRAM-BLUE 0 255 1.00 0 0)
		; Imposta una sfocatura casuale orizzontale e verticale.
		; La sfocatura orizzontale ha un valore casuale tra 10 e 40 pixel
		; La sfocatura verticale è di un terzo dell'altezza della foto
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE inImage inRed (+ 10 (random 30)) (/ theHeight 3))

		; Aggiunge una maschera al livello
		(let
			( (inMask (car (gimp-layer-create-mask inRed ADD-WHITE-MASK))) )
			(gimp-layer-add-mask inRed inMask)
		)
		; Riempie la maschera con il gradiente Default
		(set! theDrawable (car (gimp-image-get-active-drawable inImage)))
		(gimp-context-get-gradient "Default")
		; Applica il gradiente selezionando dei punti a caso nell'immagine
		(gimp-edit-blend theDrawable 3 0 0 100 0 REPEAT-NONE FALSE FALSE 0 0 FALSE (random theWidth) (random theHeight) (random theWidth) (random theHeight))
		
		; Duplica il livello per il giallo
		(let
			((inYellow (car (gimp-layer-copy inRed TRUE))))
			(gimp-image-add-layer inImage inYellow -1)
			; Cambia la tonalità della bruciatura gialla
			(gimp-hue-saturation inYellow 0 25 0 0)
			; Modalità livello Tonalità
			(gimp-layer-set-mode inYellow ADDITION-MODE)
			(gimp-layer-set-opacity inYellow (+ 20 (random 30)))

			; Modalità e opacità bruciatura rossa
			(gimp-layer-set-mode inRed SCREEN-MODE)
			(gimp-layer-set-opacity inRed (+ 60 (random 40)))
			
			; Appiattisce i livelli se deciso
			(if (= theMerge 0)
				(begin
					; Unisce il livello della tonalità allo sfondo
					(gimp-image-merge-down inImage inRed 1)
					; Unisce il livello della saturazione
					(gimp-image-merge-down inImage inYellow 1)
				)
			)
		)

		; Cronologia, fine
		(gimp-image-undo-group-end inImage)

		; Aggiorna l'immagine
		(gimp-displays-flush)
	)
)

; -----------------------------------------------------------------------
; Lomo Color
; 
; Effetti di colore tipici delle foto scattate con pellicole ad alta 
; saturazione. Vengono inoltre aggiunti effetti di sfocatura e 
; vignettatura.
; 
(define (script-fu-lomo-color inImage inBackground preBright preContrast theMerge theContrast theColor theBlur theVign)

	; Avvia il gruppo della cronologia per questo script
	(gimp-image-undo-group-start inImage)

	; Aggiunge luminosità o toglie contrasto al livello
	(if (or (> preBright 0) (< preContrast 100))
		(gimp-brightness-contrast inBackground preBright preContrast)
	)

	; ---------------------------------------------------------------------
	; Aggiunge la sfocatura ai bordi
	;
	(if (= theBlur 1)
		(begin
			(set! inBackground (lomo-border 0 inImage inBackground 10 9 100))
		)
	)

	; ---------------------------------------------------------------------
	; Aggiunge un effetto di vignettatura
	; 
	(if (> theVign 0)
		(begin
			(set! inBackground (lomo-border 1 inImage inBackground theVign 9 100))
		)
	)

	; ---------------------------------------------------------------------
	; Effetto Lomo
	; Da questo punto vengono applicati gli effetti di colore all'immagine
	; 
	(let*
		(
		 ; Esegue la copia del livello per la tonalità
		 (inTonal (car (gimp-layer-copy inBackground TRUE)))
		 ; Copia il livello per la desaturazione
		 (inDesat (car (gimp-layer-copy inBackground TRUE)))
		)

		; Inserisce il livello tonalità
		(gimp-image-add-layer inImage inTonal -1)
		; Inserisce il livello da desaturare
		(gimp-image-add-layer inImage inDesat -1)

		; Inverte i livelli del canale blu del livello principale
		(gimp-levels inBackground HISTOGRAM-BLUE 0 255 1.00 255 0)

		; Imposta la modalità della tonalità come Luce Forte
		(gimp-layer-set-mode inTonal HARDLIGHT-MODE)
		
		; Desatura l'ultimo livello
		(gimp-desaturate-full inDesat DESATURATE-LUMINOSITY)
		; Modalità livello desaturazione: Solo Toni Scuri
		(gimp-layer-set-mode inDesat DARKEN-ONLY-MODE)
		; Regola livelli (contrasta)
		(gimp-levels inDesat HISTOGRAM-VALUE 40 215 1.00 0 255)
		; Imposta Opacità al 40%
		(gimp-layer-set-opacity inDesat 40)
		
		; Aumenta il contrasto regolando le curve di inTonal
		(if (= theContrast 1)
			(begin
				(gimp-curves-spline inTonal 0 8 #( 0 0 70 50 195 215 255 255))
				(gimp-curves-spline inTonal 1 8 #( 0 0 84 70 212 208 255 255))
				(gimp-curves-spline inTonal 2 8 #( 0 0 144 127 212 212 255 255))
				(gimp-curves-spline inTonal 3 8 #( 0 0 70 82 194 210 255 255))
			)
		)

		; Aggiunge del colore
		(if (= theColor 1)
			(begin
				(plug-in-retinex RUN-NONINTERACTIVE inImage inBackground 80 3 2 1.9)
			)
		)

		; Appiattisce i livelli se deciso
		(if (= theMerge 0)
			(begin
				; Unisce il livello della tonalità allo sfondo
				(gimp-image-merge-down inImage inTonal 1)
				; Unisce il livello della saturazione
				(gimp-image-merge-down inImage inDesat 1)
			)
		)
		
		; Chiude il gruppo della cronologia
		(gimp-image-undo-group-end inImage)

		; Aggiorna l'immagine
		(gimp-displays-flush)
	)

)

; -----------------------------------------------------------------------
;
; Lomo Border
; Permette di scurire e sfumare i bordi
;
(define (script-fu-lomo-border inImage inBackground blurPercent blurSize blurSoft vignPercent vignSize vignSoft)
	; Inizia cronologia
	(gimp-image-undo-group-start inImage)
	
	; Sfocatura (default 10%)
	(if (> blurPercent 0)
		(begin
			(set! inBackground (lomo-border 0 inImage inBackground blurPercent (* (- 11 blurSize) 3) blurSoft))
		)
	)

	; Vignettatura
	(if (> vignPercent 0)
		(begin
			(set! inBackground (lomo-border 1 inImage inBackground vignPercent (* (- 11 vignSize) 3) vignSoft))
		)
	)

	; Chiude cronologia
	(gimp-image-undo-group-end inImage)

	; Aggiorna il video
	(gimp-displays-flush)
)

; -----------------------------------------------------------------------
; Registra i filtri nel menu
; -----------------------------------------------------------------------
(script-fu-register
	"script-fu-lomo-color"
	"Lomo Color"
	"Genera un effetto Lomo"
	"Milo Martini" 
	"2012"
	"February 4, 2012"
	"RGB*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Livello da duplicare" 0
	SF-ADJUSTMENT "[PRE] Aumenta luminosità" '(0 0 100 1 10 0 0)
	SF-ADJUSTMENT "[PRE] Diminuisci contrasto" '(0 -100 0 1 10 0 0)
	SF-TOGGLE "Crea livelli separati" FALSE
	SF-TOGGLE "Aumenta il contrasto (Blu enfatizzati)" FALSE
	SF-TOGGLE "Colora (Rende i colori più vivaci)" FALSE
	SF-TOGGLE "Aggiungi una sfocatura ai bordi" FALSE
	SF-ADJUSTMENT "Vignettatura" '(0 0 100 1 10 0 0)
)

(script-fu-register
	"script-fu-lomo-burn"
	"Lomo Burn"
	"Crea un effetto casuale di bruciatura sulla pellicola"
	"Milo Martini" 
	"2012"
	"February 4, 2012"
	"RGB*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Livello da duplicare" 0
	SF-TOGGLE "Crea livelli separati" FALSE
)

(script-fu-register
	"script-fu-lomo-border"
	"Lomo Border"
	"Sfuma e scurisce i bordi"
	"Milo Martini"
	"2012"
	"February 5, 2012"
	"RGB*"
	SF-IMAGE "Image" 0
	SF-DRAWABLE "Livello da duplicare" 0
	SF-ADJUSTMENT "Sfocatura (%)" '(10 0 100 1 10 0 0)
	SF-ADJUSTMENT "Sfocatura dimensione" '(8 1 10 1 10 0 0)
	SF-ADJUSTMENT "Sfocatura morbidezza" '(100 1 100 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura (%)" '(10 0 100 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura dimensione" '(8 1 10 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura morbidezza" '(100 1 100 1 10 0 0)
)

; Inserisce i filtri nel menu di Gimp
(script-fu-menu-register "script-fu-lomo-color" "<Image>/Filters/Jackroom")
(script-fu-menu-register "script-fu-lomo-burn" "<Image>/Filters/Jackroom")
(script-fu-menu-register "script-fu-lomo-border" "<Image>/Filters/Jackroom")

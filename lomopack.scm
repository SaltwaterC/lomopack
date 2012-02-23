; GIMP Lomopack v0.5.2
; Apply a 'lomographic' effect
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
; Shared functions
; -----------------------------------------------------------------------

; -----------------------------------------------------------------------
; (lomo-border action image level intensity)
; Vignette and border-blur
; 
; action: 0 = blur, 1 = vignette
;
(define (lomo-border theAction inImage inBackground theStrength theSize theSoftness)
	(let* 
		(
			(inBlur (car (gimp-layer-copy inBackground TRUE)))
			; Imposta un raggio proporzionale per la sfocatura
			(theWidth (car (gimp-drawable-width inBlur)))
			(theHeight (car (gimp-drawable-height inBlur)))
			(theBlursize (/ (* (/ (+ theHeight theWidth) 20) theStrength) 100))
			(theDrawable 0)
			(theE theSize)
			(theS (- 101 theSoftness))
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
			
		; Mask the blur layer
		(let
			( (inMask (car (gimp-layer-create-mask inBlur ADD-WHITE-MASK))) )
			(gimp-layer-add-mask inBlur inMask)
		)
			
		; Disegna un'ellisse nella maschera
		(set! theDrawable (car (gimp-image-get-active-drawable inImage)))
		; Apply a radial gradient mask (default type)
		(gimp-context-get-gradient "Default")
		(gimp-edit-blend theDrawable 3 0 2 100 theS REPEAT-NONE FALSE FALSE 0 0 FALSE (/ theWidth 2) (/ theHeight 2) (/ theWidth theE) (/ theHeight theE))
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE inImage theDrawable (/ (* (/ theWidth 10) theSoftness) 100) (/ (* (/ theHeight 10) theSoftness) 100))
		
		; Merge the blur layer
		(gimp-image-merge-down inImage inBlur 1)
		; Activate modified layer
		(set! inBackground (car (gimp-image-get-active-drawable inImage)))
		; inBackground
	)
)

; -----------------------------------------------------------------------
; Lomo-pack filters
; -----------------------------------------------------------------------

; -----------------------------------------------------------------------
; Lomo Burn
; 
; Apply a film burn effect.
;
(define (script-fu-lomo-burn inImage inBackground theMerge)

	; Undos, Start
	(gimp-image-undo-group-start inImage)

	(let*
		(
			; Duplicate layer (red)
		 	(inRed (car (gimp-layer-copy inBackground TRUE)))
			; Set the image size
		 	(theWidth (car (gimp-drawable-width inBackground)))
			(theHeight (car (gimp-drawable-height inBackground)))
			(theDrawable 0)
		)

		; Insert red-layer
		(gimp-image-add-layer inImage inRed -1)
		; Horizontal reflection
		(gimp-drawable-transform-flip-simple inRed 0 TRUE 0.0 TRUE)
		; Burn and color effect
		(gimp-levels inRed HISTOGRAM-GREEN 0 255 1.00 0 0)
		(gimp-levels inRed HISTOGRAM-BLUE 0 255 1.00 0 0)
		; Apply a random blur (vertical and horizontal)
		; Horizontal blur range varies between 10 and 40 pixels
		; Vertical blur size is a third of the layer height
		(plug-in-gauss-rle2 RUN-NONINTERACTIVE inImage inRed (+ 10 (random 30)) (/ theHeight 3))

		; Add a layer mask
		(let
			( (inMask (car (gimp-layer-create-mask inRed ADD-WHITE-MASK))) )
			(gimp-layer-add-mask inRed inMask)
		)
		; Fill the layer mask with default gradient
		(set! theDrawable (car (gimp-image-get-active-drawable inImage)))
		(gimp-context-get-gradient "Default")
		; Select two random points to apply the gradient
		(gimp-edit-blend theDrawable 3 0 0 100 0 REPEAT-NONE FALSE FALSE 0 0 FALSE (random theWidth) (random theHeight) (random theWidth) (random theHeight))
		
		; Duplicate layer (yellow)
		(let
			((inYellow (car (gimp-layer-copy inRed TRUE))))
			(gimp-image-add-layer inImage inYellow -1)
			; Modify hue and set the layer-mode
			(gimp-hue-saturation inYellow 0 25 0 0)
			(gimp-layer-set-mode inYellow ADDITION-MODE)
			(gimp-layer-set-opacity inYellow (+ 20 (random 30)))

			; Layer mode for red (screen)
			(gimp-layer-set-mode inRed SCREEN-MODE)
			(gimp-layer-set-opacity inRed (+ 60 (random 40)))
			
			; Merge layers if selected
			(if (= theMerge 0)
				(begin
					(gimp-image-merge-down inImage inRed 1)
					(gimp-image-merge-down inImage inYellow 1)
				)
			)
		)

		; Undos, End
		(gimp-image-undo-group-end inImage)

		; Update the image
		(gimp-displays-flush)
	)
)

; -----------------------------------------------------------------------
; Lomo Color
; 
; Color effects as in high-saturated films with blur and vignette
; 
(define (script-fu-lomo-color inImage inBackground preBright preContrast theMerge theContrast theColor theBlur theVign)

	; Undos, Start
	(gimp-image-undo-group-start inImage)

	; Add luminosity, remove contrast
	(if (or (> preBright 0) (< preContrast 100))
		(gimp-brightness-contrast inBackground preBright preContrast)
	)

	; ---------------------------------------------------------------------
	; Border-blur
	;
	(if (= theBlur 1)
		(begin
			(set! inBackground (lomo-border 0 inImage inBackground 20 9 70))
		)
	)

	; ---------------------------------------------------------------------
	; Vignette
	; 
	(if (> theVign 0)
		(begin
			(set! inBackground (lomo-border 1 inImage inBackground theVign 9 70))
		)
	)

	; ---------------------------------------------------------------------
	; Lomo color effects
	; 
	(let*
		(
		 ; Duplicate layer (tonality)
		 (inTonal (car (gimp-layer-copy inBackground TRUE)))
		 ; Duplicate layer (desaturate)
		 (inDesat (car (gimp-layer-copy inBackground TRUE)))
		)

		; Insert duplicated layers
		(gimp-image-add-layer inImage inTonal -1)
		(gimp-image-add-layer inImage inDesat -1)

		; Invert blue channel levels
		(gimp-levels inBackground HISTOGRAM-BLUE 0 255 1.00 255 0)

		; Change layer-mode for the tonality
		(gimp-layer-set-mode inTonal HARDLIGHT-MODE)
		
		; Desaturate last level
		(gimp-desaturate-full inDesat DESATURATE-LUMINOSITY)
		; Change mode for the desaturated layer
		(gimp-layer-set-mode inDesat DARKEN-ONLY-MODE)
		; Modify levels to add contrast
		(gimp-levels inDesat HISTOGRAM-VALUE 40 215 1.00 0 255)
		; Set opacity layer to 40%
		(gimp-layer-set-opacity inDesat 40)
		
		; Change inTonal curves to add contrast
		(if (= theContrast 1)
			(begin
				(gimp-curves-spline inTonal 0 8 #( 0 0 70 50 195 215 255 255))
				(gimp-curves-spline inTonal 1 8 #( 0 0 84 70 212 208 255 255))
				(gimp-curves-spline inTonal 2 8 #( 0 0 144 127 212 212 255 255))
				(gimp-curves-spline inTonal 3 8 #( 0 0 70 82 194 210 255 255))
			)
		)

		; Colorize
		(if (= theColor 1)
			(begin
				(plug-in-retinex RUN-NONINTERACTIVE inImage inBackground 80 3 2 1.9)
			)
		)

		; Merge layers
		(if (= theMerge 0)
			(begin
				(gimp-image-merge-down inImage inTonal 1)
				(gimp-image-merge-down inImage inDesat 1)
			)
		)
		
		; Undos, End
		(gimp-image-undo-group-end inImage)

		; Update image
		(gimp-displays-flush)
	)

)

; -----------------------------------------------------------------------
;
; Lomo Border
; Vignette and blur effect
;
(define (script-fu-lomo-border inImage inBackground blurPercent blurSize blurSoft vignPercent vignSize vignSoft)
	; Undos, Start
	(gimp-image-undo-group-start inImage)
	
	; Blur effect (default 10%)
	(if (> blurPercent 0)
		(begin
			(set! inBackground (lomo-border 0 inImage inBackground blurPercent (* (- 11 blurSize) 3) blurSoft))
		)
	)

	; Vignette
	(if (> vignPercent 0)
		(begin
			(set! inBackground (lomo-border 1 inImage inBackground vignPercent (* (- 11 vignSize) 3) vignSoft))
		)
	)

	; Undos, End
	(gimp-image-undo-group-end inImage)

	; Update image
	(gimp-displays-flush)
)

; -----------------------------------------------------------------------
; Register and insert filters in the gimp menu
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
	SF-ADJUSTMENT "Sfocatura (%)" '(20 0 100 1 10 0 0)
	SF-ADJUSTMENT "Sfocatura dimensione" '(3 1 10 1 10 0 0)
	SF-ADJUSTMENT "Sfocatura morbidezza" '(50 1 100 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura (%)" '(30 0 100 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura dimensione" '(3 1 10 1 10 0 0)
	SF-ADJUSTMENT "Vignettatura morbidezza" '(50 1 100 1 10 0 0)
)

; Insert filters in the menu
(script-fu-menu-register "script-fu-lomo-color" "<Image>/Filters/Jackroom")
(script-fu-menu-register "script-fu-lomo-burn" "<Image>/Filters/Jackroom")
(script-fu-menu-register "script-fu-lomo-border" "<Image>/Filters/Jackroom")

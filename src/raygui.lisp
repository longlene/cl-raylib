;;;; raygui v5.0 - A simple and easy-to-use immediate-mode gui library
;;;;
;;;; Common Lisp port of raygui.h (raylib/examples/*/raygui.h, RAYGUI_VERSION "5.0")
;;;;
;;;; raygui is a tools-dev-focused immediate-mode-gui library based on raylib but also
;;;; available as a standalone library, as long as input and drawing functions are provided.
;;;;
;;;; PORT NOTES:
;;;;   - Functions follow raygui.h order and naming: GuiButton() -> gui-button
;;;;   - C pointer parameters (bool *active, int *value, float *value, char *text,
;;;;     Vector2 *scroll, Rectangle *view, Color *color, Vector3 *colorHsv...) are passed
;;;;     by value and their updated values are returned as extra values, in parameter order,
;;;;     after the control result:  (multiple-value-setq (result value) (gui-slider ... value ...))
;;;;   - Text is processed internally as NUL-terminated UTF-8 octet vectors with byte offsets,
;;;;     exactly like the C char pointers; the public API takes and returns Lisp strings
;;;;   - Only the raylib (non RAYGUI_STANDALONE) configuration is ported, with icons enabled
;;;;     and RAYGUI_FONT_ICONS_BAKING disabled (default configuration)
;;;;
;;;; LICENSE: zlib/libpng
;;;;
;;;; Copyright (c) 2014-2026 Ramon Santamaria (@raysan5)

(defpackage #:raygui
  (:use #:cl #:raylib)
  (:export
   ;; Global gui state control functions
   #:gui-enable #:gui-disable #:gui-lock #:gui-unlock #:gui-is-locked
   #:gui-set-alpha #:gui-set-state #:gui-get-state
   ;; Font set/get functions
   #:gui-set-font #:gui-get-font
   ;; Style set/get functions
   #:gui-set-style #:gui-get-style
   ;; Styles loading functions
   #:gui-load-style #:gui-load-style-from-memory #:gui-load-style-default
   ;; Tooltips management functions
   #:gui-enable-tooltip #:gui-disable-tooltip #:gui-set-tooltip
   ;; Icons functionality
   #:gui-icon-text #:gui-set-icon-scale #:gui-get-icons #:gui-load-icons #:gui-load-icons-from-memory
   #:gui-draw-icon #:gui-get-text-width
   ;; Container/separator controls
   #:gui-window-box #:gui-group-box #:gui-line #:gui-panel #:gui-scroll-panel
   ;; Basic controls set
   #:gui-label #:gui-button #:gui-label-button #:gui-toggle #:gui-toggle-group #:gui-toggle-slider
   #:gui-check-box #:gui-combo-box #:gui-dropdown-box #:gui-spinner #:gui-value-box
   #:gui-value-box-float #:gui-text-box #:gui-slider #:gui-slider-bar #:gui-progress-bar
   #:gui-status-bar #:gui-dummy-rec #:gui-grid
   ;; Advance controls set
   #:gui-list-view #:gui-list-view-ex #:gui-tab-bar #:gui-tab-bar-ex #:gui-message-box
   #:gui-text-input-box #:gui-color-picker #:gui-color-panel #:gui-color-bar-alpha
   #:gui-color-bar-hue #:gui-color-picker-hsv #:gui-color-panel-hsv
   ;; Version
   #:+raygui-version+ #:+raygui-version-major+ #:+raygui-version-minor+ #:+raygui-version-patch+
   ;; GuiResult
   #:+result-none+ #:+result-pressed+ #:+result-changed+ #:+result-tab-close+
   ;; GuiState
   #:+state-normal+ #:+state-focused+ #:+state-pressed+ #:+state-disabled+
   ;; GuiTextAlignment, GuiTextAlignmentVertical, GuiTextWrapMode
   #:+text-align-left+ #:+text-align-center+ #:+text-align-right+
   #:+text-align-top+ #:+text-align-middle+ #:+text-align-bottom+
   #:+text-wrap-none+ #:+text-wrap-char+ #:+text-wrap-word+
   ;; GuiControl
   #:+default+ #:+label+ #:+button+ #:+toggle+ #:+slider+ #:+progressbar+ #:+checkbox+
   #:+combobox+ #:+dropdownbox+ #:+textbox+ #:+valuebox+ #:+tabbar+ #:+listview+
   #:+colorpicker+ #:+scrollbar+ #:+statusbar+
   ;; GuiControlProperty
   #:+border-color-normal+ #:+base-color-normal+ #:+text-color-normal+
   #:+border-color-focused+ #:+base-color-focused+ #:+text-color-focused+
   #:+border-color-pressed+ #:+base-color-pressed+ #:+text-color-pressed+
   #:+border-color-disabled+ #:+base-color-disabled+ #:+text-color-disabled+
   #:+border-width+ #:+text-padding+ #:+text-alignment+ #:+baseprop16+
   ;; GuiDefaultProperty
   #:+text-size+ #:+text-spacing+ #:+line-color+ #:+background-color+ #:+text-line-spacing+
   #:+text-alignment-vertical+ #:+text-wrap-mode+ #:+extprop08+
   ;; Controls extended properties
   #:+group-padding+ #:+group-width-full+
   #:+slider-width+ #:+slider-padding+
   #:+progress-padding+ #:+progress-side+
   #:+arrows-size+ #:+arrows-visible+ #:+scroll-slider-padding+ #:+scroll-slider-size+
   #:+scroll-padding+ #:+scroll-speed+
   #:+check-padding+
   #:+combo-button-width+ #:+combo-button-spacing+
   #:+arrow-padding+ #:+dropdown-items-spacing+ #:+dropdown-arrow-hidden+ #:+dropdown-roll-up+
   #:+text-readonly+
   #:+spinner-button-width+ #:+spinner-button-spacing+
   #:+tab-items-width+ #:+tab-close-button+ #:+tab-line-side+
   #:+scrollbar-left-side+ #:+scrollbar-right-side+
   #:+list-items-height+ #:+list-items-spacing+ #:+scrollbar-width+ #:+scrollbar-side+
   #:+list-items-border-normal+ #:+list-items-border-width+
   #:+color-selector-size+ #:+huebar-width+ #:+huebar-padding+ #:+huebar-selector-height+
   #:+huebar-selector-overflow+
   ;; Icons
   #:+raygui-icon-size+ #:+raygui-icon-max-icons+
   ;; Color conversion (raygui internal, exported for convenience as in the examples)
   #:convert-hsv-to-rgb #:convert-rgb-to-hsv))

(in-package #:raygui)

(defconstant +raygui-version-major+ 5)
(defconstant +raygui-version-minor+ 0)
(defconstant +raygui-version-patch+ 0)
(defparameter +raygui-version+ "5.0")

;;----------------------------------------------------------------------------------
;; Defines and Macros
;;----------------------------------------------------------------------------------
;; Simple log system to avoid printf() calls if required
(defmacro raygui-log (fmt &rest args)
  `(format t ,fmt ,@args))

;; Macros to define required UI inputs, including mapping to gamepad controls
(declaim (inline gui-button-down-p gui-button-down-alt-p gui-button-pressed-p gui-button-pressed-mid-p
                 gui-button-released-p gui-scroll-delta gui-pointer-position gui-key-down-p gui-key-pressed-p
                 gui-input-key))
(defun gui-button-down-p ()
  (or (is-mouse-button-down +mouse-left-button+) (is-gamepad-button-down 0 +gamepad-button-right-face-down+)))
(defun gui-button-down-alt-p ()       ; Mapping to alternative button down pressed
  (or (is-mouse-button-down +mouse-right-button+) (is-gamepad-button-down 0 +gamepad-button-right-face-right+)))
(defun gui-button-pressed-p ()
  (or (is-mouse-button-pressed +mouse-left-button+) (is-gamepad-button-pressed 0 +gamepad-button-right-face-down+)))
(defun gui-button-pressed-mid-p ()
  (or (is-mouse-button-pressed +mouse-middle-button+) (is-gamepad-button-pressed 0 +gamepad-button-right-face-up+)))
(defun gui-button-released-p ()
  (or (is-mouse-button-released +mouse-left-button+) (is-gamepad-button-released 0 +gamepad-button-right-face-down+)))
;; Mapping to scroll delta changes
(defun gui-scroll-delta ()
  (- (+ (get-mouse-wheel-move) (+ (get-gamepad-axis-movement 0 +gamepad-axis-right-trigger+) 1))
     (+ (get-gamepad-axis-movement 0 +gamepad-axis-left-trigger+) 1)))
(defun gui-pointer-position () (get-mouse-position))
(defun gui-key-down-p (key) (is-key-down key))
(defun gui-key-pressed-p (key) (is-key-pressed key))
(defun gui-input-key () (get-char-pressed))

;;----------------------------------------------------------------------------------
;; Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Gui control result
(defconstant +result-none+ 0)
(defconstant +result-pressed+ 1)
(defconstant +result-changed+ 2)
(defconstant +result-tab-close+ 4)        ; GuiTabBar(), tab close request

;; Gui control state
(defconstant +state-normal+ 0)
(defconstant +state-focused+ 1)
(defconstant +state-pressed+ 2)
(defconstant +state-disabled+ 3)

;; Gui control text alignment
(defconstant +text-align-left+ 0)
(defconstant +text-align-center+ 1)
(defconstant +text-align-right+ 2)

;; Gui control text alignment vertical
;; NOTE: Text vertical position inside the text bounds
(defconstant +text-align-top+ 0)
(defconstant +text-align-middle+ 1)
(defconstant +text-align-bottom+ 2)

;; Gui control text wrap mode
;; NOTE: Useful for multiline text
(defconstant +text-wrap-none+ 0)
(defconstant +text-wrap-char+ 1)
(defconstant +text-wrap-word+ 2)

;; Gui controls
;; NOTE: Up to 16 controls supported or 32 controls (v500)
;; Default -> populates to all controls when set
(defconstant +default+ 0)
;; Basic controls
(defconstant +label+ 1)                   ; Used also for: LABELBUTTON
(defconstant +button+ 2)
(defconstant +toggle+ 3)                  ; Used also for: TOGGLEGROUP
(defconstant +slider+ 4)                  ; Used also for: SLIDERBAR, TOGGLESLIDER
(defconstant +progressbar+ 5)
(defconstant +checkbox+ 6)
(defconstant +combobox+ 7)
(defconstant +dropdownbox+ 8)
(defconstant +textbox+ 9)                 ; Used also for: TEXTBOXMULTI
(defconstant +valuebox+ 10)
(defconstant +tabbar+ 11)
(defconstant +listview+ 12)
(defconstant +colorpicker+ 13)
(defconstant +scrollbar+ 14)
(defconstant +statusbar+ 15)

;; Controls BASE properties for every control (RAYGUI_MAX_PROPS_BASE = 16)
;; NOTE: Properties required for all controls, DEFAULT control sets
;; default values for them but they can be overriden per control
(defconstant +border-color-normal+ 0)     ; Control border color in STATE_NORMAL
(defconstant +base-color-normal+ 1)       ; Control base color in STATE_NORMAL
(defconstant +text-color-normal+ 2)       ; Control text color in STATE_NORMAL
(defconstant +border-color-focused+ 3)    ; Control border color in STATE_FOCUSED
(defconstant +base-color-focused+ 4)      ; Control base color in STATE_FOCUSED
(defconstant +text-color-focused+ 5)      ; Control text color in STATE_FOCUSED
(defconstant +border-color-pressed+ 6)    ; Control border color in STATE_PRESSED
(defconstant +base-color-pressed+ 7)      ; Control base color in STATE_PRESSED
(defconstant +text-color-pressed+ 8)      ; Control text color in STATE_PRESSED
(defconstant +border-color-disabled+ 9)   ; Control border color in STATE_DISABLED
(defconstant +base-color-disabled+ 10)    ; Control base color in STATE_DISABLED
(defconstant +text-color-disabled+ 11)    ; Control text color in STATE_DISABLED
(defconstant +border-width+ 12)           ; Control border size, 0 for no border
(defconstant +text-padding+ 13)           ; Control text padding, not considering border
(defconstant +text-alignment+ 14)         ; Control text horizontal alignment inside control text bound (after border and padding): 0-Left, 1-Center, 2-Right
(defconstant +baseprop16+ 15)             ; Not used yet...

;; DEFAULT control, extended properties
;; NOTE: Those properties are global for all controls, they can not be setup per control
(defconstant +text-size+ 16)              ; Text size (glyphs max height)
(defconstant +text-spacing+ 17)           ; Text spacing between glyphs
(defconstant +line-color+ 18)             ; Line control color
(defconstant +background-color+ 19)       ; Background color
(defconstant +text-line-spacing+ 20)      ; Text spacing between lines
(defconstant +text-alignment-vertical+ 21) ; Text vertical alignment inside text bounds (after border and padding): 0-Top, 1-Middle, 2-Bottom
(defconstant +text-wrap-mode+ 22)         ; Text wrap-mode inside text bounds
(defconstant +extprop08+ 23)              ; Not used yet...

;; Toggle/ToggleGroup
(defconstant +group-padding+ 16)          ; ToggleGroup separation between toggles
(defconstant +group-width-full+ 17)       ; ToggleGroup bounds width considers all items: 0-Width per item, 1-Full width

;; Slider/SliderBar
(defconstant +slider-width+ 16)           ; Slider size of internal bar
(defconstant +slider-padding+ 17)         ; Slider/SliderBar internal bar padding

;; ProgressBar
(defconstant +progress-padding+ 16)       ; ProgressBar internal padding
(defconstant +progress-side+ 17)          ; ProgressBar increment side: 0-Left->Right, 1-Right->Left

;; ScrollBar
(defconstant +arrows-size+ 16)            ; ScrollBar arrows size
(defconstant +arrows-visible+ 17)         ; ScrollBar arrows visible
(defconstant +scroll-slider-padding+ 18)  ; ScrollBar slider internal padding
(defconstant +scroll-slider-size+ 19)     ; ScrollBar slider size
(defconstant +scroll-padding+ 20)         ; ScrollBar scroll padding from arrows
(defconstant +scroll-speed+ 21)           ; ScrollBar scrolling speed

;; CheckBox
(defconstant +check-padding+ 16)          ; CheckBox internal check padding

;; ComboBox
(defconstant +combo-button-width+ 16)     ; ComboBox right button width
(defconstant +combo-button-spacing+ 17)   ; ComboBox button separation

;; DropdownBox
(defconstant +arrow-padding+ 16)          ; DropdownBox arrow separation from border and items
(defconstant +dropdown-items-spacing+ 17) ; DropdownBox items separation
(defconstant +dropdown-arrow-hidden+ 18)  ; DropdownBox arrow hidden
(defconstant +dropdown-roll-up+ 19)       ; DropdownBox roll up flag: 0-Roll down, 1-Roll up

;; TextBox/TextBoxMulti/ValueBox/Spinner
(defconstant +text-readonly+ 16)          ; TextBox in read-only mode: 0-Text editable, 1-Text read-only

;; ValueBox/Spinner
(defconstant +spinner-button-width+ 16)   ; Spinner left/right buttons width
(defconstant +spinner-button-spacing+ 17) ; Spinner buttons separation

;; TabBar
(defconstant +tab-items-width+ 16)        ; TabBar tab items width
(defconstant +tab-close-button+ 17)       ; TabBar tab close button: 0-Not shown, 1-Shown
(defconstant +tab-line-side+ 18)          ; TabBar tabs side: 0-Bottom, 1-Top

;; ListView
(defconstant +scrollbar-left-side+ 0)
(defconstant +scrollbar-right-side+ 1)

(defconstant +list-items-height+ 16)      ; ListView items height
(defconstant +list-items-spacing+ 17)     ; ListView items separation
(defconstant +scrollbar-width+ 18)        ; ListView scrollbar size (usually width)
(defconstant +scrollbar-side+ 19)         ; ListView scrollbar side: 0-Left side, 1-Right Side
(defconstant +list-items-border-normal+ 20) ; ListView items border enabled in normal state
(defconstant +list-items-border-width+ 21) ; ListView items border width

;; ColorPicker
(defconstant +color-selector-size+ 16)    ; ColorPicker selector square size
(defconstant +huebar-width+ 17)           ; ColorPicker right hue bar width
(defconstant +huebar-padding+ 18)         ; ColorPicker right hue bar separation from panel
(defconstant +huebar-selector-height+ 19) ; ColorPicker right hue bar selector height
(defconstant +huebar-selector-overflow+ 20) ; ColorPicker right hue bar selector overflow

;;----------------------------------------------------------------------------------
;; Icons enumeration (GuiIconName)
;;----------------------------------------------------------------------------------
(defconstant +icon-none+ 0)
(defconstant +icon-folder-file-open+ 1)
(defconstant +icon-file-save-classic+ 2)
(defconstant +icon-folder-open+ 3)
(defconstant +icon-folder-save+ 4)
(defconstant +icon-file-open+ 5)
(defconstant +icon-file-save+ 6)
(defconstant +icon-file-export+ 7)
(defconstant +icon-file-add+ 8)
(defconstant +icon-file-delete+ 9)
(defconstant +icon-filetype-text+ 10)
(defconstant +icon-filetype-audio+ 11)
(defconstant +icon-filetype-image+ 12)
(defconstant +icon-filetype-play+ 13)
(defconstant +icon-filetype-video+ 14)
(defconstant +icon-filetype-info+ 15)
(defconstant +icon-file-copy+ 16)
(defconstant +icon-file-cut+ 17)
(defconstant +icon-file-paste+ 18)
(defconstant +icon-cursor-hand+ 19)
(defconstant +icon-cursor-pointer+ 20)
(defconstant +icon-cursor-classic+ 21)
(defconstant +icon-pencil+ 22)
(defconstant +icon-pencil-big+ 23)
(defconstant +icon-brush-classic+ 24)
(defconstant +icon-brush-painter+ 25)
(defconstant +icon-water-drop+ 26)
(defconstant +icon-color-picker+ 27)
(defconstant +icon-rubber+ 28)
(defconstant +icon-color-bucket+ 29)
(defconstant +icon-text-t+ 30)
(defconstant +icon-text-a+ 31)
(defconstant +icon-scale+ 32)
(defconstant +icon-resize+ 33)
(defconstant +icon-filter-point+ 34)
(defconstant +icon-filter-bilinear+ 35)
(defconstant +icon-crop+ 36)
(defconstant +icon-crop-alpha+ 37)
(defconstant +icon-square-toggle+ 38)
(defconstant +icon-symmetry+ 39)
(defconstant +icon-symmetry-horizontal+ 40)
(defconstant +icon-symmetry-vertical+ 41)
(defconstant +icon-lens+ 42)
(defconstant +icon-lens-big+ 43)
(defconstant +icon-eye-on+ 44)
(defconstant +icon-eye-off+ 45)
(defconstant +icon-filter-top+ 46)
(defconstant +icon-filter+ 47)
(defconstant +icon-target-point+ 48)
(defconstant +icon-target-small+ 49)
(defconstant +icon-target-big+ 50)
(defconstant +icon-target-move+ 51)
(defconstant +icon-cursor-move+ 52)
(defconstant +icon-cursor-scale+ 53)
(defconstant +icon-cursor-scale-right+ 54)
(defconstant +icon-cursor-scale-left+ 55)
(defconstant +icon-undo+ 56)
(defconstant +icon-redo+ 57)
(defconstant +icon-reredo+ 58)
(defconstant +icon-mutate+ 59)
(defconstant +icon-rotate+ 60)
(defconstant +icon-repeat+ 61)
(defconstant +icon-shuffle+ 62)
(defconstant +icon-emptybox+ 63)
(defconstant +icon-target+ 64)
(defconstant +icon-target-small-fill+ 65)
(defconstant +icon-target-big-fill+ 66)
(defconstant +icon-target-move-fill+ 67)
(defconstant +icon-cursor-move-fill+ 68)
(defconstant +icon-cursor-scale-fill+ 69)
(defconstant +icon-cursor-scale-right-fill+ 70)
(defconstant +icon-cursor-scale-left-fill+ 71)
(defconstant +icon-undo-fill+ 72)
(defconstant +icon-redo-fill+ 73)
(defconstant +icon-reredo-fill+ 74)
(defconstant +icon-mutate-fill+ 75)
(defconstant +icon-rotate-fill+ 76)
(defconstant +icon-repeat-fill+ 77)
(defconstant +icon-shuffle-fill+ 78)
(defconstant +icon-emptybox-small+ 79)
(defconstant +icon-box+ 80)
(defconstant +icon-box-top+ 81)
(defconstant +icon-box-top-right+ 82)
(defconstant +icon-box-right+ 83)
(defconstant +icon-box-bottom-right+ 84)
(defconstant +icon-box-bottom+ 85)
(defconstant +icon-box-bottom-left+ 86)
(defconstant +icon-box-left+ 87)
(defconstant +icon-box-top-left+ 88)
(defconstant +icon-box-center+ 89)
(defconstant +icon-box-circle-mask+ 90)
(defconstant +icon-pot+ 91)
(defconstant +icon-alpha-multiply+ 92)
(defconstant +icon-alpha-clear+ 93)
(defconstant +icon-dithering+ 94)
(defconstant +icon-mipmaps+ 95)
(defconstant +icon-box-grid+ 96)
(defconstant +icon-grid+ 97)
(defconstant +icon-box-corners-small+ 98)
(defconstant +icon-box-corners-big+ 99)
(defconstant +icon-four-boxes+ 100)
(defconstant +icon-grid-fill+ 101)
(defconstant +icon-box-multisize+ 102)
(defconstant +icon-zoom-small+ 103)
(defconstant +icon-zoom-medium+ 104)
(defconstant +icon-zoom-big+ 105)
(defconstant +icon-zoom-all+ 106)
(defconstant +icon-zoom-center+ 107)
(defconstant +icon-box-dots-small+ 108)
(defconstant +icon-box-dots-big+ 109)
(defconstant +icon-box-concentric+ 110)
(defconstant +icon-box-grid-big+ 111)
(defconstant +icon-ok-tick+ 112)
(defconstant +icon-cross+ 113)
(defconstant +icon-arrow-left+ 114)
(defconstant +icon-arrow-right+ 115)
(defconstant +icon-arrow-down+ 116)
(defconstant +icon-arrow-up+ 117)
(defconstant +icon-arrow-left-fill+ 118)
(defconstant +icon-arrow-right-fill+ 119)
(defconstant +icon-arrow-down-fill+ 120)
(defconstant +icon-arrow-up-fill+ 121)
(defconstant +icon-audio+ 122)
(defconstant +icon-fx+ 123)
(defconstant +icon-wave+ 124)
(defconstant +icon-wave-sinus+ 125)
(defconstant +icon-wave-square+ 126)
(defconstant +icon-wave-triangular+ 127)
(defconstant +icon-cross-small+ 128)
(defconstant +icon-player-previous+ 129)
(defconstant +icon-player-play-back+ 130)
(defconstant +icon-player-play+ 131)
(defconstant +icon-player-pause+ 132)
(defconstant +icon-player-stop+ 133)
(defconstant +icon-player-next+ 134)
(defconstant +icon-player-record+ 135)
(defconstant +icon-magnet+ 136)
(defconstant +icon-lock-close+ 137)
(defconstant +icon-lock-open+ 138)
(defconstant +icon-clock+ 139)
(defconstant +icon-tools+ 140)
(defconstant +icon-gear+ 141)
(defconstant +icon-gear-big+ 142)
(defconstant +icon-bin+ 143)
(defconstant +icon-hand-pointer+ 144)
(defconstant +icon-laser+ 145)
(defconstant +icon-coin+ 146)
(defconstant +icon-explosion+ 147)
(defconstant +icon-1up+ 148)
(defconstant +icon-player+ 149)
(defconstant +icon-player-jump+ 150)
(defconstant +icon-key+ 151)
(defconstant +icon-demon+ 152)
(defconstant +icon-text-popup+ 153)
(defconstant +icon-gear-ex+ 154)
(defconstant +icon-crack+ 155)
(defconstant +icon-crack-points+ 156)
(defconstant +icon-star+ 157)
(defconstant +icon-door+ 158)
(defconstant +icon-exit+ 159)
(defconstant +icon-mode-2d+ 160)
(defconstant +icon-mode-3d+ 161)
(defconstant +icon-cube+ 162)
(defconstant +icon-cube-face-top+ 163)
(defconstant +icon-cube-face-left+ 164)
(defconstant +icon-cube-face-front+ 165)
(defconstant +icon-cube-face-bottom+ 166)
(defconstant +icon-cube-face-right+ 167)
(defconstant +icon-cube-face-back+ 168)
(defconstant +icon-camera+ 169)
(defconstant +icon-special+ 170)
(defconstant +icon-link-net+ 171)
(defconstant +icon-link-boxes+ 172)
(defconstant +icon-link-multi+ 173)
(defconstant +icon-link+ 174)
(defconstant +icon-link-broke+ 175)
(defconstant +icon-text-notes+ 176)
(defconstant +icon-notebook+ 177)
(defconstant +icon-suitcase+ 178)
(defconstant +icon-suitcase-zip+ 179)
(defconstant +icon-mailbox+ 180)
(defconstant +icon-monitor+ 181)
(defconstant +icon-printer+ 182)
(defconstant +icon-photo-camera+ 183)
(defconstant +icon-photo-camera-flash+ 184)
(defconstant +icon-house+ 185)
(defconstant +icon-heart+ 186)
(defconstant +icon-corner+ 187)
(defconstant +icon-vertical-bars+ 188)
(defconstant +icon-vertical-bars-fill+ 189)
(defconstant +icon-life-bars+ 190)
(defconstant +icon-info+ 191)
(defconstant +icon-crossline+ 192)
(defconstant +icon-help+ 193)
(defconstant +icon-filetype-alpha+ 194)
(defconstant +icon-filetype-home+ 195)
(defconstant +icon-layers-visible+ 196)
(defconstant +icon-layers+ 197)
(defconstant +icon-window+ 198)
(defconstant +icon-hidpi+ 199)
(defconstant +icon-filetype-binary+ 200)
(defconstant +icon-hex+ 201)
(defconstant +icon-shield+ 202)
(defconstant +icon-file-new+ 203)
(defconstant +icon-folder-add+ 204)
(defconstant +icon-alarm+ 205)
(defconstant +icon-cpu+ 206)
(defconstant +icon-rom+ 207)
(defconstant +icon-step-over+ 208)
(defconstant +icon-step-into+ 209)
(defconstant +icon-step-out+ 210)
(defconstant +icon-restart+ 211)
(defconstant +icon-breakpoint-on+ 212)
(defconstant +icon-breakpoint-off+ 213)
(defconstant +icon-burger-menu+ 214)
(defconstant +icon-case-sensitive+ 215)
(defconstant +icon-reg-exp+ 216)
(defconstant +icon-folder+ 217)
(defconstant +icon-file+ 218)
(defconstant +icon-sand-timer+ 219)
(defconstant +icon-warning+ 220)
(defconstant +icon-help-box+ 221)
(defconstant +icon-info-box+ 222)
(defconstant +icon-priority+ 223)
(defconstant +icon-layers-iso+ 224)
(defconstant +icon-layers2+ 225)
(defconstant +icon-mlayers+ 226)
(defconstant +icon-maps+ 227)
(defconstant +icon-hot+ 228)
(defconstant +icon-label+ 229)
(defconstant +icon-name-id+ 230)
(defconstant +icon-slicing+ 231)
(defconstant +icon-manual-control+ 232)
(defconstant +icon-collision+ 233)
(defconstant +icon-circle-add+ 234)
(defconstant +icon-circle-add-fill+ 235)
(defconstant +icon-circle-warning+ 236)
(defconstant +icon-circle-warning-fill+ 237)
(defconstant +icon-box-more+ 238)
(defconstant +icon-box-more-fill+ 239)
(defconstant +icon-box-minus+ 240)
(defconstant +icon-box-minus-fill+ 241)
(defconstant +icon-union+ 242)
(defconstant +icon-intersection+ 243)
(defconstant +icon-difference+ 244)
(defconstant +icon-sphere+ 245)
(defconstant +icon-cylinder+ 246)
(defconstant +icon-cone+ 247)
(defconstant +icon-ellipsoid+ 248)
(defconstant +icon-capsule+ 249)
(defconstant +icon-filetype-font+ 250)
(defconstant +icon-filetype-3d+ 251)
(defconstant +icon-filetype-code-xml+ 252)
(defconstant +icon-filetype-code-c+ 253)
(defconstant +icon-filetype-code-python+ 254)
(defconstant +icon-filetype-code-js+ 255)
(defconstant +icon-filetype-icon+ 256)

;;----------------------------------------------------------------------------------
;; Module implementation (RAYGUI_IMPLEMENTATION)
;;----------------------------------------------------------------------------------
;; Check if two rectangles are equal, used to validate a slider bounds as an id
(defun check-bounds-id (src dst)
  (and (= (truncate (rectangle-x src)) (truncate (rectangle-x dst)))
       (= (truncate (rectangle-y src)) (truncate (rectangle-y dst)))
       (= (truncate (rectangle-width src)) (truncate (rectangle-width dst)))
       (= (truncate (rectangle-height src)) (truncate (rectangle-height dst)))))

;; Embedded icons, no external file provided
(defconstant +raygui-icon-size+ 16)              ; Size of icons in pixels (squared)
(defconstant +raygui-icon-max-icons+ 512)        ; Maximum number of icons
(defconstant +raygui-icon-max-font-backed+ 257)  ; Maximum number of icons to back in font atlas
(defconstant +raygui-icon-max-name-length+ 32)   ; Maximum length of icon name id
(defconstant +raygui-icon-font-atlas-padding+ 1) ; Padding between backed icons in font atlas

;; Icons data is defined by bit array (every bit represents one pixel)
;; Those arrays are stored as unsigned int data arrays, so,
;; every array element defines 32 pixels (bits) of information
;; One icon is defined by 8 int, (8 int*32 bit = 256 bit = 16*16 pixels)
;; NOTE: Number of elemens depend on RAYGUI_ICON_SIZE (by default 16x16 pixels)
(defconstant +raygui-icon-data-elements+ (truncate (* +raygui-icon-size+ +raygui-icon-size+) 32))

;; Icons data for all gui possible icons (allocated on data segment by default)
;;
;; NOTE 1: Every icon is codified in binary form, using 1 bit per pixel, so,
;; every 16x16 icon requires 8 integers (16*16/32) to be stored
;;
;; NOTE 2: A different icon set could be loaded over this array using GuiLoadIcons(),
;; but loaded icons set must be same RAYGUI_ICON_SIZE and no more than RAYGUI_ICON_MAX_ICONS
;;
;; guiIcons size is by default: 512*(16*16/32) = 16384 bytes = 16 KB
(defparameter *gui-icons*
  (let ((data (make-array (* +raygui-icon-max-icons+ +raygui-icon-data-elements+) :element-type '(unsigned-byte 32) :initial-element 0)))
    (replace data
             '(
               #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000   ; ICON_NONE
               #x3ff80000 #x2f082008 #x2042207e #x40027fc2 #x40024002 #x40024002 #x40024002 #x00007ffe   ; ICON_FOLDER_FILE_OPEN
               #x3ffe0000 #x44226422 #x400247e2 #x5ffa4002 #x57ea500a #x500a500a #x40025ffa #x00007ffe   ; ICON_FILE_SAVE_CLASSIC
               #x00000000 #x0042007e #x40027fc2 #x40024002 #x41024002 #x44424282 #x793e4102 #x00000100   ; ICON_FOLDER_OPEN
               #x00000000 #x0042007e #x40027fc2 #x40024002 #x41024102 #x44424102 #x793e4282 #x00000000   ; ICON_FOLDER_SAVE
               #x3ff00000 #x201c2010 #x20042004 #x21042004 #x24442284 #x21042104 #x20042104 #x00003ffc   ; ICON_FILE_OPEN
               #x3ff00000 #x201c2010 #x20042004 #x21042004 #x21042104 #x22842444 #x20042104 #x00003ffc   ; ICON_FILE_SAVE
               #x3ff00000 #x201c2010 #x00042004 #x20041004 #x20844784 #x00841384 #x20042784 #x00003ffc   ; ICON_FILE_EXPORT
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x22042204 #x22042f84 #x20042204 #x00003ffc   ; ICON_FILE_ADD
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x25042884 #x25042204 #x20042884 #x00003ffc   ; ICON_FILE_DELETE
               #x3ff00000 #x201c2010 #x20042004 #x20042ff4 #x20042ff4 #x20042ff4 #x20042004 #x00003ffc   ; ICON_FILETYPE_TEXT
               #x3ff00000 #x201c2010 #x27042004 #x244424c4 #x26442444 #x20642664 #x20042004 #x00003ffc   ; ICON_FILETYPE_AUDIO
               #x3ff00000 #x201c2010 #x26042604 #x20042004 #x35442884 #x2414222c #x20042004 #x00003ffc   ; ICON_FILETYPE_IMAGE
               #x3ff00000 #x201c2010 #x20c42004 #x22442144 #x22442444 #x20c42144 #x20042004 #x00003ffc   ; ICON_FILETYPE_PLAY
               #x3ff00000 #x3ffc2ff0 #x3f3c2ff4 #x3dbc2eb4 #x3dbc2bb4 #x3f3c2eb4 #x3ffc2ff4 #x00002ff4   ; ICON_FILETYPE_VIDEO
               #x3ff00000 #x201c2010 #x21842184 #x21842004 #x21842184 #x21842184 #x20042184 #x00003ffc   ; ICON_FILETYPE_INFO
               #x0ff00000 #x381c0810 #x28042804 #x28042804 #x28042804 #x28042804 #x20102ffc #x00003ff0   ; ICON_FILE_COPY
               #x00000000 #x701c0000 #x079c1e14 #x55a000f0 #x079c00f0 #x701c1e14 #x00000000 #x00000000   ; ICON_FILE_CUT
               #x01c00000 #x13e41bec #x3f841004 #x204420c4 #x20442044 #x20442044 #x207c2044 #x00003fc0   ; ICON_FILE_PASTE
               #x00000000 #x3aa00fe0 #x2abc2aa0 #x2aa42aa4 #x20042aa4 #x20042004 #x3ffc2004 #x00000000   ; ICON_CURSOR_HAND
               #x00000000 #x003c000c #x030800c8 #x30100c10 #x10202020 #x04400840 #x01800280 #x00000000   ; ICON_CURSOR_POINTER
               #x00000000 #x00180000 #x01f00078 #x03e007f0 #x07c003e0 #x04000e40 #x00000000 #x00000000   ; ICON_CURSOR_CLASSIC
               #x00000000 #x04000000 #x11000a00 #x04400a80 #x01100220 #x00580088 #x00000038 #x00000000   ; ICON_PENCIL
               #x04000000 #x15000a00 #x50402880 #x14102820 #x05040a08 #x015c028c #x007c00bc #x00000000   ; ICON_PENCIL_BIG
               #x01c00000 #x01400140 #x01400140 #x0ff80140 #x0ff80808 #x0aa80808 #x0aa80aa8 #x00000ff8   ; ICON_BRUSH_CLASSIC
               #x1ffc0000 #x5ffc7ffe #x40004000 #x00807f80 #x01c001c0 #x01c001c0 #x01c001c0 #x00000080   ; ICON_BRUSH_PAINTER
               #x00000000 #x00800000 #x01c00080 #x03e001c0 #x07f003e0 #x036006f0 #x000001c0 #x00000000   ; ICON_WATER_DROP
               #x00000000 #x3e003800 #x1f803f80 #x0c201e40 #x02080c10 #x00840104 #x00380044 #x00000000   ; ICON_COLOR_PICKER
               #x00000000 #x07800300 #x1fe00fc0 #x3f883fd0 #x0e021f04 #x02040402 #x00f00108 #x00000000   ; ICON_RUBBER
               #x00c00000 #x02800140 #x08200440 #x20081010 #x2ffe3004 #x03f807fc #x00e001f0 #x00000040   ; ICON_COLOR_BUCKET
               #x00000000 #x21843ffc #x01800180 #x01800180 #x01800180 #x01800180 #x03c00180 #x00000000   ; ICON_TEXT_T
               #x00800000 #x01400180 #x06200340 #x0c100620 #x1ff80c10 #x380c1808 #x70067004 #x0000f80f   ; ICON_TEXT_A
               #x78000000 #x50004000 #x00004800 #x03c003c0 #x03c003c0 #x00100000 #x0002000a #x0000000e   ; ICON_SCALE
               #x75560000 #x5e004002 #x54001002 #x41001202 #x408200fe #x40820082 #x40820082 #x00006afe   ; ICON_RESIZE
               #x00000000 #x3f003f00 #x3f003f00 #x3f003f00 #x00400080 #x001c0020 #x001c001c #x00000000   ; ICON_FILTER_POINT
               #x6d800000 #x00004080 #x40804080 #x40800000 #x00406d80 #x001c0020 #x001c001c #x00000000   ; ICON_FILTER_BILINEAR
               #x40080000 #x1ffe2008 #x14081008 #x11081208 #x10481088 #x10081028 #x10047ff8 #x00001002   ; ICON_CROP
               #x00100000 #x3ffc0010 #x2ab03550 #x22b02550 #x20b02150 #x20302050 #x2000fff0 #x00002000   ; ICON_CROP_ALPHA
               #x40000000 #x1ff82000 #x04082808 #x01082208 #x00482088 #x00182028 #x35542008 #x00000002   ; ICON_SQUARE_TOGGLE
               #x00000000 #x02800280 #x06c006c0 #x0ea00ee0 #x1e901eb0 #x3e883e98 #x7efc7e8c #x00000000   ; ICON_SYMMETRY
               #x01000000 #x05600100 #x1d480d50 #x7d423d44 #x3d447d42 #x0d501d48 #x01000560 #x00000100   ; ICON_SYMMETRY_HORIZONTAL
               #x01800000 #x04200240 #x10080810 #x00001ff8 #x00007ffe #x0ff01ff8 #x03c007e0 #x00000180   ; ICON_SYMMETRY_VERTICAL
               #x00000000 #x010800f0 #x02040204 #x02040204 #x07f00308 #x1c000e00 #x30003800 #x00000000   ; ICON_LENS
               #x00000000 #x061803f0 #x08240c0c #x08040814 #x0c0c0804 #x23f01618 #x18002400 #x00000000   ; ICON_LENS_BIG
               #x00000000 #x00000000 #x1c7007c0 #x638e3398 #x1c703398 #x000007c0 #x00000000 #x00000000   ; ICON_EYE_ON
               #x00000000 #x10002000 #x04700fc0 #x610e3218 #x1c703098 #x001007a0 #x00000008 #x00000000   ; ICON_EYE_OFF
               #x00000000 #x00007ffc #x40047ffc #x10102008 #x04400820 #x02800280 #x02800280 #x00000100   ; ICON_FILTER_TOP
               #x00000000 #x40027ffe #x10082004 #x04200810 #x02400240 #x02400240 #x01400240 #x000000c0   ; ICON_FILTER
               #x00800000 #x00800080 #x00000080 #x3c9e0000 #x00000000 #x00800080 #x00800080 #x00000000   ; ICON_TARGET_POINT
               #x00800000 #x00800080 #x00800080 #x3f7e01c0 #x008001c0 #x00800080 #x00800080 #x00000000   ; ICON_TARGET_SMALL
               #x00800000 #x00800080 #x03e00080 #x3e3e0220 #x03e00220 #x00800080 #x00800080 #x00000000   ; ICON_TARGET_BIG
               #x01000000 #x04400280 #x01000100 #x43842008 #x43849ab2 #x01002008 #x04400100 #x01000280   ; ICON_TARGET_MOVE
               #x01000000 #x04400280 #x01000100 #x41042108 #x41049ff2 #x01002108 #x04400100 #x01000280   ; ICON_CURSOR_MOVE
               #x781e0000 #x500a4002 #x04204812 #x00000240 #x02400000 #x48120420 #x4002500a #x0000781e   ; ICON_CURSOR_SCALE
               #x00000000 #x20003c00 #x24002800 #x01000200 #x00400080 #x00140024 #x003c0004 #x00000000   ; ICON_CURSOR_SCALE_RIGHT
               #x00000000 #x0004003c #x00240014 #x00800040 #x02000100 #x28002400 #x3c002000 #x00000000   ; ICON_CURSOR_SCALE_LEFT
               #x00000000 #x00100020 #x10101fc8 #x10001020 #x10001000 #x10001000 #x00001fc0 #x00000000   ; ICON_UNDO
               #x00000000 #x08000400 #x080813f8 #x00080408 #x00080008 #x00080008 #x000003f8 #x00000000   ; ICON_REDO
               #x00000000 #x3ffc0000 #x20042004 #x20002000 #x20402000 #x3f902020 #x00400020 #x00000000   ; ICON_REREDO
               #x00000000 #x3ffc0000 #x20042004 #x27fc2004 #x20202000 #x3fc82010 #x00200010 #x00000000   ; ICON_MUTATE
               #x00000000 #x0ff00000 #x10081818 #x11801008 #x10001180 #x18101020 #x00100fc8 #x00000020   ; ICON_ROTATE
               #x00000000 #x04000200 #x240429fc #x20042204 #x20442004 #x3f942024 #x00400020 #x00000000   ; ICON_REPEAT
               #x00000000 #x20001000 #x22104c0e #x00801120 #x11200040 #x4c0e2210 #x10002000 #x00000000   ; ICON_SHUFFLE
               #x7ffe0000 #x50024002 #x44024802 #x41024202 #x40424082 #x40124022 #x4002400a #x00007ffe   ; ICON_EMPTYBOX
               #x00800000 #x03e00080 #x08080490 #x3c9e0808 #x08080808 #x03e00490 #x00800080 #x00000000   ; ICON_TARGET
               #x00800000 #x00800080 #x00800080 #x3ffe01c0 #x008001c0 #x00800080 #x00800080 #x00000000   ; ICON_TARGET_SMALL_FILL
               #x00800000 #x00800080 #x03e00080 #x3ffe03e0 #x03e003e0 #x00800080 #x00800080 #x00000000   ; ICON_TARGET_BIG_FILL
               #x01000000 #x07c00380 #x01000100 #x638c2008 #x638cfbbe #x01002008 #x07c00100 #x01000380   ; ICON_TARGET_MOVE_FILL
               #x01000000 #x07c00380 #x01000100 #x610c2108 #x610cfffe #x01002108 #x07c00100 #x01000380   ; ICON_CURSOR_MOVE_FILL
               #x781e0000 #x6006700e #x04204812 #x00000240 #x02400000 #x48120420 #x700e6006 #x0000781e   ; ICON_CURSOR_SCALE_FILL
               #x00000000 #x38003c00 #x24003000 #x01000200 #x00400080 #x000c0024 #x003c001c #x00000000   ; ICON_CURSOR_SCALE_RIGHT_FILL
               #x00000000 #x001c003c #x0024000c #x00800040 #x02000100 #x30002400 #x3c003800 #x00000000   ; ICON_CURSOR_SCALE_LEFT_FILL
               #x00000000 #x00300020 #x10301ff8 #x10001020 #x10001000 #x10001000 #x00001fc0 #x00000000   ; ICON_UNDO_FILL
               #x00000000 #x0c000400 #x0c081ff8 #x00080408 #x00080008 #x00080008 #x000003f8 #x00000000   ; ICON_REDO_FILL
               #x00000000 #x3ffc0000 #x20042004 #x20002000 #x20402000 #x3ff02060 #x00400060 #x00000000   ; ICON_REREDO_FILL
               #x00000000 #x3ffc0000 #x20042004 #x27fc2004 #x20202000 #x3ff82030 #x00200030 #x00000000   ; ICON_MUTATE_FILL
               #x00000000 #x0ff00000 #x10081818 #x11801008 #x10001180 #x18301020 #x00300ff8 #x00000020   ; ICON_ROTATE_FILL
               #x00000000 #x06000200 #x26042ffc #x20042204 #x20442004 #x3ff42064 #x00400060 #x00000000   ; ICON_REPEAT_FILL
               #x00000000 #x30001000 #x32107c0e #x00801120 #x11200040 #x7c0e3210 #x10003000 #x00000000   ; ICON_SHUFFLE_FILL
               #x00000000 #x30043ffc #x24042804 #x21042204 #x20442084 #x20142024 #x3ffc200c #x00000000   ; ICON_EMPTYBOX_SMALL
               #x00000000 #x20043ffc #x20042004 #x20042004 #x20042004 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX
               #x00000000 #x23c43ffc #x23c423c4 #x200423c4 #x20042004 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_TOP
               #x00000000 #x3e043ffc #x3e043e04 #x20043e04 #x20042004 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_TOP_RIGHT
               #x00000000 #x20043ffc #x20042004 #x3e043e04 #x3e043e04 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_RIGHT
               #x00000000 #x20043ffc #x20042004 #x20042004 #x3e042004 #x3e043e04 #x3ffc3e04 #x00000000   ; ICON_BOX_BOTTOM_RIGHT
               #x00000000 #x20043ffc #x20042004 #x20042004 #x23c42004 #x23c423c4 #x3ffc23c4 #x00000000   ; ICON_BOX_BOTTOM
               #x00000000 #x20043ffc #x20042004 #x20042004 #x207c2004 #x207c207c #x3ffc207c #x00000000   ; ICON_BOX_BOTTOM_LEFT
               #x00000000 #x20043ffc #x20042004 #x207c207c #x207c207c #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_LEFT
               #x00000000 #x207c3ffc #x207c207c #x2004207c #x20042004 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_TOP_LEFT
               #x00000000 #x20043ffc #x20042004 #x23c423c4 #x23c423c4 #x20042004 #x3ffc2004 #x00000000   ; ICON_BOX_CENTER
               #x7ffe0000 #x40024002 #x47e24182 #x4ff247e2 #x47e24ff2 #x418247e2 #x40024002 #x00007ffe   ; ICON_BOX_CIRCLE_MASK
               #x7fff0000 #x40014001 #x40014001 #x49555ddd #x4945495d #x400149c5 #x40014001 #x00007fff   ; ICON_POT
               #x7ffe0000 #x53327332 #x44ce4cce #x41324332 #x404e40ce #x48125432 #x4006540e #x00007ffe   ; ICON_ALPHA_MULTIPLY
               #x7ffe0000 #x53327332 #x44ce4cce #x41324332 #x5c4e40ce #x44124432 #x40065c0e #x00007ffe   ; ICON_ALPHA_CLEAR
               #x7ffe0000 #x42fe417e #x42fe417e #x42fe417e #x42fe417e #x42fe417e #x42fe417e #x00007ffe   ; ICON_DITHERING
               #x07fe0000 #x1ffa0002 #x7fea000a #x402a402a #x5b2a512a #x5128552a #x40205128 #x00007fe0   ; ICON_MIPMAPS
               #x00000000 #x1ff80000 #x12481248 #x12481ff8 #x1ff81248 #x12481248 #x00001ff8 #x00000000   ; ICON_BOX_GRID
               #x12480000 #x7ffe1248 #x12481248 #x12487ffe #x7ffe1248 #x12481248 #x12487ffe #x00001248   ; ICON_GRID
               #x00000000 #x1c380000 #x1c3817e8 #x08100810 #x08100810 #x17e81c38 #x00001c38 #x00000000   ; ICON_BOX_CORNERS_SMALL
               #x700e0000 #x700e5ffa #x20042004 #x20042004 #x20042004 #x20042004 #x5ffa700e #x0000700e   ; ICON_BOX_CORNERS_BIG
               #x3f7e0000 #x21422142 #x21422142 #x00003f7e #x21423f7e #x21422142 #x3f7e2142 #x00000000   ; ICON_FOUR_BOXES
               #x00000000 #x3bb80000 #x3bb83bb8 #x3bb80000 #x3bb83bb8 #x3bb80000 #x3bb83bb8 #x00000000   ; ICON_GRID_FILL
               #x7ffe0000 #x7ffe7ffe #x77fe7000 #x77fe77fe #x777e7700 #x777e777e #x777e777e #x0000777e   ; ICON_BOX_MULTISIZE
               #x781e0000 #x40024002 #x00004002 #x01800000 #x00000180 #x40020000 #x40024002 #x0000781e   ; ICON_ZOOM_SMALL
               #x781e0000 #x40024002 #x00004002 #x03c003c0 #x03c003c0 #x40020000 #x40024002 #x0000781e   ; ICON_ZOOM_MEDIUM
               #x781e0000 #x40024002 #x07e04002 #x07e007e0 #x07e007e0 #x400207e0 #x40024002 #x0000781e   ; ICON_ZOOM_BIG
               #x781e0000 #x5ffa4002 #x1ff85ffa #x1ff81ff8 #x1ff81ff8 #x5ffa1ff8 #x40025ffa #x0000781e   ; ICON_ZOOM_ALL
               #x00000000 #x2004381c #x00002004 #x00000000 #x00000000 #x20040000 #x381c2004 #x00000000   ; ICON_ZOOM_CENTER
               #x00000000 #x1db80000 #x10081008 #x10080000 #x00001008 #x10081008 #x00001db8 #x00000000   ; ICON_BOX_DOTS_SMALL
               #x35560000 #x00002002 #x00002002 #x00002002 #x00002002 #x00002002 #x35562002 #x00000000   ; ICON_BOX_DOTS_BIG
               #x7ffe0000 #x40024002 #x48124ff2 #x49924812 #x48124992 #x4ff24812 #x40024002 #x00007ffe   ; ICON_BOX_CONCENTRIC
               #x00000000 #x10841ffc #x10841084 #x1ffc1084 #x10841084 #x10841084 #x00001ffc #x00000000   ; ICON_BOX_GRID_BIG
               #x00000000 #x00000000 #x10000000 #x04000800 #x01040200 #x00500088 #x00000020 #x00000000   ; ICON_OK_TICK
               #x00000000 #x10080000 #x04200810 #x01800240 #x02400180 #x08100420 #x00001008 #x00000000   ; ICON_CROSS
               #x00000000 #x02000000 #x00800100 #x00200040 #x00200010 #x00800040 #x02000100 #x00000000   ; ICON_ARROW_LEFT
               #x00000000 #x00400000 #x01000080 #x04000200 #x04000800 #x01000200 #x00400080 #x00000000   ; ICON_ARROW_RIGHT
               #x00000000 #x00000000 #x00000000 #x08081004 #x02200410 #x00800140 #x00000000 #x00000000   ; ICON_ARROW_DOWN
               #x00000000 #x00000000 #x01400080 #x04100220 #x10040808 #x00000000 #x00000000 #x00000000   ; ICON_ARROW_UP
               #x00000000 #x02000000 #x03800300 #x03e003c0 #x03e003f0 #x038003c0 #x02000300 #x00000000   ; ICON_ARROW_LEFT_FILL
               #x00000000 #x00400000 #x01c000c0 #x07c003c0 #x07c00fc0 #x01c003c0 #x004000c0 #x00000000   ; ICON_ARROW_RIGHT_FILL
               #x00000000 #x00000000 #x00000000 #x0ff81ffc #x03e007f0 #x008001c0 #x00000000 #x00000000   ; ICON_ARROW_DOWN_FILL
               #x00000000 #x00000000 #x01c00080 #x07f003e0 #x1ffc0ff8 #x00000000 #x00000000 #x00000000   ; ICON_ARROW_UP_FILL
               #x00000000 #x18a008c0 #x32881290 #x24822686 #x26862482 #x12903288 #x08c018a0 #x00000000   ; ICON_AUDIO
               #x00000000 #x04800780 #x004000c0 #x662000f0 #x08103c30 #x130a0e18 #x0000318e #x00000000   ; ICON_FX
               #x00000000 #x00800000 #x08880888 #x2aaa0a8a #x0a8a2aaa #x08880888 #x00000080 #x00000000   ; ICON_WAVE
               #x00000000 #x00600000 #x01080090 #x02040108 #x42044204 #x24022402 #x00001800 #x00000000   ; ICON_WAVE_SINUS
               #x00000000 #x07f80000 #x04080408 #x04080408 #x04080408 #x7c0e0408 #x00000000 #x00000000   ; ICON_WAVE_SQUARE
               #x00000000 #x00000000 #x00a00040 #x22084110 #x08021404 #x00000000 #x00000000 #x00000000   ; ICON_WAVE_TRIANGULAR
               #x00000000 #x00000000 #x04200000 #x01800240 #x02400180 #x00000420 #x00000000 #x00000000   ; ICON_CROSS_SMALL
               #x00000000 #x18380000 #x12281428 #x10a81128 #x112810a8 #x14281228 #x00001838 #x00000000   ; ICON_PLAYER_PREVIOUS
               #x00000000 #x18000000 #x11801600 #x10181060 #x10601018 #x16001180 #x00001800 #x00000000   ; ICON_PLAYER_PLAY_BACK
               #x00000000 #x00180000 #x01880068 #x18080608 #x06081808 #x00680188 #x00000018 #x00000000   ; ICON_PLAYER_PLAY
               #x00000000 #x1e780000 #x12481248 #x12481248 #x12481248 #x12481248 #x00001e78 #x00000000   ; ICON_PLAYER_PAUSE
               #x00000000 #x1ff80000 #x10081008 #x10081008 #x10081008 #x10081008 #x00001ff8 #x00000000   ; ICON_PLAYER_STOP
               #x00000000 #x1c180000 #x14481428 #x15081488 #x14881508 #x14281448 #x00001c18 #x00000000   ; ICON_PLAYER_NEXT
               #x00000000 #x03c00000 #x08100420 #x10081008 #x10081008 #x04200810 #x000003c0 #x00000000   ; ICON_PLAYER_RECORD
               #x00000000 #x0c3007e0 #x13c81818 #x14281668 #x14281428 #x1c381c38 #x08102244 #x00000000   ; ICON_MAGNET
               #x07c00000 #x08200820 #x3ff80820 #x23882008 #x21082388 #x20082108 #x1ff02008 #x00000000   ; ICON_LOCK_CLOSE
               #x07c00000 #x08000800 #x3ff80800 #x23882008 #x21082388 #x20082108 #x1ff02008 #x00000000   ; ICON_LOCK_OPEN
               #x01c00000 #x0c180770 #x3086188c #x60832082 #x60034781 #x30062002 #x0c18180c #x01c00770   ; ICON_CLOCK
               #x0a200000 #x1b201b20 #x04200e20 #x04200420 #x04700420 #x0e700e70 #x0e700e70 #x04200e70   ; ICON_TOOLS
               #x01800000 #x3bdc318c #x0ff01ff8 #x7c3e1e78 #x1e787c3e #x1ff80ff0 #x318c3bdc #x00000180   ; ICON_GEAR
               #x01800000 #x3ffc318c #x1c381ff8 #x781e1818 #x1818781e #x1ff81c38 #x318c3ffc #x00000180   ; ICON_GEAR_BIG
               #x00000000 #x08080ff8 #x08081ffc #x0aa80aa8 #x0aa80aa8 #x0aa80aa8 #x08080aa8 #x00000ff8   ; ICON_BIN
               #x00000000 #x00000000 #x20043ffc #x08043f84 #x04040f84 #x04040784 #x000007fc #x00000000   ; ICON_HAND_POINTER
               #x00000000 #x24400400 #x00001480 #x6efe0e00 #x00000e00 #x24401480 #x00000400 #x00000000   ; ICON_LASER
               #x00000000 #x03c00000 #x08300460 #x11181118 #x11181118 #x04600830 #x000003c0 #x00000000   ; ICON_COIN
               #x00000000 #x10880080 #x06c00810 #x366c07e0 #x07e00240 #x00001768 #x04200240 #x00000000   ; ICON_EXPLOSION
               #x00000000 #x3d280000 #x2528252c #x3d282528 #x05280528 #x05e80528 #x00000000 #x00000000   ; ICON_1UP
               #x01800000 #x03c003c0 #x018003c0 #x0ff007e0 #x0bd00bd0 #x0a500bd0 #x02400240 #x02400240   ; ICON_PLAYER
               #x01800000 #x03c003c0 #x118013c0 #x03c81ff8 #x07c003c8 #x04400440 #x0c080478 #x00000000   ; ICON_PLAYER_JUMP
               #x3ff80000 #x30183ff8 #x30183018 #x3ff83ff8 #x03000300 #x03c003c0 #x03e00300 #x000003e0   ; ICON_KEY
               #x3ff80000 #x3ff83ff8 #x33983ff8 #x3ff83398 #x3ff83ff8 #x00000540 #x0fe00aa0 #x00000fe0   ; ICON_DEMON
               #x00000000 #x0ff00000 #x20041008 #x25442004 #x10082004 #x06000bf0 #x00000300 #x00000000   ; ICON_TEXT_POPUP
               #x00000000 #x11440000 #x07f00be8 #x1c1c0e38 #x1c1c0c18 #x07f00e38 #x11440be8 #x00000000   ; ICON_GEAR_EX
               #x00000000 #x20080000 #x0c601010 #x07c00fe0 #x07c007c0 #x0c600fe0 #x20081010 #x00000000   ; ICON_CRACK
               #x00000000 #x20080000 #x0c601010 #x04400fe0 #x04405554 #x0c600fe0 #x20081010 #x00000000   ; ICON_CRACK_POINTS
               #x00000000 #x00800080 #x01c001c0 #x1ffc3ffe #x03e007f0 #x07f003e0 #x0c180770 #x00000808   ; ICON_STAR
               #x0ff00000 #x08180810 #x08100818 #x0a100810 #x08180810 #x08100818 #x08100810 #x00001ff8   ; ICON_DOOR
               #x0ff00000 #x08100810 #x08100810 #x10100010 #x4f902010 #x10102010 #x08100010 #x00000ff0   ; ICON_EXIT
               #x00040000 #x001f000e #x0ef40004 #x12f41284 #x0ef41214 #x10040004 #x7ffc3004 #x10003000   ; ICON_MODE_2D
               #x78040000 #x501f600e #x0ef44004 #x12f41284 #x0ef41284 #x10140004 #x7ffc300c #x10003000   ; ICON_MODE_3D
               #x7fe00000 #x50286030 #x47fe4804 #x44224402 #x44224422 #x241275e2 #x0c06140a #x000007fe   ; ICON_CUBE
               #x7fe00000 #x5ff87ff0 #x47fe4ffc #x44224402 #x44224422 #x241275e2 #x0c06140a #x000007fe   ; ICON_CUBE_FACE_TOP
               #x7fe00000 #x50386030 #x47c2483c #x443e443e #x443e443e #x241e75fe #x0c06140e #x000007fe   ; ICON_CUBE_FACE_LEFT
               #x7fe00000 #x50286030 #x47fe4804 #x47fe47fe #x47fe47fe #x27fe77fe #x0ffe17fe #x000007fe   ; ICON_CUBE_FACE_FRONT
               #x7fe00000 #x50286030 #x47fe4804 #x44224402 #x44224422 #x3bf27be2 #x0bfe1bfa #x000007fe   ; ICON_CUBE_FACE_BOTTOM
               #x7fe00000 #x70286030 #x7ffe7804 #x7c227c02 #x7c227c22 #x3c127de2 #x0c061c0a #x000007fe   ; ICON_CUBE_FACE_RIGHT
               #x7fe00000 #x6fe85ff0 #x781e77e4 #x7be27be2 #x7be27be2 #x24127be2 #x0c06140a #x000007fe   ; ICON_CUBE_FACE_BACK
               #x00000000 #x2a0233fe #x22022602 #x22022202 #x2a022602 #x00a033fe #x02080110 #x00000000   ; ICON_CAMERA
               #x00000000 #x200c3ffc #x000c000c #x3ffc000c #x30003000 #x30003000 #x3ffc3004 #x00000000   ; ICON_SPECIAL
               #x00000000 #x0022003e #x012201e2 #x0100013e #x01000100 #x79000100 #x4f004900 #x00007800   ; ICON_LINK_NET
               #x00000000 #x44007c00 #x45004600 #x00627cbe #x00620022 #x45007cbe #x44004600 #x00007c00   ; ICON_LINK_BOXES
               #x00000000 #x0044007c #x0010007c #x3f100010 #x3f1021f0 #x3f100010 #x3f0021f0 #x00000000   ; ICON_LINK_MULTI
               #x00000000 #x0044007c #x00440044 #x0010007c #x00100010 #x44107c10 #x440047f0 #x00007c00   ; ICON_LINK
               #x00000000 #x0044007c #x00440044 #x0000007c #x00000010 #x44007c10 #x44004550 #x00007c00   ; ICON_LINK_BROKE
               #x02a00000 #x22a43ffc #x20042004 #x20042ff4 #x20042ff4 #x20042ff4 #x20042004 #x00003ffc   ; ICON_TEXT_NOTES
               #x3ffc0000 #x20042004 #x245e27c4 #x27c42444 #x2004201e #x201e2004 #x20042004 #x00003ffc   ; ICON_NOTEBOOK
               #x00000000 #x07e00000 #x04200420 #x24243ffc #x24242424 #x24242424 #x3ffc2424 #x00000000   ; ICON_SUITCASE
               #x00000000 #x0fe00000 #x08200820 #x40047ffc #x7ffc5554 #x40045554 #x7ffc4004 #x00000000   ; ICON_SUITCASE_ZIP
               #x00000000 #x20043ffc #x3ffc2004 #x13c81008 #x100813c8 #x10081008 #x1ff81008 #x00000000   ; ICON_MAILBOX
               #x00000000 #x40027ffe #x5ffa5ffa #x5ffa5ffa #x40025ffa #x03c07ffe #x1ff81ff8 #x00000000   ; ICON_MONITOR
               #x0ff00000 #x6bfe7ffe #x7ffe7ffe #x68167ffe #x08106816 #x08100810 #x0ff00810 #x00000000   ; ICON_PRINTER
               #x3ff80000 #xfffe2008 #x870a8002 #x904a888a #x904a904a #x870a888a #xfffe8002 #x00000000   ; ICON_PHOTO_CAMERA
               #x0fc00000 #xfcfe0cd8 #x8002fffe #x84428382 #x84428442 #x80028382 #xfffe8002 #x00000000   ; ICON_PHOTO_CAMERA_FLASH
               #x00000000 #x02400180 #x08100420 #x20041008 #x23c42004 #x22442244 #x3ffc2244 #x00000000   ; ICON_HOUSE
               #x00000000 #x1c700000 #x3ff83ef8 #x3ff83ff8 #x0fe01ff0 #x038007c0 #x00000100 #x00000000   ; ICON_HEART
               #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x80000000 #xe000c000   ; ICON_CORNER
               #x00000000 #x14001c00 #x15c01400 #x15401540 #x155c1540 #x15541554 #x1ddc1554 #x00000000   ; ICON_VERTICAL_BARS
               #x00000000 #x03000300 #x1b001b00 #x1b601b60 #x1b6c1b60 #x1b6c1b6c #x1b6c1b6c #x00000000   ; ICON_VERTICAL_BARS_FILL
               #x00000000 #x00000000 #x403e7ffe #x7ffe403e #x7ffe0000 #x43fe43fe #x00007ffe #x00000000   ; ICON_LIFE_BARS
               #x7ffc0000 #x43844004 #x43844284 #x43844004 #x42844284 #x42844284 #x40044384 #x00007ffc   ; ICON_INFO
               #x40008000 #x10002000 #x04000800 #x01000200 #x00400080 #x00100020 #x00040008 #x00010002   ; ICON_CROSSLINE
               #x00000000 #x1ff01ff0 #x18301830 #x1f001830 #x03001f00 #x00000300 #x03000300 #x00000000   ; ICON_HELP
               #x3ff00000 #x2abc3550 #x2aac3554 #x2aac3554 #x2aac3554 #x2aac3554 #x2aac3554 #x00003ffc   ; ICON_FILETYPE_ALPHA
               #x3ff00000 #x201c2010 #x22442184 #x28142424 #x29942814 #x2ff42994 #x20042004 #x00003ffc   ; ICON_FILETYPE_HOME
               #x07fe0000 #x04020402 #x7fe20402 #x44224422 #x44224422 #x402047fe #x40204020 #x00007fe0   ; ICON_LAYERS_VISIBLE
               #x07fe0000 #x04020402 #x7c020402 #x44024402 #x44024402 #x402047fe #x40204020 #x00007fe0   ; ICON_LAYERS
               #x00000000 #x40027ffe #x7ffe4002 #x40024002 #x40024002 #x40024002 #x7ffe4002 #x00000000   ; ICON_WINDOW
               #x09100000 #x09f00910 #x09100910 #x00000910 #x24a2779e #x27a224a2 #x709e20a2 #x00000000   ; ICON_HIDPI
               #x3ff00000 #x201c2010 #x2a842e84 #x2e842a84 #x2ba42004 #x2aa42aa4 #x20042ba4 #x00003ffc   ; ICON_FILETYPE_BINARY
               #x00000000 #x00000000 #x00120012 #x4a5e4bd2 #x485233d2 #x00004bd2 #x00000000 #x00000000   ; ICON_HEX
               #x01800000 #x381c0660 #x23c42004 #x23c42044 #x13c82204 #x08101008 #x02400420 #x00000180   ; ICON_SHIELD
               #x007e0000 #x20023fc2 #x40227fe2 #x400a403a #x400a400a #x400a400a #x4008400e #x00007ff8   ; ICON_FILE_NEW
               #x00000000 #x0042007e #x40027fc2 #x44024002 #x5f024402 #x44024402 #x7ffe4002 #x00000000   ; ICON_FOLDER_ADD
               #x44220000 #x12482244 #xf3cf0000 #x14280420 #x48122424 #x08100810 #x1ff81008 #x03c00420   ; ICON_ALARM
               #x0aa00000 #x1ff80aa0 #x1068700e #x1008706e #x1008700e #x1008700e #x0aa01ff8 #x00000aa0   ; ICON_CPU
               #x07e00000 #x04201db8 #x04a01c38 #x04a01d38 #x04a01d38 #x04a01d38 #x04201d38 #x000007e0   ; ICON_ROM
               #x00000000 #x03c00000 #x3c382ff0 #x3c04380c #x01800000 #x03c003c0 #x00000180 #x00000000   ; ICON_STEP_OVER
               #x01800000 #x01800180 #x01800180 #x03c007e0 #x00000180 #x01800000 #x03c003c0 #x00000180   ; ICON_STEP_INTO
               #x01800000 #x07e003c0 #x01800180 #x01800180 #x00000180 #x01800000 #x03c003c0 #x00000180   ; ICON_STEP_OUT
               #x00000000 #x0ff003c0 #x181c1c34 #x303c301c #x30003000 #x1c301800 #x03c00ff0 #x00000000   ; ICON_RESTART
               #x00000000 #x00000000 #x07e003c0 #x0ff00ff0 #x0ff00ff0 #x03c007e0 #x00000000 #x00000000   ; ICON_BREAKPOINT_ON
               #x00000000 #x00000000 #x042003c0 #x08100810 #x08100810 #x03c00420 #x00000000 #x00000000   ; ICON_BREAKPOINT_OFF
               #x00000000 #x00000000 #x1ff81ff8 #x1ff80000 #x00001ff8 #x1ff81ff8 #x00000000 #x00000000   ; ICON_BURGER_MENU
               #x00000000 #x00000000 #x00880070 #x0c880088 #x1e8810f8 #x3e881288 #x00000000 #x00000000   ; ICON_CASE_SENSITIVE
               #x00000000 #x02000000 #x07000a80 #x07001fc0 #x02000a80 #x00300030 #x00000000 #x00000000   ; ICON_REG_EXP
               #x00000000 #x0042007e #x40027fc2 #x40024002 #x40024002 #x40024002 #x7ffe4002 #x00000000   ; ICON_FOLDER
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x20042004 #x20042004 #x20042004 #x00003ffc   ; ICON_FILE
               #x1ff00000 #x20082008 #x17d02fe8 #x05400ba0 #x09200540 #x23881010 #x2fe827c8 #x00001ff0   ; ICON_SAND_TIMER
               #x01800000 #x02400240 #x05a00420 #x09900990 #x11881188 #x21842004 #x40024182 #x00003ffc   ; ICON_WARNING
               #x7ffe0000 #x4ff24002 #x4c324ff2 #x4f824c02 #x41824f82 #x41824002 #x40024182 #x00007ffe   ; ICON_HELP_BOX
               #x7ffe0000 #x41824002 #x40024182 #x41824182 #x41824182 #x41824182 #x40024182 #x00007ffe   ; ICON_INFO_BOX
               #x01800000 #x04200240 #x10080810 #x7bde2004 #x0a500a50 #x08500bd0 #x08100850 #x00000ff0   ; ICON_PRIORITY
               #x01800000 #x18180660 #x80016006 #x98196006 #x99996666 #x19986666 #x01800660 #x00000000   ; ICON_LAYERS_ISO
               #x07fe0000 #x1c020402 #x74021402 #x54025402 #x54025402 #x500857fe #x40205ff8 #x00007fe0   ; ICON_LAYERS2
               #x0ffe0000 #x3ffa0802 #x7fea200a #x402a402a #x422a422a #x422e422a #x40384e28 #x00007fe0   ; ICON_MLAYERS
               #x0ffe0000 #x3ffa0802 #x7fea200a #x402a402a #x5b2a512a #x512e552a #x40385128 #x00007fe0   ; ICON_MAPS
               #x04200000 #x1cf00c60 #x11f019f0 #x0f3807b8 #x1e3c0f3c #x1c1c1e1c #x1e3c1c1c #x00000f70   ; ICON_HOT
               #x00000000 #x20803f00 #x2a202e40 #x20082e10 #x08021004 #x02040402 #x00900108 #x00000060   ; ICON_LABEL
               #x00000000 #x042007e0 #x47e27c3e #x4ffa4002 #x47fa4002 #x4ffa4002 #x7ffe4002 #x00000000   ; ICON_NAME_ID
               #x7fe00000 #x402e4020 #x43ce5e0a #x40504078 #x438e4078 #x402e5e0a #x7fe04020 #x00000000   ; ICON_SLICING
               #x00000000 #x40027ffe #x47c24002 #x55425d42 #x55725542 #x50125552 #x10105016 #x00001ff0   ; ICON_MANUAL_CONTROL
               #x7ffe0000 #x43c24002 #x48124422 #x500a500a #x500a500a #x44224812 #x400243c2 #x00007ffe   ; ICON_COLLISION
               #x03c00000 #x10080c30 #x21842184 #x4ff24182 #x41824ff2 #x21842184 #x0c301008 #x000003c0   ; ICON_CIRCLE_ADD
               #x03c00000 #x1ff80ff0 #x3e7c3e7c #x700e7e7e #x7e7e700e #x3e7c3e7c #x0ff01ff8 #x000003c0   ; ICON_CIRCLE_ADD_FILL
               #x03c00000 #x10080c30 #x21842184 #x41824182 #x40024182 #x21842184 #x0c301008 #x000003c0   ; ICON_CIRCLE_WARNING
               #x03c00000 #x1ff80ff0 #x3e7c3e7c #x7e7e7e7e #x7ffe7e7e #x3e7c3e7c #x0ff01ff8 #x000003c0   ; ICON_CIRCLE_WARNING_FILL
               #x00000000 #x10041ffc #x10841004 #x13e41084 #x10841084 #x10041004 #x00001ffc #x00000000   ; ICON_BOX_MORE
               #x00000000 #x1ffc1ffc #x1f7c1ffc #x1c1c1f7c #x1f7c1f7c #x1ffc1ffc #x00001ffc #x00000000   ; ICON_BOX_MORE_FILL
               #x00000000 #x1ffc1ffc #x1ffc1ffc #x1c1c1ffc #x1ffc1ffc #x1ffc1ffc #x00001ffc #x00000000   ; ICON_BOX_MINUS
               #x00000000 #x10041ffc #x10041004 #x13e41004 #x10041004 #x10041004 #x00001ffc #x00000000   ; ICON_BOX_MINUS_FILL
               #x07fe0000 #x055606aa #x7ff606aa #x55766eba #x55766eaa #x55606ffe #x55606aa0 #x00007fe0   ; ICON_UNION
               #x07fe0000 #x04020402 #x7fe20402 #x456246a2 #x456246a2 #x402047fe #x40204020 #x00007fe0   ; ICON_INTERSECTION
               #x07fe0000 #x055606aa #x7ff606aa #x4436442a #x4436442a #x402047fe #x40204020 #x00007fe0   ; ICON_DIFFERENCE
               #x03c00000 #x10080c30 #x20042004 #x60064002 #x47e2581a #x20042004 #x0c301008 #x000003c0   ; ICON_SPHERE
               #x03e00000 #x08080410 #x0c180808 #x08080be8 #x08080808 #x08080808 #x04100808 #x000003e0   ; ICON_CYLINDER
               #x00800000 #x01400140 #x02200220 #x04100410 #x08080808 #x1c1c13e4 #x08081004 #x000007f0   ; ICON_CONE
               #x00000000 #x07e00000 #x20841918 #x40824082 #x40824082 #x19182084 #x000007e0 #x00000000   ; ICON_ELLIPSOID
               #x00000000 #x00000000 #x20041ff8 #x40024002 #x40024002 #x1ff82004 #x00000000 #x00000000   ; ICON_CAPSULE
               #x3ff00000 #x201c2010 #x21042004 #x22842384 #x264426c4 #x2c242fe4 #x20043e74 #x00003ffc   ; ICON_FILETYPE_FONT
               #x3ff00000 #x201c2010 #x20042004 #x27742004 #x29742944 #x27742944 #x20042004 #x00003ffc   ; ICON_FILETYPE_3D
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x24242244 #x24242814 #x20042244 #x00003ffc   ; ICON_FILETYPE_XML
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x21042f04 #x21042104 #x20042f04 #x00003ffc   ; ICON_FILETYPE_C
               #x3ff00000 #x201c2010 #x23842004 #x2bf42a04 #x2fd42814 #x21c42054 #x20042004 #x00003ffc   ; ICON_FILETYPE_PYTHON
               #x3ff00000 #x201c2010 #x20042004 #x20042004 #x22842ee4 #x28a42e84 #x20042e64 #x00003ffc   ; ICON_FILETYPE_JS
               #x3ff00000 #x241c2010 #x2a8c3104 #x28242454 #x2ed42004 #x2a542a54 #x20042ed4 #x00003ffc   ; ICON_FILETYPE_ICON
               #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000   ; ICON_257
               ))
    data))

;; NOTE: A pointer to current icons array should be defined
(defparameter *gui-icons-ptr* *gui-icons*)

;; WARNING: Those values define the total size of the style data array,
;; if changed, previous saved styles could become incompatible
(defconstant +raygui-max-controls+ 16)          ; Maximum number of controls
(defconstant +raygui-max-props-base+ 16)        ; Maximum number of base properties
(defconstant +raygui-max-props-extended+ 8)     ; Maximum number of extended properties

;;----------------------------------------------------------------------------------
;; Module Types and Structures Definition
;;----------------------------------------------------------------------------------
;; Gui control property style color element
(defconstant +border+ 0)
(defconstant +base+ 1)
(defconstant +text+ 2)
(defconstant +other+ 3)

;; Defines from function bodies (raygui.h #define inside functions)
(defconstant +raygui-windowbox-statusbar-height+ 24)
(defconstant +raygui-windowbox-closebutton-height+ 18)
(defconstant +raygui-groupbox-line-thick+ 1)
(defconstant +raygui-line-margin-text+ 12)
(defconstant +raygui-line-text-padding+ 4)
(defconstant +raygui-panel-border-width+ 1)
(defconstant +raygui-min-scrollbar-width+ 40)
(defconstant +raygui-min-scrollbar-height+ 40)
(defconstant +raygui-min-mouse-wheel-speed+ 20)
(defconstant +raygui-togglegroup-max-item-text-size+ 256)
(defconstant +raygui-textbox-auto-cursor-cooldown+ 20) ; Frames to wait for autocursor movement
(defconstant +raygui-textbox-auto-cursor-delay+ 1)     ; Frames delay for autocursor movement
(defconstant +raygui-valuebox-max-chars+ 32)
(defconstant +raygui-colorbaralpha-checked-size+ 10)
(defconstant +raygui-messagebox-button-height+ 24)
(defconstant +raygui-messagebox-button-padding+ 12)
(defconstant +raygui-textinputbox-button-height+ 24)
(defconstant +raygui-textinputbox-button-padding+ 12)
(defconstant +raygui-textinputbox-height+ 26)
(defconstant +raygui-grid-alpha+ 0.15)
(defconstant +max-line-buffer-size+ 256)
(defconstant +raygui-icon-text-padding+ 4)
(defconstant +raygui-max-text-lines+ 128)
(defconstant +raygui-textsplit-max-items+ 128)
(defconstant +raygui-textsplit-max-text-size+ 1024)   ; WARNING: Max expected size for all concat items

;;----------------------------------------------------------------------------------
;; Global Variables Definition
;;----------------------------------------------------------------------------------
(defvar *gui-state* +state-normal+)             ; Gui global state, if !STATE_NORMAL, forces defined state

(defvar *gui-font* nil)                         ; Gui current font (WARNING: highly coupled to raylib)
(defvar *gui-font-name* "")                     ; Gui font filename, can be loaded from .rgs (Version: >=600)
(defvar *gui-locked* nil)                       ; Gui lock state (no inputs processed)
(defvar *gui-alpha* 1.0)                        ; Gui controls transparency

(defvar *gui-icon-scale* 1)                     ; Gui icon default scale (if icons enabled)
(defvar *gui-icon-font-offset-y* 0)             ; Gui icon font atlas offset (if icons backed)

(defvar *gui-tooltip* nil)                      ; Tooltip enabled/disabled
(defvar *gui-tooltip-ptr* nil)                  ; Tooltip string pointer (string provided by user)

(defvar *gui-control-exclusive-mode* nil)       ; Gui control exclusive mode (no inputs processed except current control)
(defvar *gui-control-exclusive-rec* (make-rectangle)) ; Gui control exclusive bounds rectangle, used as an unique identifier

(defvar *text-box-cursor-index* 0)              ; Cursor index, shared by all GuiTextBox*()
(defvar *auto-cursor-counter* 0)                ; Frame counter for automatic repeated cursor movement on key-down (cooldown and delay)

;;----------------------------------------------------------------------------------
;; Style data array for all gui style properties (allocated on data segment by default)
;;
;; NOTE 1: First set of BASE properties are generic to all controls but could be individually
;; overwritten per control, first set of EXTENDED properties are generic to all controls and
;; can not be overwritten individually but custom EXTENDED properties can be used by control
;;
;; NOTE 2: A new style set could be loaded over this array using GuiLoadStyle(),
;; but default gui style could always be recovered with GuiLoadStyleDefault()
;;
;; guiStyle size is by default: 16*(16 + 8) = 384*4 = 1536 bytes = 1.5 KB
;;----------------------------------------------------------------------------------
(defvar *gui-style* (make-array (* +raygui-max-controls+ (+ +raygui-max-props-base+ +raygui-max-props-extended+))
                                :element-type '(unsigned-byte 32) :initial-element 0))

(defvar *gui-style-loaded* nil)                 ; Style loaded flag for lazy style initialization

;;----------------------------------------------------------------------------------
;; C helpers: rectangles passed by value, NUL-terminated UTF-8 strings
;;----------------------------------------------------------------------------------
(declaim (inline %rec))
(defun %rec (x y width height)
  "Rectangle literal, (Rectangle){ x, y, width, height }"
  (make-rectangle :x (float x 1.0) :y (float y 1.0) :width (float width 1.0) :height (float height 1.0)))

(defun %rec-copy (rec)
  "Rectangle struct copy (C pass by value)"
  (%rec (rectangle-x rec) (rectangle-y rec) (rectangle-width rec) (rectangle-height rec)))

(defun %cstr (text)
  "NUL-terminated UTF-8 octet vector for TEXT (NIL stays NIL, like a NULL pointer)"
  (etypecase text
    (null nil)
    (string (let* ((octets (babel:string-to-octets text :encoding :utf-8))
                   (buffer (make-array (1+ (length octets)) :element-type '(unsigned-byte 8) :initial-element 0)))
              (replace buffer octets)))
    ((array (unsigned-byte 8) (*))
     (if (and (> (length text) 0) (zerop (aref text (1- (length text)))))
         text
         (let ((buffer (make-array (1+ (length text)) :element-type '(unsigned-byte 8) :initial-element 0)))
           (replace buffer text))))))

(declaim (inline %cref))
(defun %cref (buffer index)
  "text[index], reading past the buffer end returns '\\0'"
  (if (and (>= index 0) (< index (length buffer))) (aref buffer index) 0))

(defun %strlen (buffer &optional (start 0))
  (loop for i from start
        until (zerop (%cref buffer i))
        count t))

(defun %lisp-string (buffer &optional (start 0))
  "Lisp string from the NUL-terminated UTF-8 text at BUFFER + START"
  (babel:octets-to-string buffer :start start :end (+ start (%strlen buffer start))
                                 :encoding :utf-8 :errorp nil))

(defun %c-isspace (c) (or (= c 32) (<= 9 c 13)))
(defun %c-ispunct (c) (or (<= 33 c 47) (<= 58 c 64) (<= 91 c 96) (<= 123 c 126)))

;; GetCodepointNext() from raylib rtext.c, working on UTF-8 octets
(defun %get-codepoint-next (buffer pos)
  "Returns (values codepoint codepoint-size)"
  (let ((codepoint #x3f)
        (size 1)
        (b0 (%cref buffer pos)) (b1 (%cref buffer (+ pos 1)))
        (b2 (%cref buffer (+ pos 2))) (b3 (%cref buffer (+ pos 3))))
    (flet ((tail-p (b) (= (logand b #xc0) #x80)))
      (cond ((= #xf0 (logand #xf8 b0))
             ;; 4 byte UTF-8 codepoint
             (when (and (tail-p b1) (tail-p b2) (tail-p b3))
               (setf codepoint (logior (ash (logand #x07 b0) 18) (ash (logand #x3f b1) 12) (ash (logand #x3f b2) 6) (logand #x3f b3))
                     size 4)))
            ((= #xe0 (logand #xf0 b0))
             ;; 3 byte UTF-8 codepoint
             (when (and (tail-p b1) (tail-p b2))
               (setf codepoint (logior (ash (logand #x0f b0) 12) (ash (logand #x3f b1) 6) (logand #x3f b2))
                     size 3)))
            ((= #xc0 (logand #xe0 b0))
             ;; 2 byte UTF-8 codepoint
             (when (tail-p b1)
               (setf codepoint (logior (ash (logand #x1f b0) 6) (logand #x3f b1))
                     size 2)))
            ((= 0 (logand #x80 b0))
             ;; 1 byte UTF-8 codepoint
             (setf codepoint b0
                   size 1))))
    (values codepoint size)))

;; GetCodepointPrevious() from raylib rtext.c, working on UTF-8 octets
(defun %get-codepoint-previous (buffer pos)
  "Returns (values codepoint codepoint-size)"
  (let ((ptr pos))
    ;; Move to previous codepoint
    (loop do (decf ptr)
          while (and (/= (logand #x80 (%cref buffer ptr)) 0) (= (logand #xc0 (%cref buffer ptr)) #x80)))
    (multiple-value-bind (codepoint cp-size) (%get-codepoint-next buffer ptr)
      (values codepoint (if (/= codepoint 0) cp-size 1)))))

;; CodepointToUTF8() from raylib rtext.c
(defun %codepoint-to-utf8 (codepoint)
  "Returns (values octets size), octets always holds 6 bytes"
  (let ((utf8 (make-array 6 :element-type '(unsigned-byte 8) :initial-element 0))
        (size 0))
    (cond ((<= codepoint #x7f)
           (setf (aref utf8 0) (ldb (byte 8 0) codepoint)
                 size 1))
          ((<= codepoint #x7ff)
           (setf (aref utf8 0) (logior (logand (ash codepoint -6) #x1f) #xc0)
                 (aref utf8 1) (logior (logand codepoint #x3f) #x80)
                 size 2))
          ((<= codepoint #xffff)
           (setf (aref utf8 0) (logior (logand (ash codepoint -12) #x0f) #xe0)
                 (aref utf8 1) (logior (logand (ash codepoint -6) #x3f) #x80)
                 (aref utf8 2) (logior (logand codepoint #x3f) #x80)
                 size 3))
          ((<= codepoint #x10ffff)
           (setf (aref utf8 0) (logior (logand (ash codepoint -18) #x07) #xf0)
                 (aref utf8 1) (logior (logand (ash codepoint -12) #x3f) #x80)
                 (aref utf8 2) (logior (logand (ash codepoint -6) #x3f) #x80)
                 (aref utf8 3) (logior (logand codepoint #x3f) #x80)
                 size 4)))
    (values utf8 size)))

(defun %font-texture-id (font)
  (if font (texture-id (font-texture font)) 0))

(defun %u8 (x) (ldb (byte 8 0) (truncate x)))

;;----------------------------------------------------------------------------------
;; Gui Setup Functions Definition
;;----------------------------------------------------------------------------------
;; Enable gui global state
;; NOTE: Checking for STATE_DISABLED to avoid messing custom global state setups
(defun gui-enable () (when (= *gui-state* +state-disabled+) (setf *gui-state* +state-normal+)) nil)

;; Disable gui global state
;; NOTE: Checking for STATE_NORMAL to avoid messing custom global state setups
(defun gui-disable () (when (= *gui-state* +state-normal+) (setf *gui-state* +state-disabled+)) nil)

;; Lock gui global state
(defun gui-lock () (setf *gui-locked* t) nil)

;; Unlock gui global state
(defun gui-unlock () (setf *gui-locked* nil) nil)

;; Check if gui is locked (global state)
(defun gui-is-locked () *gui-locked*)

;; Set gui controls alpha global state
(defun gui-set-alpha (alpha)
  (let ((alpha (float alpha 1.0)))
    (cond ((< alpha 0.0) (setf alpha 0.0))
          ((> alpha 1.0) (setf alpha 1.0)))
    (setf *gui-alpha* alpha))
  nil)

;; Set gui state (global state)
(defun gui-set-state (state) (setf *gui-state* state) nil)

;; Get gui state (global state)
(defun gui-get-state () *gui-state*)

;; Set custom gui font
;; NOTE: Font loading/unloading is external to raygui
(defun gui-set-font (font)
  (when (> (%font-texture-id font) 0)
    ;; NOTE: If a font is tried to be set but default style has not been lazily loaded first,
    ;; it will be overwritten, so default style loading needs to be forced first
    (unless *gui-style-loaded* (gui-load-style-default))

    (setf *gui-font* font))
  nil)

;; Get custom gui font
(defun gui-get-font ()
  *gui-font*)

;; Set control style property value
(defun gui-set-style (control property value)
  (unless *gui-style-loaded* (gui-load-style-default))
  (let ((value (ldb (byte 32 0) value)))
    (setf (aref *gui-style* (+ (* control (+ +raygui-max-props-base+ +raygui-max-props-extended+)) property)) value)

    ;; Default properties are propagated to all controls
    (when (and (= control 0) (< property +raygui-max-props-base+))
      (loop for i from 1 below +raygui-max-controls+
            do (setf (aref *gui-style* (+ (* i (+ +raygui-max-props-base+ +raygui-max-props-extended+)) property)) value))))
  nil)

;; Get control style property value
(defun gui-get-style (control property)
  (unless *gui-style-loaded* (gui-load-style-default))
  (let ((value (aref *gui-style* (+ (* control (+ +raygui-max-props-base+ +raygui-max-props-extended+)) property))))
    (if (>= value #x80000000) (- value #x100000000) value))) ; unsigned int -> int

;; Short alias used through the module: GetColor(GuiGetStyle(control, property))
(declaim (inline %style-color))
(defun %style-color (control property)
  (get-color (gui-get-style control property)))

;; RAYGUI_FONT_ICONS_BAKING: bake icons into the font atlas loaded by GuiLoadStyle() (disabled by default)
(defvar *raygui-font-icons-baking* nil)

;; roundf(): round half away from zero
(defun %roundf (x)
  (let ((r (ftruncate x)))
    (cond ((>= (- x r) 0.5) (+ r 1.0))
          ((<= (- x r) -0.5) (- r 1.0))
          (t r))))

;; Short names for the most used calls: GuiGetStyle(), GetColor(GuiGetStyle())
(declaim (inline gs gcol))
(defun gs (control property) (gui-get-style control property))
(defun gcol (control property) (get-color (gui-get-style control property)))

;;----------------------------------------------------------------------------------
;; Gui Controls Functions Definition
;;----------------------------------------------------------------------------------

;; Window Box control
(defun gui-window-box (bounds title)
  ;; Window title bar height (including borders)
  ;; NOTE: This define is also used by GuiMessageBox() and GuiTextInputBox()
  (let* ((result +result-none+)
         ;;(state *gui-state*)
         (bounds (%rec-copy bounds))
         (status-bar-height +raygui-windowbox-statusbar-height+)
         (status-border-width (gs +statusbar+ +border-width+))

         (status-bar (%rec (rectangle-x bounds) (rectangle-y bounds) (rectangle-width bounds) (float status-bar-height))))
    (when (< (rectangle-height bounds) (* status-bar-height 2.0)) (setf (rectangle-height bounds) (* status-bar-height 2.0)))

    (let* ((v-padding (- (/ status-bar-height 2.0) (truncate +raygui-windowbox-closebutton-height+ 2)))
           (window-panel (%rec (rectangle-x bounds) (- (+ (rectangle-y bounds) (float status-bar-height)) (float status-border-width))
                               (rectangle-width bounds) (+ (- (rectangle-height bounds) (float status-bar-height)) (float status-border-width))))
           (close-button-rec (%rec (- (+ (rectangle-x status-bar) (rectangle-width status-bar)) (float status-border-width)
                                      +raygui-windowbox-closebutton-height+ v-padding)
                                   (+ (rectangle-y status-bar) v-padding)
                                   +raygui-windowbox-closebutton-height+ +raygui-windowbox-closebutton-height+)))

      ;; Update control
      ;;--------------------------------------------------------------------
      ;; NOTE: Logic is directly managed by button
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      (gui-panel window-panel nil)      ; Draw window base

      (let ((temp-text-alignment (gs +statusbar+ +text-alignment+)))
        (gui-set-style +statusbar+ +text-alignment+ +text-align-left+)
        (gui-status-bar status-bar title) ; Draw window header as status bar
        (gui-set-style +statusbar+ +text-alignment+ temp-text-alignment))

      ;; Draw window close button
      (let ((temp-border-width (gs +button+ +border-width+))
            (temp-text-alignment (gs +button+ +text-alignment+)))
        (gui-set-style +button+ +border-width+ 1)
        (gui-set-style +button+ +text-alignment+ +text-align-center+)
        (setf result (gui-button close-button-rec (gui-icon-text +icon-cross-small+ nil)))
        (gui-set-style +button+ +border-width+ temp-border-width)
        (gui-set-style +button+ +text-alignment+ temp-text-alignment)))
    ;;--------------------------------------------------------------------

    result))

;; Group Box control with text name
(defun gui-group-box (bounds text)
  (let ((result +result-none+)
        (state *gui-state*)
        (x (rectangle-x bounds)) (y (rectangle-y bounds)) (w (rectangle-width bounds)) (h (rectangle-height bounds)))

    ;; Draw control
    ;;--------------------------------------------------------------------
    (flet ((line-color () (gcol +default+ (if (= state +state-disabled+) +border-color-disabled+ +line-color+))))
      (%gui-draw-rectangle (%rec x y +raygui-groupbox-line-thick+ h) 0 +blank+ (line-color))
      (%gui-draw-rectangle (%rec x (- (+ y h) 1) w +raygui-groupbox-line-thick+) 0 +blank+ (line-color))
      (%gui-draw-rectangle (%rec (- (+ x w) 1) y +raygui-groupbox-line-thick+ h) 0 +blank+ (line-color)))

    (gui-line (%rec x (- y (truncate (gs +default+ +text-size+) 2)) w (float (gs +default+ +text-size+))) text)
    ;;--------------------------------------------------------------------

    result))

;; Line control
(defun gui-line (bounds text)
  (let* ((result +result-none+)
         (state *gui-state*)
         (color (gcol +default+ (if (= state +state-disabled+) +border-color-disabled+ +line-color+)))
         (text (%cstr text))
         (x (rectangle-x bounds)) (y (rectangle-y bounds)) (w (rectangle-width bounds)) (h (rectangle-height bounds)))

    ;; Draw control
    ;;--------------------------------------------------------------------
    (if (null text)
        (%gui-draw-rectangle (%rec x (+ y (/ h 2)) w 1) 0 +blank+ color)
        (let ((text-bounds (make-rectangle)))
          (setf (rectangle-width text-bounds) (+ (float (gui-get-text-width text)) 2)
                (rectangle-height text-bounds) h
                (rectangle-x text-bounds) (+ x +raygui-line-margin-text+)
                (rectangle-y text-bounds) y)

          ;; Draw line with embedded text label: "--- text --------------"
          (%gui-draw-rectangle (%rec x (+ y (/ h 2)) (- +raygui-line-margin-text+ +raygui-line-text-padding+) 1) 0 +blank+ color)
          (%gui-draw-text text 0 text-bounds +text-align-left+ color)
          (%gui-draw-rectangle (%rec (+ x 12 (rectangle-width text-bounds) 4) (+ y (/ h 2))
                                     (- w (rectangle-width text-bounds) +raygui-line-margin-text+ +raygui-line-text-padding+) 1)
                               0 +blank+ color)))
    ;;--------------------------------------------------------------------

    result))

;; Panel control
(defun gui-panel (bounds text)
  (let ((result +result-none+)
        (state *gui-state*)
        (bounds (%rec-copy bounds))
        (text (%cstr text)))

    ;; Text will be drawn as a header bar (if provided)
    (let ((status-bar (%rec (rectangle-x bounds) (rectangle-y bounds) (rectangle-width bounds) (float +raygui-windowbox-statusbar-height+))))
      (when (and text (< (rectangle-height bounds) (* +raygui-windowbox-statusbar-height+ 2.0)))
        (setf (rectangle-height bounds) (* +raygui-windowbox-statusbar-height+ 2.0)))

      (when text
        ;; Move panel bounds after the header bar
        (incf (rectangle-y bounds) (- (float +raygui-windowbox-statusbar-height+) 1))
        (decf (rectangle-height bounds) (- (float +raygui-windowbox-statusbar-height+) 1)))

      ;; Draw control
      ;;--------------------------------------------------------------------
      (when text (setf result (gui-status-bar status-bar text))) ; Draw panel header as status bar

      (%gui-draw-rectangle bounds +raygui-panel-border-width+
                           (gcol +default+ (if (= state +state-disabled+) +border-color-disabled+ +line-color+))
                           (gcol +default+ (if (= state +state-disabled+) +base-color-disabled+ +background-color+))))
    ;;--------------------------------------------------------------------

    result))

;; Scroll Panel control
;; Returns (values result scroll view)
(defun gui-scroll-panel (bounds text content scroll &optional view)
  (declare (ignore view))
  (let* ((result +result-none+)
         (state *gui-state*)
         (bounds (%rec-copy bounds))
         (text (%cstr text))
         (view nil)
         (scroll-pos (if scroll (vec2 (vx scroll) (vy scroll)) (vec2 0.0 0.0)))

         ;; Text will be drawn as a header bar (if provided)
         (status-bar (%rec (rectangle-x bounds) (rectangle-y bounds) (rectangle-width bounds) (float +raygui-windowbox-statusbar-height+))))

    (when (< (rectangle-height bounds) (* +raygui-windowbox-statusbar-height+ 2.0))
      (setf (rectangle-height bounds) (* +raygui-windowbox-statusbar-height+ 2.0)))

    (when text
      ;; Move panel bounds after the header bar
      (incf (rectangle-y bounds) (- (float +raygui-windowbox-statusbar-height+) 1))
      (decf (rectangle-height bounds) (+ (float +raygui-windowbox-statusbar-height+) 1)))

    (let* ((bw (gs +default+ +border-width+))
           (has-horizontal-scroll-bar (> (rectangle-width content) (- (rectangle-width bounds) (* 2 bw))))
           (has-vertical-scroll-bar (> (rectangle-height content) (- (rectangle-height bounds) (* 2 bw)))))

      ;; Recheck to account for the other scrollbar being visible
      (unless has-horizontal-scroll-bar
        (setf has-horizontal-scroll-bar (and has-vertical-scroll-bar (> (rectangle-width content) (- (rectangle-width bounds) (* 2 bw) (gs +listview+ +scrollbar-width+))))))
      (unless has-vertical-scroll-bar
        (setf has-vertical-scroll-bar (and has-horizontal-scroll-bar (> (rectangle-height content) (- (rectangle-height bounds) (* 2 bw) (gs +listview+ +scrollbar-width+))))))

      (let* ((horizontal-scroll-bar-width (if has-horizontal-scroll-bar (gs +listview+ +scrollbar-width+) 0))
             (vertical-scroll-bar-width (if has-vertical-scroll-bar (gs +listview+ +scrollbar-width+) 0))
             (left-side (= (gs +listview+ +scrollbar-side+) +scrollbar-left-side+))
             (horizontal-scroll-bar
               (%rec (+ (if left-side (+ (rectangle-x bounds) vertical-scroll-bar-width) (rectangle-x bounds)) bw)
                     (- (+ (rectangle-y bounds) (rectangle-height bounds)) horizontal-scroll-bar-width bw)
                     (- (rectangle-width bounds) vertical-scroll-bar-width (* 2 bw))
                     (float horizontal-scroll-bar-width)))
             (vertical-scroll-bar
               (%rec (if left-side
                         (+ (rectangle-x bounds) bw)
                         (- (+ (rectangle-x bounds) (rectangle-width bounds)) vertical-scroll-bar-width bw))
                     (+ (rectangle-y bounds) bw)
                     (float vertical-scroll-bar-width)
                     (- (rectangle-height bounds) horizontal-scroll-bar-width (* 2 bw)))))

        ;; Make sure scroll bars have a minimum width/height
        (when (< (rectangle-width horizontal-scroll-bar) +raygui-min-scrollbar-width+)
          (setf (rectangle-width horizontal-scroll-bar) (float +raygui-min-scrollbar-width+)))
        (when (< (rectangle-height vertical-scroll-bar) +raygui-min-scrollbar-height+)
          (setf (rectangle-height vertical-scroll-bar) (float +raygui-min-scrollbar-height+)))

        ;; Calculate view area (area without the scrollbars)
        (setf view (if left-side
                       (%rec (+ (rectangle-x bounds) vertical-scroll-bar-width bw) (+ (rectangle-y bounds) bw)
                             (- (rectangle-width bounds) (* 2 bw) vertical-scroll-bar-width)
                             (- (rectangle-height bounds) (* 2 bw) horizontal-scroll-bar-width))
                       (%rec (+ (rectangle-x bounds) bw) (+ (rectangle-y bounds) bw)
                             (- (rectangle-width bounds) (* 2 bw) vertical-scroll-bar-width)
                             (- (rectangle-height bounds) (* 2 bw) horizontal-scroll-bar-width))))

        ;; Clip view area to the actual content size
        (when (> (rectangle-width view) (rectangle-width content)) (setf (rectangle-width view) (rectangle-width content)))
        (when (> (rectangle-height view) (rectangle-height content)) (setf (rectangle-height view) (rectangle-height content)))

        (let ((horizontal-min (- (if left-side (float (- vertical-scroll-bar-width)) 0.0) (float bw)))
              (horizontal-max (if has-horizontal-scroll-bar
                                  (- (+ (- (rectangle-width content) (rectangle-width bounds)) (float vertical-scroll-bar-width) bw)
                                     (if left-side (float vertical-scroll-bar-width) 0.0))
                                  (float (- bw))))
              (vertical-min (- (float bw)))
              (vertical-max (if has-vertical-scroll-bar
                                (+ (- (rectangle-height content) (rectangle-height bounds)) (float horizontal-scroll-bar-width) (float bw))
                                (float (- bw)))))

          ;; Update control
          ;;--------------------------------------------------------------------
          (when (and (/= state +state-disabled+) (not *gui-locked*))
            (let ((mouse-point (gui-pointer-position)))

              ;; Check button state
              (when (check-collision-point-rec mouse-point bounds)
                (if (gui-button-down-p)
                    (setf state +state-pressed+)
                    (setf state +state-focused+))

                (let ((scroll-delta (gui-scroll-delta))
                      ;; Set scrolling speed with mouse wheel based on ratio between bounds and content
                      (scroll-speed (vec2 (/ (rectangle-width content) (rectangle-width bounds))
                                          (/ (rectangle-height content) (rectangle-height bounds)))))
                  (when (< (vx scroll-speed) +raygui-min-mouse-wheel-speed+) (setf (vx scroll-speed) (float +raygui-min-mouse-wheel-speed+)))
                  (when (< (vy scroll-speed) +raygui-min-mouse-wheel-speed+) (setf (vy scroll-speed) (float +raygui-min-mouse-wheel-speed+)))

                  ;; Horizontal and vertical scrolling with mouse wheel
                  (if (and has-horizontal-scroll-bar (or (gui-key-down-p +key-left-control+) (gui-key-down-p +key-left-shift+)))
                      (incf (vx scroll-pos) (* scroll-delta (vx scroll-speed)))
                      (incf (vy scroll-pos) (* scroll-delta (vy scroll-speed)))))))) ; Vertical scroll

          ;; Normalize scroll values
          (when (> (vx scroll-pos) (- horizontal-min)) (setf (vx scroll-pos) (- horizontal-min)))
          (when (< (vx scroll-pos) (- horizontal-max)) (setf (vx scroll-pos) (- horizontal-max)))
          (when (> (vy scroll-pos) (- vertical-min)) (setf (vy scroll-pos) (- vertical-min)))
          (when (< (vy scroll-pos) (- vertical-max)) (setf (vy scroll-pos) (- vertical-max)))
          ;;--------------------------------------------------------------------

          ;; Draw control
          ;;--------------------------------------------------------------------
          (when text (setf result (gui-status-bar status-bar text))) ; Draw panel header as status bar

          (%gui-draw-rectangle bounds 0 +blank+ (gcol +default+ +background-color+)) ; Draw background

          ;; Save size of the scrollbar slider
          (let ((slider (gs +scrollbar+ +scroll-slider-size+)))

            ;; Draw horizontal scrollbar if visible
            (if has-horizontal-scroll-bar
                (progn
                  ;; Change scrollbar slider size to show the diff in size between the content width and the widget width
                  (gui-set-style +scrollbar+ +scroll-slider-size+
                                 (truncate (* (/ (- (rectangle-width bounds) (* 2 bw) vertical-scroll-bar-width) (truncate (rectangle-width content)))
                                              (- (truncate (rectangle-width bounds)) (* 2 bw) vertical-scroll-bar-width))))
                  (setf (vx scroll-pos) (float (- (%gui-scroll-bar horizontal-scroll-bar (truncate (- (vx scroll-pos)))
                                                                   (truncate horizontal-min) (truncate horizontal-max))))))
                (setf (vx scroll-pos) 0.0))

            ;; Draw vertical scrollbar if visible
            (if has-vertical-scroll-bar
                (progn
                  ;; Change scrollbar slider size to show the diff in size between the content height and the widget height
                  (gui-set-style +scrollbar+ +scroll-slider-size+
                                 (truncate (* (/ (- (rectangle-height bounds) (* 2 bw) horizontal-scroll-bar-width) (truncate (rectangle-height content)))
                                              (- (truncate (rectangle-height bounds)) (* 2 bw) horizontal-scroll-bar-width))))
                  (setf (vy scroll-pos) (float (- (%gui-scroll-bar vertical-scroll-bar (truncate (- (vy scroll-pos)))
                                                                   (truncate vertical-min) (truncate vertical-max))))))
                (setf (vy scroll-pos) 0.0))

            ;; Draw detail corner rectangle if both scroll bars are visible
            (when (and has-horizontal-scroll-bar has-vertical-scroll-bar)
              (let ((corner (%rec (if left-side
                                      (+ (rectangle-x bounds) bw 2)
                                      (+ (rectangle-x horizontal-scroll-bar) (rectangle-width horizontal-scroll-bar) 2))
                                  (+ (rectangle-y vertical-scroll-bar) (rectangle-height vertical-scroll-bar) 2)
                                  (- (float horizontal-scroll-bar-width) 4) (- (float vertical-scroll-bar-width) 4))))
                (%gui-draw-rectangle corner 0 +blank+ (gcol +listview+ (+ +text+ (* state 3))))))

            ;; Draw scrollbar lines depending on current state
            (%gui-draw-rectangle bounds (gs +listview+ +border-width+) (gcol +listview+ (+ +border+ (* state 3))) +blank+)

            ;; Set scrollbar slider size back to the way it was before
            (gui-set-style +scrollbar+ +scroll-slider-size+ slider)))))
    ;;--------------------------------------------------------------------

    (values result scroll-pos view)))

;; Label control
(defun gui-label (bounds text)
  (let ((result +result-none+)
        (state *gui-state*))

    ;; Update control
    ;;--------------------------------------------------------------------
    ;;...
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-text (%cstr text) 0 (%get-text-bounds +label+ bounds) (gs +label+ +text-alignment+) (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    result))

;; Button control, returns true when clicked
(defun gui-button (bounds text)
  (let ((result +result-none+)
        (state *gui-state*))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check button state
        (when (check-collision-point-rec mouse-point bounds)
          (if (gui-button-down-p)
              (setf state +state-pressed+)
              (setf state +state-focused+))

          (when (gui-button-released-p) (setf result +result-pressed+)))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +button+ +border-width+) (gcol +button+ (+ +border+ (* state 3))) (gcol +button+ (+ +base+ (* state 3))))
    (%gui-draw-text (%cstr text) 0 (%get-text-bounds +button+ bounds) (gs +button+ +text-alignment+) (gcol +button+ (+ +text+ (* state 3))))

    (when (= state +state-focused+) (%gui-tooltip bounds))
    ;;------------------------------------------------------------------

    result))

;; Label button control
(defun gui-label-button (bounds text)
  (let* ((result +result-none+)
         (state *gui-state*)
         (bounds (%rec-copy bounds))
         (text (%cstr text))

         ;; NOTE: Force bounds.width to be all text
         (text-width (float (gui-get-text-width text))))
    (when (< (- (rectangle-width bounds) (* 2 (gs +label+ +border-width+)) (* 2 (gs +label+ +text-padding+))) text-width)
      (setf (rectangle-width bounds) (+ text-width (* 2 (gs +label+ +border-width+)) (* 2 (gs +label+ +text-padding+)) 2)))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check checkbox state
        (when (check-collision-point-rec mouse-point bounds)
          (if (gui-button-down-p)
              (setf state +state-pressed+)
              (setf state +state-focused+))

          (when (gui-button-released-p) (setf result +result-pressed+)))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-text text 0 (%get-text-bounds +label+ bounds) (gs +label+ +text-alignment+) (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    result))

;; Toggle Button control
;; Returns (values result active)
(defun gui-toggle (bounds text active)
  (let ((result +result-none+)
        (state *gui-state*)
        (text (%cstr text)))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check toggle button state
        (when (check-collision-point-rec mouse-point bounds)
          (cond ((gui-button-down-p) (setf state +state-pressed+))
                ((gui-button-released-p)
                 (setf state +state-normal+
                       active (not active)
                       result +result-changed+))
                (t (setf state +state-focused+))))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (if (= state +state-normal+)
        (progn
          (%gui-draw-rectangle bounds (gs +toggle+ +border-width+)
                               (gcol +toggle+ (if active +border-color-pressed+ (+ +border+ (* state 3))))
                               (gcol +toggle+ (if active +base-color-pressed+ (+ +base+ (* state 3)))))
          (%gui-draw-text text 0 (%get-text-bounds +toggle+ bounds) (gs +toggle+ +text-alignment+)
                          (gcol +toggle+ (if active +text-color-pressed+ (+ +text+ (* state 3))))))
        (progn
          (%gui-draw-rectangle bounds (gs +toggle+ +border-width+) (gcol +toggle+ (+ +border+ (* state 3))) (gcol +toggle+ (+ +base+ (* state 3))))
          (%gui-draw-text text 0 (%get-text-bounds +toggle+ bounds) (gs +toggle+ +text-alignment+) (gcol +toggle+ (+ +text+ (* state 3))))))

    (when (= state +state-focused+) (%gui-tooltip bounds))
    ;;--------------------------------------------------------------------

    (values result active)))

;; Toggle Group control
;; Returns (values result active)
(defun gui-toggle-group (bounds text active)
  (let* ((result +result-none+)
         (bounds (%rec-copy bounds))
         (text-ptr (%cstr text))
         ;; One toggle group item text
         (item-text (make-array +raygui-togglegroup-max-item-text-size+ :element-type '(unsigned-byte 8) :initial-element 0))
         (prev-active (or active 0))
         (active (or active 0))
         (toggle nil)                   ; Required for individual toggles
         (item-ready nil)
         (init-bounds-x (rectangle-x bounds))
         (init-bounds-y (rectangle-y bounds)))

    (when (/= (gs +toggle+ +group-width-full+) 0)
      ;; Calculate item width considring all horizontal items
      ;; NOTE: bounds.height still considers individual items height
      (let ((item-count 1))
        (loop for c from 0
              until (zerop (%cref text-ptr c))
              do (when (= (%cref text-ptr c) (char-code #\;)) (incf item-count)))
        (setf (rectangle-width bounds) (/ (rectangle-width bounds) item-count))))

    ;; Text parsing needed to consider potential row and col entries (vertical/horizontal layout)
    ;; when '\n' found move vertically next toggle, when ';' found move horizontally
    (loop with k = 0 and item-index = 0 and row = 0 and col = 0 and exit = 0
          for c from 0
          while (= exit 0)
          do ;; Process text to get items one by one
             ;; NOTE: Setting columns and rows index properly
             (let ((ch (%cref text-ptr c)))
               (cond ((= ch 10)
                      (incf row)
                      (setf col 0
                            item-ready t))
                     ((= ch (char-code #\;))
                      (incf col)
                      (setf item-ready t))
                     ((= ch 0)
                      (setf item-ready t
                            exit 1))
                     (t
                      (setf (aref item-text k) ch)
                      (incf k))))

             (when item-ready
               ;; When a next item is ready, draw its toggle
               (if (= item-index active)
                   (progn
                     (setf toggle t)
                     (gui-toggle bounds item-text toggle))
                   (progn
                     (setf toggle nil)
                     (setf toggle (nth-value 1 (gui-toggle bounds item-text toggle)))
                     (when toggle (setf active item-index))))

               ;; Calculate next item position
               (setf (rectangle-x bounds) (+ init-bounds-x (* col (+ (rectangle-width bounds) (gs +toggle+ +group-padding+))))
                     (rectangle-y bounds) (+ init-bounds-y (* row (+ (rectangle-height bounds) (gs +toggle+ +group-padding+)))))

               (incf item-index)
               (setf item-ready nil)

               (fill item-text 0 :end (min (1+ k) (length item-text)))
               (setf k 0)))

    (when (/= prev-active active) (setf result +result-changed+))

    (values result active)))

;; Toggle Slider control extended
;; Returns (values result active)
(defun gui-toggle-slider (bounds text active)
  (let* ((result +result-none+)
         (state *gui-state*)
         (prev-active (or active 0))
         (active (or active 0))
         (text (%cstr text))

         ;; Get substrings items from text (items pointers)
         (items (when text (%gui-text-split text (char-code #\;))))
         (item-count (length items))

         (slider (%rec 0      ; Calculated later depending on the active toggle
                       (+ (rectangle-y bounds) (gs +slider+ +border-width+) (gs +slider+ +slider-padding+))
                       (/ (- (rectangle-width bounds) (* 2 (gs +slider+ +border-width+)) (* (+ item-count 1) (gs +slider+ +slider-padding+))) item-count)
                       (- (rectangle-height bounds) (* 2 (gs +slider+ +border-width+)) (* 2 (gs +slider+ +slider-padding+))))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*))
      (let ((mouse-point (gui-pointer-position)))

        (when (check-collision-point-rec mouse-point bounds)
          (cond ((gui-button-down-p) (setf state +state-pressed+))
                ((gui-button-released-p)
                 (setf state +state-pressed+)
                 (incf active)
                 (setf result 1))
                (t (setf state +state-focused+))))

        (when (and (/= active 0) (/= state +state-focused+)) (setf state +state-pressed+))))

    (when (>= active item-count) (setf active 0))
    (setf (rectangle-x slider) (+ (rectangle-x bounds) (gs +slider+ +border-width+) (* (+ active 1) (gs +slider+ +slider-padding+)) (* active (rectangle-width slider))))

    (when (/= prev-active active) (setf result +result-changed+))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +slider+ +border-width+) (gcol +toggle+ (+ +border+ (* state 3)))
                         (gcol +toggle+ +base-color-normal+))

    ;; Draw internal slider
    (cond ((= state +state-normal+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +base-color-pressed+)))
          ((= state +state-focused+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +base-color-focused+)))
          ((= state +state-pressed+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +base-color-pressed+))))

    ;; Draw text in slider
    (when text
      (let ((text-bounds (make-rectangle)))
        (setf (rectangle-width text-bounds) (float (gui-get-text-width text))
              (rectangle-height text-bounds) (float (gs +default+ +text-size+))
              (rectangle-x text-bounds) (- (+ (rectangle-x slider) (/ (rectangle-width slider) 2)) (/ (rectangle-width text-bounds) 2))
              (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))

        (%gui-draw-text (aref items active) 0 text-bounds (gs +toggle+ +text-alignment+) (fade (gcol +toggle+ (+ +text+ (* state 3))) *gui-alpha*))))
    ;;--------------------------------------------------------------------

    (values result active)))

;; Check Box control, returns 1 when state changed
;; Returns (values result checked)
(defun gui-check-box (bounds text checked)
  (let ((result +result-none+)
        (state *gui-state*)
        (text (%cstr text))
        (text-bounds (make-rectangle)))

    (when text
      (setf (rectangle-width text-bounds) (+ (float (gui-get-text-width text)) 2)
            (rectangle-height text-bounds) (float (gs +default+ +text-size+))
            (rectangle-x text-bounds) (+ (rectangle-x bounds) (rectangle-width bounds) (gs +checkbox+ +text-padding+))
            (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))
      (when (= (gs +checkbox+ +text-alignment+) +text-align-left+)
        (setf (rectangle-x text-bounds) (- (rectangle-x bounds) (rectangle-width text-bounds) (gs +checkbox+ +text-padding+)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position))
            (total-bounds (%rec (if (= (gs +checkbox+ +text-alignment+) +text-align-left+) (rectangle-x text-bounds) (rectangle-x bounds))
                                (rectangle-y bounds)
                                (+ (rectangle-width bounds) (rectangle-width text-bounds) (gs +checkbox+ +text-padding+))
                                (rectangle-height bounds))))

        ;; Check checkbox state
        (when (check-collision-point-rec mouse-point total-bounds)
          (if (gui-button-down-p)
              (setf state +state-pressed+)
              (setf state +state-focused+))

          (when (gui-button-released-p)
            (setf checked (not checked)
                  result +result-changed+)))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +checkbox+ +border-width+) (gcol +checkbox+ (+ +border+ (* state 3))) +blank+)

    (when checked
      (let ((check (%rec (+ (rectangle-x bounds) (gs +checkbox+ +border-width+) (gs +checkbox+ +check-padding+))
                         (+ (rectangle-y bounds) (gs +checkbox+ +border-width+) (gs +checkbox+ +check-padding+))
                         (- (rectangle-width bounds) (* 2 (+ (gs +checkbox+ +border-width+) (gs +checkbox+ +check-padding+))))
                         (- (rectangle-height bounds) (* 2 (+ (gs +checkbox+ +border-width+) (gs +checkbox+ +check-padding+)))))))
        (%gui-draw-rectangle check 0 +blank+ (gcol +checkbox+ (+ +text+ (* state 3))))))

    (%gui-draw-text text 0 text-bounds (if (= (gs +checkbox+ +text-alignment+) +text-align-right+) +text-align-left+ +text-align-right+)
                    (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    (values result checked)))

;; Combo Box control
;; Returns (values result active)
(defun gui-combo-box (bounds text active)
  (let* ((result +result-none+)
         (state *gui-state*)
         (prev-active (or active 0))
         (active (or active 0))
         (bounds (%rec-copy bounds)))

    (decf (rectangle-width bounds) (+ (gs +combobox+ +combo-button-width+) (gs +combobox+ +combo-button-spacing+)))

    (let* ((selector (%rec (+ (rectangle-x bounds) (rectangle-width bounds) (gs +combobox+ +combo-button-spacing+))
                           (rectangle-y bounds) (float (gs +combobox+ +combo-button-width+)) (rectangle-height bounds)))

           ;; Get substrings items from text (items pointers, lengths and count)
           (items (%gui-text-split (%cstr text) (char-code #\;)))
           (item-count (length items)))

      (cond ((< active 0) (setf active 0))
            ((> active (1- item-count)) (setf active (1- item-count))))

      ;; Update control
      ;;--------------------------------------------------------------------
      (when (and (/= state +state-disabled+) (not *gui-locked*) (> item-count 1) (not *gui-control-exclusive-mode*))
        (let ((mouse-point (gui-pointer-position)))

          (when (or (check-collision-point-rec mouse-point bounds)
                    (check-collision-point-rec mouse-point selector))
            (if (gui-button-down-p)
                (setf state +state-pressed+)
                (setf state +state-focused+))

            (when (gui-button-pressed-p)
              (incf active 1)
              (when (>= active item-count) (setf active 0)))))) ; Cyclic combobox

      (when (/= prev-active active) (setf result +result-changed+))
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      ;; Draw combo box main
      (%gui-draw-rectangle bounds (gs +combobox+ +border-width+) (gcol +combobox+ (+ +border+ (* state 3))) (gcol +combobox+ (+ +base+ (* state 3))))
      (%gui-draw-text (aref items active) 0 (%get-text-bounds +combobox+ bounds) (gs +combobox+ +text-alignment+) (gcol +combobox+ (+ +text+ (* state 3))))

      ;; Draw selector using a custom button
      ;; NOTE: BORDER_WIDTH and TEXT_ALIGNMENT forced values
      (let ((temp-border-width (gs +button+ +border-width+))
            (temp-text-align (gs +button+ +text-alignment+)))
        (gui-set-style +button+ +border-width+ 1)
        (gui-set-style +button+ +text-alignment+ +text-align-center+)

        (gui-button selector (text-format "%i/%i" (+ active 1) item-count))

        (gui-set-style +button+ +text-alignment+ temp-text-align)
        (gui-set-style +button+ +border-width+ temp-border-width)))
    ;;--------------------------------------------------------------------

    (values result active)))

;; Dropdown Box control
;; NOTE: Returns mouse click
;; Returns (values result active)
(defun gui-dropdown-box (bounds text active edit-mode)
  (let* ((result +result-none+)
         (state *gui-state*)
         (prev-active (or active 0))
         (active (or active 0))

         (item-selected active)
         (item-focused -1)

         (direction (if (= (gs +dropdownbox+ +dropdown-roll-up+) 1) 1 0)) ; Dropdown box open direction: down (default), 1: Up

         ;; Get substrings items from text (items pointers, lengths and count)
         (items (%gui-text-split (%cstr text) (char-code #\;)))
         (item-count (length items))

         (bounds-open (%rec-copy bounds))
         (item-bounds (%rec-copy bounds)))

    (setf (rectangle-height bounds-open) (float (* (+ item-count 1) (+ (rectangle-height bounds) (gs +dropdownbox+ +dropdown-items-spacing+)))))
    (when (= direction 1)
      (decf (rectangle-y bounds-open) (+ (* item-count (+ (rectangle-height bounds) (gs +dropdownbox+ +dropdown-items-spacing+))) (gs +dropdownbox+ +dropdown-items-spacing+))))

    (flet ((next-item-bounds ()
             ;; Update item rectangle y position for next item
             (if (= direction 0)
                 (incf (rectangle-y item-bounds) (+ (rectangle-height bounds) (gs +dropdownbox+ +dropdown-items-spacing+)))
                 (decf (rectangle-y item-bounds) (+ (rectangle-height bounds) (gs +dropdownbox+ +dropdown-items-spacing+))))))

      ;; Update control
      ;;--------------------------------------------------------------------
      (when (and (/= state +state-disabled+) (or edit-mode (not *gui-locked*)) (> item-count 1) (not *gui-control-exclusive-mode*))
        (let ((mouse-point (gui-pointer-position)))

          (if edit-mode
              (progn
                (setf state +state-pressed+)

                ;; Check if mouse has been pressed or released outside limits
                (unless (check-collision-point-rec mouse-point bounds-open)
                  (when (or (gui-button-pressed-p) (gui-button-released-p)) (setf result 1)))

                ;; Check if already selected item has been pressed again
                (when (and (check-collision-point-rec mouse-point bounds) (gui-button-pressed-p)) (setf result 1))

                ;; Check focused and selected item
                (dotimes (i item-count)
                  (next-item-bounds)

                  (when (check-collision-point-rec mouse-point item-bounds)
                    (setf item-focused i)
                    (when (gui-button-released-p)
                      (setf item-selected i
                            result 1))      ; Item selected
                    (return)))

                (setf item-bounds (%rec-copy bounds)))
              (when (check-collision-point-rec mouse-point bounds)
                (if (gui-button-pressed-p)
                    (setf result 1
                          state +state-pressed+)
                    (setf state +state-focused+))))))
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      (when edit-mode (gui-panel bounds-open nil))

      (%gui-draw-rectangle bounds (gs +dropdownbox+ +border-width+) (gcol +dropdownbox+ (+ +border+ (* state 3))) (gcol +dropdownbox+ (+ +base+ (* state 3))))
      (%gui-draw-text (aref items item-selected) 0 (%get-text-bounds +dropdownbox+ bounds) (gs +dropdownbox+ +text-alignment+) (gcol +dropdownbox+ (+ +text+ (* state 3))))

      (when edit-mode
        ;; Draw visible items
        (dotimes (i item-count)
          (next-item-bounds)

          (cond ((= i item-selected)
                 (%gui-draw-rectangle item-bounds (gs +dropdownbox+ +border-width+) (gcol +dropdownbox+ +border-color-pressed+) (gcol +dropdownbox+ +base-color-pressed+))
                 (%gui-draw-text (aref items i) 0 (%get-text-bounds +dropdownbox+ item-bounds) (gs +dropdownbox+ +text-alignment+) (gcol +dropdownbox+ +text-color-pressed+)))
                ((= i item-focused)
                 (%gui-draw-rectangle item-bounds (gs +dropdownbox+ +border-width+) (gcol +dropdownbox+ +border-color-focused+) (gcol +dropdownbox+ +base-color-focused+))
                 (%gui-draw-text (aref items i) 0 (%get-text-bounds +dropdownbox+ item-bounds) (gs +dropdownbox+ +text-alignment+) (gcol +dropdownbox+ +text-color-focused+)))
                (t (%gui-draw-text (aref items i) 0 (%get-text-bounds +dropdownbox+ item-bounds) (gs +dropdownbox+ +text-alignment+) (gcol +dropdownbox+ +text-color-normal+))))))

      (when (= (gs +dropdownbox+ +dropdown-arrow-hidden+) 0)
        ;; Draw arrows (using icon if available)
        (%gui-draw-text (%cstr (if (/= direction 0) (gui-icon-text +icon-arrow-up-fill+ nil) (gui-icon-text +icon-arrow-down-fill+ nil))) 0
                        (%rec (- (+ (rectangle-x bounds) (rectangle-width bounds)) (gs +dropdownbox+ +arrow-padding+))
                              (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) 6) 10 10)
                        +text-align-center+ (gcol +dropdownbox+ (+ +text+ (* state 3)))))) ; ICON_ARROW_DOWN_FILL
    ;;--------------------------------------------------------------------

    (setf active item-selected)

    (when (/= prev-active active) (setf result +result-changed+))

    (values result active)))

;; Text Box control
;; NOTE: Returns true on ENTER pressed (useful for data validation)
;; Returns (values result text), TEXT-SIZE is the C buffer size (including '\0')
(defun gui-text-box (bounds text text-size edit-mode)
  (let* ((text (or text ""))
         (octets (babel:string-to-octets text :encoding :utf-8))
         (buffer (make-array (max text-size (1+ (length octets))) :element-type '(unsigned-byte 8) :initial-element 0)))
    (replace buffer octets)
    (let ((result (%gui-text-box bounds buffer text-size edit-mode)))
      (values result (%lisp-string buffer)))))

(defun %gui-text-box (bounds text text-size edit-mode)
  "GuiTextBox() working in place on the NUL-terminated UTF-8 buffer TEXT"
  (let* ((result +result-none+)
         (state *gui-state*)

         (multiline nil)              ; TODO: Consider multiline text input
         (wrap-mode (gs +default+ +text-wrap-mode+))

         (text-bounds (%get-text-bounds +textbox+ bounds))
         (text-length (if text (%strlen text) 0)) ; Get current text length
         (this-cursor-index *text-box-cursor-index*))
    (when (> this-cursor-index text-length) (setf this-cursor-index text-length))
    (let* ((text-width (- (%gui-get-text-width text 0) (%gui-get-text-width text this-cursor-index)))
           (text-index-offset 0)        ; Text index offset to start drawing in the box

           ;; Cursor rectangle
           ;; NOTE: Position X value should be updated
           (cursor (%rec (+ (rectangle-x text-bounds) text-width (gs +default+ +text-spacing+))
                         (- (+ (rectangle-y text-bounds) (/ (rectangle-height text-bounds) 2)) (gs +default+ +text-size+))
                         2
                         (* (float (gs +default+ +text-size+)) 2)))
           (mouse-cursor nil))

      (when (>= (rectangle-height cursor) (rectangle-height bounds))
        (setf (rectangle-height cursor) (float (- (rectangle-height bounds) (* (gs +textbox+ +border-width+) 2)))))
      (when (< (rectangle-y cursor) (+ (rectangle-y bounds) (gs +textbox+ +border-width+)))
        (setf (rectangle-y cursor) (+ (rectangle-y bounds) (gs +textbox+ +border-width+))))

      ;; Mouse cursor rectangle
      ;; NOTE: Initialized outside of screen
      (setf mouse-cursor (%rec-copy cursor))
      (setf (rectangle-x mouse-cursor) -1.0
            (rectangle-width mouse-cursor) 1.0)

      ;; Update control
      ;;--------------------------------------------------------------------
      ;; WARNING: Text editing is only supported under certain conditions:
      (when (and (/= state +state-disabled+)               ; Control not disabled
                 (= (gs +textbox+ +text-readonly+) 0)      ; TextBox not on read-only mode
                 (not *gui-locked*)                        ; Gui not locked
                 (not *gui-control-exclusive-mode*)        ; No gui slider on dragging
                 (= wrap-mode +text-wrap-none+))           ; No wrap mode
        (let ((mouse-position (gui-pointer-position)))
          (flet ((ctrl-down-p () (or (gui-key-down-p +key-left-control+) (gui-key-down-p +key-right-control+))))
            (if edit-mode
                (let ((auto-cursor-should-trigger nil)
                      (codepoint 0) (codepoint-size 0) (char-encoded nil))
                  ;; GLOBAL: Auto-cursor movement logic
                  ;; NOTE: Keystrokes are handled repeatedly when button is held down for some time
                  (if (or (gui-key-down-p +key-left+) (gui-key-down-p +key-right+)
                          (gui-key-down-p +key-up+) (gui-key-down-p +key-down+)
                          (gui-key-down-p +key-backspace+) (gui-key-down-p +key-delete+))
                      (incf *auto-cursor-counter*)
                      (setf *auto-cursor-counter* 0))

                  (setf auto-cursor-should-trigger (and (> *auto-cursor-counter* +raygui-textbox-auto-cursor-cooldown+)
                                                        (= (mod *auto-cursor-counter* +raygui-textbox-auto-cursor-delay+) 0)))

                  (setf state +state-pressed+)

                  (when (> *text-box-cursor-index* text-length) (setf *text-box-cursor-index* text-length))

                  ;; If text does not fit in the textbox and current cursor position is out of bounds,
                  ;; adding an index offset to text for drawing only what requires depending on cursor
                  (loop while (>= text-width (rectangle-width text-bounds))
                        do (let ((next-codepoint-size (nth-value 1 (%get-codepoint-next text text-index-offset))))
                             (incf text-index-offset next-codepoint-size)
                             (setf text-width (- (%gui-get-text-width text text-index-offset) (%gui-get-text-width text *text-box-cursor-index*)))))

                  (setf codepoint (gui-input-key)) ; Get Unicode codepoint
                  (when (and multiline (gui-key-pressed-p +key-enter+)) (setf codepoint (char-code #\Newline)))

                  ;; Encode codepoint as UTF-8
                  (multiple-value-setq (char-encoded codepoint-size) (%codepoint-to-utf8 codepoint))

                  ;; Handle text paste action
                  (if (and (gui-key-pressed-p +key-v+) (ctrl-down-p))
                      (let ((paste-text (%cstr (get-clipboard-text))))
                        (when paste-text
                          (let ((paste-length 0))
                            ;; Count how many codepoints to copy, stopping at the first unwanted control character
                            (loop
                              (multiple-value-bind (paste-codepoint paste-codepoint-size) (%get-codepoint-next paste-text paste-length)
                                (when (>= (+ text-length paste-length paste-codepoint-size) text-size) (return))
                                (when (and (not (and multiline (= paste-codepoint 10))) (not (>= paste-codepoint 32))) (return))
                                (incf paste-length paste-codepoint-size)))

                            (when (> paste-length 0)
                              ;; Move forward data from cursor position
                              (loop for i from (+ text-length paste-length) above *text-box-cursor-index*
                                    do (setf (aref text i) (aref text (- i paste-length))))

                              ;; Paste data in at cursor
                              (dotimes (i paste-length) (setf (aref text (+ *text-box-cursor-index* i)) (aref paste-text i)))

                              (incf *text-box-cursor-index* paste-length)
                              (incf text-length paste-length)
                              (setf (aref text text-length) 0)))))
                      (when (and (or (and multiline (= codepoint 10)) (>= codepoint 32)) (< (+ text-length codepoint-size) text-size))
                        ;; Adding codepoint to text, at current cursor position

                        ;; Move forward data from cursor position
                        (loop for i from (+ text-length codepoint-size) above *text-box-cursor-index*
                              do (setf (aref text i) (aref text (- i codepoint-size))))

                        ;; Add new codepoint in current cursor position
                        (dotimes (i codepoint-size) (setf (aref text (+ *text-box-cursor-index* i)) (aref char-encoded i)))

                        (incf *text-box-cursor-index* codepoint-size)
                        (incf text-length codepoint-size)

                        ;; Make sure text last character is EOL
                        (setf (aref text text-length) 0)))

                  ;; Move cursor to start
                  (when (and (> text-length 0) (gui-key-pressed-p +key-home+)) (setf *text-box-cursor-index* 0))

                  ;; Move cursor to end
                  (when (and (> text-length *text-box-cursor-index*) (gui-key-pressed-p +key-end+)) (setf *text-box-cursor-index* text-length))

                  ;; Delete related codepoints from text, after current cursor position
                  (cond ((and (> text-length *text-box-cursor-index*) (gui-key-pressed-p +key-delete+) (ctrl-down-p))
                         (let ((offset *text-box-cursor-index*)
                               (acc-codepoint-size 0))
                           ;; Check characters of the same type to delete (either ASCII punctuation or anything non-whitespace)
                           ;; Not using isalnum() since it only works on ASCII characters
                           (multiple-value-bind (next-codepoint next-codepoint-size) (%get-codepoint-next text offset)
                             (let ((puctuation (%c-ispunct (logand next-codepoint #xff))))
                               (loop while (< offset text-length)
                                     do (when (or (and puctuation (not (%c-ispunct (logand next-codepoint #xff))))
                                                  (and (not puctuation) (or (%c-isspace (logand next-codepoint #xff)) (%c-ispunct (logand next-codepoint #xff)))))
                                          (return))
                                        (incf offset next-codepoint-size)
                                        (incf acc-codepoint-size next-codepoint-size)
                                        (multiple-value-setq (next-codepoint next-codepoint-size) (%get-codepoint-next text offset))))

                             ;; Check whitespace to delete (ASCII only)
                             (loop while (< offset text-length)
                                   do (unless (%c-isspace (logand next-codepoint #xff)) (return))
                                      (incf offset next-codepoint-size)
                                      (incf acc-codepoint-size next-codepoint-size)
                                      (multiple-value-setq (next-codepoint next-codepoint-size) (%get-codepoint-next text offset))))

                           ;; Move text after cursor forward (including final null terminator)
                           (loop for i from offset to text-length do (setf (aref text (- i acc-codepoint-size)) (aref text i)))

                           (decf text-length acc-codepoint-size)))
                        ((and (> text-length *text-box-cursor-index*) (or (gui-key-pressed-p +key-delete+)
                                                                           (and (gui-key-down-p +key-delete+) auto-cursor-should-trigger)))
                         ;; Delete single codepoint from text, after current cursor position
                         (let ((next-codepoint-size (nth-value 1 (%get-codepoint-next text *text-box-cursor-index*))))
                           ;; Move text after cursor forward (including final null terminator)
                           (loop for i from (+ *text-box-cursor-index* next-codepoint-size) to text-length
                                 do (setf (aref text (- i next-codepoint-size)) (aref text i)))

                           (decf text-length next-codepoint-size))))

                  ;; Delete related codepoints from text, before current cursor position
                  (cond ((and (> *text-box-cursor-index* 0) (gui-key-pressed-p +key-backspace+) (ctrl-down-p))
                         (let ((offset *text-box-cursor-index*)
                               (acc-codepoint-size 0)
                               (prev-codepoint-size 0)
                               (prev-codepoint 0))
                           ;; Check whitespace to delete (ASCII only)
                           (loop while (> offset 0)
                                 do (multiple-value-setq (prev-codepoint prev-codepoint-size) (%get-codepoint-previous text offset))
                                    (unless (%c-isspace (logand prev-codepoint #xff)) (return))

                                    (decf offset prev-codepoint-size)
                                    (incf acc-codepoint-size prev-codepoint-size))

                           ;; Check characters of the same type to delete (either ASCII punctuation or anything non-whitespace)
                           ;; Not using isalnum() since it only works on ASCII characters
                           (let ((puctuation (%c-ispunct (logand prev-codepoint #xff))))
                             (loop while (> offset 0)
                                   do (multiple-value-setq (prev-codepoint prev-codepoint-size) (%get-codepoint-previous text offset))
                                      (when (or (and puctuation (not (%c-ispunct (logand prev-codepoint #xff))))
                                                (and (not puctuation) (or (%c-isspace (logand prev-codepoint #xff)) (%c-ispunct (logand prev-codepoint #xff)))))
                                        (return))

                                      (decf offset prev-codepoint-size)
                                      (incf acc-codepoint-size prev-codepoint-size)))

                           ;; Move text after cursor forward (including final null terminator)
                           (loop for i from *text-box-cursor-index* to text-length do (setf (aref text (- i acc-codepoint-size)) (aref text i)))

                           (decf text-length acc-codepoint-size)
                           (decf *text-box-cursor-index* acc-codepoint-size)))
                        ((and (> *text-box-cursor-index* 0) (or (gui-key-pressed-p +key-backspace+)
                                                                (and (gui-key-down-p +key-backspace+) auto-cursor-should-trigger)))
                         ;; Delete single codepoint from text, before current cursor position
                         (let ((prev-codepoint-size (nth-value 1 (%get-codepoint-previous text *text-box-cursor-index*))))
                           ;; Move text after cursor forward (including final null terminator)
                           (loop for i from *text-box-cursor-index* to text-length do (setf (aref text (- i prev-codepoint-size)) (aref text i)))

                           (decf text-length prev-codepoint-size)
                           (decf *text-box-cursor-index* prev-codepoint-size))))

                  ;; Move cursor position with keys
                  (cond ((and (> *text-box-cursor-index* 0) (gui-key-pressed-p +key-left+) (ctrl-down-p))
                         (let ((offset *text-box-cursor-index*)
                               (prev-codepoint-size 0)
                               (prev-codepoint 0))
                           ;; Check whitespace to skip (ASCII only)
                           (loop while (> offset 0)
                                 do (multiple-value-setq (prev-codepoint prev-codepoint-size) (%get-codepoint-previous text offset))
                                    (unless (%c-isspace (logand prev-codepoint #xff)) (return))

                                    (decf offset prev-codepoint-size))

                           ;; Check characters of the same type to skip (either ASCII punctuation or anything non-whitespace)
                           ;; Not using isalnum() since it only works on ASCII characters
                           (let ((puctuation (%c-ispunct (logand prev-codepoint #xff))))
                             (loop while (> offset 0)
                                   do (multiple-value-setq (prev-codepoint prev-codepoint-size) (%get-codepoint-previous text offset))
                                      (when (or (and puctuation (not (%c-ispunct (logand prev-codepoint #xff))))
                                                (and (not puctuation) (or (%c-isspace (logand prev-codepoint #xff)) (%c-ispunct (logand prev-codepoint #xff)))))
                                        (return))

                                      (decf offset prev-codepoint-size)))

                           (setf *text-box-cursor-index* offset)))
                        ((and (> *text-box-cursor-index* 0) (or (gui-key-pressed-p +key-left+)
                                                                (and (gui-key-down-p +key-left+) auto-cursor-should-trigger)))
                         (let ((prev-codepoint-size (nth-value 1 (%get-codepoint-previous text *text-box-cursor-index*))))
                           (decf *text-box-cursor-index* prev-codepoint-size)))
                        ((and (> text-length *text-box-cursor-index*) (gui-key-pressed-p +key-right+) (ctrl-down-p))
                         (let ((offset *text-box-cursor-index*))
                           ;; Check characters of the same type to skip (either ASCII punctuation or anything non-whitespace)
                           ;; Not using isalnum() since it only works on ASCII characters
                           (multiple-value-bind (next-codepoint next-codepoint-size) (%get-codepoint-next text offset)
                             (let ((puctuation (%c-ispunct (logand next-codepoint #xff))))
                               (loop while (< offset text-length)
                                     do (when (or (and puctuation (not (%c-ispunct (logand next-codepoint #xff))))
                                                  (and (not puctuation) (or (%c-isspace (logand next-codepoint #xff)) (%c-ispunct (logand next-codepoint #xff)))))
                                          (return))

                                        (incf offset next-codepoint-size)
                                        (multiple-value-setq (next-codepoint next-codepoint-size) (%get-codepoint-next text offset))))

                             ;; Check whitespace to skip (ASCII only)
                             (loop while (< offset text-length)
                                   do (unless (%c-isspace (logand next-codepoint #xff)) (return))

                                      (incf offset next-codepoint-size)
                                      (multiple-value-setq (next-codepoint next-codepoint-size) (%get-codepoint-next text offset))))

                           (setf *text-box-cursor-index* offset)))
                        ((and (> text-length *text-box-cursor-index*) (or (gui-key-pressed-p +key-right+)
                                                                           (and (gui-key-down-p +key-right+) auto-cursor-should-trigger)))
                         (let ((next-codepoint-size (nth-value 1 (%get-codepoint-next text *text-box-cursor-index*))))
                           (incf *text-box-cursor-index* next-codepoint-size))))

                  ;; Move cursor position with mouse
                  (if (check-collision-point-rec mouse-position text-bounds) ; Mouse hover text
                      (let ((scale-factor (/ (float (gs +default+ +text-size+)) (float (font-base-size *gui-font*))))
                            (codepoint-index 0)
                            (glyph-width 0.0)
                            (width-to-mouse-x 0.0)
                            (mouse-cursor-index 0))

                        (loop for i = text-index-offset then (+ i codepoint-size)
                              while (< i text-length)
                              do (multiple-value-setq (codepoint codepoint-size) (%get-codepoint-next text i))
                                 (setf codepoint-index (get-glyph-index *gui-font* codepoint))

                                 (setf glyph-width (%glyph-width codepoint-index scale-factor))

                                 (when (<= (vx mouse-position) (+ (rectangle-x text-bounds) (+ width-to-mouse-x (/ glyph-width 2))))
                                   (setf (rectangle-x mouse-cursor) (+ (rectangle-x text-bounds) width-to-mouse-x)
                                         mouse-cursor-index i)
                                   (return))

                                 (incf width-to-mouse-x (+ glyph-width (float (gs +default+ +text-spacing+)))))

                        ;; Check if mouse cursor is at the last position
                        (let ((text-end-width (%gui-get-text-width text text-index-offset)))
                          (when (>= (vx (gui-pointer-position)) (- (+ (rectangle-x text-bounds) text-end-width) (/ glyph-width 2)))
                            (setf (rectangle-x mouse-cursor) (+ (rectangle-x text-bounds) text-end-width)
                                  mouse-cursor-index text-length)))

                        ;; Place cursor at required index on mouse click
                        (when (and (>= (rectangle-x mouse-cursor) 0) (gui-button-pressed-p))
                          (setf (rectangle-x cursor) (rectangle-x mouse-cursor)
                                *text-box-cursor-index* mouse-cursor-index)))
                      (setf (rectangle-x mouse-cursor) -1.0))

                  ;; Recalculate cursor position.y depending on textBoxCursorIndex
                  (setf (rectangle-x cursor) (float (+ (- (+ (rectangle-x bounds) (gs +textbox+ +text-padding+) (%gui-get-text-width text text-index-offset))
                                                         (%gui-get-text-width text *text-box-cursor-index*))
                                                      (gs +default+ +text-spacing+))))
                  ;;if (multiline) cursor.y = GetTextLines()

                  ;; Finish text editing on ENTER or mouse click outside bounds
                  (when (or (and (not multiline) (gui-key-pressed-p +key-enter+))
                            (and (not (check-collision-point-rec mouse-position bounds)) (gui-button-pressed-p)))
                    (setf *text-box-cursor-index* 0  ; GLOBAL: Reset the shared cursor index
                          *auto-cursor-counter* 0    ; GLOBAL: Reset counter for repeated keystrokes
                          result +result-pressed+)))
                (when (check-collision-point-rec mouse-position bounds)
                  (setf state +state-focused+)

                  (when (gui-button-pressed-p)
                    (setf *text-box-cursor-index* text-length ; GLOBAL: Place cursor index to the end of current text
                          *auto-cursor-counter* 0             ; GLOBAL: Reset counter for repeated keystrokes
                          result +result-pressed+)))))))
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      (cond ((= state +state-pressed+)
             (%gui-draw-rectangle bounds (gs +textbox+ +border-width+) (gcol +textbox+ (+ +border+ (* state 3))) (gcol +textbox+ +base-color-pressed+)))
            ((= state +state-disabled+)
             (%gui-draw-rectangle bounds (gs +textbox+ +border-width+) (gcol +textbox+ (+ +border+ (* state 3))) (gcol +textbox+ +base-color-disabled+)))
            (t (%gui-draw-rectangle bounds (gs +textbox+ +border-width+) (gcol +textbox+ (+ +border+ (* state 3))) +blank+)))

      ;; Draw text considering index offset if required
      ;; NOTE: Text index offset depends on cursor position
      (%gui-draw-text text text-index-offset text-bounds (gs +textbox+ +text-alignment+) (gcol +textbox+ (+ +text+ (* state 3))))

      ;; Draw cursor
      (cond ((and edit-mode (= (gs +textbox+ +text-readonly+) 0))
             ;;if (autoCursorMode || ((blinkCursorFrameCounter/40)%2 == 0))
             (%gui-draw-rectangle cursor 0 +blank+ (gcol +textbox+ +border-color-pressed+))

             ;; Draw mouse position cursor (if required)
             (when (>= (rectangle-x mouse-cursor) 0) (%gui-draw-rectangle mouse-cursor 0 +blank+ (gcol +textbox+ +border-color-pressed+))))
            ((= state +state-focused+) (%gui-tooltip bounds)))
      ;;--------------------------------------------------------------------

      result)))

;; Spinner control, returns selected value
;; Returns (values result value)
(defun gui-spinner (bounds text value min-value max-value edit-mode)
  (let* ((result 1)
         (state *gui-state*)
         (text (%cstr text))

         (temp-value value)

         (value-box-bounds (%rec (+ (rectangle-x bounds) (gs +valuebox+ +spinner-button-width+) (gs +valuebox+ +spinner-button-spacing+))
                                 (rectangle-y bounds)
                                 (- (rectangle-width bounds) (* 2 (+ (gs +valuebox+ +spinner-button-width+) (gs +valuebox+ +spinner-button-spacing+))))
                                 (rectangle-height bounds)))
         (left-button-bound (%rec (rectangle-x bounds) (rectangle-y bounds) (float (gs +valuebox+ +spinner-button-width+)) (rectangle-height bounds)))
         (right-button-bound (%rec (- (+ (rectangle-x bounds) (rectangle-width bounds)) (gs +valuebox+ +spinner-button-width+)) (rectangle-y bounds)
                                   (float (gs +valuebox+ +spinner-button-width+)) (rectangle-height bounds)))

         (text-bounds (make-rectangle)))
    (when text
      (setf (rectangle-width text-bounds) (+ (float (gui-get-text-width text)) 2)
            (rectangle-height text-bounds) (float (gs +default+ +text-size+))
            (rectangle-x text-bounds) (+ (rectangle-x bounds) (rectangle-width bounds) (gs +valuebox+ +text-padding+))
            (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))
      (when (= (gs +valuebox+ +text-alignment+) +text-align-left+)
        (setf (rectangle-x text-bounds) (- (rectangle-x bounds) (rectangle-width text-bounds) (gs +valuebox+ +text-padding+)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check spinner state
        (when (check-collision-point-rec mouse-point bounds)
          (if (gui-button-down-p)
              (setf state +state-pressed+)
              (setf state +state-focused+)))))

    (when (/= (gui-button left-button-bound (gui-icon-text +icon-arrow-left-fill+ nil)) 0) (decf temp-value))
    (when (/= (gui-button right-button-bound (gui-icon-text +icon-arrow-right-fill+ nil)) 0) (incf temp-value))

    (unless edit-mode
      (when (< temp-value min-value) (setf temp-value min-value))
      (when (> temp-value max-value) (setf temp-value max-value)))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (multiple-value-setq (result temp-value) (gui-value-box value-box-bounds nil temp-value min-value max-value edit-mode))

    ;; Draw value selector custom buttons
    ;; NOTE: BORDER_WIDTH and TEXT_ALIGNMENT forced values
    (let ((temp-border-width (gs +button+ +border-width+))
          (temp-text-align (gs +button+ +text-alignment+)))
      (gui-set-style +button+ +border-width+ (gs +valuebox+ +border-width+))
      (gui-set-style +button+ +text-alignment+ +text-align-center+)

      (gui-set-style +button+ +text-alignment+ temp-text-align)
      (gui-set-style +button+ +border-width+ temp-border-width))

    ;; Draw text label if provided
    (%gui-draw-text text 0 text-bounds (if (= (gs +valuebox+ +text-alignment+) +text-align-right+) +text-align-left+ +text-align-right+)
                    (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    ;;if (tempValue != *value) result = RESULT_CHANGED; // WARNING: Stops editing
    (values result temp-value)))

;; Value Box control, updates input text with numbers
;; Returns (values result value)
(defun gui-value-box (bounds text value min-value max-value edit-mode)
  (let* ((result +result-none+)
         (state *gui-state*)
         (text (%cstr text))

         ;;int prevValue = *value;
         (text-value (make-array (+ +raygui-valuebox-max-chars+ 2) :element-type '(unsigned-byte 8) :initial-element 0))
         (text-bounds (make-rectangle)))
    (replace text-value (babel:string-to-octets (text-format "%i" value) :encoding :utf-8) :end1 +raygui-valuebox-max-chars+)

    (when text
      (setf (rectangle-width text-bounds) (+ (float (gui-get-text-width text)) 2)
            (rectangle-height text-bounds) (float (gs +default+ +text-size+))
            (rectangle-x text-bounds) (+ (rectangle-x bounds) (rectangle-width bounds) (gs +valuebox+ +text-padding+))
            (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))
      (when (= (gs +valuebox+ +text-alignment+) +text-align-left+)
        (setf (rectangle-x text-bounds) (- (rectangle-x bounds) (rectangle-width text-bounds) (gs +valuebox+ +text-padding+)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position))
            (value-has-changed nil))

        (if edit-mode
            (let ((key-count (%strlen text-value)))
              (setf state +state-pressed+)

              ;; Add or remove minus symbol
              (when (gui-key-pressed-p +key-minus+)
                (cond ((= (aref text-value 0) (char-code #\-))
                       (dotimes (i key-count) (setf (aref text-value i) (aref text-value (1+ i))))

                       (decf key-count)
                       (setf value-has-changed t))
                      ((< key-count +raygui-valuebox-max-chars+)
                       (when (= key-count 0)
                         (setf (aref text-value 0) (char-code #\0)
                               (aref text-value 1) 0)
                         (incf key-count))

                       (loop for i from key-count downto 0 do (setf (aref text-value (1+ i)) (aref text-value i)))

                       (setf (aref text-value 0) (char-code #\-))
                       (incf key-count)
                       (setf value-has-changed t))))

              ;; Add new digit to text value
              (when (and (>= key-count 0) (< key-count +raygui-valuebox-max-chars+) (< (gui-get-text-width text-value) (rectangle-width bounds)))
                (let ((key (gui-input-key)))

                  ;; Only allow keys in range [48..57]
                  (when (<= 48 key 57)
                    (setf (aref text-value key-count) key)
                    (incf key-count)
                    (setf value-has-changed t))))

              ;; Delete text
              (when (and (> key-count 0) (gui-key-pressed-p +key-backspace+))
                (decf key-count)
                (setf (aref text-value key-count) 0)
                (setf value-has-changed t))

              (when value-has-changed (setf value (text-to-integer (%lisp-string text-value))))

              ;; NOTE: Values are not clamped until user input finishes
              ;;if (*value > maxValue) *value = maxValue;
              ;;else if (*value < minValue) *value = minValue;

              (when (or (or (gui-key-pressed-p +key-enter+) (gui-key-pressed-p +key-kp-enter+))
                        (and (not (check-collision-point-rec mouse-point bounds)) (gui-button-pressed-p)))
                (cond ((> value max-value) (setf value max-value))
                      ((< value min-value) (setf value min-value)))

                (setf result +result-pressed+)))
            (progn
              (cond ((> value max-value) (setf value max-value))
                    ((< value min-value) (setf value min-value)))

              (when (check-collision-point-rec mouse-point bounds)
                (setf state +state-focused+)
                (when (gui-button-pressed-p) (setf result +result-pressed+)))))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (let ((base-color +blank+))
      (cond ((= state +state-pressed+) (setf base-color (gcol +valuebox+ +base-color-pressed+)))
            ((= state +state-disabled+) (setf base-color (gcol +valuebox+ +base-color-disabled+))))

      (%gui-draw-rectangle bounds (gs +valuebox+ +border-width+) (gcol +valuebox+ (+ +border+ (* state 3))) base-color)
      (%gui-draw-text text-value 0 (%get-text-bounds +valuebox+ bounds) +text-align-center+ (gcol +valuebox+ (+ +text+ (* state 3)))))

    ;; Draw cursor rectangle
    (when edit-mode
      ;; NOTE: ValueBox internal text is always centered
      (let ((cursor (%rec (+ (rectangle-x bounds) (truncate (gui-get-text-width text-value) 2) (/ (rectangle-width bounds) 2) 1)
                          (+ (rectangle-y bounds) (gs +textbox+ +border-width+) 2)
                          2 (- (rectangle-height bounds) (* (gs +textbox+ +border-width+) 2) 4))))
        (when (> (rectangle-height cursor) (rectangle-height bounds))
          (setf (rectangle-height cursor) (float (- (rectangle-height bounds) (* (gs +textbox+ +border-width+) 2)))))
        (%gui-draw-rectangle cursor 0 +blank+ (gcol +valuebox+ +border-color-pressed+))))

    ;; Draw text label if provided
    (%gui-draw-text text 0 text-bounds (if (= (gs +valuebox+ +text-alignment+) +text-align-right+) +text-align-left+ +text-align-right+)
                    (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    (values result value)))

;; Floating point Value Box control, updates input val_str with numbers
;; Returns (values result text-value value)
(defun gui-value-box-float (bounds text text-value value edit-mode)
  (let* ((result +result-none+)
         (state *gui-state*)
         (text (%cstr text))
         (text-value-buffer (make-array (+ +raygui-valuebox-max-chars+ 2) :element-type '(unsigned-byte 8) :initial-element 0))
         (value (float value 1.0))

         ;;float prevValue = *value;
         (text-bounds (make-rectangle)))
    (replace text-value-buffer (babel:string-to-octets (or text-value "") :encoding :utf-8) :end1 +raygui-valuebox-max-chars+)

    (when text
      (setf (rectangle-width text-bounds) (+ (float (gui-get-text-width text)) 2)
            (rectangle-height text-bounds) (float (gs +default+ +text-size+))
            (rectangle-x text-bounds) (+ (rectangle-x bounds) (rectangle-width bounds) (gs +valuebox+ +text-padding+))
            (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))
      (when (= (gs +valuebox+ +text-alignment+) +text-align-left+)
        (setf (rectangle-x text-bounds) (- (rectangle-x bounds) (rectangle-width text-bounds) (gs +valuebox+ +text-padding+)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position))
            (value-has-changed nil))

        (if edit-mode
            (let ((key-count (%strlen text-value-buffer)))
              (setf state +state-pressed+)

              ;; Add or remove minus symbol
              (when (gui-key-pressed-p +key-minus+)
                (cond ((= (aref text-value-buffer 0) (char-code #\-))
                       (dotimes (i key-count) (setf (aref text-value-buffer i) (aref text-value-buffer (1+ i))))

                       (decf key-count)
                       (setf value-has-changed t))
                      ((< key-count (1- +raygui-valuebox-max-chars+))
                       (when (= key-count 0)
                         (setf (aref text-value-buffer 0) (char-code #\0)
                               (aref text-value-buffer 1) 0)
                         (incf key-count))

                       (loop for i from key-count downto 0 do (setf (aref text-value-buffer (1+ i)) (aref text-value-buffer i)))

                       (setf (aref text-value-buffer 0) (char-code #\-))
                       (incf key-count)
                       (setf value-has-changed t))))

              ;; Only allow keys in range [48..57]
              (when (< key-count +raygui-valuebox-max-chars+)
                (when (< (gui-get-text-width text-value-buffer) (rectangle-width bounds))
                  (let ((key (gui-input-key)))
                    (when (or (<= 48 key 57)
                              (= key (char-code #\.))
                              (and (= key-count 0) (= key (char-code #\+))) ; NOTE: Sign can only be in first position
                              (and (= key-count 0) (= key (char-code #\-))))
                      (setf (aref text-value-buffer key-count) key)
                      (incf key-count)

                      (setf value-has-changed t)))))

              ;; Pressed backspace
              (when (gui-key-pressed-p +key-backspace+)
                (when (> key-count 0)
                  (decf key-count)
                  (setf (aref text-value-buffer key-count) 0)
                  (setf value-has-changed t)))

              (when value-has-changed (setf value (float (text-to-float (%lisp-string text-value-buffer)) 1.0)))

              (when (or (or (gui-key-pressed-p +key-enter+) (gui-key-pressed-p +key-kp-enter+))
                        (and (not (check-collision-point-rec mouse-point bounds)) (gui-button-pressed-p)))
                (setf result +result-pressed+)))
            (when (check-collision-point-rec mouse-point bounds)
              (setf state +state-focused+)
              (when (gui-button-pressed-p) (setf result +result-pressed+))))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (let ((base-color +blank+))
      (cond ((= state +state-pressed+) (setf base-color (gcol +valuebox+ +base-color-pressed+)))
            ((= state +state-disabled+) (setf base-color (gcol +valuebox+ +base-color-disabled+))))

      (%gui-draw-rectangle bounds (gs +valuebox+ +border-width+) (gcol +valuebox+ (+ +border+ (* state 3))) base-color)
      (%gui-draw-text text-value-buffer 0 (%get-text-bounds +valuebox+ bounds) +text-align-center+ (gcol +valuebox+ (+ +text+ (* state 3)))))

    ;; Draw cursor
    (when edit-mode
      ;; NOTE: ValueBox internal text is always centered
      (let ((cursor (%rec (+ (rectangle-x bounds) (truncate (gui-get-text-width text-value-buffer) 2) (/ (rectangle-width bounds) 2) 1)
                          (+ (rectangle-y bounds) (* 2 (gs +valuebox+ +border-width+))) 4
                          (- (rectangle-height bounds) (* 4 (gs +valuebox+ +border-width+))))))
        (%gui-draw-rectangle cursor 0 +blank+ (gcol +valuebox+ +border-color-pressed+))))

    ;; Draw text label if provided
    (%gui-draw-text text 0 text-bounds (if (= (gs +valuebox+ +text-alignment+) +text-align-right+) +text-align-left+ +text-align-right+)
                    (gcol +label+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    (values result (%lisp-string text-value-buffer) value)))

;; Slider control with pro parameters
;; NOTE: Other GuiSlider*() controls use this one
;; Returns (values result value)
(defun gui-slider (bounds text-left text-right value min-value max-value)
  (let* ((result +result-none+)
         (state *gui-state*)
         (min-value (float min-value 1.0))
         (max-value (float max-value 1.0))
         (value (if value (float value 1.0) (/ (- max-value min-value) 2.0)))
         (prev-value value)
         (text-left (%cstr text-left))
         (text-right (%cstr text-right))

         (slider-width (gs +slider+ +slider-width+))

         (slider (%rec (rectangle-x bounds) (+ (rectangle-y bounds) (gs +slider+ +border-width+) (gs +slider+ +slider-padding+))
                       0 (- (rectangle-height bounds) (* 2 (gs +slider+ +border-width+)) (* 2 (gs +slider+ +slider-padding+))))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*))
      (let ((mouse-point (gui-pointer-position)))

        (cond (*gui-control-exclusive-mode* ; Allows to keep dragging outside of bounds
               (if (gui-button-down-p)
                   (when (check-bounds-id bounds *gui-control-exclusive-rec*)
                     (setf state +state-pressed+)
                     ;; Get equivalent value and slider position from mousePosition.x
                     (setf value (+ (* (- max-value min-value) (/ (- (vx mouse-point) (rectangle-x bounds) (truncate slider-width 2))
                                                                  (- (rectangle-width bounds) slider-width)))
                                    min-value)))
                   (setf *gui-control-exclusive-mode* nil
                         *gui-control-exclusive-rec* (%rec 0 0 0 0))))
              ((check-collision-point-rec mouse-point bounds)
               (if (gui-button-down-p)
                   (progn
                     (setf state +state-pressed+
                           *gui-control-exclusive-mode* t
                           *gui-control-exclusive-rec* (%rec-copy bounds)) ; Store bounds as an identifier when dragging starts

                     (unless (check-collision-point-rec mouse-point slider)
                       ;; Get equivalent value and slider position from mousePosition.x
                       (setf value (+ (* (- max-value min-value) (/ (- (vx mouse-point) (rectangle-x bounds) (truncate slider-width 2))
                                                                    (- (rectangle-width bounds) slider-width)))
                                      min-value))))
                   (setf state +state-focused+))))

        (cond ((> value max-value) (setf value max-value))
              ((< value min-value) (setf value min-value)))))

    ;; Slider bar limits check
    (let ((slider-value (* (/ (- value min-value) (- max-value min-value)) (- (rectangle-width bounds) slider-width (* 2 (gs +slider+ +border-width+))))))
      (cond ((> slider-width 0)         ; Slider
             (incf (rectangle-x slider) slider-value)
             (setf (rectangle-width slider) (float slider-width))
             (cond ((<= (rectangle-x slider) (+ (rectangle-x bounds) (gs +slider+ +border-width+)))
                    (setf (rectangle-x slider) (+ (rectangle-x bounds) (gs +slider+ +border-width+))))
                   ((>= (+ (rectangle-x slider) (rectangle-width slider)) (+ (rectangle-x bounds) (rectangle-width bounds)))
                    (setf (rectangle-x slider) (- (+ (rectangle-x bounds) (rectangle-width bounds)) (rectangle-width slider) (gs +slider+ +border-width+))))))
            ((= slider-width 0)         ; SliderBar
             (incf (rectangle-x slider) (gs +slider+ +border-width+))
             (setf (rectangle-width slider) slider-value)
             (when (> (rectangle-width slider) (rectangle-width bounds))
               (setf (rectangle-width slider) (- (rectangle-width bounds) (* 2 (gs +slider+ +border-width+))))))))

    (when (/= prev-value value) (setf result +result-changed+))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +slider+ +border-width+) (gcol +slider+ (+ +border+ (* state 3)))
                         (gcol +slider+ (if (/= state +state-disabled+) +base-color-normal+ +base-color-disabled+)))

    ;; Draw slider internal bar (depends on state)
    (cond ((= state +state-normal+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +base-color-pressed+)))
          ((= state +state-focused+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +text-color-focused+)))
          ((= state +state-pressed+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +text-color-pressed+)))
          ((= state +state-disabled+) (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ +text-color-disabled+))))

    ;; Draw left/right text if provided
    (when text-left
      (let ((text-bounds (make-rectangle)))
        (setf (rectangle-width text-bounds) (float (gui-get-text-width text-left))
              (rectangle-height text-bounds) (float (gs +default+ +text-size+))
              (rectangle-x text-bounds) (- (rectangle-x bounds) (rectangle-width text-bounds) (gs +slider+ +text-padding+))
              (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))

        (%gui-draw-text text-left 0 text-bounds +text-align-right+ (gcol +label+ (+ +text+ (* state 3))))))

    (when text-right
      (let ((text-bounds (make-rectangle)))
        (setf (rectangle-width text-bounds) (float (gui-get-text-width text-right))
              (rectangle-height text-bounds) (float (gs +default+ +text-size+))
              (rectangle-x text-bounds) (+ (rectangle-x bounds) (rectangle-width bounds) (gs +slider+ +text-padding+))
              (rectangle-y text-bounds) (- (+ (rectangle-y bounds) (/ (rectangle-height bounds) 2)) (truncate (gs +default+ +text-size+) 2)))

        (%gui-draw-text text-right 0 text-bounds +text-align-left+ (gcol +label+ (+ +text+ (* state 3))))))
    ;;--------------------------------------------------------------------

    (values result value)))

;; Slider Bar control extended, returns selected value
;; Returns (values result value)
(defun gui-slider-bar (bounds text-left text-right value min-value max-value)
  (let ((result +result-none+)
        (pre-slider-width (gs +slider+ +slider-width+)))
    (gui-set-style +slider+ +slider-width+ 0)
    (multiple-value-setq (result value) (gui-slider bounds text-left text-right value min-value max-value))
    (gui-set-style +slider+ +slider-width+ pre-slider-width)

    (values result value)))

;; Progress Bar control extended, shows current progress value
;; Returns (values result value)
(defun gui-progress-bar (bounds text-left text-right value min-value max-value)
  (let* ((result +result-none+)
         (state *gui-state*)
         (min-value (float min-value 1.0))
         (max-value (float max-value 1.0))
         (value (if value (float value 1.0) (/ (- max-value min-value) 2.0)))
         (prev-value value)
         (text-left (%cstr text-left))
         (text-right (%cstr text-right))
         (bw (gs +progressbar+ +border-width+))

         ;; Progress bar
         (progress (%rec (+ (rectangle-x bounds) bw)
                         (+ (rectangle-y bounds) bw (gs +progressbar+ +progress-padding+)) 0
                         (- (rectangle-height bounds) bw (* 2 (gs +progressbar+ +progress-padding+)) 1)))
         (x (rectangle-x bounds)) (y (rectangle-y bounds)) (w (rectangle-width bounds)) (h (rectangle-height bounds)))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (> value max-value) (setf value max-value))

    ;; WARNING: Working with floats could lead to rounding issues
    (when (/= state +state-disabled+)
      (setf (rectangle-width progress) (* (/ value (- max-value min-value)) (- w (* 2 bw)))))

    (when (/= prev-value value) (setf result +result-changed+))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (if (= state +state-disabled+)
        (%gui-draw-rectangle bounds bw (gcol +progressbar+ (+ +border+ (* state 3))) +blank+)
        (let ((pw (truncate (rectangle-width progress))))
          (if (> value min-value)
              (progn
                ;; Draw progress bar with colored border, more visual
                (%gui-draw-rectangle (%rec x y (+ pw (float bw)) (float bw)) 0 +blank+ (gcol +progressbar+ +border-color-focused+))
                (%gui-draw-rectangle (%rec x (+ y 1) (float bw) (- h 2)) 0 +blank+ (gcol +progressbar+ +border-color-focused+))
                (%gui-draw-rectangle (%rec x (- (+ y h) 1) (+ pw (float bw)) (float bw)) 0 +blank+ (gcol +progressbar+ +border-color-focused+)))
              (%gui-draw-rectangle (%rec x y (float bw) (- (+ h bw) 1)) 0 +blank+ (gcol +progressbar+ +border-color-normal+)))

          (if (>= value max-value)
              (%gui-draw-rectangle (%rec (+ x (rectangle-width progress) (float bw)) y (float bw) (- (+ h bw) 1)) 0 +blank+ (gcol +progressbar+ +border-color-focused+))
              (progn
                ;; Draw borders not yet reached by value
                (%gui-draw-rectangle (%rec (+ x pw (float bw)) y (- w (float bw) pw 1) (float bw)) 0 +blank+ (gcol +progressbar+ +border-color-normal+))
                (%gui-draw-rectangle (%rec (+ x pw (float bw)) (- (+ y h) 1) (- w (float bw) pw 1) (float bw)) 0 +blank+ (gcol +progressbar+ +border-color-normal+))
                (%gui-draw-rectangle (%rec (- (+ x w) (float bw)) y (float bw) (- (+ h bw) 1)) 0 +blank+ (gcol +progressbar+ +border-color-normal+))))

          ;; Draw slider internal progress bar (depends on state)
          (if (= (gs +progressbar+ +progress-side+) 0) ; Left-->Right
              (%gui-draw-rectangle progress 0 +blank+ (gcol +progressbar+ +base-color-pressed+))
              (progn                    ; Right-->Left
                (setf (rectangle-x progress) (- (+ x w) (rectangle-width progress) bw))
                (%gui-draw-rectangle progress 0 +blank+ (gcol +progressbar+ +base-color-pressed+))))))

    ;; Draw left/right text if provided
    (when text-left
      (let ((text-bounds (make-rectangle)))
        (setf (rectangle-width text-bounds) (float (gui-get-text-width text-left))
              (rectangle-height text-bounds) (float (gs +default+ +text-size+))
              (rectangle-x text-bounds) (- x (rectangle-width text-bounds) (gs +progressbar+ +text-padding+))
              (rectangle-y text-bounds) (- (+ y (/ h 2)) (truncate (gs +default+ +text-size+) 2)))

        (%gui-draw-text text-left 0 text-bounds +text-align-right+ (gcol +label+ (+ +text+ (* state 3))))))

    (when text-right
      (let ((text-bounds (make-rectangle)))
        (setf (rectangle-width text-bounds) (float (gui-get-text-width text-right))
              (rectangle-height text-bounds) (float (gs +default+ +text-size+))
              (rectangle-x text-bounds) (+ x w (gs +progressbar+ +text-padding+))
              (rectangle-y text-bounds) (- (+ y (/ h 2)) (truncate (gs +default+ +text-size+) 2)))

        (%gui-draw-text text-right 0 text-bounds +text-align-left+ (gcol +label+ (+ +text+ (* state 3))))))
    ;;--------------------------------------------------------------------

    (values result value)))

;; Status Bar control
(defun gui-status-bar (bounds text)
  (let ((result +result-none+)
        (state *gui-state*))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check checkbox state
        (when (check-collision-point-rec mouse-point bounds)
          ;;if (GUI_BUTTON_DOWN) state = STATE_PRESSED;
          ;;else state = STATE_FOCUSED;

          (when (gui-button-released-p) (setf result +result-pressed+)))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +statusbar+ +border-width+) (gcol +statusbar+ (+ +border+ (* state 3))) (gcol +statusbar+ (+ +base+ (* state 3))))
    (%gui-draw-text (%cstr text) 0 (%get-text-bounds +statusbar+ bounds) (gs +statusbar+ +text-alignment+) (gcol +statusbar+ (+ +text+ (* state 3))))
    ;;--------------------------------------------------------------------

    result))

;; Dummy rectangle control, intended for placeholding
(defun gui-dummy-rec (bounds text)
  (let ((result +result-none+)
        (state *gui-state*))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (let ((mouse-point (gui-pointer-position)))

        ;; Check button state
        (when (check-collision-point-rec mouse-point bounds)
          (if (gui-button-down-p)
              (setf state +state-pressed+)
              (setf state +state-focused+))

          (when (gui-button-released-p) (setf result +result-pressed+)))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds 0 +blank+ (gcol +default+ (if (/= state +state-disabled+) +base-color-normal+ +base-color-disabled+)))
    (%gui-draw-text (%cstr text) 0 (%get-text-bounds +default+ bounds) +text-align-center+
                    (gcol +button+ (if (/= state +state-disabled+) +text-color-normal+ +text-color-disabled+)))
    ;;------------------------------------------------------------------

    result))

;; List View control
;; Returns (values result scroll-index active)
(defun gui-list-view (bounds text scroll-index active)
  (let* ((text (%cstr text))
         (items (when text (%gui-text-split text (char-code #\;)))))
    (multiple-value-bind (result scroll-index active)
        (%gui-list-view-ex bounds items (length items) scroll-index active nil)
      (values result scroll-index active))))

;; List View control using text entries list and returning focus entry
;; Returns (values result scroll-index active focus)
(defun gui-list-view-ex (bounds text count scroll-index active focus)
  (%gui-list-view-ex bounds (when text (map 'vector #'%cstr text)) count scroll-index active focus))

(defun %gui-list-view-ex (bounds text count scroll-index active focus)
  (let* ((result +result-none+)
         (state *gui-state*)

         (item-focused (or focus -1))
         (item-selected (or active -1))
         (prev-active (or active 0))

         ;; Check if scroll bar is needed
         (use-scroll-bar (> (* (+ (gs +listview+ +list-items-height+) (gs +listview+ +list-items-spacing+)) count) (rectangle-height bounds)))

         ;; Define base item rectangle [0]
         (item-bounds (make-rectangle)))
    (setf (rectangle-x item-bounds) (+ (rectangle-x bounds) (gs +listview+ +list-items-spacing+))
          (rectangle-y item-bounds) (+ (rectangle-y bounds) (gs +listview+ +list-items-spacing+) (gs +default+ +border-width+))
          (rectangle-width item-bounds) (- (rectangle-width bounds) (* 2 (gs +listview+ +list-items-spacing+)) (gs +default+ +border-width+))
          (rectangle-height item-bounds) (float (gs +listview+ +list-items-height+)))
    (when use-scroll-bar (decf (rectangle-width item-bounds) (gs +listview+ +scrollbar-width+)))

    ;; Get items on the list
    (let* ((visible-items (truncate (truncate (rectangle-height bounds)) (+ (gs +listview+ +list-items-height+) (gs +listview+ +list-items-spacing+))))
           (start-index 0) (end-index 0))
      (when (> visible-items count) (setf visible-items count))

      (setf start-index (or scroll-index 0))
      (when (or (< start-index 0) (> start-index (- count visible-items))) (setf start-index 0))
      (setf end-index (+ start-index visible-items))

      ;; Update control
      ;;--------------------------------------------------------------------
      (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
        (let ((mouse-point (gui-pointer-position)))

          ;; Check mouse inside list view
          (if (check-collision-point-rec mouse-point bounds)
              (progn
                (setf state +state-focused+)

                ;; Check focused and selected item
                (dotimes (i visible-items)
                  (when (check-collision-point-rec mouse-point item-bounds)
                    (setf item-focused (+ start-index i))
                    (when (gui-button-pressed-p)
                      (if (= item-selected (+ start-index i))
                          (setf item-selected -1)
                          (setf item-selected (+ start-index i))))
                    (return))

                  ;; Update item rectangle y position for next item
                  (incf (rectangle-y item-bounds) (+ (gs +listview+ +list-items-height+) (gs +listview+ +list-items-spacing+))))

                (when use-scroll-bar
                  (let ((scroll-delta (gui-scroll-delta)))
                    (decf start-index (truncate scroll-delta))

                    (cond ((< start-index 0) (setf start-index 0))
                          ((> start-index (- count visible-items)) (setf start-index (- count visible-items))))

                    (setf end-index (+ start-index visible-items))
                    (when (> end-index count) (setf end-index count)))))
              (setf item-focused -1))

          ;; Reset item rectangle y to [0]
          (setf (rectangle-y item-bounds) (+ (rectangle-y bounds) (gs +listview+ +list-items-spacing+) (gs +default+ +border-width+)))))
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      (%gui-draw-rectangle bounds (gs +listview+ +border-width+) (gcol +listview+ (+ +border+ (* state 3))) (gcol +default+ +background-color+)) ; Draw background

      ;; Draw visible items
      (loop for i from 0
            while (and (< i visible-items) text)
            do (when (/= (gs +listview+ +list-items-border-normal+) 0)
                 (%gui-draw-rectangle item-bounds (gs +listview+ +list-items-border-width+) (gcol +listview+ +border-color-normal+) +blank+))

               (if (= state +state-disabled+)
                   (progn
                     (when (= (+ start-index i) item-selected)
                       (%gui-draw-rectangle item-bounds (gs +listview+ +list-items-border-width+) (gcol +listview+ +border-color-disabled+) (gcol +listview+ +base-color-disabled+)))

                     (%gui-draw-text (aref text (+ start-index i)) 0 (%get-text-bounds +listview+ item-bounds) (gs +listview+ +text-alignment+) (gcol +listview+ +text-color-disabled+)))
                   (cond ((and (= (+ start-index i) item-selected) active)
                          ;; Draw item selected
                          (%gui-draw-rectangle item-bounds (gs +listview+ +list-items-border-width+) (gcol +listview+ +border-color-pressed+) (gcol +listview+ +base-color-pressed+))
                          (%gui-draw-text (aref text (+ start-index i)) 0 (%get-text-bounds +listview+ item-bounds) (gs +listview+ +text-alignment+) (gcol +listview+ +text-color-pressed+)))
                         ((= (+ start-index i) item-focused) ; && (focus != NULL)) // NOTE: Items focused, despite not returned
                          ;; Draw item focused
                          (%gui-draw-rectangle item-bounds (gs +listview+ +list-items-border-width+) (gcol +listview+ +border-color-focused+) (gcol +listview+ +base-color-focused+))
                          (%gui-draw-text (aref text (+ start-index i)) 0 (%get-text-bounds +listview+ item-bounds) (gs +listview+ +text-alignment+) (gcol +listview+ +text-color-focused+)))
                         (t
                          ;; Draw item normal (no rectangle)
                          (%gui-draw-text (aref text (+ start-index i)) 0 (%get-text-bounds +listview+ item-bounds) (gs +listview+ +text-alignment+) (gcol +listview+ +text-color-normal+)))))

               ;; Update item rectangle y position for next item
               (incf (rectangle-y item-bounds) (+ (gs +listview+ +list-items-height+) (gs +listview+ +list-items-spacing+))))

      (when use-scroll-bar
        (let* ((scroll-bar-bounds (%rec (- (+ (rectangle-x bounds) (rectangle-width bounds)) (gs +listview+ +border-width+) (gs +listview+ +scrollbar-width+))
                                        (+ (rectangle-y bounds) (gs +listview+ +border-width+)) (float (gs +listview+ +scrollbar-width+))
                                        (- (rectangle-height bounds) (* 2 (gs +default+ +border-width+)))))

               ;; Calculate percentage of visible items and apply same percentage to scrollbar
               (percent-visible (/ (float (- end-index start-index)) count))
               (slider-size (* (rectangle-height bounds) percent-visible))

               (prev-slider-size (gs +scrollbar+ +scroll-slider-size+)) ; Save default slider size
               (prev-scroll-speed (gs +scrollbar+ +scroll-speed+))) ; Save default scroll speed
          (gui-set-style +scrollbar+ +scroll-slider-size+ (truncate slider-size)) ; Change slider size
          (gui-set-style +scrollbar+ +scroll-speed+ (- count visible-items)) ; Change scroll speed

          (setf start-index (%gui-scroll-bar scroll-bar-bounds start-index 0 (- count visible-items)))

          (gui-set-style +scrollbar+ +scroll-speed+ prev-scroll-speed) ; Reset scroll speed to default
          (gui-set-style +scrollbar+ +scroll-slider-size+ prev-slider-size))) ; Reset slider size to default
      ;;--------------------------------------------------------------------

      (let ((new-active (if active item-selected active))
            (new-focus (if focus item-focused focus))
            (new-scroll-index (if scroll-index start-index scroll-index)))

        (when (/= prev-active (or new-active 0)) (setf result +result-changed+))

        (values result new-scroll-index new-active new-focus)))))

;; Tab Bar control
;; Returns (values result hscroll active)
(defun gui-tab-bar (bounds text hscroll active)
  (let* ((text (%cstr text))
         (items (when text (%gui-text-split text (char-code #\;)))))
    (multiple-value-bind (result hscroll active)
        (%gui-tab-bar-ex bounds items (length items) hscroll active nil)
      (values result hscroll active))))

;; Tab Bar control, using text entries list and returning focus entry
;; Returns (values result hscroll active focus)
(defun gui-tab-bar-ex (bounds text count hscroll active focus)
  (%gui-tab-bar-ex bounds (when text (map 'vector #'%cstr text)) count hscroll active focus))

;; NOTE: In case of tab close result, consider focused tab
;; TODO: Reeplace GuiToggle() usage for custom implementation for the TABS
(defun %gui-tab-bar-ex (bounds text count hscroll active focus)
  (let* ((result +result-none+)
         ;;GuiState state = guiState;
         (tab-items-width (gs +tabbar+ +tab-items-width+))
         (tab-bounds (%rec (rectangle-x bounds) (rectangle-y bounds) (float tab-items-width) (rectangle-height bounds))))

    (cond ((< active 0) (setf active 0))
          ((> active (1- count)) (setf active (1- count))))

    (let ((prev-active active)
          (offset-x 0)                  ; Required in case tabs go out of screen
          (toggle nil))                 ; Required for individual toggles
      (setf offset-x (- (* active tab-items-width) (get-screen-width)))
      (when (< offset-x 0) (setf offset-x 0))

      ;; Draw control
      ;;--------------------------------------------------------------------
      ;;if ((state != STATE_DISABLED) && !guiLocked && !guiControlExclusiveMode) // TODO: Support disabled
      (dotimes (i count)
        (setf (rectangle-x tab-bounds) (+ (rectangle-x bounds) (* (+ tab-items-width 4) i) offset-x))

        (when (< (rectangle-x tab-bounds) (get-screen-width))
          ;; Draw tabs as toggle controls
          (let ((text-alignment (gs +toggle+ +text-alignment+))
                (text-padding (gs +toggle+ +text-padding+)))
            (gui-set-style +toggle+ +text-alignment+ +text-align-left+)
            (gui-set-style +toggle+ +text-padding+ 8)

            (if (= i active)
                (progn
                  (setf toggle t)
                  (gui-toggle tab-bounds (aref text i) toggle))
                (progn
                  (setf toggle nil)
                  (setf toggle (nth-value 1 (gui-toggle tab-bounds (aref text i) toggle)))
                  (when toggle (setf active i))))

            ;; Close tab with middle mouse button pressed
            (when (and (check-collision-point-rec (gui-pointer-position) tab-bounds) (gui-button-pressed-mid-p)) (setf result +result-tab-close+))

            (gui-set-style +toggle+ +text-padding+ text-padding)
            (gui-set-style +toggle+ +text-alignment+ text-alignment))

          (when (/= (gs +tabbar+ +tab-close-button+) 0)
            ;; Draw tab close button
            ;; NOTE: Only draw close button for current tab: if (CheckCollisionPointRec(mousePosition, tabBounds))
            (let ((temp-border-width (gs +button+ +border-width+))
                  (temp-text-alignment (gs +button+ +text-alignment+)))
              (gui-set-style +button+ +border-width+ 1)
              (gui-set-style +button+ +text-alignment+ +text-align-center+)
              (when (/= (gui-button (%rec (- (+ (rectangle-x tab-bounds) (rectangle-width tab-bounds)) 14 5) (+ (rectangle-y tab-bounds) 5) 14 14)
                                    (gui-icon-text +icon-cross-small+ nil))
                        0)
                (setf result +result-tab-close+))
              (gui-set-style +button+ +border-width+ temp-border-width)
              (gui-set-style +button+ +text-alignment+ temp-text-alignment)))))

      ;; Draw tab-bar bottom line
      (%gui-draw-rectangle (%rec (rectangle-x bounds) (- (+ (rectangle-y bounds) (rectangle-height bounds)) 1) (rectangle-width bounds) 1) 0 +blank+
                           (gcol +tabbar+ +border-color-normal+))
      ;;--------------------------------------------------------------------

      ;; NOTE: In case of tab close result, consider focused tab
      (when (and (/= result +result-tab-close+) (/= prev-active active)) (setf result +result-changed+))

      (values result hscroll active focus))))

;; Color Panel control
;; Returns (values result color)
(defun gui-color-panel (bounds text color)
  (let* ((result +result-none+)
         (vcolor (vec3 (/ (float (first color)) 255.0) (/ (float (second color)) 255.0) (/ (float (third color)) 255.0)))
         (hsv (convert-rgb-to-hsv vcolor))
         (prev-hsv (vcopy hsv)))        ; NOTE: Workaround to see if GuiColorPanelHSV() modifies the hsv

    (multiple-value-setq (result hsv) (gui-color-panel-hsv bounds text hsv))

    ;; Check if the hsv was changed, only then change the color
    ;; This is required, because the Color->HSV->Color conversion has precision errors
    ;; Thus the assignment from HSV to Color should only be made, if the HSV has a new user-entered value
    ;; Otherwise GuiColorPanel would often modify it's color without user input
    (when (or (/= (vx hsv) (vx prev-hsv)) (/= (vy hsv) (vy prev-hsv)) (/= (vz hsv) (vz prev-hsv)))
      (let ((rgb (convert-hsv-to-rgb hsv)))
        (setf color (list (%u8 (* 255.0 (vx rgb))) (%u8 (* 255.0 (vy rgb))) (%u8 (* 255.0 (vz rgb))) (fourth color)))))

    (values result color)))

;; Color Bar Alpha control
;; NOTE: Returns alpha value normalized [0..1]
;; Returns (values result alpha)
(defun gui-color-bar-alpha (bounds text alpha)
  (declare (ignore text))
  (let* ((result +result-none+)
         (state *gui-state*)
         (alpha (float alpha 1.0))
         (prev-alpha alpha)
         (selector (%rec (- (+ (rectangle-x bounds) (* alpha (rectangle-width bounds))) (truncate (gs +colorpicker+ +huebar-selector-height+) 2))
                         (- (rectangle-y bounds) (gs +colorpicker+ +huebar-selector-overflow+))
                         (float (gs +colorpicker+ +huebar-selector-height+))
                         (+ (rectangle-height bounds) (* (gs +colorpicker+ +huebar-selector-overflow+) 2)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*))
      (let ((mouse-point (gui-pointer-position)))

        (cond (*gui-control-exclusive-mode* ; Allows to keep dragging outside of bounds
               (if (gui-button-down-p)
                   (when (check-bounds-id bounds *gui-control-exclusive-rec*)
                     (setf state +state-pressed+)

                     (setf alpha (/ (- (vx mouse-point) (rectangle-x bounds)) (rectangle-width bounds)))
                     (when (<= alpha 0.0) (setf alpha 0.0))
                     (when (>= alpha 1.0) (setf alpha 1.0)))
                   (setf *gui-control-exclusive-mode* nil
                         *gui-control-exclusive-rec* (%rec 0 0 0 0))))
              ((or (check-collision-point-rec mouse-point bounds) (check-collision-point-rec mouse-point selector))
               (if (gui-button-down-p)
                   (progn
                     (setf state +state-pressed+
                           *gui-control-exclusive-mode* t
                           *gui-control-exclusive-rec* (%rec-copy bounds)) ; Store bounds as an identifier when dragging starts

                     (setf alpha (/ (- (vx mouse-point) (rectangle-x bounds)) (rectangle-width bounds)))
                     (when (<= alpha 0.0) (setf alpha 0.0))
                     (when (>= alpha 1.0) (setf alpha 1.0)))
                   ;;selector.x = bounds.x + (int)(((alpha - 0)/(100 - 0))*(bounds.width - 2*GuiGetStyle(SLIDER, BORDER_WIDTH))) - selector.width/2;
                   (setf state +state-focused+))))))

    (when (/= prev-alpha alpha) (setf result +result-changed+))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    ;; Draw alpha bar: checked background
    (if (/= state +state-disabled+)
        (let ((checks-x (truncate (truncate (rectangle-width bounds)) +raygui-colorbaralpha-checked-size+))
              (checks-y (truncate (truncate (rectangle-height bounds)) +raygui-colorbaralpha-checked-size+)))
          (dotimes (x checks-x)
            (dotimes (y checks-y)
              (let ((check (%rec (+ (rectangle-x bounds) (* x +raygui-colorbaralpha-checked-size+))
                                 (+ (rectangle-y bounds) (* y +raygui-colorbaralpha-checked-size+))
                                 +raygui-colorbaralpha-checked-size+ +raygui-colorbaralpha-checked-size+)))
                (%gui-draw-rectangle check 0 +blank+ (if (oddp (+ x y))
                                                         (fade (gcol +colorpicker+ +border-color-disabled+) 0.4)
                                                         (fade (gcol +colorpicker+ +base-color-disabled+) 0.4))))))

          (draw-rectangle-gradient-ex bounds (list 255 255 255 0) (list 255 255 255 0)
                                      (fade (list 0 0 0 255) *gui-alpha*) (fade (list 0 0 0 255) *gui-alpha*)))
        (draw-rectangle-gradient-ex bounds (fade (gcol +colorpicker+ +base-color-disabled+) 0.1) (fade (gcol +colorpicker+ +base-color-disabled+) 0.1)
                                    (fade (gcol +colorpicker+ +border-color-disabled+) *gui-alpha*) (fade (gcol +colorpicker+ +border-color-disabled+) *gui-alpha*)))

    (%gui-draw-rectangle bounds (gs +colorpicker+ +border-width+) (gcol +colorpicker+ (+ +border+ (* state 3))) +blank+)

    ;; Draw alpha bar: selector
    (%gui-draw-rectangle selector 0 +blank+ (gcol +colorpicker+ (+ +border+ (* state 3))))
    ;;--------------------------------------------------------------------

    (values result alpha)))

;; Color Bar Hue control
;; Returns hue value normalized [0..1]
;; NOTE: Other similar bars (for reference):
;;      Color GuiColorBarSat() [WHITE->color]
;;      Color GuiColorBarValue() [BLACK->color], HSV/HSL
;;      float GuiColorBarLuminance() [BLACK->WHITE]
;; Returns (values result hue)
(defun gui-color-bar-hue (bounds text hue)
  (declare (ignore text))
  (let* ((result +result-none+)
         (state *gui-state*)
         (hue (float hue 1.0))
         (prev-hue hue)
         (selector (%rec (- (rectangle-x bounds) (gs +colorpicker+ +huebar-selector-overflow+))
                         (- (+ (rectangle-y bounds) (* (/ hue 360.0) (rectangle-height bounds))) (truncate (gs +colorpicker+ +huebar-selector-height+) 2))
                         (+ (rectangle-width bounds) (* (gs +colorpicker+ +huebar-selector-overflow+) 2))
                         (float (gs +colorpicker+ +huebar-selector-height+)))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*))
      (let ((mouse-point (gui-pointer-position)))

        (cond (*gui-control-exclusive-mode* ; Allows to keep dragging outside of bounds
               (if (gui-button-down-p)
                   (when (check-bounds-id bounds *gui-control-exclusive-rec*)
                     (setf state +state-pressed+)

                     (setf hue (/ (* (- (vy mouse-point) (rectangle-y bounds)) 360) (rectangle-height bounds)))
                     (when (<= hue 0.0) (setf hue 0.0))
                     (when (>= hue 359.0) (setf hue 359.0)))
                   (setf *gui-control-exclusive-mode* nil
                         *gui-control-exclusive-rec* (%rec 0 0 0 0))))
              ((or (check-collision-point-rec mouse-point bounds) (check-collision-point-rec mouse-point selector))
               (if (gui-button-down-p)
                   (progn
                     (setf state +state-pressed+
                           *gui-control-exclusive-mode* t
                           *gui-control-exclusive-rec* (%rec-copy bounds)) ; Store bounds as an identifier when dragging starts

                     (setf hue (/ (* (- (vy mouse-point) (rectangle-y bounds)) 360) (rectangle-height bounds)))
                     (when (<= hue 0.0) (setf hue 0.0))
                     (when (>= hue 359.0) (setf hue 359.0)))
                   (setf state +state-focused+))))))

    (when (/= prev-hue hue) (setf result +result-changed+))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (if (/= state +state-disabled+)
        ;; Draw hue bar:color bars
        ;; NOTE: Using DrawRectangleGradientEx(bounds, color1, color2, color2, color1);
        (let ((x (rectangle-x bounds)) (y (rectangle-y bounds)) (w (rectangle-width bounds)) (h (rectangle-height bounds)))
          (flet ((bar (i c1 c2)
                   (draw-rectangle-gradient-ex (%rec x (+ y (* i (/ h 6.0))) w (/ h 6.0))
                                               (fade c1 *gui-alpha*) (fade c2 *gui-alpha*) (fade c2 *gui-alpha*) (fade c1 *gui-alpha*))))
            (draw-rectangle-gradient-ex (%rec x y w (/ h 6.0))
                                        (fade (list 255 0 0 255) *gui-alpha*) (fade (list 255 255 0 255) *gui-alpha*)
                                        (fade (list 255 255 0 255) *gui-alpha*) (fade (list 255 0 0 255) *gui-alpha*))
            (bar 1 (list 255 255 0 255) (list 0 255 0 255))
            (bar 2 (list 0 255 0 255) (list 0 255 255 255))
            (bar 3 (list 0 255 255 255) (list 0 0 255 255))
            (bar 4 (list 0 0 255 255) (list 255 0 255 255))
            (bar 5 (list 255 0 255 255) (list 255 0 0 255))))
        (draw-rectangle-gradient-ex bounds
                                    (fade (fade (gcol +colorpicker+ +base-color-disabled+) 0.1) *gui-alpha*) (fade (gcol +colorpicker+ +border-color-disabled+) *gui-alpha*)
                                    (fade (gcol +colorpicker+ +border-color-disabled+) *gui-alpha*) (fade (fade (gcol +colorpicker+ +base-color-disabled+) 0.1) *gui-alpha*)))

    (%gui-draw-rectangle bounds (gs +colorpicker+ +border-width+) (gcol +colorpicker+ (+ +border+ (* state 3))) +blank+)

    ;; Draw hue bar: selector
    (%gui-draw-rectangle selector 0 +blank+ (gcol +colorpicker+ (+ +border+ (* state 3))))
    ;;--------------------------------------------------------------------

    (values result hue)))

;; Color Picker control
;; NOTE: It's divided in multiple controls:
;;      Color GuiColorPanel(Rectangle bounds, Color color)
;;      float GuiColorBarAlpha(Rectangle bounds, float alpha)
;;      float GuiColorBarHue(Rectangle bounds, float value)
;; NOTE: bounds define GuiColorPanel() size
;; NOTE: this picker converts RGB to HSV, which can cause the Hue control to jump. If you have this problem, consider using the HSV variant instead
;; Returns (values result color)
(defun gui-color-picker (bounds text color)
  (declare (ignore text))
  (let ((result +result-none+)
        (color (or color (list 200 0 0 255))))

    (multiple-value-setq (result color) (gui-color-panel bounds nil color))

    (let* ((bounds-hue (%rec (+ (rectangle-x bounds) (rectangle-width bounds) (gs +colorpicker+ +huebar-padding+)) (rectangle-y bounds)
                             (float (gs +colorpicker+ +huebar-width+)) (rectangle-height bounds)))
           ;;Rectangle boundsAlpha = { bounds.x, bounds.y + bounds.height + GuiGetStyle(COLORPICKER, BARS_PADDING), bounds.width, GuiGetStyle(COLORPICKER, BARS_THICK) };

           ;; NOTE: this conversion can cause low hue-resolution, if the r, g and b value are very similar, which causes the hue bar to shift around when only the GuiColorPanel is used
           (hsv (convert-rgb-to-hsv (vec3 (/ (first color) 255.0) (/ (second color) 255.0) (/ (third color) 255.0)))))

      (multiple-value-bind (hue-result hue) (gui-color-bar-hue bounds-hue nil (vx hsv))
        (setf result hue-result
              (vx hsv) hue))

      ;;color.a = (unsigned char)(GuiColorBarAlpha(boundsAlpha, (float)color.a/255.0f)*255.0f);
      (let ((rgb (convert-hsv-to-rgb hsv)))
        (setf color (list (%u8 (%roundf (* (vx rgb) 255.0))) (%u8 (%roundf (* (vy rgb) 255.0))) (%u8 (%roundf (* (vz rgb) 255.0))) (fourth color)))))

    (values result color)))

;; Color Picker control that avoids conversion to RGB and back to HSV on each call, thus avoiding jittering
;; The user can call ConvertHSVtoRGB() to convert *colorHsv value to RGB
;; NOTE: It's divided in multiple controls:
;;      int GuiColorPanelHSV(Rectangle bounds, const char *text, Vector3 *colorHsv)
;;      int GuiColorBarAlpha(Rectangle bounds, const char *text, float *alpha)
;;      float GuiColorBarHue(Rectangle bounds, float value)
;; NOTE: bounds define GuiColorPanelHSV() size
;; Returns (values result color-hsv)
(defun gui-color-picker-hsv (bounds text color-hsv)
  (declare (ignore text))
  (let ((result +result-none+)
        (color-hsv (if color-hsv (vcopy color-hsv) (convert-rgb-to-hsv (vec3 (/ 200.0 255.0) 0.0 0.0)))))

    (multiple-value-setq (result color-hsv) (gui-color-panel-hsv bounds nil color-hsv))

    (let ((bounds-hue (%rec (+ (rectangle-x bounds) (rectangle-width bounds) (gs +colorpicker+ +huebar-padding+)) (rectangle-y bounds)
                            (float (gs +colorpicker+ +huebar-width+)) (rectangle-height bounds))))

      (when (= result +result-none+)
        (multiple-value-bind (hue-result hue) (gui-color-bar-hue bounds-hue nil (vx color-hsv))
          (setf result hue-result
                (vx color-hsv) hue))))

    (values result color-hsv)))

;; Color Panel control - HSV variant
;; Returns (values result color-hsv)
(defun gui-color-panel-hsv (bounds text color-hsv)
  (declare (ignore text))
  (let* ((result +result-none+)
         (state *gui-state*)
         (color-hsv (vcopy color-hsv))
         (prev-color-hsv (vcopy color-hsv))
         (picker-selector (vec2 0.0 0.0))

         (col-white (list 255 255 255 255))
         (col-black (list 0 0 0 255)))

    (setf (vx picker-selector) (+ (rectangle-x bounds) (* (vy color-hsv) (rectangle-width bounds))) ; HSV: Saturation
          (vy picker-selector) (+ (rectangle-y bounds) (* (- 1.0 (vz color-hsv)) (rectangle-height bounds)))) ; HSV: Value

    (let* ((max-hue (vec3 (vx color-hsv) 1.0 1.0))
           (rgb-hue (convert-hsv-to-rgb max-hue))
           (max-hue-col (list (%u8 (* 255.0 (vx rgb-hue))) (%u8 (* 255.0 (vy rgb-hue))) (%u8 (* 255.0 (vz rgb-hue))) 255)))

      ;; Update control
      ;;--------------------------------------------------------------------
      (when (and (/= state +state-disabled+) (not *gui-locked*))
        (let ((mouse-point (gui-pointer-position)))
          (flet ((pick ()
                   ;; Calculate color from picker
                   (let ((color-pick (vec2 (- (vx picker-selector) (rectangle-x bounds)) (- (vy picker-selector) (rectangle-y bounds)))))
                     (setf (vx color-pick) (/ (vx color-pick) (rectangle-width bounds))) ; Get normalized value on x
                     (setf (vy color-pick) (/ (vy color-pick) (rectangle-height bounds))) ; Get normalized value on y

                     (setf (vy color-hsv) (vx color-pick)
                           (vz color-hsv) (- 1.0 (vy color-pick))))))
            (cond (*gui-control-exclusive-mode* ; Allows to keep dragging outside of bounds
                   (if (gui-button-down-p)
                       (when (check-bounds-id bounds *gui-control-exclusive-rec*)
                         (setf picker-selector (vcopy mouse-point))

                         (when (< (vx picker-selector) (rectangle-x bounds)) (setf (vx picker-selector) (rectangle-x bounds)))
                         (when (> (vx picker-selector) (+ (rectangle-x bounds) (rectangle-width bounds))) (setf (vx picker-selector) (+ (rectangle-x bounds) (rectangle-width bounds))))
                         (when (< (vy picker-selector) (rectangle-y bounds)) (setf (vy picker-selector) (rectangle-y bounds)))
                         (when (> (vy picker-selector) (+ (rectangle-y bounds) (rectangle-height bounds))) (setf (vy picker-selector) (+ (rectangle-y bounds) (rectangle-height bounds))))

                         (pick))
                       (setf *gui-control-exclusive-mode* nil
                             *gui-control-exclusive-rec* (%rec 0 0 0 0))))
                  ((check-collision-point-rec mouse-point bounds)
                   (if (gui-button-down-p)
                       (progn
                         (setf state +state-pressed+
                               *gui-control-exclusive-mode* t
                               *gui-control-exclusive-rec* (%rec-copy bounds)
                               picker-selector (vcopy mouse-point))

                         (pick))
                       (setf state +state-focused+)))))))

      (when (or (/= (vx prev-color-hsv) (vx color-hsv))
                (/= (vy prev-color-hsv) (vy color-hsv))
                (/= (vz prev-color-hsv) (vz color-hsv)))
        (setf result +result-changed+))
      ;;--------------------------------------------------------------------

      ;; Draw control
      ;;--------------------------------------------------------------------
      (if (/= state +state-disabled+)
          (progn
            (draw-rectangle-gradient-ex bounds (fade col-white *gui-alpha*) (fade col-white *gui-alpha*) (fade max-hue-col *gui-alpha*) (fade max-hue-col *gui-alpha*))
            (draw-rectangle-gradient-ex bounds (fade col-black 0) (fade col-black *gui-alpha*) (fade col-black *gui-alpha*) (fade col-black 0))

            ;; Draw color picker: selector
            (let ((selector (%rec (- (vx picker-selector) (truncate (gs +colorpicker+ +color-selector-size+) 2))
                                  (- (vy picker-selector) (truncate (gs +colorpicker+ +color-selector-size+) 2))
                                  (float (gs +colorpicker+ +color-selector-size+)) (float (gs +colorpicker+ +color-selector-size+)))))
              (%gui-draw-rectangle selector 0 +blank+ col-white)))
          (draw-rectangle-gradient-ex bounds (fade (fade (gcol +colorpicker+ +base-color-disabled+) 0.1) *gui-alpha*) (fade (fade col-black 0.6) *gui-alpha*)
                                      (fade (fade col-black 0.6) *gui-alpha*) (fade (fade (gcol +colorpicker+ +border-color-disabled+) 0.6) *gui-alpha*)))

      (%gui-draw-rectangle bounds (gs +colorpicker+ +border-width+) (gcol +colorpicker+ (+ +border+ (* state 3))) +blank+))
    ;;--------------------------------------------------------------------

    (values result color-hsv)))

;; Message Box control
;; NOTE: Button pressed is returned through btnActive parameter, 0 for window close button
;; Returns (values result btn-active)
(defun gui-message-box (bounds title message btn-text &optional btn-active)
  (let* ((result +result-none+)
         (btn-text-list (%gui-text-split (%cstr btn-text) (char-code #\;)))
         (button-count (length btn-text-list))
         (button-bounds (make-rectangle))
         (text-bounds (make-rectangle)))
    (setf (rectangle-x button-bounds) (+ (rectangle-x bounds) +raygui-messagebox-button-padding+)
          (rectangle-y button-bounds) (- (+ (rectangle-y bounds) (rectangle-height bounds)) +raygui-messagebox-button-height+ +raygui-messagebox-button-padding+)
          (rectangle-width button-bounds) (/ (- (rectangle-width bounds) (* +raygui-messagebox-button-padding+ (+ button-count 1))) button-count)
          (rectangle-height button-bounds) (float +raygui-messagebox-button-height+))

    ;;int textWidth = GuiGetTextWidth(message) + 2;

    (setf (rectangle-x text-bounds) (+ (rectangle-x bounds) +raygui-messagebox-button-padding+)
          (rectangle-y text-bounds) (+ (rectangle-y bounds) +raygui-windowbox-statusbar-height+ +raygui-messagebox-button-padding+)
          (rectangle-width text-bounds) (- (rectangle-width bounds) (* +raygui-messagebox-button-padding+ 2))
          (rectangle-height text-bounds) (- (rectangle-height bounds) +raygui-windowbox-statusbar-height+ (* 3 +raygui-messagebox-button-padding+) +raygui-messagebox-button-height+))

    ;; Draw control
    ;;--------------------------------------------------------------------
    (when (= (gui-window-box bounds title) +result-pressed+)
      (setf btn-active 0
            result +result-pressed+))

    (let ((prev-text-alignment (gs +label+ +text-alignment+)))
      (gui-set-style +label+ +text-alignment+ +text-align-center+)
      (gui-label text-bounds message)
      (gui-set-style +label+ +text-alignment+ prev-text-alignment))

    (let ((prev-text-alignment (gs +button+ +text-alignment+)))
      (gui-set-style +button+ +text-alignment+ +text-align-center+)

      (dotimes (i button-count)
        (when (/= (gui-button button-bounds (aref btn-text-list i)) 0)
          (setf btn-active (+ i 1)
                result +result-pressed+))

        (incf (rectangle-x button-bounds) (+ (rectangle-width button-bounds) +raygui-messagebox-button-padding+)))

      (gui-set-style +button+ +text-alignment+ prev-text-alignment))
    ;;--------------------------------------------------------------------

    (values result btn-active)))

;; Used to enable text edit mode
;; WARNING: No more than one GuiTextInputBox() should be open at the same time
(defvar *text-input-box-edit-mode* nil)

;; Text Input Box control
;; NOTE: Button pressed is returned through btnActive parameter, 0 for window close button
;; Returns (values result text btn-active secret-view-active)
(defun gui-text-input-box (bounds title message text text-size btn-text btn-active &optional secret-view-active)
  (let* ((result +result-none+)
         (message (%cstr message))
         (text-buffer (let* ((octets (babel:string-to-octets (or text "") :encoding :utf-8))
                             (buffer (make-array (max text-size (1+ (length octets))) :element-type '(unsigned-byte 8) :initial-element 0)))
                        (replace buffer octets)))

         (btn-text-list (%gui-text-split (%cstr btn-text) (char-code #\;)))
         (button-count (length btn-text-list))
         (button-bounds (make-rectangle))
         (message-input-height 0)
         (text-bounds (make-rectangle))
         (text-box-bounds (make-rectangle)))
    (setf (rectangle-x button-bounds) (+ (rectangle-x bounds) +raygui-textinputbox-button-padding+)
          (rectangle-y button-bounds) (- (+ (rectangle-y bounds) (rectangle-height bounds)) +raygui-textinputbox-button-height+ +raygui-textinputbox-button-padding+)
          (rectangle-width button-bounds) (/ (- (rectangle-width bounds) (* +raygui-textinputbox-button-padding+ (+ button-count 1))) button-count)
          (rectangle-height button-bounds) (float +raygui-textinputbox-button-height+))

    (setf message-input-height (- (truncate (rectangle-height bounds)) +raygui-windowbox-statusbar-height+ (gs +statusbar+ +border-width+)
                                  +raygui-textinputbox-button-height+ (* 2 +raygui-textinputbox-button-padding+)))

    (when message
      (let ((message-text-size (+ (gui-get-text-width message) 2)))
        (setf (rectangle-x text-bounds) (- (+ (rectangle-x bounds) (/ (rectangle-width bounds) 2)) (truncate message-text-size 2))
              (rectangle-y text-bounds) (- (+ (rectangle-y bounds) +raygui-windowbox-statusbar-height+ (truncate message-input-height 4))
                                           (/ (float (gs +default+ +text-size+)) 2))
              (rectangle-width text-bounds) (float message-text-size)
              (rectangle-height text-bounds) (float (gs +default+ +text-size+)))))

    (setf (rectangle-x text-box-bounds) (+ (rectangle-x bounds) +raygui-textinputbox-button-padding+)
          (rectangle-y text-box-bounds) (- (+ (rectangle-y bounds) +raygui-windowbox-statusbar-height+) (truncate +raygui-textinputbox-height+ 2)))
    (if (null message)
        (setf (rectangle-y text-box-bounds) (+ (rectangle-y bounds) 24 +raygui-textinputbox-button-padding+))
        (incf (rectangle-y text-box-bounds) (+ (truncate message-input-height 2) (truncate message-input-height 4))))
    (setf (rectangle-width text-box-bounds) (- (rectangle-width bounds) (* +raygui-textinputbox-button-padding+ 2))
          (rectangle-height text-box-bounds) (float +raygui-textinputbox-height+))

    ;; Draw control
    ;;--------------------------------------------------------------------
    (when (= (gui-window-box bounds title) +result-pressed+)
      (setf btn-active 0
            result +result-pressed+))

    ;; Draw message if available
    (when message
      (let ((prev-text-alignment (gs +label+ +text-alignment+)))
        (gui-set-style +label+ +text-alignment+ +text-align-center+)
        (gui-label text-bounds message)
        (gui-set-style +label+ +text-alignment+ prev-text-alignment)))

    (let ((prev-text-box-alignment (gs +textbox+ +text-alignment+)))
      (gui-set-style +textbox+ +text-alignment+ +text-align-left+)

      (if secret-view-active
          (let ((stars (%cstr "****************")))
            (when (= (%gui-text-box (%rec (rectangle-x text-box-bounds) (rectangle-y text-box-bounds)
                                          (- (rectangle-width text-box-bounds) 4 +raygui-textinputbox-height+) (rectangle-height text-box-bounds))
                                    (if (or (eql secret-view-active t) *text-input-box-edit-mode*) text-buffer stars)
                                    text-size *text-input-box-edit-mode*)
                     +result-pressed+)
              (setf *text-input-box-edit-mode* (not *text-input-box-edit-mode*)))

            (setf secret-view-active
                  (nth-value 1 (gui-toggle (%rec (- (+ (rectangle-x text-box-bounds) (rectangle-width text-box-bounds)) +raygui-textinputbox-height+)
                                                 (rectangle-y text-box-bounds) +raygui-textinputbox-height+ +raygui-textinputbox-height+)
                                           (if (eql secret-view-active t) (gui-icon-text +icon-eye-on+ nil) (gui-icon-text +icon-eye-off+ nil))
                                           (eql secret-view-active t)))))
          (when (= (%gui-text-box text-box-bounds text-buffer text-size *text-input-box-edit-mode*) +result-pressed+)
            (setf *text-input-box-edit-mode* (not *text-input-box-edit-mode*))))

      (gui-set-style +textbox+ +text-alignment+ prev-text-box-alignment))

    (let ((prev-btn-text-alignment (gs +button+ +text-alignment+)))
      (gui-set-style +button+ +text-alignment+ +text-align-center+)

      (dotimes (i button-count)
        (when (/= (gui-button button-bounds (aref btn-text-list i)) 0)
          (setf btn-active (+ i 1)
                result +result-pressed+))

        (incf (rectangle-x button-bounds) (+ (rectangle-width button-bounds) +raygui-messagebox-button-padding+)))

      (when (= result +result-pressed+) (setf *text-input-box-edit-mode* nil))

      (gui-set-style +button+ +text-alignment+ prev-btn-text-alignment))
    ;;--------------------------------------------------------------------

    (values result (%lisp-string text-buffer) btn-active secret-view-active)))

;; Grid control
;; NOTE: Returns grid mouse-hover selected cell
;; About drawing lines at subpixel spacing, simple put, not easy solution:
;; REF: https://stackoverflow.com/questions/4435450/2d-opengl-drawing-lines-that-dont-exactly-fit-pixel-raster
;; Returns (values result mouse-cell)
(defun gui-grid (bounds text spacing subdivs &optional mouse-cell)
  (declare (ignore text mouse-cell))
  (let* ((result +result-none+)
         (state *gui-state*)
         (spacing (float spacing 1.0))

         (mouse-point (gui-pointer-position))
         (current-mouse-cell (vec2 -1.0 -1.0))

         (space-width (/ spacing (float subdivs)))
         (lines-v (+ (truncate (/ (rectangle-width bounds) space-width)) 1))
         (lines-h (+ (truncate (/ (rectangle-height bounds) space-width)) 1))

         (color (gs +default+ +line-color+)))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*) (not *gui-control-exclusive-mode*))
      (when (check-collision-point-rec mouse-point bounds)
        ;; NOTE: Cell values must be the upper left of the cell the mouse is in
        (setf (vx current-mouse-cell) (ffloor (/ (- (vx mouse-point) (rectangle-x bounds)) spacing))
              (vy current-mouse-cell) (ffloor (/ (- (vy mouse-point) (rectangle-y bounds)) spacing)))
        (setf result +result-pressed+)))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (when (= state +state-disabled+) (setf color (gs +default+ +border-color-disabled+)))

    (when (> subdivs 0)
      ;; Draw vertical grid lines
      (dotimes (i lines-v)
        (let ((line-v (%rec (+ (rectangle-x bounds) (/ (* spacing i) subdivs)) (rectangle-y bounds) 1 (+ (rectangle-height bounds) 1))))
          (%gui-draw-rectangle line-v 0 +blank+ (if (= (mod i subdivs) 0)
                                                    (%gui-fade (get-color color) (* +raygui-grid-alpha+ 4))
                                                    (%gui-fade (get-color color) +raygui-grid-alpha+)))))

      ;; Draw horizontal grid lines
      (dotimes (i lines-h)
        (let ((line-h (%rec (rectangle-x bounds) (+ (rectangle-y bounds) (/ (* spacing i) subdivs)) (+ (rectangle-width bounds) 1) 1)))
          (%gui-draw-rectangle line-h 0 +blank+ (if (= (mod i subdivs) 0)
                                                    (%gui-fade (get-color color) (* +raygui-grid-alpha+ 4))
                                                    (%gui-fade (get-color color) +raygui-grid-alpha+))))))

    (values result current-mouse-cell)))

;;----------------------------------------------------------------------------------
;; Tooltip management functions
;; NOTE: Tooltips requires some global variables: tooltipPtr
;;----------------------------------------------------------------------------------
;; Enable gui tooltips (global state)
(defun gui-enable-tooltip () (setf *gui-tooltip* t) nil)

;; Disable gui tooltips (global state)
(defun gui-disable-tooltip () (setf *gui-tooltip* nil) nil)

;; Set tooltip string
(defun gui-set-tooltip (tooltip) (setf *gui-tooltip-ptr* tooltip) nil)

;;----------------------------------------------------------------------------------
;; Styles loading functions
;;----------------------------------------------------------------------------------

;; Load raygui style file (.rgs)
;; NOTE: By default a binary file is expected, that file could contain a custom font,
;; in that case, custom font image atlas is GRAY+ALPHA and pixel data can be compressed (DEFLATE)
(defun gui-load-style (file-name)
  (let ((try-binary nil))
    (unless *gui-style-loaded* (gui-load-style-default))

    ;; Try reading the files as text file first
    (with-open-file (rgs-file file-name :direction :input :if-does-not-exist nil
                                        :external-format '(:utf-8 :replacement #\?))
      (when rgs-file
        (let ((buffer (or (read-line rgs-file nil) "")))
          (if (and (> (length buffer) 0) (char= (char buffer 0) #\#))
              (let ((version 0))
                (loop
                  (when (> (length buffer) 0)
                    (case (char buffer 0)
                      (#\v (let ((value (%scan-integer buffer 1)))
                             (when value (setf version value))))
                      (#\p
                       ;; Style property: p <control_id> <property_id> <property_value> <property_name>
                       (multiple-value-bind (control-id pos) (%scan-integer buffer 1)
                         (multiple-value-bind (property-id pos) (%scan-integer buffer pos)
                           (let* ((hex-start (search "0x" buffer :start2 pos))
                                  (property-value (when hex-start (parse-integer buffer :start (+ hex-start 2) :radix 16 :junk-allowed t))))
                             (gui-set-style (or control-id 0) (or property-id 0) (or property-value 0))))))
                      (#\f
                       ;; Style font: f <gen_font_size> <font_file> <charmap_file>
                       (multiple-value-bind (font-size pos) (%scan-integer buffer 1)
                         (let* ((words (uiop:split-string (string-trim '(#\Space #\Tab #\Return) (subseq buffer pos))
                                                          :separator '(#\Space #\Tab)))
                                (first-word (or (first words) ""))
                                (rest-text (string-right-trim '(#\Return) (format nil "~{~a~^ ~}" (rest words))))
                                (font-file-name (if (>= version 600) first-word rest-text))
                                (charmap-file-name (if (>= version 600) rest-text first-word))
                                (font nil)
                                (codepoints nil)
                                (codepoint-count 0))
                           (setf font-file-name (subseq font-file-name 0 (min 31 (length font-file-name)))
                                 charmap-file-name (subseq charmap-file-name 0 (min 31 (length charmap-file-name))))

                           ;; GLOBAL: Copy font file name into guiFontName
                           (setf *gui-font-name* font-file-name)

                           (when (and (> (length charmap-file-name) 0) (char/= (char charmap-file-name 0) #\0))
                             ;; Load text data from file
                             ;; NOTE: Expected an UTF-8 array of codepoints, no separation
                             (let ((text-data (load-file-text (text-format "%s/%s" (get-directory-path file-name) charmap-file-name))))
                               (multiple-value-setq (codepoints codepoint-count) (load-codepoints text-data))))

                           (when (> (length font-file-name) 0)
                             (if (> codepoint-count 0)
                                 (setf font (load-font-ex (text-format "%s/%s" (get-directory-path file-name) font-file-name) (or font-size 0) codepoints codepoint-count))
                                 (setf font (load-font-ex (text-format "%s/%s" (get-directory-path file-name) font-file-name) (or font-size 0) nil 0)))) ; Default to 95 standard codepoints

                           ;; If font texture not properly loaded, revert to default font and size/spacing
                           (when (= (%font-texture-id font) 0)
                             (setf font (get-font-default))
                             (gui-set-style +default+ +text-size+ 10)
                             (gui-set-style +default+ +text-spacing+ 1))

                           (when (and (> (%font-texture-id font) 0) (> (font-glyph-count font) 0)) (gui-set-font font)))))))
                  (setf buffer (read-line rgs-file nil))
                  (unless buffer (return))))
              (setf try-binary t)))))

    (when try-binary
      (multiple-value-bind (file-data file-data-size) (load-file-data file-name)
        (when (and file-data (> file-data-size 0))
          (gui-load-style-from-memory file-data file-data-size)))))
  nil)

(defun %scan-integer (string start)
  "sscanf() %d helper: skips blanks and reads an integer, returns (values integer end)"
  (let ((pos (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) string :start (min start (length string))) (length string))))
    (multiple-value-bind (value end) (parse-integer string :start pos :junk-allowed t)
      (values value end))))

(defun %s16 (data offset) (let ((u (logior (aref data offset) (ash (aref data (+ offset 1)) 8)))) (if (>= u #x8000) (- u #x10000) u)))
(defun %u32 (data offset) (logior (aref data offset) (ash (aref data (+ offset 1)) 8) (ash (aref data (+ offset 2)) 16) (ash (aref data (+ offset 3)) 24)))
(defun %s32 (data offset) (let ((u (%u32 data offset))) (if (>= u #x80000000) (- u #x100000000) u)))
(defun %f32 (data offset) (sb-kernel:make-single-float (%s32 data offset)))

;; Load style from memory
;; WARNING: Binary files only
(defun gui-load-style-from-memory (file-data data-size)
  (declare (ignore data-size))
  ;; Style File Structure (.rgs)
  ;; ------------------------------------------------------
  ;; Offset  | Size    | Type       | Description
  ;; ------------------------------------------------------
  ;; 0       | 4       | char       | Signature: "rGS "
  ;; 4       | 2       | short      | Version: 200, 400, 600
  ;; 6       | 2       | short      | reserved
  ;; 8       | 4       | int        | Num properties (only changed ones from default style)

  ;; Properties Data (8 bytes per property)
  ;; WARNING: Only properties required that differ from default (light) internal style
  ;; foreach (property)
  ;; {
  ;;   8+8*i  | 2       | short      | ControlId
  ;;   8+8*i  | 2       | short      | PropertyId
  ;;   8+8*i  | 4       | int        | PropertyValue
  ;; }

  ;; Custom Font Data : Parameters (64 bytes)
  ;; ...     | 4       | int        | Font data size (0 - no font, no more fields added!)
  ;; ...     | 32      | char       | Font filename (with extension) - VERSION: >=600
  ;; ...     | 4       | int        | Font base size
  ;; ...     | 4       | int        | Font glyph count [glyphCount]
  ;; ...     | 4       | int        | Font type (0-NORMAL, 1-SDF)
  ;; ...     | 16      | Rectangle  | Font white rectangle

  ;; Custom Font Data : Image (20 bytes + imData)
  ;; NOTE: Font image atlas is always converted to GRAY+ALPHA
  ;; and atlas image data can be compressed (DEFLATE)
  ;; ...     | 4       | int        | Image data size (uncompressed)
  ;; ...     | 4       | int        | Image data size (compressed)
  ;; ...     | 4       | int        | Image width
  ;; ...     | 4       | int        | Image height
  ;; ...     | 4       | int        | Image format: GRAY+ALPHA (expected)
  ;; ...     | imSize  | byte       | Image data (comp or uncomp)

  ;; Custom Font Data : Recs (32 bytes*glyphCount)
  ;; NOTE: Font recs data can be compressed (DEFLATE)
  ;; ...     | 4       | int        | Recs data compressed size (0 - not compressed, 1-compressed) - VERSION: >=400
  ;; ...     | 16*N    | Rectangle  | Glyph rectangles (in image), or compressed data

  ;; Custom Font Data : Glyph Info (32 bytes*glyphCount)
  ;; NOTE: Font glyphs info data can be compressed (DEFLATE)
  ;; ...     | 4       | int        | Glyphs data compressed size (0 - not compressed) - VERSION: >=400
  ;; ...     | 16*N    | int[4]     | Glyph value, offset X, offset Y, advance X, or compressed data
  ;; ------------------------------------------------------
  (let* ((ptr 0)
         (signature (map 'string #'code-char (subseq file-data 0 4)))
         (version (%s16 file-data 4))
         ;;(reserved (%s16 file-data 6))
         (property-count (%s32 file-data 8)))
    (setf ptr 12)

    (when (string= signature "rGS ")
      (dotimes (i property-count)
        (let ((control-id (%s16 file-data ptr))
              (property-id (%s16 file-data (+ ptr 2)))
              (property-value (%u32 file-data (+ ptr 4))))
          (incf ptr 8)

          (if (= control-id 0)        ; DEFAULT control
              (progn
                ;; If a DEFAULT property is loaded, it is propagated to all controls
                ;; NOTE: All DEFAULT properties should be defined first in the file
                (gui-set-style 0 property-id property-value)

                (when (< property-id +raygui-max-props-base+)
                  (loop for j from 1 below +raygui-max-controls+ do (gui-set-style j property-id property-value))))
              (gui-set-style control-id property-id property-value))))

      ;; Load custom font if available
      ;; NOTE: Font texture loading requires raylib
      (let ((font-data-size (%s32 file-data ptr)))
        (incf ptr 4)

        (when (> font-data-size 0)
          (let ((font (make-font))
                (font-type 0))          ; 0-Normal, 1-SDF
            (declare (ignorable font-type))

            ;; WARNING: Version 600 adds 32 bytes for the font filename (with extension)
            (if (>= version 600)
                (progn
                  ;; GLOBAL: Copy font file name into guiFontName
                  (setf *gui-font-name* (%lisp-string (%cstr (subseq file-data ptr (+ ptr 32)))))
                  (incf ptr 32))
                (setf *gui-font-name* ""))

            (setf (font-base-size font) (%s32 file-data ptr)
                  (font-glyph-count font) (%s32 file-data (+ ptr 4))
                  font-type (%s32 file-data (+ ptr 8)))
            (incf ptr 12)

            ;; Load font white rectangle
            (let ((font-white-rec (%rec (%f32 file-data ptr) (%f32 file-data (+ ptr 4)) (%f32 file-data (+ ptr 8)) (%f32 file-data (+ ptr 12))))
                  (font-image-uncomp-size 0)
                  (font-image-comp-size 0)
                  (im-font (make-image :mipmaps 1)))
              (incf ptr 16)

              ;; Load font image parameters
              (setf font-image-uncomp-size (%s32 file-data ptr)
                    font-image-comp-size (%s32 file-data (+ ptr 4)))
              (incf ptr 8)

              (setf (image-width im-font) (%s32 file-data ptr)
                    (image-height im-font) (%s32 file-data (+ ptr 4))
                    (image-format im-font) (%s32 file-data (+ ptr 8)))
              (incf ptr 12)

              (if (and (> font-image-comp-size 0) (/= font-image-comp-size font-image-uncomp-size))
                  ;; Compressed font atlas image data (DEFLATE), it requires DecompressData()
                  (let ((comp-data (subseq file-data ptr (+ ptr font-image-comp-size))))
                    (incf ptr font-image-comp-size)
                    (multiple-value-bind (data data-uncomp-size) (decompress-data comp-data font-image-comp-size)
                      (setf (image-data im-font) data)

                      ;; Security check, dataUncompSize must match the provided fontImageUncompSize
                      (when (/= data-uncomp-size font-image-uncomp-size) (raygui-log "WARNING: Uncompressed font atlas image data could be corrupted"))))
                  ;; Font atlas image data is not compressed
                  (progn
                    (setf (image-data im-font) (subseq file-data ptr (+ ptr font-image-uncomp-size)))
                    (incf ptr font-image-uncomp-size)))

              ;; Load font recs data (glyphs position and size in the image atlas)
              (let ((recs-data-size (* (font-glyph-count font) 16))
                    (recs-data-compressed-size 0)
                    (recs-data nil))

                ;; WARNING: Version 400 adds the compression size parameter
                (when (>= version 400)
                  ;; RGS files version 400 support compressed recs data
                  (setf recs-data-compressed-size (%s32 file-data ptr))
                  (incf ptr 4))

                (if (and (> recs-data-compressed-size 0) (/= recs-data-compressed-size recs-data-size))
                    ;; Recs data is compressed, uncompress it
                    (let ((recs-data-compressed (subseq file-data ptr (+ ptr recs-data-compressed-size))))
                      (incf ptr recs-data-compressed-size)
                      (multiple-value-bind (data recs-data-uncomp-size) (decompress-data recs-data-compressed recs-data-compressed-size)
                        (setf recs-data data)

                        ;; Security check, data uncompressed size must match the expected original data size
                        (when (/= recs-data-uncomp-size recs-data-size) (raygui-log "WARNING: Uncompressed font recs data could be corrupted"))))
                    ;; Recs data is uncompressed
                    (progn
                      (setf recs-data (subseq file-data ptr (+ ptr recs-data-size)))
                      (incf ptr recs-data-size)))

                (setf (font-recs font)
                      (let ((recs (make-array (font-glyph-count font))))
                        (dotimes (i (font-glyph-count font) recs)
                          (setf (aref recs i) (%rec (%f32 recs-data (* i 16)) (%f32 recs-data (+ (* i 16) 4))
                                                    (%f32 recs-data (+ (* i 16) 8)) (%f32 recs-data (+ (* i 16) 12))))))))

              ;; Load font glyphs info data
              (let ((glyphs-data-size (* (font-glyph-count font) 16)) ; 16 bytes data per glyph
                    (glyphs-data-compressed-size 0)
                    (glyphs-data nil))

                ;; WARNING: Version 400 adds the compression size parameter
                (when (>= version 400)
                  ;; RGS files version 400 support compressed glyphs data
                  (setf glyphs-data-compressed-size (%s32 file-data ptr))
                  (incf ptr 4))

                (if (and (> glyphs-data-compressed-size 0) (/= glyphs-data-compressed-size glyphs-data-size))
                    ;; Glyphs data is compressed, uncompress it
                    (let ((glyphs-data-compressed (subseq file-data ptr (+ ptr glyphs-data-compressed-size))))
                      (incf ptr glyphs-data-compressed-size)
                      (multiple-value-bind (data glyphs-data-uncomp-size) (decompress-data glyphs-data-compressed glyphs-data-compressed-size)
                        (setf glyphs-data data)

                        ;; Security check, data uncompressed size must match the expected original data size
                        (when (/= glyphs-data-uncomp-size glyphs-data-size) (raygui-log "WARNING: Uncompressed font glyphs data could be corrupted"))))
                    ;; Glyphs data is uncompressed
                    (progn
                      (setf glyphs-data (subseq file-data ptr (+ ptr glyphs-data-size)))
                      (incf ptr glyphs-data-size)))

                ;; Allocate required glyphs space to fill with data
                (setf (font-glyphs font)
                      (let ((glyphs (make-array (font-glyph-count font))))
                        (dotimes (i (font-glyph-count font) glyphs)
                          (setf (aref glyphs i) (make-glyph-info :value (%s32 glyphs-data (* i 16))
                                                                 :offset-x (%s32 glyphs-data (+ (* i 16) 4))
                                                                 :offset-y (%s32 glyphs-data (+ (* i 16) 8))
                                                                 :advance-x (%s32 glyphs-data (+ (* i 16) 12))))))))

              (when *raygui-font-icons-baking*
                ;; Font atlas image icons baking
                (multiple-value-bind (icon-offset-y updated-white-rec) (%gui-font-icon-baking im-font font)
                  (setf *gui-icon-font-offset-y* icon-offset-y)
                  (when (> *gui-icon-font-offset-y* 0) (setf font-white-rec updated-white-rec))))

              ;; Load texture from image
              (when (/= (%font-texture-id font) (%font-texture-id (get-font-default))) (when (font-texture font) (unload-texture (font-texture font))))
              (setf (font-texture font) (load-texture-from-image im-font))

              ;; Fallback to default raylib texture if font texture loading fails
              (if (/= (%font-texture-id font) 0)
                  ;; Set font texture source rectangle to be used as white texture to draw shapes
                  ;; NOTE: It makes possible to draw shapes and text (full UI) in a single draw call
                  (when (and (> (rectangle-x font-white-rec) 0)
                             (> (rectangle-y font-white-rec) 0)
                             (> (rectangle-width font-white-rec) 0)
                             (> (rectangle-height font-white-rec) 0))
                    (set-shapes-texture (font-texture font) font-white-rec))
                  (setf font (get-font-default)))

              (gui-set-font font)))))))
  nil)

;; Load style default over global style
(defun gui-load-style-default ()
  ;; Setting this flag first to avoid cyclic function calls
  ;; when calling GuiSetStyle() and GuiGetStyle()
  (setf *gui-style-loaded* t)

  ;; Initialize default LIGHT style property values
  ;; WARNING: Default value are applied to all controls on set but
  ;; they can be overwritten later on for every custom control
  (gui-set-style +default+ +border-color-normal+ #x838383ff)
  (gui-set-style +default+ +base-color-normal+ #xc9c9c9ff)
  (gui-set-style +default+ +text-color-normal+ #x686868ff)
  (gui-set-style +default+ +border-color-focused+ #x5bb2d9ff)
  (gui-set-style +default+ +base-color-focused+ #xc9effeff)
  (gui-set-style +default+ +text-color-focused+ #x6c9bbcff)
  (gui-set-style +default+ +border-color-pressed+ #x0492c7ff)
  (gui-set-style +default+ +base-color-pressed+ #x97e8ffff)
  (gui-set-style +default+ +text-color-pressed+ #x368bafff)
  (gui-set-style +default+ +border-color-disabled+ #xb5c1c2ff)
  (gui-set-style +default+ +base-color-disabled+ #xe6e9e9ff)
  (gui-set-style +default+ +text-color-disabled+ #xaeb7b8ff)
  (gui-set-style +default+ +border-width+ 1)
  (gui-set-style +default+ +text-padding+ 0)
  (gui-set-style +default+ +text-alignment+ +text-align-center+)

  ;; Initialize default extended property values
  ;; NOTE: By default, extended property values are initialized to 0
  (gui-set-style +default+ +text-size+ 10)               ; DEFAULT, shared by all controls
  (gui-set-style +default+ +text-spacing+ 1)             ; DEFAULT, shared by all controls
  (gui-set-style +default+ +line-color+ #x90abb5ff)      ; DEFAULT specific property
  (gui-set-style +default+ +background-color+ #xf5f5f5ff) ; DEFAULT specific property
  (gui-set-style +default+ +text-line-spacing+ 12)       ; DEFAULT, pixels between lines, from bottom of first line to top of second
  (gui-set-style +default+ +text-alignment-vertical+ +text-align-middle+) ; DEFAULT, text aligned vertically to middle of text-bounds

  ;; Initialize control-specific property values
  ;; NOTE: Those properties are in default list but require specific values by control type
  (gui-set-style +label+ +text-alignment+ +text-align-left+)
  (gui-set-style +button+ +border-width+ 2)
  (gui-set-style +slider+ +text-padding+ 4)
  (gui-set-style +progressbar+ +text-padding+ 4)
  (gui-set-style +checkbox+ +text-padding+ 4)
  (gui-set-style +checkbox+ +text-alignment+ +text-align-right+)
  (gui-set-style +dropdownbox+ +text-padding+ 0)
  (gui-set-style +dropdownbox+ +text-alignment+ +text-align-center+)
  (gui-set-style +textbox+ +text-padding+ 4)
  (gui-set-style +textbox+ +text-alignment+ +text-align-left+)
  (gui-set-style +valuebox+ +text-padding+ 0)
  (gui-set-style +valuebox+ +text-alignment+ +text-align-left+)
  (gui-set-style +statusbar+ +text-padding+ 8)
  (gui-set-style +statusbar+ +text-alignment+ +text-align-left+)
  (gui-set-style +tabbar+ +tab-items-width+ 160)

  ;; Initialize extended property values
  ;; NOTE: By default, extended property values are initialized to 0
  (gui-set-style +toggle+ +group-padding+ 2)
  (gui-set-style +slider+ +slider-width+ 16)
  (gui-set-style +slider+ +slider-padding+ 1)
  (gui-set-style +progressbar+ +progress-padding+ 1)
  (gui-set-style +checkbox+ +check-padding+ 1)
  (gui-set-style +combobox+ +combo-button-width+ 32)
  (gui-set-style +combobox+ +combo-button-spacing+ 2)
  (gui-set-style +dropdownbox+ +arrow-padding+ 16)
  (gui-set-style +dropdownbox+ +dropdown-items-spacing+ 2)
  (gui-set-style +valuebox+ +spinner-button-width+ 24)
  (gui-set-style +valuebox+ +spinner-button-spacing+ 2)
  (gui-set-style +scrollbar+ +border-width+ 0)
  (gui-set-style +scrollbar+ +arrows-visible+ 0)
  (gui-set-style +scrollbar+ +arrows-size+ 6)
  (gui-set-style +scrollbar+ +scroll-slider-padding+ 0)
  (gui-set-style +scrollbar+ +scroll-slider-size+ 16)
  (gui-set-style +scrollbar+ +scroll-padding+ 0)
  (gui-set-style +scrollbar+ +scroll-speed+ 12)
  (gui-set-style +listview+ +list-items-height+ 28)
  (gui-set-style +listview+ +list-items-spacing+ 2)
  (gui-set-style +listview+ +list-items-border-width+ 1)
  (gui-set-style +listview+ +scrollbar-width+ 12)
  (gui-set-style +listview+ +scrollbar-side+ +scrollbar-right-side+)
  (gui-set-style +colorpicker+ +color-selector-size+ 8)
  (gui-set-style +colorpicker+ +huebar-width+ 16)
  (gui-set-style +colorpicker+ +huebar-padding+ 8)
  (gui-set-style +colorpicker+ +huebar-selector-height+ 8)
  (gui-set-style +colorpicker+ +huebar-selector-overflow+ 2)

  (when (/= (%font-texture-id *gui-font*) (%font-texture-id (get-font-default)))
    ;; Unload previous font texture
    (when *gui-font* (unload-texture (font-texture *gui-font*)))

    ;; Setup default raylib font
    (setf *gui-font* (get-font-default))

    ;; NOTE: Default raylib font character 95 is a white square
    (let ((white-char (aref (font-recs *gui-font*) 95)))
      ;; NOTE: Setting up a 1px padding on char rectangle to avoid pixel bleeding on MSAA filtering
      (set-shapes-texture (font-texture *gui-font*) (%rec (+ (rectangle-x white-char) 1) (+ (rectangle-y white-char) 1)
                                                          (- (rectangle-width white-char) 2) (- (rectangle-height white-char) 2))))

    ;; Reset baked icons offset in font
    (setf *gui-icon-font-offset-y* 0))
  nil)

;; Get text with icon id prepended
;; NOTE: Useful to add icons by name id (enum) instead of
;; a number that can change between ricon versions
(defun gui-icon-text (icon-id text)
  (if text
      (let ((text (if (stringp text) text (%lisp-string text))))
        (concatenate 'string (text-format "#%03i#" icon-id) (subseq text 0 (min (length text) (- 1024 5 1)))))
      (text-format "#%03i#" icon-id)))

;; Get full icons data pointer
(defun gui-get-icons () *gui-icons-ptr*)

;; Load raygui icons file (.rgi)
(defun gui-load-icons (file-name load-icons-name)
  (multiple-value-bind (file-data data-size) (load-file-data file-name)
    (when (and file-data (> data-size 0))
      (gui-load-icons-from-memory file-data data-size load-icons-name))))

;; Load icons from memory
;; GLOBAL: Updates global variable: guiIconsPtr
;; Returns the list of icon names when LOAD-ICONS-NAME
(defun gui-load-icons-from-memory (file-data data-size load-icons-name)
  (declare (ignore data-size))
  ;; Icon File Structure (.rgi)
  ;; ------------------------------------------------------
  ;; Offset  | Size    | Type       | Description
  ;; ------------------------------------------------------
  ;; 0       | 4       | char       | Signature: "rGI "
  ;; 4       | 2       | short      | Version: 100, 500
  ;; 6       | 2       | short      | reserved
  ;; 8       | 2       | short      | Num icons (N)
  ;; 10      | 2       | short      | Icons Size (Options: 16, 32, 64)

  ;; Icons name id (32 bytes per name id)
  ;; foreach (icon)
  ;; {
  ;;   12+32*i  | 32   | char       | Icon NameId
  ;; }

  ;; Icons data: One bit per pixel, stored as unsigned int array (depends on icon size)
  ;; Size*Size pixels/32bit per unsigned int = K unsigned int per icon
  ;; foreach (icon)
  ;; {
  ;;   ...   | K       | unsigned int | Icon Data
  ;; }
  ;; ------------------------------------------------------
  (let* ((ptr 0)
         (gui-icons-name nil)
         (signature (map 'string #'code-char (subseq file-data 0 4)))
         ;;(version (%s16 file-data 4))
         ;;(reserved (%s16 file-data 6))
         (icon-count (%s16 file-data 8))
         (icon-size (%s16 file-data 10)))
    (setf ptr 12)

    (when (string= signature "rGI ")
      (if load-icons-name
          (dotimes (i icon-count)
            (push (%lisp-string (%cstr (subseq file-data ptr (+ ptr +raygui-icon-max-name-length+)))) gui-icons-name)
            (incf ptr +raygui-icon-max-name-length+))
          ;; Skip icon name data if not required
          (incf ptr (* icon-count +raygui-icon-max-name-length+)))

      (let* ((icon-data-count (* icon-count (truncate (* icon-size icon-size) 32)))
             (icons (make-array (max icon-data-count (length *gui-icons*)) :element-type '(unsigned-byte 32) :initial-element 0)))
        (dotimes (i icon-data-count) (setf (aref icons i) (%u32 file-data (+ ptr (* i 4)))))
        (setf *gui-icons-ptr* icons)))

    (nreverse gui-icons-name)))

;; Draw selected icon using rectangles pixel-by-pixel
(defun gui-draw-icon (icon-id pos-x pos-y pixel-size color)
  (if (and (> *gui-icon-font-offset-y* 0) (< icon-id +raygui-icon-max-font-backed+))
      (let* ((max-icons-per-line (truncate (texture-width (font-texture *gui-font*)) (+ +raygui-icon-size+ (* 2 +raygui-icon-font-atlas-padding+))))
             (x (mod icon-id max-icons-per-line))
             (y (truncate icon-id max-icons-per-line))
             (src-rec (%rec (+ (* (float x) (+ +raygui-icon-size+ (* 2 +raygui-icon-font-atlas-padding+))) +raygui-icon-font-atlas-padding+)
                            (+ *gui-icon-font-offset-y* (* (float y) (+ +raygui-icon-size+ (* 2 +raygui-icon-font-atlas-padding+))) +raygui-icon-font-atlas-padding+)
                            +raygui-icon-size+ +raygui-icon-size+))
             (dst-rec (%rec pos-x pos-y (* (float pixel-size) +raygui-icon-size+) (* (float pixel-size) +raygui-icon-size+))))
        (draw-texture-pro (font-texture *gui-font*) src-rec dst-rec (vec2 0.0 0.0) 0.0 color))
      (loop with y = 0
            for i from 0 below (truncate (* +raygui-icon-size+ +raygui-icon-size+) 32)
            do (dotimes (k 32)
                 (when (logbitp k (aref *gui-icons-ptr* (+ (* icon-id +raygui-icon-data-elements+) i)))
                   (%gui-draw-rectangle (%rec (+ (float pos-x) (* (mod k +raygui-icon-size+) pixel-size))
                                              (+ (float pos-y) (* y pixel-size)) (float pixel-size) (float pixel-size))
                                        0 +blank+ color))

                 (when (or (= k 15) (= k 31)) (incf y)))))
  nil)

;; Set icon drawing size
(defun gui-set-icon-scale (scale)
  (when (>= scale 1) (setf *gui-icon-scale* scale))
  nil)

;; Get text width considering gui style and icon size (if required).
;; For multi-line text (containing '\n'), returns the width of the widest line.
(defun gui-get-text-width (text)
  (let ((text (%cstr text)))
    (if text (%gui-get-text-width text 0) 0)))

(defun %gui-get-text-width (text start)
  (when (null text) (return-from %gui-get-text-width 0))

  (let ((max-width 0)
        (line-ptr start))
    (loop while (and (/= (%cref text line-ptr) 0) (< (- line-ptr start) +max-line-buffer-size+))
          do (let ((line-width (%get-line-width text line-ptr)))
               (when (> line-width max-width) (setf max-width line-width))

               ;; Skip to the next '\n' (or end of string/buffer)
               (loop while (and (/= (%cref text line-ptr) 0) (/= (%cref text line-ptr) 10) (< (- line-ptr start) +max-line-buffer-size+))
                     do (incf line-ptr))

               ;; Advance past the '\n' delimiter to the start of the next line
               (when (= (%cref text line-ptr) 10) (incf line-ptr))))

    max-width))

;;----------------------------------------------------------------------------------
;; Module Internal Functions Definition
;;----------------------------------------------------------------------------------
;; Glyph width scaled, glyph advanceX or rec width when advanceX is 0
(defun %glyph-width (index scale-factor)
  (let ((advance-x (glyph-info-advance-x (aref (font-glyphs *gui-font*) index))))
    (if (= advance-x 0)
        (* (float (rectangle-width (aref (font-recs *gui-font*) index))) scale-factor)
        (* (float advance-x) scale-factor))))

;; GetCodepoint() from raylib rtext.c (strict UTF-8 validation), working on UTF-8 octets
(defun %get-codepoint (text pos)
  "Returns (values codepoint codepoint-size)"
  (let ((codepoint #x3f)
        (size 1)
        (octet (%cref text pos)))
    (block decode
      (cond ((<= octet #x7f)
             ;; Only one octet (ASCII range x00-7F)
             (setf codepoint octet))
            ((= (logand octet #xe0) #xc0)
             ;; Two octets
             ;; [0]xC2-DF    [1]UTF8-tail(x80-BF)
             (let ((octet1 (%cref text (+ pos 1))))
               (when (or (= octet1 0) (/= (ash octet1 -6) 2)) (setf size 2) (return-from decode)) ; Unexpected sequence

               (when (<= #xc2 octet #xdf)
                 (setf codepoint (logior (ash (logand octet #x1f) 6) (logand octet1 #x3f))
                       size 2))))
            ((= (logand octet #xf0) #xe0)
             ;; Three octets
             (let ((octet1 (%cref text (+ pos 1)))
                   (octet2 0))
               (when (or (= octet1 0) (/= (ash octet1 -6) 2)) (setf size 2) (return-from decode)) ; Unexpected sequence

               (setf octet2 (%cref text (+ pos 2)))

               (when (or (= octet2 0) (/= (ash octet2 -6) 2)) (setf size 3) (return-from decode)) ; Unexpected sequence

               ;; [0]xE0    [1]xA0-BF       [2]UTF8-tail(x80-BF)
               ;; [0]xE1-EC [1]UTF8-tail    [2]UTF8-tail(x80-BF)
               ;; [0]xED    [1]x80-9F       [2]UTF8-tail(x80-BF)
               ;; [0]xEE-EF [1]UTF8-tail    [2]UTF8-tail(x80-BF)

               (when (or (and (= octet #xe0) (not (<= #xa0 octet1 #xbf)))
                         (and (= octet #xed) (not (<= #x80 octet1 #x9f))))
                 (setf size 2) (return-from decode))

               (when (<= #xe0 octet #xef)
                 (setf codepoint (logior (ash (logand octet #xf) 12) (ash (logand octet1 #x3f) 6) (logand octet2 #x3f))
                       size 3))))
            ((= (logand octet #xf8) #xf0)
             ;; Four octets
             (when (> octet #xf4) (return-from decode))

             (let ((octet1 (%cref text (+ pos 1)))
                   (octet2 0)
                   (octet3 0))
               (when (or (= octet1 0) (/= (ash octet1 -6) 2)) (setf size 2) (return-from decode)) ; Unexpected sequence

               (setf octet2 (%cref text (+ pos 2)))

               (when (or (= octet2 0) (/= (ash octet2 -6) 2)) (setf size 3) (return-from decode)) ; Unexpected sequence

               (setf octet3 (%cref text (+ pos 3)))

               (when (or (= octet3 0) (/= (ash octet3 -6) 2)) (setf size 4) (return-from decode)) ; Unexpected sequence

               ;; [0]xF0       [1]x90-BF       [2]UTF8-tail  [3]UTF8-tail
               ;; [0]xF1-F3    [1]UTF8-tail    [2]UTF8-tail  [3]UTF8-tail
               ;; [0]xF4       [1]x80-8F       [2]UTF8-tail  [3]UTF8-tail

               (when (or (and (= octet #xf0) (not (<= #x90 octet1 #xbf)))
                         (and (= octet #xf4) (not (<= #x80 octet1 #x8f))))
                 (setf size 2) (return-from decode)) ; Unexpected sequence

               (when (>= octet #xf0)
                 (setf codepoint (logior (ash (logand octet #x7) 18) (ash (logand octet1 #x3f) 12) (ash (logand octet2 #x3f) 6) (logand octet3 #x3f))
                       size 4))))))

    (when (> codepoint #x10ffff) (setf codepoint #x3f)) ; Codepoints after U+10ffff are invalid

    (values codepoint size)))

;; Get text line width (stops at '\n' or '\0')
;; NOTE: Considers icon marker '#NNN#'
(defun %get-line-width (text start)
  (let ((text-size-x 0.0)
        (text-icon-offset 0))

    (when (and text (/= (%cref text start) 0))
      ;; Icon marker: '#' + 1..3 digits + '#' (matches GetTextIcon())
      (when (= (%cref text start) (char-code #\#))
        (let ((pos 1))
          (loop while (and (< pos 4) (<= 48 (%cref text (+ start pos)) 57)) do (incf pos))
          (when (= (%cref text (+ start pos)) (char-code #\#)) (setf text-icon-offset (+ pos 1)))))

      (let ((start (+ start text-icon-offset))
            ;; Make sure guiFont is set, GuiGetStyle() initializes it lazynessly
            (font-size (float (gs +default+ +text-size+))))

        ;; Custom MeasureText() implementation -- single line only
        (when (> (%font-texture-id *gui-font*) 0)
          ;; Get size in bytes of the line, considering end of line and line break
          (let ((size 0))
            (dotimes (i +max-line-buffer-size+)
              (if (and (/= (%cref text (+ start i)) 0) (/= (%cref text (+ start i)) 10))
                  (incf size)
                  (return)))

            (let ((scale-factor (/ font-size (float (font-base-size *gui-font*)))))
              (loop with i = 0
                    while (< i size)
                    do (multiple-value-bind (codepoint codepoint-size) (%get-codepoint-next text (+ start i))
                         (let ((codepoint-index (get-glyph-index *gui-font* codepoint)))
                           (incf text-size-x (+ (%glyph-width codepoint-index scale-factor) (float (gs +default+ +text-spacing+)))))
                         (incf i codepoint-size)))))))

      (when (> text-icon-offset 0) (incf text-size-x (+ +raygui-icon-size+ +raygui-icon-text-padding+))))

    (truncate text-size-x)))

;; Get text bounds considering control bounds
(defun %get-text-bounds (control bounds)
  (let ((text-bounds (%rec-copy bounds)))
    (setf (rectangle-x text-bounds) (+ (rectangle-x bounds) (gs control +border-width+))
          (rectangle-y text-bounds) (+ (rectangle-y bounds) (gs control +border-width+) (gs control +text-padding+))
          (rectangle-width text-bounds) (- (rectangle-width bounds) (* 2 (gs control +border-width+)) (* 2 (gs control +text-padding+)))
          (rectangle-height text-bounds) (- (rectangle-height bounds) (* 2 (gs control +border-width+)) (* 2 (gs control +text-padding+)))) ; NOTE: Text is processed line per line!

    ;; Depending on control, TEXT_PADDING and TEXT_ALIGNMENT properties could affect the text-bounds
    ;; NOTE: COMBOBOX, DROPDOWNBOX, LISTVIEW, SLIDER, CHECKBOX, VALUEBOX, TABBAR are special cases, all use default
    ;; WARNING: TEXT_ALIGNMENT is already considered in GuiDrawText()
    (if (= (gs control +text-alignment+) +text-align-right+)
        (decf (rectangle-x text-bounds) (gs control +text-padding+))
        (incf (rectangle-x text-bounds) (gs control +text-padding+)))

    text-bounds))

;; Get text icon if provided and move text cursor
;; NOTE: Up to RAYGUI_ICON_MAX_ICONS supported for iconId
;; Returns (values text-start icon-id)
(defun %get-text-icon (text start)
  (let ((icon-id -1))
    (when (= (%cref text start) (char-code #\#)) ; Maybe an icon, if it starts with # but an ending # must be found
      (let ((icon-value (make-string 3 :initial-element #\Nul)) ; Maximum length for icon value: 3 digits + '\0'
            (pos 1))
        (loop while (and (< pos 4) (<= 48 (%cref text (+ start pos)) 57))
              do (setf (char icon-value (1- pos)) (code-char (%cref text (+ start pos))))
                 (incf pos))

        (when (= (%cref text (+ start pos)) (char-code #\#))
          (let ((raw-icon-id (text-to-integer (string-right-trim '(#\Nul) icon-value))))
            (when (< raw-icon-id +raygui-icon-max-icons+)
              (setf icon-id raw-icon-id)

              ;; Move text pointer after icon
              ;; WARNING: If only icon provided, it could point to EOL character: '\0'
              (when (>= icon-id 0) (incf start (+ pos 1))))))))

    (values start icon-id)))

;; Get text divided into lines (by line-breaks '\n')
;; WARNING: It returns pointers to new lines but it does not add NULL ('\0') terminator!
;; Returns the vector of line start offsets
(defun %get-text-lines (text start)
  (let ((lines (make-array +raygui-max-text-lines+ :fill-pointer 0))
        (text-length (%strlen text start)))
    (vector-push start lines)

    (loop for i from 0
          while (and (< i text-length) (< (length lines) +raygui-max-text-lines+))
          do (when (and (= (%cref text (+ start i)) 10) (< (+ i 1) text-length))
               (vector-push (+ start i 1) lines)))

    lines))

;; Get text width to next space for provided string
;; Returns (values width next-space-index)
(defun %get-next-space-width (text start)
  (let ((width 0.0)
        (next-space-index 0)
        (scale-factor (/ (float (gs +default+ +text-size+)) (font-base-size *gui-font*))))

    (loop for i from 0
          until (= (%cref text (+ start i)) 0)
          do (if (/= (%cref text (+ start i)) (char-code #\Space))
                 (let* ((codepoint (%get-codepoint text (+ start i)))
                        (index (get-glyph-index *gui-font* codepoint)))
                   (incf width (+ (%glyph-width index scale-factor) (float (gs +default+ +text-spacing+)))))
                 (progn
                   (setf next-space-index i)
                   (return))))

    (values width next-space-index)))

;; Gui draw text using default font
(defun %gui-draw-text (text start text-bounds alignment tint)
  (flet ((text-valign-pixel-offset (h) (rem (truncate h) 2))) ; Vertical alignment for pixel perfect
    (when (or (null text) (= (%cref text start) 0)) (return-from %gui-draw-text)) ; Security check

    ;; PROCEDURE:
    ;;   - Text is processed line per line
    ;;   - For every line, horizontal alignment is defined
    ;;   - For all text, vertical alignment is defined (multiline text only)
    ;;   - For every line, wordwrap mode is checked (useful for GuitextBox(), read-only)

    ;; Get text lines (using '\n' as delimiter) to be processed individually
    ;; WARNING: GuiTextSplit() function can't be used now because it can have already been used
    ;; before the GuiDrawText() call and its buffer is static, it would be overriden :(
    (let* ((lines (%get-text-lines text start))
           (line-count (length lines))

           ;; Text style variables
           ;;int alignment = GuiGetStyle(DEFAULT, TEXT_ALIGNMENT);
           (alignment-vertical (gs +default+ +text-alignment-vertical+))
           (wrap-mode (gs +default+ +text-wrap-mode+)) ; Wrap-mode only available in read-only mode, no for text editing

           ;; TODO: WARNING: This totalHeight is not valid for vertical alignment in case of word-wrap
           (total-height (float (+ (* line-count (gs +default+ +text-size+)) (* (- line-count 1) (gs +default+ +text-line-spacing+)))))
           (pos-offset-y 0.0)
           (tbx (rectangle-x text-bounds)) (tby (rectangle-y text-bounds))
           (tbw (rectangle-width text-bounds)) (tbh (rectangle-height text-bounds)))

      (dotimes (i line-count)
        (let ((icon-id 0))
          (multiple-value-bind (line-start id) (%get-text-icon text (aref lines i)) ; Check text for icon and move cursor
            (setf (aref lines i) line-start
                  icon-id id))

          ;; Get text position depending on alignment and iconId
          ;;---------------------------------------------------------------------------------
          (let* ((line (aref lines i))
                 (text-bounds-position (vec2 tbx tby))
                 (text-bounds-width-offset 0.0)

                 ;; NOTE: Icon was already stripped above by GetTextIcon(); GetLineWidth()
                 ;; takes no icon path here and returns only the glyph width of this line.
                 (text-size-x (%get-line-width text line)))

            ;; If text requires an icon, add size to measure
            (when (>= icon-id 0)
              (incf text-size-x (* +raygui-icon-size+ *gui-icon-scale*))

              ;; WARNING: If only icon provided, text could be pointing to EOF character: '\0'
              (when (/= (%cref text line) 0) (incf text-size-x +raygui-icon-text-padding+)))

            ;; Check guiTextAlign global variables
            (case alignment
              (#.+text-align-left+ (setf (vx text-bounds-position) tbx))
              (#.+text-align-center+ (setf (vx text-bounds-position) (- (+ tbx (/ tbw 2)) (truncate text-size-x 2))))
              (#.+text-align-right+ (setf (vx text-bounds-position) (- (+ tbx tbw) text-size-x))))

            (when (and (> text-size-x tbw) (/= (%cref text line) 0)) (setf (vx text-bounds-position) tbx))

            (case alignment-vertical
              ;; Only valid in case of wordWrap = 0;
              (#.+text-align-top+ (setf (vy text-bounds-position) (+ tby pos-offset-y)))
              (#.+text-align-middle+ (setf (vy text-bounds-position) (+ (- (+ tby pos-offset-y (/ tbh 2)) (/ total-height 2)) (text-valign-pixel-offset tbh))))
              (#.+text-align-bottom+ (setf (vy text-bounds-position) (+ (- (+ tby pos-offset-y tbh) total-height) (text-valign-pixel-offset tbh)))))

            ;; NOTE: Make sure getting pixel-perfect coordinates,
            ;; In case of decimals, it could result in text positioning artifacts
            (setf (vx text-bounds-position) (float (truncate (vx text-bounds-position)))
                  (vy text-bounds-position) (float (truncate (vy text-bounds-position))))
            ;;---------------------------------------------------------------------------------

            ;; Draw text (with icon if available)
            ;;---------------------------------------------------------------------------------
            (when (>= icon-id 0)
              ;; NOTE: Considering icon height, probably different than text size
              (gui-draw-icon icon-id (truncate (vx text-bounds-position))
                             (truncate (+ (- (+ tby (/ tbh 2)) (truncate (* +raygui-icon-size+ *gui-icon-scale*) 2)) (text-valign-pixel-offset tbh)))
                             *gui-icon-scale* tint)
              (incf (vx text-bounds-position) (float (+ (* +raygui-icon-size+ *gui-icon-scale*) +raygui-icon-text-padding+)))
              (setf text-bounds-width-offset (float (+ (* +raygui-icon-size+ *gui-icon-scale*) +raygui-icon-text-padding+))))

            ;; Get size in bytes of text,
            ;; considering end of line and line break
            (let ((line-size 0))
              (loop for c from line
                    until (member (%cref text c) '(0 10 13))
                    do (incf line-size))

              (let ((scale-factor (/ (float (gs +default+ +text-size+)) (font-base-size *gui-font*)))
                    (last-space-index 0)
                    (temp-wrap-char-mode nil)
                    (text-offset-y 0)
                    (text-offset-x 0.0)
                    (glyph-width 0.0)
                    (ellipsis-width (gui-get-text-width "..."))
                    (text-overflow nil))
                (flet ((draw-codepoint (codepoint x)
                         (draw-text-codepoint *gui-font* codepoint (vec2 x (+ (vy text-bounds-position) text-offset-y))
                                              (float (gs +default+ +text-size+)) (%gui-fade tint *gui-alpha*))))
                  (loop with c = 0 and codepoint-size = 0
                        while (< c line-size)
                        do (let ((codepoint 0) (index 0))
                             (multiple-value-setq (codepoint codepoint-size) (%get-codepoint-next text (+ line c)))
                             (setf index (get-glyph-index *gui-font* codepoint))

                             ;; NOTE: Normally, exiting the decoding sequence as soon as a bad byte is found (and return 0x3f)
                             ;; but all of the bad bytes need to be drawn using the '?' symbol, moving one byte
                             (when (= codepoint #x3f) (setf codepoint-size 1)) ; WARNING: Not recognized codepoints size

                             ;; Get glyph width to check if it goes out of bounds
                             (setf glyph-width (%glyph-width index scale-factor))

                             ;; Wrap mode text measuring, to validate if
                             ;; it can be drawn or a new line is required
                             (cond ((= wrap-mode +text-wrap-char+)
                                    ;; Jump to next line if current character reach end of the box limits
                                    (when (> (+ text-offset-x glyph-width) (- tbw text-bounds-width-offset))
                                      (setf text-offset-x 0.0)
                                      (incf text-offset-y (+ (gs +default+ +text-size+) (gs +default+ +text-line-spacing+)))

                                      (when temp-wrap-char-mode ; Wrap at char level when too long words
                                        (setf wrap-mode +text-wrap-word+
                                              temp-wrap-char-mode nil))))
                                   ((= wrap-mode +text-wrap-word+)
                                    (when (= codepoint 32) (setf last-space-index c))

                                    ;; Get width to next space in line
                                    (let ((next-space-width (%get-next-space-width text (+ line c)))
                                          (next-word-size (%get-next-space-width text (+ line last-space-index 1))))

                                      (cond ((> next-word-size (- tbw text-bounds-width-offset))
                                             ;; Considering the case the next word is longer than bounds
                                             (setf temp-wrap-char-mode t
                                                   wrap-mode +text-wrap-char+))
                                            ((> (+ text-offset-x next-space-width) (- tbw text-bounds-width-offset))
                                             (setf text-offset-x 0.0)
                                             (incf text-offset-y (+ (gs +default+ +text-size+) (gs +default+ +text-line-spacing+))))))))

                             (when (= codepoint 10) (return)) ; WARNING: Lines are already processed manually, no need to keep drawing after this codepoint

                             ;; WARNING: There are multiple types of spaces in Unicode,
                             ;; maybe it's a good idea to add support for more: http://jkorpela.fi/chars/spaces.html
                             (when (and (/= codepoint 32) (/= codepoint 9)) ; Do not draw codepoints with no glyph
                               (cond ((= wrap-mode +text-wrap-none+)
                                      ;; Draw only required text glyphs fitting the textBounds.width
                                      (if (> text-size-x tbw)
                                          (cond ((<= text-offset-x (- tbw glyph-width text-bounds-width-offset ellipsis-width))
                                                 (draw-codepoint codepoint (+ (vx text-bounds-position) text-offset-x)))
                                                ((not text-overflow)
                                                 (setf text-overflow t)

                                                 (let ((step (truncate ellipsis-width 3)))
                                                   (when (> step 0)
                                                     (loop for j from 0 below ellipsis-width by step
                                                           do (draw-codepoint (char-code #\.) (+ (vx text-bounds-position) text-offset-x j)))))))
                                          (draw-codepoint codepoint (+ (vx text-bounds-position) text-offset-x))))
                                     ((or (= wrap-mode +text-wrap-char+) (= wrap-mode +text-wrap-word+))
                                      ;; Draw only glyphs inside the bounds
                                      (when (<= (+ (vy text-bounds-position) text-offset-y) (- (+ tby tbh) (gs +default+ +text-size+)))
                                        (draw-codepoint codepoint (+ (vx text-bounds-position) text-offset-x))))))

                             (incf text-offset-x (+ (%glyph-width index scale-factor) (float (gs +default+ +text-spacing+)))))
                           (incf c codepoint-size)))

                (cond ((= wrap-mode +text-wrap-none+)
                       (incf pos-offset-y (float (+ (gs +default+ +text-size+) (gs +default+ +text-line-spacing+)))))
                      ((or (= wrap-mode +text-wrap-char+) (= wrap-mode +text-wrap-word+))
                       (incf pos-offset-y (+ text-offset-y (gs +default+ +text-size+)))))))))))
    ;;---------------------------------------------------------------------------------
    nil))

;; Gui draw rectangle using default raygui plain style with borders
(defun %gui-draw-rectangle (rec border-width border-color color)
  (let ((x (truncate (rectangle-x rec))) (y (truncate (rectangle-y rec)))
        (w (truncate (rectangle-width rec))) (h (truncate (rectangle-height rec))))
    (when (> (fourth color) 0)
      ;; Draw rectangle filled with color
      (draw-rectangle x y w h (%gui-fade color *gui-alpha*)))

    (when (> border-width 0)
      ;; Draw rectangle border lines with color
      (draw-rectangle x y w border-width (%gui-fade border-color *gui-alpha*))
      (draw-rectangle x (+ y border-width) border-width (- h (* 2 border-width)) (%gui-fade border-color *gui-alpha*))
      (draw-rectangle (- (+ x w) border-width) (+ y border-width) border-width (- h (* 2 border-width)) (%gui-fade border-color *gui-alpha*))
      (draw-rectangle x (- (+ y h) border-width) w border-width (%gui-fade border-color *gui-alpha*))))
  nil)

;; Draw tooltip using control bounds
(defun %gui-tooltip (control-rec)
  (when (and (not *gui-locked*) *gui-tooltip* *gui-tooltip-ptr* (not *gui-control-exclusive-mode*))
    (let* ((control-rec (%rec-copy control-rec))
           (text-size (measure-text-ex *gui-font* *gui-tooltip-ptr* (float (gs +default+ +text-size+)) (float (gs +default+ +text-spacing+)))))

      (when (> (+ (rectangle-x control-rec) (vx text-size) 16) (get-screen-width))
        (decf (rectangle-x control-rec) (- (+ (vx text-size) 16) (rectangle-width control-rec))))

      (let ((line-count (length (%get-text-lines (%cstr *gui-tooltip-ptr*) 0)))) ; Only using the line count

        (when (> (+ (rectangle-y control-rec) (rectangle-height control-rec) (vy text-size) 4 (* 8 line-count)) (get-screen-height))
          (decf (rectangle-y control-rec) (+ (rectangle-height control-rec) (vy text-size) 4 (* 8 line-count))))

        ;; TODO: Probably TEXT_LINE_SPACING should be considered on panel size instead of hardcoding 8.0f
        (gui-panel (%rec (rectangle-x control-rec) (+ (rectangle-y control-rec) (rectangle-height control-rec) 4)
                         (+ (vx text-size) 16) (+ (vy text-size) (* 8.0 line-count)))
                   nil)

        (let ((text-padding (gs +label+ +text-padding+))
              (text-alignment (gs +label+ +text-alignment+)))
          (gui-set-style +label+ +text-padding+ 0)
          (gui-set-style +label+ +text-alignment+ +text-align-center+)
          (gui-label (%rec (rectangle-x control-rec) (+ (rectangle-y control-rec) (rectangle-height control-rec) 4)
                           (+ (vx text-size) 16) (+ (vy text-size) (* 8.0 line-count)))
                     *gui-tooltip-ptr*)
          (gui-set-style +label+ +text-alignment+ text-alignment)
          (gui-set-style +label+ +text-padding+ text-padding)))))
  nil)

;; Scroll bar control (used by GuiScrollPanel())
(defun %gui-scroll-bar (bounds value min-value max-value)
  (let* ((state *gui-state*)

         ;; Is the scrollbar horizontal or vertical?
         (is-vertical (not (> (rectangle-width bounds) (rectangle-height bounds))))

         ;; The size (width or height depending on scrollbar type) of the spinner buttons
         (spinner-size (if (/= (gs +scrollbar+ +arrows-visible+) 0)
                           (if is-vertical
                               (- (truncate (rectangle-width bounds)) (* 2 (gs +scrollbar+ +border-width+)))
                               (- (truncate (rectangle-height bounds)) (* 2 (gs +scrollbar+ +border-width+))))
                           0))

         ;; Arrow buttons [<] [>] [∧] [∨]
         (arrow-up-left nil)
         (arrow-down-right nil)

         ;; Actual area of the scrollbar excluding the arrow buttons
         (scrollbar nil)

         ;; Slider bar that moves     --[///]-----
         (slider nil)
         (value-range 0)
         (slider-size 0))

    ;; Normalize value
    (when (> value max-value) (setf value max-value))
    (when (< value min-value) (setf value min-value))

    (setf value-range (- max-value min-value))
    (when (<= value-range 0) (setf value-range 1))

    (setf slider-size (gs +scrollbar+ +scroll-slider-size+))
    (when (< slider-size 1) (setf slider-size 1)) ; Consider a minimum slider size of 1 pixel

    ;; Calculate rectangles for all of the components
    (setf arrow-up-left (%rec (+ (rectangle-x bounds) (gs +scrollbar+ +border-width+))
                              (+ (rectangle-y bounds) (gs +scrollbar+ +border-width+))
                              (float spinner-size) (float spinner-size)))

    (if is-vertical
        (progn
          (setf arrow-down-right (%rec (+ (rectangle-x bounds) (gs +scrollbar+ +border-width+))
                                       (- (+ (rectangle-y bounds) (rectangle-height bounds)) spinner-size (gs +scrollbar+ +border-width+))
                                       (float spinner-size) (float spinner-size)))
          (setf scrollbar (%rec (+ (rectangle-x bounds) (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-padding+))
                                (+ (rectangle-y arrow-up-left) (rectangle-height arrow-up-left))
                                (- (rectangle-width bounds) (* 2 (+ (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-padding+))))
                                (- (rectangle-height bounds) (rectangle-height arrow-up-left) (rectangle-height arrow-down-right) (* 2 (gs +scrollbar+ +border-width+)))))

          ;; Make sure the slider won't get outside of the scrollbar
          (setf slider-size (if (>= slider-size (rectangle-height scrollbar)) (- (truncate (rectangle-height scrollbar)) 2) slider-size))
          (setf slider (%rec (+ (rectangle-x bounds) (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-slider-padding+))
                             (+ (rectangle-y scrollbar) (truncate (* (/ (float (- value min-value)) value-range) (- (rectangle-height scrollbar) slider-size))))
                             (- (rectangle-width bounds) (* 2 (+ (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-slider-padding+))))
                             (float slider-size))))
        (progn                          ; horizontal
          (setf arrow-down-right (%rec (- (+ (rectangle-x bounds) (rectangle-width bounds)) spinner-size (gs +scrollbar+ +border-width+))
                                       (+ (rectangle-y bounds) (gs +scrollbar+ +border-width+))
                                       (float spinner-size) (float spinner-size)))
          (setf scrollbar (%rec (+ (rectangle-x arrow-up-left) (rectangle-width arrow-up-left))
                                (+ (rectangle-y bounds) (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-padding+))
                                (- (rectangle-width bounds) (rectangle-width arrow-up-left) (rectangle-width arrow-down-right) (* 2 (gs +scrollbar+ +border-width+)))
                                (- (rectangle-height bounds) (* 2 (+ (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-padding+))))))

          ;; Make sure the slider won't get outside of the scrollbar
          (setf slider-size (if (>= slider-size (rectangle-width scrollbar)) (- (truncate (rectangle-width scrollbar)) 2) slider-size))
          (setf slider (%rec (+ (rectangle-x scrollbar) (truncate (* (/ (float (- value min-value)) value-range) (- (rectangle-width scrollbar) slider-size))))
                             (+ (rectangle-y bounds) (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-slider-padding+))
                             (float slider-size)
                             (- (rectangle-height bounds) (* 2 (+ (gs +scrollbar+ +border-width+) (gs +scrollbar+ +scroll-slider-padding+))))))))

    ;; Update control
    ;;--------------------------------------------------------------------
    (when (and (/= state +state-disabled+) (not *gui-locked*))
      (let ((mouse-point (gui-pointer-position)))
        (flet ((value-from-mouse ()
                 (if is-vertical
                     (truncate (+ (/ (* (float (- (vy mouse-point) (rectangle-y scrollbar) (/ (rectangle-height slider) 2))) value-range)
                                     (- (rectangle-height scrollbar) (rectangle-height slider)))
                                  min-value))
                     (truncate (+ (/ (* (float (- (vx mouse-point) (rectangle-x scrollbar) (/ (rectangle-width slider) 2))) value-range)
                                     (- (rectangle-width scrollbar) (rectangle-width slider)))
                                  min-value)))))
          (cond (*gui-control-exclusive-mode* ; Allows to keep dragging outside of bounds
                 (if (and (gui-button-down-p)
                          (not (check-collision-point-rec mouse-point arrow-up-left))
                          (not (check-collision-point-rec mouse-point arrow-down-right)))
                     (when (check-bounds-id bounds *gui-control-exclusive-rec*)
                       (setf state +state-pressed+)
                       (setf value (value-from-mouse)))
                     (setf *gui-control-exclusive-mode* nil
                           *gui-control-exclusive-rec* (%rec 0 0 0 0))))
                ((check-collision-point-rec mouse-point bounds)
                 (setf state +state-focused+)

                 ;; Handle mouse wheel
                 (let ((scroll-delta (gui-scroll-delta)))
                   (when (/= scroll-delta 0) (incf value (truncate scroll-delta))))

                 ;; Handle mouse button down
                 (when (gui-button-pressed-p)
                   (setf *gui-control-exclusive-mode* t
                         *gui-control-exclusive-rec* (%rec-copy bounds)) ; Store bounds as an identifier when dragging starts

                   ;; Check arrows click
                   (cond ((check-collision-point-rec mouse-point arrow-up-left) (decf value (truncate value-range (gs +scrollbar+ +scroll-speed+))))
                         ((check-collision-point-rec mouse-point arrow-down-right) (incf value (truncate value-range (gs +scrollbar+ +scroll-speed+))))
                         ((not (check-collision-point-rec mouse-point slider))
                          ;; If click on scrollbar position but not on slider, place slider directly on that position
                          (setf value (value-from-mouse))))

                   (setf state +state-pressed+))

                 ;; Keyboard control on mouse hover scrollbar
                 ;;if (isVertical)
                 ;;{
                 ;;    if (GUI_KEY_DOWN(KEY_DOWN)) value += 5;
                 ;;    else if (GUI_KEY_DOWN(KEY_UP)) value -= 5;
                 ;;}
                 ;;else
                 ;;{
                 ;;    if (GUI_KEY_DOWN(KEY_RIGHT)) value += 5;
                 ;;    else if (GUI_KEY_DOWN(KEY_LEFT)) value -= 5;
                 ;;}
                 )))

        ;; Normalize value
        (when (> value max-value) (setf value max-value))
        (when (< value min-value) (setf value min-value))))
    ;;--------------------------------------------------------------------

    ;; Draw control
    ;;--------------------------------------------------------------------
    (%gui-draw-rectangle bounds (gs +scrollbar+ +border-width+) (gcol +listview+ (+ +border+ (* state 3))) (gcol +default+ +border-color-disabled+)) ; Draw the background

    (%gui-draw-rectangle scrollbar 0 +blank+ (gcol +button+ +base-color-normal+)) ; Draw the scrollbar active area background
    (%gui-draw-rectangle slider 0 +blank+ (gcol +slider+ (+ +border+ (* state 3)))) ; Draw the slider bar

    ;; Draw arrows (using icon if available)
    (when (/= (gs +scrollbar+ +arrows-visible+) 0)
      (let ((size (if is-vertical (rectangle-width bounds) (rectangle-height bounds))))
        (%gui-draw-text (%cstr (if is-vertical (gui-icon-text +icon-arrow-up-fill+ nil) (gui-icon-text +icon-arrow-left-fill+ nil))) 0
                        (%rec (rectangle-x arrow-up-left) (rectangle-y arrow-up-left) size size)
                        +text-align-center+ (gcol +scrollbar+ (+ +text+ (* state 3)))) ; ICON_ARROW_UP_FILL / ICON_ARROW_LEFT_FILL
        (%gui-draw-text (%cstr (if is-vertical (gui-icon-text +icon-arrow-down-fill+ nil) (gui-icon-text +icon-arrow-right-fill+ nil))) 0
                        (%rec (rectangle-x arrow-down-right) (rectangle-y arrow-down-right) size size)
                        +text-align-center+ (gcol +scrollbar+ (+ +text+ (* state 3)))))) ; ICON_ARROW_DOWN_FILL / ICON_ARROW_RIGHT_FILL
    ;;--------------------------------------------------------------------

    value))

;; Update font image atlas to append raygui icons
;; NOTE: Only used when RAYGUI_FONT_ICONS_BAKING is enabled
;; Returns (values icon-offset-y white-rec)
(defun %gui-font-icon-baking (im-font font)
  (let ((icon-offset-y 0)
        (white-rec (make-rectangle))
        ;; Check max glyph rec y bottom position in the atlas image,
        ;; to start drawing icons below that line
        (max-glyph-rec-y 0)
        (icon-cell (+ +raygui-icon-size+ (* 2 +raygui-icon-font-atlas-padding+))))
    (dotimes (i (font-glyph-count font))
      (let ((rec (aref (font-recs font) i)))
        (when (> (+ (rectangle-y rec) (rectangle-height rec)) max-glyph-rec-y)
          (setf max-glyph-rec-y (+ (truncate (rectangle-y rec)) (truncate (rectangle-height rec)))))))

    (let* ((max-icons-per-line (truncate (image-width im-font) icon-cell))
           (req-icon-lines (+ (truncate +raygui-icon-max-font-backed+ max-icons-per-line) (mod +raygui-icon-max-font-backed+ max-icons-per-line) 1)) ; One extra line
           (req-height (* req-icon-lines icon-cell)))

      ;; Check if image requires scaling and how much
      (when (> (+ max-glyph-rec-y req-height) (image-height im-font))
        (let ((new-im-height 1))
          (loop while (< new-im-height (+ max-glyph-rec-y req-height)) do (setf new-im-height (ash new-im-height 1))) ; Round to next POT

          (let ((new-im-data (make-array (* (image-width im-font) new-im-height 2) :element-type '(unsigned-byte 8) :initial-element 0))
                (width (image-width im-font)))
            (replace new-im-data (image-data im-font) :end2 (* width (image-height im-font) 2))

            (flet ((set-pixel (index value)
                     (setf (aref new-im-data (* index 2)) (ldb (byte 8 0) value)
                           (aref new-im-data (+ (* index 2) 1)) (ldb (byte 8 8) value))))
              ;; Clear imFont bottom-right corner rec
              (dotimes (y 4)
                (dotimes (x 4)
                  (set-pixel (- (+ (* (- (image-height im-font) y 1) width) width) x 1) #x0000)))

              (setf (image-data im-font) new-im-data
                    (image-height im-font) new-im-height)

              ;; Set new image white corner and new white rec
              (dotimes (y 3)
                (dotimes (x 3)
                  (set-pixel (- (+ (* (- (image-height im-font) y 1) width) width) x 1) #xffff))))

            (setf white-rec (%rec (- (float width) 2) (- (float (image-height im-font)) 2) 1 1))))))

    ;; Calculate image offset positions to start drawing
    ;; NOTE: For Y, look for a multiple of icon size + 2*padding, for better alignment
    (let* ((offset-x +raygui-icon-font-atlas-padding+)
           (offset-y (+ (* (truncate (+ max-glyph-rec-y (- icon-cell 1)) icon-cell) icon-cell) +raygui-icon-font-atlas-padding+))
           (pixels (image-data im-font))
           (width (image-width im-font)))
      (setf icon-offset-y (- offset-y +raygui-icon-font-atlas-padding+))

      (dotimes (icon-id +raygui-icon-max-font-backed+)
        ;; Wrap to next line if next icon won't fit
        (when (> (+ offset-x icon-cell) width)
          (setf offset-x +raygui-icon-font-atlas-padding+)
          (incf offset-y icon-cell))

        (loop with y = 0
              for i from 0 below +raygui-icon-data-elements+
              do (let ((data (aref *gui-icons-ptr* (+ (* icon-id +raygui-icon-data-elements+) i))))
                   (dotimes (k 32)
                     (let ((index (+ (* (+ offset-y y) width) offset-x (mod k 16)))
                           (value (if (logbitp k data) #xffff #x00ff)))
                       (setf (aref pixels (* index 2)) (ldb (byte 8 0) value)
                             (aref pixels (+ (* index 2) 1)) (ldb (byte 8 8) value)))

                     (when (or (= k 15) (= k 31)) (incf y)))))

        ;; Update current icon X position
        (incf offset-x icon-cell)))

    (values icon-offset-y white-rec)))

;; Split controls text into multiple strings
;; Returns a vector of NUL-terminated UTF-8 items
(defun %gui-text-split (text delimiter)
  ;; NOTE: Current implementation returns a copy of the provided string with '\0' (string end delimiter)
  ;; inserted between strings defined by "delimiter" parameter. No memory is dynamically allocated,
  ;; all used memory is static... it has some limitations:
  ;;      1. Maximum number of possible split strings is set by RAYGUI_TEXTSPLIT_MAX_ITEMS
  ;;      2. Maximum size of text to split is RAYGUI_TEXTSPLIT_MAX_TEXT_SIZE
  ;; NOTE: Those definitions could be externally provided if required
  (let ((buffer (make-array (1+ +raygui-textsplit-max-text-size+) :element-type '(unsigned-byte 8) :initial-element 0))
        (item-ptrs (list 0)))

    ;; Count how many substrings text contains and point to every one of them
    (dotimes (i +raygui-textsplit-max-text-size+)
      (setf (aref buffer i) (%cref text i))
      (cond ((= (aref buffer i) 0) (return))
            ((or (= (aref buffer i) delimiter) (= (aref buffer i) 10))
             (push (+ i 1) item-ptrs)
             (setf (aref buffer i) 0)  ; Set terminator for current item

             (when (>= (length item-ptrs) +raygui-textsplit-max-items+) (return)))))

    (map 'vector (lambda (start)
                   (let ((end (+ start (%strlen buffer start))))
                     (%cstr (subseq buffer start end))))
         (nreverse item-ptrs))))

;; Convert color data from RGB to HSV
;; NOTE: Color data should be passed normalized
(defun convert-rgb-to-hsv (rgb)
  (let ((hsv (vec3 0.0 0.0 0.0))
        (min 0.0) (max 0.0) (delta 0.0))

    (setf min (if (< (vx rgb) (vy rgb)) (vx rgb) (vy rgb)))
    (setf min (if (< min (vz rgb)) min (vz rgb)))

    (setf max (if (> (vx rgb) (vy rgb)) (vx rgb) (vy rgb)))
    (setf max (if (> max (vz rgb)) max (vz rgb)))

    (setf (vz hsv) max)                 ; Value
    (setf delta (- max min))

    (when (< delta 0.00001)
      (setf (vy hsv) 0.0
            (vx hsv) 0.0)               ; Undefined, maybe NAN?
      (return-from convert-rgb-to-hsv hsv))

    (if (> max 0.0)
        ;; NOTE: If max is 0, this divide would cause a crash
        (setf (vy hsv) (/ delta max))   ; Saturation
        (progn
          ;; NOTE: If max is 0, then r = g = b = 0, s = 0, h is undefined
          (setf (vy hsv) 0.0
                (vx hsv) 0.0)           ; Undefined, maybe NAN?
          (return-from convert-rgb-to-hsv hsv)))

    ;; NOTE: Comparing float values could not work properly
    (cond ((>= (vx rgb) max) (setf (vx hsv) (/ (- (vy rgb) (vz rgb)) delta))) ; Between yellow & magenta
          ((>= (vy rgb) max) (setf (vx hsv) (+ 2.0 (/ (- (vz rgb) (vx rgb)) delta)))) ; Between cyan & yellow
          (t (setf (vx hsv) (+ 4.0 (/ (- (vx rgb) (vy rgb)) delta))))) ; Between magenta & cyan

    (setf (vx hsv) (* (vx hsv) 60.0))  ; Convert to degrees

    (when (< (vx hsv) 0.0) (incf (vx hsv) 360.0))

    hsv))

;; Convert color data from HSV to RGB
;; NOTE: Color data should be passed normalized
(defun convert-hsv-to-rgb (hsv)
  (let ((rgb (vec3 0.0 0.0 0.0))
        (hh 0.0) (p 0.0) (q 0.0) (tt 0.0) (ff 0.0)
        (i 0))

    ;; NOTE: Comparing float values could not work properly
    (when (<= (vy hsv) 0.0)
      (setf (vx rgb) (vz hsv)
            (vy rgb) (vz hsv)
            (vz rgb) (vz hsv))
      (return-from convert-hsv-to-rgb rgb))

    (setf hh (vx hsv))
    (when (>= hh 360.0) (setf hh 0.0))
    (setf hh (/ hh 60.0))

    (setf i (truncate hh)
          ff (- hh i)
          p (* (vz hsv) (- 1.0 (vy hsv)))
          q (* (vz hsv) (- 1.0 (* (vy hsv) ff)))
          tt (* (vz hsv) (- 1.0 (* (vy hsv) (- 1.0 ff)))))

    (case i
      (0 (setf (vx rgb) (vz hsv) (vy rgb) tt (vz rgb) p))
      (1 (setf (vx rgb) q (vy rgb) (vz hsv) (vz rgb) p))
      (2 (setf (vx rgb) p (vy rgb) (vz hsv) (vz rgb) tt))
      (3 (setf (vx rgb) p (vy rgb) q (vz rgb) (vz hsv)))
      (4 (setf (vx rgb) tt (vy rgb) p (vz rgb) (vz hsv)))
      (t (setf (vx rgb) (vz hsv) (vy rgb) p (vz rgb) q)))

    rgb))

;; Color fade-in or fade-out, alpha goes from 0.0f to 1.0f
;; WARNING: It multiplies current alpha by alpha scale factor,
;; raylib Fade() multiplies alpha by 255.0f
(defun %gui-fade (color alpha)
  (let ((alpha (float alpha 1.0)))
    (cond ((< alpha 0.0) (setf alpha 0.0))
          ((> alpha 1.0) (setf alpha 1.0)))

    (list (first color) (second color) (third color) (truncate (* (fourth color) alpha)))))

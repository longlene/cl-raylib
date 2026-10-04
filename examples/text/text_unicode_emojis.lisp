;;;; raylib [text] example - unicode emojis
;;;;
;;;; Example complexity rating: [★★★★] 4/4
;;;;
;;;; Example originally created with raylib 2.5, last time updated with raylib 4.0
;;;;
;;;; Example contributed by Vlad Adrian (@demizdor) and reviewed by Ramon Santamaria (@raysan5)
;;;;
;;;; Example licensed under an unmodified zlib/libpng license, which is an OSI-certified,
;;;; BSD-like license that allows static linking with closed source software
;;;;
;;;; Copyright (c) 2019-2025 Vlad Adrian (@demizdor) and Ramon Santamaria (@raysan5)
;;;; Common Lisp port of raylib/examples/text/text_unicode_emojis.c

(require :cl-raylib)

(defpackage #:raylib-examples/text-unicode-emojis
  (:use #:cl #:raylib))
(in-package #:raylib-examples/text-unicode-emojis)

(defconstant +emoji-per-width+ 8)
(defconstant +emoji-per-height+ 4)

;;--------------------------------------------------------------------------------------
;; Global Variables Definition
;;--------------------------------------------------------------------------------------
;; Arrays that holds the random emojis
(defstruct emoji
  (index 0)                             ; Index inside `*emoji-codepoints*`
  (message 0)                           ; Message index
  (color +blank+))                      ; Emoji color

(defparameter *emoji* (let ((v (make-array (* +emoji-per-width+ +emoji-per-height+))))
                        (dotimes (i (length v) v) (setf (aref v i) (make-emoji)))))

(defvar *hovered* -1)
(defvar *selected* -1)

;; 180 emoji codepoints (C: one string with the UTF-8 codepoints separated by a NUL char)
(defparameter *emoji-codepoints*
  #("🌀" "😀" "😂" "🤣" "😃" "😆" "😉" "😋" "😎" "😍" "😘" "😗"
    "😙" "😚" "🙂" "🤗" "🤩" "🤔" "🤨" "😐" "😑" "😶" "🙄" "😏"
    "😣" "😥" "😮" "🤐" "😯" "😪" "😫" "😴" "😌" "😛" "😝" "🤤"
    "😒" "😕" "🙃" "🤑" "😲" "🙁" "😖" "😞" "😟" "😤" "😢" "😭"
    "😦" "😩" "🤯" "😬" "😰" "😱" "😳" "🤪" "😵" "😡" "😠" "🤬"
    "😷" "🤒" "🤕" "🤢" "🤮" "🤧" "😇" "🤠" "🤫" "🤭" "🧐" "🤓"
    "😈" "👿" "👹" "👺" "💀" "👻" "👽" "👾" "🤖" "💩" "😺" "😸"
    "😹" "😻" "😽" "🙀" "😿" "🌾" "🌿" "🍀" "🍃" "🍇" "🍓" "🥝"
    "🍅" "🥥" "🥑" "🍆" "🥔" "🥕" "🌽" "🌶" "🥒" "🥦" "🍄" "🥜"
    "🌰" "🍞" "🥐" "🥖" "🥨" "🥞" "🧀" "🍖" "🍗" "🥩" "🥓" "🍔"
    "🍟" "🍕" "🌭" "🥪" "🌮" "🌯" "🥙" "🥚" "🍳" "🥘" "🍲" "🥣"
    "🥗" "🍿" "🥫" "🍱" "🍘" "🍝" "🍠" "🍢" "🍥" "🍡" "🥟" "🥡"
    "🍦" "🍪" "🎂" "🍰" "🥧" "🍫" "🍯" "🍼" "🥛" "🍵" "🍶" "🍾"
    "🍷" "🍻" "🥂" "🥃" "🥤" "🥢" "👁" "👅" "👄" "💋" "💘" "💓"
    "💗" "💙" "💛" "🧡" "💜" "🖤" "💝" "💟" "💌" "💤" "💢" "💣"))

;; Array containing all of the emojis messages: (text language)
(defparameter *messages*
  (vector
   (list "Falsches Üben von Xylophonmusik quält jeden größeren Zwerg" "German")
   (list "Beiß nicht in die Hand, die dich füttert." "German")
   (list "Außerordentliche Übel erfordern außerordentliche Mittel." "German")
   (list "Կրնամ ապակի ուտել և ինծի անհանգիստ չըներ" "Armenian")
   (list "Երբ որ կացինը եկաւ անտառ, ծառերը ասացին... «Կոտը մերոնցից է:»" "Armenian")
   (list "Գառը՝ գարնան, ձիւնը՝ ձմռան" "Armenian")
   (list "Jeżu klątw, spłódź Finom część gry hańb!" "Polish")
   (list "Dobrymi chęciami jest piekło wybrukowane." "Polish")
   (list (format nil "Îți mulțumesc că ai ales raylib.~%Și sper să ai o zi bună!") "Romanian")
   (list "Эх, чужак, общий съём цен шляп (юфть) вдрызг!" "Russian")
   (list "Я люблю raylib!" "Russian")
   (list (format nil "Молчи, скрывайся и таи~%И чувства и мечты свои –~%Пускай в душевной глубине~%И всходят и зайдут оне~%Как звезды ясные в ночи-~%Любуйся ими – и молчи.") "Russian")
   (list "Voix ambiguë d’un cœur qui au zéphyr préfère les jattes de kiwi" "French")
   (list "Benjamín pidió una bebida de kiwi y fresa; Noé, sin vergüenza, la más exquisita champaña del menú." "Spanish")
   (list "Ταχίστη αλώπηξ βαφής ψημένη γη, δρασκελίζει υπέρ νωθρού κυνός" "Greek")
   (list "Η καλύτερη άμυνα είναι η επίθεση." "Greek")
   (list "Χρόνια και ζαμάνια!" "Greek")
   (list "Πώς τα πας σήμερα;" "Greek")
   (list "我能吞下玻璃而不伤身体。" "Chinese")
   (list "你吃了吗？" "Chinese")
   (list "不作不死。" "Chinese")
   (list "最近好吗？" "Chinese")
   (list "塞翁失马，焉知非福。" "Chinese")
   (list "千军易得, 一将难求" "Chinese")
   (list "万事开头难。" "Chinese")
   (list "风无常顺，兵无常胜。" "Chinese")
   (list "活到老，学到老。" "Chinese")
   (list "一言既出，驷马难追。" "Chinese")
   (list "路遥知马力，日久见人心" "Chinese")
   (list "有理走遍天下，无理寸步难行。" "Chinese")
   (list "猿も木から落ちる" "Japanese")
   (list "亀の甲より年の功" "Japanese")
   (list "うらやまし  思ひ切る時  猫の恋" "Japanese")
   (list "虎穴に入らずんば虎子を得ず。" "Japanese")
   (list "二兎を追う者は一兎をも得ず。" "Japanese")
   (list "馬鹿は死ななきゃ治らない。" "Japanese")
   (list "枯野路に　影かさなりて　わかれけり" "Japanese")
   (list "繰り返し麦の畝縫ふ胡蝶哉" "Japanese")
   (list (format nil "아득한 바다 위에 갈매기 두엇 날아 돈다.~%너훌너훌 시를 쓴다. 모르는 나라 글자다.~%널따란 하늘 복판에 나도 같이 시를 쓴다.") "Korean")
   (list "제 눈에 안경이다" "Korean")
   (list "꿩 먹고 알 먹는다" "Korean")
   (list "로마는 하루아침에 이루어진 것이 아니다" "Korean")
   (list "고생 끝에 낙이 온다" "Korean")
   (list "개천에서 용 난다" "Korean")
   (list "안녕하세요?" "Korean")
   (list "만나서 반갑습니다" "Korean")
   (list "한국말 하실 줄 아세요?" "Korean")
   ))

;;--------------------------------------------------------------------------------------
;; Module Functions Definition
;;--------------------------------------------------------------------------------------

;; Fills the emoji array with random emoji (only those emojis present in fontEmoji)
(defun randomize-emoji ()
  (setf *hovered* -1
        *selected* -1)
  (let ((start (get-random-value 45 360)))

    (dotimes (i (length *emoji*))
      (let ((e (aref *emoji* i)))
        ;; 0-179 emoji codepoints (C: from emoji char array, each 4bytes + null char)
        (setf (emoji-index e) (get-random-value 0 179))

        ;; Generate a random color for this emoji
        (setf (emoji-color e) (fade (color-from-hsv (float (mod (* start (1+ i)) 360)) 0.6 0.85) 0.8))

        ;; Set a random message for this emoji
        (setf (emoji-message e) (get-random-value 0 (1- (length *messages*))))))))

;; UTF-8 decoding of OCTETS at I like raylib GetCodepoint(), returns (values codepoint byte-count)
;; NOTE: 0x3f ('?') is returned on failure
(defun octets-codepoint (octets i)
  (let ((length (length octets)))
    (flet ((byte-at (j) (if (< j length) (aref octets j) 0))
           (continuation-p (b) (= (logand b #xc0) #x80)))
      (let ((b0 (byte-at i)))
        (cond ((<= b0 #x7f) (values b0 1))
              ((= (logand b0 #xe0) #xc0)
               (let ((b1 (byte-at (+ i 1))))
                 (if (continuation-p b1)
                     (let ((cp (logior (ash (logand b0 #x1f) 6) (logand b1 #x3f))))
                       (if (>= cp #x80) (values cp 2) (values #x3f 1)))
                     (values #x3f 1))))
              ((= (logand b0 #xf0) #xe0)
               (let ((b1 (byte-at (+ i 1))) (b2 (byte-at (+ i 2))))
                 (if (and (continuation-p b1) (continuation-p b2))
                     (let ((cp (logior (ash (logand b0 #x0f) 12) (ash (logand b1 #x3f) 6) (logand b2 #x3f))))
                       (if (and (>= cp #x800) (not (<= #xd800 cp #xdfff))) (values cp 3) (values #x3f 1)))
                     (values #x3f 1))))
              ((= (logand b0 #xf8) #xf0)
               (let ((b1 (byte-at (+ i 1))) (b2 (byte-at (+ i 2))) (b3 (byte-at (+ i 3))))
                 (if (and (continuation-p b1) (continuation-p b2) (continuation-p b3))
                     (let ((cp (logior (ash (logand b0 #x07) 18) (ash (logand b1 #x3f) 12) (ash (logand b2 #x3f) 6) (logand b3 #x3f))))
                       (if (and (>= cp #x10000) (<= cp #x10ffff)) (values cp 4) (values #x3f 1)))
                     (values #x3f 1))))
              (t (values #x3f 1)))))))

;; Draw text using font inside rectangle limits with support for text selection
;; NOTE: Like C, I and K walk the UTF-8 bytes of TEXT
(defun draw-text-boxed-selectable (font text rec font-size spacing word-wrap tint select-start select-length select-tint select-back-tint)
  (let* ((octets (babel:string-to-octets text :encoding :utf-8))
         (length (length octets))       ; Total length in bytes of the text, scanned by codepoints in loop

         (text-offset-y 0.0)            ; Offset between lines (on line break '\n')
         (text-offset-x 0.0)            ; Offset X to next character to draw

         (scale-factor (/ font-size (float (font-base-size font)))) ; Character rectangle scaling factor

         ;; Word/character wrapping mechanism variables
         (measure-state 0)
         (draw-state 1)
         (state (if word-wrap measure-state draw-state))

         (start-line -1)                ; Index where to begin drawing (where a line begins)
         (end-line -1)                  ; Index where to stop drawing (where a line ends)
         (lastk -1)                     ; Holds last value of the character position
         (i 0)
         (k 0))

    (loop while (< i length)
          do (multiple-value-bind (codepoint codepoint-byte-count) (octets-codepoint octets i)
               ;; Get next codepoint from byte string and glyph index in font
               (let ((index (get-glyph-index font codepoint))
                     (glyph-width 0.0))

                 ;; NOTE: Normally we exit the decoding sequence as soon as a bad byte is found (and return 0x3f)
                 ;; but we need to draw all of the bad bytes using the '?' symbol moving one byte
                 (when (= codepoint #x3f) (setf codepoint-byte-count 1))
                 (incf i (1- codepoint-byte-count))

                 (when (/= codepoint (char-code #\Newline))
                   (setf glyph-width (if (= (glyph-info-advance-x (aref (font-glyphs font) index)) 0)
                                         (* (rectangle-width (aref (font-recs font) index)) scale-factor)
                                         (* (glyph-info-advance-x (aref (font-glyphs font) index)) scale-factor)))

                   (when (< (1+ i) length) (setf glyph-width (+ glyph-width spacing))))

                 ;; NOTE: When wordWrap is ON we first measure how much of the text we can draw before going outside of the rec container
                 ;; We store this info in startLine and endLine, then we change states, draw the text between those two variables
                 ;; and change states again and again recursively until the end of the text (or until we get outside of the container)
                 ;; When wordWrap is OFF we don't need the measure state so we go to the drawing state immediately
                 ;; and begin drawing on the next line before we can get outside the container
                 (if (= state measure-state)
                     (progn
                       ;; TODO: There are multiple types of spaces in UNICODE, maybe it's a good idea to add support for more
                       ;; Ref: http://jkorpela.fi/chars/spaces.html
                       (when (member codepoint (list (char-code #\Space) (char-code #\Tab) (char-code #\Newline))) (setf end-line i))

                       (cond ((> (+ text-offset-x glyph-width) (rectangle-width rec))
                              (setf end-line (if (< end-line 1) i end-line))
                              (when (= i end-line) (decf end-line codepoint-byte-count))
                              (when (= (+ start-line codepoint-byte-count) end-line) (setf end-line (- i codepoint-byte-count)))

                              (setf state (- 1 state)))
                             ((= (1+ i) length)
                              (setf end-line i)
                              (setf state (- 1 state)))
                             ((= codepoint (char-code #\Newline)) (setf state (- 1 state))))

                       (when (= state draw-state)
                         (setf text-offset-x 0.0
                               i start-line
                               glyph-width 0.0)

                         ;; Save character position when we switch states
                         (let ((tmp lastk))
                           (setf lastk (1- k)
                                 k tmp))))
                     (progn
                       (if (= codepoint (char-code #\Newline))
                           (unless word-wrap
                             (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                             (setf text-offset-x 0.0))
                           (progn
                             (when (and (not word-wrap) (> (+ text-offset-x glyph-width) (rectangle-width rec)))
                               (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                               (setf text-offset-x 0.0))

                             ;; When text overflows rectangle height limit, just stop drawing
                             (when (> (+ text-offset-y (* (font-base-size font) scale-factor)) (rectangle-height rec)) (return))

                             ;; Draw selection background
                             (let ((is-glyph-selected nil))
                               (when (and (>= select-start 0) (>= k select-start) (< k (+ select-start select-length)))
                                 (draw-rectangle-rec (make-rectangle :x (- (+ (rectangle-x rec) text-offset-x) 1) :y (+ (rectangle-y rec) text-offset-y)
                                                                     :width glyph-width :height (* (float (font-base-size font)) scale-factor))
                                                     select-back-tint)
                                 (setf is-glyph-selected t))

                               ;; Draw current character glyph
                               (when (and (/= codepoint (char-code #\Space)) (/= codepoint (char-code #\Tab)))
                                 (draw-text-codepoint font codepoint (vec2 (+ (rectangle-x rec) text-offset-x) (+ (rectangle-y rec) text-offset-y))
                                                      font-size (if is-glyph-selected select-tint tint))))))

                       (when (and word-wrap (= i end-line))
                         (incf text-offset-y (* (+ (font-base-size font) (/ (float (font-base-size font)) 2)) scale-factor))
                         (setf text-offset-x 0.0
                               start-line end-line
                               end-line -1
                               glyph-width 0.0)
                         (incf select-start (- lastk k))
                         (setf k lastk)

                         (setf state (- 1 state)))))

                 (incf text-offset-x glyph-width)))
             (incf i)
             (incf k))))

;; Draw text using font inside rectangle limits
(defun draw-text-boxed (font text rec font-size spacing word-wrap tint)
  (draw-text-boxed-selectable font text rec font-size spacing word-wrap tint 0 0 +white+ +white+))

;;------------------------------------------------------------------------------------
;; Program main entry point
;;------------------------------------------------------------------------------------
(defun main ()
  ;; Initialization
  ;;--------------------------------------------------------------------------------------
  (let ((screen-width 800)
        (screen-height 450))

    (set-config-flags (logior +flag-msaa-4x-hint+ +flag-vsync-hint+))
    (init-window screen-width screen-height "raylib [text] example - unicode emojis")

    ;; Load the font resources
    ;; NOTE: fontAsian is for asian languages,
    ;; fontEmoji is the emojis and fontDefault is used for everything else
    (let ((font-default (load-font "resources/dejavu.fnt")) ; Requires "resources/dejavu.png"
          (font-asian (load-font "resources/noto_cjk.fnt")) ; Requires "resources/noto_cjk.png"
          (font-emoji (load-font "resources/symbola.fnt")) ; Requires "resources/symbola.png"

          (hovered-pos (vec2 0.0 0.0))
          (selected-pos (vec2 0.0 0.0)))

      ;; Set a random set of emojis when starting up
      (randomize-emoji)

      (set-target-fps 60)               ; Set our game to run at 60 frames-per-second
      ;;--------------------------------------------------------------------------------------

      ;; Main loop
      (loop until (window-should-close) ; Detect window close button or ESC key
            do ;; Update
               ;;----------------------------------------------------------------------------------
               ;; Add a new set of emojis when SPACE is pressed
               (when (is-key-pressed +key-space+) (randomize-emoji))

               ;; Set the selected emoji
               (when (and (is-mouse-button-pressed +mouse-button-left+) (/= *hovered* -1) (/= *hovered* *selected*))
                 (setf *selected* *hovered*
                       selected-pos hovered-pos))

               (let ((mouse (get-mouse-position))
                     (position (vec2 28.8 10.0)))
                 (setf *hovered* -1)
                 ;;----------------------------------------------------------------------------------

                 ;; Draw
                 ;;----------------------------------------------------------------------------------
                 (begin-drawing)

                 (clear-background +raywhite+)

                 ;; Draw random emojis in the background
                 ;;------------------------------------------------------------------------------
                 (dotimes (i (length *emoji*))
                   (let* ((e (aref *emoji* i))
                          (txt (aref *emoji-codepoints* (emoji-index e)))
                          (emoji-rect (make-rectangle :x (vx position) :y (vy position)
                                                      :width (float (font-base-size font-emoji)) :height (float (font-base-size font-emoji)))))

                     (if (not (check-collision-point-rec mouse emoji-rect))
                         (draw-text-ex font-emoji txt position (float (font-base-size font-emoji)) 1.0 (if (= *selected* i) (emoji-color e) (fade +lightgray+ 0.4)))
                         (progn
                           (draw-text-ex font-emoji txt position (float (font-base-size font-emoji)) 1.0 (emoji-color e))
                           (setf *hovered* i
                                 hovered-pos (vcopy position))))

                     (if (and (/= i 0) (= (mod i +emoji-per-width+) 0))
                         (setf (vy position) (+ (vy position) (font-base-size font-emoji) 24.25)
                               (vx position) 28.8)
                         (incf (vx position) (+ (font-base-size font-emoji) 28.8)))))
                 ;;------------------------------------------------------------------------------

                 ;; Draw the message when a emoji is selected
                 ;;------------------------------------------------------------------------------
                 (when (/= *selected* -1)
                   (let* ((message (emoji-message (aref *emoji* *selected*)))
                          (message-text (first (aref *messages* message)))
                          (language (second (aref *messages* message)))
                          (horizontal-padding 20)
                          (vertical-padding 30)
                          (font font-default))

                     ;; Set correct font for asian languages
                     (when (or (text-is-equal language "Chinese")
                               (text-is-equal language "Korean")
                               (text-is-equal language "Japanese"))
                       (setf font font-asian))

                     ;; Calculate size for the message box (approximate the height and width)
                     (let ((sz (measure-text-ex font message-text (float (font-base-size font)) 1.0)))
                       (cond ((> (vx sz) 300) (setf (vy sz) (* (vy sz) (/ (vx sz) 300))
                                                    (vx sz) 300.0))
                             ((< (vx sz) 160) (setf (vx sz) 160.0)))

                       (let* ((msg-rect (make-rectangle :x (- (vx selected-pos) 38.8) :y (vy selected-pos)
                                                        :width (+ (* 2 horizontal-padding) (vx sz)) :height (+ (* 2 vertical-padding) (vy sz))))
                              (a nil) (b nil) (c nil))
                         (decf (rectangle-y msg-rect) (rectangle-height msg-rect))

                         ;; Coordinates for the chat bubble triangle
                         (setf a (vec2 (vx selected-pos) (+ (rectangle-y msg-rect) (rectangle-height msg-rect)))
                               b (vec2 (+ (vx a) 8) (+ (vy a) 10))
                               c (vec2 (+ (vx a) 10) (vy a)))

                         ;; Don't go outside the screen
                         (when (< (rectangle-x msg-rect) 10) (incf (rectangle-x msg-rect) 28))
                         (when (< (rectangle-y msg-rect) 10)
                           (setf (rectangle-y msg-rect) (+ (vy selected-pos) 84))
                           (setf (vy a) (rectangle-y msg-rect)
                                 (vy c) (vy a)
                                 (vy b) (- (vy a) 10))

                           ;; Swap values so we can actually render the triangle :(
                           (rotatef a b))

                         (when (> (+ (rectangle-x msg-rect) (rectangle-width msg-rect)) screen-width)
                           (decf (rectangle-x msg-rect) (+ (- (+ (rectangle-x msg-rect) (rectangle-width msg-rect)) screen-width) 10)))

                         ;; Draw chat bubble
                         (draw-rectangle-rec msg-rect (emoji-color (aref *emoji* *selected*)))
                         (draw-triangle a b c (emoji-color (aref *emoji* *selected*)))

                         ;; Draw the main text message
                         (let ((text-rect (make-rectangle :x (+ (rectangle-x msg-rect) (/ (float horizontal-padding) 2)) :y (+ (rectangle-y msg-rect) (/ (float vertical-padding) 2))
                                                          :width (- (rectangle-width msg-rect) horizontal-padding) :height (rectangle-height msg-rect))))
                           (draw-text-boxed font message-text text-rect (float (font-base-size font)) 1.0 t +white+)

                           ;; Draw the info text below the main message
                           (let* ((size (length (babel:string-to-octets message-text :encoding :utf-8)))
                                  (length (get-codepoint-count message-text))
                                  (info (text-format "%s %u characters %i bytes" language length size)))
                             (setf sz (measure-text-ex (get-font-default) info 10 1.0))

                             (draw-text info (truncate (- (+ (rectangle-x text-rect) (rectangle-width text-rect)) (vx sz)))
                                        (truncate (- (+ (rectangle-y msg-rect) (rectangle-height msg-rect)) (vy sz) 2)) 10 +raywhite+)))))))
                 ;;------------------------------------------------------------------------------

                 ;; Draw the info text
                 (draw-text "These emojis have something to tell you, click each to find out!" (floor (- screen-width 650) 2) (- screen-height 40) 20 +gray+)
                 (draw-text "Each emoji is a unicode character from a font, not a texture... Press [SPACEBAR] to refresh" (floor (- screen-width 484) 2) (- screen-height 16) 10 +gray+)

                 (end-drawing)))
      ;;----------------------------------------------------------------------------------

      ;; De-Initialization
      ;;--------------------------------------------------------------------------------------
      (unload-font font-default)        ; Unload font resource
      (unload-font font-asian)          ; Unload font resource
      (unload-font font-emoji)          ; Unload font resource

      (close-window))))                 ; Close window and OpenGL context

(main)

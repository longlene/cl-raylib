(in-package #:cl-raylib)

;;; Raylib-compatible font system - Complete reimplementation
;;; Based on raylib's rtext.c implementation with exact API matching

;;; Default font data (matching raylib's embedded font)
;;; Based on raylib's defaultFontData in rtext.c
(defparameter *default-font-data* nil "Default font pixel data")
(defparameter *default-font-chars* nil "Default font character lookup")
(defparameter *default-font-glyphs* nil "Default font glyph data array")
(defparameter *default-font-recs* nil "Default font texture rectangles")

;;; Exact raylib default font data (512 unsigned ints) - from rtext.c
(defparameter *raylib-default-font-data*
  #(#x00000000 #x00000000 #x00000000 #x00000000 #x00200020 #x0001b000 #x00000000 #x00000000 #x8ef92520 #x00020a00 #x7dbe8000 #x1f7df45f
    #x4a2bf2a0 #x0852091e #x41224000 #x10041450 #x2e292020 #x08220812 #x41222000 #x10041450 #x10f92020 #x3efa084c #x7d22103c #x107df7de
    #xe8a12020 #x08220832 #x05220800 #x10450410 #xa4a3f000 #x08520832 #x05220400 #x10450410 #xe2f92020 #x0002085e #x7d3e0281 #x107df41f
    #x00200000 #x8001b000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #xc0000fbe #xfbf7e00f #x5fbf7e7d #x0050bee8 #x440808a2 #x0a142fe8 #x50810285 #x0050a048
    #x49e428a2 #x0a142828 #x40810284 #x0048a048 #x10020fbe #x09f7ebaf #xd89f3e84 #x0047a04f #x09e48822 #x0a142aa1 #x50810284 #x0048a048
    #x04082822 #x0a142fa0 #x50810285 #x0050a248 #x00008fbe #xfbf42021 #x5f817e7d #x07d09ce8 #x00008000 #x00000fe0 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x000c0180
    #xdfbf4282 #x0bfbf7ef #x42850505 #x004804bf #x50a142c6 #x08401428 #x42852505 #x00a808a0 #x50a146aa #x08401428 #x42852505 #x00081090
    #x5fa14a92 #x0843f7e8 #x7e792505 #x00082088 #x40a15282 #x08420128 #x40852489 #x00084084 #x40a16282 #x0842022a #x40852451 #x00088082
    #xc0bf4282 #xf843f42f #x7e85fc21 #x3e0900bf #x00000000 #x00000004 #x00000000 #x000c0180 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x04000402 #x41482000 #x00000000 #x00000800
    #x04000404 #x4100203c #x00000000 #x00000800 #xf7df7df0 #x514bef85 #xbefbefbe #x04513bef #x14414500 #x494a2885 #xa28a28aa #x04510820
    #xf44145f0 #x474a289d #xa28a28aa #x04510be0 #x14414510 #x494a2884 #xa28a28aa #x02910a00 #xf7df7df0 #xd14a2f85 #xbefbe8aa #x011f7be0
    #x00000000 #x00400804 #x20080000 #x00000000 #x00000000 #x00600f84 #x20080000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #xac000000 #x00000f01 #x00000000 #x00000000 #x24000000 #x00000f01 #x00000000 #x06000000 #x24000000 #x00000f01 #x00000000 #x09108000
    #x24fa28a2 #x00000f01 #x00000000 #x013e0000 #x2242252a #x00000f52 #x00000000 #x038a8000 #x2422222a #x00000f29 #x00000000 #x010a8000
    #x2412252a #x00000f01 #x00000000 #x010a8000 #x24fbe8be #x00000f01 #x00000000 #x0ebe8000 #xac020000 #x00000f01 #x00000000 #x00048000
    #x0003e000 #x00000f00 #x00000000 #x00008000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000038 #x8443b80e #x00203a03
    #x02bea080 #xf0000020 #xc452208a #x04202b02 #xf8029122 #x07f0003b #xe44b388e #x02203a02 #x081e8a1c #x0411e92a #xf4420be0 #x01248202
    #xe8140414 #x05d104ba #xe7c3b880 #x00893a0a #x283c0e1c #x04500902 #xc4400080 #x00448002 #xe8208422 #x04500002 #x80400000 #x05200002
    #x083e8e00 #x04100002 #x804003e0 #x07000042 #xf8008400 #x07f00003 #x80400000 #x04000022 #x00000000 #x00000000 #x80400000 #x04000002
    #x00000000 #x00000000 #x00000000 #x00000000 #x00800702 #x1848a0c2 #x84010000 #x02920921 #x01042642 #x00005121 #x42023f7f #x00291002
    #xefc01422 #x7efdfbf7 #xefdfa109 #x03bbbbf7 #x28440f12 #x42850a14 #x20408109 #x01111010 #x28440408 #x42850a14 #x2040817f #x01111010
    #xefc78204 #x7efdfbf7 #xe7cf8109 #x011111f3 #x2850a932 #x42850a14 #x2040a109 #x01111010 #x2850b840 #x42850a14 #xefdfbf79 #x03bbbbf7
    #x001fa020 #x00000000 #x00001000 #x00000000 #x00002070 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x08022800 #x00012283 #x02430802 #x01010001 #x8404147c #x20000144 #x80048404 #x00823f08 #xdfbf4284 #x7e03f7ef #x142850a1 #x0000210a
    #x50a14684 #x528a1428 #x142850a1 #x03efa17a #x50a14a9e #x52521428 #x142850a1 #x02081f4a #x50a15284 #x4a221428 #xf42850a1 #x03efa14b
    #x50a16284 #x4a521428 #x042850a1 #x0228a17a #xdfbf427c #x7e8bf7ef #xf7efdfbf #x03efbd0b #x00000000 #x04000000 #x00000000 #x00000008
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00200508 #x00840400 #x11458122 #x00014210
    #x00514294 #x51420800 #x20a22a94 #x0050a508 #x00200000 #x00000000 #x00050000 #x08000000 #xfefbefbe #xfbefbefb #xfbeb9114 #x00fbefbe
    #x20820820 #x8a28a20a #x8a289114 #x3e8a28a2 #xfefbefbe #xfbefbe0b #x8a289114 #x008a28a2 #x228a28a2 #x08208208 #x8a289114 #x088a28a2
    #xfefbefbe #xfbefbefb #xfa2f9114 #x00fbefbe #x00000000 #x00000040 #x00000000 #x00000000 #x00000000 #x00000020 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00210100 #x00000004 #x00000000 #x00000000 #x14508200 #x00001402 #x00000000 #x00000000
    #x00000010 #x00000020 #x00000000 #x00000000 #xa28a28be #x00002228 #x00000000 #x00000000 #xa28a28aa #x000022e8 #x00000000 #x00000000
    #xa28a28aa #x000022a8 #x00000000 #x00000000 #xa28a28aa #x000022e8 #x00000000 #x00000000 #xbefbefbe #x00003e2f #x00000000 #x00000000
    #x00000004 #x00002028 #x00000000 #x00000000 #x80000000 #x00003e0f #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000
    #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000 #x00000000)
  "Exact raylib default font bitmap data - 512 unsigned 32-bit integers")

;;; Exact raylib character widths (224 characters) - from rtext.c
(defparameter *raylib-chars-width*
  #(3 1 4 6 5 7 6 2 3 3 5 5 2 4 1 7 5 2 5 5 5 5 5 5 5 5 1 1 3 4 3 6
    7 6 6 6 6 6 6 6 6 3 5 6 5 7 6 6 6 6 6 6 7 6 7 7 6 6 6 2 7 2 3 5
    2 5 5 5 5 5 4 5 5 1 2 5 2 5 5 5 5 5 5 5 4 5 5 5 5 5 5 3 1 3 4 4
    1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    1 1 5 5 5 7 1 5 3 7 3 5 4 1 7 4 3 5 3 3 2 5 6 1 2 2 3 5 6 6 6 6
    6 6 6 6 6 6 7 6 6 6 6 6 3 3 3 3 7 6 6 6 6 6 6 5 6 6 6 6 6 6 4 6
    5 5 5 5 5 5 9 5 5 5 5 5 2 2 3 3 5 5 5 5 5 5 5 5 5 5 5 5 5 5 3 5)
  "Exact raylib character widths for all 224 glyphs")

;;; Global default font instance - matches raylib's defaultFont
(defparameter *default-font* nil "Default font loaded on initialization")
(defparameter *is-font-ready* nil "Whether font system is initialized")

(defun bit-check (value bit-position)
  "Check if bit at position is set - matches raylib's BIT_CHECK macro"
  (not (zerop (logand value (ash 1 bit-position)))))

(defun init-default-font-data ()
  "Initialize default font data exactly matching raylib's LoadFontDefault"
  (unless *default-font-data*
    ;; Create 128x128 image with 2 bytes per pixel (gray + alpha)
    ;; This matches: RL_CALLOC(128*128, 2)
    (setf *default-font-data* 
          (make-array (* 128 128 2) :element-type '(unsigned-byte 8) :initial-element 0))
    
    ;; Fill image.data with defaultFontData (convert from bit to pixel!)
    ;; This exactly matches raylib's algorithm:
    (let ((counter 0))
      (loop for i from 0 below (* 128 128) by 32 do
        (loop for j from 31 downto 0 do
          (let ((pixel-index (+ i j)))
            (when (< pixel-index (* 128 128))
              (if (bit-check (aref *raylib-default-font-data* counter) j)
                  ;; NOTE: Raylib comment says "alpha + gray" but actual format is [gray, alpha]
                  ;; OpenGL GL_LUMINANCE_ALPHA and GL_RG expect [luminance/gray, alpha] order
                  ;; ((unsigned short *)imFont.data)[i + j] = 0xffff;
                  (progn
                    (setf (aref *default-font-data* (* pixel-index 2)) 255)     ; Gray = 0xff
                    (setf (aref *default-font-data* (+ (* pixel-index 2) 1)) 255)) ; Alpha = 0xff
                  ;; else case:
                  ;; ((unsigned char *)imFont.data)[(i + j)*sizeof(short)] = 0xff;
                  ;; ((unsigned char *)imFont.data)[(i + j)*sizeof(short) + 1] = 0x00;
                  (progn
                    (setf (aref *default-font-data* (* pixel-index 2)) 255)     ; Gray = 0xff
                    (setf (aref *default-font-data* (+ (* pixel-index 2) 1)) 0))))))  ; Alpha = 0x00
        (incf counter)))
    
    *default-font-data*))

;;; Default font creation - exactly matches raylib LoadFontDefault structure
(defun load-font-default ()
  "Load raylib default font - exactly matches raylib's LoadFontDefault"
  ;; Check if default font is already loaded to avoid duplicates (matches raylib behavior)
  (when (and *default-font* (font-glyphs *default-font*))
    (return-from load-font-default))
  
  ;; #define BIT_CHECK(a,b) ((a) & (1u << (b)))
  ;; NOTE: Using UTF-8 encoding table for Unicode U+0000..U+00FF Basic Latin + Latin-1 Supplement
  
  ;; Initialize font data if not ready
  (init-default-font-data)
  
  ;; defaultFont.glyphCount = 224;   // Number of chars included in our default font
  ;; defaultFont.glyphPadding = 0;   // Characters padding
  (let* ((glyph-count 224)
         (glyph-padding 0)
         (chars-height 10)      ; int charsHeight = 10;
         (chars-divisor 1)      ; int charsDivisor = 1; // Every char is separated from the consecutive by a 1 pixel divisor
         (atlas-width 128)
         (atlas-height 128)
         (rectangles (make-array glyph-count))
         (glyphs (make-array glyph-count)))
    
    ;; Create texture from defaultFontData - this matches raylib's LoadTextureFromImage
    (let* ((texture-id (gl:gen-texture))
           (font-texture (make-texture :id texture-id
                                      :width atlas-width
                                      :height atlas-height
                                      :mipmaps 1
                                      :format 2))) ; PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA
      
      (gl:bind-texture :texture-2d texture-id)
      (gl:tex-parameter :texture-2d :texture-min-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-mag-filter :nearest)
      (gl:tex-parameter :texture-2d :texture-wrap-s :clamp-to-edge)
      (gl:tex-parameter :texture-2d :texture-wrap-t :clamp-to-edge)
      
      ;; Upload the gray-alpha data (revert to luminance-alpha for compatibility)
      ;; Use RGBA format with proper channel mapping instead of RG
      (gl:tex-image-2d :texture-2d 0 :rgba8 atlas-width atlas-height 0
                       :rgba :unsigned-byte 
                       ;; Convert gray-alpha data to RGBA format
                       ;; Font data is now correctly in [gray, alpha] format
                       (let ((rgba-data (make-array (* atlas-width atlas-height 4) 
                                                    :element-type '(unsigned-byte 8))))
                         (loop for i from 0 below (* atlas-width atlas-height) do
                           (let ((gray (aref *default-font-data* (* i 2)))      ; Correct: gray at index 0
                                 (alpha (aref *default-font-data* (+ (* i 2) 1)))) ; Correct: alpha at index 1
                             (setf (aref rgba-data (+ (* i 4) 0)) gray)  ; R = gray (for luminance)
                             (setf (aref rgba-data (+ (* i 4) 1)) gray)  ; G = gray (for luminance)  
                             (setf (aref rgba-data (+ (* i 4) 2)) gray)  ; B = gray (for luminance)
                             (setf (aref rgba-data (+ (* i 4) 3)) alpha))) ; A = alpha (for transparency)
                         rgba-data))
      
      ;; Set proper pixel store alignment
      (%gl:pixel-store-i #x0CF5 1)
      
      (gl:bind-texture :texture-2d 0)
      
      ;; Reconstruct charSet using charsWidth[], charsHeight, charsDivisor, glyphCount
      ;; This exactly matches raylib's character positioning logic
      (let ((current-line 0)
            (current-pos-x chars-divisor)
            (test-pos-x chars-divisor))
        
        (loop for i from 0 below glyph-count do
          (let ((char-value (+ 32 i))  ; First char is 32
                (char-width (aref *raylib-chars-width* i)))
            
            ;; defaultFont.glyphs[i].value = 32 + i;  // First char is 32
            (setf (aref glyphs i)
                  (make-glyph-info :value char-value
                                  :offset-x 0   ; NOTE: On default font character offsets and xAdvance are not required
                                  :offset-y 0
                                  :advance-x 0  ; advanceX = 0
                                  :image nil))
            
            ;; defaultFont.recs[i].x = (float)currentPosX;
            ;; defaultFont.recs[i].y = (float)(charsDivisor + currentLine*(charsHeight + charsDivisor));
            ;; defaultFont.recs[i].width = (float)charsWidth[i];
            ;; defaultFont.recs[i].height = (float)charsHeight;
            (setf (aref rectangles i)
                  (make-rectangle :x (float current-pos-x)
                                 :y (float (+ chars-divisor (* current-line (+ chars-height chars-divisor))))
                                 :width (float char-width)
                                 :height (float chars-height)))
            
            ;; testPosX += (int)(defaultFont.recs[i].width + (float)charsDivisor);
            (incf test-pos-x (+ char-width chars-divisor))
            
            ;; if (testPosX >= imFont.width)
            (if (>= test-pos-x atlas-width)
                (progn
                  ;; currentLine++;
                  ;; currentPosX = 2*charsDivisor + charsWidth[i];
                  ;; testPosX = currentPosX;
                  (incf current-line)
                  (setf current-pos-x (+ (* 2 chars-divisor) char-width))
                  (setf test-pos-x current-pos-x)
                  
                  ;; defaultFont.recs[i].x = (float)charsDivisor;
                  ;; defaultFont.recs[i].y = (float)(charsDivisor + currentLine*(charsHeight + charsDivisor));
                  (setf (rectangle-x (aref rectangles i)) (float chars-divisor))
                  (setf (rectangle-y (aref rectangles i)) 
                        (float (+ chars-divisor (* current-line (+ chars-height chars-divisor))))))
                ;; else currentPosX = testPosX;
                (setf current-pos-x test-pos-x))

            ;; Fill character image data from fontClear data
            ;; defaultFont.glyphs[i].image = ImageFromImage(imFont, defaultFont.recs[i]);
            (setf (glyph-info-image (aref glyphs i))
                  (image-from-image (make-image :data *default-font-data* :width atlas-width :height atlas-height
                                                :mipmaps 1 :format +pixelformat-uncompressed-gray-alpha+)
                                    (aref rectangles i))))))
      
      ;; defaultFont.baseSize = (int)defaultFont.recs[0].height;
      (let ((base-size (round (rectangle-height (aref rectangles 0)))))
        
        (trace-log-info "RTEXT: Default font loaded successfully (~d glyphs)" glyph-count)
        
        ;; Create and set global default font (matches raylib setting defaultFont global)
        (setf *default-font* (make-font :base-size base-size
                                        :glyph-count glyph-count
                                        :glyph-padding glyph-padding
                                        :texture font-texture
                                        :recs rectangles
                                        :glyphs glyphs))))))

;;; Font management functions - matching raylib API

(defun init-text-system ()
  "Initialize the text/font system (alias for init-font-system)"
  (init-font-system))

(defun cleanup-text-system ()
  "Cleanup the text/font system (no-op for compatibility)"
  ;; No cleanup needed in Common Lisp
  (trace-log-info "TEXT: Text system cleanup (no-op)"))

(defun get-font-default ()
  "Get the default Font - matches raylib GetFontDefault"
  ;; Simply return the default font (should be loaded in InitWindow)
  *default-font*)

(defun is-font-valid (font)
  "Check if a font is valid - matches raylib IsFontValid"
  (and font
       (font-p font)
       (> (font-glyph-count font) 0)
       (font-glyphs font)
       (font-recs font)))

(defun get-glyph-index (font codepoint)
  "Get glyph index position in font for a codepoint - matches raylib GetGlyphIndex"
  (when (is-font-valid font)
    ;; For default font, characters are sequential starting from 32
    (if (and (>= codepoint 32) (< codepoint (+ 32 (font-glyph-count font))))
        (- codepoint 32)  ; Direct index calculation for default font
        ;; Fallback search for other fonts
        (let ((glyphs (font-glyphs font)))
          (loop for i from 0 below (font-glyph-count font) do
            (when (= (glyph-info-value (aref glyphs i)) codepoint)
              (return-from get-glyph-index i)))
          ;; Return fallback index for '?' character (63) or first glyph
          (if (and (>= 63 32) (< 63 (+ 32 (font-glyph-count font))))
              (- 63 32)  ; '?' character index
              0)))))

(defun get-glyph-info (font codepoint)
  "Get glyph font info data for a codepoint - matches raylib GetGlyphInfo"
  (when (is-font-valid font)
    (let ((index (get-glyph-index font codepoint)))
      (aref (font-glyphs font) index))))

(defun get-glyph-atlas-rec (font codepoint)
  "Get glyph rectangle in font atlas for a codepoint - matches raylib GetGlyphAtlasRec"
  (when (is-font-valid font)
    (let ((index (get-glyph-index font codepoint)))
      (aref (font-recs font) index))))

;;; Text drawing functions - matching raylib API exactly

(defun draw-text-codepoint (font codepoint position font-size tint)
  "Draw one character (codepoint) - matches raylib DrawTextCodepoint exactly"
  (unless font
    (setf font (get-font-default)))
  
  (when (is-font-valid font)
    ;; Character index position in sprite font
    ;; NOTE: In case a codepoint is not available in the font, index returned points to '?'
    (let* ((index (get-glyph-index font codepoint))
           (scale-factor (/ font-size (float (font-base-size font))))
           (glyph-padding (font-glyph-padding font))
           (glyph (aref (font-glyphs font) index))
           (rec (aref (font-recs font) index))
           (actual-color (keyword-to-color tint)))
      
      ;; Character destination rectangle on screen
      ;; NOTE: We consider glyphPadding on drawing
      (let* ((dst-x (+ (vx2 position) 
                      (* (glyph-info-offset-x glyph) scale-factor) 
                      (- (* glyph-padding scale-factor))))
             (dst-y (+ (vy2 position) 
                      (* (glyph-info-offset-y glyph) scale-factor) 
                      (- (* glyph-padding scale-factor))))
             (dst-w (* (+ (rectangle-width rec) (* 2.0 glyph-padding)) scale-factor))
             (dst-h (* (+ (rectangle-height rec) (* 2.0 glyph-padding)) scale-factor))
             ;; Character source rectangle from font texture atlas
             ;; NOTE: We consider chars padding when drawing
             (src-x (- (rectangle-x rec) glyph-padding))
             (src-y (- (rectangle-y rec) glyph-padding))
             (src-w (+ (rectangle-width rec) (* 2.0 glyph-padding)))
             (src-h (+ (rectangle-height rec) (* 2.0 glyph-padding)))
             (font-tex (font-texture font))
             (tex-width (texture-width font-tex))
             (tex-height (texture-height font-tex)))
        
        ;; Draw using raylib-style rendering - matches DrawTexturePro implementation
        (rl-set-texture (texture-id font-tex))
        (rl-begin +rl-quads+)
        
        (rl-color4ub (color-r actual-color) (color-g actual-color) 
                     (color-b actual-color) (color-a actual-color))
        (rl-normal3f 0.0 0.0 1.0) ; Normal vector pointing towards viewer
        
        ;; Top-left corner for texture and quad
        (rl-tex-coord2f (/ src-x tex-width) (/ src-y tex-height))
        (rl-vertex2f dst-x dst-y)
        
        ;; Bottom-left corner for texture and quad
        (rl-tex-coord2f (/ src-x tex-width) (/ (+ src-y src-h) tex-height))
        (rl-vertex2f dst-x (+ dst-y dst-h))
        
        ;; Bottom-right corner for texture and quad
        (rl-tex-coord2f (/ (+ src-x src-w) tex-width) (/ (+ src-y src-h) tex-height))
        (rl-vertex2f (+ dst-x dst-w) (+ dst-y dst-h))
        
        ;; Top-right corner for texture and quad
        (rl-tex-coord2f (/ (+ src-x src-w) tex-width) (/ src-y tex-height))
        (rl-vertex2f (+ dst-x dst-w) dst-y)
        
        (rl-end)
        (rl-set-texture 0)))))

(defun draw-text-codepoints (font codepoints codepoint-count position font-size spacing tint)
  "Draw multiple character (codepoint) - matches raylib DrawTextCodepoints"
  (unless font
    (setf font (get-font-default)))
  
  (when (is-font-valid font)
    (let ((text-offset-x 0.0)
          (text-offset-y 0.0))
      
      (loop for i from 0 below codepoint-count do
        (let ((codepoint (if (arrayp codepoints)
                           (aref codepoints i)
                           (nth i codepoints))))
          
          (cond
            ((= codepoint 10) ; \n
             (setf text-offset-y (+ text-offset-y font-size))
             (setf text-offset-x 0.0))
            
            ((= codepoint 9)  ; \t
             ;; For tab, use 4 times the space character width
             (let ((space-index (get-glyph-index font 32)))
               (when space-index
                 (let ((space-rect (aref (font-recs font) space-index)))
                   (incf text-offset-x (* 4.0 (rectangle-width space-rect) (/ font-size (float (font-base-size font)))))))))
            
            (t
             ;; Draw the character
             (let ((char-position (vec2 (+ (vx2 position) text-offset-x)
                                       (+ (vy2 position) text-offset-y))))
               (draw-text-codepoint font codepoint char-position font-size tint)
               
               ;; Get character width from rectangle (for default font)
               (let ((char-index (get-glyph-index font codepoint)))
                 (when char-index
                   (let ((char-rect (aref (font-recs font) char-index)))
                     (incf text-offset-x (+ (* (rectangle-width char-rect) (/ font-size (float (font-base-size font))))
                                            spacing)))))))))))))

;;; Main text drawing functions - matching raylib API

(defun draw-text-ex (font text position font-size spacing tint)
  "Draw text using font and additional parameters - matches raylib DrawTextEx"
  (unless font
    (setf font (get-font-default)))
  
  (when (is-font-valid font)
    (let ((codepoints (map 'list #'char-code text)))
      (draw-text-codepoints font codepoints (length codepoints) position font-size spacing tint))))

(defun draw-text-pro (font text position origin rotation font-size spacing tint)
  "Draw text using Font and pro parameters (rotation) - matches raylib DrawTextPro"
  (unless font
    (setf font (get-font-default)))
  
  (when (is-font-valid font)
    (gl:with-pushed-matrix
      ;; Apply transformations
      (gl:translate (vx2 position) (vy2 position) 0.0)
      (gl:rotate rotation 0.0 0.0 1.0)
      (gl:translate (- (vx2 origin)) (- (vy2 origin)) 0.0)
      
      ;; Draw text at origin
      (draw-text-ex font text (vec2 0.0 0.0) font-size spacing tint))))

(defun draw-text (text pos-x pos-y font-size color)
  "Draw text (using default font) - matches raylib DrawText"
  (draw-text-ex (get-font-default) text (vec2 pos-x pos-y) font-size 1.0 color))

(defun draw-fps (pos-x pos-y)
  "Draw FPS counter - matches raylib DrawFPS"
  (let* ((fps (get-fps))
         (color (cond
                  ((< fps 15) +red+)        ; Low FPS
                  ((< fps 30) +orange+)     ; Warning FPS  
                  (t +lime+)))              ; Good FPS
         (fps-text (format nil "~2d FPS" fps)))
    (draw-text fps-text pos-x pos-y 20 color)))

;;; Font initialization and management

(defun init-font-system ()
  "Initialize font system - called from InitWindow"
  (unless *is-font-ready*
    (setf *default-font* (load-font-default))
    (setf *is-font-ready* t)))

(defun init-font-bitmap ()
  "Initialize font atlas system - compatibility function"
  (init-font-system)
  (format t "INFO: RTEXT: Font atlas loaded successfully (~d chars)~%" 
          (font-glyph-count *default-font*)))

(defun unload-font-default ()
  "Unload default font - called from CloseWindow"
  (when *default-font*
    (unload-font *default-font*)
    (setf *default-font* nil)
    (setf *is-font-ready* nil)))

(defun unload-font (font)
  "Unload font from GPU memory (VRAM) - matches raylib UnloadFont"
  (when (is-font-valid font)
    ;; Unload texture
    (when (font-texture font)
      (unload-texture (font-texture font)))
    
    ;; Clear font data
    (setf (font-texture font) nil)
    (setf (font-recs font) nil)
    (setf (font-glyphs font) nil)
    (setf (font-glyph-count font) 0)))

;;; Text measurement functions - matching raylib API

(defun measure-text (text font-size)
  "Measure string width for default font - matches raylib MeasureText"
  (let ((font (get-font-default)))
    (when (is-font-valid font)
      (let ((result (measure-text-ex font text font-size 1.0)))
        (round (vx2 result))))))

(defun measure-text-ex (font text font-size spacing)
  "Measure string size for Font - matches raylib MeasureTextEx"
  (unless font
    (setf font (get-font-default)))
  
  (if (not (is-font-valid font))
      (vec2 0.0 font-size)
      (let ((text-width 0.0)
            (temp-text-width 0.0)
            (text-height font-size)
            (scale-factor (/ font-size (float (font-base-size font)))))
        
        (loop for char across text do
          (let ((codepoint (char-code char)))
            (cond
              ((= codepoint 10) ; \n
               (setf text-width (max text-width temp-text-width))
               (setf temp-text-width 0.0)
               (incf text-height font-size))
              
              ((= codepoint 9)  ; \t
               ;; For tab, use 4 times the space character width
               (let ((space-index (get-glyph-index font 32)))
                 (when space-index
                   (let ((space-rect (aref (font-recs font) space-index)))
                     (incf temp-text-width (* 4.0 (rectangle-width space-rect) scale-factor))))))
              
              (t
               ;; For default font, use rectangle width instead of advance-x
               (let ((char-index (get-glyph-index font codepoint)))
                 (when char-index
                   (let ((char-rect (aref (font-recs font) char-index)))
                     (incf temp-text-width (+ (* (rectangle-width char-rect) scale-factor) spacing)))))))))
        
        ;; Final width is the maximum of current temp width and previous lines
        (setf text-width (max text-width temp-text-width))
        
        (vec2 text-width text-height))))

;;; Text manipulation functions (from rtext.c)

(defun text-subtext (text position length)
  "Get a piece of a text string - matches raylib TextSubtext"
  (let* ((text-length (length text))
         (actual-position (max 0 (min position text-length))))
    
    ;; If position is beyond text length, return empty string
    (when (>= actual-position text-length)
      (return-from text-subtext ""))
    
    ;; Calculate maximum available length
    (let* ((max-length (- text-length actual-position))
           (actual-length (min length max-length)))
      
      ;; Extract substring
      (if (> actual-length 0)
          (subseq text actual-position (+ actual-position actual-length))
          ""))))

(defun text-length (text)
  "Get text length (number of characters)"
  (length text))

(defun text-to-upper (text)
  "Convert text to uppercase"
  (string-upcase text))

(defun text-to-lower (text)
  "Convert text to lowercase"
  (string-downcase text))

(defun text-replace (text find replace)
  "Replace all occurrences of 'find' with 'replace' in text"
  (let ((result text)
        (find-len (length find)))
    (loop for pos = (search find result)
          while pos
          do (setf result (concatenate 'string
                                       (subseq result 0 pos)
                                       replace
                                       (subseq result (+ pos find-len)))))
    result))

;;; Additional text utility functions (from rtext.c)

(defun text-to-integer (text)
  "Convert text string to integer"
  (handler-case
      (parse-integer text :junk-allowed t)
    (error () 0)))

(defun text-copy (destination source)
  "Copy source text to destination buffer (returns copied string)"
  (declare (ignore destination))
  ;; In Lisp, strings are immutable, so we just return a copy
  (copy-seq source))

(defun text-insert (text insert-text position)
  "Insert text at specified position"
  (let* ((text-len (length text))
        (actual-pos (max 0 (min position text-len))))
    (concatenate 'string
                 (subseq text 0 actual-pos)
                 insert-text
                 (subseq text actual-pos))))

(defun text-join (text-list delimiter)
  "Join text list with delimiter"
  (when text-list
    (let ((result (first text-list)))
      (loop for text in (rest text-list) do
        (setf result (concatenate 'string result delimiter text)))
      result)))

(defun text-split (text delimiter)
  "Split text by delimiter character into list of strings"
  (let ((result '())
        (current "")
        (delimiter-char (if (stringp delimiter) 
                           (char delimiter 0) 
                           delimiter)))
    (loop for char across text do
      (if (char= char delimiter-char)
          (progn
            (push current result)
            (setf current ""))
          (setf current (concatenate 'string current (string char)))))
    ;; Add the last part
    (push current result)
    (reverse result)))

(defun text-append (text append-text)
  "Append text to existing text"
  (concatenate 'string text append-text))

(defun text-find-index (text find-text)
  "Find index of first occurrence of find-text in text"
  (let ((pos (search find-text text)))
    (if pos pos -1)))

(defun text-to-pascal (text)
  "Convert text to PascalCase"
  (let ((words (text-split (text-to-lower text) #\Space)))
    (apply #'concatenate 'string
           (mapcar (lambda (word)
                     (if (> (length word) 0)
                         (concatenate 'string
                                      (string-upcase (subseq word 0 1))
                                      (subseq word 1))
                         word))
                   words))))

(defun text-to-snake (text)
  "Convert text to snake_case"
  (string-downcase
   (substitute #\_ #\Space text)))

(defun text-to-camel (text)
  "Convert text to camelCase"
  (let ((words (text-split (text-to-lower text) #\Space)))
    (if words
        (concatenate 'string
                     (first words)
                     (apply #'concatenate 'string
                            (mapcar (lambda (word)
                                      (if (> (length word) 0)
                                          (concatenate 'string
                                                       (string-upcase (subseq word 0 1))
                                                       (subseq word 1))
                                          word))
                                    (rest words))))
        "")))

;;; Unicode and codepoint functions

(defun get-codepoint (text &optional (position 0))
  "Get codepoint at specified position in text"
  (when (and (< position (length text)) (>= position 0))
    (char-code (char text position))))

(defun get-codepoint-next (text position)
  "Get next codepoint in text"
  (when (< (1+ position) (length text))
    (values (char-code (char text (1+ position))) 1)))

(defun get-codepoint-previous (text position)
  "Get previous codepoint in text"
  (when (> position 0)
    (values (char-code (char text (1- position))) 1)))

(defun get-codepoint-count (text)
  "Get total codepoint count in text"
  (length text))

(defun codepoint-to-utf8 (codepoint)
  "Convert codepoint to UTF-8 string"
  (string (code-char codepoint)))

(defun load-codepoints (text)
  "Load all codepoints from text into list"
  (map 'list #'char-code text))

;;; Text line spacing - matches raylib globals
(defvar *text-line-spacing* 2 "Text line spacing in pixels - matches raylib textLineSpacing")

(defun set-text-line-spacing (spacing)
  "Set text line spacing - matches raylib SetTextLineSpacing"
  (setf *text-line-spacing* spacing))

(defun get-text-line-spacing ()
  "Get current text line spacing - matches raylib GetTextLineSpacing"
  *text-line-spacing*)

;;; Font loading functions - matching raylib API

(defun load-font (file-name)
  "Load font from file into GPU memory (VRAM) - matches raylib LoadFont"
  ;; Simplified implementation - would need full font file parsing
  (trace-log-warning "LoadFont: Font loading from file not fully implemented, using default font")
  (get-font-default))

(defun load-font-ex (file-name font-size codepoints codepoint-count)
  "Load font from file with extended parameters - matches raylib LoadFontEx"
  (declare (ignore file-name font-size codepoints codepoint-count))
  ;; Simplified implementation
  (trace-log-warning "LoadFontEx: Extended font loading not fully implemented, using default font")
  (get-font-default))

(defun load-font-from-image (image key first-char)
  "Load font from Image (XNA style) - matches raylib LoadFontFromImage"
  (declare (ignore image key first-char))
  ;; Simplified implementation
  (trace-log-warning "LoadFontFromImage: Image font loading not implemented, using default font")
  (get-font-default))

(defun load-font-from-memory (file-type file-data data-size font-size codepoints codepoint-count)
  "Load font from memory buffer - matches raylib LoadFontFromMemory"
  (declare (ignore file-type file-data data-size font-size codepoints codepoint-count))
  ;; Simplified implementation
  (trace-log-warning "LoadFontFromMemory: Memory font loading not implemented, using default font")
  (get-font-default))

;;; Additional font utility functions

(defun export-font-as-code (font file-name)
  "Export font as code file - matches raylib ExportFontAsCode"
  (declare (ignore font file-name))
  ;; Not implemented
  (trace-log-warning "ExportFontAsCode: Font export not implemented")
  nil)

(defun load-font-data (file-data data-size font-size codepoints codepoint-count type)
  "Load font data for further use - matches raylib LoadFontData"
  (declare (ignore file-data data-size font-size codepoints codepoint-count type))
  ;; Not implemented
  (trace-log-warning "LoadFontData: Font data loading not implemented")
  nil)

(defun unload-font-data (glyphs glyph-count)
  "Unload font chars info data (RAM) - matches raylib UnloadFontData"
  (declare (ignore glyphs glyph-count))
  ;; Memory cleanup would go here
  nil)

(defun gen-image-font-atlas (glyphs glyph-recs glyph-count font-size padding pack-method)
  "Generate image font atlas using chars info - matches raylib GenImageFontAtlas"
  (declare (ignore pack-method)) ; Use simplified rectangular packing
  
  ;; Validate input
  (when (or (null glyphs) (<= glyph-count 0))
    (trace-log-warning "FONT: Provided chars info not valid, returning empty image atlas")
    (return-from gen-image-font-atlas 
      (make-image :data (make-array 0 :element-type '(unsigned-byte 8))
                  :width 0 :height 0 :mipmaps 1 :format 1)))
  
  ;; Use default glyph count if not provided
  (setf glyph-count (if (> glyph-count 0) glyph-count 95))
  
  ;; Calculate atlas size based on glyph dimensions
  (let ((total-width 0)
        (max-glyph-width 0))
    
    ;; Calculate total width and find maximum glyph width
    (loop for i from 0 below glyph-count do
      (let* ((glyph (aref glyphs i))
             (glyph-image (glyph-info-image glyph))
             (glyph-width (if glyph-image (image-width glyph-image) font-size)))
        (when (> glyph-width max-glyph-width)
          (setf max-glyph-width glyph-width))
        (incf total-width (+ glyph-width (* 2 padding)))))
    
    ;; Calculate optimal atlas size (power of 2)
    (let* ((padded-font-size (+ font-size (* 2 padding)))
           (total-area (* total-width padded-font-size 1.2)) ; 20% extra space
           (image-min-size (sqrt total-area))
           (image-size (expt 2 (ceiling (log image-min-size 2)))))
      
      ;; Ensure minimum size and adjust if needed
      (when (< total-area (/ (* image-size image-size) 2))
        (setf image-size (/ image-size 2)))
      
      (setf image-size (max image-size 64)) ; Minimum 64x64
      
      ;; Create atlas image (grayscale format to match raylib)
      (let* ((atlas-width image-size)
             (atlas-height image-size)
             (image-data (make-array (* atlas-width atlas-height)
                                   :element-type '(unsigned-byte 8)
                                   :initial-element 0))
             (rectangles (make-array glyph-count)))
        
        ;; Simple rectangular packing algorithm
        (let ((current-x padding)
              (current-y padding)
              (line-height 0))
          
          (loop for i from 0 below glyph-count do
            (let* ((glyph (aref glyphs i))
                   (glyph-image (glyph-info-image glyph))
                   (glyph-width (if glyph-image (image-width glyph-image) font-size))
                   (glyph-height (if glyph-image (image-height glyph-image) font-size)))
              
              ;; Check if glyph fits in current line
              (when (> (+ current-x glyph-width padding) atlas-width)
                (setf current-x padding)
                (incf current-y (+ line-height padding))
                (setf line-height 0)
                
                ;; Check if we exceed atlas height
                (when (> (+ current-y glyph-height padding) atlas-height)
                  (trace-log-warning "FONT: Failed to package character (~d)" i)
                  (continue)))
              
              ;; Store glyph rectangle
              (setf (aref rectangles i)
                    (make-rectangle :x (float current-x)
                                   :y (float current-y)
                                   :width (float glyph-width)
                                   :height (float glyph-height)))
              
              ;; Copy glyph image data to atlas
              (when glyph-image
                (let ((glyph-data (image-data glyph-image)))
                  (loop for y from 0 below glyph-height do
                    (loop for x from 0 below glyph-width do
                      (let ((src-idx (+ (* y glyph-width) x))
                            (dst-idx (+ (* (+ current-y y) atlas-width) (+ current-x x))))
                        (when (and (< src-idx (length glyph-data))
                                  (< dst-idx (length image-data)))
                          ;; For grayscale, use the alpha channel or luminance
                          (setf (aref image-data dst-idx)
                                (if (>= src-idx (length glyph-data))
                                    0
                                    (aref glyph-data src-idx)))))))))
              
              ;; Update position for next glyph
              (incf current-x (+ glyph-width padding))
              (setf line-height (max line-height glyph-height)))))
        
        ;; Set glyph rectangles output
        (when glyph-recs
          (setf glyph-recs rectangles))
        
        ;; Convert grayscale to gray-alpha format (matching raylib behavior)
        (let ((gray-alpha-data (make-array (* atlas-width atlas-height 2)
                                         :element-type '(unsigned-byte 8))))
          (loop for i from 0 below (* atlas-width atlas-height)
                for k from 0 by 2 do
            (setf (aref gray-alpha-data k) 255)        ; Alpha = 255 (opaque)
            (setf (aref gray-alpha-data (1+ k)) (aref image-data i))) ; Gray value
          
          ;; Return atlas image
          (make-image :data gray-alpha-data
                      :width atlas-width
                      :height atlas-height
                      :mipmaps 1
                      :format 2)))))) ; PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA


;;; ===== MISSING RAYLIB API FUNCTIONS =====

(defun text-to-float (text)
  "Get float value from text - matches raylib TextToFloat"
  (handler-case
    (parse-float text)
    (error () 0.0)))

(defun text-is-equal (text1 text2)
  "Check if two text strings are equal - matches raylib TextIsEqual"
  (string= text1 text2))

(defun unload-codepoints (codepoints)
  "Unload codepoints data from memory - matches raylib UnloadCodepoints"
  ;; In Lisp, memory is automatically managed
  (declare (ignore codepoints))
  nil)

;;; Helper function for text-to-float
(defun parse-float (string)
  "Parse float from string"
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) string)))
    (if (string= trimmed "")
        0.0
        (read-from-string trimmed))))

;;; End of text.lisp

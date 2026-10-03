(in-package #:cl-raylib)

;;;===================================================================================
;;; jar_mod - MOD module player
;;; Port of raylib/src/external/jar_mod.h (based on HxCMOD), used by raudio
;;;
;;; NOTE: Like in C, patterns and samples are read directly from the module data,
;;; reads past the end of the data return 0
;;; NOTE: The tracker state buffer (jar_mod_tracker_buffer_state) is not ported
;;;===================================================================================

(defconstant +jar-mod-num-max-channels+ 32)
(defconstant +jar-mod-max-notes+ (* 12 12))
(defconstant +jar-mod-default-sample-rate+ 48000)
(defconstant +jar-mod-period-table-length+ +jar-mod-max-notes+)
(defconstant +jar-mod-full-period-table-length+ (* +jar-mod-period-table-length+ 8))

;; Effects
(defconstant +mod-effect-arpeggio+ #x0)
(defconstant +mod-effect-portamento-up+ #x1)
(defconstant +mod-effect-portamento-down+ #x2)
(defconstant +mod-effect-tone-portamento+ #x3)
(defconstant +mod-effect-vibrato+ #x4)
(defconstant +mod-effect-volslide-toneporta+ #x5)
(defconstant +mod-effect-volslide-vibrato+ #x6)
(defconstant +mod-effect-set-offset+ #x9)
(defconstant +mod-effect-volume-slide+ #xa)
(defconstant +mod-effect-jump-position+ #xb)
(defconstant +mod-effect-set-volume+ #xc)
(defconstant +mod-effect-pattern-break+ #xd)
(defconstant +mod-effect-extended+ #xe)
(defconstant +mod-effect-e-fine-porta-up+ #x1)
(defconstant +mod-effect-e-fine-porta-down+ #x2)
(defconstant +mod-effect-e-pattern-loop+ #x6)
(defconstant +mod-effect-e-fine-volslide-up+ #xa)
(defconstant +mod-effect-e-fine-volslide-down+ #xb)
(defconstant +mod-effect-e-note-cut+ #xc)
(defconstant +mod-effect-e-pattern-delay+ #xe)

(alexandria:define-constant +jar-mod-period-table+
  #(27392 25856 24384 23040 21696 20480 19328 18240 17216 16256 15360 14496
    13696 12928 12192 11520 10848 10240  9664  9120  8606  8128  7680  7248
     6848  6464  6096  5760  5424  5120  4832  4560  4304  4064  3840  3624
     3424  3232  3048  2880  2712  2560  2416  2280  2152  2032  1920  1812
     1712  1616  1524  1440  1356  1280  1208  1140  1076  1016   960   906
      856   808   762   720   678   640   604   570   538   508   480   453
      428   404   381   360   339   320   302   285   269   254   240   226
      214   202   190   180   170   160   151   143   135   127   120   113
      107   101    95    90    85    80    75    71    67    63    60    56
       53    50    47    45    42    40    37    35    33    31    30    28
       27    25    24    22    21    20    19    18    17    16    15    14
       13    13    12    11    11    10     9     9     8     8     7     7)
  :test #'equalp)

(alexandria:define-constant +jar-mod-sin-table+
  #(0 24 49 74 97 120 141 161 180 197 212 224 235 244 250 253
    255 253 250 244 235 224 212 197 180 161 141 120 97 74 49 24)
  :test #'equalp)

(alexandria:define-constant +jar-mod-list+
  '(("M!K!" . 4) ("M.K." . 4) ("FLT4" . 4) ("FLT8" . 8) ("4CHN" . 4) ("6CHN" . 6) ("8CHN" . 8)
    ("10CH" . 10) ("12CH" . 12) ("14CH" . 14) ("16CH" . 16) ("18CH" . 18) ("20CH" . 20) ("22CH" . 22)
    ("24CH" . 24) ("26CH" . 26) ("28CH" . 28) ("30CH" . 30) ("32CH" . 32))
  :test #'equal)

(declaim (inline %mod-u64 %s16-wrap %mod-u8 %mod-u16))
(defun %mod-u64 (x) (logand x #xffffffffffffffff))
(defun %s16-wrap (x) (%i16 x))
(defun %mod-u8 (x) (logand x #xff))
(defun %mod-u16 (x) (logand x #xffff))

(defstruct (jar-mod-channel (:conc-name mc-))
  (sampdata nil)                        ; Offset of the sample data in the module data, NIL if none
  (sampnum 0) (length 0) (reppnt 0) (replen 0)
  (samppos 0)
  (period 0)
  (volume 0)
  (ticks 0)
  (effect 0) (parameffect 0) (effect-code 0)
  (decalperiod 0) (portaspeed 0) (portaperiod 0) (vibraperiod 0)
  (arpperiods (make-array 3 :initial-element 0))
  (arpindex 0)
  (oldk 0)
  (volumeslide 0) (vibraparam 0) (vibrapointeur 0) (finetune 0) (cut-param 0)
  (patternloopcnt 0) (patternloopstartpoint 0))

(defstruct (jar-mod-context (:conc-name mod-) (:constructor %make-jar-mod-context))
  (song (make-array 1084 :element-type '(unsigned-byte 8) :initial-element 0))   ; Raw module header
  (song-speed 0)
  (sample-lengths (make-array 31 :initial-element 0))
  (sample-reppnts (make-array 31 :initial-element 0))
  (sample-replens (make-array 31 :initial-element 0))
  (sampledata (make-array 31 :initial-element nil))
  (patterndata (make-array 128 :initial-element nil))
  (playrate 0)
  (tablepos 0) (patternpos 0) (patterndelay 0) (jump-loop-effect 0)
  (bpm 0)
  (patternticks 0) (patterntickse 0) (patternticksaim 0)
  (sampleticksconst 0)
  (samplenb 0)
  (channels (let ((v (make-array +jar-mod-num-max-channels+)))
              (dotimes (i +jar-mod-num-max-channels+ v) (setf (svref v i) (make-jar-mod-channel)))))
  (number-of-channels 0)
  (fullperiod (make-array (* +jar-mod-max-notes+ 8) :initial-element 0))
  (mod-loaded 0)
  (last-r-sample 0) (last-l-sample 0)
  (stereo 0) (stereo-separation 0) (bits 0) (filter 0)
  (modfile nil)                         ; The raw mod file
  (modfilesize 0)
  (loopcount 0))

;; Song header accessors
(defun %mod-song-length (ctx) (aref (mod-song ctx) 950))
(defun %mod-song-patterntable (ctx i) (aref (mod-song ctx) (+ 952 i)))
(defun %mod-sample-finetune (ctx i) (aref (mod-song ctx) (+ 20 (* i 30) 24)))
(defun %mod-sample-volume (ctx i) (aref (mod-song ctx) (+ 20 (* i 30) 25)))

(defun %mod-byte (ctx offset)
  "Read a module data byte, 0 past the end of the data"
  (let ((data (mod-modfile ctx)))
    (if (and data (>= offset 0) (< offset (length data))) (aref data offset) 0)))

(defun %mod-getnote (ctx period finetune)
  (declare (ignore finetune))
  (let ((fullperiod (mod-fullperiod ctx)))
    (dotimes (i +jar-mod-full-period-table-length+ +jar-mod-max-notes+)
      (when (>= period (aref fullperiod i))
        (return i)))))

(defun %mod-fullperiod-ref (ctx index)
  ;; NOTE: Negative indexes (low notes with negative finetune) read before the array in C
  (if (and (>= index 0) (< index (length (mod-fullperiod ctx)))) (aref (mod-fullperiod ctx) index) 0))

(defun %mod-worknote (nptr cptr ctx)
  (let* ((b0 (%mod-byte ctx nptr)) (b1 (%mod-byte ctx (+ nptr 1)))
         (b2 (%mod-byte ctx (+ nptr 2))) (b3 (%mod-byte ctx (+ nptr 3)))
         (sample (logior (logand b0 #xf0) (ash b2 -4)))
         (period (logior (ash (logand b0 #xf) 8) b1))
         (effect (logior (ash (logand b2 #xf) 8) b3))
         (operiod (mc-period cptr)))
    (when (or (/= period 0) (/= sample 0))
      (when (and (/= sample 0) (< sample 32))
        (setf (mc-sampnum cptr) (- sample 1)))
      (let ((n (mc-sampnum cptr)))
        (setf (mc-sampdata cptr) (aref (mod-sampledata ctx) n)
              (mc-length cptr) (aref (mod-sample-lengths ctx) n)
              (mc-reppnt cptr) (aref (mod-sample-reppnts ctx) n)
              (mc-replen cptr) (aref (mod-sample-replens ctx) n)
              (mc-finetune cptr) (logand (%mod-sample-finetune ctx n) #xf))
        (when (and (/= (ash effect -8) 4) (/= (ash effect -8) 6))
          (setf (mc-vibraperiod cptr) 0
                (mc-vibrapointeur cptr) 0)))
      (when (and (/= sample 0) (/= (ash effect -8) +mod-effect-volslide-toneporta+))
        (setf (mc-volume cptr) (%mod-sample-volume ctx (mc-sampnum cptr))
              (mc-volumeslide cptr) 0))
      (when (and (/= (ash effect -8) +mod-effect-tone-portamento+) (/= (ash effect -8) +mod-effect-volslide-toneporta+))
        (when (/= period 0)
          (setf (mc-samppos cptr) 0)))
      (setf (mc-decalperiod cptr) 0)
      (when (/= period 0)
        (when (/= (mc-finetune cptr) 0)
          (if (<= (mc-finetune cptr) 7)
              (setf period (%mod-fullperiod-ref ctx (+ (%mod-getnote ctx period 0) (mc-finetune cptr))))
              (setf period (%mod-fullperiod-ref ctx (- (%mod-getnote ctx period 0) (- 16 (mc-finetune cptr)))))))
        (setf (mc-period cptr) period)))
    (setf (mc-effect cptr) 0
          (mc-parameffect cptr) 0
          (mc-effect-code cptr) effect)
    (case (ash effect -8)
      (#.+mod-effect-arpeggio+
       (when (/= (logand effect #xff) 0)
         (setf (mc-effect cptr) +mod-effect-arpeggio+
               (mc-parameffect cptr) (logand effect #xff)
               (mc-arpindex cptr) 0)
         (let ((curnote (%mod-getnote ctx (mc-period cptr) (mc-finetune cptr)))
               (last (- +jar-mod-full-period-table-length+ 1)))
           (setf (aref (mc-arpperiods cptr) 0) (%s16-wrap (mc-period cptr)))
           (let ((arpnote (min last (+ curnote (* (logand (ash (mc-parameffect cptr) -4) #xf) 8)))))
             (setf (aref (mc-arpperiods cptr) 1) (%s16-wrap (aref (mod-fullperiod ctx) arpnote))))
           (let ((arpnote (min last (+ curnote (* (logand (mc-parameffect cptr) #xf) 8)))))
             (setf (aref (mc-arpperiods cptr) 2) (%s16-wrap (aref (mod-fullperiod ctx) arpnote)))))))
      (#.+mod-effect-portamento-up+
       (setf (mc-effect cptr) +mod-effect-portamento-up+
             (mc-parameffect cptr) (logand effect #xff)))
      (#.+mod-effect-portamento-down+
       (setf (mc-effect cptr) +mod-effect-portamento-down+
             (mc-parameffect cptr) (logand effect #xff)))
      (#.+mod-effect-tone-portamento+
       (setf (mc-effect cptr) +mod-effect-tone-portamento+)
       (when (/= (logand effect #xff) 0)
         (setf (mc-portaspeed cptr) (logand effect #xff)))
       (when (/= period 0)
         (setf (mc-portaperiod cptr) (%s16-wrap period)
               (mc-period cptr) operiod)))
      (#.+mod-effect-vibrato+
       (setf (mc-effect cptr) +mod-effect-vibrato+)
       (when (/= (logand effect #x0f) 0)   ; Depth continue or change?
         (setf (mc-vibraparam cptr) (logior (logand (mc-vibraparam cptr) #xf0) (logand effect #x0f))))
       (when (/= (logand effect #xf0) 0)   ; Speed continue or change?
         (setf (mc-vibraparam cptr) (logior (logand (mc-vibraparam cptr) #x0f) (logand effect #xf0)))))
      (#.+mod-effect-volslide-toneporta+
       (when (/= period 0)
         (setf (mc-portaperiod cptr) (%s16-wrap period)
               (mc-period cptr) operiod))
       (setf (mc-effect cptr) +mod-effect-volslide-toneporta+)
       (when (/= (logand effect #xff) 0)
         (setf (mc-volumeslide cptr) (logand effect #xff))))
      (#.+mod-effect-volslide-vibrato+
       (setf (mc-effect cptr) +mod-effect-volslide-vibrato+)
       (when (/= (logand effect #xff) 0)
         (setf (mc-volumeslide cptr) (logand effect #xff))))
      (#.+mod-effect-set-offset+
       (setf (mc-samppos cptr) (+ (* (ash effect -4) 4096) (* (logand effect #xf) 256))))
      (#.+mod-effect-volume-slide+
       (setf (mc-effect cptr) +mod-effect-volume-slide+
             (mc-volumeslide cptr) (logand effect #xff)))
      (#.+mod-effect-jump-position+
       (setf (mod-tablepos ctx) (logand effect #xff))
       (when (>= (mod-tablepos ctx) (%mod-song-length ctx))
         (setf (mod-tablepos ctx) 0))
       (setf (mod-patternpos ctx) 0
             (mod-jump-loop-effect ctx) 1))
      (#.+mod-effect-set-volume+
       (setf (mc-volume cptr) (logand effect #xff)))
      (#.+mod-effect-pattern-break+
       (setf (mod-patternpos ctx) (%mod-u16 (* (+ (* (logand (ash effect -4) #xf) 10) (logand effect #xf))
                                           (mod-number-of-channels ctx)))
             (mod-jump-loop-effect ctx) 1)
       (setf (mod-tablepos ctx) (%mod-u16 (+ (mod-tablepos ctx) 1)))
       (when (>= (mod-tablepos ctx) (%mod-song-length ctx))
         (setf (mod-tablepos ctx) 0)))
      (#.+mod-effect-extended+
       (case (logand (ash effect -4) #xf)
         (#.+mod-effect-e-fine-porta-up+
          (setf (mc-period cptr) (%mod-u16 (- (mc-period cptr) (logand effect #xf))))
          (when (< (mc-period cptr) 113) (setf (mc-period cptr) 113)))
         (#.+mod-effect-e-fine-porta-down+
          (setf (mc-period cptr) (%mod-u16 (+ (mc-period cptr) (logand effect #xf))))
          (when (> (mc-period cptr) 856) (setf (mc-period cptr) 856)))
         (#.+mod-effect-e-fine-volslide-up+
          (setf (mc-volume cptr) (%mod-u8 (+ (mc-volume cptr) (logand effect #xf))))
          (when (> (mc-volume cptr) 64) (setf (mc-volume cptr) 64)))
         (#.+mod-effect-e-fine-volslide-down+
          (setf (mc-volume cptr) (%mod-u8 (- (mc-volume cptr) (logand effect #xf))))
          (when (> (mc-volume cptr) 200) (setf (mc-volume cptr) 0)))
         (#.+mod-effect-e-pattern-loop+
          (if (/= (logand effect #xf) 0)
              (if (/= (mc-patternloopcnt cptr) 0)
                  (progn
                    (setf (mc-patternloopcnt cptr) (%mod-u16 (- (mc-patternloopcnt cptr) 1)))
                    (if (/= (mc-patternloopcnt cptr) 0)
                        (setf (mod-patternpos ctx) (mc-patternloopstartpoint cptr)
                              (mod-jump-loop-effect ctx) 1)
                        (setf (mc-patternloopstartpoint cptr) (mod-patternpos ctx))))
                  (setf (mc-patternloopcnt cptr) (logand effect #xf)
                        (mod-patternpos ctx) (mc-patternloopstartpoint cptr)
                        (mod-jump-loop-effect ctx) 1))
              ;; Start point
              (setf (mc-patternloopstartpoint cptr) (mod-patternpos ctx))))
         (#.+mod-effect-e-pattern-delay+
          (setf (mod-patterndelay ctx) (logand effect #xf)))
         (#.+mod-effect-e-note-cut+
          (setf (mc-effect cptr) +mod-effect-e-note-cut+
                (mc-cut-param cptr) (logand effect #xf))
          (when (= (mc-cut-param cptr) 0)
            (setf (mc-volume cptr) 0)))))
      (#xf
       (let ((param (logand effect #xff)))
         (when (and (< param #x21) (/= param 0))
           (setf (mod-song-speed ctx) param
                 (mod-patternticksaim ctx) (%mod-u64 (* (mod-song-speed ctx)
                                                    (floor (* (mod-playrate ctx) 5) (* 2 (mod-bpm ctx)))))))
         (when (>= param #x21)
           (setf (mod-bpm ctx) param
                 (mod-patternticksaim ctx) (%mod-u64 (* (mod-song-speed ctx)
                                                    (floor (* (mod-playrate ctx) 5) (* 2 (mod-bpm ctx))))))))))))

(defun %mod-workeffect (cptr)
  (case (mc-effect cptr)
    (#.+mod-effect-arpeggio+
     (when (/= (mc-parameffect cptr) 0)
       (setf (mc-decalperiod cptr) (%s16-wrap (- (mc-period cptr) (aref (mc-arpperiods cptr) (mc-arpindex cptr)))))
       (setf (mc-arpindex cptr) (%mod-u8 (+ (mc-arpindex cptr) 1)))
       (when (> (mc-arpindex cptr) 2)
         (setf (mc-arpindex cptr) 0))))
    (#.+mod-effect-portamento-up+
     (when (/= (mc-period cptr) 0)
       (setf (mc-period cptr) (%mod-u16 (- (mc-period cptr) (mc-parameffect cptr))))
       (when (or (< (mc-period cptr) 113) (> (mc-period cptr) 20000))
         (setf (mc-period cptr) 113))))
    (#.+mod-effect-portamento-down+
     (when (/= (mc-period cptr) 0)
       (setf (mc-period cptr) (%mod-u16 (+ (mc-period cptr) (mc-parameffect cptr))))
       (when (> (mc-period cptr) 20000)
         (setf (mc-period cptr) 20000))))
    ((#.+mod-effect-volslide-toneporta+ #.+mod-effect-tone-portamento+)
     (when (and (/= (mc-period cptr) 0) (/= (mc-period cptr) (mc-portaperiod cptr)) (/= (mc-portaperiod cptr) 0))
       (if (> (mc-period cptr) (mc-portaperiod cptr))
           (if (>= (- (mc-period cptr) (mc-portaperiod cptr)) (mc-portaspeed cptr))
               (setf (mc-period cptr) (%mod-u16 (- (mc-period cptr) (mc-portaspeed cptr))))
               (setf (mc-period cptr) (%mod-u16 (mc-portaperiod cptr))))
           (if (>= (- (mc-portaperiod cptr) (mc-period cptr)) (mc-portaspeed cptr))
               (setf (mc-period cptr) (%mod-u16 (+ (mc-period cptr) (mc-portaspeed cptr))))
               (setf (mc-period cptr) (%mod-u16 (mc-portaperiod cptr)))))
       (when (= (mc-period cptr) (mc-portaperiod cptr))
         (setf (mc-portaperiod cptr) 0)))
     (when (= (mc-effect cptr) +mod-effect-volslide-toneporta+)
       (if (> (mc-volumeslide cptr) #x0f)
           (progn
             (setf (mc-volume cptr) (%mod-u8 (+ (mc-volume cptr) (ash (mc-volumeslide cptr) -4))))
             (when (> (mc-volume cptr) 63) (setf (mc-volume cptr) 63)))
           (progn
             (setf (mc-volume cptr) (%mod-u8 (- (mc-volume cptr) (mc-volumeslide cptr))))
             (when (> (mc-volume cptr) 63) (setf (mc-volume cptr) 0))))))
    ((#.+mod-effect-volslide-vibrato+ #.+mod-effect-vibrato+)
     (setf (mc-vibraperiod cptr) (%s16-wrap (ash (* (logand (mc-vibraparam cptr) #xf)
                                                    (svref +jar-mod-sin-table+ (logand (mc-vibrapointeur cptr) #x1f)))
                                                 -7)))
     (when (> (mc-vibrapointeur cptr) 31)
       (setf (mc-vibraperiod cptr) (%s16-wrap (- (mc-vibraperiod cptr)))))
     (setf (mc-vibrapointeur cptr) (logand (+ (mc-vibrapointeur cptr) (logand (ash (mc-vibraparam cptr) -4) #xf)) #x3f))
     (when (= (mc-effect cptr) +mod-effect-volslide-vibrato+)
       (if (> (mc-volumeslide cptr) #xf)
           (progn
             (setf (mc-volume cptr) (%mod-u8 (+ (mc-volume cptr) (ash (mc-volumeslide cptr) -4))))
             (when (> (mc-volume cptr) 64) (setf (mc-volume cptr) 64)))
           (progn
             (setf (mc-volume cptr) (%mod-u8 (- (mc-volume cptr) (mc-volumeslide cptr))))
             (when (> (mc-volume cptr) 64) (setf (mc-volume cptr) 0))))))
    (#.+mod-effect-volume-slide+
     (if (> (mc-volumeslide cptr) #xf)
         (progn
           (setf (mc-volume cptr) (%mod-u8 (+ (mc-volume cptr) (ash (mc-volumeslide cptr) -4))))
           (when (> (mc-volume cptr) 64) (setf (mc-volume cptr) 64)))
         (progn
           (setf (mc-volume cptr) (%mod-u8 (- (mc-volume cptr) (logand (mc-volumeslide cptr) #xf))))
           (when (> (mc-volume cptr) 64) (setf (mc-volume cptr) 0)))))
    (#.+mod-effect-e-note-cut+
     (when (/= (mc-cut-param cptr) 0)
       (setf (mc-cut-param cptr) (%mod-u8 (- (mc-cut-param cptr) 1))))
     (when (= (mc-cut-param cptr) 0)
       (setf (mc-volume cptr) 0)))))

;;;----------------------------------------------------------------------------------
;;; Public functions
;;;----------------------------------------------------------------------------------

(defun jar-mod-init (modctx)
  "Clear the context and set the default configuration"
  ;; memclear(modctx, 0, sizeof(jar_mod_context_t))
  (let ((new (%make-jar-mod-context)))
    (dolist (slot '(song song-speed sample-lengths sample-reppnts sample-replens sampledata patterndata
                    playrate tablepos patternpos patterndelay jump-loop-effect bpm patternticks patterntickse
                    patternticksaim sampleticksconst samplenb channels number-of-channels fullperiod mod-loaded
                    last-r-sample last-l-sample stereo stereo-separation bits filter modfile modfilesize loopcount))
      (setf (slot-value modctx slot) (slot-value new slot))))
  (setf (mod-playrate modctx) +jar-mod-default-sample-rate+
        (mod-stereo modctx) 1
        (mod-stereo-separation modctx) 1
        (mod-bits modctx) 16
        (mod-filter modctx) 1)
  (let ((table +jar-mod-period-table+))
    (dotimes (i (- +jar-mod-period-table-length+ 1))
      (dotimes (j 8)
        (setf (aref (mod-fullperiod modctx) (+ (* i 8) j))
              (- (svref table i) (* (truncate (- (svref table i) (svref table (+ i 1))) 8) j))))))
  t)

(defun make-jar-mod-context ()
  (let ((ctx (%make-jar-mod-context)))
    (jar-mod-init ctx)
    ctx))

(defun jar-mod-setcfg (modctx samplerate bits stereo stereo-separation filter)
  (setf (mod-playrate modctx) samplerate
        (mod-stereo modctx) (if stereo 1 0))
  (when (< stereo-separation 4)
    (setf (mod-stereo-separation modctx) stereo-separation))
  (setf (mod-bits modctx) (if (or (= bits 8) (= bits 16)) bits 16)
        (mod-filter modctx) (if filter 1 0))
  t)

;; jar_mod_load(), MOD-DATA is (and must stay) the context modfile
(defun jar-mod-load (modctx mod-data mod-data-size)
  (let ((modmemory 0)
        (endmodmemory mod-data-size)
        (song (mod-song modctx)))
    (setf (mod-modfile modctx) mod-data)
    (dotimes (i 1084)
      (setf (aref song i) (%mod-byte modctx i)))
    (setf (mod-number-of-channels modctx) 0)
    (loop for (signature . numberofchannels) in +jar-mod-list+
          do (when (every (lambda (c k) (= (char-code c) (aref song (+ 1080 k)))) signature '(0 1 2 3))
               (setf (mod-number-of-channels modctx) numberofchannels)))
    (if (= (mod-number-of-channels modctx) 0)
        (progn
          (replace song (map 'vector #'char-code "M.K.") :start1 1080)
          ;; Old 15 samples module: move the song length and pattern table
          (replace song (subseq song 470 600) :start1 950)
          (fill song 0 :start 470 :end 950)
          (incf modmemory 600)
          (setf (mod-number-of-channels modctx) 4))
        (incf modmemory 1084))
    (when (>= modmemory endmodmemory)
      (return-from jar-mod-load nil))   ; End passed ? - Probably a bad file !
    (let ((max 0))
      (dotimes (i 128)
        (loop while (<= max (%mod-song-patterntable modctx i))
              do (setf (aref (mod-patterndata modctx) max) modmemory)
                 (incf modmemory (* 256 (mod-number-of-channels modctx)))
                 (incf max)
                 (when (>= modmemory endmodmemory)
                   (return-from jar-mod-load nil)))))
    (fill (mod-sampledata modctx) nil)
    (dotimes (i 31)
      (let ((base (+ 20 (* i 30))))
        (flet ((swapped (o) (%mod-u16 (* 2 (logior (ash (aref song (+ base o)) 8) (aref song (+ base o 1)))))))
          (setf (aref (mod-sample-lengths modctx) i) (swapped 22)
                (aref (mod-sample-reppnts modctx) i) (swapped 26)
                (aref (mod-sample-replens modctx) i) (swapped 28))))
      (unless (= (aref (mod-sample-lengths modctx) i) 0)
        (setf (aref (mod-sampledata modctx) i) modmemory)
        (incf modmemory (aref (mod-sample-lengths modctx) i))
        (when (> (+ (aref (mod-sample-replens modctx) i) (aref (mod-sample-reppnts modctx) i))
                 (aref (mod-sample-lengths modctx) i))
          (setf (aref (mod-sample-replens modctx) i)
                (%mod-u16 (- (aref (mod-sample-lengths modctx) i) (aref (mod-sample-reppnts modctx) i)))))
        (when (> modmemory endmodmemory)
          (return-from jar-mod-load nil))))   ; End passed ? - Probably a bad file !
    (let ((rate (mod-playrate modctx)))
      (setf (mod-tablepos modctx) 0
            (mod-patternpos modctx) 0
            (mod-song-speed modctx) 6
            (mod-bpm modctx) 125
            (mod-samplenb modctx) 0
            (mod-patternticks modctx) (+ (floor (* (mod-song-speed modctx) rate 5) (* 2 (mod-bpm modctx))) 1)
            (mod-patternticksaim modctx) (floor (* (mod-song-speed modctx) rate 5) (* 2 (mod-bpm modctx)))
            (mod-sampleticksconst modctx) (floor 3546894 rate)))   ; 8448*428/playrate
    (dotimes (i (mod-number-of-channels modctx))
      (setf (mc-volume (svref (mod-channels modctx) i)) 0
            (mc-period (svref (mod-channels modctx) i)) 0))
    (setf (mod-mod-loaded modctx) 1)
    t))

(defun jar-mod-fillbuffer (modctx outbuffer nbsample &optional (start 0))
  "Generate NBSAMPLE stereo frames as s16 into OUTBUFFER"
  (when (and modctx outbuffer)
    (if (/= (mod-mod-loaded modctx) 0)
        (let ((ll (mod-last-l-sample modctx))
              (lr (mod-last-r-sample modctx))
              (nch (mod-number-of-channels modctx))
              (channels (mod-channels modctx))
              (data (mod-modfile modctx)))
          (flet ((pattern-row ()
                   (+ (aref (mod-patterndata modctx) (%mod-song-patterntable modctx (mod-tablepos modctx)))
                      (* (mod-patternpos modctx) 4))))
            (dotimes (i nbsample)
              (when (> (prog1 (mod-patternticks modctx)
                         (setf (mod-patternticks modctx) (%mod-u64 (+ (mod-patternticks modctx) 1))))
                       (mod-patternticksaim modctx))
                (if (= (mod-patterndelay modctx) 0)
                    (let ((nptr (pattern-row)))
                      (setf (mod-patternticks modctx) 0
                            (mod-patterntickse modctx) 0)
                      (dotimes (c nch)
                        (%mod-worknote (+ nptr (* c 4)) (svref channels c) modctx))
                      (if (= (mod-jump-loop-effect modctx) 0)
                          (setf (mod-patternpos modctx) (%mod-u16 (+ (mod-patternpos modctx) nch)))
                          (setf (mod-jump-loop-effect modctx) 0))
                      (when (= (mod-patternpos modctx) (* 64 nch))
                        (setf (mod-tablepos modctx) (%mod-u16 (+ (mod-tablepos modctx) 1))
                              (mod-patternpos modctx) 0)
                        (when (>= (mod-tablepos modctx) (%mod-song-length modctx))
                          (setf (mod-tablepos modctx) 0
                                (mod-loopcount modctx) (%mod-u16 (+ (mod-loopcount modctx) 1))))))   ; Count next loop
                    (setf (mod-patterndelay modctx) (%mod-u16 (- (mod-patterndelay modctx) 1))
                          (mod-patternticks modctx) 0
                          (mod-patterntickse modctx) 0)))
              (when (> (prog1 (mod-patterntickse modctx)
                         (setf (mod-patterntickse modctx) (%mod-u64 (+ (mod-patterntickse modctx) 1))))
                       (floor (mod-patternticksaim modctx) (mod-song-speed modctx)))
                (dotimes (c nch)
                  (%mod-workeffect (svref channels c)))
                (setf (mod-patterntickse modctx) 0))
              (let ((l 0) (r 0))
                (dotimes (j nch)
                  (let ((cptr (svref channels j)))
                    (unless (= (mc-period cptr) 0)
                      (let ((finalperiod (%s16-wrap (- (mc-period cptr) (mc-decalperiod cptr) (mc-vibraperiod cptr)))))
                        (unless (= finalperiod 0)
                          ;; NOTE: Unsigned long division by the (sign extended) short period
                          (setf (mc-samppos cptr) (%mod-u64 (+ (mc-samppos cptr)
                                                           (floor (%mod-u64 (ash (mod-sampleticksconst modctx) 10))
                                                                  (%mod-u64 finalperiod))))))
                        (setf (mc-ticks cptr) (%mod-u64 (+ (mc-ticks cptr) 1)))
                        (if (<= (mc-replen cptr) 2)
                            (when (>= (ash (mc-samppos cptr) -10) (mc-length cptr))
                              (setf (mc-length cptr) 0
                                    (mc-reppnt cptr) 0
                                    (mc-samppos cptr) 0))
                            (let ((loop-end (+ (mc-replen cptr) (mc-reppnt cptr))))
                              (when (>= (ash (mc-samppos cptr) -10) loop-end)
                                (setf (mc-samppos cptr) (%mod-u64 (+ (ash (mc-reppnt cptr) 10)
                                                                 (mod (mc-samppos cptr) (ash loop-end 10))))))))
                        (let ((k (ash (mc-samppos cptr) -10)))
                          (when (mc-sampdata cptr)
                            (let* ((offset (+ (mc-sampdata cptr) k))
                                   (byte (if (< offset (length data)) (aref data offset) 0))
                                   (s (if (>= byte 128) (- byte 256) byte)))
                              (if (or (= (logand j 3) 1) (= (logand j 3) 2))
                                  (incf r (* s (mc-volume cptr)))
                                  (incf l (* s (mc-volume cptr)))))))))))
                (let ((tl (%i16 l))
                      (tr (%i16 r)))
                  (when (/= (mod-filter modctx) 0)
                    (setf l (ash (+ l ll) -1)
                          r (ash (+ r lr) -1)))
                  (when (= (mod-stereo-separation modctx) 1)
                    (setf l (+ l (ash r -1)))
                    (setf r (+ r (ash l -1))))
                  (setf (aref outbuffer (+ start (* i 2))) (max -32768 (min 32767 l))
                        (aref outbuffer (+ start (* i 2) 1)) (max -32768 (min 32767 r)))
                  (setf ll tl
                        lr tr)))))
          (setf (mod-last-l-sample modctx) ll
                (mod-last-r-sample modctx) lr
                (mod-samplenb modctx) (%mod-u64 (+ (mod-samplenb modctx) nbsample))))
        (fill outbuffer 0 :start start :end (+ start (* nbsample 2))))))

(defun %jar-mod-reset (modctx)
  "Resets internals for mod context"
  ;; NOTE: jar_mod_init() clears the whole context, configuration and modfile included
  (jar-mod-init modctx))

(defun jar-mod-unload (modctx)
  (when modctx
    (when (mod-modfile modctx)
      (setf (mod-modfile modctx) nil
            (mod-modfilesize modctx) 0
            (mod-loopcount modctx) 0))
    (%jar-mod-reset modctx)))

(defun jar-mod-load-file (modctx file-name)
  "Load a MOD file, returns the file size or 0 on failure"
  (setf (mod-modfile modctx) nil)
  (multiple-value-bind (data fsize) (load-file-data file-name)
    (if (and data (> fsize 0) (< fsize (* 32 1024 1024)))
        (progn
          (setf (mod-modfile modctx) data
                (mod-modfilesize modctx) fsize)
          (if (jar-mod-load modctx data fsize) fsize 0))
        0)))

(defun jar-mod-current-samples (modctx)
  (if modctx (mod-samplenb modctx) 0))

(defun jar-mod-max-samples (ctx)
  (let ((buff (make-array 2 :element-type '(signed-byte 16) :initial-element 0))
        (lastcount (mod-loopcount ctx)))
    (loop while (<= (mod-loopcount ctx) lastcount)
          do (jar-mod-fillbuffer ctx buff 1))
    (prog1 (mod-samplenb ctx)
      (jar-mod-seek-start ctx))))

(defun jar-mod-seek-start (ctx)
  (when (and ctx (mod-modfile ctx))
    (let ((ftmp (mod-modfile ctx))
          (stmp (mod-modfilesize ctx))
          (lcnt (mod-loopcount ctx)))
      (when (%jar-mod-reset ctx)
        (jar-mod-load ctx ftmp stmp)
        (setf (mod-modfile ctx) ftmp
              (mod-modfilesize ctx) stmp
              (mod-loopcount ctx) lcnt)))))

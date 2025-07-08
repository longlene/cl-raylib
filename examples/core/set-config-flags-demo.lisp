;;;; SetConfigFlags Demo
;;;; 演示cl-raylib中SetConfigFlags函数的使用
;;;; Demonstrates the use of SetConfigFlags function in cl-raylib

(ql:quickload :cl-raylib)
(use-package :cl-raylib)

(defun set-config-flags-demo ()
  "Demonstrate proper use of set-config-flags function"
  (format t "~%=== SetConfigFlags Demo ===~%")
  
  ;; IMPORTANT: SetConfigFlags must be called BEFORE InitWindow
  ;; 重要：SetConfigFlags必须在InitWindow之前调用
  (format t "Setting window configuration flags...~%")
  
  ;; Combine multiple flags using logior (bitwise OR)
  ;; 使用logior（按位或）组合多个标志
  (set-config-flags (logior +flag-window-resizable+
                           +flag-vsync-hint+
                           +flag-window-highdpi+))
  
  ;; Initialize window - flags will be applied here
  ;; 初始化窗口 - 标志将在这里应用
  (init-window 800 600 "SetConfigFlags Demo")
  
  ;; Main game loop
  ;; 主游戏循环
  (loop while (not (window-should-close))
        do (progn
             (begin-drawing)
             (clear-background +raywhite+)
             
             ;; Draw information about the flags that were set
             ;; 绘制已设置标志的信息
             (draw-text "SetConfigFlags Demo" 10 10 20 +darkgray+)
             (draw-text "Configuration flags applied:" 10 40 16 +gray+)
             (draw-text "- WINDOW_RESIZABLE: Window can be resized" 20 70 12 +darkgray+)
             (draw-text "- VSYNC_HINT: V-Sync enabled" 20 90 12 +darkgray+)
             (draw-text "- WINDOW_HIGHDPI: HiDPI support enabled" 20 110 12 +darkgray+)
             
             (draw-text "Try resizing the window!" 10 150 16 +darkblue+)
             (draw-text "Press ESC to exit" 10 180 12 +gray+)
             
             ;; Show current window dimensions
             ;; 显示当前窗口尺寸
             (let ((width (get-screen-width))
                   (height (get-screen-height)))
               (draw-text (format nil "Window size: ~dx~d" width height)
                         10 220 14 +red+))
             
             (end-drawing)))
  
  ;; Cleanup
  ;; 清理
  (close-window))

(defun demonstrate-flag-usage ()
  "Show various flag combinations and their effects"
  (format t "~%=== Flag Usage Examples ===~%")
  
  ;; Example 1: Basic resizable window with VSync
  ;; 示例1：基本可调整大小的窗口，启用垂直同步
  (format t "Example 1: Basic setup~%")
  (format t "  (set-config-flags (logior +flag-window-resizable+ +flag-vsync-hint+))~%")
  
  ;; Example 2: Fullscreen window
  ;; 示例2：全屏窗口
  (format t "~%Example 2: Fullscreen window~%")
  (format t "  (set-config-flags +flag-fullscreen-mode+)~%")
  
  ;; Example 3: Borderless window
  ;; 示例3：无边框窗口
  (format t "~%Example 3: Borderless window~%")
  (format t "  (set-config-flags +flag-window-undecorated+)~%")
  
  ;; Example 4: High-quality graphics
  ;; 示例4：高质量图形
  (format t "~%Example 4: High-quality graphics~%")
  (format t "  (set-config-flags (logior +flag-msaa-4x-hint+ +flag-vsync-hint+))~%")
  
  ;; Example 5: Transparent window
  ;; 示例5：透明窗口
  (format t "~%Example 5: Transparent window~%")
  (format t "  (set-config-flags +flag-window-transparent+)~%")
  
  ;; Example 6: Always on top
  ;; 示例6：总是在顶层
  (format t "~%Example 6: Always on top~%")
  (format t "  (set-config-flags +flag-window-topmost+)~%")
  
  (format t "~%Available flags:~%")
  (let ((flags `((,+flag-window-resizable+ "WINDOW_RESIZABLE" "Allow window resizing")
                 (,+flag-window-undecorated+ "WINDOW_UNDECORATED" "Remove window frame")
                 (,+flag-window-hidden+ "WINDOW_HIDDEN" "Start window hidden")
                 (,+flag-window-minimized+ "WINDOW_MINIMIZED" "Start window minimized")
                 (,+flag-window-maximized+ "WINDOW_MAXIMIZED" "Start window maximized")
                 (,+flag-window-unfocused+ "WINDOW_UNFOCUSED" "Start window unfocused")
                 (,+flag-window-topmost+ "WINDOW_TOPMOST" "Keep window on top")
                 (,+flag-window-always-run+ "WINDOW_ALWAYS_RUN" "Run when minimized")
                 (,+flag-window-transparent+ "WINDOW_TRANSPARENT" "Transparent framebuffer")
                 (,+flag-window-highdpi+ "WINDOW_HIGHDPI" "HiDPI support")
                 (,+flag-fullscreen-mode+ "FULLSCREEN_MODE" "Fullscreen mode")
                 (,+flag-vsync-hint+ "VSYNC_HINT" "Enable V-Sync")
                 (,+flag-msaa-4x-hint+ "MSAA_4X_HINT" "4x anti-aliasing"))))
    (dolist (flag-info flags)
      (format t "  0x~4,'0X  ~20a  ~a~%" 
              (first flag-info) 
              (second flag-info) 
              (third flag-info)))))

;; Show usage examples first
(demonstrate-flag-usage)

;; Run the interactive demo
(format t "~%Press Enter to run the interactive demo...")
(read-line)
(set-config-flags-demo)
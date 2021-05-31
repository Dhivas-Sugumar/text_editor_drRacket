;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname homework_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Homework 12
; Warm Up

; Exercise 1


; positions : (x) X [List-of X] [X -> Boolean] -> [List-of Number] 
; Retruns the list of positions in lox where x occurs
(check-expect (positions 2 (list 1 3 5 6 7) =) '())
(check-expect (positions 3 (list 1 2 3 3) =) (list 2 3))
(check-expect (positions 5 (list 1 3 5 6 7) =) (list 2))
(check-expect (positions "hi" (list "ye" "sheesh" "hi" "hi" "hello" "hi") string=?) (list 2 3 5))
(define (positions x lox x=?)
  (cond [(empty? lox) '()]
        [(cons? lox) (if (x=? (first lox) x)
                         (cons 0 (index-of (positions x (rest lox) x=?)))
                         (index-of (positions x (rest lox) x=?)))]))
 
; index-of : [List-of Nat] -> [List-of Nat]
; adds 1 to every element in the list
(check-expect (index-of '()) '())
(check-expect (index-of (list 5 3 4)) (list 6 4 5))
(check-expect (index-of (list 4)) (list 5))

(define (index-of l)
  (cond [(empty? l) '()]
        [(cons? l) (cons (add1 (first l))
                         (index-of (rest l)))]))

; Exercise 2

; positions.v2 : (X)(Y) X [List-of X] [X -> Boolean] -> [List-of Y]
; returns the list of positions in lox where x occurs
(check-expect (positions.v2 2 (list 1 2 3 4 2) =) (list 1 4))
(check-expect (positions.v2 7 (list 1 2 3 4 2) =) '())
(check-expect (positions.v2 "hi" (list "hi" "sheesh" "dance" "sheesh" "hello") string=?) (list 0))
(check-expect (positions.v2 "hi" (list "sheesh" "dance" "sheesh" "hello") string=?) '())

(define (positions.v2 x lox x=?)
  (local [; positions.v2/acc : (X) X [List-of X] [X->Boolean] NatNum [List-of NatNum] -> [List-of NatNum]
          (define (positions.v2/acc x lox x=? c acc)
            (cond [(empty? lox) acc]
                  [(cons? lox)
                   (if (x=? x (first lox))
                       (positions.v2/acc x (rest lox) x=? (add1 c) (cons c acc))
                       (positions.v2/acc x (rest lox) x=? (add1 c) acc))]))]
    (reverse (positions.v2/acc x lox x=? 0 '()))))

; positions is teh more efficent one. This is because unlike teh positions.v2 version it
; does not need to reverse the list produced and it traverses teh list fewere times.
; Project Part 3

(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA DEFINITIONS FOR A TEXT BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define BACKGROUND (empty-scene 1000 500 "white")) ; the background for the text buffer
(define DEFAULT-FSIZE 20)                          ; the default text font size
(define COLOR-TEXT "black")                        ; the text font color
(define LINE-SEP "\n")                             ; the 1-String to denote a new line
(define MENU-TEXT (list "Modes: Edit, Menu, Search"
                        " - Edit: Use this mode to change your text"
                        " - Menu: Use this mode to get help using the editor"
                        " - Search: F3 "
                        "Increase Font Size: F1"
                        "Decrease Font Size: F2"
                        "Switch Modes: Escape Key"))

;A Line is a String
;Line represents a row of text in the Buffer
(define line-0 "")
(define line-1 "A")
(define line-2 "Hi this is a line")
(define line-3 "This project is super fun to code :)")
(define (line-templ l)
  (... l ...))

; A Text is a String
; Represents all the text contained in a buffer
; Denotes new line with \n
(define text-0
  (string-append
   "About a hiring visit by Edsger Dijkstra (1930-2002)," LINE-SEP
   "a famous computer scientist:"                         LINE-SEP
   LINE-SEP))
(define text-1
  (string-append
   line-0 LINE-SEP line-1 LINE-SEP LINE-SEP line-2 LINE-SEP LINE-SEP))

; A Mode is one of:
; - "EDIT"
; - "MENU"
; - "SEARCH"
; Represents the mode that the buffer is in
(define EDIT "EDIT")
(define MENU "MENU")
(define SEARCH "SEARCH")
(define (mode-templ m)
  (cond [(string=? EDIT m) ...]
        [(string=? MENU m) ...]
        [(string=? SEARCH m) ...]))

; A CursorKey is one of:
; - "left", "right", "up", "down"
(define LEFT "left")
(define RIGHT "right")
(define UP "up")
(define DOWN "down")
(define (cursorkey-templ ck)
  (... (cond [(string=? ck LEFT) ...]
             [(string=? ck RIGHT) ...]
             [(string=? ck UP) ...]
             [(string=? ck DOWN) ...])))

(define-struct buffer [lines line-num col-num font-size mode])

; A Buffer is a (make-struct [List-of Line] NatNum NatNum Mode)
; Represents the text, line number where the cursor is,
;column number where the cursor is, font size, and buffer mode
(define buffer-0 (make-buffer (list line-0) 0 0 DEFAULT-FSIZE EDIT))
(define buffer-1 (make-buffer (list line-1 line-2) 1 4 DEFAULT-FSIZE EDIT))
(define buffer-2 (make-buffer (list line-1 line-2) 1 17 DEFAULT-FSIZE EDIT))
(define buffer-3 (make-buffer (list line-1 line-2 line-3) 2 22 DEFAULT-FSIZE EDIT))
(define buffer-3-search (make-buffer (list line-1 line-2 line-3) 2 22 DEFAULT-FSIZE SEARCH))
(define buffer-4 (make-buffer (list line-1 line-2) 1 0 DEFAULT-FSIZE EDIT))
(define buffer-5 (make-buffer (list line-1 line-2 line-3) 0 0 DEFAULT-FSIZE EDIT))
(define buffer-6 (make-buffer (list line-1 line-2 line-3) 2 4 DEFAULT-FSIZE EDIT))
(define buffer-7 (make-buffer (list line-1 line-2 line-3) 1 4  DEFAULT-FSIZE EDIT))
(define buffer-search (make-buffer (list "this" line-1 line-2 line-3) 1 0 DEFAULT-FSIZE SEARCH))
(define buffer-search-error (make-buffer (list "bye" line-1 line-2 line-3) 1 13 DEFAULT-FSIZE SEARCH))
(define buffer-1-search (make-buffer (list line-1 line-2) 1 4 DEFAULT-FSIZE SEARCH))

(define (buffer-templ bf)
  (... (line-templ (buffer-lines bf))
       (buffer-line-num bf)
       (buffer-col-num bf)
       (buffer-font-size bf)
       (buffer-mode bf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN: THE BIG-BANG WORLD PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; editor : Text -> Text 
; Uses a Text of arbitrary length to
; display the big bang window for the text editor. Returns
; a Text upon exiting the window that can be used to save progress

(define (editor initial-text)
  (local [; converts the given initial text into a buffer
          (define initial-e (make-buffer (text->lol initial-text) 0 0 DEFAULT-FSIZE EDIT))]
    (lol->text (buffer-lines (big-bang initial-e
                               [to-draw draw-editor]
                               [on-key process-key] 
                               [name "A Simple Text Editor"])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UTILITY FUNCTIONS OPERATING ON A BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; text->lol : Text -> [NEList-of Line]
; Converts a given text to a list of lines deliminated by LINE-SEP
(check-expect (text->lol text-0) (list "About a hiring visit by Edsger Dijkstra (1930-2002),"
                                       "a famous computer scientist:"
                                       ""
                                       ""))
(check-expect (text->lol text-1) (list ""
                                       "A"
                                       ""
                                       "Hi this is a line"
                                       ""
                                       ""))
(define (text->lol t)
  (split t LINE-SEP))

; split : String 1-String -> [List-of String]
; Splits the given string into substrings deliminated by the given 1-string
(check-expect (split "A|B|C|D"   "|") (list "A" "B" "C" "D"))
(check-expect (split "|A|B|C|D"  "|") (list "" "A" "B" "C" "D"))
(check-expect (split "|A|B|C|D|" "|") (list "" "A" "B" "C" "D" ""))
(check-expect (split "A"         "|") (list "A"))
(check-expect (split ""          "|") (list ""))
(check-expect (split "|"         "|") (list "" ""))
(check-expect (split "||"        "|") (list "" "" ""))
(define (split s c)
  (local [; check-for-c : String [List-of String] -> [List-of String]
          ; Replaces c with an empty string
          ; (check-expect (check-for-c "|" (list "|" "A" "|" "B")) (list "" "|" "A" "|" "B"))
          (define (check-for-c x y)
            (if (string=? c x)
                (append (list "") y)
                (cons (string-append x (first y)) (rest y))))]
    (foldr check-for-c (list "") (explode s))))

; lol->text : [NEList-of Line] -> Text
; Converts a given non empty list of lines to a Text
(check-expect (lol->text (list "About a hiring visit by Edsger Dijkstra (1930-2002),"
                               "a famous computer scientist:"
                               ""
                               ""))
              text-0)
(check-expect (lol->text (list ""
                               "A"
                               ""
                               "Hi this is a line"
                               ""
                               ""))
              text-1)

(define (lol->text lol)
  (local [; combine : Line Line -> String
          ; appends two lines with a LINE-SEP in between
          ; (check-expect (combine "Hi" "Bye") "Hi\nBye")
          (define (combine nextline current)
            (string-append current LINE-SEP nextline))]
    (foldl combine (first lol) (rest lol))))

; start-of-buffer? : Buffer -> Boolean
; is the cursor at line 0 and common 0?
; this means that we are at the top-left of the buffer. 
(check-expect (start-of-buffer? buffer-0) #t)
(check-expect (start-of-buffer? buffer-4) #f)
(check-expect (start-of-buffer? buffer-1) #f)
(check-expect (start-of-buffer? buffer-2) #f)
(define (start-of-buffer? buffer)
  (and (= (buffer-line-num buffer) 0)
       (= (buffer-col-num buffer) 0)))
 
; end-of-buffer? : Buffer -> Boolean
; are we at the lower right corner of the buffer, i.e. last line, last column?
(check-expect (end-of-buffer? buffer-0) #t)
(check-expect (end-of-buffer? buffer-2) #t)
(check-expect (end-of-buffer? buffer-3) #f)
(check-expect (end-of-buffer? buffer-1) #f)
(define (end-of-buffer? b)
  (and (= (sub1 (length (buffer-lines b))) (buffer-line-num b))
       (= (string-length (first (reverse (buffer-lines b)))) (buffer-col-num b))))


; end-of-line? : Buffer -> Boolean
; Is the cursor at the end of the current line
(check-expect (end-of-line? buffer-2) #t)
(check-expect (end-of-line? buffer-1) #f)
(check-expect (end-of-line? buffer-0) #t)
(define (end-of-line? b)
  (= (end-of-line b (buffer-line-num b)) (buffer-col-num b)))
 
; end-of-line : Buffer Nat -> Nat
; the end-of-line position of the given line in the buffer
(check-expect (end-of-line buffer-0 0) 0)
(check-expect (end-of-line buffer-3 2) 36)
(check-expect (end-of-line buffer-3 1) 17)
(define (end-of-line b n)
  (string-length (list-ref (buffer-lines b) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DRAWING THE BUFFER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; draw-editor : Buffer -> Buffer
; Draws a buffer in edit mode or menu mode
(check-expect (draw-editor buffer-3)
              (place-image/align (above/align "left"
                                              (text/font line-1 DEFAULT-FSIZE COLOR-TEXT
                                                         "Monospace" "default"
                                                         "normal" "normal" #f)
                                              (text/font line-2 DEFAULT-FSIZE COLOR-TEXT
                                                         "Monospace" "default" "normal" "normal" #f)
                                              (first (draw-cursor-line (list line-3)
                                                                       22 DEFAULT-FSIZE)))
                                 0 0 "left" "top" BACKGROUND))



(define (draw-editor b)
  (cond [(string=? EDIT (buffer-mode b)) (draw-editor-edit b)]
        [(string=? MENU (buffer-mode b)) (draw-editor-menu b)]
        [(string=? SEARCH (buffer-mode b)) (draw-editor-search b)]))
      
; draw-editor-edit : Buffer -> Buffer   
; Draws the buffer with the current cursor position in edit mode
(check-expect (draw-editor-edit buffer-3)
              (place-image/align (above/align "left"
                                              (text/font line-1 DEFAULT-FSIZE COLOR-TEXT
                                                         "Monospace" "default"
                                                         "normal" "normal" #f)
                                              (text/font line-2 DEFAULT-FSIZE COLOR-TEXT
                                                         "Monospace" "default" "normal" "normal" #f)
                                              (first (draw-cursor-line (list line-3)
                                                                       22 DEFAULT-FSIZE)))
                                 0 0 "left" "top" BACKGROUND))
(define (draw-editor-edit b)
  (local [(define lines (buffer-lines b))         ; The lines of the buffer
          (define line-num (buffer-line-num b))   ; The line num that the cursor is at
          (define col-num (buffer-col-num b))     ; The col num that the cursor is at
          (define fsize (buffer-font-size b))]    ; The font size of the buffer
    (stack-images-left-aligned
     (append
      (draw-lines (get-lines-before-line lines line-num) fsize)
      (draw-cursor-line (get-line-at-line lines line-num) col-num fsize)
      (draw-lines (get-lines-after-line lines line-num) fsize)))))

; draw-editor-menu
; Draws the buffer in menu mode, which displays information about the editor
(define (draw-editor-menu b)
  (stack-images-left-aligned
   (draw-lines MENU-TEXT DEFAULT-FSIZE)))

; draw-editor-search
; draw-editor-search
(define (draw-editor-search b)
  (local [(define lines (buffer-lines b))         ; The lines of the buffer
          (define line-num (buffer-line-num b))   ; The line num that the cursor is at
          (define col-num (buffer-col-num b))     ; The col num that the cursor is at
          (define fsize (buffer-font-size b))
          (define search-buffer (make-buffer (list "") 0 0 fsize SEARCH))]    ; The font size of the buffer
    (stack-images-left-aligned
     (append
      (draw-lines (list "Search instructions....") DEFAULT-FSIZE)
      (draw-lines (get-lines-before-line (buffer-lines search-buffer) (buffer-line-num search-buffer)) DEFAULT-FSIZE)
      (draw-cursor-line (get-line-at-line (buffer-lines search-buffer) (buffer-line-num search-buffer)) (buffer-col-num search-buffer) fsize)))))


#|
      (draw-lines (get-lines-before-line lines line-num) fsize)
      (draw-cursor-line (get-line-at-line lines line-num) col-num fsize)
      (draw-lines (get-lines-after-line lines line-num) fsize)))))
    
   ;(draw-editor-edit (make-buffer (list "") 0 0 DEFAULT-FSIZE SEARCH)))

|#
; stack-images-left-aligned : [List-of Image] -> Image
; Takes a list of an arbitrary number of images and stacks them, left aligned

(check-expect (stack-images-left-aligned (list
                                          (draw-line line-1 23)
                                          (draw-line line-2 23)
                                          (first (draw-cursor-line (list line-3) 4 23))))
              (place-image/align (above/align "left"
                                              (draw-line line-1 23)
                                              (draw-line line-2 23)
                                              (first (draw-cursor-line (list line-3) 4 23)))
                                 0 0 "left" "top" BACKGROUND))


(define (stack-images-left-aligned loi)
  (place-image/align (foldr (lambda (x y) (above/align "left"  x y)) empty-image loi)
                     0 0 "left" "top" BACKGROUND))

; draw-lines : [List-of Line] NatNum -> [List-of Image]
; Returns a list of images representing each line in the given list of lines and a NatNum
; representing the font size of the text
;(check-expect (draw-lines (list line-1)) (list 
(check-expect (draw-lines (list "hello" "hi") 20) (list (draw-line "hello" 20) (draw-line "hi" 20)))
(check-expect (draw-lines (list "this" "is" "difficult") 20)
              (list (draw-line "this" 20) (draw-line "is" 20) (draw-line "difficult" 20)))
(check-expect (draw-lines (list "hello" "") 18) (list (draw-line "hello" 18) (draw-line "" 18))) 
(define (draw-lines lol fs)
  (cond [(empty? lol) '()]
        [(cons? lol)
         (cons (draw-line (first lol) fs) (draw-lines (rest lol) fs))]))

; draw-line : Line NatNum -> Image
; Draws an image representing the given line with the given font size
(check-expect (draw-line "hello" 18)
              (text/font "hello" 18 COLOR-TEXT
                         "Monospace" "default" "normal" "normal" #f))
(check-expect (draw-line "" 23)
              (text/font "" 23 COLOR-TEXT
                         "Monospace" "default" "normal" "normal" #f))
(check-expect (draw-line "space here" 23)
              (text/font "space here" 23 COLOR-TEXT
                         "Monospace" "default" "normal" "normal" #f))
(check-expect (draw-line "Capital question?" 18)
              (text/font "Capital question?" 18 COLOR-TEXT
                         "Monospace" "default" "normal" "normal" #f))

(define (draw-line l fs)
  (text/font l fs COLOR-TEXT "Monospace" "default" "normal" "normal" #f))


; get-lines-before-line : [List-of Line] NatNum -> [List-of Line]
; Returns the lines of text that come before a given line number, exclusive of the given line
(check-expect (get-lines-before-line (list line-1 line-2 line-3) 2) (list line-1 line-2))
(check-expect (get-lines-before-line (list line-0 line-1 line-2 line-3) 2) (list line-0 line-1))
(check-expect (get-lines-before-line (list line-0 line-1 line-2) 0) '())
(check-expect (get-lines-before-line (list line-0 line-1 line-2 line-3) 4)
              (list line-0 line-1 line-2 line-3))
(define (get-lines-before-line lol n)
  (build-list n (lambda (x) (list-ref lol x))))

; draw-cursor-line : [List-of Line] NatNum NatNum -> [List-of Image]
; Returns a list of an image representing the given line
; and the current position of the cursor in that line, as well as
; the given font size 

(check-expect (draw-cursor-line (list "Eureka I got it") 4 20)
              (list (beside/align "bottom"
                                  (text/font
                                   "Eure"
                                   20 COLOR-TEXT
                                   "Monospace" "default" "normal" "normal" #f)
                                  (text/font
                                   "k"
                                   20 COLOR-TEXT
                                   "Monospace" "default" "normal" "normal" #t)
                                  (text/font
                                   "a I got it"
                                   20 COLOR-TEXT
                                   "Monospace" "default" "normal" "normal" #f))))
(check-expect (draw-cursor-line (list line-3) 0 17)
              (list (beside/align "bottom"
                                  (text/font
                                   ""
                                   17 COLOR-TEXT "Monospace" "default" "normal" "normal" #f)
                                  (text/font
                                   "T"
                                   17 COLOR-TEXT "Monospace" "default" "normal" "normal" #t)
                                  (text/font
                                   "his project is super fun to code :)"
                                   17 COLOR-TEXT
                                   "Monospace" "default" "normal" "normal" #f))))
(check-expect (draw-cursor-line (list line-3) 36 5)
              (list (beside/align "bottom"
                                  (text/font
                                   line-3
                                   5 COLOR-TEXT "Monospace" "default" "normal" "normal" #f)
                                  (text/font
                                   " "
                                   5 COLOR-TEXT
                                   "Monospace" "default" "normal" "normal" #t))))

(define (draw-cursor-line l c fs)
  (if (>= c (string-length (first l)))
      (list (beside/align "bottom"
                          (text/font
                           (substring (first l) 0 c)
                           fs COLOR-TEXT "Monospace" "default" "normal" "normal" #f)
                          (text/font
                           " "
                           fs COLOR-TEXT "Monospace" "default" "normal" "normal" #t))) 
                      
      (list (beside/align "bottom"
                          (text/font
                           (substring (first l) 0 c)
                           fs COLOR-TEXT "Monospace" "default" "normal" "normal" #f)
                          (text/font
                           (substring (first l) c (add1 c))
                           fs COLOR-TEXT "Monospace" "default" "normal" "normal" #t) 
                          (text/font
                           (substring (first l) (add1 c))
                           fs COLOR-TEXT "Monospace" "default" "normal" "normal" #f)))))


; get-line-at-line : [List-of Line] NatNum -> [List-of Line]
; Returns the line of text at the given line number
(check-expect (get-line-at-line (list line-1 line-2 line-3) 2) (list line-3))
(check-expect (get-line-at-line (list line-1 line-2) 1) (list line-2))
(check-expect (get-line-at-line (list '()) 0) (list '()))
(define (get-line-at-line lol n)
  (list (list-ref lol n)))
  
; get-lines-after-line : [List-of Line] NatNum -> [List-of Line]
; Returns the lines of text that come after a given line number, exclusive of the given line
(check-expect (get-lines-after-line (list line-1 line-2 line-3) 1)
              (list "This project is super fun to code :)"))
(check-expect (get-lines-after-line (list line-0 line-1 line-2 line-3) 2)
              (list "This project is super fun to code :)"))
(check-expect (get-lines-after-line (list line-0 line-1 line-2) 0) (list "A" "Hi this is a line"))
(define (get-lines-after-line lol n)
  (reverse (build-list (sub1 (- (length lol) n)) (lambda (x) (list-ref (reverse lol) x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RESPONDING TO KEY EVENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The following keys are whitelisted 
; A List-Of-AcceptableCharacter is one of:
; - '()
; - (list "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l"
;         "z" "x" "c" "v" "b" "n" "m" "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "A" "S"
;         "D" "F" "G" "H" "J" "K" "L" "Z" "X" "C" "V" "B" "N" "M" "1" "2" "3" "4"  "5""6"
;         "7" "8" "9" "0" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_" "+" "=" "`" "~"
;         "{" "[" "]" "}" ":" ";" "'" "" "," "<" "." ">" "/" "?" "|" "\"" "\\")

(define LIST-AC (list "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "a" "s" "d" "f" "g" "h" "j" "k" "l"
                      "z" "x" "c" "v" "b" "n" "m" "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "A" "S"
                      "D" "F" "G" "H" "J" "K" "L" "Z" "X" "C" "V" "B" "N" "M" "1" "2" "3" "4"  "5"
                      "6" "7" "8" "9" "0" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-" "_" "+" "=" "`"
                      "~" "{" "[" "]" "}" ":" ";" "'" "" "," "<" "." ">" "/" "?" "|" "\"" "\\" " "))

; process-key : Buffer KeyEvent -> Buffer
; It allows for user input of charcters and each characters assosicated display
; If editor is in menu mode, key inputs are ignored, except for escape
; If user is in search mode it allows the user to search for certain text. 
(check-expect (process-key buffer-3 "q")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super qfun to code :)")
               2 23  DEFAULT-FSIZE EDIT))

(check-expect (process-key buffer-3-search "q")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super qfun to code :)")
               2 23  DEFAULT-FSIZE SEARCH))

(check-expect (process-key buffer-3 "left")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super fun to code :)")
               2 21  DEFAULT-FSIZE EDIT))

(check-expect (process-key buffer-3 "\b")
              (make-buffer
               (list "A" "Hi this is a line" "This project is superfun to code :)")
               2 21  DEFAULT-FSIZE EDIT))

(check-expect (process-key buffer-3 "\r")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super " "fun to code :)")
               3 0  DEFAULT-FSIZE EDIT))

(check-expect (process-key buffer-3 "home")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super fun to code :)")
               2 0  DEFAULT-FSIZE EDIT))

(check-expect (process-key buffer-3 "end")
              (make-buffer
               (list "A" "Hi this is a line" "This project is super fun to code :)")
               2 36  DEFAULT-FSIZE EDIT))
              
(define (process-key b ke)
  (cond  [(string=? EDIT (buffer-mode b))(cond [(acceptable-char? ke) (insert-char b ke)]
                                               [(cursor-key? ke) (move-cursor b ke)]
                                               [(string=? ke "\b") (backspace b)]
                                               [(string=? ke "\r") (return b)]
                                               [(string=? ke "home") (home b)]
                                               [(string=? ke "end") (end b)]
                                               [(string=? ke "f1") (font-up b)]
                                               [(string=? ke "f2") (font-down b)]
                                               [(string=? ke "f3") (toggle-search b)]
                                               [(string=? ke "escape") (toggle-mode b)]
                                               [else b])]
         [(and (string=? MENU (buffer-mode b)) (string=? ke "escape")) (toggle-mode b)]
         [(string=? SEARCH (buffer-mode b)) (cond [(acceptable-char? ke) (insert-char b ke)]
                                                  [(string=? ke "\b") (backspace b)]
                                                  [(string=? ke "\r") (return b)]
                                                  [(string=? ke "f3") (search b)]
                                                  [else b])]))

; acceptable-char? : String -> Boolean
; Is the given string an Acceptable Character 
(check-expect (acceptable-char? "1") #t)
(check-expect (acceptable-char? "a") #t)
(check-expect (acceptable-char? ".") #t)
(check-expect (acceptable-char? "\\") #t)
(check-expect (acceptable-char? "\"") #t)
(check-expect (acceptable-char? "f6") #f)
(check-expect (acceptable-char? "\u007F") #f)
(define (acceptable-char? string)
  (local [; each-check : KeyEvent -> Boolean
          ; It checks if the given key event matches with one member of
          ;[List-of AcceptableCharacter]
          ; (check-expect (each-check "r") #t)
          ; (check-expect (each-check "\t") #f)
          (define (each-check s)
            (string=? s string))]
    (ormap each-check LIST-AC)))

; insert-char : Buffer AcceptableCharacter  -> Buffer 
; Inserts an acceptable character to the buffer at the current cursor position and
; moves the cursor one space to the right
(check-expect (insert-char buffer-1 "a")
              (make-buffer (list line-1 "Hi tahis is a line") 1 5  DEFAULT-FSIZE EDIT))
(check-expect (insert-char buffer-2 "z")
              (make-buffer (list line-1 "Hi this is a linez") 1 18 DEFAULT-FSIZE EDIT))
(check-expect (insert-char buffer-1-search "a")
               (make-buffer (list line-1 "Hi tahis is a line") 1 5  DEFAULT-FSIZE SEARCH))
(define (insert-char b ac)
  (make-buffer
   (insert-char-to-text (buffer-lines b) ac (buffer-line-num b) (buffer-col-num b))
   (buffer-line-num b)
   (add1 (buffer-col-num b))
   (buffer-font-size b)
   (buffer-mode b)))

; insert-char-to-text : [List-of Line] AcceptableCharacter l c -> [List-of Line]
; Inserts an acceptable character to the text at the given cursor position
(check-expect (insert-char-to-text (list line-1 line-2) "a" 1 4) (list line-1 "Hi tahis is a line"))
(check-expect (insert-char-to-text (list line-1 line-2) "z" 1 17) (list line-1 "Hi this is a linez"))
(define (insert-char-to-text lol ac l c)
  (append (get-lines-before-line lol l)
          (list (string-append (substring (first (get-line-at-line lol l)) 0 c)
                               ac
                               (substring (first (get-line-at-line lol l)) c)))
          (get-lines-after-line lol l)))

; cursor-key? : KeyEvent -> Boolean
; Checks if a key event is a CursorKey
(check-expect (cursor-key? LEFT) #t)
(check-expect (cursor-key? RIGHT) #t)
(check-expect (cursor-key? UP) #t)
(check-expect (cursor-key? DOWN) #t)
(check-expect (cursor-key? "t") #f)
(check-expect (cursor-key? "4") #f)
(define (cursor-key? ke)
  (or (string=? ke LEFT)
      (string=? ke RIGHT)
      (string=? ke UP)
      (string=? ke DOWN)))
            
; move-cursor : Buffer CursorKey -> Buffer
; Moves the cursor depending on which CursorKey was pressed
(check-expect (move-cursor buffer-1 LEFT)
              (make-buffer (list line-1 line-2) 1 3  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor
               (make-buffer (list "Hello" "World") 1 0  DEFAULT-FSIZE EDIT) LEFT)
              (make-buffer (list "Hello" "World") 0 5  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor buffer-0 LEFT) buffer-0)
(check-expect (move-cursor buffer-1 RIGHT)
              (make-buffer (list line-1 line-2) 1 5  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor
               (make-buffer (list "My" "names" "Jeff") 1 5  DEFAULT-FSIZE EDIT) RIGHT)
              (make-buffer (list "My" "names" "Jeff") 2 0  DEFAULT-FSIZE EDIT)) 
(check-expect (move-cursor buffer-2 RIGHT) buffer-2)
(check-expect (move-cursor buffer-1 UP)
              (make-buffer (list line-1 line-2) 0 1  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor buffer-4 UP)
              (make-buffer (list line-1 line-2) 0 0  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor buffer-2 DOWN)
              (make-buffer (list "A" "Hi this is a line") 1 17  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor buffer-5 DOWN)
              (make-buffer (list line-1 line-2 line-3) 1 0  DEFAULT-FSIZE EDIT))
(define (move-cursor b ck)
  (cond [(string=? ck LEFT) (cursor-left b)]
        [(string=? ck RIGHT) (cursor-right b)]
        [(string=? ck UP) (cursor-up b)]
        [(string=? ck DOWN) (cursor-down b)]))

; cursor-left : Buffer -> Buffer
; It moves the cursor to the left
; if the cursor is already at the start of the line it moves the
; cursor to the previous line (if it exists)
(check-expect (cursor-left buffer-2) (make-buffer (list line-1 line-2) 1 16  DEFAULT-FSIZE EDIT))
(check-expect (cursor-left (make-buffer (list "Hello" "World") 1 0  DEFAULT-FSIZE EDIT))
              (make-buffer (list "Hello" "World") 0 5  DEFAULT-FSIZE EDIT))
(check-expect (cursor-left buffer-0) buffer-0)
(define (cursor-left b)
  (cond [(start-of-buffer? b) b]
        [(= (buffer-col-num b) 0) (move-cursor-to-end-of-line b (sub1 (buffer-line-num b)))]
        [(not (= (buffer-col-num b) 0)) (make-buffer
                                         (buffer-lines b)
                                         (buffer-line-num b)
                                         (sub1 (buffer-col-num b))
                                         (buffer-font-size b)
                                         (buffer-mode b))]))
  
; move-cursor-to-end-of-line : Buffer NatNum -> Buffer
; It moves the cursor to the end of the given line number
(check-expect (move-cursor-to-end-of-line buffer-2 0)
              (make-buffer (list line-1 line-2) 0 1  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor-to-end-of-line buffer-3 1)
              (make-buffer (list line-1 line-2 line-3) 1 17  DEFAULT-FSIZE EDIT))
(check-expect (move-cursor-to-end-of-line buffer-3 2)
              (make-buffer (list line-1 line-2 line-3) 2 36  DEFAULT-FSIZE EDIT))
(define (move-cursor-to-end-of-line b n)
  (make-buffer (buffer-lines b) n (end-of-line b n) (buffer-font-size b) (buffer-mode b)))

;cursor-right : Buffer -> Buffer
; It moves the cursor to the right 
; if the cursor is already at the end of the line it must move
; the cursor to the next line (if it exists)
(check-expect (cursor-right buffer-3)
              (make-buffer (list line-1 line-2 line-3) 2 23  DEFAULT-FSIZE EDIT))
(check-expect (cursor-right
               (make-buffer (list "My" "names" "Jeff") 1 5  DEFAULT-FSIZE EDIT))
              (make-buffer (list "My" "names" "Jeff") 2 0  DEFAULT-FSIZE EDIT)) 
(check-expect (cursor-right buffer-2) buffer-2)
(define (cursor-right b)
  (cond [(end-of-buffer? b) b]
        [(end-of-line? b) (make-buffer
                           (buffer-lines b)
                           (add1 (buffer-line-num b))
                           0
                           (buffer-font-size b)
                           (buffer-mode b))]
        [else (make-buffer
               (buffer-lines b)
               (buffer-line-num b)
               (add1 (buffer-col-num b))
               (buffer-font-size b)
               (buffer-mode b))]))

; cursor-up : Buffer -> Buffer 
; Moves the cursor up a line. If there is not text directly above the cursor, it
; moves the cursor to the end of the previous line
(check-expect (cursor-up buffer-2) (make-buffer (list line-1 line-2) 0 1  DEFAULT-FSIZE EDIT))
(check-expect (cursor-up buffer-4) (make-buffer (list line-1 line-2) 0 0  DEFAULT-FSIZE EDIT))
(check-expect (cursor-up buffer-5) buffer-5)
(check-expect (cursor-up buffer-6) (make-buffer (list line-1 line-2 line-3) 1 4  DEFAULT-FSIZE EDIT))
(define (cursor-up b)
  (cond [(= (buffer-line-num b) 0) b]
        [(< (end-of-line b (sub1 (buffer-line-num b)))
            (buffer-col-num b)) (move-cursor-to-end-of-line b (sub1 (buffer-line-num b)))]
        [else (make-buffer (buffer-lines b)
                           (sub1 (buffer-line-num b))
                           (buffer-col-num b)
                           (buffer-font-size b)
                           (buffer-mode b))]))

; cursor-down : Buffer -> Buffer
; Moves the cursor down a line. If there is not text directly below the cursor, it
; moves the cursor to the end of the next line
(check-expect (cursor-down buffer-2) buffer-2)
(check-expect (cursor-down buffer-4) buffer-4)
(check-expect (cursor-down buffer-5)
              (make-buffer (list line-1 line-2 line-3) 1 0  DEFAULT-FSIZE EDIT))
(check-expect (cursor-down buffer-7)
              (make-buffer (list line-1 line-2 line-3) 2 4  DEFAULT-FSIZE EDIT))
(define (cursor-down b)
  (cond [(= (buffer-line-num b) (sub1 (length (buffer-lines b)))) b] 
        [(< (end-of-line b (add1 (buffer-line-num b)))
            (buffer-col-num b)) (move-cursor-to-end-of-line b (add1 (buffer-line-num b)))]
        [else (make-buffer (buffer-lines b)
                           (add1 (buffer-line-num b))
                           (buffer-col-num b)
                           (buffer-font-size b)
                           (buffer-mode b))]))
  
; backspace : Buffer -> Buffer 
; deletes a character at the current cursor position
(check-expect (backspace buffer-2)
              (make-buffer (list line-1 "Hi this is a lin") 1 16  DEFAULT-FSIZE EDIT))
(check-expect (backspace buffer-4)
              (make-buffer (list "AHi this is a line") 0 1  DEFAULT-FSIZE EDIT))

(define (backspace b)
  (cond [(start-of-buffer? b) b]
        [(= 0 (buffer-col-num b)) (backspace-beginning-line b)]
        [else (make-buffer
               (remove-char-at-index-full-text (buffer-lines b)
                                               (buffer-line-num b)
                                               (buffer-col-num b))
               (buffer-line-num b)
               (sub1 (buffer-col-num b))
               (buffer-font-size b)
               (buffer-mode b))]))

; backspace-beginning-line : Buffer -> Buffer
; Appends current line to the previous line
(check-expect (backspace-beginning-line buffer-3)
              (make-buffer (list line-1 "Hi this is a lineThis project is super fun to code :)")
                           1 17  DEFAULT-FSIZE EDIT))
(check-expect (backspace-beginning-line buffer-2)
              (make-buffer (list "AHi this is a line") 0 1  DEFAULT-FSIZE EDIT))

(define (backspace-beginning-line b)
  (make-buffer
   (append (get-lines-before-line (buffer-lines b) (sub1 (buffer-line-num b)))
           (list
            (string-append
             (first (reverse (get-lines-before-line (buffer-lines b) (buffer-line-num b))))
             (first (get-line-at-line (buffer-lines b) (buffer-line-num b)))))
           (get-lines-after-line (buffer-lines b) (buffer-line-num b)))
   (sub1 (buffer-line-num b))
   (string-length (first (get-line-at-line (buffer-lines b) (sub1 (buffer-line-num b)))))
   (buffer-font-size b)
   (buffer-mode b)))

; remove-char-at-index-full-text : [List-of Line] NatNum NatNum -> [List-of Line]
; Removes the character at the given cursor position from the list of lines
(check-expect (remove-char-at-index-full-text (list line-1 line-2 line-3) 2 2)
              (list "A" "Hi this is a line" "Tis project is super fun to code :)"))
(check-expect (remove-char-at-index-full-text (list line-1 line-2) 1 3)
              (list "A" "Hithis is a line"))
(define (remove-char-at-index-full-text lol l c)
  (append (get-lines-before-line lol l)
          (list (remove-char-at-index (first (get-line-at-line lol l)) c))
          (get-lines-after-line lol l)))

; remove-char-at-index : Non-Empty-String NatNum -> String
; removes the character in a nonempty string at the given index
(check-expect (remove-char-at-index "hello" 2) "hllo")
(check-expect (remove-char-at-index "why do we do" 4) "whydo we do")
(define (remove-char-at-index s n)
  (string-append (substring s 0 (sub1 n))
                 (substring s n (string-length s))))

; return : Buffer -> Buffer 
; Inserts a new line into the editor below the current cursor position.
; The current line is truncated after the current cursor position and the truncated
; text is inserted to the new line
(check-expect (return buffer-1)
              (make-buffer (list "A" "Hi t" "his is a line") 2 0  DEFAULT-FSIZE EDIT))
(check-expect (return buffer-2)
              (make-buffer (list "A" "Hi this is a line" "") 2 0  DEFAULT-FSIZE EDIT))
(define (return b)
  (if (end-of-line? b)
      (insert-line b "" (buffer-line-num b))
      (create-return-line b)))
        
; insert-line : Buffer Line NatNum -> Buffer
; inserts the given line to the buffer after the given line number and sets the cursor
; position to the beginning of the new line
(check-expect (insert-line buffer-1 "" 1)
              (make-buffer (list line-1 line-2 "") 2 0  DEFAULT-FSIZE EDIT))
(check-expect (insert-line buffer-3 "" 1)
              (make-buffer (list line-1 line-2 "" line-3) 2 0  DEFAULT-FSIZE EDIT))
(check-expect (insert-line buffer-3 "fsd" 1)
              (make-buffer (list line-1 line-2 "fsd" line-3) 2 0  DEFAULT-FSIZE EDIT))
(define (insert-line b l ln)
  (make-buffer (append (get-lines-before-line (buffer-lines b) ln)
                       (get-line-at-line (buffer-lines b) ln)
                       (list l)
                       (get-lines-after-line (buffer-lines b) ln))
               (add1 ln)
               0
               (buffer-font-size b)
               (buffer-mode b)))

; create-return-line : Buffer -> Line
; Truncates the text after the current cursor position, adds it
; to a new line below the current line, and sets the cursor position to the beginning
; of that line
(check-expect (create-return-line buffer-1)
              (make-buffer (list "A" "Hi t" "his is a line") 2 0  DEFAULT-FSIZE EDIT))
(check-expect (create-return-line buffer-2)
              (make-buffer (list "A" "Hi this is a line" "") 2 0  DEFAULT-FSIZE EDIT))
(define (create-return-line b)
  (make-buffer (append (get-lines-before-line (buffer-lines b) (buffer-line-num b))
                       (list (substring
                              (first (get-line-at-line (buffer-lines b) (buffer-line-num b)))
                              0
                              (buffer-col-num b)))
                       (list (substring
                              (first (get-line-at-line (buffer-lines b) (buffer-line-num b)))
                              (buffer-col-num b)))
                       (get-lines-after-line (buffer-lines b) (buffer-line-num b)))
               (add1 (buffer-line-num b))
               0
               (buffer-font-size b)
               (buffer-mode b)))

; home : Buffer -> Buffer
; When  home is pressed the cursor goes to the start of the current line.
; If the cursor is already at the start of the current line it goes to the start
; of the the entire document
(check-expect (home buffer-3) (make-buffer (list line-1 line-2 line-3) 2 0  DEFAULT-FSIZE EDIT))
(check-expect (home (make-buffer (list line-1 line-2 line-3) 2 0  DEFAULT-FSIZE EDIT))
              (make-buffer (list line-1 line-2 line-3) 0 0  DEFAULT-FSIZE EDIT))
(define (home b)
  (if (= 0 (buffer-col-num b))
      (make-buffer (buffer-lines b) 0 0 (buffer-font-size b) (buffer-mode b))
      (make-buffer (buffer-lines b) (buffer-line-num b) 0 (buffer-font-size b) (buffer-mode b))))

; end : Buffer -> Buffer
; When end is pressed the cursor goes to the end of the current line.
; If the cursor is already at the end of the current line it goes to the start
; of the the entire document
(check-expect (end buffer-3) (make-buffer (list line-1 line-2 line-3) 2 36  DEFAULT-FSIZE EDIT))
(check-expect (end (make-buffer (list "hi" "my name is" "fundies") 1 10  DEFAULT-FSIZE EDIT))
              (make-buffer (list "hi" "my name is" "fundies") 2 7  DEFAULT-FSIZE EDIT))

(define (end b)
  (local [(define lol  (buffer-lines  b))
          (define lnum (buffer-line-num b))
          (define cnum (buffer-col-num b))
          (define fsize (buffer-font-size b))
          (define mode (buffer-mode b))
          (define new-lnum (if (end-of-line? b) (sub1 (length lol)) lnum))
          (define new-cnum (end-of-line b new-lnum))]
    (make-buffer lol
                 new-lnum
                 new-cnum
                 fsize
                 mode)))

; font-up : Buffer -> Buffer
; Increases font size of the buffer by 1
(check-expect (font-up (make-buffer (list line-0 line-1 line-2)
                                    1 3
                                    20
                                    EDIT))
              (make-buffer (list line-0 line-1 line-2)
                           1 3
                           21
                           EDIT))
(check-expect (font-up (make-buffer (list line-1 line-2)
                                    1 3
                                    12
                                    EDIT))
              (make-buffer (list line-1 line-2)
                           1 3
                           13
                           EDIT))
(define (font-up b)
  (make-buffer (buffer-lines b)
               (buffer-line-num b)
               (buffer-col-num b)
               (add1 (buffer-font-size b))
               (buffer-mode b)))

; font-down : Buffer -> Buffer
; Decreases font size of the buffer by 1
; Never allows font size to be less than 1
(check-expect (font-down (make-buffer (list line-0 line-1 line-2)
                                      1 3
                                      20
                                      EDIT))
              (make-buffer (list line-0 line-1 line-2)
                           1 3
                           19
                           EDIT))
(check-expect (font-down (make-buffer (list line-1 line-2)
                                      1 3
                                      12
                                      EDIT))
              (make-buffer (list line-1 line-2)
                           1 3
                           11
                           EDIT))
(define (font-down b)
  (if (= 1 (buffer-font-size b))
      b
      (make-buffer (buffer-lines b)
                   (buffer-line-num b)
                   (buffer-col-num b)
                   (sub1 (buffer-font-size b))
                   (buffer-mode b))))

; toggle-mode : Buffer -> Buffer
; Switches the mode from edit to menu or menu to edit
(check-expect (toggle-mode (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE EDIT))
              (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE MENU))
(check-expect (toggle-mode (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE MENU))
              (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE EDIT))
(define (toggle-mode b)
  (make-buffer (buffer-lines b)
               (buffer-line-num b)
               (buffer-col-num b)
               (buffer-font-size b)
               (if (string=? EDIT (buffer-mode b))
                   MENU
                   EDIT)))

; toggle-search : Buffer -> Buffer
; switched the mode from edit to search or from search to edit
(check-expect (toggle-search (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE EDIT))
              (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE SEARCH))
(check-expect (toggle-search (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE SEARCH))
              (make-buffer (list line-1 line-2) 1 3 DEFAULT-FSIZE EDIT))
(define (toggle-search b)
  (make-buffer (buffer-lines b)
               (buffer-line-num b)
               (buffer-col-num b)
               (buffer-font-size b)
               (if (string=? EDIT (buffer-mode b))
                   SEARCH
                   EDIT)))

; string-search : String String NatNum -> NatNum
; It returns the position of the first occurence of a particular string in a second string.
; If no match is found it returns the leangth of teh second string plus 1
(check-expect (string-search "pattern" "pattern string test"  0)  0) 
(check-expect (string-search "pattern" "pattern string test"  1) 20) 
(check-expect (string-search "pattern" "pattern string test" 30) 20) 
(check-expect (string-search "string"  "pattern string test"  0)  8)
(check-expect (string-search "string"  "pattern string test"  8)  8) 
(check-expect (string-search "string"  "pattern string test"  9) 20) 
(check-expect (string-search ""        "pattern string test"  0)  0) 
(check-expect (string-search "paddern" "pattern string test"  0) 20)
(define (string-search s t p)
  (if (and (< p (string-length t)) (string-contains? s (substring t p)))
  (string-is-substring s t p (+ p (string-length s)))
  (add1 (string-length t))))

; string-is-substring : String String NatNum NatNum -> NatNum
; It returns the postion of the first occurence of a particular string in a second string
; given that the string that we are checking for exists.
; tend must be greater than or equal to tstart.
(check-expect (string-is-substring "pattern" "pattern string test"  0 7)  0) 
(check-expect (string-is-substring "string"  "pattern string test"  0 6)  8) 
(check-expect (string-is-substring "string"  "pattern string test"  8 14)  8) 
(check-expect (string-is-substring ""        "pattern string test"  0 0)  0) 
(define (string-is-substring s t tstart tend)
  (if (string=? s (substring t tstart tend))
      tstart
      (string-is-substring s t (add1 tstart) (add1 tend))))

;search : Buffer -> Buffer
; Searches the entire buffer for a given string tht is stored as the first
;line in the list-of-lines of a buffer
(check-expect (search buffer-search)
              (make-buffer
               (list "A" "Hi this is a line" "This project is super fun to code :)")
               1
               4
               20
               "EDIT"))
(check-expect (search buffer-search-error)
              (make-buffer
               (list "A" "Hi this is a line" "This project is super fun to code :)")
               1
               13
               20
               "EDIT"))


(define (search b)
  (local[
    (define lines (buffer-lines b))
    (define lnum (buffer-line-num b))
    (define colnum (buffer-col-num b))
    (define fsize (buffer-font-size b))
    (define mode (buffer-mode b))
    (define search-pos
      (string-search (first lines) (lol->text (rest lines))
                     (lnum-cnum->pos (rest lines) lnum colnum)))] 
    (if (< search-pos (string-length (lol->text (rest lines))))
        (make-buffer (rest (buffer-lines b))
                     (first (pos->lnum-cnum (rest lines) search-pos)) 
                     (first (rest (pos->lnum-cnum (rest lines) search-pos)))
                     fsize
                     EDIT)
        (make-buffer (rest (buffer-lines b))
                     lnum
                     colnum
                     fsize
                     EDIT))))


; lnum-cnum->pos : [NEList-of Line] NatNum NatNum -> NatNum
; Converts the cursor position, given as a line num and col num, from a
; non empty list of line to a single position.
(check-expect (lnum-cnum->pos (list line-1 line-2 line-3) 2 3) 21)
(check-expect (lnum-cnum->pos (list line-1 line-2 line-3) 0 0) 0)
(check-expect (lnum-cnum->pos (list line-1 line-2 line-3) 1 4) 5)
(define (lnum-cnum->pos nelol line-num col-num)
  (local [(define (count-pos line pos)
            (+ (string-length line) pos))]
  (+ (foldr count-pos 0 (get-lines-before-line nelol line-num)) col-num)))


; pos->lnum-cnum : [NEList-of line] NatNum -> NatNum NatNum
; converts the sngle position of the cursor to a line num and a col num
(check-expect (pos->lnum-cnum (list line-1 line-2 line-3) 21) (list 2 3))
(check-expect (pos->lnum-cnum (list line-1 line-2 line-3) 0) (list 0 0))
(check-expect (pos->lnum-cnum (list line-1 line-2 line-3) 5) (list 1 4))
(define (pos->lnum-cnum nelol posnum)
  (pos->lnum-cnum/acc nelol posnum 0))

; pos->lnum-cnum/acc : [NEList-of line] NatNum NatNum -> [List-of NatNum] 
; converts the single position of the cursor to a list containing line num and a col num
(check-expect (pos->lnum-cnum/acc (list line-1 line-2 line-3) 21 0) (list 2 3))
(check-expect (pos->lnum-cnum/acc (list line-1 line-2 line-3) 0 0) (list 0 0))
(check-expect (pos->lnum-cnum/acc (list line-1 line-2 line-3) 5 0) (list 1 4))
;(check-expect (pos->lnum-cnum/acc (list line-1 line-2 line-3) 
(define (pos->lnum-cnum/acc lol posnum linenum)
  (local [(define lfl (string-length (first lol)))]
  (cond [(empty? lol) (list linenum)]
        [(<  lfl posnum) (pos->lnum-cnum/acc (rest lol) (- posnum lfl) (add1 linenum))]
        [(>= lfl posnum) (list linenum posnum)])))

  


;; FISHING GAME
;; CS 5001 FINIAL PROJECT
;; JING WANG (JANE) (001636175)

#lang racket
#|
   The fishing game 
   --------------

   The fishing game is set in the sea where has abundant fishes
   and a boat that can hook the fish.

   The game is based on timed mode and the task is to hook the most fishes in 30 seconds.
   Getting fishes will give the player bonus time or cost more if you get the wrong fish:
   Normal fish: +1 seconds;
   Star fish: +10 seconds;
   Bone fish: -10 seconds.
   *Shark: Game over immediately.
   Different fishes have diverse speed, size and probability of occurrence.
   And with the time passed, fishes will speed up to make the game harder.

   The player uses three keys to control the boat:
    -- with "left" and "right" arrow keys, the boat will move towards the direction.
    -- with "down" key, the boat will send a hook down for the fish.
    -- with "space" key, the game will be paused.
   When the hook touch the fish, the fish will be hooked up and placed in the boat.
   
   Play
   ----
 
   Run and evaluate 
    (start-fishing)
   This will pop up a window with music for interacting with the program. 
|#

(require 2htdp/image 2htdp/universe)
(require lang/posn)
(require (prefix-in gui: racket/gui))
(require rsound)
;;** for including rsound library
;; 1. go to File/ Package Manager/ Available from Catalog
;; 2. Filter for rsound
;; 3. Click the rsound and install**


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Fishing World
;;;;;;;;;;;;;;;;;;;;;;;;;
;; -----------------------------------------------------------------------------
;; Data Definitions
;; The fishing World as Data:
(struct fishing-world (boat hook fishes goals counter bonus speedup pause) #:transparent)
;; A FishingWorld is a (fishing-world boat hook [listof swiming fishes] [listof hooked fishes] Nat bonus Nat)
;; The counter field is the time counter for counting down 30s.
;; The speedup field is the current increment adding to original fish speed.

(struct bonus (number posn fade) #:transparent)
;; A bonus is a (bonus Nat posn Nat)
;; The number field is the number of bonus time, +1 +10 or -10.
;; The fade field is a Natural Number that represents the number
;; of ticks until the bonus image fades. 

(struct fish (image [speed #:mutable] [posn #:mutable]) #:transparent)
;; A fish is a （fish size Nat posn)
;; reprenting a swimming fish with specific image, size, swimming speed and current position
(struct size (w h) #:transparent)
;; A size is a (size width height)
;; Represents the size or the image of the fish
 
(struct boat (posn) #:transparent)
;; A boat is a (boat posn), a boat as specific position

(struct hook (down? up? posn) #:transparent)
;; A hook is a (hook Boolean Boolean Nat posn)
;; The down? is a boolean value telling if the hook is moving down
;; the up? is a boolean value telling if the hook is moving down
;; the speed is the moving speed of the hook

(struct posn (x y) #:transparent #:mutable)
;; A posn is (posn number number)
;; Represents a two dimensional point

;; -----------------------------------------------------------------------------
;; Constants

;; Tick Rate 
(define TICK-RATE 1/28)

;; Graphical constants 
(define WIDTH-PX  600)
(define HEIGHT-PX 600)
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

;;Object speed
(define SPEED-FISH 60)
(define SPEED-SHARK 40)
(define SPEED-BONE 30)
(define SPEED-STAR 80)
(define SPEED-HOOK 200/28)
(define SPEED-MOVE 10)
(define SPEEDUP-RATE TICK-RATE)

;; Visual constants
;; Image frames: Diverse fishes, boat, hook, background elements
(define FISH-IMG (bitmap "graphics/FISH.png"))
(define SHARK-IMG  (bitmap "graphics/SHARK.png"))
(define BONE-IMG (bitmap "graphics/BONE.png"))
(define STAR-IMG (bitmap "graphics/STAR.png"))
(define BOAT-IMG (bitmap "graphics/BOAT.png"))
(define HOOK-IMG (bitmap "graphics/HOOK.png"))
(define SEAWEED-IMG (bitmap "graphics/SEAWEED.png"))
(define CORAL-IMG (bitmap "graphics/CORAL.png"))
(define BLUECORAL-IMG (bitmap "graphics/BLUECORAL.png"))
(define SUN-IMG (bitmap "graphics/SUN.png"))

;; Object position
(define boat-Start-point (posn (/ WIDTH-PX 2) (/ HEIGHT-PX 6)))
(define hook-Start-point (posn (posn-x boat-Start-point)
                              (+ (posn-y boat-Start-point)
                                 (/ (image-height BOAT-IMG) 2)
                                 (/ (image-height HOOK-IMG) 2))))
(define SUN-POSN (make-posn 530 70))
(define CORAL-POSN  (make-posn 200 518))
(define SEAWEED-POSN (make-posn 480 460))
(define BLUECORAL-POSN (make-posn 300 553))
(define SCORE-POSN (make-posn 150 50))
(define COUNTER-POSN (make-posn 50 50))


;; fonts & texts & colors
(define TEXT-SIZE 28)
(define TEXT-COLOR "orange")
(define END-TEXT-SIZE 50)
(define END-TEXT-COLOR "lightcoral")
(define LINE (make-pen "grey" 3 "solid" "round" "round"))


;; Sound files, Pstream and current length of the Pstream
;; game sound
(define CATCH-MU "graphics/CATCH.wav")
(define ERROR-MU "graphics/ERROR.wav")
(define GAMEOVER-MU "graphics/GAMEOVER.wav")
;; background sound 
(define BACK-MU (rs-read "graphics/SEA.wav"))
(define PSTREAM (make-pstream))
(define CUR-LEN 0)


;                                          
;                                          
;                                          
;                          ;               
;                          ;               
;  ;;;   ;;;                               
;   ;;   ;;                                
;   ; ; ; ;     ;;;;     ;;;      ;; ;;;   
;   ; ; ; ;    ;    ;      ;       ;;   ;  
;   ; ; ; ;         ;      ;       ;    ;  
;   ;  ;  ;    ;;;;;;      ;       ;    ;  
;   ;     ;   ;     ;      ;       ;    ;  
;   ;     ;   ;    ;;      ;       ;    ;  
;  ;;;   ;;;   ;;;; ;;  ;;;;;;;   ;;;  ;;; 
;                                          
;                                          
;                                          
;                                          
;; -----------------------------------------------------------------------------

;; Start the Game with music
(define (start-fishing)
 (go-fishing)
 (stop))
;; Start big bang of the game
(define (go-fishing)
  (big-bang (initialize-fishing-world)
    (on-tick fish-swimming TICK-RATE)
    (on-key direct-and-hook-fish)
    (to-draw render-fishing)
    (stop-when end-of-fishing? game-over-text)))

;; -> fishing-world
;; creates an fishing-world ready for fishing fishes
(define (initialize-fishing-world)  
  (let ([boat0 (boat boat-Start-point)]
        [fishes0 (list (initialize-fish))]
        [hook0 (hook #f #f hook-Start-point)]
        [goals0 '()]
        [counter0 30]
        [bonus0 (bonus 0 (posn 0 0) 28)]
        [speedup0 0]
        [pause0 #f])
        (fishing-world boat0 hook0 fishes0 goals0 counter0 bonus0 speedup0 pause0)))

;; -> fish
;; creates an random fish 
(define (initialize-fish)
    (case (random 12)
      [(0 2 4 6 8 10) (fish FISH-IMG SPEED-FISH (random-posn))]
      [(1 3 9 7) (fish BONE-IMG SPEED-BONE (random-posn))]
      [(11) (fish STAR-IMG SPEED-STAR (random-posn))]
      [(5) (fish SHARK-IMG SPEED-SHARK (random-posn))]))

;; -> posn
;; creates an random posn for fish       
(define (random-posn)
  (define half-height (/ HEIGHT-PX 2))
  (posn 0 (- (+ (random half-height)  half-height) 30)))


;                                                                                          
;                                                                                          
;                                                                                          
;     ;;;;    ;;                    ;;              ;;;;;;     ;            ;;             
;    ;   ;     ;                     ;              ;  ;                     ;             
;   ;          ;     ;;;;    ;;; ;   ; ;;;;            ;     ;;;     ;;; ;   ; ;;;;  ;;;;; 
;   ;          ;    ;    ;  ;;  ;;   ;  ;              ;       ;    ;;  ;;   ;  ;   ;    ; 
;   ;          ;    ;    ;  ;        ;;;               ;       ;    ;        ;;;     ;;;;  
;   ;          ;    ;    ;  ;        ; ;               ;       ;    ;        ; ;         ; 
;    ;   ;     ;    ;    ;  ;    ;   ;  ;              ;       ;    ;    ;   ;  ;   ;    ; 
;     ;;;    ;;;;;   ;;;;    ;;;;   ;;  ;;;           ;;;    ;;;;;   ;;;;   ;;  ;;; ;;;;;  
;                                                                                          
;                                                                                          
;                                                                                          
;                                                                                          

;; -----------------------------------------------------------------------------
;; fishing-world -> fishing-world
;; fishes swim, boat/hook/counter changes, speedup all the fishes
(define (fish-swimming w) 
  (let* ([fishes (fishing-world-fishes w)]
         [boat (fishing-world-boat w)]
         [hook (fishing-world-hook w)]
         [goals (fishing-world-goals w)]
         [counter (fishing-world-counter w)]
         [bonus (fishing-world-bonus w)]
         [speedup (fishing-world-speedup w)]
         [fish-catched (fish-hooked hook fishes)]
         [x-hook (posn-x (hook-posn hook))]
         [pause (fishing-world-pause w)])
     (playforever PSTREAM BACK-MU)
     (play-bonus fish-catched)
    (if pause
        w
        (fishing-world boat
                    (hook-act hook fish-catched)
                    (update-fishes fishes fish-catched counter speedup)
                    (update-goals goals fish-catched x-hook)
                    (update-counter counter fish-catched)
                    (update-bonus bonus fish-catched)
                    (update-speedup speedup)
                    pause))))

;; speedup -> speedup
;; increase the speedup number 
(define (update-speedup s)
  (+ s SPEEDUP-RATE))

;; bonus -> bonus
;; update current bonus of the world
(define (update-bonus b f)
  (cond [(and (not (= (bonus-number b) 0)) (empty? f) (> (bonus-fade b) 0)) (bonus (bonus-number b) (bonus-posn b) (- (bonus-fade b) 1))]
        [(<= (bonus-fade b) 0) (bonus 0 (posn 0 0) 28)]
        [(empty? f) b]
        [(bonus (time+ f) (fish-posn f) (- (bonus-fade b) 1))]))
         
;; counter -> counter
;; update current counter of the world, applying the bonus
(define (update-counter counter fish-catched)
  (define new-counter (+ (time+ fish-catched) (- counter TICK-RATE)))
  (if (< new-counter 0) 0
      new-counter))

;; fish -> Number
;; accoding to the fish type, return the number of bonus time, +1 +10 or -10.
(define (time+ f)
  (cond [(empty? f) 0]
        [(equal? (fish-image f) FISH-IMG) 1]
        [(equal? (fish-image f) STAR-IMG) 10]
        [(equal? (fish-image f) BONE-IMG) -10]
        [else 0]))

;; fishes(fish-list) -> fishes(fish-list)
;; Each second, create a new fish that has been speed up in the list
(define (update-fishes fishes fish-catched counter speedup)
  (define born (initialize-fish))
  (set-fish-speed! born (+ (fish-speed born) speedup))
  (set! fishes  (remove fish-catched fishes))
  (when (zero? (remainder (numerator counter) (denominator counter))) (set! fishes (cons born fishes))) 
  (map swim fishes)
  fishes)

;; fish -> fish
;; swim fish, move the x of the fish posn 
(define (swim f)
  (define fish-position (fish-posn f))
  (set-posn-x! fish-position (+ (posn-x fish-position) (* (fish-speed f) TICK-RATE)))
  (set-fish-posn! f fish-position))

;; goals -> goals
;; add new catched fish in goal-list
(define (update-goals goals fish-catched x-hook)
  (define (catch g)
    (define g-y (posn-y (fish-posn g)))
    (define in-boat-new-posn (posn x-hook g-y))
    (define catch-new-posn (posn x-hook (- g-y SPEED-HOOK)))
    (if (<= g-y (- (posn-y boat-Start-point) (/ (image-height (fish-image g)) 4)))
        (fish (fish-image g) (fish-speed g) in-boat-new-posn)
        (fish (fish-image g) (fish-speed g) catch-new-posn)))
  (if (empty? fish-catched) (map catch goals)
  (map catch (cons fish-catched goals))))

;; hook -> hook
;; change hook state and act the hook
(define (hook-act hook fish-catched)
  (cond [(and (<= (posn-y (hook-posn hook)) (posn-y hook-Start-point))
              (hook-up? hook))
         (reset hook)]
        [(hook-up? hook) (hook-up hook)]
        [(or (not (empty? fish-catched)) (>= (posn-y (hook-posn hook)) HEIGHT-PX)) (reverse hook)]
        [else (hook-down hook)]))

;; hook -> hook
;; make the hook state up, down? is #f, up? is #t
(define (reverse h)
  (hook #f #t (hook-posn h)))

;; hook -> hook
;; make the hook state original, down? is #f, up? is #f
(define (reset h)
  (hook #f #f (hook-posn h)))

;; hook -> hook
;; decrease the y of hook posn
(define (hook-up h)
  (define (posn-move p) (posn (posn-x p) (- (posn-y p) SPEED-HOOK)))
  (if (hook-up? h) (hook (hook-down? h) (hook-up? h) (posn-move (hook-posn h)))
      h))

;; hook -> hook
;; increase the y of hook posn
(define (hook-down h)
  (define (posn-move p) (posn (posn-x p) (+ (posn-y p) SPEED-HOOK)))
  (cond [(hook-down? h) (hook (hook-down? h) (hook-up? h) (posn-move (hook-posn h)))]
        [else h]))

;; hook fishes-> fish or empty
;; get the fish that has been hooked
(define (fish-hooked hook fishes)
  (cond [(empty? fishes) empty]
        [(fish-to-hook? hook (first fishes)) (first fishes)]
        [else (fish-hooked hook (rest fishes))]))

;; hook fish -> boolean
;; has the fish hooked?m
(define (fish-to-hook? hook fish)
  (define x-hook (posn-x (hook-posn hook)))
  (define y-hook (posn-y (hook-posn hook)))
  (define x-fish (posn-x (fish-posn fish)))
  (define y-fish (posn-y (fish-posn fish)))
  (and (<= x-hook (+ x-fish (/ (image-width (fish-image fish)) 2)))
       (>= x-hook (- x-fish (/ (image-width (fish-image fish)) 2)))
       (<= y-hook (+ y-fish (/ (image-height (fish-image fish)) 2)))
       (>= y-hook (- y-fish (/ (image-height (fish-image fish)) 2)))))

;                                                                                                      
;                                                                                                      
;                                                                                                      
;                                                                                                      
;   ;;; ;;;;
;    ;   ;
;    ;  ;       ;;;    ;;;   ;;;   ;;;; ;
;    ; ;       ;   ;    ;     ;   ;    ;;
;    ;;;;     ;     ;    ;   ;    ;
;    ;   ;    ;;;;;;;    ;   ;     ;;;;;
;    ;   ;    ;           ; ;           ;
;    ;    ;    ;    ;     ; ;     ;     ;
;   ;;;   ;;    ;;;;       ;      ;;;;;;
;                          ;
;                         ;
;                      ;;;;;
;                                                                                                      
;; -----------------------------------------------------------------------------

;; fishing-world KeyEvent -> fishing-world
;; Handle a key event
(define (direct-and-hook-fish w key)
  (fishing-world (direct-boat (fishing-world-boat w) key)
                 (direct-hook (fishing-world-hook w) key)
                 (fishing-world-fishes w)
                 (fishing-world-goals w)
                 (fishing-world-counter w)
                 (fishing-world-bonus w)
                 (fishing-world-speedup w)
                 (update-pause (fishing-world-pause w) key)))

;; Boat Direction-> boat
;; Move the boat towards direction
(define (direct-boat boat key)
  (cond [(or (string=? key "left") (string=? key "right"))
         (boat-move boat key)]
        [else boat]))

;; Boat Direction-> boat
;; change the posn of the boat towards direction
(define (boat-move b dir)
  (define (posn-change b dir)
    (define current-posn (boat-posn b))
    (cond [(string=? dir "left") (posn (- (posn-x current-posn) SPEED-MOVE) (posn-y current-posn))]
          [(string=? dir "right") (posn (+ (posn-x current-posn) SPEED-MOVE) (posn-y current-posn))]))
  (boat (posn-change b dir)))

;; hook Direction-> hook
;; move hook towards direction
(define (direct-hook hook key)
  (cond [(string=? key "down") (hook-move hook)]
        [(or (string=? key "left") (string=? key "right")) (hook-follow hook key)]
        [else hook]))

;; hook Direction-> hook
;; let hook follow the boat
(define (hook-follow h dir)
  (define (posn-change h dir)
    (define current-posn (hook-posn h))
    (cond [(string=? dir "left") (posn (- (posn-x current-posn) SPEED-MOVE) (posn-y current-posn))]
          [(string=? dir "right") (posn (+ (posn-x current-posn) SPEED-MOVE) (posn-y current-posn))]))
  (hook (hook-down? h) (hook-up? h) (posn-change h dir)))

;; hook -> hook
;; let hook start move, down? is #t
(define (hook-move h)
  (hook #t (hook-up? h) (hook-posn h)))

;;pause key- pause
;; return oppsite pause if the key is pause key
(define (update-pause pause key)
  (if (is-pause-key-event? key) (not pause)
      pause))
  

;; help function for key event
;; is-pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (is-pause-key-event? ke)
  (key=? ke " "))

;
;
;
; 
;                                      ;;
;   ;;;;;;                              ;
;    ;    ;                             ;
;    ;    ;     ;;;     ;; ;;;      ;;; ;     ;;;      ;;  ;;;
;    ;    ;    ;   ;     ;;   ;    ;   ;;    ;   ;      ;;;
;    ;;;;;    ;     ;    ;    ;   ;     ;   ;     ;     ;
;    ;  ;     ;;;;;;;    ;    ;   ;     ;   ;;;;;;;     ;
;    ;   ;    ;          ;    ;   ;     ;   ;           ;
;    ;    ;    ;    ;    ;    ;    ;   ;;    ;    ;     ;
;   ;;;   ;;    ;;;;    ;;;  ;;;    ;;; ;;    ;;;;     ;;;;;
; 
; 
; 
;                                                                                            
;; -----------------------------------------------------------------------------


;; fishing-world -> Scene
;; Draws the elements onto the scene

(define (render-fishing w)
  (define bonus (fishing-world-bonus w))
  (define fishes (fishing-world-fishes w))
  (define goals  (fishing-world-goals w))
  (define counter (fishing-world-counter w))
  (define current-boat-posn (boat-posn (fishing-world-boat w)))
  (define current-hook-posn (hook-posn (fishing-world-hook w)))
  (define fish-number (number->string (length goals)))

;;place all the background pictures
  (define background (place-images
   (list SUN-IMG
         CORAL-IMG
         SEAWEED-IMG
         BLUECORAL-IMG
         (text (string-append fish-number " fishes") TEXT-SIZE TEXT-COLOR)
         (text (string-append (number->string (quotient (numerator counter) (denominator counter))) " s") TEXT-SIZE TEXT-COLOR)
         BOAT-IMG
         HOOK-IMG)
   (list SUN-POSN
         CORAL-POSN
         SEAWEED-POSN
         BLUECORAL-POSN
         SCORE-POSN 
         COUNTER-POSN 
         (make-posn (posn-x current-boat-posn) (posn-y current-boat-posn))
         (make-posn (posn-x current-hook-posn) (posn-y current-hook-posn)))         
   MT-SCENE))

;; draw the line between hook and boat
  (define add-line-boat&hook (scene+line background
            (posn-x current-boat-posn)
            (+ (posn-y current-boat-posn) (/ (image-height BOAT-IMG) 2))
            (posn-x current-hook-posn)
            (- (posn-y current-hook-posn) (/ (image-height HOOK-IMG) 2))
            LINE))
  
;; draw all the fishes 
  (define fishes+scene (fish+scene (append goals fishes) add-line-boat&hook))
  (if (= 0 (bonus-number bonus)) fishes+scene
      (place-image (text (if (< (bonus-number bonus) 0)
                         (string-append (number->string (bonus-number bonus)) " s")
                         (string-append "+"(number->string (bonus-number bonus)) " s"))
                         TEXT-SIZE TEXT-COLOR)
               (posn-x (bonus-posn bonus))
               (posn-y (bonus-posn bonus))
               fishes+scene)))

;; [Listof fish] Scene -> Scene
;; draws all of the fishes(fish-list) to a scene
(define (fish+scene fish-list scene)
    (if (empty? fish-list) scene
        (put-fish-in-scene (first fish-list) (fish+scene (rest fish-list) scene))))

;; ;; [fish] Scene -> Scene
;; draws one fish to a scene
(define (put-fish-in-scene fish scene)
  (place-image (fish-image fish)
             (posn-x (fish-posn fish))
             (posn-y (fish-posn fish))
             scene))
                                                     

;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;                            ;;                                                    
;   ;;;;;;;                   ;               ;;;; ;                               
;    ;    ;                   ;              ;    ;;                               
;    ;    ;   ;; ;;;      ;;; ;             ;           ;;;;   ;; ;  ;      ;;;    
;    ;  ;      ;;   ;    ;   ;;             ;          ;    ;   ;; ;; ;    ;   ;   
;    ;;;;      ;    ;   ;     ;             ;               ;   ;  ;  ;   ;     ;  
;    ;  ;      ;    ;   ;     ;             ;   ;;;;;  ;;;;;;   ;  ;  ;   ;;;;;;;  
;    ;    ;    ;    ;   ;     ;             ;      ;  ;     ;   ;  ;  ;   ;        
;    ;    ;    ;    ;    ;   ;;              ;     ;  ;    ;;   ;  ;  ;    ;    ;  
;   ;;;;;;;   ;;;  ;;;    ;;; ;;              ;;;;;    ;;;; ;; ;;; ;; ;;    ;;;;   
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;; -----------------------------------------------------------------------------


;; fishing-world -> Boolean
;; Determine if the game is end
(define (end-of-fishing? w)
  (define fishes (fishing-world-fishes w))
  (define goals (fishing-world-goals w))
  (define counter (fishing-world-counter w))
  (or (get-shark? goals) (time-out? counter)))

;; counter -> Boolean
;; Determine if the counter is 0
(define (time-out? counter)
  (if (<= counter 0) (begin (playsound GAMEOVER-MU) #t) 
      #f))

;; goals -> Boolean
;; Determine if the shark is hooked
(define (get-shark? goals)
  (cond [(empty? goals) #f]
        [(equal? (fish-image (first goals)) SHARK-IMG)
         (begin (playsound GAMEOVER-MU) #t)]
        [else #f]))

;; Displays a game over text, when the game is over
(define (game-over-text w)
  (define goals (fishing-world-goals w))
  (define fish-number (number->string (length goals)))
  (overlay (text/font (string-append "     GAME  OVER\n YOU GET " fish-number " FISHES")
                      END-TEXT-SIZE END-TEXT-COLOR "Terminal" 'default 'normal 'bold #f) MT-SCENE))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;Music
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pstream sound -> pstream
;; loop a sound file forever until (stop)
(define (playforever pstream sound)
  (define current-p (pstream-current-frame pstream))
  (when (>= current-p CUR-LEN)
    (pstream-queue pstream sound current-p)
    (set! CUR-LEN (+ CUR-LEN (rs-frames sound)))))

;; fish -> sound
;; play a sound according to fish type
(define (play-bonus f)
 (cond [(and (not (empty? f))
             (or (equal? BONE-IMG (fish-image f))
                 (equal? SHARK-IMG (fish-image f))))
         (playsound ERROR-MU)]
        [(not (empty? f)) (playsound CATCH-MU)]))

;; play a sound fileusing play-sound
(define (playsound sound)
 (gui:play-sound sound #t))

(start-fishing)
  

  
  

  










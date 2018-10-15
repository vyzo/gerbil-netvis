;;; -*- Gerbil -*-
;;; © vyzo
;;; network visualization

(import :gerbil/gambit/threads
        :gerbil/gambit/random
        :std/iter
        :std/misc/shuffle
        :std/srfi/1
        :vyzo/cairo)
(export #t)

(def pi 3.14159)

(def white  [1. 1. 1.])
(def grey   [.5 .5 .5])
(def red    [1. 0. 0.])
(def green  [0. 1. 0.])
(def blue   [0. 0. 1.])
(def yellow [1. 1. 0.])
(def orange [1. .65 0.])

(defstruct node (id pos sz))

(def (make-network count (width 1600) (height 900) (size 10))
  (def x0 (/ width 2))
  (def y0 (/ height 2))
  (def r (- (min x0 y0) (* 2 size)))
  (def dphi (/ (* 2 pi) count))
  (let lp ((i 0) (net []))
    (if (= i count)
      (reverse net)
      (let* ((phi (* i dphi))
             (x (exact->inexact (+ x0 (* r (cos phi)))))
             (y (exact->inexact (+ y0 (* r (sin phi))))))
        (lp (1+ i)
            (cons (make-node i [x y] (exact->inexact size)) net))))))

(def (make-random-graph net (count 10))
  (for/fold (r []) (n net)
    (let (net* (shuffle (filter (lambda (n*) (not (eq? n n*))) net)))
      (foldl (lambda (n* r) (cons (cons n n*) r)) r (take net* count)))))

(def (make-random-trace links (step 10) (len 100))
  (let lp ((i 0) (trace []))
    (if (= i len)
      (reverse trace)
      (let lp2 ((j 0) (pkts []))
        (if (< j step)
          (lp2 (1+ j)
               (cons (list-ref links (random-integer (length links))) pkts))
          (lp (1+ i) (cons pkts trace)))))))

(def (draw-bg cr color)
  (with ([r g b] color)
    (cairo-set-source-rgb cr r g b)
    (cairo-paint cr)))

(def (draw-node cr n color)
  (with (((node _ [x y] sz) n)
         ([r g b] color))
    (cairo-set-source-rgb cr r g b)
    (cairo-move-to cr x y)
    (cairo-arc cr x y sz 0. (* 2 pi))
    (cairo-fill cr)))

(def (draw-edge cr n1 n2 color (lsz .1) (width 1600.) (height 900.))
  (let ((values n1 n2)
        (if (> (node-id n1) (node-id n2))
          (values n1 n2)
          (values n2 n1)))
    (with (((node _ [x1 y1]) n1)
           ((node _ [x2 y2]) n2)
           ([r g b] color))
      (cairo-set-line-width cr lsz)
      (cairo-set-source-rgb cr r g b)
      (cairo-move-to cr x1 y1)
      (let ((X (/ width 2)) (Y (/ height 2)))
        (cairo-curve-to cr x1 y1 X Y x2 y2))
      (cairo-stroke cr))))

(def (draw-graph cr nodes links color (lsz .1))
  (for (n nodes)
    (draw-node cr n color))
  (for ([n1 . n2] links)
    (draw-edge cr n1 n2 color lsz)))

(def (draw-frame cr underlay (overlay1 #f) (overlay2 #f))
  (cairo-push-group cr)
  (draw-bg cr white)
  (with ([nodes . links] underlay)
    (draw-graph cr nodes links blue))
  (when overlay1
    (with ([nodes . links] overlay1)
      (draw-graph cr nodes links orange 1.0)))
  (when overlay2
    (with ([nodes . links] overlay2)
      (draw-graph cr nodes links red 1.0)))
  (cairo-pop-group-to-source cr)
  (cairo-paint cr))

(def (animate! underlay overlay trace
               delta: (delta .1)
               width: (width 1600)
               height: (height 900))
  (def sfc (cairo-create-x11-surface width height))
  (def cr (cairo-create sfc))
  (def (draw! underlay overlay overlay2)
    (draw-frame cr underlay overlay overlay2)
    (cairo-surface-flush sfc)
    (cairo-x11-pump-events sfc))
  (def (fini!)
    (cairo-destroy cr)
    (cairo-destroy-x11-surface sfc))

  (draw! underlay overlay [[]])
  (thread-sleep! delta)
  (for (packets trace)
    (draw! underlay overlay (cons (map car packets) packets))
    (thread-sleep! delta))
  (draw! underlay overlay [[]])
  (thread-sleep! delta)
  #;(fini!)
  )

(def (animate-floodsub! trace-file node-count delta: (delta .1))
  (def trace (call-with-input-file trace-file (cut read-all <> read)))
  (def nodes (make-network (1+ node-count)))
  (def nodetab
    (let (ht (make-hash-table-eq))
      (for (n nodes)
        (hash-put! ht (node-id n) n))
      ht))
  (def edges [])
  (def packets [])

  (let lp ((rest trace) (ts 0) (pkts []))
    (match rest
      ([t . rest]
       (defrules with-ts ()
         ((_ ts* body)
          (if (> (- ts* ts) .01)
            (begin
              (unless (null? pkts)
                (set! packets (cons pkts packets)))
              (lp (cons t rest) ts* []))
            body)))
       (match t
         (['trace ts* src dst what]
          (with-ts ts*
            (match what
              (['!!pubsub.connect]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn))
                      (dst-src (cons dstn srcn)))
                 (if (or (member src-dst edges)
                         (member dst-src edges))
                   (lp rest ts pkts)
                   (begin
                     (set! edges (cons src-dst edges))
                     (lp rest ts pkts)))))
              (['!!pubsub.publish . _]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn)))
                 (lp rest ts (cons src-dst pkts)))))))
         (['publish ts* src #f msg]
          (with-ts ts*
            (lp rest ts pkts)))
         (['deliver ts* src #f msg]
          (with-ts ts*
            (lp rest ts pkts)))))
      (else
       (unless (null? pkts)
         (set! packets (cons pkts packets)))
       (set! packets (reverse packets)))))

  (animate! (cons nodes edges) #f packets delta: delta))


(def (animate2! underlay trace
                delta: (delta .1)
                width: (width 1600)
                height: (height 900))
  (def sfc (cairo-create-x11-surface width height))
  (def cr (cairo-create sfc))
  (def (draw! underlay overlay overlay2)
    (draw-frame cr underlay overlay overlay2)
    (cairo-surface-flush sfc)
    (cairo-x11-pump-events sfc))
  (def (fini!)
    (cairo-destroy cr)
    (cairo-destroy-x11-surface sfc))

  (draw! underlay #f [[]])
  (thread-sleep! delta)
  (for (packets trace)
    (let lp ((rest packets) (o1 []) (o2 []))
      (match rest
        ([pkt . rest]
         (case (car pkt)
           ((CONTROL)
            (lp rest (cons (cdr pkt) o1) o2))
           ((DATA)
            (lp rest o1 (cons (cdr pkt) o2)))
           (else
            (error "Unexpected packet" pkt))))
        (else
         (draw! underlay (cons (map car o1) o1) (cons (map car o2) o2)))))
    (thread-sleep! delta))
  (draw! underlay #f [[]])
  (thread-sleep! delta)
  #;(fini!)
  )

(def (animate-gossipsub! trace-file node-count delta: (delta .1))
  (def trace (call-with-input-file trace-file (cut read-all <> read)))
  (def nodes (make-network (1+ node-count)))
  (def nodetab
    (let (ht (make-hash-table-eq))
      (for (n nodes)
        (hash-put! ht (node-id n) n))
      ht))
  (def edges [])
  (def packets [])

  (let lp ((rest trace) (ts 0) (pkts []))
    (match rest
      ([t . rest]
       (defrules with-ts ()
         ((_ ts* body)
          (if (> (- ts* ts) .01)
            (begin
              (unless (null? pkts)
                (set! packets (cons pkts packets)))
              (lp (cons t rest) ts* []))
            body)))
       (match t
         (['trace ts* src dst what]
          (with-ts ts*
            (match what
              (['!!pubsub.connect]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn))
                      (dst-src (cons dstn srcn)))
                 (if (or (member src-dst edges)
                         (member dst-src edges))
                   (lp rest ts pkts)
                   (begin
                     (set! edges (cons src-dst edges))
                     (lp rest ts pkts)))))
              (['!!pubsub.publish . _]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn)))
                 (lp rest ts (cons (cons 'DATA src-dst) pkts))))
              #;([(or '!!gossipsub.ihave '!!gossipsub.iwant '!!gossipsub.graft '!!gossipsub.prune) . _]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn)))
                 (lp rest ts (cons (cons 'CONTROL src-dst) pkts))))
              ([(or '!!gossipsub.ihave '!!gossipsub.iwant) . _]
               (let* ((srcn (hash-ref nodetab src))
                      (dstn (hash-ref nodetab dst))
                      (src-dst (cons srcn dstn)))
                 (lp rest ts (cons (cons 'CONTROL src-dst) pkts))))
              ([(or '!!gossipsub.graft '!!gossipsub.prune) . _]
               (lp rest ts pkts))
              )))
         (['publish ts* src #f msg]
          (with-ts ts*
            (lp rest ts pkts)))
         (['deliver ts* src #f msg]
          (with-ts ts*
            (lp rest ts pkts)))))
      (else
       (unless (null? pkts)
         (set! packets (cons pkts packets)))
       (set! packets (reverse packets)))))

  (animate2! (cons nodes edges) packets delta: delta))
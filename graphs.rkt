#lang racket
(define g '((1 2 3) (2 3 6) (3 4 6) (4 1 5) (5 3) (6 5) (7)))

(define (nodes G)
  (map car G))

(nodes g)

(define (edges G)
  (apply append (map
                 (lambda (v)
                   (map (lambda (x)
                          (list (car v) x))
                        (cdr v)))
                 G)))

(edges g)

(define (contains? x L)
  (cond ((null? L) #f)
        ((equal? x (car L)) #t)
        (else (contains? x (cdr L)))))

(define (search f L)
  (cond ((null? L) #f)
        ((f (car L)))
        (else (search f (cdr L)))))

(define (edge-exists? start end G)
  (contains? end (cdr (assoc start G))))

(edge-exists? 2 6 g)
(edge-exists? 2 4 g)

(define (successors node G)
  (cdr (assoc node G)))

(successors 2 g)

(define (bfs-path u v g)
  (define (extend path)
    (map (lambda (c) (cons c path)) (cdr (assoc (car path) g))))

  (define (extend-acyclic path)
    (filter (lambda (p) (not (contains? (car p) (cdr p)))) (extend path)))

  (define (extend-level l)
    (apply append (map extend-acyclic l)))

  (define (bfs-level l)
    (cond ((null? l) #f)
          ((search (lambda (path) (if (equal? (car path) v) path #f)) l))
          (else (bfs-level (extend-level l)))))

  (let ((result (bfs-level (list (list u)))))
    (if result
        (reverse result)
        #f)))

(bfs-path 1 6 g)

(define (dfs-path u v g)
  (define (dfs-path-search path)
    (cond ((eq? (car path) v) (reverse path))
          ((contains? (car path) (cdr path)) #f)
          (else (search (lambda (c) (dfs-path-search (cons c path))) (successors (car path) g)))))
  (dfs-path-search (list u)))

(dfs-path 1 6 g)

(define (dfs-contains? path g)
  (define (dfs-search ps current-node)
    (cond ((null? ps) #t)
          ((= (car ps) current-node) (search (lambda (c) (dfs-search (cdr ps) c)) (successors (car ps) g)))
          (else #f)))
  (search (lambda (c) (dfs-search path c)) (nodes g)))

(dfs-contains? (list 2 3 6) g)

(define (has-cycle? g)
  (define (dfs-search path)
    (cond ((memq (car path) (cdr path)) #t)
          (else (search (lambda (c) (dfs-search (cons c path))) (successors (car path) g)))))
  (search (lambda (c) (dfs-search (list c))) (nodes g)))

(has-cycle? g)

(define g1 '((1 2 3) (2 3 6) (3 4 6) (4 5) (5) (6 5) (7)))
(has-cycle? g1)
(module loganal mzscheme
  (require (lib "pregexp.ss"))
  (require (lib "32.ss" "srfi"))
  (require (prefix s1: (lib "1.ss" "srfi")))
  (require (planet "aif.ss" ("schematics" "macro.plt" 1)))
  (require (lib "dns" "net"))

  (define selector-hash (make-hash-table 'equal))
  (define ip-hash (make-hash-table 'equal))
  (define month-hash (make-hash-table 'equal))

  (define-struct month-entry (hits days))

  (define (hashinc! hash key)
    (let ((val (hash-table-get hash key 0)))
      (hash-table-put! hash key (+ 1 val))))

  (define (processline type date ip selector . args)
    (apply
      (lambda (year month day h m s)
        (let ((monthspec (string-append year "-" month)))
          (aif ent (hash-table-get month-hash monthspec #f)
            (begin
              (set-month-entry-hits!
                ent (+ 1 (month-entry-hits ent)))
              (vector-set!
                (month-entry-days ent) (string->number day)
                (vector-ref
                  (month-entry-days ent) (string->number day))))
            (begin
              (hash-table-put!
                month-hash monthspec
                (make-month-entry
                  1
                  (let ((v (make-vector 31)))
                    (vector-set! v (string->number day) 1)
                    v))))))

        (hashinc! selector-hash selector)
        (hashinc! ip-hash ip))
      (cdr (pregexp-match "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2}):(\\d{2})" date))))

  (define (nextline)
    (let ((l (read-line)))
      (when (not (eof-object? l))
        (apply processline (pregexp-split "\t" l))
        (nextline))))

  (define (hash-top hash n)
    (let* ((l (list-sort
                (lambda (a1 b1)
                  (> (cdr a1) (cdr b1)))
                (hash-table-map hash (lambda (k v) (cons k v)))))
           (ll (length l)))
      (s1:take l (if (< n ll) n ll))))

  (define (writemult str cnt)
    (let wr ((c cnt))
      (display str)
      (if (> c 1) (wr (- c 1)))))

  (define (row-print format row)
    (let field ((tcell row)
                (fcell format))
      (when (pair? tcell)
        (let ((sl (string-length (car tcell)))
              (just (caar fcell))
              (len (cadar fcell)))
          (if (equal? 'l just)
            (begin
              (display (car tcell))
              (if (< sl len)
                (writemult " " (- len sl))))
            (begin
              (if (< sl len)
                (writemult " " (- len sl)))
              (display (car tcell))))
          (display "  ")
          (field (cdr tcell) (cdr fcell)))))
    (display "\n"))

  (define (table-lengths table)
    (map (lambda (col)
           (apply max
                  (map (lambda (row)
                         (string-length (list-ref row col)))
                       table)))
         (s1:iota (length (car table)))))

  (define (table-print format table)
    (let* ((lengths (table-lengths table))
           (form (map (lambda (f l) (list f l)) format lengths)))
      (for-each (lambda (row)
                  (row-print form row))
                table)))

  (define (place-table top)
    (table-print
      '(r r l)
      (map (lambda (place row)
             (list (string-append (number->string (+ 1 place)) ".")
                   (number->string (cdr row))
                   (car row)))
           (s1:iota (length top))
           top)))

  (nextline)

  (display "\nMonthly hits:\n")
  (table-print
    '(l r)

    (map (lambda (month)
           (list
             month
             (number->string
               (month-entry-hits (hash-table-get month-hash month)))))
         (list-sort
           (lambda (a b) (string<? a b))
           (hash-table-map month-hash (lambda (k v) k)))))

  (display "\nTop selectors:\n")
  (place-table (hash-top selector-hash 20))
  (display "\nTop hosts:\n")

  (let ((dns (dns-find-nameserver))
        (top (hash-top ip-hash 20)))
    (table-print
      '(r r l l)
      (map (lambda (place row)
             (list (string-append (number->string (+ 1 place)) ".")
                   (number->string (cdr row))
                   (car row)
                   (with-handlers
                     ((exn? (lambda (exn) "?")))
                     (sleep 0.1)
                     (dns-get-name dns (car row)))))
           (s1:iota (length top))
           top)))
  )

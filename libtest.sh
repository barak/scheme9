#!/bin/sh

testfile=libtest.scm

trap '
	cleanup
	exit 1
' 1 2 3 15

cleanup() {
	rm -f $testfile
}

cat >$testfile <<EOT
(define Errors 0)

(define (check src expr result)
  (if (not (equal? expr result))
      (begin (write src)
             (display " FAILED!")
             (newline)
             (display "Expected: ")
             (write result)
             (newline)
             (display "But got:  ")
             (write expr)
             (newline)
             (set! Errors (+ 1 Errors)))))

(define-syntax test
  (syntax-rules (==>)
    ((_) #t)
    ((_ expr ==> result)
       (check 'expr expr 'result))
    ((_ expr ==> result . more)
       (begin (check 'expr expr 'result)
              (test . more)))))

EOT

for f in lib/*.scm contrib/*.scm; do
	if grep '^; Example: ' $f >/dev/null 2>&1; then
		echo "(load-from-library \"`basename $f`\")" >>$testfile
		echo "(test" >>$testfile
		sed -ne '/^; Example: /,/^$/p' <$f | \
			sed -e '/^$/d' | \
			sed -e 's/^;..........//' >>$testfile
		echo ")" >>$testfile
		echo "" >>$testfile
	fi
done

cat >>$testfile <<EOT
(if (= 0 Errors)
    (begin (display "Everything fine!")
           (newline)))
EOT

trap '
	exit 1
' 1 2 3 15

env S9FES_LIBRARY_PATH=.:lib ./s9 -nf libtest.scm
(ns tp-lisp-interpreter.interpreter-test
  (:require [clojure.test :refer :all]))

(defn controlar-aridad
  "Si la longitud de una lista dada es la esperada, devuelve esa longitud.
   Si no, devuelve una lista con un mensaje de error (una lista con *error* como primer elemento)."
  [elm expected-arity]
  (cond (= (count elm) expected-arity) expected-arity
        (> (count elm) expected-arity)  (list '*error* 'too-many-args)
        (< (count elm) expected-arity)  (list '*error* 'too-few-args)
        :else  (list '*error* 'something-wrong-happened))
  )

(deftest controlar-aridad-test
  (testing "controlar aridad."
    (is (= (controlar-aridad '(a b c) 3) 3))
    (is (= (controlar-aridad '(1 2 3) 2) '(*error* too-many-args)))
    (is (= (controlar-aridad '(1 2 3) 4) '(*error* too-few-args)))
    )
  )


(defn igual?
  "Verifica la igualdad entre dos elementos al estilo de TLC-LISP (case-insensitive)."
  [elm1 elm2]
  (cond (and (symbol? elm1) (symbol? elm2))
          (= (clojure.string/lower-case (str elm1)) (clojure.string/lower-case (str elm2)))
        (and (coll? elm1) (coll? elm2))
          (if (= (count elm1) (count elm2))
            (every? identity (map igual? elm1 elm2))
            false)
        (and (= elm1 'NIL) (= elm2 nil))
          true
        (and (= elm1 nil) (= elm2 'NIL))
          true
        (= elm1 elm2)
          true
        :else
         false
        )
  )

(deftest igual?-test
  (testing "controlar igual."
    (is (= (igual? 1 1) true))
    (is (= (igual? 1 2) false))
    (is (= (igual? 'a 'a) true))
    (is (= (igual? 'A 'A) true))
    (is (= (igual? 'A 'a) true))
    (is (= (igual? 'a 'A) true))
    (is (= (igual? 'a 'b) false))
    (is (= (igual? '(a b c) '(A B C)) true))
    (is (= (igual? '(a b c) '(A B D)) false))
    (is (= (igual? nil nil) true))
    (is (= (igual? nil 'NIL) true))
    (is (= (igual? 'NIL  nil) true))
    (is (= (igual? 'NIL 'NIL) true))
    (is (= (igual? () ()) true))
    (is (= (igual? () '(nil)) false))
    (is (= (igual? "a" "a") true))
    (is (= (igual? "a" "A") false))
    (is (= (igual? 'a "a") false))
    (is (= (igual? 'a "A") false))
    )
  )

(defn error?
  "Devuelve true o false, segun sea o no el arg. un mensaje de error (una lista con *error* como primer elemento)."
  [l]
  (cond
    (not (coll? l)) false
    (and (symbol? (first l))
         (= (clojure.string/lower-case (str (first l))) "*error*"))  true
    :else false)
  )

(deftest error?-test
  (testing "controlar igual."
    (is (= (error? '(*error* too-few-args)) true))
    (is (= (error? (list '*error* 'too-few-args)) true))
    (is (= (error? (list '*ERROR* 'too-few-args)) true))
    (is (= (error? (list '*Error* 'too-few-args)) true))
    (is (= (error? (list 'too-few-args)) false))
    (is (= (error? (list '*error*)) true))
    (is (= (error? '*error*) false))
    (is (= (error? ()) false))
    (is (= (error? nil) false))
    )
  )

(defn revisar-fnc
  "Si la lista es un mensaje de error, lo devuelve; si no, devuelve nil."
  [l]
  (if (error? l) l nil)
  )

(deftest revisar-fnc-test
  (testing "revisar fnc."
    (is (= (revisar-fnc '(*error* too-few-args)) '(*error* too-few-args)))
    (is (= (revisar-fnc '('too-few-args)) nil))
    (is (= (revisar-fnc '*error*) nil))
    (is (= (revisar-fnc nil) nil))
    (is (= (revisar-fnc ()) nil))
    )
  )


(defn revisar-lae
  "Devuelve el primer elemento que es un mensaje de error. Si no hay ninguno, devuelve nil."
  [l]
  (cond (empty? l) nil
        (coll? l) (if (error? (first l)) (first l) (revisar-lae (rest l)))
        :else nil)
  )

(deftest revisar-lae-test
  (testing "revisar fnc."
    (is (= (revisar-lae '(1 2 3)) nil))
    (is (= (revisar-lae nil) nil))
    (is (= (revisar-lae ()) nil))
    (is (= (revisar-lae '(1 (*error* too-few-args) 3)) '(*error* too-few-args)))
    (is (= (revisar-lae '(1 (*error* too-few-args) (*error* too-many-args) 3)) '(*error* too-few-args)))
    )
  )


(defn replace-if-match
  "Given a pair of a symbol and a value, replace the value for the
  pair if the symbol match the first of the pair"
  [symbol value pair]
  (if (igual? (first pair) symbol)
    (list symbol value)
    pair)
  )

(defn actualizar-amb
  "Devuelve un ambiente actualizado con una clave (nombre de la variable o funcion) y su valor.
  Si el valor es un error, el ambiente no se modifica. De lo contrario, se le carga o reemplaza el valor."
  [amb symbol value]
  (cond (error? value) amb
        (empty? amb) (list symbol value)
        (some #{symbol} amb) (apply concat (map (partial replace-if-match symbol value) (partition 2 amb)))
        :else (seq (conj (vec amb) symbol value)))
  )

(deftest actualizar-amb-test
  (testing "actualizar amb test."
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'd 4) '(a 1 b 2 c 3 d 4)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b 4) '(a 1 b 4 c 3)))
    (is (= (actualizar-amb '(a 1 b 2 c 3) 'b (list '*error* 'mal 'hecho)) '(a 1 b 2 c 3)))
    (is (= (actualizar-amb () 'b 7) '(b 7))))
  )

(defn check-if-match
  "Given a pair of a symbol and a value, replace the value for the
  pair if the symbol match the first of the pair"
  [symbol pair]
  (igual? (first pair) symbol)
  )


(defn buscar
  "Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
   y devuelve el valor asociado. Devuelve un mensaje de error si no la encuentra."
  [symbol amb]
  (let [found-pair (first (filter (partial check-if-match symbol) (partition 2 amb)))]
    (if found-pair (second found-pair) (list '*error* 'unbound-symbol symbol))
    )
  )

(deftest buscar-amb-test
  (testing "actualizar amb test."
    (is (= (buscar 'c '(a 1 b 2 c 3 d 4 e 5)) 3)
    (is (= (buscar 'f '(a 1 b 2 c 3 d 4 e 5)) '(*error* unbound-symbol f)))
  )))

(defn spy
  ([x] (do (prn x) (prn) x))
  ([msg x] (do (print msg) (print ": ") (prn x) (prn) x))
  )


(defn fnc-append
  "Devuelve el resultado de fusionar 2 sublistas."
  [l]
  (let [ari (controlar-aridad l 2)]
    (cond (seq? ari) ari
          (and (not (nil? (first l))) (not (coll? (first l)))) (spy (list '*error* 'list 'expected (first l)))
          (and (not (nil? (second l))) (not (coll? (second l)))) (list '*error* 'list 'expected (second l))
          (and (nil? (first l)) (nil? (second l))) nil
          (and (empty? (first l)) (empty? (second l))) nil
    :else (concat (first l) (second l))))
  )

(deftest fnc-append-test
  (testing "fnc append test."
    (is (= (fnc-append '( (1 2) )) '(*error* too-few-args)))
    (is (= (fnc-append '( (1 2) (3) (4 5) (6 7) )) '(*error* too-many-args)))
    (is (= (fnc-append '( (1 2) 3 )) '(*error* list expected 3)))
    (is (= (fnc-append '( (1 2) A )) '(*error* list expected A)))
    (is (= (fnc-append '( (1 2) (3))) '(1 2 3)))
    (is (= (fnc-append '( (1 2) nil )) '(1 2)))
    (is (= (fnc-append '( () (1 2) )) '(1 2)))
    (is (= (fnc-append '(nil nil)) nil))
    (is (= (fnc-append '(() ())) nil))
    )
  )

; user=> (fnc-env () '(a 1 b 2) '(c 3 d 4))
; (a 1 b 2 c 3 d 4)
; user=> (fnc-env '(5) '(a 1 b 2) '(c 3 d 4))
; (*error* too-many-args)
(defn fnc-env
  "Devuelve la fusion de los ambientes global y local."
  [args amb-local amb-global]
  (cond (not (empty? args)) (list '*error* 'too-many-args)
        :else (concat amb-local amb-global))
  )

(deftest fnc-env-test
  (testing "fnc append test."
    (is (= (fnc-env () '(a 1 b 2) '(c 3 d 4)) '(a 1 b 2 c 3 d 4)))
    (is (= (fnc-env '(5) '(a 1 b 2) '(c 3 d 4)) '(*error* too-many-args))))
  )

; user=> (fnc-equal '(1 1))
; t
; user=> (fnc-equal '(A a))
; t
; user=> (fnc-equal '("1" "1"))
; t
; user=> (fnc-equal '(nil NIL))
; t
; user=> (fnc-equal '(1 2))
; nil
; user=> (fnc-equal '(A B))
; nil
; user=> (fnc-equal '("1" 1))
; nil


; user=> (fnc-equal ())
; (*error* too-few-args)
; user=> (fnc-equal '(A))
; (*error* too-few-args)
; user=> (fnc-equal '(A a A))
; (*error* too-many-args)
(defn transf-bool [boolean] (if boolean 't nil))

(defn fnc-equal
  "Compara 2 elementos. Si son iguales, devuelve t. Si no, nil."
  [args]
  (let [ari (controlar-aridad args 2)]
    (cond (seq? ari) ari
          :else (transf-bool (igual? (first args) (second args))))
    ))

(deftest fnc-equal-test
  (testing "fnc append test."
    (is (= (fnc-equal '(1 1)) 't))
    (is (= (fnc-equal '(A a)) 't))
    (is (= (fnc-equal '("1" "1")) 't))
    (is (= (fnc-equal  '(nil NIL)) 't))
    (is (= (fnc-equal '(1 2)) nil))
    (is (= (fnc-equal '(A B)) nil))
    (is (= (fnc-equal '("1" 1)) nil))
    (is (= (fnc-equal ()) '(*error* too-few-args)))
    (is (= (fnc-equal '(A)) '(*error* too-few-args)))
    (is (= (fnc-equal '(A a A)) '(*error* too-many-args)))
  ))

; user=> (fnc-read ())
; 1
; 1
; user=> (fnc-read ())
; a
; a
; user=> (fnc-read ())
; "hola"
; "hola"
; user=> (fnc-read ())
; (hola mundo)
; (hola mundo)
; user=> (fnc-read ())
; (hola
; mundo)
; (hola mundo)
; user=> (fnc-read ())
; ()
; nil
; user=> (fnc-read ())
; nil
; nil
; user=> (fnc-read '(1))
; (*error* not-implemented)
; user=> (fnc-read '(1 2))
; (*error* not-implemented)
(defn fnc-read
  "Devuelve la lectura de un elemento de TLC-LISP desde la terminal/consola."
  [args]
  (let [screen-read (read)]
    (cond (not (empty? args)) (list '*error* 'not-implemented)
          (= 'quote (first screen-read))
            (if (empty? screen-read)
              nil
              (list '*error* 'not-implemented))
          :else screen-read))
  )
; Ver después como hacer los tests porque no me salen

; user=> (fnc-terpri ())
;
; nil
; user=> (fnc-terpri '(1))
; (*error* not-implemented)
; user=> (fnc-terpri '(1 2))
; (*error* not-implemented)
(defn fnc-terpri
  "Imprime un salto de línea y devuelve nil."
  [l]
  (if (empty? l)
    (do (print "\n") nil)
    (list '*error* 'not-implemented)
    )
  )

(deftest fnc-terpri-test
  (testing "fnc terpri test."
    (is (= (fnc-terpri '(1)) '(*error* not-implemented)))
    (is (= (fnc-terpri '(1 2)) '(*error* not-implemented)))
    (is (= (fnc-terpri ()) nil))
    )
  )

(defn fail-if-not-number
  "fails if the argument is not a number."
  [n]
  (cond (number? n) n
        :else (list '*error* 'number-expected n)))

(defn fnc-add
  "Suma los elementos de una lista. Minimo 2 elementos."
  [l]
  (cond (>= (count l) 2)
        (let [err (revisar-lae (map fail-if-not-number l))]
          (if (= err nil) (apply + l) err))
        :else
          (list '*error* 'too-few-args)
        )
  )


(deftest fnc-add-test
  (testing "fnc add test."
    (is (= (fnc-add ()) '(*error* too-few-args)))
    (is (= (fnc-add '(3)) '(*error* too-few-args)))
    (is (= (fnc-add '(3 4)) 7))
    (is (= (fnc-add '(3 4 5)) 12))
    (is (= (fnc-add '(3 4 5 6)) 18))
    (is (= (fnc-add '(A 4 5 6)) '(*error* number-expected A)))
    (is (= (fnc-add '(3 A 5 6)) '(*error* number-expected A)))
    (is (= (fnc-add '(3 4 A 6)) '(*error* number-expected A)))
    )
  )

(defn fnc-sub
  "Resta los elementos de un lista. Minimo 1 elemento."
  [l]
  (cond (>= (count l) 1)
        (let [err (revisar-lae (map fail-if-not-number l))]
          (if (= err nil) (apply - l) err))
        :else
        (list '*error* 'too-few-args)
        )
  )

(deftest fnc-sub-test
  (testing "fnc sub test."
    (is (= (fnc-sub ()) '(*error* too-few-args)))
    (is (= (fnc-sub '(3)) -3))
    (is (= (fnc-sub '(3 4)) -1))
    (is (= (fnc-sub '(3 4 5)) -6))
    (is (= (fnc-sub '(3 4 5 6)) -12))
    (is (= (fnc-sub '(A 4 5 6)) '(*error* number-expected A)))
    (is (= (fnc-sub '(3 A 5 6)) '(*error* number-expected A)))
    (is (= (fnc-sub '(3 4 A 6)) '(*error* number-expected A)))
    )
  )

(defn eval-condition
  "Eval a certain condition given some constraints to input list.
  Must be 2 elements in list, and both must be numbers"
  [l condition]
  (let [size (count l)]
    (cond
      (> size 2) (list '*error* 'too-many-args)
      (= size 2)
      (let [err (revisar-lae (map fail-if-not-number l))
            first (first l)
            second (second l)]
        (if (= err nil)
          (if (condition first second) 't nil)
          err))
      :else
      (list '*error* 'too-few-args)
      )))

(defn fnc-lt
  "Devuelve t si el primer numero es menor que el segundo; si no, nil."
  [l]
  (eval-condition l <)
  )

(deftest fnc-lt-test
  (testing "fnc lt test."
    (is (= (fnc-lt ()) '(*error* too-few-args)))
    (is (= (fnc-lt '(1)) '(*error* too-few-args)))
    (is (= (fnc-lt '(1 2)) 't))
    (is (= (fnc-lt '(1 1)) nil))
    (is (= (fnc-lt '(2 1)) nil))
    (is (= (fnc-lt '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-lt '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-lt '(1 2 3)) '(*error* too-many-args)))
    )
  )

(defn fnc-gt
  "Devuelve t si el primer numero es mayor que el segundo; si no, nil."
  [l]
  (eval-condition l >)
  )

(deftest fnc-gt-test
  (testing "fnc gt test."
    (is (= (fnc-gt ()) '(*error* too-few-args)))
    (is (= (fnc-gt '(1)) '(*error* too-few-args)))
    (is (= (fnc-gt '(2 1)) 't))
    (is (= (fnc-gt '(1 1)) nil))
    (is (= (fnc-gt '(1 2)) nil))
    (is (= (fnc-gt '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-gt '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-gt '(1 2 3)) '(*error* too-many-args)))
    )
  )

(defn fnc-ge
  "Devuelve t si el primer numero es mayor o igual que el segundo; si no, nil."
  [l]
  (eval-condition l >=)
  )

(deftest fnc-ge-test
  (testing "fnc ge test."
    (is (= (fnc-ge ()) '(*error* too-few-args)))
    (is (= (fnc-ge '(1)) '(*error* too-few-args)))
    (is (= (fnc-ge '(2 1)) 't))
    (is (= (fnc-ge '(1 1)) 't))
    (is (= (fnc-ge '(1 2)) nil))
    (is (= (fnc-ge '(A 1)) '(*error* number-expected A)))
    (is (= (fnc-ge '(1 A)) '(*error* number-expected A)))
    (is (= (fnc-ge '(1 2 3)) '(*error* too-many-args)))
    )
  )

; user=> (fnc-reverse ())
; (*error* too-few-args)
; user=> (fnc-reverse '(1))
; (*error* list expected 1)
; user=> (fnc-reverse '(A))
; (*error* list expected A)
; user=> (fnc-reverse '((1)) )
; (1)
; user=> (fnc-reverse '((1 2 3)) )
; (3 2 1)
; user=> (fnc-reverse '((1 2 3)(4)) )
; (*error* too-many-args)
(defn fnc-reverse
  "Devuelve una lista con sus elementos en orden inverso."
  [l]
  (let [ari (controlar-aridad l 1)]
    (cond (seq? ari) ari
          (coll? (first l)) (reverse (flatten l))
          :else (list '*error* 'list 'expected (first l)))
  ))

(deftest fnc-reverse-test
  (testing "fnc reverse test."
    (is (= (fnc-reverse ()) '(*error* too-few-args)))
    (is (= (fnc-reverse '(1)) '(*error* list expected 1)))
    (is (= (fnc-reverse '(A)) '(*error* list expected A)))
    (is (= (fnc-reverse '((1))) '(1)))
    (is (= (fnc-reverse '((1 2 3))) '(3 2 1)))
    (is (= (fnc-reverse '((1 2 3)(4))) '(*error* too-many-args)))
    )
  )

(defn search-symbol
  "Search for a symbol in a local ambient and in a global ambient."
  [symbol amb-global amb-local]
  (let [elm-amb-local (buscar symbol amb-global)
        elm-amb-global (buscar symbol amb-local)]
    (if (= (revisar-fnc elm-amb-global) nil)
      (list elm-amb-global  (actualizar-amb amb-global symbol elm-amb-local))
      (list elm-amb-local   (actualizar-amb amb-global symbol elm-amb-local))
      ))
  )

(defn evaluar-escalar
  "Evalua una expresion escalar consultando, si corresponde, los ambientes local y global. Devuelve una lista con el resultado y un ambiente."
  [esc amb-global amb-local]
  (cond (symbol? esc) (search-symbol esc amb-global amb-local)
        :else (list esc amb-global))
  )

(deftest evaluar-escalar-test
  (testing "evaluar escalar test."
    (is (= (evaluar-escalar 32 '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(32 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar "chau" '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("chau" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'Z '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '("hola" (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'w '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(3 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'x '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '(5 (v 1 w 3 x 6))))
    (is (= (evaluar-escalar 'n '(v 1 w 3 x 6) '(x 5 y 11 z "hola")) '((*error* unbound-symbol n) (v 1 w 3 x 6))))
    )
  )
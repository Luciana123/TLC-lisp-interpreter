# Introduction to tp-lisp-interpreter

TLC-LISP es un dialecto extinto de LISP. Fue lanzado en la década de 1980, fue utilizado en el campo de la inteligencia
artificial.

Actualmente, TLC-LISP ya no se comercializa más y, en consecuencia, para utilizar el software
existente desarrollado en él, se desea construir en este trabajo práctico un intérprete que corra en la
JVM (Java Virtual Machine). Por ello, el lenguaje elegido para su implementación es Clojure.

Deberá poder cargarse y correrse el siguiente Sistema de Producción, que resuelve el problema de
obtener 4 litros de líquido utilizando dos jarras lisas (sin escala), una de 5 litros y otra de 8 litros:

```
(load 'breadth)
(de jarra5 (x) (first x))
(de jarra8 (x) (first (rest x)))
(setq bc '(
(lambda (x) (if (lt (jarra5 x) 5) (list 5 (jarra8 x)) x))
(lambda (x) (if (gt (jarra5 x) 0) (list 0 (jarra8 x)) x))
(lambda (x) (if (ge (- 5 (jarra5 x)) (jarra8 x)) (list (+ (jarra5 x) (jarra8 x)) 0) x))
(lambda (x) (if (lt (- 5 (jarra5 x)) (jarra8 x)) (list 5 (- (jarra8 x) (- 5 (jarra5 x)))) x))
(lambda (x) (if (lt (jarra8 x) 8) (list (jarra5 x) 8) x))
(lambda (x) (if (gt (jarra8 x) 0) (list (jarra5 x) 0) x))
(lambda (x) (if (ge (- 8 (jarra8 x)) (jarra5 x)) (list 0 (+ (jarra8 x) (jarra5 x))) x))
(lambda (x) (if (lt (- 8 (jarra8 x)) (jarra5 x)) (list (- (jarra5 x) (- 8 (jarra8 x))) 8) x))))
'Carga-exitosa-de-jarras-lsp
```

```
(de breadth-first (bc)
(prin3 "Ingrese el estado inicial: ") (setq inicial (read))
(prin3 "Ingrese el estado final: ") (setq final (read))
(cond ((equal inicial final) (prin3 "El problema ya esta resuelto !!!") (terpri) (breadth-first bc))
(t (buscar bc final (list (list inicial)) nil))))
(de buscar (bc fin grafobusq estexp)
(cond ((null grafobusq) (fracaso))
((pertenece fin (first grafobusq)) (exito grafobusq))
(t (buscar bc fin (append (rest grafobusq) (expandir (first grafobusq) bc estexp))
(if (pertenece (first (first grafobusq)) estexp)
estexp
(cons (first (first grafobusq)) estexp))))))
(de expandir (linea basecon estexp)
(if (or (null basecon) (pertenece (first linea) estexp))
nil
(if (not (equal ((eval (first basecon)) (first linea)) (first linea)))
(cons (cons ((eval (first basecon)) (first linea)) linea) (expandir linea (rest basecon) estexp))
(expandir linea (rest basecon) estexp))))
(de pertenece (x lista)
(cond ((null lista) nil)
((equal x (first lista)) t)
(t (pertenece x (rest lista)))))
(de exito (grafobusq)
(prin3 "Exito !!!") (terpri)
(prin3 "Prof ....... ") (prin3 (- (length (first grafobusq)) 1)) (terpri)
(prin3 "Solucion ... ") (prin3 (reverse (first grafobusq))) (terpri) t)
(de fracaso ()
(prin3 "No existe solucion") (terpri) t)
'Carga-exitosa-de-breadth-lsp
```

A los efectos de desarrollar el intérprete solicitado, se deberá partir de los siguientes materiales
proporcionados por la cátedra en el aula virtual de la materia en el campus FIUBA:
+ Apunte de la cátedra: Interpretación de programas de computadora, donde se explican la
estructura y el funcionamiento de los distintos tipos de intérpretes posibles, en particular la
interpretación recursiva, que es la estrategia a utilizar en este trabajo práctico.
+ Apunte de la cátedra: Clojure, donde se resume el lenguaje a utilizar para el desarrollo.
+ Tutorial: Pasos para crear un proyecto en Clojure usando Leiningen, donde se indica cómo
desarrollar un proyecto dividido en código fuente y pruebas, y su despliegue como archivo jar.
+ Código fuente de un intérprete de TLC-LISP sin terminar, para completarlo. El mismo contiene
dos funciones que deben ser terminadas y 23 funciones que deben desarrollarse por completo.
+ Archivos jarras.lsp, breadth.lsp y demo.lsp para interpretar.
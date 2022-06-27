# TLC lisp interpreter

A Clojure library designed to act as an interpreter from TLC Lisp programming language.

## Usage

Run tests with leiningen
```
lein test
```

Run TLC-Lisp repl and load demo
```
lein run
....
>>> (load 'demo)
```

Run TLC-Lisp repl, load bc and run it
```
lein run
....
>>> (load 'jarras)
>>> (breadth-first bc)
```

Final and initial state must be completed as pair lists:
```
Ingrese el estado inicial: (0 0)
Ingrese el estado   final: (0 4)
```

After a little while the program should reach a solution.

To leave the interpreter use `(exit)`
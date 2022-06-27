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
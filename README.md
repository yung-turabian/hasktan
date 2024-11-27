# hasktan 🌿

A small, interpreted Haskell-derived toy language.

# How to Build & Run

Configure your enviroment with this script, or setup GHC along with Alex and Happy.
```bash
$ chmod +x configure.sh && ./configure.sh
```

Build with make and spits out the executable.
```bash
$ make
```

To interpret a file.
```bash
$ hasktan file.hs
```

Open REPL mode.
```bash
$ hasktan -i
```

# Employed the use of...
+ [Haskell](https://www.haskell.org/)
+ [Alex Manual](https://haskell-alex.readthedocs.io/en/latest/introduction.html)
+ [Happy Manual](https://haskell-happy.readthedocs.io/en/latest/using.html)

+ [Readline library](https://hackage.haskell.org/package/readline-1.0.3.0/docs/System-Console-Readline.html)

# License
[BSD-3 License](LICENSE)

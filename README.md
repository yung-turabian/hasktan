# hasktan 

# About

Hasktan is a small, interpreted Haskell-derived toy language. From Hebrew it would translate to 'small has\[kell\].' Currently there are very few keywords, those being **if**, **then**, **else**, **hd**, **tl**, (+, -, /, *, ^), **rem**, **quot**, ::, ++ and \\ .

# Usage

```bash
# Configure your enviroment with this script, or setup GHC along with Alex and Happy.
$ chmod +x configure.sh && ./configure.sh

# Build with make and spits out the executable.
$ make

# Or With Cabal
$ cabal build


# To interpret a file.
$ hasktan file.hs


# Open REPL mode.
$ hasktan -i
```

# External Libraries

+ [Haskell](https://www.haskell.org/)
+ [Alex Manual](https://haskell-alex.readthedocs.io/en/latest/introduction.html)
+ [Happy Manual](https://haskell-happy.readthedocs.io/en/latest/using.html)
+ [Readline library](https://hackage.haskell.org/package/readline-1.0.3.0/docs/System-Console-Readline.html)

# License

[BSD-3 License](LICENSE)

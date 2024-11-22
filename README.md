# hasktan ח

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

**Note**: Until I figure out monads and how IO works in Haskell, interpreting a file works as such
```bash
$ hasktan < file.hs
```

# Employed the use of...
+ [Haskell](https://www.haskell.org/)
+ [Alex Manual](https://haskell-alex.readthedocs.io/en/latest/introduction.html)
+ [Happy Manual](https://haskell-happy.readthedocs.io/en/latest/using.html)

# License
[BSD-3 License](LICENSE)

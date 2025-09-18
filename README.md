# Orwell

## About

Orwell is a small, interpreted Haskell-derived toy language.

## Reference

- [Orwell reference site](https://www.henrywandover.com/hasqtan)

## Building From Source

```bash
# Build with Cabal
$ cabal build # Or cabal install to add to ~/.local/bin

# To interpret a file.
$ orwl file.hs

# Open REPL mode.
$ orwli
```

## External Libraries

+ [Haskell](https://www.haskell.org/)
+ [Alex Manual](https://haskell-alex.readthedocs.io/en/latest/introduction.html)
+ [Happy Manual](https://haskell-happy.readthedocs.io/en/latest/using.html)
+ [Hasekline library](https://hackage.haskell.org/package/haskeline)
    + Haskeline does not rely on readline, therefore better for portability.

## License

[GPL-3 License](LICENSE)

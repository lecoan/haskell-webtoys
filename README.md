# Haskell Web Server

The project is a simple web server which is used for learning `Haskell` and `Monad`.

The source files are list below:


```text
.
├── app
│  └── Main.hs
├── src
│  ├── App.hs
│  ├── Handler.hs
│  └── Route.hs
└── package.yaml
```

## Used Monads

- Maybe Monad
- State Monad
- Reader Monad
- Except Monad

## Used Libraries

- wai
- warp
- mtl
- bytestring
- attoparsec
- http-types
- basic-prelude

## How to Run

1. Install `stack` (e.g., `pacman -S stack` for ArchLinux)
2. `stack setup`
3. `stack build`
4. `stack exec haskell-webtoys-exe `

## Resources

[How to understand Reader and State Monad](https://qr.ae/pGbMjk)

[Build yourself a Haskell web framework](https://cbaatz.github.io/build-a-haskell-web-framework/#1)


md2html
=======

md2html walks a directory of markdown files, running pandoc on each one to create a similar tree of html files. It is hugely more efficient when processing a large number of files. On 4000 files we saw an 8 minute run reduce to a 10 second run.

Installation
---

This is all very provisional and needs to be verified!

First you'll need to install the 2014 [Haskell Platform](http://www.haskell.org/platform/). You need at least ghc 7.8 and cabal 1.18.

Then, clone this repo and use cabal to install it in a sandbox. If prompted for other dependencies, install those too using `cabal install dependency_name`.

```
git clone github.com/gmp26/md2html.git
cd md2html
cabal sandbox init
cabal configure
cabal install
```

When this is successful, the executable will be built in `.cabal-sandbox/bin/md2html`. It's too large to distribute here since it embeds pandoc.

Test
---

```
cd md2html
mv html html.orig
.cabal-sandbox/bin/md2html
```

This will rebuild the `html` folder from the `md` folder.

-- Configuration for docidx; should be (will be) dynamic/from a config
-- file.

module Distribution.DocIdx.Config where

import Distribution.DocIdx.Common

data DocIdxCfg = DocIdxCfg {
    pageTitle :: String
  , pageCss :: [String]
  , favIcon :: String
  , tocExtras :: [TocItem]
  }

defaultConfig :: DocIdxCfg
defaultConfig =
  DocIdxCfg { pageTitle = "Local Haskell package docs"
            , pageCss = ["http://hackage.haskell.org/packages/hackage.css"]
            , favIcon = "http://hackage.haskell.org/images/Cabal-tiny.png"
            , tocExtras = TocSeparator : map (uncurry TocItem) [
                           ("hackage", "http://hackage.haskell.org/packages/archive/pkg-list.html"),
                           ("stdlibs", ghcDocs ++ "libraries/index.html"),
                           ("index", "file:///Users/gimbo/.cabal/share/doc/index.html"),
                           ("prelude", ghcDocs ++ "libraries/base-4.2.0.0/Prelude.html"),
                           ("ghc", ghcDocs ++ "users_guide/index.html"),
                           ("report", "file:///Users/gimbo/Documents/prog/haskell/haskell98-report-html/index.html"),
                           ("parsec", "file:///Users/gimbo/Documents/prog/haskell/parsec/parsec.html"),
                           ("haddock", "file:///Users/gimbo/Documents/prog/haskell/haddock/index.html"),
                           ("quickcheck", "file:///Users/gimbo/Documents/prog/haskell/quickcheck/manual_body.html"),
                           ("(for parsec)", "file:///Users/gimbo/Documents/prog/haskell/quickcheck/qc_for_parsec/Parsec%20Parser%20Testing%20with%20QuickCheck%20%C2%AB%20lstephen.html"),
                           ("gtk2hs", "file:///Users/gimbo/Documents/prog/haskell/gtk2hs-docs-0.10.0/index.html"),
                           ("cabal", ghcDocs ++ "Cabal/index.html"),
                           ("nums", "http://book.realworldhaskell.org/read/using-typeclasses.html#numerictypes.conversion")]
            }
    where ghcDocs = "file:///Library/Frameworks/GHC.framework/Versions/Current/usr/share/doc/ghc/html/"

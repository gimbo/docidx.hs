#!/usr/bin/env runhaskell

{-

A tidy rewrite of an ugly Haskell port of a "horribly hacked together"
Python script to generate an HTML index page for Haddock-generated
Haskell package docs. It also works quite nicely as a general index
for other docs. Note that the docs directory is hard-coded, below: see
the comments there.

Forked from: <http://github.com/andyprice/docidx.hs> which was ported
from the python at: <http://gimbo.org.uk/blog/2009/09/23/>

-}

import Data.Time
import System.Environment

import Distribution.DocIdx.Config
import Distribution.DocIdx.Html
import Distribution.GhcPkgList

main :: IO ()
main = do
  (gs,args) <- getArgs >>= extract
  pkgs <- installedPackages gs
  now <- getCurrentTime
  config <- getConfig
  let page = htmlPage config pkgs now
  if not (null args) then writeFile (head args) page else putStrLn page
  where
    extract = return . go ([],[])
    go (gs,as) ("-g":arg:ys)  = go (arg:gs,as) ys
    go (gs,as) (y:ys)         = go (gs,y:as) ys
    go (gs,as) []             = (gs,as)


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

import Data.Char (isAlpha, toUpper)
import Data.List
import Data.Ord
import Data.Time
import Data.Version
import qualified Data.Map as M
import System.Environment
import System.FilePath
import System.Locale
import Text.Html

import Distribution.GhcPkgList

-- Here's some stuff which should be in a config file.

pageTitle :: String
pageTitle = "Local Haskell package docs"

pageCss :: [String]
pageCss = ["http://hackage.haskell.org/packages/hackage.css"]

favIcon :: String
favIcon = "http://hackage.haskell.org/images/Cabal-tiny.png"

tocExtras :: [TocItem]
tocExtras = TocSeparator : map (uncurry TocItem) [
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
  where ghcDocs = "file:///Library/Frameworks/GHC.framework/Versions/Current/usr/share/doc/ghc/html/"

-- And now the work begins.

homePage :: String
homePage = "http://github.com/gimbo/docidx.hs"

main :: IO ()
main = do
  pkgs <- installedPackages
  now <- getCurrentTime
  let page = htmlPage pkgs now
  args <- getArgs
  if not (null args) then writeFile (head args) page else putStrLn page

-- Rendering page HTML.

-- | Create and render entire page.
htmlPage :: PackageMap [FilePath] -> UTCTime -> String
htmlPage pkgs now = renderHtml [htmlHeader, htmlBody]
  where htmlHeader = header << ((thetitle << pageTitle) : fav : css)
        fav = thelink ![rel "shortcut icon", href favIcon] << noHtml
        css = map oneCss pageCss
        oneCss cp = thelink ![rel "stylesheet",
                              thetype "text/css", href cp] << noHtml
        htmlBody = body << (title' ++ toc ++ secs ++ nowFoot)
          where title' = [h2 << "Local packages with docs"]
                toc = [htmlToc am]
                secs = concatMap (uncurry $ htmlPkgsAlpha) $ M.assocs am
                am = alphabetize pkgs
                now' = formatTime defaultTimeLocale rfc822DateFormat now
                nowFoot = [p ![theclass "toc"] $
                           stringToHtml ("Page rendered " ++ now' ++ " by ")
                           +++ (anchor ![href homePage] << stringToHtml "docidx")]

-- | An AlphaMap groups packages together by their name's first character.
type AlphaMap = M.Map Char (PackageMap [FilePath])

-- | Group packages together by their name's first character.
alphabetize :: PackageMap [FilePath] -> AlphaMap
alphabetize = foldr addAlpha M.empty
  where addAlpha (n, vs) = M.insertWith (++) c [(n, vs)]
          where c = if isAlpha c' then c' else '\0'
                c' = toUpper $ head n

-- | Elements of the table of contents.
data TocItem = TocItem String String
             | TocSeparator
             | TocNewline
               deriving (Eq, Ord, Show)

-- | Generate the table of contents.
htmlToc :: AlphaMap -> Html
htmlToc am = p ![theclass "toc"] << tocHtml (alphaItems ++ tocExtras)
  where tocHtml = intersperse bull . concatMap tocItemHtml
        alphaItems = map (\k -> TocItem [k] ('#':[k])) $ sort $ M.keys am

-- | Render toc elements to HTML.
tocItemHtml :: TocItem -> [Html]
tocItemHtml (TocItem nm path) = [anchor ![href path] << nm]
tocItemHtml TocSeparator = [mdash]
tocItemHtml TocNewline = [br] -- Hmmm... you still get the bullets?

-- | Render a collection of packages with the same first character.
htmlPkgsAlpha :: Char -> PackageMap [FilePath] -> [Html]
htmlPkgsAlpha c pm = [heading, packages]
  where heading = h3 ![theclass "category"] << anchor ![name [c]] << [c]
        packages = ulist ![theclass "packages"] <<
                     map (uncurry htmlPkg) pm'
        pm' = sortBy (comparing (map toUpper . fst)) pm

-- | Render a particularly-named package (all versions of it).
htmlPkg :: String -> VersionMap [FilePath] -> Html
htmlPkg nm vs = li << pvsHtml (flattenPkgVersions nm syn vs)
  where syn = Nothing

-- | Everything we want to know about a particular version of a
-- package, nicely flattened and ready to use.  (Actually, we'd also
-- like to use the synopsis, but this isn't exposed through the Cabal
-- library, sadly.  We could conceivably grab it from the haddock docs
-- (and hackage for packages with no local docs)  but this
-- seems excessive so for now we forget about it.
data PkgVersion = PkgVersion {
    pvName ::String
  , pvSynopsis :: Maybe String
  , pvVersion :: Version
  , pvExposed :: Bool
  , pvHaddocks :: Maybe FilePath
  } deriving (Eq, Ord, Show)

-- | Flatten a given package's various versions into a list of
-- PkgVersion values, which is much nicer to iterate over when
-- building the HTML for this package.
flattenPkgVersions :: String -> Maybe String -> VersionMap [FilePath] ->
                      [PkgVersion]
flattenPkgVersions nm syn vs = concatMap (uncurry flatten') $ reverse vs
  where flatten' :: Version -> [VersionInfo [FilePath]] -> [PkgVersion]
        -- We reverse here to put user versions of pkgs before
        -- identically versioned global versions.
        flatten' v = concatMap (uncurry flatten'') . reverse
          where flatten'' :: Bool -> [FilePath] -> [PkgVersion]
                flatten'' ex [] = [PkgVersion nm syn v ex Nothing]
                flatten'' ex ps = map (PkgVersion nm syn v ex . Just) ps

-- | Render the HTML for a list of versions of (we presume) the same
-- package.
pvsHtml :: [PkgVersion] -> Html
pvsHtml pvs = pvHeader (head pvs) +++ spaceHtml +++ pvVersions pvs +++
                pvSyn pvs

-- | Render the "header" part of some package's HTML: name (with link
-- to default version of local docs if available) and hackage link.
pvHeader :: PkgVersion -> [Html]
pvHeader pv = [maybeURL nme (pvHaddocks pv)
              ,spaceHtml
              ,anchor ![href $ hackagePath pv] << extLinkArrow
              ]
  where nme = if not (pvExposed pv) then "(" ++ nm ++ ")" else nm
        nm = pvName pv

-- | Render HTML linking to the various versions of a package
-- installed, listed by version number only (name is implicit).
pvVersions :: [PkgVersion] -> Html
pvVersions [_] = noHtml -- Don't bother if there's only one version.
pvVersions pvs = stringToHtml "[" +++
                  intersperse comma (map pvOneVer pvs) +++
                  stringToHtml "]"
  where pvOneVer pv = maybeURL (showVersion $ pvVersion pv) (pvHaddocks pv)

-- | Render the synopsis of a package, if present.
pvSyn :: [PkgVersion] -> Html
pvSyn (pv:_) = maybe noHtml (\x -> mdash +++ stringToHtml x) $ pvSynopsis pv
pvSyn _ = noHtml

-- | Render a URL if there's a path; otherwise, just render some text.
-- (Useful in cases where a package is installed but no documentation
-- was found: you'll still get the hackage link.)
maybeURL :: String -> Maybe String -> Html
maybeURL nm Nothing = stringToHtml nm
maybeURL nm (Just path) = anchor ![href $ joinPath [path, "index.html"]] << nm
                           
-- | Compute the URL to a package's page on hackage.
hackagePath :: PkgVersion -> String
hackagePath pv = "http://hackage.haskell.org/package/" ++ pvTag
  where pvTag = pvName pv ++ "-" ++ showVersion (pvVersion pv)

-- Some primitives.

bull, comma, extLinkArrow, mdash :: Html
bull = primHtml " &bull; "
comma = stringToHtml ", "
extLinkArrow = primHtml "&#x2b08;"
mdash = primHtml " &mdash; "

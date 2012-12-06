-- | Get contents of installed packages by querying "ghc-pkg" via
-- Cabal.

module Distribution.GhcPkgList (
  PackageMap,
  VersionMap,
  VersionInfo,
  HaddockInfo,
  installedPackages
) where

import Control.Arrow
import Data.List
import Data.List.Utils (addToAL)
import Data.Maybe (fromMaybe)
import qualified Distribution.InstalledPackageInfo as I
import qualified Distribution.Package as P
import Distribution.Simple.Compiler (PackageDB(GlobalPackageDB,
                                               UserPackageDB))
import Distribution.Simple.GHC (getInstalledPackages)
import Distribution.Simple.PackageIndex (PackageIndex, allPackagesByName)
import Distribution.Simple.Program (ghcProgram, ghcPkgProgram)
import Distribution.Simple.Program.Db (addKnownPrograms,
                                       configureAllKnownPrograms,
                                       emptyProgramDb)
import Distribution.Verbosity (normal)
import Distribution.Version (Version)
import System.FilePath
import Text.HTML.TagSoup

-- XXX Slightly nasty that we import this here, as otherwise this
-- module stands pleasantly independent of the rest of docidx.
import Distribution.DocIdx.Common

-- | A package map maps package names to information about the
-- versions installed.
type PackageMap a = [(String, VersionMap a)]

-- | A version map maps version numbers to information about the
-- various installations of that version.
type VersionMap a = [(Version, [VersionInfo a])]

-- | Information about a particular version of a package; at a minimum
-- this is whether it is exposed; other information may be attached
-- (in particular we will attach paths to Haddock docs and, later,
-- whether those docs exist and are readable, and their synopses).
type VersionInfo a = (Bool, [a])

-- | Cabal tells us about locations of Haddock docs, but they might
-- not actually exist or be readable.  If they do, we're interested in
-- their path and the package synopsis extracted from the title tag of
-- their index.html.
type HaddockInfo = Maybe (FilePath, String)

-- | Get exposure/haddock information about all versions of all
-- installed packages.
installedPackages :: IO (PackageMap HaddockInfo)
installedPackages = fmap groupPackages listInstalledPackages >>= checkHaddocks

-- Nothing from here down is exposed.

-- | Get the list of installed packages, via Cabal's existing
-- machinery.
listInstalledPackages :: IO PackageIndex
listInstalledPackages =
  let pdb = addKnownPrograms [ghcProgram,ghcPkgProgram] emptyProgramDb
  in configureAllKnownPrograms normal pdb >>=
         getInstalledPackages normal [GlobalPackageDB, UserPackageDB]

-- | Group installed package information together by package name and
-- version number.  At this stage all we know about the Haddock docs
-- are where Cabal says they are (not whether they exist), so
-- PackageMap is parameterised over such paths.
groupPackages :: PackageIndex -> PackageMap FilePath
groupPackages = foldr groupPackages' [] . map snd . allPackagesByName

groupPackages' :: [I.InstalledPackageInfo] -> PackageMap FilePath ->
                  PackageMap FilePath
groupPackages' ps pm = foldr groupPackages'' pm ps

groupPackages'' :: I.InstalledPackageInfo -> PackageMap FilePath ->
                   PackageMap FilePath
groupPackages'' ipi pm =
  addToAL pm nm $ addToVersionMap vs' ver (ex, had)
    where vs' = fromMaybe [] (nm `lookup` pm)
          pid = I.sourcePackageId ipi
          (P.PackageName nm) = P.pkgName pid
          ver = P.pkgVersion pid
          ex = I.exposed ipi
          had = I.haddockHTMLs ipi

addToVersionMap :: Eq a => VersionMap a -> Version -> VersionInfo a ->
                   VersionMap a
addToVersionMap vm v vi = addToAL vm v xs'
  where xs' = case v `lookup` vm of
                -- No duplicates please.
                Just xs -> if vi `elem` xs then xs else xs ++ [vi]
                Nothing -> [vi]

-- Checking existence of Haddock docs, and reading synopses from them.

-- | Given a PackageMap over paths to Haddock directories, turn it
-- into one over HaddockInfo values (checking if the Haddocks exist
-- and are readable, and if so, extracting the package synopsis from
-- each).
checkHaddocks :: PackageMap FilePath -> IO (PackageMap HaddockInfo)
checkHaddocks = pmMegaLift checkHaddock

-- Try to read the index.html file of some Haddock directory, and
-- extract the package synopsis.
checkHaddock :: FilePath -> IO HaddockInfo
checkHaddock hp = do
  result <- tryReadFile $ joinPath [hp, "index.html"]
  case result of
    Just x -> return $ Just (hp, parsePackageSynopsis x)
    Nothing -> return Nothing

-- | Parses a Haddock index.html to find the package's synopsis (in
-- the title tag).
parsePackageSynopsis :: String -> String
parsePackageSynopsis s = if null w then t else unwords $ tail w
  where w = words t
        t = findTitleTag $ canonicalizeTags $ parseTags s
        findTitleTag ts = maybe "" (fromTagText . snd) $ seekT ts
        seekT ts = find (isTagOpenName "title" . fst) (zip ts $ tail ts)

-- | Lift a function on the second element of a VersionInfo into a
-- function on a PackageMap.  Sorry this is so wild - it's just
-- digging deep into the (fairly repetitive) PackageMap structure; I
-- expect that if I understood, say, Control.Arrow, better, this could
-- be written more sensibly.
pmMegaLift :: (a -> IO b) -> PackageMap a -> IO (PackageMap b)
pmMegaLift = mapSndM . mapSndM . mapSndM . mapM
  where mapSndM = mapM . sndM
        -- | Weird monadic second-ish combinator.  Modified from answers on
        -- http://stackoverflow.com/questions/3998133/
        --   does-this-simple-haskell-function-already-have-a-well-known-name
        sndM :: (Functor m, Monad m) => (a -> m b) -> (c, a) -> m (c, b)
        sndM f = uncurry (fmap . (,)) . second f

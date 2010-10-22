-- | Get contents of installed packages by querying "ghc-pkg" via
-- Cabal.

module Distribution.GhcPkgList (
  PackageMap,
  VersionMap,
  VersionInfo,
  installedPackages
) where

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
type VersionInfo a = (Bool, a)



-- | Get exposure/haddock information about all versions of all
-- installed packages.
installedPackages :: IO (PackageMap [FilePath])
installedPackages = fmap groupPackages listInstalledPackages

-- Nothing from here down is exposed.

-- | Get the list of installed packages, via Cabal's existing
-- machinery.
listInstalledPackages :: IO PackageIndex
listInstalledPackages =
  let pdb = addKnownPrograms [ghcProgram,ghcPkgProgram] emptyProgramDb
  in configureAllKnownPrograms normal pdb >>=
         getInstalledPackages normal [GlobalPackageDB, UserPackageDB]

-- | Group installed package information together by package name and
-- version number.
groupPackages :: PackageIndex -> PackageMap [FilePath]
groupPackages = foldr groupPackages' [] . allPackagesByName

groupPackages' :: [I.InstalledPackageInfo] -> PackageMap [FilePath] ->
                  PackageMap [FilePath]
groupPackages' ps pm = foldr groupPackages'' pm ps

groupPackages'' :: I.InstalledPackageInfo -> PackageMap [FilePath] ->
                   PackageMap [FilePath]
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

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
type PackageMap = [(String, VersionMap)]

-- | A version map maps version numbers to information about the
-- various installations of that version.
type VersionMap = [(Version, [VersionInfo])]

-- | The information we're interested about a version: is it exposed,
-- and where are its Haddock docs installed?  (Not sure why there can
-- be multiple haddock paths, but that's what Cabal gives us, so
-- that's what we take.)
type VersionInfo = (Bool, [FilePath])

-- | Get exposure/haddock information about all versions of all
-- installed packages.
installedPackages :: IO PackageMap
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
groupPackages :: PackageIndex -> PackageMap
groupPackages = foldr groupPackages' [] . allPackagesByName

groupPackages' :: [I.InstalledPackageInfo] -> PackageMap -> PackageMap
groupPackages' ps pm = foldr groupPackages'' pm ps

groupPackages'' :: I.InstalledPackageInfo -> PackageMap -> PackageMap
groupPackages'' ipi pm =
  addToAL pm nm $ addToVersionMap vs' ver (ex, had)
    where vs' = fromMaybe [] (nm `lookup` pm)
          pid = I.sourcePackageId ipi
          (P.PackageName nm) = P.pkgName pid
          ver = P.pkgVersion pid
          ex = I.exposed ipi
          had = I.haddockHTMLs ipi

addToVersionMap :: VersionMap -> Version -> VersionInfo -> VersionMap
addToVersionMap vm v vi = addToAL vm v xs'
  where xs' = case v `lookup` vm of
                -- No duplicates please.
                Just xs -> if vi `elem` xs then xs else xs ++ [vi]
                Nothing -> [vi]

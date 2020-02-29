-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build.PackageInfoModule
-- Copyright   :
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the PackageInfo_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their package information.
module Distribution.Simple.Build.PackageInfoModule
  ( generatePackageInfoModule
  ) where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
  ( PackageName
  , packageName
  , packageVersion
  , unPackageName
  )
import Distribution.Types.PackageDescription (PackageDescription (..))
import Distribution.Types.Version (versionNumbers)
import Distribution.Utils.ShortText (fromShortText)

import qualified Distribution.Simple.Build.PackageInfoModule.Z as Z

-- ------------------------------------------------------------

-- * Building Paths_<pkg>.hs

-- ------------------------------------------------------------

generatePackageInfoModule :: PackageDescription -> BSL.ByteString
generatePackageInfoModule pkg_descr =
  BSB.toLazyByteString $
  Z.render
    Z.Z
      { Z.zPackageName = showPkgName $ packageName pkg_descr
      , Z.zVersionDigits = show $ versionNumbers $ packageVersion pkg_descr
      , Z.zSynopsis = fromShortText $ synopsis pkg_descr
      , Z.zCopyright = fromShortText $ copyright pkg_descr
      , Z.zHomepage = fromShortText $ homepage pkg_descr
      }

showPkgName :: PackageName -> String
showPkgName = map fixchar . unPackageName

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c

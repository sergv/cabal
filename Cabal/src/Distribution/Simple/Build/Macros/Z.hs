{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Simple.Build.Macros.Z (render, Z(..), ZPackage (..), ZTool (..)) where

import Control.Monad
import qualified Data.ByteString.Builder as BSB
import Distribution.ZinzaPrelude

data Z
    = Z {zPackages :: ([ZPackage]),
         zTools :: ([ZTool]),
         zPackageKey :: String,
         zComponentId :: String,
         zPackageVersion :: Version,
         zNotNull :: (String -> Bool),
         zManglePkgName :: (PackageName -> String),
         zMangleStr :: (String -> String)}

    deriving Generic

data ZPackage
    = ZPackage {zpkgName :: PackageName,
                zpkgVersion :: Version,
                zpkgX :: !Int,
                zpkgY :: !Int,
                zpkgZ :: !Int}

    deriving Generic

data ZTool
    = ZTool {ztoolName :: String,
             ztoolVersion :: Version,
             ztoolX :: !Int,
             ztoolY :: !Int,
             ztoolZ :: !Int}
    deriving Generic

render :: Z -> BSB.Builder
render z_root = execWriter $ do
  tellB "/* DO NOT EDIT: This file is automatically generated by Cabal */\n"
  tellC '\n'
  forM_ (zPackages z_root) $ \z_var0_pkg -> do
    tellB "/* package "
    tellS (prettyShow (zpkgName z_var0_pkg))
    tellC '-'
    tellS (prettyShow (zpkgVersion z_var0_pkg))
    tellB " */\n"
    tellB "#ifndef VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellC '\n'
    tellB "#define VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellB " \""
    tellS (prettyShow (zpkgVersion z_var0_pkg))
    tellB "\"\n"
    tellB "#endif /* VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellB " */\n"
    tellB "#ifndef MIN_VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellC '\n'
    tellB "#define MIN_VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellB "(major1,major2,minor) (\\\n"
    tellB "  (major1) <  "
    tellI (zpkgX z_var0_pkg)
    tellB " || \\\n"
    tellB "  (major1) == "
    tellI (zpkgX z_var0_pkg)
    tellB " && (major2) <  "
    tellI (zpkgY z_var0_pkg)
    tellB " || \\\n"
    tellB "  (major1) == "
    tellI (zpkgX z_var0_pkg)
    tellB " && (major2) == "
    tellI (zpkgY z_var0_pkg)
    tellB " && (minor) <= "
    tellI (zpkgZ z_var0_pkg)
    tellB ")\n"
    tellB "#endif /* MIN_VERSION_"
    tellS (zManglePkgName z_root (zpkgName z_var0_pkg))
    tellB " */\n"
  tellC '\n'
  forM_ (zTools z_root) $ \z_var1_tool -> do
    tellB "/* tool "
    tellS (ztoolName z_var1_tool)
    tellC '-'
    tellS (prettyShow (ztoolVersion z_var1_tool))
    tellB " */\n"
    tellB "#ifndef TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB "\n"
    tellB "#define TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB " \""
    tellS (prettyShow (ztoolVersion z_var1_tool))
    tellB "\"\n"
    tellB "#endif /* TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB " */\n"
    tellB "#ifndef MIN_TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB "\n"
    tellB "#define MIN_TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB "(major1,major2,minor) (\\\n"
    tellB "  (major1) <  "
    tellI (ztoolX z_var1_tool)
    tellB " || \\\n"
    tellB "  (major1) == "
    tellI (ztoolX z_var1_tool)
    tellB " && (major2) <  "
    tellI (ztoolY z_var1_tool)
    tellB " || \\\n"
    tellB "  (major1) == "
    tellI (ztoolX z_var1_tool)
    tellB " && (major2) == "
    tellI (ztoolY z_var1_tool)
    tellB " && (minor) <= "
    tellI (ztoolZ z_var1_tool)
    tellB ")\n"
    tellB "#endif /* MIN_TOOL_VERSION_"
    tellS (zMangleStr z_root (ztoolName z_var1_tool))
    tellB " */\n"
  tellC '\n'
  when (zNotNull z_root (zPackageKey z_root)) $ do
    tellB "#ifndef CURRENT_PACKAGE_KEY\n"
    tellB "#define CURRENT_PACKAGE_KEY \""
    tellS (zPackageKey z_root)
    tellB "\"\n"
    tellB "#endif /* CURRENT_packageKey */\n"
  when (zNotNull z_root (zComponentId z_root)) $ do
    tellB "#ifndef CURRENT_COMPONENT_ID\n"
    tellB "#define CURRENT_COMPONENT_ID \""
    tellS (zComponentId z_root)
    tellB "\"\n"
    tellB "#endif /* CURRENT_COMPONENT_ID */\n"
  tellB "#ifndef CURRENT_PACKAGE_VERSION\n"
  tellB "#define CURRENT_PACKAGE_VERSION \""
  tellS (prettyShow (zPackageVersion z_root))
  tellB "\"\n"
  tellB "#endif /* CURRENT_PACKAGE_VERSION */\n"

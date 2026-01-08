{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_avg (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "avg"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "\1053\1072\1084\1080\1088\1072 \1089\1088\1077\1076\1085\1086\1072\1088\1080\1090\1084\1077\1090\1080\1095\1085\1086 \1085\1072 \1074\1098\1074\1077\1076\1077\1085\1080 \1095\1080\1089\1083\1072"
copyright :: String
copyright = ""
homepage :: String
homepage = ""

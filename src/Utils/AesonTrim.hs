module Utils.AesonTrim where

import Data.Aeson.TH
import Control.Lens
import Data.Char    (toLower)

defaultOptionsWithTrim :: String -> Options
defaultOptionsWithTrim str = defaultOptions
  {fieldLabelModifier = over _head toLower . drop (length str)}

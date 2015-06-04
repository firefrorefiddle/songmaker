module SongMaker.Read.Sheet (isSheetLine) where

import SongMaker.Common
import Data.List

isSheetLine :: Line -> Bool
isSheetLine = ("|" `isPrefixOf`)

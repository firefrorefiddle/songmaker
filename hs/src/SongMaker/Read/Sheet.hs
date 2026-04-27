module SongMaker.Read.Sheet (isSheetLine) where

import Data.List
import SongMaker.Common

isSheetLine :: Line -> Bool
isSheetLine = ("|" `isPrefixOf`)

module DataConvert where

import ApiType
import Schema
import Data.Text

fromDBChord :: DbChord -> Chord
fromDBChord (DbChord name) = Chord 0 (unpack name) []

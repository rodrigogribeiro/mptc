
module Tests.Cases.IfaceReaderTest where

import Data.List
import qualified Data.Map as Map

import Language.Haskell.Exts hiding (name)

import Tc.Assumption
import Tc.Class
import Tc.Kc.KcEnv

import Iface.IfaceReader

import Utils.Id
import qualified Utils.Env as Env


testifacereader1 
    = parseInterface modulen >> return ()




modulen = "/home/rodrigo/Dropbox/projects/haskell/mptc/src/Tests/Data/Full/Teste1.hi"

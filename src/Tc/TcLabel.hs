
module Tc.TcLabel where

import Language.Haskell.Exts

import Utils.Id
import Utils.Env

-- this module only defines the
-- label type, that is used to
-- represent record labels.

type Label = (Id, Type)

-- an environment for labels

type LabelEnv = Env [Label]
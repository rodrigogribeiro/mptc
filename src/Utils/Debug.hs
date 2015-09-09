
module Utils.Debug (debug, trace) where

import Debug.Trace (trace)

-- simple function for non-monadic depuration

debug x = trace (show x) x
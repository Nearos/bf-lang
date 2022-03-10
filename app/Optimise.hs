module Optimise where

optimise :: String -> String
optimise str = case optimiseStep str of
    (res, True) -> optimise res
    (res, False) -> res 

optimiseStep :: String -> (String, Bool)

optimiseStep ('<':'>':xs) = let (t, _) = optimiseStep xs
                            in (t, True)

optimiseStep ('>':'<':xs) = let (t, _) = optimiseStep xs
                            in (t, True)

optimiseStep (x:xs) =   let (t, a) = optimiseStep xs
                        in (x:t, a)

optimiseStep [] = ("", False)
{-# LANGUAGE OverloadedStrings #-}

module GLM 
    where 

import Prelude

clamp :: Double -> Double -> Double -> Double
clamp x minval maxval = min (max x minval) maxval
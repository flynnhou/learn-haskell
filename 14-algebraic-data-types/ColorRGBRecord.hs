module ColorRGBRecord (Color) where

data Color = RGB
    { red :: Int
    , green :: Int
    , blue :: Int
    } deriving Show

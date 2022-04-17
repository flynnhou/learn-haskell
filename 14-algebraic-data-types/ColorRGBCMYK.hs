module ColorRGBCMYK (Color) where

data Color = RGB Int Int Int | CMYK Float Float Float Float deriving Show

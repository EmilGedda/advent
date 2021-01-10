{-# LANGUAGE QuasiQuotes #-}
module Advent.SVG where

import Data.ByteString          (ByteString)
import Data.Char                (toLower)
import Data.String.Interpolate  (__i)


data Color = Silver | Gold deriving (Enum, Show, Eq)

badge :: Color -> Int -> ByteString
badge star count =
    let
        offset | count > 9 = 3 :: Int
               | otherwise = 6
        color = map toLower (show star)
    in [__i|
        <svg xmlns="http://www.w3.org/2000/svg" width="140" height="20">
          <defs xmlns="http://www.w3.org/2000/svg">
            <linearGradient id="workflow-fill" x1="50%" y1="0%" x2="50%" y2="100%">
              <stop stop-color="#444D56" offset="0%"/>
              <stop stop-color="#24292E" offset="100%"/>
            </linearGradient>
            <linearGradient id="state-fill" x1="50%" y1="0%" x2="50%" y2="100%">
              <stop stop-color="#34D058" offset="0%"/>
              <stop stop-color="#28A745" offset="100%"/>
            </linearGradient>
            <linearGradient xmlns="http://www.w3.org/2000/svg" id="gold-star" x1="50%" y1="0%" x2="50%" y2="100%">
              <stop stop-color="#FFFF99" offset="0%"/>
              <stop stop-color="#7f8000" offset="100%"/>
            </linearGradient>
            <linearGradient xmlns="http://www.w3.org/2000/svg" id="silver-star" x1="50%" y1="0%" x2="50%" y2="100%">
              <stop stop-color="#B5B5B5" offset="0%"/>
              <stop stop-color="#595959" offset="100%"/>
            </linearGradient>
          </defs>
          <g fill="none" fill-rule="evenodd">
            <g font-family="'DejaVu Sans',Verdana,Geneva,sans-serif" font-size="11">
              <path id="workflow-bg" d="M0,3 C0,1.3431 1.3552,0 3.02702703,0 L112,0 112,20 L3.02702703,20 C1.3552,20 0,18.6569 0,17 L0,3 Z" fill="url(#workflow-fill)" fill-rule="nonzero"/>
              <text fill="#010101" fill-opacity=".3">
                <tspan x="22.1981982" y="15">Advent of Code</tspan>
              </text>
              <text fill="#FFFFFF">
                <tspan xmlns="http://www.w3.org/2000/svg" x="22.1981982" y="14">Advent of Code</tspan>
              </text>
            </g>
            <g transform="translate(112)" font-family="'DejaVu Sans',Verdana,Geneva,sans-serif" font-size="11">
              <path d="M0 0h25.939C27.282 0 28 1.343 28 3v14c0 1.657-1.37 3-3.061 3H0V0z" id="state-bg" fill="url(#state-fill)" fill-rule="nonzero"/>
              <text xmlns="http://www.w3.org/2000/svg" transform="translate(#{offset})" fill="#010101" fill-opacity=".3">
                <tspan xmlns="http://www.w3.org/2000/svg" y="15" x="4">#{count}</tspan>
              </text>
              <text xmlns="http://www.w3.org/2000/svg" fill="#FFFFFF" transform="translate(#{offset})">
                <tspan x="4" y="14">#{count}</tspan>
              </text>
            </g>
            <path fill="url(##{color}-star)" d="M 11.000 13.000 L 15.702 16.472 L 13.853 10.927 L 18.608 7.528 L 12.763 7.573 L 11.000 2.000 L 9.237 7.573 L 3.392 7.528 L 8.147 10.927 L 6.298 16.472 L 11.000 13.000 "/>
          </g>
        </svg>
    |]

module Representation where

-- | Polynomial as a list of coefficients, lowest power first
-- e.g. x^3-1 is represented as P [-1,0,0,1]
-- invariant: no trailing zeros
newtype DensePoly a = P { unP :: [a] } deriving Show

sampleDP = P [-1,0,0,1]

-- | Polynomial as a list of coefficients, highest power first
-- e.g. x^3-1 is represented as R [1,0,0,-1]
-- invariant: no leading zeros
newtype ReversePoly a = R { unR :: [a] } deriving Show

sampleRP = R [1,0,0,-1]

-- | Polynomial as a list of pairs (power, coefficient)
-- e.g. x^3-1 is represented as S [(3,1),(0,-1)]
-- invariant: in descending order of powers; no zero coefficients
newtype SparsePoly a = S { unS :: [(Int, a)] } deriving Show

sampleSP = S [(3,1),(0,-1)]

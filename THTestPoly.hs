{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Data.List(sortOn)
import DensePoly
import SparsePoly
import Representation
import PolyClass

type DPI = DensePoly Int
type SPI = SparsePoly Int

prop_AddCommDP :: DPI -> DPI -> Property
prop_AddCommDP p q = p + q === q + p

prop_AddZeroRDP :: DensePoly Int -> Property
prop_AddZeroRDP p = p + zeroP === p

prop_MulZeroRDP :: DensePoly Int -> Property
prop_MulZeroRDP p = p * zeroP === zeroP

prop_MulZeroLDP :: DensePoly Int -> Property
prop_MulZeroLDP p = zeroP * p === zeroP

prop_MulCommDP :: DPI -> DPI -> Property
prop_MulCommDP p q = p * q === q * p

prop_OneRDP :: DensePoly Int -> Property
prop_OneRDP p = -- label ("Degree of p is " ++ show(degree p)) $
  p * constP 1 === p

prop_DistLDP :: DPI -> DPI -> DPI -> Property
prop_DistLDP p q r = p*(q+r) === p*q + p*r

prop_ShiftLDP :: NonNegative(Small Int) -> DPI -> DPI -> Property
prop_ShiftLDP (NonNegative (Small n)) p q = shiftP n p * q === shiftP n (p*q)

prop_EvalPlus  :: Int ->  DPI -> DPI -> Property
prop_EvalPlus x p q = evalP(p + q) x === evalP p x + evalP q x

-- SPI

prop_AddCommSP :: SPI -> SPI -> Property
prop_AddCommSP p q = within 100000 $ p + q === q + p

prop_AddZeroRSP :: SPI -> Property
prop_AddZeroRSP p = p + zeroP === p

prop_MulZeroRSP :: SPI -> Property
prop_MulZeroRSP p = p * zeroP === zeroP

prop_MulZeroLSP :: SPI -> Property
prop_MulZeroLSP p = zeroP * p === zeroP

prop_OneRSP :: SPI -> Property
prop_OneRSP p = -- label ("Degree of p is " ++ show(degree p)) $
  p * constP 1 === p

-- within: prop fails if it does not complete within the given number of microseconds.
prop_MulCommSP :: SPI -> SPI -> Property
prop_MulCommSP p q = within 100000 $ p * q === q * p

prop_DistLSP :: SPI -> SPI -> SPI -> Property
prop_DistLSP p q r = within 100000 $ p*(q+r) === p*q + p*r

prop_ShiftLSP :: NonNegative(Small Int) -> SPI -> SPI -> Property
prop_ShiftLSP (NonNegative (Small n)) p q = shiftP n p * q === shiftP n (p*q)

-- conversions

prop_fromToDP :: SPI -> Bool
prop_fromToDP p = fromDP(toDP p) == p

prop_toFromDP :: DPI -> Bool
prop_toFromDP p = toDP(fromDP p) == p


type SPR = SparsePoly Rational

prop_qr1 :: SPR -> (NonZero SPR) -> Bool
prop_qr1 p (NonZero s) = p == q*s + r where (q,r) = qrP p s

prop_qr2 :: SPR -> (NonZero SPR) -> Bool
prop_qr2 p (NonZero s) = degree r < degree s where (q,r) = qrP p s

writeln :: String -> IO ()
writeln = putStrLn

-- Hic sunt leones

instance (Num a, Arbitrary a) => Arbitrary (DensePoly a) where
  arbitrary = P <$> arbitrary
  shrink = map P . shrink . unP

log2 :: Int -> Int
log2 0 = 0
log2 n = 1 + log2 (div n 2)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (SparsePoly a) where
  arbitrary = S . norm <$> sized g where
    norm = sortOn (negate . fst)
    g 0 = return []
    g n = do
      let p = log2 n `div` 2
      a <- frequency [(n-p, return 0), (p, arbitrary)]
      r <- g(n-1)
      return $ if a /= 0 then (n,a):r else r
  shrink (S ps) = map S $ s ps where
    s [] = []
    -- s ((a,n):ps) = ps:[(a',n'):ps' | a' <- shrink a, n' <- shrink n, S ps' <- shrink (S ps)]
    s ((a,n):ps) = ps:[(a,n):ps' | S ps' <- shrink (S ps)]



return []
runTests = $quickCheckAll

main = runTests
{-
main = do
  writeln "prop_AddCommDP: p+q = q+p"
  quickCheck prop_AddCommDP
  writeln "\nprop_AddZeroRDP: p+0 = p"
  quickCheck prop_AddZeroRDP

  writeln "\n\nprop_MulZeroRDP: p*0 = 0"
  quickCheck prop_MulZeroRDP
  writeln "\nprop_MulCommDP: p*q =q*p"
  quickCheck prop_MulCommDP
  writeln "\nprop_OneRDP"
  quickCheck prop_OneRDP
  writeln "\nprop_DistL"
  quickCheck prop_DistL
  writeln "\nprop_ShiftL"
  quickCheck prop_ShiftL
-}

{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.HUnit
import Data.List(sortOn)
import Data.List(sort)
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

prop_AddZeroLDP :: DensePoly Int -> Property
prop_AddZeroLDP p = zeroP + p === p

prop_MulZeroRDP :: DensePoly Int -> Property
prop_MulZeroRDP p = p * zeroP === zeroP

prop_MulZeroLDP :: DensePoly Int -> Property
prop_MulZeroLDP p = zeroP * p === zeroP

prop_MulCommDP :: DPI -> DPI -> Property
prop_MulCommDP p q = p * q === q * p

prop_NegNegDP :: DPI -> Property
prop_NegNegDP p = -(-p) === p

prop_AddNegDP :: DPI -> Property
prop_AddNegDP p = p - p === zeroP

prop_OneRDP :: DensePoly Int -> Property
prop_OneRDP p = p * constP 1 === p

prop_OneLDP :: DensePoly Int -> Property
prop_OneLDP p = (constP 1) * p === p

prop_DistLDP :: DPI -> DPI -> DPI -> Property
prop_DistLDP p q r = p * (q + r) === p * q + p * r

prop_ShiftLDP :: NonNegative(Small Int) -> DPI -> DPI -> Property
prop_ShiftLDP (NonNegative (Small n)) p q = shiftP n p * q === shiftP n (p*q)

prop_EqDP :: DPI -> DPI -> Property
prop_EqDP p q = (p == q) === (q == p)

is_canonic_DP :: DPI -> Property
is_canonic_DP (P []) = True ==> True
is_canonic_DP (P l) = True ==> last l /= 0

prop_add_canonic_DP p q = is_canonic_DP (p + q)
prop_mul_canonic_DP p q = is_canonic_DP (p * q)
prop_sub_canonic_DP p q = is_canonic_DP (p - q)
prop_neg_canonic_DP p = is_canonic_DP (-p)
prop_const_canonic_DP c = is_canonic_DP (constP c)
prop_shift_canonic_DP n p = is_canonic_DP (shiftP n p)

handmade_DP_evalP_0 = TestCase (assertEqual "dp_eval_0" 1 (evalP (P [1, 2, 3]) 0))
handmade_DP_evalP_1 = TestCase (assertEqual "dp_eval_1" (-2) (evalP (P [-2, 2, 3]) 0))
handmade_DP_evalP_2 = TestCase (assertEqual "dp_eval_2" 6 (evalP (P [1, 2, 3]) 1))
handmade_DP_evalP_3 = TestCase (assertEqual "dp_eval_3" (1 - 4 + 12) (evalP (P [1, 2, 3]) (-2)))
handmade_DP_add_0 = TestCase (assertEqual "dp_add_0" (P [-2, 0, 0, 2]) (sampleDP + sampleDP))
handmade_DP_add_1 = TestCase (assertEqual "dp_add_1" (P [0, 0, 0, 1]) (sampleDP + (P [1])))
handmade_DP_add_2 = TestCase (assertEqual "dp_add_2" (P [-1]) (sampleDP + (P [0, 0, 0, -1])))
handmade_DP_mul_0 = TestCase (assertEqual "dp_mul_0" (P [1, 0, 0, -2, 0, 0, 1]) (sampleDP * sampleDP))
handmade_DP_mul_1 = TestCase (assertEqual "dp_mul_1" (P [1, 0, 0, -1]) (sampleDP * (P [-1])))
handmade_DP_mul_2 = TestCase (assertEqual "dp_mul_2" (P [0, 0, 0, 1, 0, 0, -1]) (sampleDP * (P [0, 0, 0, -1])))
handmade_DP_neg_0 = TestCase (assertEqual "dp_neg_0" (P [1, 0, 0, -1]) (-sampleDP))
handmade_DP_shift_0 = TestCase (assertEqual "dp_shift_0" (P [-1, 0, 0, 1]) (shiftP 0 sampleDP))
handmade_DP_shift_1 = TestCase (assertEqual "dp_shift_1" (P [0, -1, 0, 0, 1]) (shiftP 1 sampleDP))
handmade_DP_shift_2 = TestCase (assertEqual "dp_shift_2" (P [0, 0, -1, 0, 0, 1]) (shiftP 2 sampleDP))

prop_EvalPlus  :: Int ->  DPI -> DPI -> Property
prop_EvalPlus x p q = evalP(p + q) x === evalP p x + evalP q x

prop_Fmap_id_DP :: DPI -> Property
prop_Fmap_id_DP p = p === (fmap id p)

-- SPI

prop_AddCommSP :: SPI -> SPI -> Property
prop_AddCommSP p q = within 100000 $ p + q === q + p

prop_AddZeroRSP :: SPI -> Property
prop_AddZeroRSP p = p + zeroP === p

prop_AddZeroLSP :: SPI -> Property
prop_AddZeroLSP p = zeroP + p === p

prop_MulZeroRSP :: SPI -> Property
prop_MulZeroRSP p = p * zeroP === zeroP

prop_MulZeroLSP :: SPI -> Property
prop_MulZeroLSP p = zeroP * p === zeroP

prop_NegNegSP :: SPI -> Property
prop_NegNegSP p = -(-p) === p

prop_OneRSP :: SPI -> Property
prop_OneRSP p = p * constP 1 === p

prop_OneLSP :: SPI -> Property
prop_OneLSP p = (constP 1) * p === p

-- within: prop fails if it does not complete within the given number of microseconds.
prop_MulCommSP :: SPI -> SPI -> Property
prop_MulCommSP p q = within 100000 $ p * q === q * p

prop_DistLSP :: SPI -> SPI -> SPI -> Property
prop_DistLSP p q r = within 100000 $ p*(q+r) === p*q + p*r

prop_ShiftLSP :: NonNegative(Small Int) -> SPI -> SPI -> Property
prop_ShiftLSP (NonNegative (Small n)) p q = shiftP n p * q === shiftP n (p*q)

prop_EqSP :: SPI -> SPI -> Property
prop_EqSP p q = (p == q) === (q == p)

is_canonic_SP :: SPI -> Property
is_canonic_SP (S []) = True ==> True
is_canonic_SP (S l) =
  let mapped = map fst l in
  let sorted = Data.List.sort mapped in
  True ==> (mapped == reverse sorted && not (or [x == y | (x, y) <- zip mapped (tail mapped)]) && snd (head l) /= 0)

prop_add_canonic_SP p q = is_canonic_SP (p + q)
prop_mul_canonic_SP p q = is_canonic_SP (p * q)
prop_sub_canonic_SP p q = is_canonic_SP (p - q)
prop_neg_canonic_SP p = is_canonic_SP (-p)
prop_const_canonic_SP c = is_canonic_SP (constP c)
prop_shift_canonic_SP n p = is_canonic_SP (shiftP n p)

prop_Fmap_id_SP :: SPI -> Property
prop_Fmap_id_SP p = p === (fmap id p)

-- conversions

prop_fromToDP :: SPI -> Bool
prop_fromToDP p = fromDP(toDP p) == p

prop_toFromDP :: DPI -> Bool
prop_toFromDP p = toDP(fromDP p) == p


prop_add_DP_SP :: DPI -> DPI -> Property
prop_add_DP_SP p q = p + q === toDP ((fromDP p) + (fromDP q))

prop_mul_DP_SP :: DPI -> DPI -> Property
prop_mul_DP_SP p q = p * q === toDP ((fromDP p) * (fromDP q))

prop_sub_DP_SP :: DPI -> DPI -> Property
prop_sub_DP_SP p q = p - q === toDP ((fromDP p) - (fromDP q))

prop_neg_DP_SP :: DPI -> Property
prop_neg_DP_SP p = -p === toDP (-(fromDP p))

prop_const_DP_SP :: Int -> Property
prop_const_DP_SP c = constP c === toDP (constP c)

-- prop_shift_DP_SP :: Int -> DPI -> Property
-- prop_shift_DP_SP n p = shiftP n p === toDP (shiftP n (fromDP p)) -- TODO: mail do prowadzacego
prop_shift_DP_SP :: (NonNegative Int) -> DPI -> Property
prop_shift_DP_SP (NonNegative n) p = shiftP n p === toDP (shiftP n (fromDP p))


type SPR = SparsePoly Rational

prop_qr1 :: SPR -> (NonZero SPR) -> Bool
prop_qr1 p (NonZero s) = p == q*s + r where (q,r) = qrP p s

prop_qr2 :: SPR -> (NonZero SPR) -> Bool
prop_qr2 p (NonZero s) = degree r < degree s where (q,r) = qrP p s

prop_qr_whole :: SPR -> (NonZero SPR) -> Bool
prop_qr_whole p (NonZero s) = r == 0 where (q, r) = qrP (p * s) s

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

handmade_tests = TestList [
  handmade_DP_evalP_0, handmade_DP_evalP_1, handmade_DP_evalP_2, handmade_DP_evalP_3, 
  handmade_DP_add_0, handmade_DP_add_1, handmade_DP_add_2,
  handmade_DP_mul_0, handmade_DP_mul_1, handmade_DP_mul_2,
  handmade_DP_neg_0,
  handmade_DP_shift_0,
  handmade_DP_shift_1,
  handmade_DP_shift_2]

return []
runTests = do
  $forAllProperties (quickCheckResult . withMaxSuccess 1000)
  runTestTT handmade_tests

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

module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP (P tab) = S (from_dp_helper 0 tab) where
    from_dp_helper k [] = []
    from_dp_helper k (0:tl) = from_dp_helper (k+1) tl
    from_dp_helper k (hd:tl) = ((k, hd):(from_dp_helper (k+1) tl))

toDP (S tab) = P (to_dp_helper 0 tab) where
    to_dp_helper k [] = []
    to_dp_helper k ((m, wsp):tl) = if (k == m) then 
            wsp:(to_dp_helper (k+1) tl)
        else 
            0:(to_dp_helper (k+1) ((m, wsp):tl))

first :: (a -> a') -> (a, b) -> (a', b)
first = undefined
second :: (b -> b') -> (a, b) -> (a, b')
second = undefined

instance Functor SparsePoly where
    fmap fun (S []) = (S [])
    fmap fun (S ((k, wsp):tl)) = S ((k, (fun wsp)):(unS (fmap fun (S tl))))

instance Polynomial SparsePoly where
    zeroP = (S []) 
    constP c = (S [(0, c)])
    varP = (S [(1, 0)])
    x = (S [(1, 0)])
    evalP (S []) x = 0
    evalP (S ((k, wsp):tl)) x = (x ^ k) * wsp + (evalP (S tl) x)
    shiftP (S []) n = (S [])
    shiftP (S ((k, wsp):tl)) n = (S (((k + n), wsp):(unP (shiftP (S tl) n))))
    degree (S []) = -1
    degree (S [(k, _)] = k
    degree (S ((_, _):tl)) = degree (S tl)

instance (Eq a, Num a) => Num (SparsePoly a) where

instance (Eq a, Num a) => Eq (SparsePoly a) where
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP = undefined

-- | Division example
-- >>> let x = varP in qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True

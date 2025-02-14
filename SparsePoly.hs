module SparsePoly(fromDP, toDP, qrP) where
import PolyClass
import Representation

-- | fromDP example
-- >>> fromDP sampleDP
-- S {unS = [(3,1),(0,-1)]}
fromDP :: (Eq a, Num a) => DensePoly a -> SparsePoly a
toDP :: (Eq a, Num a) => SparsePoly a -> DensePoly a

fromDP (P tab) = S (reverse (from_dp_helper 0 tab)) where
    from_dp_helper k [] = []
    from_dp_helper k (0:tl) = from_dp_helper (k+1) tl
    from_dp_helper k (hd:tl) = ((k, hd):(from_dp_helper (k+1) tl))

toDP (S tab) = P (to_dp_helper 0 (reverse tab)) where
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
    constP 0 = (S [])
    constP c = (S [(0, c)])
    varP = (S [(1, 1)])
    x = (S [(1, 1)])
    evalP (S []) x = 0
    evalP (S ((k, wsp):tl)) x = ((x ^ k) * wsp) + (evalP (S tl) x)
    shiftP n (S s) = S (shift_array n s)
    degree (S []) = -1
    degree (S ((k, _):tl)) = k
    nullP (S []) = True
    nullP _ = False
    
shift_array :: (Num a) => Int -> [(Int, a)] -> [(Int, a)]
shift_array n tab = map (\(k, wsp) -> ((k+n), wsp)) tab


reduce_array :: (Num a, Eq a) => [(Int, a)] -> [(Int, a)] 
reduce_array tab = filter (\(exp, wsp) -> (wsp /= 0)) tab


instance (Eq a, Num a) => Num (SparsePoly a) where
    (+) (S tab1) (S tab2) = S (reduce_array (add_arrays tab1 tab2))          
    (*) (S tab1) (S tab2) = S (reduce_array (multiply_arrays tab1 tab2))
    negate (S tab) = (S (reduce_array (negate_array tab)))
    fromInteger 0 = (S [])
    fromInteger k = (S [(0, (fromInteger k))])
    abs = undefined
    signum = undefined

negate_array :: (Num a) => [(Int, a)] -> [(Int, a)]
negate_array tab = map (\(k, wsp) -> (k, -wsp)) tab 
    
add_arrays :: (Num a, Eq a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
add_arrays tab [] = tab
add_arrays [] tab = tab
add_arrays ((k1, wsp1):tl1) ((k2, wsp2):tl2) = 
        if k1 == k2 then 
            ((k1, (wsp1 + wsp2)):(add_arrays tl1 tl2))
        else 
            if k1 > k2 then 
                ((k1, wsp1):(add_arrays tl1 ((k2, wsp2):tl2)))
            else 
                ((k2, wsp2):(add_arrays tl2 ((k1, wsp1):tl1)))
    
    
multiply_arrays :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
multiply_arrays [] _ = []
multiply_arrays _ [] = []
multiply_arrays ((exp, wsp):tl) tab = add_arrays (((shift_array exp).(multiply_by wsp)) tab) (multiply_arrays tl tab)
    where 
        multiply_by a tab = map (\(exp, wsp) -> (exp, (wsp * a))) tab


create_poly_tab :: Int -> a -> [(Int, a)]
create_poly_tab exp wsp = [(exp, wsp)]          
            
instance (Eq a, Num a) => Eq (SparsePoly a) where
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP (S divident) (S divisor) = (S (reduce_array (fst res)), S (reduce_array (snd res))) where
    res = (divide_arrays divident divisor)

divide_arrays :: (Eq a, Fractional a, Num a) => [(Int, a)] -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
divide_arrays [] tab = ([], [])
divide_arrays tab [] = undefined
divide_arrays ((ahdexp, ahdwsp):atl) ((bhdexp, bhdwsp):btl) = 
    if ahdexp < bhdexp then 
        ([], ((ahdexp, ahdwsp):atl))
    else 
        (res, remainder) where
            res = add_arrays f (create_poly_tab h c)
            (f, remainder) = divide_arrays e ((bhdexp, bhdwsp):btl)
            e = add_arrays atl d
            d = map (\(exp, wsp) -> ((exp + h), (wsp * (negate c)))) btl -- btl * (-c) * x^h
            c = (ahdwsp / bhdwsp)
            h = ahdexp - bhdexp


-- | Division example
-- >>> let x = varP in qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True

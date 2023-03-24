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
    shiftP n (S s) = S (shift_array n s)
    degree (S []) = -1
    degree (S [(k, _)]) = k
    degree (S ((_, _):tl)) = degree (S tl)
    nullP (S []) = True
    nullP _ = False
    
shift_array :: Int -> [(Int, a)] -> [(Int, a)]
shift_array n [] = []
shift_array n ((k, wsp):tl) = ((k + n), wsp):(shift_array n tl)

reduce_array :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] 
reduce_array [] = []
reduce_array ((exp, wsp):tl) = 
    if wsp == 0 then 
        (reduce_array tl)
    else 
        ((exp, wsp):(reduce_array tl)) 

instance (Eq a, Num a) => Num (SparsePoly a) where
    (+) (S tab1) (S tab2) = S (reduce_array (add_arrays tab1 tab2))          
    (*) (S tab1) (S tab2) = S (reduce_array (multiply_arrays tab1 tab2))
    negate (S []) = (S []) 
    negate (S ((k, wsp):tl)) = S ((k, -wsp):(unS (negate (S tl))))
    fromInteger 0 = (S [])
    fromInteger k = (S [(1, (fromInteger k))])
    abs = undefined
    signum = undefined
    
    
add_arrays :: (Num a, Eq a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
add_arrays tab [] = tab
add_arrays [] tab = tab
add_arrays ((k1, wsp1):tl1) ((k2, wsp2):tl2) = 
        if k1 == k2 then 
            ((k1, (wsp1 + wsp2)):(add_arrays tl1 tl2))
        else 
            if k1 < k2 then 
                ((k1, wsp1):(add_arrays tl1 ((k2, wsp2):tl2)))
            else 
                ((k2, wsp2):(add_arrays tl2 ((k1, wsp1):tl1)))
    
    
multiply_arrays :: (Eq a, Num a) => [(Int, a)] -> [(Int, a)] -> [(Int, a)]
multiply_arrays [] _ = []
multiply_arrays _ [] = []
multiply_arrays ((k, wsp):tl) tab = add_arrays (((shift_array k).(multiply_by wsp)) tab) (multiply_arrays tl tab)
    where 
        shift_array n [] = []
        shift_array n ((k, wsp):tl) = (((k + n), wsp):(shift_array n tl))
        multiply_by a [] = []
        multiply_by a ((k, wsp):tl) = ((k, (wsp * a)):(multiply_by a tl))


create_poly_tab :: Int -> a -> [(Int, a)]
create_poly_tab exp wsp = [(exp, wsp)]          
            
instance (Eq a, Num a) => Eq (SparsePoly a) where
    p == q = nullP(p-q)

-- qrP s t | not(nullP t) = (q, r) iff s == q*t + r && degree r < degree t
qrP :: (Eq a, Fractional a) => SparsePoly a -> SparsePoly a -> (SparsePoly a, SparsePoly a)
qrP (S divident) (S divisor) = (S (reduce_array (fst res)), S (reduce_array (snd res))) where
    res = (divide_arrays (reverse divident) (reverse divisor))

divide_arrays :: (Eq a, Fractional a, Num a) => [(Int, a)] -> [(Int, a)] -> ([(Int, a)], [(Int, a)])
divide_arrays [] tab = ([], [])
divide_arrays tab [] = ([], [])
divide_arrays ((ahdexp, ahdwsp):atl) ((bhdexp, bhdwsp):btl) = 
    if ahdexp < bhdexp then 
        ([], ((bhdexp, bhdwsp):btl))
    else 
        (res, g) where
            res = add_arrays f (create_poly_tab (ahdexp - bhdexp) c)
            (f, g) = divide_arrays atl e
            e = add_arrays btl d
            d = map (\(exp, wsp) -> (exp, (wsp * (negate c)))) btl
            c = (ahdwsp / bhdwsp)


-- | Division example
-- >>> let x = varP in qrP (x^2 - 1) (x -1) == ((x + 1), 0)
-- True

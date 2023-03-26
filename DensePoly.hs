module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where
    fmap fun (P []) = (P [])
    fmap fun (P (h:t)) = (P ((fun h):(unP (fmap fun (P t)))))

instance Polynomial DensePoly where
    zeroP = (P [])
    constP 0 = (P [])
    constP c = (P [c])
    varP = (P [0, 1])
    x = (P [0, 1])
    evalP (P tab) x = eval_array_taily x 0 1 tab
    shiftP n (P tab) = (P (reduce_array (shift_array n tab)))
    degree (P tab) = (length (reduce_array tab)) - 1
    nullP (P []) = True
    nullP _ = False
    
    
eval_array_taily :: (Num a) => a -> a -> a -> [a] -> a
eval_array_taily x acc mul [] = acc
eval_array_taily x acc mul (hd:tl) = eval_array_taily x (hd*mul + acc) (mul*x) tl

shift_array :: (Num a) => Int -> [a] -> [a]
shift_array n [] = []
shift_array 0 tab = tab
shift_array n (hd:tl) = 
    if n > 0 then 
        shift_array (n-1) (0:(hd:tl))
    else 
        shift_array (n+1) tl

reduce_array :: (Num a, Eq a) => [a] -> [a]  -- deletes leading zeros
reduce_array [] = []
reduce_array (0:tl) = 
    if (only_zeros tl) then []
    else (0:(reduce_array tl))
reduce_array (hd:tl) = 
    if (only_zeros tl) then [hd]
    else (hd:(reduce_array tl))
    
only_zeros :: (Num a, Eq a) => [a] -> Bool
only_zeros [] = True
only_zeros (0:tl) = only_zeros tl
only_zeros (hd:tl) = False

  

instance (Eq a, Num a) => Num (DensePoly a) where
    (+) (P tab1) (P tab2) = P (reduce_array (add_arrays tab1 tab2))
    (*) (P tab1) (P tab2) = P (reduce_array (multiply_arrays tab1 tab2))
    negate (P tab) = P (reduce_array (negate_array tab))
    abs = undefined
    signum p = undefined 
    fromInteger 0 = (P [])
    fromInteger k = (P [(fromInteger k)])
   
negate_array :: (Num a) => [a] -> [a] 
negate_array tab = map (\x -> (-x)) tab
    
    
add_arrays :: (Num a) => [a] -> [a] -> [a]
add_arrays [] p = p
add_arrays p [] = p
add_arrays (h1:t1) (h2:t2) = (h1 + h2):(add_arrays t1 t2)

multiply_arrays :: (Num a) => [a] -> [a] -> [a]
multiply_arrays _ [] = []
multiply_arrays [] _ = []
multiply_arrays (h1:t1) (h2:t2) = (add_arrays (add_arrays ([h1 * h2]) (multiply_helper 1 h1 t2)) (add_arrays (multiply_helper 1 h2 t1) ((shift_array 2) (multiply_arrays t1 t2))))
    
multiply_helper :: (Num a) => Int -> a -> [a] -> [a]
-- mnoży każdy element tab przez multiplier i przesuwa w prawo o shifter
multiply_helper shifter multiplier = (multiply multiplier).(shift_array shifter)
    where 
        multiply c tab = map (\x -> (c * x)) tab

     

-- |
-- >>> let x = varP :: DensePoly Integer in x^3 - 1
-- P {unP = [-1,0,0,1]}
instance (Eq a, Num a) => Eq (DensePoly a) where
    (==) (P tab1) (P tab2) = nullP (P (reduce_array (add_arrays tab1 (negate_array tab2))))

-- |
-- >>>  P [1,2] == P [1,2]
-- True

-- |
-- >>> fromInteger 0 == (zeroP :: DensePoly Int)
-- True

-- |
-- >>>  P [0,1] == P [1,0]
-- False

-- | Degree examples
-- >>> degree (zeroP :: DensePoly Int)
-- -1
-- >>> degree (constP 1 :: DensePoly Int)
-- 0

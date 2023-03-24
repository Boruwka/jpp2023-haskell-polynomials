module DensePoly() where
import PolyClass
import Representation

instance Functor DensePoly where
    fmap fun (P []) = (P [])
    fmap fun (P (h:t)) = (P ((fun h):(unP (fmap fun (P t)))))

instance Polynomial DensePoly where
    zeroP = (P [])
    constP c = (P [c])
    varP = (P [0, 1])
    x = (P [0, 1])
    evalP (P []) x = 0
    evalP (P (hd:tl)) x = hd + x * (evalP (P tl) x) -- warto by przerobić na ogonowe
    shiftP n (P tab) = P (shift_array n tab)
    degree (P []) = -1
    degree (P (hd:tl)) = 1 + (degree (P tl))
    nullP (P []) = True
    nullP _ = False

shift_array :: (Num a) => Int -> [a] -> [a]
shift_array 0 tab = tab
shift_array n tab = shift_array (n-1) (0:tab)

  

instance (Eq a, Num a) => Num (DensePoly a) where
    (+) (P tab1) (P tab2) = P (add_arrays tab1 tab2)
    (*) (P tab1) (P tab2) = P (multiply_arrays tab1 tab2)
    negate (P []) = (P [])
    negate (P (h:t)) = P ((-h):(unP (negate (P t))))
    abs = undefined
    signum p = undefined 
    fromInteger 0 = (P [])
    fromInteger k = (P [(fromInteger k)])
    
    
add_arrays :: (Num a) => [a] -> [a] -> [a]
add_arrays [] p = p
add_arrays p [] = p
add_arrays (h1:t1) (h2:t2) = (h1 + h2):(add_arrays t1 t2)

multiply_arrays :: (Num a) => [a] -> [a] -> [a]
multiply_arrays _ [] = []
multiply_arrays [] _ = []
multiply_arrays (h1:t1) (h2:t2) = (add_arrays (add_arrays ([h1 + h2]) (multiply_helper 1 h1 t2)) (add_arrays (multiply_helper 1 h2 t1) ((shift 2) (multiply_arrays t1 t2))))
        where 
            shift 0 tab = tab
            shift shifter tab = shift (shifter - 1) (0:tab)
    
multiply_helper :: (Num a) => Int -> a -> [a] -> [a]
-- mnoży każdy element tab przez multiplier i przesuwa w prawo o shifter
multiply_helper shifter multiplier = (multiply multiplier).(shift shifter)
    where 
        multiply multiplier tab = map (\x -> (multiplier*x)) tab
        shift 0 tab = tab
        shift shifter tab = shift (shifter - 1) (0:tab)

     

-- |
-- >>> let x = varP :: DensePoly Integer in x^3 - 1
-- P {unP = [-1,0,0,1]}
instance (Eq a, Num a) => Eq (DensePoly a) where
    (==) (P []) (P []) = True
    (==) (P []) _ = False
    (==) _ (P []) = False
    (==) (P (hd1:tl1)) (P (hd2:tl2)) = ((hd1 == hd2) && ((==) (P tl1) (P tl2))) 

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

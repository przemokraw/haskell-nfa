{- Przemysław Krawczyk, 305178 -}
module Auto (Auto , accepts, emptyA, epsA, symA, leftA, sumA, thenA, fromLists, toLists) where
    import Data.List

    data Auto a q  = A { states :: [q],  initStates :: [q], isAccepting :: q -> Bool, transition :: q -> a -> [q] } 

    accepts :: Eq q => Auto a q -> [a] -> Bool
    accepts (A s i acc t) w = any acc (foldl (\a b -> nub (concatMap (\x -> t x b) a)) i w)

    emptyA :: Auto a ()
    emptyA = A [()] [()] (\x -> False) (\_ _ -> [()])

    epsA :: Auto a ()
    epsA = A [()] [()] (\x -> True) (\_ _ -> [])

    symA :: Eq a => a -> Auto a Bool
    symA c = A [False, True] [False] (id) (\x y -> if x == False && y == c then [True] else [])

    leftA :: Auto a q -> Auto a (Either q r)
    leftA (A s1 i1 acc1 t1) = A s2 i2 acc2 t2 where
        s2 = map Left s1
        i2 = map Left i1
        acc2 = either acc1 (\z -> False)
        t2 = (\x y -> either (\z -> map Left (t1 z y)) (\z -> []) x)

    sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
    sumA (A s1 i1 acc1 t1) (A s2 i2 acc2 t2) = A s3 i3 acc3 t3 where
        s3 = (map Left s1) ++ (map Right s2)
        i3 = (map Left i1) ++ (map Right i2)
        acc3 = either acc1 acc2
        t3 = (\x y -> either (\z-> map Left (t1 z y)) (\z -> map Right (t2 z y))  x)

    thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
    thenA (A s1 i1 acc1 t1) (A s2 i2 acc2 t2) = A s3 i3 acc3 t3 where
        s3 = (map Left s1) ++ (map Right s2)
        i3 = if any acc1 i1 then (map Left i1) ++ (map Right i2) else (map Left i1) -- jeśli a1 akceptuje słowo puste, to dodaję stany początkowe z a2
        acc3 = either (\z -> if acc1 z && any acc2 i2 then True else False) (\z -> acc2 z)
        t3 = (\x y -> either 
            (\z -> if any acc1 (t1 z y) then (map Left (t1 z y)) ++  (map Right i2) else map Left (t1 z y))
            (\z -> map Right (t2 z y))
            x)

    toLists :: (Enum a, Bounded a) => Auto a q -> ([q], [q], [q], [(q, a, [q])])
    toLists (A s i f t) = (s, i, filter f s, [(x,y, t x y) | x <- s, y <- [minBound ..], not (null (t x y))]) 

    ok :: (Eq q, Eq a) => q -> a -> (q, a, [q]) -> Bool
    ok a b (x,y,_) = a == x && b == y

    get3 :: (q, a, [q]) -> [q]
    get3 (_,_,c) = c

    -- z listy przejść wybiera stany, do których można dojść z q przez a
    myfind :: (Eq q, Eq a) => q -> a -> [(q, a, [q])] -> [q]
    myfind a b l = let filtered = (filter (ok a b) l)  in if length filtered > 0 then nub (concatMap get3 filtered) else []

    fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
    fromLists s i f t = A (nub s) (nub i) (\x -> elem x f) (\x y -> myfind x y t)

    instance (Show a, Enum a, Bounded a, Show q) => Show (Auto a q) where
        show (A s i f t) = show (toLists (A s i f t))                                                       
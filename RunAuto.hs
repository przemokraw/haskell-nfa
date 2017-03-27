{- Przemysław Krawczyk, 305178 -}
module Main where
    import System.Environment
    import System.IO
    import Text.Read
    import Auto
    import Data.Maybe
    import Data.Char

    main = do
        args <- getArgs
        case args of
            [file] -> do
                h <- readFile file
                let autoDesc = let lst = lines h in if (null lst) || (last lst) /= "" then  [l | l <- (lines h), l /= ""] else [l | l <-  lst, l /= ""] ++ [""] -- potrzebne, żeby móc podawać puste słowo
                if length autoDesc < 4 then
                    printFail
                else do
                    let numOfStates = readMaybe (autoDesc !! 0) :: Maybe Int
                    let initStates = readMaybe (autoDesc !! 1) :: Maybe [Int]
                    let finalStates = readMaybe (autoDesc !! 2) :: Maybe [Int]
                    let word = last autoDesc
                    if (isNothing numOfStates || isNothing initStates || isNothing finalStates || 
                        any (not . isUpper) word) || any (> (fromJust numOfStates)) (fromJust initStates ++ fromJust finalStates)
                    then 
                        printFail
                    else do
                        let transitions = parseTransitions (fromJust numOfStates) (drop 3 (init autoDesc))
                        if isJust transitions then do
                            let auto = fromLists [1.. (fromJust numOfStates)] (fromJust initStates) (fromJust finalStates) (fromJust transitions)
                            print (accepts auto word)
                        else
                            printFail
            _ -> putStrLn "Wrong number of parameters."


    printFail :: IO ()
    printFail = putStrLn "BAD INPUT"

    -- parsuje wszystkie przejścia
    parseTransitions :: Int -> [String]  -> Maybe [(Int, Char, [Int])]
    parseTransitions maxState ls = if any (isNothing) transList then Nothing 
        else
            Just (concatMap (fromJust) transList)
        where
            transList = map ((parseTransition maxState) . words) ls

    -- parsuje jedną linię opisującą przejście w automacie
    parseTransition :: Int -> [String] -> Maybe [(Int, Char, [Int])]
    parseTransition maxState (q:[]) = Nothing
    parseTransition maxState (q:c:d) = if (isJust mState) && (fromJust mState) <= maxState && (all (\x -> x >= 'A' && x <= 'Z') mAlphas) && (all isJust mTrans) 
        then
            let listOfDestinations = [fromJust t | t <- mTrans] in if any (>maxState) listOfDestinations then
                Nothing
            else
                Just [(fromJust mState, mAlpha, listOfDestinations) | mAlpha <- mAlphas]
        else
            Nothing
        where
            mState = readMaybe q :: Maybe Int
            mAlphas = c
            mTrans = map (\x -> readMaybe x :: Maybe Int) d

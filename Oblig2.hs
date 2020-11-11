-- Francis Soliman Dyrhovden, Gruppe 1

-- 2.4: 10 (s 0 0, b 2 2) 4 1 7 1 5 2 6 2

-- Jeg antar at når man starter spillet så er det kun mulig å skrive quit, c eller r før man går inn i gameloop, og at det ikke er mulig å skrive c eller r i gameloop.
-- x og y koordinater er byttet om i min kode i forhold til oppgaven. Jeg oppdaget dette litt sent, og besluttet at det ikke var verdt å endre store deler av koden for dette da prinsippet er det samme.
-- Antar også at brukeren angir en fil som eksisterer og bruker formatet som er beskrevet i 2.3.

import System.IO ()
import Data.Char ( isDigit )
import Data.List ( delete )

type Board = [Pos]
type Pos = (Int,Int)
type Rule = [Int]
type Size = Int

main :: IO ()
main = do 
    putStrLn "Welcome! Enter a command:"
    command <- getLine
    let cs = words command
    case cs of 
         ("quit":[]) ->  do 
            putStrLn "Program has quit."
            return ()
         ("c":n:[]) -> board (read n :: Int)
         ("r":name:[]) -> do
            fc <- readFile name
            let commands = map (delete ')') (map (delete '(') (map (delete ',') (words fc)))
            if (commands == [])
                then do 
                    putStrLn "File is empty"
                    main
                else do
                    let boardSize = read (head commands) :: Int
                    cleanBoard (boardSize)
                    newGame (tail commands) boardSize [] [] []
         _ -> do                                                            
            putStrLn "Unknown command"
            main

board :: Size -> IO ()
board n 
    | n <= 0 || n >= 100 = error "Number must be between 1 and 99"
    | otherwise = do
        putStr "\ESC[2J"                                                
        goto (0,0)                                                    
        putStr $ spaces 2                                               
        mapM_ (putStr . (\num -> spaces (3 - l num) ++ show num)) [1..n]   
        putStrLn ""                                                     
        mapM_ (putStr . (rows n )) [1..n]                           
        printInstructions n 
        gameOfLife n [] [] [] 

rows :: Size -> Size -> String
rows cols row = spaces (2 - l row)                                  
                    ++ (show row)                                       
                    ++ concat ["  ." | _ <- [1..cols]]                 
                    ++ "\n"                                             

l :: Int -> Int
l = length . show

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

spaces :: Int -> String
spaces n = replicate n ' '

gameOfLife :: Size -> Rule -> Rule -> Board -> IO()
gameOfLife n s b cells = do
    goto (0,n+4)                                                        
    putStr "\ESC[0J"                                                    
    command <- getLine                                                 
    let cs = words command                                            
    case cs of                                                          
        ("quit":[]) -> do
            putStrLn "Program has quit."
            return ()                                              
        ("n":ls) -> do
                        let newCells = addCells n ls cells                                                  
                        changeArr ls n "O"
                        gameOfLife n s b newCells
        ("e":ls) -> do      
                        let newCells = removeCells n ls cells                                            
                        changeArr ls n "."
                        gameOfLife n s b newCells
        ("b":x:y:[]) -> 
                        if (isValid x y)
                            then do
                                goto (18,n+3)
                                putStr "\ESC[0J"
                                gameOfLife n s [read x :: Int, read y :: Int] cells
                            else do
                                printMessage "Not valid rule for births" n
                                gameOfLife n s b cells
        ("s":x:y:[]) -> 
                        if (isValid x y)
                            then do
                                goto (18,n+3)
                                putStr "\ESC[0J" 
                                gameOfLife n [read x, read y] b cells
                            else do
                                printMessage "Not valid rule for survivors" n
                                gameOfLife n s b cells
        ("?":[]) -> do
                            showRules n s b
                            gameOfLife n s b cells     
        ("w":[]) -> do
                        printLivingCells n cells
                        gameOfLife n s b cells
        ([]) -> do
                        let surv = survivors n s cells
                        let birth = births n b cells
                        cleanBoard n
                        changeArr (map show (convert surv)) n "O"
                        changeArr (map show (convert birth)) n "O"
                        gameOfLife n s b (concat [surv,birth])
        ("l":x:[]) ->
                        if (isDigit (head x))
                            then do
                                let updated = updateCells (read x :: Int) n s b cells
                                liveMode (read x :: Int) n s b cells
                                gameOfLife n s b updated
                            else do 
                                printMessage "Parameter must be an integer" n
                                gameOfLife n s b cells
        _ -> do                                                            
                        printMessage "Unknown command" n
                        gameOfLife n s b cells
                        
newGame :: [String] -> Size -> Rule -> Rule -> Board -> IO()
newGame ls n s b cells = case ls of
    [] -> gameOfLife n s b cells
    ("s":x:y:ls) -> if (isValid x y)
                        then do
                            goto (18,n+3)
                            putStr "\ESC[0J" 
                            newGame ls n [read x, read y] b cells
                        else do
                            printMessage "Not valid rule for survivors" n
                            gameOfLife n s b cells
    ("b":x:y:ls) -> if (isValid x y)
                        then do
                            goto (18,n+3)
                            putStr "\ESC[0J" 
                            newGame ls n s [read x, read y] cells
                        else do
                            printMessage "Not valid rule for survivors" n
                            gameOfLife n s b cells
    ls -> do
                            let newCells = addCells n ls cells                                                  
                            changeArr ls n "O"
                            newGame [] n s b newCells                        

updateCells :: Int -> Size -> Rule -> Rule -> Board -> Board
updateCells 0 _ _ _ cells = cells
updateCells x n s b cells = if (cells == (concat [survivors n s cells,births n b cells]))
                            then updateCells 0 n s b cells
                            else updateCells (x-1) n s b (concat [survivors n s cells,births n b cells])

liveMode :: Int -> Size -> Rule -> Rule -> Board -> IO()
liveMode 0 _ _ _ _ = return()
liveMode x n s b cells = do
    let surv = survivors n s cells
    let birth = births n b cells
    if (cells == (concat [surv,birth]))
        then do 
            printMessage "Board has reached a stable configuration." n
            liveMode 0 n s b cells
        else do
            cleanBoard n
            changeArr (map show (convert surv)) n "O"
            changeArr (map show (convert birth)) n "O"
            sequence_ [return () | _ <- [1..5000000]] -- Delay
            liveMode (x-1) n s b (concat [surv,birth])

convert :: [(Int,Int)] -> Rule
convert ls = concat (map (\(x,y) -> [x,y]) ls)

cleanBoard :: Size -> IO()
cleanBoard n  = do
        putStr "\ESC[2J"                                                
        goto (0,0)                                                    
        putStr $ spaces 2                                               
        mapM_ (putStr . (\num -> spaces (3 - l num) ++ show num)) [1..n]   
        putStrLn ""                                                     
        mapM_ (putStr . (rows n )) [1..n]
        printInstructions n 

births :: Size -> Rule -> Board -> [Pos]
births n b cells = [(x,y) | x <- [1..n], y <- [1..n], not (isAlive cells (x,y)), elem (liveneighbs n cells (x,y)) b ]

survivors :: Size -> Rule -> Board -> [Pos]
survivors n s cells = ([cell | cell <- cells, elem (liveneighbs n cells cell) s])

neighbours :: Size -> Pos -> [Pos]
neighbours n (x,y) = filter (\(x,y) -> x <= n && y <= n && x > 0 && y > 0) [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

liveneighbs :: Size -> [Pos] -> Pos -> Int
liveneighbs n cells = length . filter (isAlive cells) . neighbours n

isAlive :: Board -> Pos -> Bool
isAlive cells pos = elem pos cells

showRules :: Size -> Rule -> Rule -> IO()
showRules n [] [] = printMessage "No rules yet" n
showRules n s b = do
            goto (18,n+3)
            putStr "\ESC[0J"
            putStr "Rules: ( s "
            putStr (show s)
            putStr ", b "
            putStr (show b)
            putStr " )"

isValid :: String -> String -> Bool
isValid x y = and (map (all isDigit) [x,y]) && (read x :: Int) <= (read y :: Int) && (read x :: Int) >= 0

removeCells :: Size -> [String] -> Board -> Board
removeCells _ [] cells = cells
removeCells _ (_:[]) cells = cells
removeCells n (x:y:ls) cells = if ( elem (read x, read y)) cells
                                then delete (read x, read y) cells
                                else removeCells n ls cells

printLivingCells :: Size -> Board -> IO()
printLivingCells n [] =  do 
    goto (18,n+3)
    putStr "\ESC[0J"
    putStr "No living cells"
printLivingCells n cells = do
    goto (18,n+3)
    putStr "\ESC[0J"
    putStr (show cells)

addCells :: Size -> [String] -> Board -> Board
addCells _ [] cells = cells
addCells _ (_:[]) cells = cells
addCells n (x:y:ls) cells 
                    | and (map (all isDigit) [x,y]) = if ( elem (read x, read y)) cells then addCells n ls cells
                                                        else 
                                                            let (nx,ny) = (read x, read y) in
                                                            if (nx <= n && nx > 0 && ny <= n && ny > 0)
                                                                then addCells n ls (cells ++ [(nx,ny)])
                                                                else addCells n ls cells
                    | otherwise = addCells n ls cells

change :: String -> String -> Size -> String -> IO()                         
change x y n str
        | and (map (all isDigit) [x,y]) = let (nx,ny) = (read x, read y) in 
                                            if (nx > n || nx < 1 || ny > n || ny < 1)
                                                then printMessage "Out of bounds" n
                                                else printInGrid str nx ny n
        | otherwise = printMessage "Coordinates must be numbers" n

changeArr :: [String] -> Size -> String -> IO()
changeArr [] _ _ = return ()
changeArr (_:[]) n _ = printMessage "Coordinates must be in pairs" n
changeArr (x:y:ls) n str = do
    change x y n str
    changeArr ls n str

printInGrid :: String -> Int -> Int -> Size -> IO()                         
printInGrid s x y n = do
            printMessage "" n                                                  
            goto (x*3+2, y+1)                                               
            putStr s                                                        

printMessage :: String -> Size -> IO() 
printMessage s n = do
        goto (18,n+3) 
        putStr "\ESC[0J" 
        putStr s 

printInstructions :: Size -> IO()
printInstructions n = do
                goto (0,n+3) 
                putStrLn "Enter a command:" 
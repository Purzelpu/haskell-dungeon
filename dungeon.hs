import Data.Maybe


data Thing = Wall | Door Room | Empty
data Room = Room {north :: Thing, south :: Thing , west:: Thing, east:: Thing}
data Hero = Hero {position :: Room}

class Desc a where
        describe :: a -> String

instance Desc Thing where
        describe Wall = "a wall"
        describe (Door x) = "a door"

instance Desc Room where
         describe (Room n s w e) = "North: " ++ describe n ++ " South: " ++ describe s ++ " West: "
                ++ describe w ++ " East: " ++ describe e

class Interactive a where
        inter :: a -> Hero -> IO Hero

instance Interactive Thing where
        inter Wall hero = putStrLn "There is no way." >> return hero
        inter (Door toX) hero = putStrLn "You go through the door." >> return hero{position = toX}
	inter Empty hero = return hero

getDirection :: Char -> IO (Room -> Thing)
getDirection 'N' = putStrLn "You decide to go North" >> return north
getDirection 'S' = putStrLn "You decide to go South" >> return south
getDirection 'W' = putStrLn "You decide to go West" >> return west
getDirection 'E' = putStrLn "You decide to go East" >> return east
getDirection _ = putStrLn "You are confused" >> return (const Empty)


gameLoop :: Hero -> IO ()
gameLoop hero = do 
        putStrLn $ describe $ position hero
        next <- getLine
        putStrLn ""
        let direction = getDirection $ head next in do
		dir <- direction
                nextRound <- (inter (dir $ position hero) (hero))
		gameLoop nextRound
main = do
        gameLoop hero
        where 
                start = Room door1 Wall Wall Wall
                raum1 = Room Wall door2 Wall Wall
                door1 = Door raum1
                door2 = Door start
                hero = Hero start
        

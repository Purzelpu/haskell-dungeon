import Data.Maybe


data Thing = Wall | Door Room
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
        inter :: a -> Hero -> Hero

instance Interactive Thing where
        inter Wall hero = hero
        inter (Door toX) hero = hero{position = toX}

getDirection :: Char -> (Room -> Thing)
getDirection 'N' = north
getDirection 'S' = south
getDirection 'W' = west
getDirection 'E' = east


gameLoop :: Hero -> IO ()
gameLoop hero = do 
        putStrLn $ describe $ position hero
        next <- getLine
        putStrLn ""
        gameLoop (inter ((getDirection $ head next ) $ position hero) (hero))
main = do
        gameLoop hero
        where 
                start = Room door1 Wall Wall Wall
                raum1 = Room Wall door2 Wall Wall
                door1 = Door raum1
                door2 = Door start
                hero = Hero start
        

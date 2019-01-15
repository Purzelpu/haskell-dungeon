import Data.Maybe


data Thing = Wall | Door Room | Empty | Chest
data Room = Room {north :: Thing, south :: Thing , west:: Thing, east:: Thing, extra :: [Thing]}
data Hero = Hero {position :: Room, treasure:: Bool}
type Option = (Char,String, (Hero -> IO Hero))

class Desc a where
        describe :: a -> String

instance Desc Thing where
        describe Wall = "wall"
        describe (Door x) = "door"
        describe Chest = "chest"

instance Desc Room where
         describe (Room n s w e ex) = "North: " ++ describe n ++ " South: " ++ describe s ++ " West: "
                ++ describe w ++ " East: " ++ describe e
                ++ additionalDescription ex

additionalDescription :: [Thing] -> String
additionalDescription [] = ""
additionalDescription l = "\nAdditionally you see: " ++ (concat $ map describe l)
--TODO: format

class Interactive a where
        inter :: a -> Hero -> IO Hero
        actionName :: a -> String

instance Interactive Thing where
        inter Wall hero = putStrLn "There is no way." >> return hero
        inter (Door toX) hero = putStrLn "You go through the door." >> return hero{position = toX}
        inter Empty hero = return hero
        inter Chest hero = putStrLn "You open the chest. It contains the treasure" >> return hero{treasure=True}
        actionName (Door x) = "go through"
        actionName Wall = "examine"
        actionName Empty  ="do nothing"
        actionName Chest = "open"

getOptions :: Room -> [Option]
getOptions (Room n s w e ex) = ('N', (actionName n) ++ " the Northern " ++ describe n, inter n)
                        : ('S', (actionName s) ++ " the Southern " ++ describe s, inter s)
                        : ('W',(actionName w) ++ " the Western " ++ describe w, inter w)
                        : ('E', (actionName e) ++ " the Eastern " ++ describe e, inter e)
                        : zip3 ['a' .. 'z'] [actionName x ++ " the " ++ describe x| x<-ex] [inter x|x<-ex]

optionToString :: Option -> String
optionToString (c,s,_) = c:')':' ':s

describeOption :: [Option] -> Char -> String
describeOption l c = head [b | (a,b,_)<-l, a == c]

selectOption :: [Option] -> Char -> (Hero -> IO Hero)
selectOption l c = head [b | (a,_,b)<-l, a == c]


gameLoop :: Hero -> IO ()
gameLoop hero 
 |treasure hero == False = do 
        putStrLn $ describe $ position hero
        putStr $ unlines (map optionToString  (getOptions $ position hero))
        putStrLn "What do you do?"
        next <- getLine
        putStrLn ""
        let option = selectOption (getOptions$position hero) (head next) in do
                nextStep <- option hero
                gameLoop nextStep
 |treasure hero == True = do
        putStrLn "You found the treasure. Your quest has ended."

main = do
        gameLoop hero
        where 
                start = Room door1 Wall Wall Wall []
                raum1 = Room Wall doorS Wall door2 []
                raum2 = Room Wall Wall door1 Wall [Chest]
                doorS = Door start
                door1 = Door raum1
                door2 = Door raum2
                hero = Hero start False

import Data.Maybe

data Raum = Raum {nord :: Maybe Raum, ost :: Maybe Raum, sued ::  Maybe Raum, west :: Maybe Raum} deriving(Show)

describeRoom :: Raum -> String
describeRoom x = "Du siehst im Norden " ++  (describeExit $ nord x) ++ 
                 ", im Osten " ++ (describeExit $ ost x) ++
		 ", im Sueden " ++ (describeExit $ sued  x) ++
		 " und im Westen: " ++ (describeExit $ west  x) ++ "."


describeDoor :: Raum -> String
describeDoor x = "eine Tuer"

describeExit :: Maybe Raum -> String
describeExit x = (maybe "eine Wand" describeDoor x)

move :: Raum -> (Raum-> Maybe Raum) -> Raum
move room direction = case direction room of
		      Nothing -> room
                      Just x -> x

getDirection :: Char -> (Raum -> Maybe Raum)
getDirection 'N' = nord
getDirection 'S' = sued
getDirection 'O' = ost
getDirection 'W' = west
getDirection x = nord

getFeedback :: Char -> String
getFeedback 'N' = "Du gehst nach Norden."
getFeedback 'S' = "Du gehst nach Sueden."
getFeedback 'O' = "Du gehst nach Osten."
getFeedback 'W' = "Du gehst nach Westen."
getFeedback x = "Du bist verwirrt"

gameLoop r = do
       putStrLn $ describeRoom r
       putStrLn "Wohin jetzt?"
       dirLine <- getLine
       putStrLn $ getFeedback $ head dirLine
       putStrLn ""
       gameLoop $ move r (getDirection $head dirLine )



main = do 
	gameLoop start
	where start=Raum Nothing Nothing (Just raum1) Nothing
	      raum1 = Raum (Just start) Nothing Nothing Nothing

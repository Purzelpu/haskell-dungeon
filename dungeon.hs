import Data.Maybe

data Raum = Raum {nord :: Maybe Raum, ost :: Maybe Raum, sued ::  Maybe Raum, west :: Maybe Raum, schatz::Bool, text::String} deriving(Show)

describeRoom :: Raum -> String
describeRoom x = text x ++ " " ++
                 "Du siehst im Norden " ++  (describeExit $ nord x) ++ 
                 ", im Osten " ++ (describeExit $ ost x) ++
		 ", im Sueden " ++ (describeExit $ sued  x) ++
		 " und im Westen " ++ (describeExit $ west  x) ++ "."


describeDoor :: Raum -> String
describeDoor x = "eine Tuer"

describeExit :: Maybe Raum -> String
describeExit x = (maybe "eine Wand" describeDoor x)

move :: Raum -> Char -> (Raum,String)
move room dirChar = case (getDirection dirChar) room of
		      Nothing -> (room, "Hier ist keine Geheimtuer.")
                      Just x -> (x, getFeedback dirChar)

getDirection :: Char -> (Raum -> Maybe Raum)
getDirection 'N' = nord
getDirection 'S' = sued
getDirection 'O' = ost
getDirection 'W' = west
getDirection x = nord

getFeedback :: Char -> String
getFeedback 'N' = "Du gehst nach Norden"
getFeedback 'S' = "Du gehst nach Sueden"
getFeedback 'O' = "Du gehst nach Osten"
getFeedback 'W' = "Du gehst nach Westen"

gameLoop :: Raum -> IO ()
gameLoop r = case r of 
    Raum _ _ _ _ False _ -> do
       putStrLn $ describeRoom r
       putStrLn "Wohin jetzt?"
       dirLine <- getLine
       putStrLn ""
       putStrLn $ snd $ move r (head dirLine)
       gameLoop $ fst $ move r (head dirLine)
    Raum _ _ _ _ True _ -> do 
       putStrLn $ text r
       putStrLn "Du hast den Schatz gefunden!"



main = do 
	gameLoop start
	where start = Raum (Just raum2) Nothing (Just raum1) Nothing False "Der Eingang des Dungeons."
	      raum1 = Raum (Just start) Nothing Nothing Nothing True "Die Schatzkammer"
	      raum2 = Raum Nothing Nothing (Just start) Nothing False "Ein kleiner Raum."

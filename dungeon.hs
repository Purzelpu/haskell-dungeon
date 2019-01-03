import Data.Maybe

data Raum = Raum {nord :: Maybe Raum, ost :: Maybe Raum, sued ::  Maybe Raum, west :: Maybe Raum, schatz::Bool, text::String, creature :: Maybe Creature} deriving(Show)
data Hero = Hero {position :: Raum, message :: String}

data Creature = Orc deriving(Show)

describeCreature :: Creature -> String
describeCreature Orc = " Orc."

fightCreature :: Creature -> Bool
fightCreature Orc = True

attackCreature :: Creature -> String
attackCreature c 
  | fightCreature c = "Du besiegst den" ++ describeCreature c
  | otherwise = "Aua"

describeRoom :: Raum -> String
describeRoom r = case r of 
    Raum n o s w False t Nothing ->  t ++ " " ++
                 "Du siehst im Norden " ++  (describeExit $ n) ++ 
                 ", im Osten " ++ (describeExit $ o) ++
                 ", im Sueden " ++ (describeExit $ s) ++
                 " und im Westen " ++ (describeExit $ w) ++ "."
    Raum _ _ _ _ _ t (Just c) -> 
                 t ++ "Im Raum ist ein"  ++ describeCreature c
    Raum _ _ _ _ True t Nothing ->
                 t ++ " Hier ist der Schatz. Und niemand, der auf ihn aufpasst."

describeExit :: Maybe Raum -> String
describeExit Nothing = "eine Wand"
describeExit (Just _) = "eine Tuer"

move :: Raum -> Char -> Hero
move room dirChar = case (getDirection dirChar) room of
                      Nothing -> Hero room "Hier ist keine Geheimtuer."
                      Just x ->  Hero x (getFeedback dirChar)

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

gameLoop :: Hero -> IO ()
gameLoop h = let r = position h in
    case r of 
    Raum _ _ _ _ False _ (Just c) -> do
       putStrLn $ describeRoom r
       --TODO: Kampf oder Flucht?
       putStrLn $ attackCreature c 
       gameLoop h{position = r{creature=Nothing}}
    Raum _ _ _ _ False _ Nothing -> do
       putStrLn $ describeRoom r
       putStrLn "Wohin jetzt?"
       dirLine <- getLine
       putStrLn ""
       putStrLn $ message $ move r (head dirLine)
       gameLoop $ move r (head dirLine)
    Raum _ _ _ _ True _ Nothing -> do 
       putStrLn $ describeRoom r
       putStrLn "Du hast den Schatz gefunden!"



main = do 
        gameLoop (Hero start "")
        where start = Raum (Just raum2) Nothing (Just schatz) Nothing False "Der Eingang des Dungeons." Nothing
              schatz = Raum (Just start) Nothing Nothing Nothing True "Die Schatzkammer" Nothing
              raum2 = Raum Nothing (Just raum1) (Just start) Nothing False "Ein kleiner Raum." Nothing
              raum1 = Raum Nothing Nothing Nothing (Just raum2) False "Ein Raum mit abgenagten Knochen." (Just Orc)

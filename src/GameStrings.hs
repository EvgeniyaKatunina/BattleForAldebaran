module GameStrings where

gameName :: String
gameName = "Battle for Aldebaran"

gameIntro :: [String]
gameIntro = ["Commander, destroy all enemy ships!",
             "Avoid collision with the star or losing ships out of bounds"]

controlsInfo :: [String]
controlsInfo = ["Controls for P1:",
                "Left/Right - rotate ship",
                "Up - accelerate",
                "Down - shoot",
                ". and , - select ship",
                "",
                "Controls for P2:",
                "A/D - rotate  ship",
                "W - accelerate",
                "S - shoot",
                "Q and E - select ship",
                "",
                "Press SPACE to start or F2 to exit game"]

restartInfo :: [String]
restartInfo = ["Press SPACE to return to the title screen or F2 to exit"]

shipsSelection :: Int -> [String]
shipsSelection n = ["Number of ships: " ++ show n ++ " (enter 1 to 9)"]

gameFinishedStr :: String
gameFinishedStr = "Game finished"

winnerMessage :: Int -> String
winnerMessage a = "P" ++ show (a + 1) ++ " is the winner"

drawMessage :: String
drawMessage = "Draw"
module DisplayGame where
    
import Graphics.Gloss

--The main window
window1 :: Display
window1 = InWindow "Project_Snake" (1200, 800) (10, 10)

displayGame :: IO ()
displayGame = display window1 backgrounds mainScreens

--The background color of the window
backgrounds :: Color
backgrounds = white

--The main screen. This is the base for the play area + GUI
mainScreens:: Picture
mainScreens = pictures[playArea, seperators, gameStatsArea] --circle 80

score1 :: Picture
score1 =  color red $ translate (0) (10) $ scale 0.4 0.4 $ text "0000000"

scoreLbl :: Picture
scoreLbl =  color blue $ translate (0) (55) $ scale 0.2 0.2 $ text "Score"

gameStatsLable :: Picture
gameStatsLable = color orange $ translate (1000) (80) $ scale 0.2 0.2 $ text "Level 01"

gameLabbel :: Picture
gameLabbel = color green $ translate (1000) (10) $ scale 0.2 0.2 $ text "Lives: 3"


--Walls, takes in a float to adjust the offset/placement on the play area. 
--In gloss point (0,0) seems to be true center of the window
seperator :: Float -> Picture
seperator offset =
      translate 0 offset $
        color seperatorColor $
          rectangleSolid 1200 5

--The wall color
seperatorColor = greyN 0.25

--All the walls grouped into one array of pictures. 
seperators = pictures [seperator 300]

--The background color of the play area. Made this a seperate field so we can change
--the play area background without changing the background color of the upper game stats area
playAreaColor = greyN 0.75

--The placement on the y axis of the play area.
playAreaOffset = -50

gameStatsOffset = 300 
gameStatsArea = translate (-600) gameStatsOffset $ pictures[score1, scoreLbl, gameStatsLable, gameLabbel]

playAreaBackground :: Picture
playAreaBackground = translate 0 playAreaOffset $ color playAreaColor $ rectangleSolid 1200 700

playeAreaLbl:: Picture
playeAreaLbl = translate (0) (0) $ text "Game"

-- ***** This is the play area. Add your visuals here ********
playArea = pictures[playAreaBackground]
-- ***********************************************************


{- snake :: Picture
snake = imgToPic 0.75 "assets/snake.png" -}
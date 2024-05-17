{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module TitleScreen where

import DisplayGame
import Game
import Data.GI.Base
import qualified GI.Gtk as Gtk

import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap

--This is the GTK code. Cant get it to work on Windows but works on Linux
titleScreen :: IO()
titleScreen = do
    Gtk.init Nothing

--This is where the window for the title screen is created
    win <- new Gtk.Window [#title := "Snake"]
    on win #destroy Gtk.mainQuit
    #resize win 1910 1050
     
    --These boxes are the panels that we add the GUI to 
    box <- new Gtk.Box [#orientation := Gtk.OrientationVertical]
    box1 <- new Gtk.Box [#orientation := Gtk.OrientationVertical]

    {- container <- new Gtk.Container
    box3 <- play win1 black 30 initWorld render handleInput update -}
    --box3 <- new Gtk.Overlay [#child : box1]
    
    --This will be deleted
    headerBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    scoreBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    livesBox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
    #add win box

    {- msg <- new Gtk.Label [#label := "Hello"]
    #add box msg -}

--Getting the home directory
    home <- getHomeDirectory
    
--Setting the images
    image <- Gtk.imageNewFromFile $ home ++ "/Documents/357/cs357l-super-snake-game/images/snake.gif"
    image1 <- Gtk.imageNewFromFile $ home ++ "/Documents/357/cs357l-super-snake-game/images/grass.jpg"
    image2 <- Gtk.imageNewFromFile $ home ++ "/Documents/357/cs357l-super-snake-game/images/Start-Game.png"
    image3 <- Gtk.imageNewFromFile $ home ++ "/Documents/357/cs357l-super-snake-game/images/Super-Snake.png"
    image4 <- Gtk.imageNewFromFile $ home ++ "/Documents/357/cs357l-super-snake-game/images/600000.png"
    scoreImage <- Gtk.imageNewFromFile $ home -- ++ "/Documents/357/cs357l-super-snake-game/000000.png"
    livesImage <- Gtk.imageNewFromFile $ home -- ++ "/Documents/357/cs357l-super-snake-game/OIP1.jpg"
    
--The back button for the menu
    backButton <- new Gtk.Button [#label := "<- Back"]
    Gtk.buttonSetRelief backButton Gtk.ReliefStyleNone
    #packStart box1 backButton False False 10

--The start game button
    btn <- new Gtk.Button []
    Gtk.buttonSetRelief btn Gtk.ReliefStyleNone
    Gtk.buttonSetImage btn $ Just image2
    
--Level editor button
    btn1 <- new Gtk.Button [#label := "Level Editor"]
    Gtk.buttonSetRelief btn1 Gtk.ReliefStyleNone

--Exit button
    btn2 <- new Gtk.Button [#label := "Exit"]
    Gtk.buttonSetRelief btn2 Gtk.ReliefStyleNone

--Add the ui to the box panels
    #add box image3
    #add box image
    #add scoreBox scoreImage

    #packStart livesBox image4 False True 100
    #packStart livesBox livesImage True True 10

--Adds the score and lives to the header box. This will probably be deleted
    #add headerBox scoreBox
    #add headerBox livesBox

    #add box1 headerBox 
    #packStart box1 image1 True False 10

--Packstart just gives eveerything space and a margin
    #packStart box btn True False 10
    #packStart box btn1 True False 10
    #packStart box btn2 True False 10

--The start button clicked event handler
    on btn #clicked $ do
        #remove win box
        #add win box1
        mainGame--play mainWindow black 30 initWorld render1 handleInput update1
        
--The level editor clicked event handler        
    on btn1 #clicked $ do
        #remove win box
        #add win box1
        #showAll win
    
--The exit button click event
    on backButton #clicked $ do
        #remove win box1
        #add win box
        #showAll win
    
--Show the window 
    #showAll win

    Gtk.main


--Paste this in main when you want to call this class. also import the class
{- main :: IO ()
main = displayTitle -}
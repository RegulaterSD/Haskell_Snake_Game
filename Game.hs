module Game (
--list everything for the game
    Score,
    gridSquares,
    Snake,
    Direction,
    Food,
    Walls,
    mainGame
) where
    --This will be used to generate the random position of the food
--Will be called each time food is ate on a list of all available positions
--for food to spawn
import System.Random (randomRIO)
import System.Random
import Graphics.Gloss.Interface.Pure.Game as Gloss
import DisplayGame

--Wall Positions
type Walls = [Pos]

--Score as an Int
type Score = Int

--Need to decide if we will run with a x * y Array
--Or a number position on grid.
--currently position will be an X * Y
type Pos = (Int, Int)

--Row/Col
type ColPixels = Int
type RowPixels = Int

--This is the state of the snake.
--Position of the snake is held in an array
--the direction it is moving
--and if it is to grow or stay same size
data Snake
    = Snake
        { snakePosition  :: [Pos]
        , growSnake  :: Bool
        , invincible :: Bool
        } deriving (Show, Eq)

--This is the direction
data Direction 
    = MoveDown 
    | MoveUp 
    | MoveLeft 
    | MoveRight
    | None
    deriving (Show, Eq)

--this is the user input to move the snake
data UserPreviousKey
    = KeyDown
    | KeyUp
    | KeyLeft
    | KeyRight
    | NoKey
    deriving (Show, Eq)


--This is the food.
--SpecialFood for the future food update
-- Possible bug, 1 food item with a list of positions, should be a list of food items with 1 position?
-- Changed [Pos] to Pos for a single food item for this game version
data Food 
    = Food
        { foodPosition :: Pos
        , specialFood :: Bool
        , pointValue :: Int
        } deriving (Show, Eq)

--This is for the walls. 
--Traps added for the future of traps.
-- data Walls
--     = Walls
--         { position :: [POS]
--         , traps :: Bool
--         } deriving (Show, Eq)

--This is for the graphics. What the grid point is occupied by
data GridCell 
    = Empty
    | S 
    | F 
    | W  
  deriving (Show, Eq)

type Grid = [GridCell]

data GameState 
    = GameState
    { snake :: Snake
    , walls :: Walls
    , food :: Food
    , score :: Score
    , stepRate :: Int
    , gameOver :: Bool
    , direction :: Direction
    , previousKey :: UserPreviousKey
    , gridSquares :: Int
    } deriving (Show, Eq)


{-
                    Graphics
=============================================================================================
-}

--Starting FPS
fps :: Int
fps = 7

--Window Information
windowSize, windowOffset :: Int
windowSize = 600
windowOffset = 200

window :: Gloss.Display
window = Gloss.InWindow "Snake" (windowSize, windowSize) (windowOffset, windowOffset)

background :: Gloss.Color
background = Gloss.white

--Translate to move the drawing of grid to top left/right corner instead of center.
translate' :: Float -> Float -> Gloss.Picture -> Gloss.Picture
translate' x y = Gloss.Translate (x - (s / 2) - 200) (-(y - (s / 2) -175))
    where
        s = fromIntegral windowSize

gridColor :: GridCell -> Gloss.Color
gridColor Empty = Gloss.white
gridColor S = Gloss.red
gridColor W = Gloss.black
gridColor F = Gloss.blue

--Build Game Grid to be drawn
--This is a Array of the entire game state 
--buildGrid :: [[Int]] -> GameState -> Grid
--buildGrid [] _ = []
--buildGrid (x:xs) state  
--    | x `elem` snakeBody state = S ++ buildGrid xs state
--    | x `elem` Food state = F ++ buildGrid xs state
--    | x `elem` walls state = W ++ buildGrid xs state
--    | otherwise = Empty ++ buildGrid xs state



{-
                    Snake Functions
=============================================================================================
-}

--returns the position of the snake body
snakeBody :: GameState -> [Pos]
snakeBody = snakePosition . snake

--returns the position of only the snake head
snakeHead :: GameState -> Pos
snakeHead = head . snakeBody

--Grow Snake
grow :: GameState -> GameState
grow state = state { snake = (snake state) { growSnake = True}}

--snakeGrow :: GameState -> Bool
--snakeGrow state = grow snake

-- Function to cut snake where it ate itself
cutSnake :: Snake -> Pos -> Snake
cutSnake snake snakeHead =
    let snakeTail = tail $ snakePosition snake -- drop head from body
        newBodyTail = takeWhile (/= snakeHead) snakeTail -- save body up to snakeHead == collision position
    in snake { snakePosition = snakeHead : newBodyTail } -- prepend snakeHead back to body

-- Detects if snake eats itself
checkselfCollision :: GameState -> GameState
checkselfCollision state
    | snakeHead `elem` snakeTail =
        if invincible $ snake state
            then state { snake = cutSnake (snake state) snakeHead } -- Cut the snake if it's invincible
            else state { gameOver = True } -- Set gameOver to True if not invincible
    | otherwise = state -- Continue the game if no collision
    where
        snakeHead = head $ snakePosition $ snake state
        snakeTail = tail $ snakePosition $ snake state

checkGameOver :: GameState -> GameState
checkGameOver state = 
    let stateAfterSelfCollision = checkselfCollision state
    in checkWallCollision stateAfterSelfCollision


{-
                    Food Functions
=============================================================================================
-}
--This will create a list of all coordinate positions on the Map
allCords :: Int -> Int -> [Pos]
allCords 0 _ = []
allCords n k = if k < n then [(x, k) | x <- [0 .. n - 1]] ++ allCords n (k+1) else []

removeAll :: [Pos] -> [Pos] -> [Pos]
removeAll [] [] = []
removeAll [] ls = ls
removeAll (x:xs) ls = removeAll xs (removeElement ls x)

--Now we will subtract all positions of the walls and Snake from the coordinates that way
--the food can spawn in an empty position. 

removeElement :: [Pos] -> Pos -> [Pos]
removeElement [] ls2 = []
removeElement ls1 ls2 = remove ls2 ls1
    where remove element = filter (/= element)

grid :: [Pos]
grid = allCords 50 0

getSnakeWalls :: GameState -> [Pos]
getSnakeWalls state = walls state ++ snakePosition (snake state)

--spawnRandomFood :: GameState -> GameState
--spawnRandomFood state = state { foodPosition food = (removeAll getSnakeWalls grid)!!(randomRIO(0,(length $ (removeAll getSnakeWalls grid)))) }

{- 
Randomized version of food generation 
randomFood :: [Pos] -> Pos
randomFood [] = []
randomFood ls = ls!!randomRIO(0,(length ls))

-- Half Baked implementation of new food generation with random lib
-- all <- lines might need a do function like updateFood right below
-- updates Gamestates food item using Random
updateFood :: GameState -> IO GameState
updateFood state = do
    newFood <- generateFood
    return $ state { food = newFood }

generateFood :: IO Food
pos <- randomPosition -- Generate New Position/Coordinates ("<-" defines save from IO datatype to 
-- standard datatype like Int in this case (Int, Int))
isSpecial <- randomBool -- Generate speciality of food
let points = calculatePointValue isSpecial -- Get points from speciality of food


-- Helper: Food Position Generation
-- TODO: needs implementation for available coordinates on map instead of pure random
randomPosition :: IO Pos
randomPosition = do
    x <- randomRIO (0, gridSizeX) -- (lo,hi) format
    y <- randomRIO (0, gridsizeY)
    return (x, y)

-- Helper Food Speciality generation
randomBool :: IO Bool
randomBool = randomRIO(False, True)

-- Helper: Point value determinance from speciality
calculatePointValue :: Bool -> Int
calculatePointValue = if isSpecial then 20 else 5
-}

--randomFoodPostion :: GameState -> Pos
--randomFood state = (removeAll (getSnakeWalls state) grid)!!(fromIntegral (randomRIO(0,(length $ (removeAll (getSnakeWalls state) grid)))))

-- Main Helper Function to create new food after it is eaten
generateFood :: Food
generateFood = Food 
    { foodPosition = (5,5)  -- Generate New Position/Coordinates
    , specialFood = False  -- Generate speciality of food
    , pointValue = 5  -- Get points from speciality of food
    }

-- Call to update GameState Food after it is eaten
updateFood :: GameState -> GameState
updateFood state = state { food = generateFood }

-- TODO: Needs to turn on invincible for snake
-- Detects when snake eats food
-- Assuming Food position :: Pos and not [Pos]
detectFoodCollision :: GameState -> GameState
detectFoodCollision state
    | snakeHead == fPosition = updateFood $ updateScore $ setInvincible $ grow state
    | otherwise = state
    where
        snakeHead = head $ snakePosition $ snake state
        fPosition = foodPosition $ food state -- Assuming foodPosition is a function or field of Food

-- Helper function to set the snake's invincible bool to false
setInvincible :: GameState -> GameState
setInvincible state =
    state { snake = (snake state) { invincible = False } }

{-
                    Wall Functions
==============================================================================================
-}

-- Detects & handles when a snake collides with walls
checkWallCollision :: GameState -> GameState
checkWallCollision state
    | snakeHead `elem` (walls state) = state {gameOver = True}
    | otherwise = state
    where
        snakeHead = head $ snakePosition $ snake state 

{-
                    Game Functions
===============================================================================================
-}
-- TODO: 
-- Need main game loop?
-- Re-map input direction function to new move function instead of commented out move
-- Need to implement editor commands

--Game Over
gameOverState :: GameState -> GameState
gameOverState state = state {gameOver = True }

--Update Game Score
updateScore :: GameState -> GameState
updateScore state = 
    let currentScore = score state  -- Retrieve the current score from the state
        additionalPoints = pointValue $ food state  -- Get the point value of the food
    in state { score = currentScore + additionalPoints }  -- Update the score in the state
-- TODO: 
-- Needs stack function to enqueue and pop pressed moves for seemless movement
{-
Changed implementation of move to nextMove and nextHead functions
-}
-- Main move function that moves snake and grows snake
nextMove :: GameState -> GameState
nextMove state = 
    let oldSnake = snake state -- get current snake variable from GameState aka state
        oldBody = snakePosition oldSnake -- get old body from snake
        newHead = nextHead (direction state) (head $ snakePosition oldSnake) -- get new head position
        nextBody = if growSnake oldSnake || null oldBody
                    then newHead : oldBody -- grow or single segment snake
                    else newHead : init oldBody -- update body
    in state { snake = oldSnake { snakePosition = nextBody, growSnake = False }} -- save as new snake state


-- Helper function for moving snake, calculating next head coordinate
nextHead :: Direction -> Pos -> Pos
nextHead MoveUp (x, y) = (x, y-1)
nextHead MoveDown (x, y) = (x, y+1)
nextHead MoveLeft (x, y) = (x-1, y)
nextHead MoveRight (x, y) = (x+1, y)
nextHead None pos = pos  -- No movement

inputDirection :: Gloss.Event -> GameState -> GameState
inputDirection event gameState =
    case event of
        Gloss.EventKey (Gloss.SpecialKey Gloss.KeyUp) Gloss.Down _ _ ->
            if previousKey gameState /= Game.KeyDown
                then gameState { direction = MoveUp, previousKey = Game.KeyUp }
                else gameState
        Gloss.EventKey (Gloss.SpecialKey Gloss.KeyDown) Gloss.Down _ _ ->
            if previousKey gameState /= Game.KeyUp
                then gameState { direction = MoveDown, previousKey = Game.KeyDown }
                else gameState
        Gloss.EventKey (Gloss.SpecialKey Gloss.KeyLeft) Gloss.Down _ _ ->
            if previousKey gameState /= Game.KeyRight
                then gameState { direction = MoveLeft, previousKey = Game.KeyLeft }
                else gameState
        Gloss.EventKey (Gloss.SpecialKey Gloss.KeyRight) Gloss.Down _ _ ->
            if previousKey gameState /= Game.KeyLeft
                then gameState { direction = MoveRight, previousKey = Game.KeyRight }
                else gameState
        _ -> gameState  -- Ignore other keys






{-
    Initial Game State Initializations
==================================================================================
-}

-- Main function for initial game state conditions
initialGameState :: GameState
initialGameState = GameState
    { snake = initialSnake
    , walls = initialWalls
    , food = initialFood
    , score = 0
    , stepRate = fps
    , gameOver = False
    , direction = None
    , previousKey = NoKey -- Assuming NoKey is a constructor for UserPreviousKey
    , gridSquares = gridSize -- size n in nxn grid, 5 in 5x5 for example
    }

-- Main Helper function for initial snake conditions
initialSnake :: Snake
initialSnake = Snake 
    { snakePosition = initialSnakePosition
    , growSnake = False
    , invincible = False}

-- Subhelper functions for initial snake conditions
initialSnakePosition :: [Pos]
initialSnakePosition = [(25, 25), (26, 25), (27, 25)]


-- Traps the snake in a box (initial walls outlines the gridsize)
-- Assuming gridsize = n in nxn grid
initialWalls :: Walls
initialWalls = topBottomWalls ++ leftRightWalls
  where
    topBottomWalls = [(x, 0) | x <- [0 .. gridSize - 1]] ++
                     [(x, gridSize - 1) | x <- [0 .. gridSize - 1]]
    leftRightWalls = [(0, y) | y <- [1 .. gridSize - 2]] ++ 
                     [(gridSize - 1, y) | y <- [1 .. gridSize - 2]]


initialFood :: Food
initialFood = Food 
    { foodPosition = (10, 10) -- initial position of the food
    , specialFood = False     -- assuming regular food initially
    , pointValue = 5          -- assuming a point value of 1 for regular food
    }


gridSize :: Int
gridSize = 50 -- Grid size of 5x5 for example

-- Update function: updates the game state as time progresses
update :: Float -> GameState -> GameState
update seconds state
    | gameOver state = state  -- If the game is over, don't update the state
    | otherwise = 
        let newState = nextMove state  -- Move the snake based on its current direction
            --newState' = checkGameOver newState  -- Check if the game is over after the move
            newState'' = detectFoodCollision newState  -- Check for food collision
        in newState''

-- Main render function
render :: GameState -> Picture
render state
    | gameOver state = renderGameOver state
    | otherwise = Pictures [renderSnake (snake state), renderFood (food state), renderWallsFromState state, renderScore (score state)]


-- Function to render the snake
renderSnake :: Snake -> Picture
renderSnake snake = renderSnakeSegment (snakePosition snake)

renderSnakeSegment :: [Pos] -> Picture
renderSnakeSegment positions = Pictures $ map renderSegment positions
  where
    renderSegment (x, y) = translate' (fromIntegral x * segmentSize) (fromIntegral y * segmentSize) $ color green $ rectangleSolid segmentSize segmentSize
    segmentSize = 20



-- Function to render the food
renderFood :: Food -> Picture
renderFood food = translate' (fromIntegral fx * segmentSize) (fromIntegral fy * segmentSize) $ color red $ circleSolid (segmentSize / 2)
    where 
        (fx, fy) = foodPosition food
        segmentSize = 20

-- Function to render the walls using GameState
renderWallsFromState :: GameState -> Picture
renderWallsFromState state = Pictures $ map renderWallSegment (walls state)

renderWallSegment :: Pos -> Picture
renderWallSegment (x, y) = translate (fromIntegral x * wallSize - halfGridWidth) (fromIntegral y * wallSize - halfGridHeight) $ color wallColor $ rectangleSolid wallSize wallSize
    where
        wallSize = 20  -- Size of each wall segment
        wallColor = makeColorI 255 0 0 255  -- Using bright red for visibility
        halfGridWidth = fromIntegral gridSize * wallSize / 2 - wallSize / 2
        halfGridHeight = fromIntegral gridSize * wallSize / 2 - wallSize / 2



-- Function to render the score
renderScore :: Score -> Picture
renderScore score = translate (-300) (300) $ scale 0.3 0.3 $ color black $ text $ "Score: " ++ show score

-- Function to render the game over screen
renderGameOver :: GameState -> Picture
renderGameOver state = translate (-100) 0 $ scale 0.5 0.5 $ color black $ text "Game Over!"


mainGame :: IO ()
mainGame = play window1 white 15 initialGameState render inputDirection update
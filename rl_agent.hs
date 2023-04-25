import System.Random
import Control.Monad
import Control.Monad.State

-- Terrain parameters
width :: Int
width = 10

height :: Int
height = 10

density :: Float
density = 0.3

-- Reinforcement learning parameters
alpha :: Float
alpha = 0.1

gamma :: Float
gamma = 0.9

-- Terrain
type Terrain = [[Float]]

randomTerrain :: Int -> Int -> Float -> IO Terrain
randomTerrain w h d = replicateM h $ replicateM w randomCell
  where
    randomCell = do
      r <- randomRIO (0, 1)
      return $ if r < d then 1 else 0

-- Reinforcement learning agent
type Agent = (Int, Int)

type QTable = [[Float]]

move :: Agent -> Int -> Agent
move (x, y) 0 = (min (x + 1) (width - 1), y)  -- Right
move (x, y) 1 = (x, min (y + 1) (height - 1)) -- Up
move (x, y) 2 = (max (x - 1) 0, y)            -- Left
move (x, y) 3 = (x, max (y - 1) 0)            -- Down

qUpdate :: QTable -> Terrain -> Agent -> Int -> Float
qUpdate qTable terrain agent action = (1 - alpha) * qOld + alpha * (reward + gamma * qMax)
  where
    qOld = (qTable !! fst agent) !! snd agent
    nextState = move agent action
    qMax = maximum [((qTable !! fst nextState) !! snd nextState) | action <- [0..3]]
    reward = - terrain !! fst nextState !! snd nextState

trainStep :: Terrain -> StateT (QTable, Agent) IO Agent
trainStep terrain = do
  (qTable, agent) <- get
  action <- liftIO $ randomRIO (0, 3)
  let newQ = qUpdate qTable terrain agent action
  let newQRow = take action (qTable !! fst agent) ++ [newQ] ++ drop (action + 1) (qTable !! fst agent)
  let newQTable = take (fst agent) qTable ++ [newQRow] ++ drop (fst agent + 1) qTable
  let newAgent = move agent action
  put (newQTable, newAgent)
  return newAgent

train :: Terrain -> Int -> StateT (QTable, Agent) IO [Agent]
train _ 0 = return []
train terrain n = do
  newAgent <- trainStep terrain
  restPath <- train terrain (n - 1)
  return (newAgent : restPath)

printTerrainWithPath :: Terrain -> [Agent] -> IO ()
printTerrainWithPath terrain path = mapM_ printRow $ zip [0..] terrain
  where
    printRow (rowIdx, row) = putStrLn $ map (cellToChar rowIdx) $ zip [0..] row
    cellToChar rowIdx (colIdx, cost) =
      if (colIdx, rowIdx) `elem` path
      then 'x'
      else if cost == 1 then '#' else '.'

-- Indexed fron upper left.
main :: IO ()
main = do
  terrain <- randomTerrain width height density
  let initialQTable = replicate width $ replicate height 0
  let initialState = (0, height `div` 2)
  (agentPath, (finalQTable, finalAgent)) <- runStateT (train terrain 2) (initialQTable, initialState)
  putStrLn "Agent path:"
  mapM_ print agentPath
  putStrLn "Final agent position:"
  print finalAgent
  putStrLn "Terrain with path:"
  printTerrainWithPath terrain agentPath
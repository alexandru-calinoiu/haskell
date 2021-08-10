import           Control.Monad.State       (State)
import           Control.Monad.State.Class (get, put)
import qualified Data.Array                as A
import           System.Random             (Random (randomR), StdGen)

data GameState
  = GameState
      { board         :: A.Array TileIndex TileState
      , currentPlayer :: Player
      , generator     :: StdGen
      }

data Player = XPlayer | OPlayer

data TileState = Empty | HasX | HasO deriving (Eq)

type TileIndex = (Int, Int)

chooseRandomMove :: State GameState TileIndex
chooseRandomMove = do
    game <- get
    let openSpots = [ fst pair | pair <- A.assocs (board game), snd pair == Empty ]
    let gen = generator game
    let (i, gen') = randomR (0, length  openSpots - 1) gen
    put $ game { generator = gen' }
    return $ openSpots !! i

applyMove :: TileIndex -> State GameState ()
applyMove i = do
  game <- get
  let p = currentPlayer game
  let newBoard = board game A.// [(i, tileForPlayer p)]
  put $ game { currentPlayer = nextPlayer p, board = newBoard }

nextPlayer :: Player -> Player
nextPlayer XPlayer = OPlayer
nextPlayer OPlayer = XPlayer

tileForPlayer :: Player -> TileState
tileForPlayer XPlayer = HasX
tileForPlayer OPlayer = HasO

resolveTurn :: State GameState Bool
resolveTurn = do
  i <- chooseRandomMove
  applyMove i
  isGameDone

isGameDone :: State GameState Bool
isGameDone = do
  game <- get
  let openSlots = [fst pair | pair <- A.assocs (board game), snd pair == Empty]
  return $ null openSlots

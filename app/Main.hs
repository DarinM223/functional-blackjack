module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Trans
import System.Random
import System.Random.Shuffle

data Suite = Hearts | Spades | Clubs | Diamonds deriving (Show)

data Card = Card { cardSuite :: Suite, cardValue :: Int } deriving (Show)

-- | Creates a new card given a suite and a value.
card :: Suite -> Int -> Card
card suite value = Card { cardSuite = suite, cardValue = value }

type Deck = [Card]

-- | Creates a standard 52 card deck.
deck :: StdGen -> Deck
deck = shuffle' cards (length cards)
  where
    cards = card <$> [Hearts, Spades, Clubs, Diamonds] <*> [1..13]

data Action = Stand | Hit deriving (Show)

data Player = Player
    { playerHighAces :: Int
    , playerScore    :: Int
    , playerMoney    :: Int
    } deriving (Show)

data Dealer = Dealer
    { dealerHighAces :: Int
    , dealerScore    :: Int
    } deriving (Show)

data GameState = GameState
    { gameDealer  :: Dealer
    , gamePlayers :: [Player]
    , gameDeck    :: Deck
    , gameBets    :: [Int]
    } deriving (Show)

type GameStateM = StateT GameState IO

-- | Returns the number of players in the game.
numPlayers :: GameState -> Int
numPlayers = length . gamePlayers

-- | Draws a card from the deck and adds it to the player given by the index.
dealCardToPlayer :: Int -> GameState -> GameState
dealCardToPlayer i s = s { gameDeck = rest, gamePlayers = players' }
  where
    (card:rest) = gameDeck s
    players' = updateList i (addCard card) $ gamePlayers s

-- | Draws a card from the deck and adds it to the dealer.
dealCardToDealer :: GameState -> GameState
dealCardToDealer s = s { gameDeck = rest, gameDealer = dealer' }
  where
    (card:rest) = gameDeck s
    dealer' = addCard card $ gameDealer s

-- | Removes players who have no money.
removeBrokePlayers :: GameState -> GameState
removeBrokePlayers s = s { gamePlayers = removedBroke }
  where
    removedBroke = filter ((/= 0) . playerMoney) $ gamePlayers s

-- | Adds the given amount of money to the player given by the index.
addMoneyToPlayer :: Int -> Int -> GameState -> GameState
addMoneyToPlayer money i s = s { gamePlayers = players' }
  where
    players' = updateList i (addMoney money) $ gamePlayers s

-- | Resets all of the player's scores to 0.
resetScores :: GameState -> GameState
resetScores s = s { gamePlayers = players' }
  where
    players' = (\p -> p { playerScore = 0}) <$> gamePlayers s

-- | Deals cards to the dealer and the players.
dealCards :: Int -> GameState -> GameState
dealCards num s = (dealCardsToDealer . dealCardsToPlayers) s
  where
    dealIndexes = (\_ b -> b) <$> [1..num] <*> [0..numPlayers s - 1]
    dealCardsToPlayers s = foldr dealCardToPlayer s dealIndexes
    dealCardsToDealer s = foldr dealDealer s [1..num]
    dealDealer _ = dealCardToDealer

-- | Asks the players for the bets and sets the bets in the state.
readBets :: GameStateM ()
readBets = do
    s <- get
    bets <- forM (zip (gamePlayers s) [0..]) $ \(player, i) ->
        lift $ readBet player (i + 1)
    put s { gameBets = bets }

-- | Checks if the player lost or won and either takes or gives money.
handleBet :: Dealer -> Player -> Int -> Int -> GameStateM ()
handleBet d p bet i
    | playerScore p > 21 = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " busted"
        modify $ addMoneyToPlayer (- bet) i
        lift $ putStrLn $ "Player lost " ++ show bet
        return ()
    | dealerScore d > 21 = do
        lift $ putStrLn $ "The dealer busted but player " ++ show (i + 1) ++ " didn't bust"
        modify $ addMoneyToPlayer bet i
        lift $ putStrLn $ "Player received " ++ show bet
        return ()
    | playerScore p > dealerScore d = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " had a higher score than the dealer"
        modify $ addMoneyToPlayer bet i
        lift $ putStrLn $ "Player received " ++ show bet
        return ()
    | otherwise = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " had a lower or equal score than the dealer"
        modify $ addMoneyToPlayer (- bet) i
        lift $ putStrLn $ "Player lost " ++ show bet 
        return ()

-- | Displays the player's money.
displayMoney :: GameStateM ()
displayMoney = do
    s <- get
    forM_ (zip (gamePlayers s) [0..]) $ \(p,i) ->
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s money is: " ++ show (playerMoney p)
    return ()

-- | Displays the scores of the players and the dealer.
displayScores :: GameStateM ()
displayScores = do
    s <- get
    lift $ putStrLn $ "The dealer's score is: " ++ show (dealerScore . gameDealer $ s)
    forM_ (zip (gamePlayers s) [0..]) $ \(p,i) ->
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s score is: " ++ show (playerScore p)
    return ()

-- | Handles the bet for every player after a turn.
handleBets :: GameStateM ()
handleBets = do
    s <- get
    forM_ (zip3 (gamePlayers s) (gameBets s) [0..]) $ \(p, b, i) ->
        handleBet (gameDealer s) p b i
    return ()

-- | Goes through every player asking for actions and applying them.
-- Will ask for actions multiple times until it gets the Stand action
-- (either automatically or chosen by the player).
handlePlayers :: Int -> Int -> GameStateM ()
handlePlayers i len
    | i < len = do
        s <- get
        let players = gamePlayers s
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s current score is: " ++ (show . playerScore) (players !! i)
        action <- (lift . decideAction) (players !! i)
        case action of
            Hit -> do
                modify $ dealCardToPlayer i
                handlePlayers i len
            Stand -> handlePlayers (i + 1) len
    | otherwise = do
        s <- get
        action <- (lift . decideAction) $ gameDealer s
        lift $ putStrLn $ "Dealer's action is: " ++ show action
        case action of
            Hit -> do
                modify dealCardToDealer
                handlePlayers i len
            Stand -> return ()

-- | Runs a turn in the blackjack game.
runTurn :: GameStateM ()
runTurn = do
    displayMoney
    readBets
    modify $ dealCards 2
    s <- get
    handlePlayers 0 $ numPlayers s
    displayScores
    handleBets
    modify resetScores
    modify removeBrokePlayers

run :: GameStateM ()
run = whileM_ (fmap ((/= 0) . numPlayers) get) runTurn

class Playable p where
    -- | Returns the player's blackjack score.
    score :: p -> Int
    -- | Returns the number of high aces (with score 11)
    -- that the player can have.
    highAces :: p -> Int
    -- | Returns an action (Hit/Stand/etc).
    decideAction :: p -> IO Action
    -- | Adds a new card to the player.
    addCard :: Card -> p -> p

instance Playable Player where
    score = playerScore
    highAces = playerHighAces
    decideAction p
        | playerScore p >= 21 = return Stand
        | otherwise           = readAction p
    addCard card player = player { playerScore = score', playerHighAces = highAces' }
      where
        (score', highAces') = updateScore card player

instance Playable Dealer where
    score = dealerScore
    highAces = dealerHighAces
    decideAction d
        | dealerScore d >= 17 = return Stand
        | otherwise           = return Hit
    addCard card dealer = dealer { dealerScore = score', dealerHighAces = highAces' }
      where
        (score', highAces') = updateScore card dealer

main :: IO ()
main = do
    gen <- getStdGen
    let initMoney = 100
    let players = const Player
            { playerHighAces = 0
            , playerScore    = 0
            , playerMoney    = initMoney
            } <$> [0..5]
    let dealer = Dealer { dealerHighAces = 0, dealerScore = 0 }
    let gameState = GameState
            { gameDealer  = dealer
            , gamePlayers = players
            , gameDeck    = deck gen
            , gameBets    = []
            }
    execStateT run gameState
    return ()

--
-- Helper functions
--

-- | Attempts to convert a string to a number.
readMaybe :: (Read r) => String -> Maybe r
readMaybe st = case reads st of
    [(x,"")] -> Just x
    _        -> Nothing

-- | Updates a list by replacing the element at the specified index
-- with a new element.
updateList :: Int -> (a -> a) -> [a] -> [a]
updateList index f list = x ++ elem' : ys
  where
    (x, elem:ys) = splitAt index list
    elem' = f elem

-- | Continues to ask the player to input an action until
-- the player enters a valid number indicating the action.
readAction :: Player -> IO Action
readAction p = do
    putStrLn "Enter (1) to Hit, (2) to Stand"
    line <- getLine
    let action = (join . mapM actionFromInput . readMaybe) line
    case action of
        Just action -> return action
        Nothing     -> readAction p
  where
    actionFromInput 1 = Just Hit
    actionFromInput 2 = Just Stand
    actionFromInput _ = Nothing

-- | Continues to ask the player to input the bet amount
-- until the player enters a valid bet amount.
readBet :: Player -> Int -> IO Int
readBet p i = do
    putStrLn $ "Enter the bet amount for player " ++ show i ++ ":"
    line <- getLine
    case (join . mapM check . readMaybe) line of
        Just amount -> return amount
        Nothing     -> readBet p i
  where
    check amount
        | playerMoney p - amount >= 0 = Just amount
        | otherwise                   = Nothing

-- | Returns the best score possible given the number of high aces
-- and the score by converting high aces to low aces only if the total
-- is above 21. This function is also a helper function for updatePlayerScore.
balanceScore :: Int -> Int -> (Int, Int)
balanceScore score highAces
    | score > 21 && highAces > 0 = balanceScore (score - 10) (highAces - 1)
    | otherwise                  = (score, highAces)

-- | Returns the updated score and high aces when adding a card.
updateScore :: (Playable p) => Card -> p -> (Int, Int)
updateScore c p
    | cardValue c == 1 = balanceScore (score p + 11) (highAces p + 1)
    | otherwise        = balanceScore (score p + cardValue c) (highAces p)

-- | Adds/subtracts money to the player flooring at zero.
addMoney :: Int -> Player -> Player
addMoney money p
    | playerMoney p + money >= 0 = p { playerMoney = playerMoney p + money }
    | otherwise                  = p { playerMoney = 0 }

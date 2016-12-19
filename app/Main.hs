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

-- Creates a new card given a suite and a value.
card :: Suite -> Int -> Card
card suite value = Card { cardSuite = suite, cardValue = value }

type Deck = [Card]

-- Creates a standard 52 card deck.
deck :: StdGen -> Deck
deck = shuffle' cards (length cards)
    where cards = card <$> [Hearts, Spades, Clubs, Diamonds] <*> [1..13]

data Action = Stand | Hit deriving (Show)

data Player = Player
    { playerHighAces :: Int
    , playerScore :: Int
    , playerMoney :: Int
    } deriving (Show)

data Dealer = Dealer
    { dealerHighAces :: Int
    , dealerScore :: Int
    } deriving (Show)

data GameState = GameState
    { gameDealer :: Dealer
    , gamePlayers :: [Player]
    , gameDeck :: Deck
    , gameBets :: [Int]
    } deriving (Show)

type GameStateM = StateT GameState IO

numPlayers :: GameStateM Int
numPlayers = state $ \s -> ((length . gamePlayers) s, s)

getPlayers :: GameStateM [Player]
getPlayers = state $ \s -> (gamePlayers s, s)

getDealer :: GameStateM Dealer
getDealer = state $ \s -> (gameDealer s, s)

getBets :: GameStateM [Int]
getBets = state $ \s -> (gameBets s, s)

setBets :: [Int] -> GameStateM ()
setBets bets = state $ \s -> ((), s { gameBets = bets })

-- Asks the players for the bets and sets the bets in the state.
readBets :: GameStateM ()
readBets = do
    players <- getPlayers
    bets <- mapM (\i -> lift $ readBet (players !! i) (i+1)) [0..length players - 1]
    setBets bets

-- Draws a card from the deck and adds it to the player given by the index.
dealCardToPlayer :: Int -> GameStateM ()
dealCardToPlayer i = state $ \s -> let (card:rest) = gameDeck s
                                       players = gamePlayers s
                                   in ((), s { gameDeck = rest
                                             , gamePlayers = updateList i (addCard card) players })

-- Draws a card from the deck and adds it to the dealer.
dealCardToDealer :: GameStateM ()
dealCardToDealer = state $ \s -> let (card:rest) = gameDeck s
                                     dealer = gameDealer s
                                 in ((), s { gameDeck = rest, gameDealer = addCard card dealer })

-- Removes players who have no money.
removeBrokePlayers :: GameStateM ()
removeBrokePlayers = state $ \s -> let players = gamePlayers s
                                       removedBroke = filter ((/= 0) . playerMoney) players
                                    in ((), s { gamePlayers = removedBroke })

-- Adds the given amount of money to the player given by the index.
addMoneyToPlayer :: Int -> Int -> GameStateM ()
addMoneyToPlayer money i = state $ \s -> let players = gamePlayers s
                                         in ((), s { gamePlayers = updateList i (addMoney money) players })

-- Resets all of the player's scores to 0.
resetScores :: GameStateM ()
resetScores = state $ \s -> let players = gamePlayers s
                            in ((), s { gamePlayers = fmap (\p -> p { playerScore = 0 }) players })

-- Deals cards to the dealer and the players.
dealCards :: Int -> GameStateM ()
dealCards num = do
    players <- numPlayers
    let dealIndexes = (\_ b -> b) <$> [1..num] <*> [0..players - 1]
    mapM_ dealCardToPlayer dealIndexes 
    mapM_ (const dealCardToDealer) [1..num]

-- Checks if the player lost or won and either takes or gives money.
handleBet :: [Int] -> Dealer -> Player -> Int -> GameStateM ()
handleBet bets d p i
    | playerScore p > 21 = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " busted"
        addMoneyToPlayer (- (bets !! i)) i
        lift $ putStrLn $ "Player lost " ++ show (bets !! i)
        return ()
    | dealerScore d > 21 = do
        lift $ putStrLn $ "The dealer busted but player " ++ show (i + 1) ++ " didn't bust"
        addMoneyToPlayer (bets !! i) i
        lift $ putStrLn $ "Player received " ++ show (bets !! i)
        return ()
    | playerScore p > dealerScore d = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " had a higher score than the dealer"
        addMoneyToPlayer (bets !! i) i
        lift $ putStrLn $ "Player received " ++ show (bets !! i)
        return ()
    | otherwise = do
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " had a lower or equal score than the dealer"
        addMoneyToPlayer (- (bets !! i)) i
        lift $ putStrLn $ "Player lost " ++ show (bets !! i)
        return ()

-- Displays the player's money.
displayMoney :: GameStateM ()
displayMoney = do
    players <- getPlayers
    forM_ (zip players [0..length players - 1]) (\(p,i) ->
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s money is: " ++ show (playerMoney p))
    return ()

-- Displays the scores of the players and the dealer.
displayScores :: GameStateM ()
displayScores = do
    dealer <- getDealer
    players <- getPlayers
    lift $ putStrLn $ "The dealer's score is: " ++ show (dealerScore dealer)
    forM_ (zip players [0..length players - 1]) (\(p,i) ->
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s score is: " ++ show (playerScore p))
    return ()

-- Handles the bet for every player after a turn.
handleBets :: GameStateM ()
handleBets = do
    dealer <- getDealer
    players <- getPlayers
    bets <- getBets
    forM_ (zip players [0..length players - 1]) (uncurry $ handleBet bets dealer)
    return ()

-- Goes through every player asking for actions and applying them.
-- Will ask for actions multiple times until it gets the Stand action
-- (either automatically or chosen by the player).
handlePlayers :: Int -> Int -> GameStateM ()
handlePlayers i len
    | i < len = do
        players <- getPlayers
        lift $ putStrLn $ "Player " ++ show (i + 1) ++ "'s current score is: " ++ (show . playerScore) (players !! i)
        action <- (lift . decideAction) (players !! i)
        case action of Hit   -> do dealCardToPlayer i; handlePlayers i len
                       Stand -> handlePlayers (i + 1) len
    | otherwise = do
        dealer <- getDealer
        action <- (lift . decideAction) dealer
        lift $ putStrLn $ "Dealer's action is: " ++ show action
        case action of Hit   -> do dealCardToDealer; handlePlayers i len
                       Stand -> return ()

-- Runs a turn in the blackjack game.
runTurn :: GameStateM ()
runTurn = do
    displayMoney
    readBets
    dealCards 2
    players <- numPlayers
    handlePlayers 0 players
    displayScores
    handleBets
    resetScores
    removeBrokePlayers

run :: GameStateM ()
run = whileM_ (fmap ((/= 0) . length) getPlayers) runTurn

class Playable p where
    score :: p -> Int
    highAces :: p -> Int
    decideAction :: p -> IO Action
    addCard :: Card -> p -> p

instance Playable Player where
    score = playerScore
    highAces = playerHighAces
    decideAction p
        | playerScore p >= 21 = return Stand
        | otherwise           = readAction p
    addCard card player = player { playerScore = score', playerHighAces = highAces' }
        where (score', highAces') = updateScore card player

instance Playable Dealer where
    score = dealerScore
    highAces = dealerHighAces
    decideAction d
        | dealerScore d >= 17 = return Stand
        | otherwise           = return Hit
    addCard card dealer = dealer { dealerScore = score', dealerHighAces = highAces' }
        where (score', highAces') = updateScore card dealer

main :: IO ()
main = do
    gen <- getStdGen
    let initMoney = 100
    let players = map (const Player { playerHighAces = 0, playerScore = 0, playerMoney = initMoney }) [0..5]
    let dealer = Dealer { dealerHighAces = 0, dealerScore = 0 }
    let gameState = GameState { gameDealer = dealer, gamePlayers = players, gameDeck = deck gen, gameBets = [] }
    execStateT run gameState
    return ()

--
-- Helper functions
--

-- Attempts to convert a string to a number.
readMaybe :: (Read r) => String -> Maybe r
readMaybe st = case reads st of [(x,"")] -> Just x
                                _        -> Nothing

-- Updates a list by replacing the element at the specified index
-- with a new element.
updateList :: Int -> (a -> a) -> [a] -> [a]
updateList index f list = let (x, elem:ys) = splitAt index list
                              newElem = f elem
                          in x ++ newElem : ys

-- Continues to ask the player to input an action until
-- the player enters a valid number indicating the action.
readAction :: Player -> IO Action
readAction p = do
    putStrLn "Enter (1) to Hit, (2) to Stand"
    line <- getLine
    let action = (join . mapM actionFromInput . readMaybe) line
    case action of Just action -> return action
                   Nothing     -> readAction p
    -- Helper function for returning the correct action from a number.
    where actionFromInput 1 = Just Hit
          actionFromInput 2 = Just Stand
          actionFromInput _ = Nothing

-- Continues to ask the player to input the bet amount
-- until the player enters a valid bet amount.
readBet :: Player -> Int -> IO Int
readBet p i = do
    putStrLn $ "Enter the bet amount for player " ++ show i ++ ":"
    line <- getLine
    case (join . mapM check . readMaybe) line of Just amount -> return amount
                                                 Nothing     -> readBet p i
    where check amount
              | playerMoney p - amount >= 0 = Just amount
              | otherwise                   = Nothing

-- Returns the best score possible given the number of high aces
-- and the score by converting high aces to low aces only if the total
-- is above 21. This function is also a helper function for updatePlayerScore.
balanceScore :: Int -> Int -> (Int, Int)
balanceScore score highAces
    | score > 21 && highAces > 0 = balanceScore (score - 10) (highAces - 1)
    | otherwise                  = (score, highAces)

-- Returns the updated score and high aces when adding a card.
updateScore :: (Playable p) => Card -> p -> (Int, Int)
updateScore c p
    | cardValue c == 1 = balanceScore (score p + 11) (highAces p + 1)
    | otherwise        = balanceScore (score p + cardValue c) (highAces p)

addMoney :: Int -> Player -> Player
addMoney money p
    | playerMoney p + money >= 0 = p { playerMoney = playerMoney p + money }
    | otherwise                  = p { playerMoney = 0 }

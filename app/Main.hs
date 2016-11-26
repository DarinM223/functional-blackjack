module Main where

import Control.Applicative
import Control.Monad
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

-- Draws a card from the deck and adds it to the player given by the index.
dealCardToPlayer :: Int -> StateT GameState IO ()
dealCardToPlayer i = state $ \s -> let (card:rest) = gameDeck s
                                       players = gamePlayers s
                                   in ((), s { gameDeck = rest
                                             , gamePlayers = updateList i (addCard card) players })

-- Draws a card from the deck and adds it to the dealer.
dealCardToDealer :: StateT GameState IO ()
dealCardToDealer = state $ \s -> let (card:rest) = gameDeck s
                                     dealer = gameDealer s
                                 in ((), s { gameDeck = rest, gameDealer = addCard card dealer })

numPlayers :: StateT GameState IO Int
numPlayers = state $ \s -> ((length . gamePlayers) s, s)

getPlayers :: StateT GameState IO [Player]
getPlayers = state $ \s -> (gamePlayers s, s)

getDealer :: StateT GameState IO Dealer
getDealer = state $ \s -> (gameDealer s, s)

getBets :: StateT GameState IO [Int]
getBets = state $ \s -> (gameBets s, s)

-- Adds the given amount of money to the player given by the index.
addMoneyToPlayer :: Int -> Int -> StateT GameState IO ()
addMoneyToPlayer money i = state $ \s -> let players = gamePlayers s
                                         in ((), s { gamePlayers = updateList i (addMoney money) players })

-- Deals cards to the dealer and the players.
dealCards :: Int -> StateT GameState IO ()
dealCards num = do
    players <- numPlayers
    let dealIndexes = (\_ b -> b) <$> [1..num] <*> [0..players - 1]
    mapM_ dealCardToPlayer dealIndexes 
    mapM_ (const dealCardToDealer) [1..num]

-- TODO(DarinM223): clean up ugly code :P
handleBets :: StateT GameState IO ()
handleBets = do
    dealer <- getDealer
    players <- getPlayers
    bets <- getBets
    if dealerScore dealer > 21
        then do
            lift $ putStrLn "Dealer busted"
            forM (zip players [0..length players - 1]) (\(p,i) ->
                if playerScore p <= 21
                    then do
                        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " won"
                        addMoneyToPlayer (bets !! i) i
                        lift $ putStrLn $ "Player received " ++ show (bets !! i)
                        return ()
                    else do
                        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " busted"
                        addMoneyToPlayer (- (bets !! i)) i
                        lift $ putStrLn $ "Player lost " ++ show (bets !! i)
                        return ())
            return ()
        else do
            forM (zip players [0..length players - 1]) (\(p,i) ->
                if playerScore p < (dealerScore dealer)
                    then do
                        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " won"
                        addMoneyToPlayer (bets !! i) i
                        lift $ putStrLn $ "Player received " ++ show (bets !! i)
                        return ()
                    else do
                        lift $ putStrLn $ "Player " ++ show (i + 1) ++ " lost"
                        addMoneyToPlayer (- (bets !! i)) i
                        lift $ putStrLn $ "Player lost " ++ show (bets !! i)
                        return ())
            return ()

-- Runs a turn in the blackjack game.
runTurn :: StateT GameState IO ()
runTurn = do
    dealCards 2
    dealer <- getDealer
    players <- getPlayers
    handlePlayers players 0 dealer
    handleBets
    where handlePlayers (p:ps) i d = do
              action <- (lift . decideAction) p
              case action of Hit   -> dealCardToPlayer i
                             Stand -> return ()
              handlePlayers ps (i + 1) d
          handlePlayers [] _ d = do
              action <- (lift . decideAction) d
              lift $ putStrLn $ "Dealer's action is: " ++ show action
              case action of Hit   -> dealCardToDealer
                             Stand -> return ()

class Playable p where
    score :: p -> Int
    highAces :: p -> Int
    decideAction :: p -> IO Action
    addCard :: Card -> p -> p

instance Playable Player where
    score = playerScore
    highAces = playerHighAces
    decideAction = readAction
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
    bets <- mapM (\i -> readBet (players !! i) (i+1)) [0..5]
    let dealer = Dealer { dealerHighAces = 0, dealerScore = 0 }
    let gameState = GameState { gameDealer = dealer, gamePlayers = players, gameDeck = deck gen, gameBets = bets }
    execStateT runTurn gameState
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
    putStrLn $ "Player's current score is: " ++ (show . playerScore) p
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

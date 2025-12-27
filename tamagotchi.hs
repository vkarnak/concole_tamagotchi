module Main where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)

maxStat :: Int
maxStat = 10

clamp :: Int -> Int
clamp = max 0 . min maxStat

saveFile :: FilePath
saveFile = "pet_save.txt"

data PetType = Cat | Dog | Rabbit | Hamster | Parrot
  deriving (Show, Read)

data Pet = Pet
  { name      :: String
  , petType   :: PetType
  , energy    :: Int
  , happiness :: Int
  , satiety   :: Int
  } deriving (Show, Read)

data Action = Feed | Play | Sleep
  deriving (Show, Read)

data GameState = Alive | Dead String

data Level = Critical | Bad | Okay | Good
  deriving (Eq, Ord)

level :: Int -> Level
level x
  | x >= 7    = Good
  | x >= 5    = Okay
  | x >= 3    = Bad
  | otherwise = Critical

worstLevel :: Pet -> Level
worstLevel p = minimum
  [ level (energy p)
  , level (satiety p)
  , level (happiness p)
  ]

playCost :: PetType -> Int
playCost Dog     = 1
playCost Cat     = 2
playCost Rabbit  = 3
playCost Hamster = 2
playCost Parrot  = 1

hungerDrain :: PetType -> Int
hungerDrain Cat     = 2
hungerDrain Dog     = 1
hungerDrain Rabbit  = 1
hungerDrain Hamster = 2
hungerDrain Parrot  = 1

performAction :: Pet -> Action -> Pet
performAction p Feed =
  p { satiety   = clamp (satiety p + 3)
    , happiness = clamp (happiness p + 1)
    }

performAction p Play =
  p { happiness = clamp (happiness p + 2)
    , energy    = clamp (energy p - playCost (petType p))
    , satiety   = clamp (satiety p - 1)
    }

performAction p Sleep =
  p { energy  = clamp (energy p + 3)
    , satiety = clamp (satiety p - 1)
    }

updateNeeds :: Pet -> Pet
updateNeeds p = p
  { satiety   = clamp (satiety p - hungerDrain (petType p))
  , happiness = clamp (happiness p - 1)
  , energy    = clamp (energy p - if satiety p < 5 then 1 else 0)
  }

petWarnings :: Pet -> [String]
petWarnings p =
  concat
    [ if level (energy p) <= Bad    then ["Tired"]   else []
    , if level (satiety p) <= Bad   then ["Hungry"]  else []
    , if level (happiness p) <= Bad then ["Sad"]     else []
    ]

petComment :: Pet -> String
petComment p
  | null (petWarnings p) = "I feel fine."
  | otherwise            = "Something feels wrong..."

checkState :: Pet -> GameState
checkState p
  | satiety p == 0   = Dead "Your pet starved to death."
  | energy p == 0    = Dead "Your pet collapsed from exhaustion."
  | happiness p == 0 = Dead "Your pet died of sadness."
  | otherwise        = Alive

catFace :: Level -> String
catFace Good     = " ^.^ "
catFace Okay     = " ^.- "
catFace Bad      = " o.o "
catFace Critical = " T.T "

rabbitFace :: Level -> String
rabbitFace Good     = " ^_^ "
rabbitFace Okay     = " ^_^; "
rabbitFace Bad      = " o_o "
rabbitFace Critical = " x_x "

dogEye :: Level -> String
dogEye Good     = "@"
dogEye Okay     = "-"
dogEye Bad      = "o"
dogEye Critical = "x"

hamsterFace :: Level -> String
hamsterFace Good     = " ^_^ "
hamsterFace Okay     = " ^.- "
hamsterFace Bad      = " o.o "
hamsterFace Critical = " x_x "

parrotEye :: Level -> String
parrotEye Good     = "o"
parrotEye Okay     = "-"
parrotEye Bad      = "O"
parrotEye Critical = "x"

showPetAscii :: PetType -> Level -> IO ()
showPetAscii Cat lvl = putStrLn $
  " /\\_/\\ \n" ++
  "(" ++ catFace lvl ++ ")\n" ++
  " > ^ <"

showPetAscii Dog lvl = putStrLn $
  "  / \\__\n" ++
  " (    " ++ dogEye lvl ++ "\\___\n" ++
  " /         O"

showPetAscii Rabbit lvl = putStrLn $
  " (\\_/)\n" ++
  " (" ++ rabbitFace lvl ++ ")\n" ++
  " /> <\\"

showPetAscii Hamster lvl = putStrLn $
  " (\\_._/)\n" ++
  " (" ++ hamsterFace lvl ++ ")\n" ++
  "  (\")(\")"

showPetAscii Parrot lvl = putStrLn $
  "  \\\\\n" ++
  "  (" ++ parrotEye lvl ++ ">\n" ++
  "  //\\\n" ++
  " _V_/__\n" ++
  "    ||\n" ++
  "    ||"

savePet :: Pet -> IO ()
savePet pet = writeFile saveFile (show pet)

loadPet :: IO (Maybe Pet)
loadPet = do
  exists <- doesFileExist saveFile
  if exists
    then do
      content <- readFile saveFile
      return $ Just (read content)
    else return Nothing

gameLoop :: Pet -> IO ()
gameLoop pet = do
  case checkState pet of
    Dead msg -> do
      putStrLn "\nGame Over!"
      putStrLn msg
      writeFile saveFile ""

    Alive -> do
      let lvl = worstLevel pet

      putStrLn $ "\nYour pet: " ++ name pet ++ " (" ++ show (petType pet) ++ ")"
      putStrLn $
        "Energy: " ++ show (energy pet) ++
        " | Satiety: " ++ show (satiety pet) ++
        " | Happiness: " ++ show (happiness pet)

      putStrLn $ "Status: " ++ petComment pet

      let warns = petWarnings pet
      if not (null warns) then do
        putStrLn "Warnings:"
        mapM_ (\w -> putStrLn ("- " ++ w)) warns
      else return ()

      showPetAscii (petType pet) lvl

      savePet pet

      putStrLn "\nChoose action:"
      putStrLn "1 - Feed"
      putStrLn "2 - Play"
      putStrLn "3 - Sleep"
      putStrLn "4 - Quit"
      putStr "> "
      hFlush stdout

      choice <- getLine

      case choice of
        "1" -> gameLoop (updateNeeds (performAction pet Feed))
        "2" -> gameLoop (updateNeeds (performAction pet Play))
        "3" -> gameLoop (updateNeeds (performAction pet Sleep))
        "4" -> do
          putStrLn $ "Bye! " ++ name pet ++ " will miss you!"
          savePet pet
        _   -> do
          putStrLn "Invalid choice!"
          gameLoop pet

choosePet :: IO Pet
choosePet = do
  mPet <- loadPet
  case mPet of
    Just pet -> do
      putStrLn $ "Loaded saved pet: " ++ name pet ++ " (" ++ show (petType pet) ++ ")"
      return pet
    Nothing -> do
      putStrLn "Choose your pet type:"
      putStrLn "1 - Cat"
      putStrLn "2 - Dog"
      putStrLn "3 - Rabbit"
      putStrLn "4 - Hamster"
      putStrLn "5 - Parrot"
      putStr "> "
      hFlush stdout

      choice <- getLine
      let chosenType = case choice of
            "1" -> Cat
            "2" -> Dog
            "3" -> Rabbit
            "4" -> Hamster
            "5" -> Parrot
            _   -> Cat

      putStrLn "\nEnter a name for your pet:"
      putStr "> "
      hFlush stdout
      petName <- getLine

      return $ Pet petName chosenType 7 7 7

main :: IO ()
main = do
  putStrLn "Welcome to Console Tamagotchi!"
  pet <- choosePet
  gameLoop pet

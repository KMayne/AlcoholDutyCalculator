import Data.Char
import Data.List
import Text.Printf

data Beverage = Beer | Cider | Perry | Spirit | Wine deriving (Enum, Show, Bounded)
data Carbonation = Still | Sparkling deriving (Enum, Show, Bounded)

main :: IO ()
main = do
  beverage <- userSelectFromList "beverage type" beverages
  putStrLn "What is the ABV in %?"
  abv <- fmap read getLine
  putStrLn "What is the volume in cL?"
  vol <- fmap read getLine
  duty <- beverageDuty beverage abv vol
  printf "The duty on a %.1v cL %.2v%% bottle of %s is £%.2v\n" vol abv (map toLower (show beverage)) duty
  where
    beverages = [minBound :: Beverage ..]

beverageDuty :: Beverage -> Double -> Double -> IO Double
beverageDuty Perry  = beverageDuty Cider
beverageDuty Beer   = return .: beerDuty
beverageDuty Cider  = co2BeverageDuty ciderDuty
beverageDuty Spirit = return .: spiritDuty
beverageDuty Wine   = co2BeverageDuty wineDuty

co2BeverageDuty :: (Carbonation -> Double -> Double -> Double) -> Double -> Double -> IO (Double)
co2BeverageDuty dutyFunc abv vol = do
  carbonation <- userSelectFromList "carbonation option" carbonations
  return (dutyFunc carbonation abv vol)
  where
    carbonations = [minBound :: Carbonation ..]

beerDuty :: Double -> Double -> Double
beerDuty abv
  | abv > 7.5 = hectoLitreDuty (18.37 * abv)
  | abv > 2.8 = hectoLitreDuty ( 5.48 * abv)
  | abv > 1.2 = hectoLitreDuty ( 8.10 * abv)

ciderDuty :: Carbonation -> Double -> Double -> Double
ciderDuty Still abv
  | abv > 8.5 = error "Abv out of range of known rules"
  | abv > 7.5 = hectoLitreDuty 58.75
  | abv > 1.2 = hectoLitreDuty 38.87
ciderDuty Sparkling abv
  | abv > 8.5 = hectoLitreDuty 268.99
  | abv > 1.2 = hectoLitreDuty  38.87

wineDuty :: Carbonation -> Double -> Double -> Double
wineDuty Still abv
  | abv > 22  = spiritDuty abv
  | abv > 15  = hectoLitreDuty 370.41
  | abv > 5.5 = hectoLitreDuty 277.84
  | abv > 4   = hectoLitreDuty 117.72
  | abv > 1.2 = hectoLitreDuty  85.60
wineDuty Sparkling abv
  | abv > 5.5 && abv < 8.5 = hectoLitreDuty 268.99
  | abv >= 8.5 && abv < 15  = hectoLitreDuty 268.99
  | otherwise = wineDuty Still abv

hectoLitreDuty :: Double -> Double -> Double
hectoLitreDuty ratePerHectoLitre amountInCentiLitres
  = amountInCentiLitres / centilitersInHectoLitre * ratePerHectoLitre
  where
    centilitersInHectoLitre = 100 * 100

spiritDuty :: Double -> Double -> Double
spiritDuty abv volume
  = volume / centilitresPerLitre * abv / 100 * ratePerLitreOfEthanol
  where
    centilitresPerLitre   = 100
    ratePerLitreOfEthanol = 27.66

userSelectFromList :: (Show a, Enum a) => String -> [a] -> IO a
userSelectFromList listItemName list = do
  putStrLn ("Select a " ++ listItemName ++ ":")
  -- Print options
  mapM putStrLn ((makeOptionStrings . map show) list)
  -- Get a choice from the user
  item <- validPrompt ("Please type an option from A to " ++ [lastOption]) validInput
  return ((toEnum . getOptNum) item)
  where
    validInput input = length input /= 0 &&
      let option = getOptNum input in 0 <= option && option < length list
    getOptNum input = (ord . toUpper . head) input - ord 'A'
    lastOption = chr (ord 'A' + length list - 1)
    makeOptionStrings = (zipWith (\letter opt -> letter:") " ++ opt) ['A'..'Z'])

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f $ g x y

validPrompt :: String -> (String -> Bool) -> IO String
validPrompt message validateInput = do
  putStrLn message
  input <- getLine
  if validateInput input
    then return input
    else validPrompt message validateInput
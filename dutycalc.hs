import Data.Char
import Text.Printf

data Beverage = Beer | Cider | Perry | Spirit | Wine deriving (Enum, Show, Bounded)
data Carbonation = Still | Sparkling deriving (Enum, Show, Bounded)

main :: IO ()
main = do
  beverage <- requestBeverageType
  putStrLn "What is the ABV in %?"
  abv <- fmap read getLine
  putStrLn "What is the volume in cL?"
  vol <- fmap read getLine
  duty <- beverageDuty beverage abv vol
  printf "The duty on a %.1v cL %.2v%% bottle of %s is £%.2v" vol abv (map toLower (show beverage)) duty

requestBeverageType :: IO Beverage
requestBeverageType = userSelectFromList "beverage type" beverages
  where
    beverages = [minBound :: Beverage ..]

userSelectFromList :: (Show a, Enum a) => String -> [a] -> IO a
userSelectFromList listItemName list = do
  putStrLn ("Select a " ++ listItemName ++ ":")
  (sequence_ . generateOptionListPrints) list
  option <- fmap ((flip (-) (ord 'A')) . ord . toUpper . (flip (!!) 0)) getLine
  if option >= 0 && option < length list
    then return (toEnum option)
    else do
      putStrLn ("Please type an option from A to " ++ [lastOption])
      userSelectFromList listItemName list
  where
    lastOption = chr (ord 'A' + length list)

generateOptionListPrints :: Show a => [a] -> [IO ()]
generateOptionListPrints
  = (map putStrLn) . makeOptionString . (map show)
  where
    makeOptionString = (zipWith (\letter opt -> letter:") " ++ opt) ['A'..'Z'])

beverageDuty :: Beverage -> Double -> Double -> IO Double
beverageDuty Beer   abv vol = return $ beerDuty abv vol
beverageDuty Cider  abv vol = do f <- (co2BeverageDuty ciderDuty); return $ f abv vol
beverageDuty Perry  abv vol = beverageDuty Cider abv vol
beverageDuty Spirit abv vol = return $ spiritDuty abv vol
beverageDuty Wine   abv vol = do f <- (co2BeverageDuty wineDuty);  return $ f abv vol

co2BeverageDuty :: (Carbonation -> Double -> Double -> Double) -> IO (Double -> Double -> Double)
co2BeverageDuty dutyFunc = do
  carbonation <- userSelectFromList "carbonation option" carbonations
  return (dutyFunc carbonation)
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

spiritDuty :: Double -> Double -> Double
spiritDuty abv volume
  = volume / 100 * abv / 100 * ratePerLitreOfEthanol
  where
    ratePerLitreOfEthanol = 27.66

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
  = amountInCentiLitres / 10000 * ratePerHectoLitre

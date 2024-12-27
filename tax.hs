import Text.Printf (printf)

calculateTaxOld :: Float -> Float
calculateTaxOld income
  | income <= 250000 = 0
  | income <= 500000 = 0.05 * (income - 250000)
  | income <= 1000000 = 12500 + 0.2 * (income - 500000)
  | otherwise = 112500 + 0.3 * (income - 1000000)

calculateTaxNew :: Float -> Float
calculateTaxNew income
  | income <= 250000 = 0
  | income <= 500000 = 0.05 * (income - 250000)
  | income <= 750000 = 12500 + 0.1 * (income - 500000)
  | income <= 1000000 = 37500 + 0.15 * (income - 750000)
  | income <= 1250000 = 75000 + 0.2 * (income - 1000000)
  | income <= 1500000 = 125000 + 0.25 * (income - 1250000)
  | otherwise = 187500 + 0.3 * (income - 1500000)

main :: IO ()
main = do
  putStrLn "Enter your annual income:"
  income <- readLn
  putStrLn "Choose tax regime (1 for Old, 2 for New):"
  regime <- readLn
  let tax = if regime == (1 :: Int)
            then calculateTaxOld income
            else calculateTaxNew income
  printf "Your tax amount is: %.2f\n" tax



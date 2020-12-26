import Data.List
import Control.Arrow
import qualified Data.Map as M

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    putStrLn $ solve' input
    
type Ingredient = String
type Product = [Ingredient]
type Allergen = String
type Input = [(Product, [Allergen])]

parse :: String -> Input
parse = lines >>> map parseLine
parseLine = words >>> break (=="(contains") >>> second (tail >>> map (filter (not . (`elem` "),"))))

solve input = length $ filter (not . (`elem` ingredientsPossiblyAllergic)) allIngredients where
    allIngredients = concat $ map fst input
    ingredientsPossiblyAllergic = nub $ concat $ map snd (ingredientAllergens input)

ingredientAllergens :: Input -> [(Allergen, [Ingredient])]
ingredientAllergens input = map (productsWithAllergen input >>> second possibleIngredientsWithAllergen) (allergens input)

solve' = intercalate "," . map snd . sort . filterAllergens . ingredientAllergens 

possibleIngredientsWithAllergen :: [Product] -> [Ingredient]
possibleIngredientsWithAllergen products = calc products where
    calc = concat >>> map (\ing -> (ing,1)) >>> M.fromListWith (+) >>> M.toList >>> filter ((==n).snd) >>> map fst 
    n = length products

productsWithAllergen :: Input -> Allergen -> (Allergen, [Product])
productsWithAllergen input allergen = (,) allergen $ map fst $ filter (elem allergen . snd) input

allergens :: Input -> [Allergen]
allergens = nub . concat . map snd

-- Same algorithm as 'filterRules' from day 16
filterAllergens :: [(Allergen, [Ingredient])] -> [(Allergen, Ingredient)]
filterAllergens list = if length singles == length list then map (second head) list else filterAllergens reduced where
    reduced = map (second removeSingles) list
    singles = concat $ filter ((== 1) . length) $ map snd list
    removeSingles ingredients = if length ingredients == 1 then ingredients else filter (not . (`elem` singles)) ingredients

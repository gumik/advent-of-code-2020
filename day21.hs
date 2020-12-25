import Data.List
import Control.Arrow
import qualified Data.Map as M

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input

type Ingredient = String
type Product = [Ingredient]
type Allergen = String
type Input = [(Product, [Allergen])]

parse :: String -> Input
parse = lines >>> map parseLine
parseLine = words >>> break (=="(contains") >>> second (tail >>> map (filter (not . (`elem` "),"))))

solve input = length $ filter (not . (`elem` ingredientsPossiblyAllergic)) allIngredients where
    allIngredients = concat $ map fst input
    ingredientsPossiblyAllergic = nub $ concat $ map snd ingredientAllergens
    ingredientAllergens= map (productsWithAllergen input >>> second possibleIngredientsWithAllergen) (allergens input)

possibleIngredientsWithAllergen :: [Product] -> [Ingredient]
possibleIngredientsWithAllergen products = calc products where
    calc = concat >>> map (\ing -> (ing,1)) >>> M.fromListWith (+) >>> M.toList >>> filter ((==n).snd) >>> map fst 
    n = length products
productsWithAllergen :: Input -> Allergen -> (Allergen, [Product])
productsWithAllergen input allergen = (,) allergen $ map fst $ filter (elem allergen . snd) input

allergens :: Input -> [Allergen]
allergens = nub . concat . map snd
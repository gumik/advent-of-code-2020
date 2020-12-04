import Data.Char
import Data.List.Extra
import qualified Data.Map as M
import Data.Map (member, fromList, (!))
import Text.Printf
import Text.Regex.Posix

main = do
    contents <- getContents
    let input = parse contents
    print $ solve input
    print $ solve' input

parse = map makeMap . map splitDocument . splitDocuments where
    makeMap = M.map (drop 1) . fromList . map (break (== ':'))
    splitDocument = filter (/= "") . splitOn " " . replace "\n" " "
    splitDocuments = splitOn "\n\n"

solve = length . documentsWithRequiredFields

solve' = length . documentsWithValidFields . documentsWithRequiredFields

documentsWithRequiredFields = filter hasRequiredFields where
    hasRequiredFields doc = all (`member` doc) requiredFields
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

documentsWithValidFields = filter isValidDocument where
    isValidDocument doc = and [ isValidNumber (doc ! "byr") 1920 2002
                              , isValidNumber (doc ! "iyr") 2010 2020
                              , isValidNumber (doc ! "eyr") 2020 2030
                              , isValidHeight (doc ! "hgt")
                              , isValidHairColor (doc ! "hcl")
                              , isValidEyeColor (doc ! "ecl")
                              , isValidPassportNumber (doc ! "pid")
                              ]

isValidNumber str min max = all isNumber str && num >= min && num <= max where
   num = read str :: Int

isValidHeight str = case unit of
        "cm" -> isValidNumber numStr 150 193
        "in" -> isValidNumber numStr 59 76
        _ -> False
    where
        (numStr, unit) = splitAt (length str - 2) str

isValidHairColor :: String -> Bool
isValidHairColor str = str =~ "^#[0-9a-f]{6}$"

isValidEyeColor str = str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

isValidPassportNumber :: String -> Bool
isValidPassportNumber str = str =~ "^[0-9]{9}$"

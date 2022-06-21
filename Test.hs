--------------------- BASICS TESTING ----------------------------
a = [1, 2, 3]
b = [4, 5, 6]
showTest = show a
-- main identifier is restricted
myMain = showTest


------------------ FUNCTION SYNTAX TESTING -----------------------
tellWhatNumber :: (Integral a) => a -> String --typeclasses ad-hoc
tellWhatNumber 2 = "Two"
tellWhatNumber 5 = "Five"
tellWhatNumber num = "That's not 2 or 5. idk"

fu :: (Int, Int) -> (Int, Int) -> (Int, Int)
fu (x1, y1) (x2, y2)
    | (x_sum < 0) && (y_sum < 0) = ((-1) * x_sum, (-1) * y_sum)
    | (x_sum < 0) && (y_sum >= 0) = ((-1) * x_sum, y_sum)
    | (x_sum >= 0) && (y_sum < 0) = (x_sum, (-1) * y_sum)
    | otherwise = (x_sum, y_sum)
    where x_sum = x1 + x2
          y_sum = y1 + y2


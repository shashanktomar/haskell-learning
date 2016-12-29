-- length of a list
length xs = sum [1 | _ <- xs]

-- which right triangle that has integers for all sides and all sides equal
-- to or smaller than 10 has a perimeter of 24? First, let's try generating
-- all triangles with sides equal to or smaller than 10
triangle = [(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], a^2 + b^2 == c^2, a+b+c==24]

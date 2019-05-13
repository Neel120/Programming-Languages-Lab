-- This function returns the number of parathas that can be bought by spending the given amount of tokens and the given cost. 
spendToken::Int->Int->Int
spendToken x y = if x >= y
                 then ((div x y) + (spendToken ((mod x y) + (div x y)) y))
                 else 0

 -- This function returns the number of parathas that can be bought given initial money and cost of a paratha in terms of money and tokens. 
countParathas::Int->Int->Int->Int
countParathas x y z = (div x y) + (spendToken (div x y) z)
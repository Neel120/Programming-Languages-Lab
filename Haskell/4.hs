allBedDims = [[x,x] | x <- [10..15]]
allHallDims = [[x,x-5] | x <- [15..20]]
allKitchDims = [[x,x-2] | x <- [7..15]]
allBathDims = [[x,x+1] | x <- [4..8]]
allBalDims = [[x,x] | x<-[5..10]]
allGarDims = [[x,x] | x<-[10..20]]
allDims = filter (\x -> x!!2 < x!!0 && x!!2 < x!!1 && x!!3 < x!!2) [[x,y,z,w,u,v] | x <- allBedDims, y <- allHallDims, z <- allKitchDims, w <- allBathDims, u <- allBalDims, v <- allGarDims]

-- Thisfunctionreturnsthenumberofkitchens requiredgiventhenumberofbedrooms.Thisfunctioncan be modiﬁed to reﬂect change in constraint. 
getKitchen::Integer->Integer
getKitchen x = div (x + 2) 3

 -- This function returns the number of bathrooms given the number of bedrooms. This function can be modiﬁed to reﬂect change in constraint.
getBathroom::Integer->Integer
getBathroom x = x + 1

 -- This function takes an Integer and a list of integers, and return the product of the Integer and the ﬁrst two Integers in the list.
multiall::Integer->[Integer]->Integer
multiall r dims = r*(dims!!0)*(dims!!1)

 -- This function takes the area and the required number of rooms and returns the possible set of dimensions satisfying all of the given criteria except the criteria to optimize unused space. 
addInfo::Integer->[Integer]->[[[Integer]]]
addInfo area numrooms = filter(\x -> (x!!0!!0) >= 0) [ [area - (sum (zipWith (multiall) numrooms x))] : numrooms : x | x<-allDims]

--  This function returns all the possible dimensions given the area and required bedroom and hall quantity. 
getAll::Integer->Integer->Integer->[[[Integer]]]
getAll area bed hall = addInfo area (bed:hall:(getKitchen bed):(getBathroom bed):1:[1])

-- This function gets the minimum value at the 0(th) index of a list of list of list of Integers.
getLowest::[[[Integer]]]->Integer
getLowest list = minimum (map (\x->x!!0!!0) list)

 -- This function returns the set of possible dimensions which have the best unused space. 
getMin::[[[Integer]]]->[[[Integer]]]
getMin list = filter (\r -> r!!1!!0==r!!0!!0) [ [z] : x | x <- list]
              where z = getLowest list

 -- A wrapper around getMin for easier readability. 
getBest::Integer->Integer->Integer->[[[Integer]]]
getBest area bednum hallnum = getMin (getAll area bednum hallnum)

 -- A function the returns the expected string given the maximum area allowed, and the requirements for the number of bedrooms and halls. 
design::Integer->Integer->Integer->[Char]
design area bednum hallnum = if (length (getBest area bednum hallnum)) > 0
                             then "Bedroom: " ++ show ((getBest area bednum hallnum)!!0!!2!!0) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!3!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!3!!1) ++") ---- " ++
                                  "Hall: " ++ show ((getBest area bednum hallnum)!!0!!2!!1) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!4!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!4!!1) ++") ---- " ++
                                  "Kitchen: " ++ show ((getBest area bednum hallnum)!!0!!2!!2) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!5!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!5!!1) ++") ---- " ++
                                  "Bathroom: " ++ show ((getBest area bednum hallnum)!!0!!2!!3) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!6!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!6!!1) ++") ---- " ++
                                  "Garden: " ++ show ((getBest area bednum hallnum)!!0!!2!!4) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!7!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!7!!1) ++") ---- " ++
                                  "Balcony: " ++ show ((getBest area bednum hallnum)!!0!!2!!5) ++ " (" ++ show ((getBest area bednum hallnum)!!0!!8!!0) ++ "x" ++ show ((getBest area bednum hallnum)!!0!!8!!1) ++") ---- " ++
                                  "Unused Space: " ++ show ((getBest area bednum hallnum)!!0!!0!!0)
                             else "No design possible for the given constraints"
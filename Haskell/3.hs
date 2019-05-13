--  This function returns the number of moves required to make all the salaries equal. 
getMoves::[Int]->Int
getMoves x = (sum x) - ((length x) * (minimum x))
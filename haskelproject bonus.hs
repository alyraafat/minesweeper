type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)


getSize :: [Cell] -> Int
getSize ((a,b):xs)	| a >= b = getSizeHelper xs a 
					| otherwise = getSizeHelper xs b

getSizeHelper :: [Cell] -> Int -> Int
getSizeHelper [] max = max + 1
getSizeHelper ((a,b):xs) max | a >= b && a >= max = getSizeHelper xs a 
							 | b >= a && b >= max = getSizeHelper xs b
							 | otherwise = getSizeHelper xs max

up:: MyState -> Int ->  MyState

up  (S (a,b) l state p) size | a == 0 = Null
							 | otherwise = (S ((a-1),b) l  "up" (S (a,b) l state p))
down:: MyState -> Int -> MyState

down  (S (a,b) l state p) size | a == size - 1 = Null
							   | otherwise = (S ((a+1),b) l  "down" (S (a,b) l state p))

left:: MyState -> Int -> MyState

left  (S (a,b) l state p) size | b == 0 = Null
							   | otherwise = (S (a,(b-1)) l  "left" (S (a,b) l state p))


right:: MyState -> Int -> MyState

right  (S (a,b) l state p) size | b == size - 1 = Null
							    | otherwise = (S (a,(b+1)) l  "right" (S (a,b) l state p))

collect:: MyState -> MyState

collectHelper (a,b) [] = True
collectHelper (a,b) (x:xs)	| (a==c && b==d) = False 
							| otherwise = collectHelper (a,b) xs where (c,d) = x
							
removeFromList (a,b) [] = []
removeFromList (a,b) (x:xs)	| (a==c && b==d) = removeFromList (a,b) xs 
							| otherwise = x : removeFromList (a,b) xs	where (c,d) = x
							
							
collect (S (a,b) l state p)	| (collectHelper (a,b) l) = Null
							| otherwise = (S (a,b) (removeFromList (a,b) l)  "collect" (S (a,b) l state p))
							

nextMyStates:: MyState -> Int -> [MyState]

nextMyStates (S (a,b) l state p) size = upHelper (S (a,b) l state p) size

upHelper (S (a,b) l state p) size	| up (S (a,b) l state p) size == Null = downHelper (S (a,b) l state p) size
									| otherwise = up(S (a,b) l state p) size : downHelper (S (a,b) l state p) size
								
downHelper (S (a,b) l state p) size	| down (S (a,b) l state p) size == Null = leftHelper (S (a,b) l state p) size
								    | otherwise = down(S (a,b) l state p) size : leftHelper (S (a,b) l state p) size
								
leftHelper (S (a,b) l state p) size	| left (S (a,b) l state p) size == Null = rightHelper (S (a,b) l state p) size
								    | otherwise = left(S (a,b) l state p) size : rightHelper (S (a,b) l state p) size
								
rightHelper (S (a,b) l state p)	size | right (S (a,b) l state p) size == Null = collectHelper2 (S (a,b) l state p) size
								     | otherwise = right(S (a,b) l state p) size : collectHelper2 (S (a,b) l state p) size


collectHelper2 (S (a,b) l state p) size	| collect (S (a,b) l state p) == Null = []
									    | otherwise = collect(S (a,b) l state p) : []								
isGoal :: MyState -> Bool

isGoal (S (a,b) l state p)	| length l == 0 = True
							| otherwise = False
							
search :: [MyState] -> Int -> MyState	

search (x:xs) size	| isGoal x = x
				    | otherwise = search (xs ++ nextMyStates x size) size

constructSolution :: MyState ->[String]

constructSolution (S (a,b) l state p) 
	| state /="" = constructSolution p++[state]
	| otherwise = []


solve :: Cell ->[Cell] -> [String]

solve (a,b) l = constructSolution (search [S (a,b) l "" Null] (getSize ((a,b):l)))  

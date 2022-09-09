type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show,Eq)

up:: MyState -> MyState

up  (S (0,b) l state p) = Null
up  (S (a,b) l state p) = (S ((a-1),b) l  "up" (S (a,b) l state p))

down:: MyState -> MyState

down  (S (3,b) l state p) = Null
down  (S (a,b) l state p) = (S ((a+1),b) l  "down" (S (a,b) l state p))

left:: MyState -> MyState

left (S (a,0) l state p) = Null
left (S (a,b) l state p) = (S (a,(b-1)) l  "left" (S (a,b) l state p))


right:: MyState -> MyState

right (S (a,3) l state p) = Null
right (S (a,b) l state p) = (S (a,(b+1)) l  "right" (S (a,b) l state p))

collect:: MyState -> MyState

collectHelper (a,b) [] = True
collectHelper (a,b) (x:xs)	| (a==c && b==d) = False 
							| otherwise = collectHelper (a,b) xs where (c,d) = x
							
removeFromList (a,b) [] = []
removeFromList (a,b) (x:xs)	| (a==c && b==d) = removeFromList (a,b) xs 
							| otherwise = x : removeFromList (a,b) xs	where (c,d) = x
							
							
collect (S (a,b) l state p)	| (collectHelper (a,b) l) = Null
							| otherwise = (S (a,b) (removeFromList (a,b) l)  "collect" (S (a,b) l state p))
							

nextMyStates:: MyState -> [MyState]

nextMyStates (S (a,b) l state p) = upHelper (S (a,b) l state p)

upHelper (S (a,b) l state p)	| up (S (a,b) l state p) == Null = downHelper (S (a,b) l state p)
								| otherwise = up(S (a,b) l state p) : downHelper (S (a,b) l state p)
								
downHelper (S (a,b) l state p)	| down (S (a,b) l state p) == Null = leftHelper (S (a,b) l state p)
								| otherwise = down(S (a,b) l state p) : leftHelper (S (a,b) l state p)
								
leftHelper (S (a,b) l state p)	| left (S (a,b) l state p) == Null = rightHelper (S (a,b) l state p)
								| otherwise = left(S (a,b) l state p) : rightHelper (S (a,b) l state p)
								
rightHelper (S (a,b) l state p)	| right (S (a,b) l state p) == Null = collectHelper2 (S (a,b) l state p)
								| otherwise = right(S (a,b) l state p) : collectHelper2 (S (a,b) l state p)


collectHelper2 (S (a,b) l state p)	| collect (S (a,b) l state p) == Null = []
									| otherwise = collect(S (a,b) l state p) : []								
isGoal :: MyState -> Bool

isGoal (S (a,b) l state p)	| length l == 0 = True
							| otherwise = False
							
search :: [MyState] -> MyState	

search (x:xs)	| isGoal x = x
				| otherwise = search (xs ++ nextMyStates x)

constructSolution :: MyState ->[String]

constructSolution (S (a,b) l state p) 
	| state /="" = constructSolution p++[state]
	| otherwise = []


solve :: Cell ->[Cell] -> [String]

solve (a,b) l = constructSolution (search [S (a,b) l "" Null])  


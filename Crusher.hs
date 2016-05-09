-- CPSC 312 - Project 2
-- by Khurram Ali Jaffery



-- Team Name: BUCS BOYZ

-- Name: Yi Xi Wang
-- Student Number: 32090136
-- csid: x4w8 

-- Name: Clarence
-- Student Number: 41436130
-- csid: f8w8

-- Name: John Yoo
-- Student Number: 39296132
-- csid: m3c9 






-- Main Components:
-- minimax algorithm
-- a board evaluator
-- state search
-- movement generators (and by extension, tree generator, new state generator)
-- crusher
-- custom data types (already done)

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--


--NOTE
--we believe that
data Piece = D | W | B deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- Some test results to see what functions are producing 
--
run = crusher ["W------------BB-BBB","----W--------BB-BBB","-W-----------BB-BBB"] 'W' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = sTrToBoard "WWW-WW-------BB-BBB"
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 4 3
heuristic0 = boardEvaluator W [] 3

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

-- crusher calls statesearch to produce the next best board possible
-- converted statesearch board output to string output through helper function boardToStr
-- converted String input (current:old) to Board output for stateSearch arguements board and boardhistory
-- generated hexagon grid g by calling generateGrid (n)(n-1)(2*(n-1))
-- generated all possible slides and leaps by calling generateSlides and generateLeaps on on grid g
-- converted char input to output piece p through charToPiece
-- this produces an error but we don't have time to fix it.

crusher :: [String] -> Char -> Int -> Int -> String
crusher (current:old) p d n = boardToStr (stateSearch (sTrToBoard current listStrToBoard old g (generateSlides g) (generateLeaps g) (charToPiece p) d n))
 where 
 	g = generateGrid n (n-1) (2 * (n - 1)) []

-- Take in a list of strings and converts it to a board
listStrToBoard :: [String] -> Board
listStrToBoard old = map (\ x -> sTrToBoard x) old

--Converts a char to a player piece
charToPiece :: Char -> Piece 
charToPiece c = check c
 where
  check 'W' = W
  check 'B' = B
  check '-' = D

--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False

-- Checks the following condition for game over
-- 1) if the current board is present in the list of boards produce True
-- 2) if the number of white or black pieces is less than the dimensions of the board produce True
-- Otherwise produce False

gameOver::Board->[Board]->Int->Bool
gameOver board history n
 |checkHistory board history = True
 |mylength board W<n = True
 |mylength board B<n = True
 |otherwise = False

-- Counts the sum of pieces in a given board
mylength :: Board -> Piece-> Int
mylength l piece
 | null l = 0
 | head l == piece = 1 + mylength (tail l) piece
 | otherwise = mylength (tail l) piece

-- check is current board is part of the list of boards
checkHistory :: Board -> [Board] -> Bool
checkHistory board history
 | any (board==) history = True
 | otherwise = False
--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
-- 	     [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
	where 
		check 'W' = W
		check 'B' = B
		check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B] 
-- 	     to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board 
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
	where 
		check W = 'W'
		check B = 'B'
		check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid 
--		   initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--		 [(0,0),(1,0),(2,0)
--		  (0,1),(1,1),(2,1),(3,1)
--		  (0,2),(1,2),(2,2),(3,2),(4,2)
--		  (0,3),(1,3),(2,3),(3,3)
--		  (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
	| n3 == -1		= acc
	| otherwise 	= generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
		where
			row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
			nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--

--generateSlides uses list comprehension to output a list of slides
--first, it calls generateSlides_helper recursively to get the list of unbounded slides given the point, then filters out invalid slides 
generateSlides :: Grid -> Grid -> Int -> [Slide]
generateSlides b b1 n 
 | null b = []
 | otherwise = generateFilteredSlides(appendSlides([x |x<-generateSlides_helper b n, fst (head b)>=0, snd (head b)>=0, fst (head b)<=(n+1), snd (head b)<=(n+1)]:[generateSlides(tail b) b1 n])) b1

--appendSlides is one of the helper functions for generateSlides
--it simply appends the recursive output of the list comprehension structure (as shown in generateSlides) so we don't end up with a list of list of slides
appendSlides :: [[Slide]] -> [Slide]
appendSlides lx
 | null lx = []
 | otherwise = head lx ++ appendSlides (tail lx)

--generateFilteredSlides is aptly named
--it takes in the unbounded list of slides and makes sure the new list of slides has only valid slides
--it also takes in an untouched grid as a comparing guideline when checking for invalid values, which would be out of the grid
generateFilteredSlides :: [Slide] -> Grid -> [Slide]
generateFilteredSlides los b1 = filter (isValid b1) los

--the filtering function for generateFilteredSlides 
-- checks second point of grid using elem
isValid :: Grid -> Slide -> Bool
isValid g (x,y) = elem y g


--the helper function for generateSlides simply takes in a grid and n sides, and outputs a list of slides based on the grid position
generateSlides_helper :: Grid ->Int-> [Slide]
generateSlides_helper b n
 |(snd (head b)) == n-1 = 
 	[(head b, (fst (head b)-1, snd (head b) -1)),
 	 (head b, (fst (head b)-1, snd (head b))), 
 	 (head b, (fst (head b), snd (head b)-1)),
 	 (head b, (fst (head b)+1, snd (head b))),
 	 (head b, (fst (head b), snd (head b)+1)),
 	 (head b, (fst (head b)-1, snd (head b)+1))]
 |(snd (head b)) < (n-1) = 
 	[(head b, (fst (head b)-1, snd (head b) -1)), 
 	(head b, (fst (head b), snd (head b)-1)),
 	 (head b, (fst (head b)+1, snd (head b))),
 	 (head b, (fst (head b)-1, snd (head b))),
 	 (head b, (fst (head b)+1, snd (head b)+1)),
 	 (head b, (fst (head b), snd (head b)+1))]
 |otherwise = 
 	[(head b, (fst (head b)+1, snd (head b) -1)), 
 	(head b, (fst (head b), snd (head b)-1)),
 	 (head b, (fst (head b)+1, snd (head b))),
 	 (head b, (fst (head b), snd (head b)+1)),
 	 (head b, (fst (head b)-1, snd (head b)+1)),
 	 (head b, (fst (head b)-1, snd (head b)))]


--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible leaps is only generated once; and when 
-- 		 generating next moves, the program decides which leaps out of 
--		 all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--

-- The logic is the same as generateslides but the difference is we have to account for 3 points and moves 1 space more than generate slides
--generateJumps uses list comprehension to output a list of jumps
--first, it calls generateJumps_helper recursively to get the list of unbounded jumps given the point, then filters out invalid jumps 
generateLeaps :: Grid -> Grid -> Int -> [Jump]
generateLeaps b b1 n
 | null b = []
 | otherwise = generateFilteredJumps(appendJumps([x |x<-generateLeaps_helper b n]:[generateLeaps(tail b) b1 n])) b1

--same logic as appendSlides
--it simply appends the recursive output of the list comprehension structure (as shown in generateJumps) so we don't end up with a list of list of jumps
appendJumps :: [[Jump]] -> [Jump]
appendJumps lx
 | null lx = []
 | otherwise = head lx ++ appendJumps (tail lx)

--same logic as generateFilteredSlides
--it takes in the unbounded list of jumps and makes sure the new list of jumps has only valid jumps
--it also takes in an untouched grid as a comparing guideline when checking for invalid values, which would be out of the grid
generateFilteredJumps :: [Jump] -> Grid -> [Jump]
generateFilteredJumps los b1 = filter (isValidJump b1) los

-- the filtering function for generatefilteredjumps
isValidJump :: Grid -> Jump -> Bool
isValidJump g (x,y,z) = elem y g && elem z g

-- same logic as generateslides_helper but takes in a grid and n sides and produces a list of jumps based on grid position
generateLeaps_helper :: Grid->Int->[Jump]
generateLeaps_helper b n
 |(snd (head b)) == n-1 = 
 	[(head b, (fst (head b)-1, snd (head b) -1), (fst (head b)-2, snd (head b)-2)),
 	 (head b, (fst (head b)-1, snd (head b)), (fst (head b)-2, snd (head b))), 
 	 (head b, (fst (head b), snd (head b)-1),(fst (head b), snd (head b)-2)),
 	 (head b, (fst (head b)+1, snd (head b)),(fst (head b)+2, snd (head b))),
 	 (head b, (fst (head b), snd (head b)+1),(fst (head b), snd (head b)+2)),
 	 (head b, (fst (head b)-1, snd (head b)+1),(fst (head b)-2, snd (head b)+2))]
 |(snd (head b)) < (n-1) = 
 	[(head b, (fst (head b)-1, snd (head b) -1),(fst (head b)-2, snd (head b)-2)), 
 	(head b, (fst (head b), snd (head b)-1),(fst (head b), snd (head b)-2)),
 	 (head b, (fst (head b)+1, snd (head b)),(fst (head b)+2, snd (head b))),
 	 (head b, (fst (head b)-1, snd (head b)),(fst (head b)-2, snd (head b))),
 	 (head b, (fst (head b)+1, snd (head b)+1),(fst (head b)+2, snd (head b)+2)),
 	 (head b, (fst (head b), snd (head b)+1),(fst (head b), snd (head b)+2))]
 |otherwise = 
 	[(head b, (fst (head b)+1, snd (head b) -1),(fst (head b)+2, snd (head b)-2)), 
 	(head b, (fst (head b), snd (head b)-1),(fst (head b), snd (head b)-2)),
 	 (head b, (fst (head b)+1, snd (head b)),(fst (head b)+2, snd (head b))),
 	 (head b, (fst (head b), snd (head b)+1),(fst (head b), snd (head b)+2)),
 	 (head b, (fst (head b)-1, snd (head b)+1),(fst (head b)-2, snd (head b)+2)),
 	 (head b, (fst (head b)-1, snd (head b)),(fst (head b)-2, snd (head b)))]

--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the 
-- board, else generate a search tree till the specified depth and apply 
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over, 
--          otherwise produces the next best board
--
--1. check if the game is over, if it is, then just return the board
--2. we also know that theres no point in playing if there are no more possible moves, which can be determined by tree depth (second conditional)
--3. if there are still options left, then we use the minimax function to make a search tree with the possible states
-- There is a error that myTurn is not defined however boardEvaluator needs to take that in as a parameter when called
stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
stateSearch board history grid slides jumps player depth num
 | gameOver board history num = board
 | depth == 0 = board
 | otherwise = minimax generateTree (board history slides jumps player depth num) boardEvaluator (player history num board myTurn)

--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

--here our basecase is either if the game is over or the depth of the tree is 0, which implies that there is nothing to be put in the tree as any potential moves
--we then go ahead and organize any of the newstates called from genNewStates on to our beautiful tree using the higher-order map function
generateTree::Board->[Board]->Grid->[Slide]->[Jump]->Piece->Int->Int->BoardTree
generateTree board history grid slides jumps player depth n
 | depth==0 || gameOver board history n = (Node depth board [])
 | otherwise = (Node depth board (map (\ boards -> generateTree boards (board:history) grid slides jumps player (depth - 1) n) (generateNewStates board history grid slides jumps player)))
--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate 
-- a list of next boards, and then checks whether or not that move would 
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

--1.
--combine board and grid
--- by zipping the points and the pieces together as tiles, then putting them in a list (AKA STATE)

--2.
--use that STATE, along with the other args, and pass it as arguments to moveGenerator

--3. 
--use the outputted moves to create new BOARDS (hardest part)

--4. 
--filter out boards already in history of boards

generateNewStates::Board->[Board]->Grid->[Slide]->[Jump]->Piece->[Board]
generateNewStates board history grid slides jumps player = 
	filter(\ boards-> not(elem boards history))
	[stateToBoard [nextMove tile move player | tile <- (makeState grid board)] | move<-moveGenerator(makeState grid board) slides jumps player]

-- makes a state which is a list of tiles given a grid and board
-- make state is recursively called to make the list of tiles that make-up the state
makeState::Grid->Board->State
makeState g b
 | null g = []
 | null b = []
 | otherwise = ((head b), (head g)):makeState(tail g) (tail b)

--takes in a tile, move and piece and moves the player
-- if conditions are false it will just be the tile
nextMove::Tile->Move->Piece->Tile
nextMove tile move player
 | snd tile == fst move = (D, fst move)
 | snd tile == snd move = (player, snd move)
 | otherwise = tile
 
-- changes the state to the board
-- this is done by taking the first of a tile (head state) which gives you a piece
-- now make a list of pieces as long as the state is not empty
stateToBoard::State->Board
stateToBoard state
 | null state = []
 | otherwise = fst (head state): stateToBoard (tail state)
--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps, 
-- a list of possible slides and a player from whose perspective 
-- to generate moves, to check which of these jumps and slides 
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--		 type State, for our purposes it is zipping the board and the
--		 grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--

--we needed these three mini functions to access elements in triplets, as defined in a jump data structure
get1st (x,_,_) = x
get2nd (_,y,_) = y
get3rd (_,_,z) = z

--the conditionals of our moveGenerator is as follows:
--base case to check if list of slides/jumps is empty
--in order for a slide to be a valid move, we check if the adjacent spot on the board/grid is empty
--otherwise, we simply recurse the function without change
--for both jump conditionals, we check if an adjacent spot has another identical player type, and if the potential jump spot is empty
moveGenerator :: State -> [Slide] -> [Jump] -> Piece -> [Move]
moveGenerator state slides jumps player
 | null slides = []
 | null jumps = []
 | elem (D,(snd(head slides))) state == True= filterMoves state (head slides:moveGenerator state (tail slides) jumps player) player
 | elem (D,(snd(head slides))) state == False= moveGenerator state (tail slides) jumps player
 | player == W && elem (W,(get2nd(head jumps))) state && elem (D,(get3rd(head jumps))) state == True =filterMoves state ((get1st(head jumps), get3rd(head jumps)): moveGenerator state slides (tail jumps) player) player
 | player == B && elem (B,(get2nd(head jumps))) state && elem (D,(get3rd(head jumps))) state == True =filterMoves state ((get1st(head jumps), get3rd(head jumps)): moveGenerator state slides (tail jumps) player) player
 | otherwise 	= moveGenerator state slides (tail jumps) player

-- if there is PLAYER at the first POINT of MOVELIST ITEM
-- true
--generateFilteredJumps :: [Jump] -> Grid -> [Jump]
--generateFilteredJumps los b1 = filter (isValidJump b1) los

--isValidJump :: Grid -> Jump -> Bool
--isValidJump g (x,y,z) = elem y g && elem z g

--the helper function filterState for moveGenerator, which takes in a state and a player, and outputs a valid state
filterState::State->Piece->State
filterState state player = filter (isValidState player) state

--converted the states to points in order to make the comparison between state and list of moves easier. we could've used list comprehension/patternmatching here, but the group member responsible for implementation
--had difficulty understanding the two concepts (STUDY TOPIC LOL)
makeStatePoints::State->[Point]
makeStatePoints state
 |null state = []
 |otherwise =  snd (head state):makeStatePoints(tail state)

--the helper function for filterState, which checks if there happens to be a player in on the board spot specified by the tile argument
isValidState :: Piece -> Tile -> Bool
isValidState player tile = player == (fst tile)

--filterMoves is the essential helper function for moveGenerator, because it calls a function composition of all the helper functions defined above
--it takes in a state, list of moves, and current player, and outputs a filtered list of moves that is validated by its helpers
filterMoves::State->[Move]->Piece->[Move]
filterMoves state moves player = filter(isValidMove (makeStatePoints(filterState state player))) moves

--the helper to analyze each move, checking if its in the list of points (that was converted from a state)
isValidMove::[Point]->Move->Bool
isValidMove points (x,y) = elem x points	



--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--
--following the Nov 24 lecture notes, we used the algorithm for a similar board evaluation/adversarial search
boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn
 |not myTurn && gameOver board history n = 10
 |myTurn && gameOver board history n = -10
 |otherwise = mylength board  W - mylength board B

--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree, 
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--
-- When the childrean of the boardTree is null produce the heuristic which is the current board
-- Otherwise find the maximum value from the children and produce that board
-- Unfortunately we did not have enough time to figure out an association using the highest value to find that board
minimax :: BoardTree -> (Board -> Bool -> Int) -> Board
minimax (Node _ b children) heuristic
	| null children = b
	| otherwise 	= maxHelper (map (\ tree -> minimax' tree heuristic True) children)

--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes 
-- a search tree, an appropriate heuristic to apply to the leaf nodes of 
-- the tree, and based on whether it would have been the maximizing 
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--				 appropriate heuristic to apply based on the size of the board,
--				 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
-- 				 or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--
-- When boardTree is null (meaning that there are no more nodes left in the boardTree) it will return the heuristic which is the int value
-- When it is your current player (maxPlayer = True by default) we will use the max helper to find the max value of the list of values produced from map on the boardTree. The player will then be switched (True -> False)
-- When it not the current player we will use min helper to find the min value of the list of values produced from map on the boardTree. The player will then be switched (False -> True)

minimax' :: BoardTree -> (Board -> Bool -> Int) -> Bool -> Int
minimax' boardTree heuristic maxPlayer
	| null boardTree 	= heuristic
	| maxPlayer == True 	 = maxHelper (map (\ tree -> minimax' tree heuristic False) boardTree)
	| otherwise 			 = minHelper (map (\ tree -> minimax' tree heuristic True)  boardTree)

-- User as a helper to find the maximum value of a list
-- Takes in a list of Integers and produces an Integer (the highest value)
-- When the rest of the list is empty produce the first of the list
-- Otherwise compare the head of the list to the rest and produce the max value. Recursion will be used on rest
maxHelper :: [Int] -> Int
maxHelper [l:_] = l
maxHelper (l:ls) = max l (maxHelper ls)

-- User as a helper to find the minimum value of a list
-- Takes in a list of Integers and produces an Integer (the lowest value)
-- When the rest of the list is empty produce the first of the list
-- Otherwise compare the head of the list to the rest and produce the minimum value. Recursion will be used on rest
minHelper :: [Int] -> Int
minHelper [l:_] = l
minHelper (l:ls) = min l (minHelper ls)

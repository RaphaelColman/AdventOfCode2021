# Advent of Code 2021

# Table of Contents
- [Advent of Code 2021](#advent-of-code-2021)
- [Table of Contents](#table-of-contents)
    - [Overview](#overview)
    - [Getting started](#getting-started)
- [Write up](#write-up)
    - [Day 1](#day-1)
    - [Day 2](#day-2)
    - [Day 3](#day-3)
    - [Day 4](#day-4)
    - [Day 5](#day-5)
    - [Day 6](#day-6)
    - [Day 7](#day-7)
    - [Day 8](#day-8)
    - [Day 9](#day-9)
    - [Day 10](#day-10)
    - [Day 11](#day-11)
    - [Day 12](#day-12)

### Overview
This is inspired by mstksg's fantastic Haskell solutions found [here](https://github.com/mstksg/advent-of-code-2020).

This year I'll attempt to write my thoughts on each day's solution, and why this challenge is so much fun in Haskell. You can use this repo as a starter project for writing your own solutions in Haskell as it abstracts away the slightly tricky IO/reading puzzle input from file etc.

### Getting started
See [here](https://www.haskell.org/platform/) for how to install the Haskell platform.
This repo is built using [stack](https://docs.haskellstack.org/en/stable/README/) which you will also need to install. After that, run `stack build` to build the project.

This project uses a .env file for configuration. See `.env.example` to create your own. You can get your session key by logging into Advent of Code then inspecting your cookies. After that, the project will handle getting your puzzle input and caching it in the /res directory.

To solve a day, just open the corresponding DayX.hs file in the /solutions directory. Each solution must be of the form:
```haskell
data AoCSolution a b c =
  MkAoCSolution
    { _parser :: Parser a
    , _part1  :: a -> b
    , _part2  :: a -> c
    }
```
See Day1.hs for an example, which has been implemented for day 1.

To run, you can use the GHCI repl. For example:
```
❯ stack ghci
Using main module: 1. Package `AdventOfCode2021' component AdventOfCode2021:exe:AdventOfCode2021-exe with main-is file: /Users/raphael.colman/Dev/AdventOfCode2021/app/Main.hs
AdventOfCode2021> initial-build-steps (lib + exe)
The following GHC options are incompatible with GHCi and have not been passed to it: -threaded
Configuring GHCi with the following packages: AdventOfCode2021
...

λ: aoc1
Part 1:
Success (Just 538464)
Part 2:
Success (Just 278783190)
Right ()

```

This is good for trialling solutions, because `:r` will reload your changes. You can also use the `printTestSolutions` function to use inputs from /res/test_inputs instead

Alternatively, you can build and run the application completely
```
❯ stack build
❯ stack exec AdventOfCode2021-exe
Which day of Advent do you want to solve? [1-25]
1
Part 1:
Success (Just 538464)
Part 2:
Success (Just 278783190)
Right ()
```


# Write up
### Day 1
Day 1 is always more of a warm-up puzzle. In this case, the 'puzzle bit' is figuring out how to convert a list like:
`[1,2,3,4,5,6,7,8]` into `[(1,2),(2,3),(3,4),(4,5),(5,6),(6,7),(7,8)]`.
Fortunately, the `zip` function comes to the rescue, which 'zips' through two lists at the same time and makes a tuple of the two values. So
```
zip [1,2,3] [4,5,6] --> [(1,4), (2,5), (3,6)]
```

With some smart pattern matching, we can zip a list with the same list minus the first element:
```haskell
window2 :: [a] -> [(a, a)]
window2 l@(_:xs) = zip l xs
window2 _        = []
```
After that it's just a matter of filtering the tuples to leave only those where the second item is greater than the first:
```haskell
sonarSweep :: [Int] -> Int
sonarSweep = length . filter id . map (\(x, y) -> y > x) . window2
```

For part 2, we do almost exactly the same thing, except this time we use `zip3` which, as the name implies, will 'zip' through three lists
```haskell
window3 :: [a] -> [(a, a, a)]
window3 l@(_:y:xs) = zip3 l (y : xs) xs
window3 _          = []
```

Adding the items in the tuple together is pretty easy:
```haskell
part2 :: Depths -> Int
part2 = sonarSweep . map (\(x, y, z) -> x + y + z) . window3
```

We can actually write a generic `windowN` function which creates windows of arbitrary length:
```haskell
windowN :: Int -> [a] -> [[a]]
windowN n xs = filter ((== n) . length) $ map (take n) $ tails xs
```
but personally, I think I prefer the window2/window3 version. Those ones make tuples rather than lists, which means you have compile-time guarantees about their length.

### Day 2
I made the mistake of adding [this browser extension](https://chrome.google.com/webstore/detail/advent-of-code-charts/ipbomkmbokofodhhjpipflmdplipblbe) and looking at delta times. I guess I'm just quite slow!
Also not such a challenging day today. I chose to use the excellent [Linear V2](https://hackage.haskell.org/package/linear-1.20.7/docs/Linear-V2.html) package to keep track of position. Probably overkill, as the reason you would use V2 is in order to add positions together like vectors, and I barely ended up doing that!
```haskell
data Direction
  = Forward
  | Up
  | Down
  deriving (Show, Eq, Enum)

data Instruction =
  MkInstruction
    { _direction :: Direction
    , _amount    :: Integer
    }
  deriving (Show, Eq)

type Position = V2 Integer
```
To implement, it's just folding over a list. You have to use `foldl` to ensure to fold from left to right. To fold, you need an aggregating function:
```haskell
addInstruction :: Instruction -> Position -> Position
addInstruction (MkInstruction direction amount) pos =
  case direction of
    Up      -> pos - V2 0 amount
    Down    -> pos + V2 0 amount
    Forward -> pos + V2 amount 0
```
And of course to supply it with a 'starting value' (in this case `V2 0 0`)
So part 1 becomes:
```haskell
part1 :: [Instruction] -> Integer
part1 = (\(V2 x y) -> x * y) . foldl' (flip addInstruction) (V2 0 0)
```
the 'flip' is because `addInstruction` takes an `Instruction` first, then a `Position`, whereas foldl' needs it to be the other way around (ie a -> b -> a). Flip will just reverse the order of the two parameters.

For part 2, we need more state than just position. Now it's position and aim:
```haskell
data PositionWithAim =
  MkPositionWithAim
    { _position :: Position
    , _aim      :: Integer
    }
  deriving (Show, Eq)
```
Which makes our aggregating function look like this:
```haskell
addInstructionWithAim :: Instruction -> PositionWithAim -> PositionWithAim
addInstructionWithAim (MkInstruction direction amount) (MkPositionWithAim position aim) =
  case direction of
    Up      -> MkPositionWithAim position $ aim - amount
    Down    -> MkPositionWithAim position $ aim + amount
    Forward -> MkPositionWithAim (position + V2 amount (aim * amount)) aim
```
Bring on day 3!

### Day 3
To be honest, I found today quite hard! That doesn't bode well as it's only day 3. I might have found it easier if I knew how to use [Data.Bits](https://hackage.haskell.org/package/binary-0.7.1.0/docs/Data-Binary-Get-Internal.html#v:getByteString) but after some wasted time reading the docs, I decided I would be better off just implementing a naive version that I could actually understand.
```haskell
data BinaryDigit
  = ZERO
  | ONE
  deriving (Eq, Show, Enum, Ord)
```
So part 1 is made easy by the super-handy `transpose` from `Data.List`
```
transpose [[1,2,3],[4,5,6]]
[[1,4],[2,5],[3,6]]
```
Which makes the implementation really simple in the end:
```haskell
part1 :: [[BinaryDigit]] -> Integer
part1 bd = gamma * epsilon
  where
    gamma = toDecimal $ map mostCommon $ transpose bd
    epsilon = toDecimal $ map leastCommon $ transpose bd
```
`toDecimal` is just a function which zips through a `BinaryDigit` and multiplies it by the corresponding power of 2 (so we can geta decimal at the end)
The `mostCommon` and `leastCommon` functions are just helpers for list which do what they say on the tin. Did you see how `BinaryDigit` derives `Ord`? That means
Haskell will use the order you've declared the data constructors to implement the `Ord` typeclass. We get the added bonus that it will take `ONE` as the tie-breaker in `mostCommon` and `ZERO` as the tie-breaker in `leastCommon`. That's lucky, because it's required for the puzzle!
```haskell
toDecimal :: [BinaryDigit] -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: [BinaryDigit] -> [Integer]
    asIntList = map (toInteger . fromEnum)

```
`toDecimal` can use `fromEnum` to get the correct values for ZERO and ONE (ZERO is the first data constructor, so its enum value is 0 etc)

Unfortunately, part 2 is significantly harder. Simply mapping through a transposed list won't work anymore because we're mutating the list as we go. I spent a while trying to figure out a clever way of doing this,
then eventually I gave up and decided to just use recursion. Any time you want to do the same sort of thing as a while loop in Haskell, you can achieve it by defining some state for your loop first:
```haskell
data ReadState =
  MkReadState
    { _index  :: Int
    , _values :: [Seq BinaryDigit]
    }
  deriving (Eq, Show)
```
`Seq` is from `Data.Sequence` - much better than plain old list because you can easily retrieve values for indexes.
Then we can define a function to take a ReadState and 'progress' to the next iteration of the loop (we'll do it for the oxygen generator rating first - the 'most common' bit)
```haskell
stepReadState :: ReadState -> Maybe ReadState
stepReadState (MkReadState index values) = do
  foundDigit <- mapM (S.lookup index) values
  filtered <- filterM (fmap (== foundDigit) . S.lookup index) values
  pure $ MkReadState (index + 1) filtered
```
What's this `mapM` thing? It's a convenience function around Monads. In this case, it will take a function which returns a `Maybe`, map it over a list of something, and instead of creating a list of Maybes, it will convert it to `Maybe List`. The actual type signature:
```haskell
mapM :: (Traversable t, Monad m) => (a -> m b) -> t a -> m (t b)
```
So in this case, if `S.lookup` failed to find anything and returned a `Nothing`, the entire list (`digits`) would be `Nothing`.
It's the same concept for `filterM`. It's just like using `filter`, but I can use it with a function which returns a Monad (a Maybe in this case).

Now we can get from one ReadState to another, we can use recursion to loop through until some condition is met (in this case, there is only one value in the list of values)
```haskell
runReadState :: ReadState -> Maybe [BinaryDigit]
runReadState rs@(MkReadState index values)
  | length values == 1 =
    let value = head values
     in pure $ toList value
  | otherwise = stepReadState rs >>= runReadState
```
This might look a little daunting, but it helps to read it line by line. It will first check the ReadState you've passed in. Specifically, it will check the length of the `values` field. If that values field is length 1, it will just return it. That's our "base case". In all other cases, it will step the ReadState once and then recurse. We've had to use `>>=` because both `StepReadState` and `runReadState` return a `Maybe`, so we use monads to bind them together into one `Maybe`. We don't normally use `>>=` that much because we would use the `Do` notation to achieve the same thing instead.
Of course, this had the `mostCommon` function hard-coded in. We can parameterise that as well:
```haskell
runReadState ::
     ([BinaryDigit] -> BinaryDigit) -> ReadState -> Maybe [BinaryDigit]
runReadState f rs@(MkReadState index values)
  | length values == 1 =
    let value = head values
     in pure $ toList value
  | otherwise = stepReadState f rs >>= runReadState f
```
Which means part 2 looks something like this:
```haskell
part2 :: [[BinaryDigit]] -> Maybe Integer
part2 bd = do
  let rs = initReadState bd
  oxyGen <- toDecimal <$> runReadState mostCommon rs
  co2Scrub <- toDecimal <$> runReadState leastCommon rs
  pure $ oxyGen * co2Scrub
```

This took me an absurd amount of time this morning, partly because I was desperate to finder a neater way of doing it than explicit recursion. I also realised that my `AoCSolution` forces you to parse to the same structure for both part 1 and part 2 - almost always the case, but it was annoying today because I had parsed much more naively for part 1 (I just made a list of strings). I'll try and find some time today to separate those out so you can treat the two parts of the puzzle more independently.

### Day 4
This one felt quite similar to yesterday, but not quite as difficult. Again you have to iterate through some state until a condition is met. The notion of playing bingo with a giant squid definitely made me smile.

This is the first puzzle where I felt I had an advantage at the parsing stage, because Haskell's parser combinators are so nice.
```haskell
type Board = [[BingoSquare]]

type Numbers = [Integer]

type BingoSquare = (Integer, Bool)

data Bingo =
  MkBingo
    { _boards        :: [Board]
    , _numbers       :: Numbers
    , _calledNumbers :: Numbers
    , _winners       :: [Board]
    }
  deriving (Eq, Show)

parseInput :: Parser Bingo
parseInput = do
  nums <- commaSep integer
  whiteSpace
  boardNumbers <- some $ count 5 $ count 5 integer
  let boards = fmap3 (, False) boardNumbers
  pure $ MkBingo boards nums [] []
```
The `count` parser lets me specify that I want to parse an exact number of something - in this case 5 integers. I love these parsers because I think reading them is so much easier than the `splitBy ','` nonsense you have to do in other langauges.

So once again, we define some state (our `Bingo` type) and define a way to step from one state to the other. In this case, every time we call a number we'll add it to _calledNumbers, and every time a board wins, we'll take it out of _boards and put it in _winners. So we need a way of checking if a board has won. `transpose` to the rescue again!
```haskell
checkBoard :: Board -> Bool
checkBoard board =
  let rowsComplete = any (all snd) board
      columnsComplete = any (all snd) $ transpose board
   in rowsComplete || columnsComplete
```
`snd` just gets the second item in a tuple (the Boolean representing whether the number has been crossed out).

After that, we just define a way of getting from one board to the next: `stepBoard`. The important logic here is to ignore the `BingoSquare` if the Boolean in it is `True` (it has already been crossed out in some previous step), and otherwise to check the value against whatever is being called out.
```haskell
stepBingo :: Bingo -> Maybe Bingo
stepBingo bingo@(MkBingo boards numbers calledNumbers winners)
  | null numbers = Nothing
  | otherwise = do
    let newBoards = fmap3 checkSquare boards
        checkSquare sq@(n, checked) =
          if checked
            then sq
            else (n, n == head numbers)
    let (newWinners, losers) = partition checkBoard newBoards
    pure $
      MkBingo
        losers
        (tail numbers)
        (head numbers : calledNumbers)
        (newWinners ++ winners)
```
We have to take it on trust that only one board will win at at time (at least for the first and last winners). It's that trust which means we have to do the ugly `++` at the end (concatening two lists). Notice that we don't have to when adding the most recently called number to `calledNumber` because we know there's only one thing to add.

NB: Why is `++` ugly? It's not that ugly, but it does force us to evaluate all the way down the spine of the first list (although without having to evaluate the values inside). It needs to evaluate the final spine in the first list so that it can attach it to the first spine in the second. If it weren't for the `++` here, everything would stay beautifully lazy. :(

By the way, `fmap3` is a helper for functors I added this morning. It allows you to get at the value nested inside 3 functors (so it's just `fmap . fmap . fmap`). Here, we have a list of lists of lists (each Board is 2-dimensional and we have a list of boards). `fmap3` lets you map over the value (the `BingoSquare` tuple) inside.

After that, part 1 and 2 are almost identical. In part 1 we run until we get our first winner. For part 2 we run until there are no boards left:
```haskell
runBingo :: Bingo -> Maybe Integer
runBingo bingo@(MkBingo boards numbers calledNumbers winners)
  | null winners = stepBingo bingo >>= runBingo
  | otherwise = pure $ evaluateBoard (head winners) (head calledNumbers)

runBingoUntilLast :: Bingo -> Maybe Integer
runBingoUntilLast bingo@(MkBingo boards numbers calledNumbers winners)
  | null boards = pure $ evaluateBoard (head winners) (head calledNumbers)
  | otherwise = stepBingo bingo >>= runBingoUntilLast
```
This should look familiar. It's the same anamorphic structure we had from yesterday's puzzle. If I were clever enough to use [this recursion-schemes library](https://hackage.haskell.org/package/recursion-schemes) then I probably wouldn't need to duplicate so much code, but understanding all the different morphisms makes my head spin.

To actually get the answer out you have to add all the unmarked squares together and multiply them by the last called number. This is, of course, a simple map/filter/fold operation provided you first concatenate all the rows together into one list: 
```haskell
evaluateBoard :: Board -> Integer -> Integer
evaluateBoard board lastCalled =
  let unmarkedSum = sum $ map fst $ filter (not . snd) $ concat board
   in unmarkedSum * lastCalled
```
The weather is awful today. Maybe I'll just stay inside and [learn more Haskell](https://media.giphy.com/media/Cz6TlrRVVyv9S/giphy.gif).

### Day 5
Busy day today, so I'm going to keep the write-up brief. We have more 2d geometry to do, so I'm pleased to announce the return of [V2](https://hackage.haskell.org/package/linear-1.20.7/docs/Linear-V2.html) which I will pretty much always use to describe points in space and vectors. I ended up with data types that look like this:
```haskell
type Point = V2 Integer
type Line = (Point, Point)
```
That makes a nice change from the more complicated data types I normally make! Part one of the puzzle tells us that we only need to worry about getting the points covered by horizontal and vertical lines. That's a relief. First, we can easily tell if lines are horizontal or vertical by checking if their x or y value stays the same between both points.
```haskell
isHorizontal :: Line -> Bool
isHorizontal (v1, v2) = v1 ^. _y == v2 ^. _y

isVertical :: Line -> Bool
isVertical (v1, v2) = v1 ^. _x == v2 ^. _x
```
I know the `^._x` stuff is a bit of an eyesore. Linear.V2 uses the [lens](https://hackage.haskell.org/package/lens) library which has a whole bunch of symbols to shorthand getters and setters. In this case, you can read ^. as 'get'

Next up, we need to be able to enumerate all the points covered by a horizontal or vertical line. We can use 'ranges' to make this part easy.
```
λ: [1..5]
[1,2,3,4,5]
λ: [5,4..1]
[5,4,3,2,1]
```
I added a helper function to make descending ranges a little easier:
```haskell
flexibleRange :: Integer -> Integer -> [Integer]
flexibleRange a b
  | b >= a = [a .. b]
  | otherwise = [a,(a - 1) .. b]
```
So now we can just enumerate some points by mapping over a range:
```haskell
horizontalPointsCovered :: Line -> [Point]
horizontalPointsCovered (V2 x1 y1, V2 x2 y2) =
  map (`V2` y1) $ flexibleRange x1 x2

verticalPointsCovered :: Line -> [Point]
verticalPointsCovered (V2 x1 y1, V2 x2 y2) = map (V2 x1) $ flexibleRange y1 y2
```

Part two makes us worry about diagonals, but fortunately the diagonals are all 45 degrees, so we don't have to do anything complicated:
```haskell
diagonalPointsCovered :: Line -> [Point]
diagonalPointsCovered (V2 x1 y1, V2 x2 y2) =
  zipWith V2 (flexibleRange x1 x2) (flexibleRange y1 y2)
```
If anything, I would say that's simpler than the horizontal and vertical versions.

Finally, we need something to list out the points covered by any of the lines:
```haskell
pointsCovered :: Line -> [Point]
pointsCovered line
  | isHorizontal line = horizontalPointsCovered line
  | isVertical line = verticalPointsCovered line
  | otherwise = diagonalPointsCovered line
```

Then we combine it all together by enumerating all the points covered, tallying up the frequency of each one (a helper function here called `freqs`) and listing all those with a count above 2. I'll put part 2 in here, and you can just trust that part 1 is exactly the same except we filter out all the diagonal lines first:
```haskell
part2 :: [Line] -> Int
part2 lines =
  let frqs = freqs $ concatMap pointsCovered lines
   in M.size $ M.filter (>= 2) frqs
```
My original version was totally ugly - it involved traversing the list multiple times (once for horizontals, once for verticals etc). I'm glad I cleaned it up. The naive solution here worked a treat, but I spoke to a friend today who implemented it properly using `y=mx+c` so it could handle any kind of diagonal line, whether it was 45 degrees or not. Sounds satisfying, if a little harder.

### Day 6
Ah, the classic, 'now try with a bigger number because your implementation is probably really slow!' puzzle part 2. I confess, my first solution to part 2 was truly idiotic - I hadn't really internalised how the order of the fish just doesn't matter, so I grouped the ones that were next to each other together but still just kept them in a list. My solution was fast enough, but pretty ugly. I've now rewritten it to be neater. First off, we define how we're keeping track of our fish:
```haskell
type Age = Integer

type FishColony = M.Map Age Integer
```
The 'age' type alias isn't really necessary, but it helps me keep track of what the keys are in the map vs what the values are. Our strategy is going to be to first add all the newborn fish as 9s. That way, we can decrement everyone together. Our decrement is simple: just wrap back around to 6 if your age goes below 0:
```haskell
modularDecrement :: Integer -> Integer
modularDecrement i =
  let aged = i - 1
   in if aged < 0
        then 6
        else aged
```
(we can't use the modulo operator here because it's legitimate to have ages above 6)

Haskell's Data.Map library has lots of useful functions for interacting with maps in the ways we need. We can define a 'step' like this:
```haskell
stepFishColony :: FishColony -> FishColony
stepFishColony fc = aged
  where
    numZeros = M.findWithDefault 0 0 fc
    newFish = M.insert 9 numZeros fc
    aged = M.filter (/= 0) $ M.mapKeysWith (+) modularDecrement newFish
```
`M.mapKeysWith` is super useful here. We map over the keys in the original map, but pass in a function to combine values if we end up with duplicate keys:
```haskell
mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
```
That can happen to us here, because some fish might be decrementing from 7->6 at the same time as fish with age 0 are wrapping back around to 6. We just pass in `(+)` to add them together in that case.

Then our run function is simple. We just use `iterate` to produce a lazy infinite list of all steps, then grab the index out of it corresponding to number of steps we actually want to evaluate.
```haskell
runFishColony :: Int -> FishColony -> Integer
runFishColony times fc = M.foldr (+) 0 $ iterate stepFishColony fc !! times
```

So our solution becomes:
```haskell
part1 :: [Integer] -> Integer
part1 = runFishColony 80 . freqs

part2 :: [Integer] -> Integer
part2 = runFishColony 256 . freqs
```
I've been getting up early because I'm excited about solving the day's puzzle, but I must admit my first passes at it are normally bonkers. Maybe I'd do better tackling them later on in the day when I've [woken up properly](https://media.giphy.com/media/3o6gb2rfGKb55YE9lC/giphy.gif).

### Day 7
Gah! I got tripped up today because I forgot how to divide numbers. Haskell is a great language, but I always forget that `/` is for fractionals and `div` is for integrals.

So for part 1, we need to find the integer that is itself closest to all the other integers in a list. The output of part 1 should be that distance. We can just generate a range from 1 -> the maximum number in the list and then test each number against all the numbers in the original list.
```haskell
bestPosition :: [Integer] -> Integer
bestPosition xs = minimum $ map totalFuel [1 .. maximum xs]
  where
    totalFuel x = sum $ map (abs . (x -)) xs
```
Not rocket science (well [actually](https://media.giphy.com/media/RMwZypp489fuGBI0Ti/giphy.gif) I guess technically kind of is).

Then for part 2 we find out that we're calculating fuel wrong. It's not the difference between the two positions - you have to add 1 to the fuel consumed for each step. So 0 -> 5 is 1 + 2 + 3 + 4 + 5 = 15
The solution, of course, is [triangle numbers](https://en.wikipedia.org/wiki/Triangular_number)! The triangle number formula is: n(n+1)/2

So we can simply substitute our fuel calculation function for the triangle number formula.
```haskell
calculateFuel :: Integer -> Integer -> Integer
calculateFuel a b = triangleX $ abs $ a - b
  where
    triangleX x = x * (x + 1) `div` 2

bestPosition :: (Integer -> Integer -> Integer) -> [Integer] -> Integer
bestPosition f xs = minimum $ map totalFuel [1 .. maximum xs]
  where
    totalFuel x = sum $ map (f x) xs
```
(This is where I spent ages trying to get `/` to work because I forgot that you use `div` for Integers)

Our part 1 and 2 becomes:
```haskell
part1 :: [Integer] -> Integer
part1 = bestPosition (\a b -> abs (a - b))

part2 :: [Integer] -> Integer
part2 = bestPosition calculateFuel
```
I promise Haskell is fun to use even though I sometimes fail to do the most basic stuff in it!

### Day 8
Phew! That was definitely the hardest one so far! I liked it a lot though - I love how it seemed impregnable at the start, but once you realise a sensible way of doing it it rapidly becomes simpler to do.
Part 1 was pretty trivial to solve, so I'm not going to bother explaining it. Let's first talk about a general approach for solving part 2. Given an input like this:
```
acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
```
We need to figure out what digits all the 'combos' on the left correspond to so we can 'translate' the combos on the right. There's a convenient table on the [wikipedia page](https://en.wikipedia.org/wiki/Seven-segment_display#Hexadecimal) for seven-segment displays which also labels each section of the display with a letter and then enumerates which combinations correspond to which integer.
For example:
```
0 =  "abcdef"
1 =  "bc"
etc.
```
You can think of this as the 'true mapping'. If our wires weren't all crossed for each input, we'd be done! The problem is that number 1 is represented as "ab" in the input, but as "bc" in the 'true mapping'. That means either:
```
a -> b
b -> c
```
OR
```
a -> c
b -> b
```
More generally, there exists some lookup table where you can plug in the letter from the input, and out will come the correct letter from the 'true mapping'. In this case:
```
a -> b, b -> c, c -> d, d -> a, e -> f and g -> e
```
It's actually possible to figure out what the lookup table is like a sudoku puzzle (the ones with fixed lengths narrow it down, then you can reason out other combinations based on which letters are missing etc) but that's hideously complicated to implement in the code. An easier way to do it is simply to try every possible lookup table (there are only 5040) until we find one which works (ie if you were to use the lookup table to map your input to some other list of letter combinations, then look up the integer in the 'true mapping' table, you would successfully get numbers out of it for all 10 items in the input)

So now we can start adding things to make the solution simpler. Let's start with some modelling:
```haskell
type Combo = S.Set Char
type Entry = ([Combo], [Combo])
type Mapping = M.Map Char Char
```
Combo is a set of characters (like 'cdfbe'). I've made it a Set so we're all clear that the order of the characters does not matter. An entry can just then be a tuple of two lists of combos (input and output)
I'm using 'Mapping' to indicate a map of a -> b, b -> c etc

Next we need a representation of the [one true mapping](https://media.giphy.com/media/3o7abspvhYHpMnHSuc/giphy.gif) (the one from wikipedia)
```haskell
knownCombos :: M.Map Combo Integer
knownCombos = M.fromList $ zip combos [0 .. 9]
  where
    combos =
      map
        S.fromList
        [ "abcdef"
        , "bc"
        , "abdeg"
        , "abcdg"
        , "bcfg"
        , "acdfg"
        , "acdefg"
        , "abc"
        , "abcdefg"
        , "abcdfg"
        ]
```
Not worring about crossed wires: if I were to look up a combo in here, I should get an Integer back out.
Next, we know that given a mapping and a list of combos, we'll need to translate it into a list of integers (for now we'll assume that we've got the correct mapping from somewhere)
```haskell
decodeCombos :: Mapping -> [Combo] -> Maybe [Integer]
decodeCombos mapping = traverse (decodeCombo mapping)
  where
    decodeCombo mapping' combo' =
      let mapped = S.map (mapping' M.!) combo'
       in M.lookup mapped knownCombos
```
Remember how I used `mapM` in a previous puzzle? `traverse` is the same thing, but I think it's newer. The point is here that we attempt to use the mapping to create our list of ints. If we didn't find our particular combo in the 'true mapping', then `M.lookup` will return a `Nothing`, and `traverse` will ensure that the entire function returns a `Nothing` in that case rather than a list of `Maybe`s

We're actually pretty close to done! We need to generate a list of all 5040 mappings now:
```haskell
allMappings :: [Mapping]
allMappings = map (curry toMapping "abcdefg") (permutations "abcdefg")
  where
    toMapping (str1, str2) = M.fromList $ zip str1 str2
```
We just zip 'abcdefg' together with the result of the handy 'permutations' function, which takes a list (strings are just lists of chars) and returns a list of all its permutations. 5040 isn't such a big number, but we get the lovely benefit that Haskell lists are completely lazy, so this list can be generated instantly and we'll only evaluate it as far as we need to when we use it.

So now the logic is like this: For an entry, go through all the possible mappings and attempt to use that mapping. If we succeeded in using the mapping for all 10 combos in the entry, then we can safely assume that it's the correct mapping and we can use it to translate the 'output' part of the entry.
```haskell
tryMappings :: Entry -> Maybe Integer
tryMappings (wires, output) = do
  goodMapping <- find (isJust . (`decodeCombos` wires)) allMappings
  decoded <- decodeCombos goodMapping output
  pure $ read (concatMap show decoded)
```
The first line will find the first item in `allMappings` where if you were to use `decodeCombos` on it, the answer would be a `Just` and not a `Nothing`. After that, we decode the output using our mapping. That means decoded will be something like `[5,3,5,3]` which we need to convert to the integer `5353`. We can just convert each integer to a string, concatenate and read for that.
And we're done! You have to sum all the outputs of that, so part 2 looks like this:
```haskell
part2 :: [Entry] -> Maybe Integer
part2 entries = sum <$> traverse tryMappings entries
```
It's really satisying that the solution here is actually quite compact, considering how complicated the problem seemed initially. I'm keeping my fingers crossed that this was a random spike in difficulty, otherwise there's no way I'll be able to keep up from here onwards.

### Day 9
Not as tricky as yesterday I think. My slowness was all me rather than the difficulty of the puzzle. This one reminded me a little of the [game of life](https://adventofcode.com/2020/day/17) type puzzles from last year, where it's all about keeping track of adjacent points. Part 1 is just finding the 'low points' on the map - the points where all four cardinal neighbours are of a higher value. We define our map as a 'grid'
```haskell
type Grid = M.Map (V2 Int) Int
```
and can define a fairly straightforward function for getting the low points out of it:
```haskell
allDirections :: [V2 Int]
allDirections = [unit _x, -unit _x, unit _y, -unit _y]

allAdjacents :: V2 Int -> [V2 Int]
allAdjacents v = map (v +) allDirections

lowPoints :: Grid -> Grid
lowPoints grid = lowPoints
  where
    lowPoints = M.filterWithKey isLowPoint grid
    isLowPoint coord value =
      let adjacents = mapMaybe (`M.lookup` grid) $ allAdjacents coord
       in value < minimum adjacents
```
all this does is filter through the grid with the predicate 'isLowPoint', which itself will attempt to look up all the adjacent points the one passed in and return true if the point we're looking at is of a lower value than all of them. `mapMaybe` is like a watered-down `traverse` specifically for Maybes. It will take a list of Maybes and remove all the `Nothing`s. That way, we only get adjacents which are actually in our map.

Once again, my first pass at part 2 was a bit idiotic. I tried defining a 'SearchState' which would keep track of the points you'd already searched and the ones you still have yet to search. I've since rewritten it using Data.Set heavily, which made it a slightly slower, but much easier to read. So to summarise the problem first: we need to find all the 'basins' now, which means taking a low point and searching around it for points that are higher until we can't find any more. What does this sound like? An [anamorphism](https://en.wikipedia.org/wiki/Anamorphism)! We're generating a sequence from a starting seed value until some condition is met.

My first go at this used explicit recursion, but for this we're going to use the anamorphism for lists: `unfoldr`! The type definition is this:
```haskell
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
```
So we give unfoldr a function which returns a Maybe of a tuple, and a starting value. The tuple itself contains whatever we want to put in the list, and a new starting value. We return a `Nothing` if we want the unfold to stop.
```haskell
explore :: Grid -> V2 Int -> S.Set (V2 Int)
explore grid point = S.fromList $ filter higherAdjacent (allAdjacents point)
  where
    higherAdjacent adjPoint =
      case M.lookup adjPoint grid of
        Just p  -> p > grid M.! point && p /= 9
        Nothing -> False

doSearch :: Grid -> V2 Int -> S.Set (V2 Int)
doSearch grid = S.unions . unfoldr go . S.singleton
  where
    go points =
      let found = S.union points $ S.unions $ S.map (explore' grid) points
       in if S.size found == S.size points
            then Nothing
            else Just (found, found)
```
So let's unpack this. `explore` is a helper function which, given a grid and a coordinate, will find all adjacent points which are higher up and return them as a set. Then `doSearch` will use unfoldr to explore the new points added to the set. So in this case, the `found` function is just all the points we found from exploring unioned together with our starting points. If we attempt a search and our search space doesn't get any bigger, then we return a `Nothing` so the unfold will stop.

This is certainly fast enough for us, but it's worth noting that it does a lot of unecessary work - it searches all points in the search space every time rather than just the new ones. It's easier to read because it doesn't bother keeping track of what we've already searched. If you wanted to make it faster, you could keep a lazy map of point -> adjacents, but fortunately the basins are not that big so there's no need.

### Day 10
I enjoyed today's puzzle. It wasn't all that difficult, but still satisfying to complete. The algorithm is fairly simple. In order to parse something like:
```
{([(<{}[<>[]}>{[]{[(<()>
```
and conclude that we got a ']' when we should have had a '}', we go through character-by-character from left to right, keeping track of all the expected closing brackets. If our current character equals the most recent expected closing bracket, then we remove it from the list. If our character is itself an opening bracket then we add its closing bracket to the list. Otherwise, we've got an error, and we stop (keeping hold of the bad character). First off, some useful helper functions:
```haskell
matchBracket :: Char -> Char
matchBracket c = M.fromList (zip "{[<(" "}]>)") M.! c

isOpeningBracket :: Char -> Bool
isOpeningBracket c = c `elem` "{[<("
```
We were always going to need these! On to the problem. This is _kind of_ a [catamorphism](https://en.wikipedia.org/wiki/Catamorphism), except it can fail, preserving the reason for failure at the point it happens. Haskell (and FP in general) has the perfect type for this: the [Either](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Either.html). Here is how it's defined:
```haskell
data  Either a b  =  Left a | Right b
```
It's a very simple datatype, and it's commonly used to represent computations that can fail, where if it's a left then it's a failure and if it's a right then it's a success. The reason for that is the way that it's implemented as a functor:
```haskell
instance Functor (Either a) where
    fmap _ (Left x) = Left x
    fmap f (Right y) = Right (f y)
```
So if you map over a Right, you'll map the value inside. If it's a left, then you won't do anything. The applicative and monad instances of the Either have the same philosphy: a left is failure, so do nothing. We can use it to solve our puzzle by defining a function which returns a `Either Char [Char]`. That means if it's a left, it's a single character (the one we failed on), and if it's a right, it's a list of closing brackets we expect. My solution looked like this in the end:
```haskell
checkLine :: String -> Either Char [Char]
checkLine = foldlM go []
  where go [] currentChar = Right [matchBracket currentChar]
        go xs@(expected:rest) currentChar
          | currentChar == expected = Right rest
          | isOpeningBracket currentChar = Right $ matchBracket currentChar : xs
          | otherwise = Left currentChar
```
We use foldlM, which is like foldl except you can use a function which returns a monad (in our case, an `Either`)
```haskell
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
```
foldlM will fold through the foldable, and it will sequence the monad (the either) for each value. The only other thing to note here is that I end up adding the new expected closing brackets to the _left_ of the list. That way, it's easy to pop the most recent one off with pattern matching, and by sheer dumb luck, it's in the correct order for part 2!

Busy weekend coming up, so these write-ups might fall behind. Well actually the puzzles will be getting harder, so they'll probably fall behind regardless of how busy I am.

### Day 11
I got myself into an absolute mess with this one! It has been my absolute worst puzzle so far. I now think it's not that hard, but I kept confusing myself, and ending up with infinite loops and broken code. For starters, I thought flashes would happen at energy >= 9, not energy > 9, which was pretty bad. Then I tied myself in knots trying to find a neat way of figuring out if all the flashes were finished this step and we could progress to the next one. After that, I named a bunch of things lazily (`runFlash`,`runFlash2` etc) and got really confused that way. Disaster!

On the flip side, I solved a problem that bugged me a lot last year. Haskell Base ships with a module called `Debug.Trace` which allows you to print things to the console even in functions which are not IO functions. It's super useful. However, for AoC sometimes you really need to just print out the 2d picture of what's going on, and `Debug.Trace` won't let you do that because it shows newline characters as `\n`, not as actual new lines. I ended up writing my own trace method which uses `unsafePerformIO` to allow me to render grids properly (I needed it because I'd written such stupid buggy code!)

So as ever, we start with some [modelling](https://media.giphy.com/media/Sv951Hh0D7bCK6C3bX/giphy.gif):

```haskell
type Octopodes = Grid Integer

type FreqMap = M.Map Point Integer

data FlashTracker =
  MkFT
    { _alreadFlashed :: S.Set Point
    , _flashes       :: S.Set Point
    , _octopodes     :: Octopodes
    }
  deriving (Eq, Show)
```
`Point` and `Grid` are some types I created in my `Common.Geometry` package which are coordinates and maps of coordinates respectively.  I decided to call the grid of octopus energy levels an `Octopodes` because, as a friend of mine keeps insisting, the plural of octopus should be 'octopodes' because its roots are in Greek, not Latin.
It took me ages to bite the bullet and write the `FlashTracker`. Amazing how easy it became once I admitted I needed a simple type to keep track of state. The idea is that we keep track of which octopodes have already flashed during a step so we only evaluate the new ones and don't end up in an infinite loop.

In order to solve this kind of problem it helps to just bash out some of the helper functions we _know_ we're going to need no matter what:
```haskell
flashing :: Octopodes -> S.Set Point
flashing = M.keysSet . M.filter (> 9)

grow :: Octopodes -> Octopodes
grow = M.map (+ 1)

---I put these in Common.Geometry because they'll definitely be useful in the future
--Get all the neighbours for a point in all directions including diagonals
neighbours :: Point -> S.Set Point
neighbours point = S.fromList $ map (+ point) directions
  where
    directions = [V2 x y | x <- units, y <- units, [x, y] /= [0, 0]]
    units = [-1, 0, 1]

--Get all the neighbours for a point which are in a specified grid
gridNeighbours :: Grid a -> Point -> M.Map Point a
gridNeighbours grid point = M.restrictKeys grid $ neighbours point


--This one is in Common.ListUtils. Given a list, it will count how many times each item appears in a list and make a frequency map
freqs :: (Ord k, Num a) => [k] -> M.Map k a
freqs xs = M.fromListWith (+) (map (, 1) xs)
```

So the important bit is evaluating flashing octopodes during a step. We'll need a recursive function which keeps growing the energy of adjacent octopodes based on how many flashes they are near. Prepare yourself! This one is a bit ugly:
```haskell
runFlash :: Octopodes -> Octopodes
runFlash octopodes = go $ MkFT S.empty (flashing octopodes) octopodes
  where
    go :: FlashTracker -> Octopodes
    go ft@(MkFT alreadyFlashed flashes octopodes')
      | null flashes = grown
      | otherwise = go (MkFT newAlreadyFlashed newFlashes grown)
      where
        freqMap =
          freqs . concatMap (M.keys . gridNeighbours octopodes') $ flashes
        grown = M.unionWith (+) freqMap octopodes'
        newAlreadyFlashed = S.union alreadyFlashed flashes
        newFlashes = S.difference (flashing grown) newAlreadyFlashed
```
I can at least break this down. There's a Haskell custom to name your explicitly recursive function `go`. That allows you to make the outer function take simple parameters, then create the extra state-tracking parameters in the body of the outer function. In this case, we're making a starter `FlashTracker` where the set that tracks which octopodes have already flashed is empty.

Then we make a map of Point -> number of flashing octopodes (called `freqMap`). It reads a bit like minesweeper:
```
.1...112.1
11...1.211
.....111..
.....11111
.....1.11.
...1121122
...1.1..1.
...111..11
```
So in this case: most of the points are near 1 flashing octopus, and some are near 2.
After that, increment each of those by the number they are next to:
```haskell
grown = M.unionWith (+) freqMap octopodes'
```
Finally, we use `S.union` to add to our set of already flashed octopodes, and generate a set of new flashes by seeing who's got a high enough energy and is not in our set. We can then recurse until `newFlashes` is empty.

Now we have our recursive `runFlash` function, defining a step is actually pretty easy. I'm using a `mapIf` helper function I defined which takes a predicate and only maps the value in the map if the predicate is met. So here we'll reset octopodes that have an energy of above 9 back to 0
```haskell
step :: Octopodes -> Octopodes
step = mapIf (> 9) (const 0) . runFlash . grow
```

After that it's pretty simple to count the number of 0s in each step for part 1, and run until we get a synchronized flash for part 2.
```haskell
part1 :: Octopodes -> Int
part1 = sum . map (length . M.filter (== 0)) . take 101 . iterate step

part2 :: Octopodes -> Maybe Int
part2 = findIndex (all (== 0) . M.elems) . iterate step
```
Fingers crossed I find tomorrow easier!

### Day 12
A lovely puzzle today! Normally there's some sort of graph-traversal problem at this point in your advent journey, although the most similar ones I can think of from the last two years were directed acyclic graphs, whereas this was just a graph.

Problems like this normally have a memoization solution which involves [recursive knot tying](https://wiki.haskell.org/Tying_the_Knot), which one of my favourite Haskelly mind-bending techniques. My first pass just used vanilla recursion, but read on to the end to hear about the weird and wonderful world of recursive knots.

So the point here is that we need to traverse from node to node until we reach the end-node, and most nodes can only be visited once. Let's get the model down:
```haskell
type Cave = String

type Path = [Cave]

type Connection = (Cave, Cave)

type CaveSystem = M.Map Cave [Cave]

bigCave :: Cave -> Bool
bigCave = all isUpper

initCaveSystem :: [Connection] -> CaveSystem
initCaveSystem paths = M.fromListWith (++) withReversed
  where
    withReversed = fmap2 (: []) $ paths ++ map swap paths
```
The CaveSystem is a map of cave to connected caves (or node -> children).

We need to enumerate all possible paths from 'start' to 'end', so our algorithm is going to be recursive. The obvious base case is that we've reached the 'end' cave, in which case we return a list with one path in it `[["end"]]`

In all other cases, we look at all the children for the cave we're on. If we've visited that child before and we're not allowed to (it's not a big cave) then we discard it (return an empty list). In all other cases, we call our recursive method again but for each child (so we get a list of lists which we can concatenate). We then bung our current node onto the start of each list and return that. And voila!
```haskell
findPaths' :: CaveSystem -> [Path]
findPaths' system = go S.empty "start"
  where
    go visited "end" = [["end"]]
    go visited cave = map (cave :) $ concatMap visitChild $ system M.! cave
      where
        visitChild child
          | bigCave child = go updateVisited child
          | child `S.member` visited = []
          | otherwise = go updateVisited child
        updateVisited = S.insert cave visited
```
It's my shame as a hot-blooded Haskeller that recursion still makes my head spin.

The solution for part 2 is only a slight change. Now we need to keep track of whether we have performed our one-time-only 'second visit', which we can just track as a boolean. To keep track of state through recursive calls, you simply add a parameter to your recursive function ('go' in this case). The version for part 2 looks like this:
```haskell
findPaths :: CaveSystem -> Bool -> [Path]
findPaths system allowSecondVisit = go S.empty (not allowSecondVisit) "start"
  where
    go visited visitedTwice "end" = [["end"]]
    go visited visitedTwice cave =
      map (cave :) $ concatMap visitChild $ system M.! cave
      where
        visitChild child
          | child == "start" = []
          | bigCave child = go updateVisited visitedTwice child
          | child `S.member` visited =
            if visitedTwice
              then []
              else go updateVisited True child
          | otherwise = go updateVisited visitedTwice child
        updateVisited = S.insert cave visited
```
Almost exactly the same, except we do some special stuff for if we've visited this node already and it's not a big cave. If we are allowed a second visit still, then we recurse but setting `visitedTwice` to true. Otherwise, we discard that child (return an empty list)

So what about this recursive knot-tying? It's worth reading [this blog post](https://jelv.is/blog/Lazy-Dynamic-Programming/) to get a feel for how it works, but the basic idea is that we want some form of memoization, so that we don't have to calculate the paths for a node more than once. With the function above, we are in danger of calling `go` multiples times for exactly the same parameters, which can be made faster by storing the results from the first call and then just using those for any subsequent ones. The trick to doing it in haskell is defining some lazy data structure where the keys are anything you might want to call `go` with and the values are lazily defined. Then, instead of recursing by calling a method, you recurse by looking up the corresponding value in your data structure (which will probably look up some other value somewhere else etc). The lazy population of your data structure mimics the gradual population of a memo. I tinkered for a while and then came up with this eyesore:
```haskell
findPathsKnots :: CaveSystem -> Bool -> [Path]
findPathsKnots system allowSecondVisit =
  memo M.! MkMemoKey "start" S.empty (not allowSecondVisit)
  where
    memo = M.fromList $ map go allMemoKeys
    allPossibleVisited = allSets $ M.keys system
    allMemoKeys =
      liftA3 MkMemoKey (M.keys system) allPossibleVisited [True, False]
    go mk@(MkMemoKey "end" _ _) = (mk, [["end"]])
    go mk@(MkMemoKey cave visited visitedTwice) =
      let paths = map (cave :) $ concatMap visitChild $ system M.! cave
       in (mk, paths)
      where
        visitChild child
          | child == "start" = []
          | bigCave child = lookupForChild child visitedTwice
          | child `S.member` visited =
            if visitedTwice
              then []
              else lookupForChild child True
          | otherwise = lookupForChild child visitedTwice
        lookupForChild child visitedTwice' =
          memo M.! MkMemoKey child (S.insert cave visited) visitedTwice'

allSets :: (Ord a) => [a] -> [S.Set a]
allSets xs = gen
  where
    sizes = [0 .. length xs]
    gen = map S.fromList $ concatMap (`tuples` xs) sizes

data MemoKey =
  MkMemoKey
    { cave         :: Cave
    , visited      :: S.Set Cave
    , visitedTwice :: Bool
    }
  deriving (Eq, Show, Ord)

```
In this one we have a lazy map where the keys are all `MemoKeys` - a combination of current cave, the set of already visited caves and the boolean tracking whether we have performed our second visit. Before you go and try and understand this one fully, I'm going to point out that it's actually _slower_ than the vanilla recursion one. What a disappointment! I traced all the occasions where it looks up a new value in the memo, and made an educated guess that for this input, it very unlikely to make the same exact lookup twice. My conclusion here is that the overhead incurred for comparing all those `MemoKeys` for equality is higher than benefit of memoization. Ah well, it was fun to try!
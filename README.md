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

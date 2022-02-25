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
    - [Day 13](#day-13)
    - [Day 14](#day-14)
    - [Day 15](#day-15)
    - [Day 16](#day-16)
    - [Day 17](#day-17)
    - [Day 18](#day-18)
    - [Day 19](#day-19)
    - [Day 20](#day-20)
    - [Day 21](#day-21)
    - [Day 22](#day-22)

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

### Day 13
I made the mistake today of trying to reuse my code from the ["Jurassic Jigsaw" puzzle last year](https://adventofcode.com/2020/day/20). My code from that puzzle was unreadable spaghetti though, so don't think it saved any time at all. My repo from 2020 is public, so have a look at that day if you want to see some truly horrifying Haskell.

As usual for 2d geometry puzzles, I relied heavily on Linear.V2.
```haskell
data Fold
  = YFold Int
  | XFold Int
  deriving (Eq, Show, Ord)

data Paper =
  MkPaper
    { _points :: S.Set Point
    , _folds  :: [Fold]
    }
  deriving (Eq, Show)
```
Another lesson I learned today - if you think you need a Set then just go ahead and use one. I stubbornly stuck to lists for ages (but that made removing duplicates that little bit more fiddly).

Some simple helper functions to reflect a coordinate around a horizontal or vertical line:
```haskell
reflectX :: Int -> Point -> Point
reflectX val (V2 x y) =
  let amount = val - x
   in V2 (val + amount) y

reflectY :: Int -> Point -> Point
reflectY val (V2 x y) =
  let amount = val - y
   in V2 x (val + amount)
```
This is not that hard! I don't know why I tried to use my spaghetti mess from last year.
A function which 'folds' a set of points about a line will look something like this:
```haskell
doFoldX :: Int -> S.Set Point -> S.Set Point
doFoldX val points = S.union folded right
  where
    withoutLine = S.filter (\(V2 x y) -> x /= val) points
    (right, left) = S.partition (\(V2 x y) -> x > val) withoutLine
    folded = S.map (reflectX val) left
```
We remove the points on the line itself. Then we partition the the points on either side of the line. Finally, we reflect the points on one side and union everything back together (which removes duplicates).

We can generalise this for folding over a horizontal or a vertical line like this:
```haskell
type Axis = (Point -> Int, Int -> Point -> Point)

inX :: (V2 a -> a, Int -> Point -> Point)
inX = ((^. _x), reflectX)

inY :: (V2 a -> a, Int -> Point -> Point)
inY = ((^. _y), reflectY)

doFold :: Int -> Axis -> S.Set Point -> S.Set Point
doFold val (get', reflectF) points = S.union folded above
  where
    withoutLine = S.filter (\v -> get' v /= val) points
    (below, above) = S.partition (\v -> get' v > val) withoutLine
    folded = S.map (reflectF val) below
```
Not a big leap - we just separate out the functions for getting hold of the x or y field from a V2, and also the function for performing the reflection.

Part 1 and part 2 end up looking like this.
```haskell
part1 :: Paper -> Int
part1 (MkPaper points folds) = length $ applyFold (head folds) points

part2 :: Paper -> S.Set Point
part2 (MkPaper points folds) = traceLns (renderVectorSet done) done
  where
    done = foldl' (flip applyFold) points folds

applyFold :: Fold -> S.Set Point -> S.Set Point
applyFold fold points =
  case fold of
    YFold n -> doFold n inY points
    XFold n -> doFold n inX points
```
I don't know about you, but I find it _really_ pleasing that both parts call the function `foldl` over a `Foldable` full of datatypes called `Fold`.

### Day 14
Today was one of those ones where you suspect you're going to get hit by something because part 1 is straightforward. The algorithm is pretty easy to implement, and happily involves using that `window2` function we wrote for puzzle number 1.
```haskell
type Element = String

type Rule = (String, Char)

type Pair = (Char, Char)

type Template = (Element, M.Map Pair Char)

runTemplateNaive :: Int -> Template -> String
runTemplateNaive times (start, rules) = iterate step start !! times
  where
    step current = head current : concatMap insert (window2 current)
    insert p@(a, b) = [rules M.! p, b]
```
I've called this one 'runTemplateNaive' because now I know that part2 is the classic AoC: 'I know you've implemented something ineffecient, hold on a sec while I set your CPU on fire'. Of course, laziness lists makes it even worse in Haskell because running this 30 more times uses of loads of memory as it builds up sequences of [thunks](https://wiki.haskell.org/Thunk) that it will, eventually, have to evaluate.

I spent a while with a pen and paper trying to do something overcomplicated with [cycles](https://media.giphy.com/media/DAkTj5U6okQY8/giphy.gif). You can draw a pretty graph to see that any one pair will either eventually produce itself after a few iterations, or produce a pair which will start cycle to itself.
Then I realised: You can just implement this like the Lanternfish puzzle! We keep a map of Pair -> Int, and just fold through it, updating our map and keeping a running total of all the new characters we added. As we often do, it helps to define some sort of state for our loop:
```haskell
data RunningTotal =
  MkRT
    { pairCount    :: PairCount
    , elementCount :: ElementCount
    }
  deriving (Eq, Show)

initRT :: Template -> RunningTotal
initRT (start, rules) = MkRT pc $ freqs start
  where
    pc = freqs $ window2 start
```

Then, we define a function which, given a running total, will produce the new running total. It's a little more complicated than the lantern fish, because you have to update the running total for each pair in the `PairCount` map, so your main logic is actually: given a pair and the number of times it occurs, give ma new running total. My one looked like this:
```haskell
runTemplate :: Int -> Template -> RunningTotal
runTemplate times template@(_, rules) =
  iterate stepRT (initRT template) !! times
  where
    stepRT :: RunningTotal -> RunningTotal
    stepRT rt@(MkRT pairCount _) = M.foldrWithKey go rt pairCount
      where
        go :: Pair -> Int -> RunningTotal -> RunningTotal
        go pair@(a, b) num (MkRT pc ec) =
          let insert = rules M.! pair
              newElementCount = M.insertWith (+) insert num ec
              updates =
                M.fromListWith (+) $
                (pair, negate num) : map (, num) [(a, insert), (insert, b)]
              newPc = M.filter (>= 0) $ M.unionWith (+) updates pc
           in MkRT newPc newElementCount
```
I'm not sure how many nested functions you're allowed before it's taking the piss. The important stuff is in `go`. That one will update the `ElementCount` by adding the 'new' character the right number of times. Then we update our pair count by updating the count of the following:
1. The current pair: we've just split it up, so it needs to be decremented (that's the `negate num` bit)
2. The two new pairs we get from inserting the new character (`insert`). These should be incremented

A little `foldrWithKey` does the trick here, by folding through each element in the `pairCount` map. In retrospect, this could be made even simpler by not bothering to keep a running total. You could instead just count the first element of every pair in the `PairCount` remembering to track whatever the final character is at the end.

This was a great puzzle, but I got really frustrated with it because my first passes at puzzle 2 worked for the test input but not for the actual input. I had a stupid bug where `initTemplate` initialised all the pairs to have a count of 1, which happens to be true for the test input. What a pain to debug!

### Day 15
Fab puzzle today! Find the path of lowest cost from a start node to a finish node. I started by writing naive, hand-baked recursive solution and realised it was never going to work when it slowed to crawl on the test input. After some frantic googling, I decided that the solution would be to implement [Dijkstra's Algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm). Believe it or not, I had heard of it before in [this xkcd comic](https://www.explainxkcd.com/wiki/index.php/342:_1337:_Part_2), but I had no idea what it was or how it worked.

The idea is that you keep track of all the nodes you haven't visited yet (the _unvisited_ nodes) and you keep a map of 'tentative distances' - your best guess so far for how costly it is to get from the start node to this particular node. Those distances all start of as infinity. Then your algorithm is to update the tentative distances of all your neighbour nodes by adding their cost to your current cost, then visit the least costly neighbour and repeat.

```haskell
dijkstra :: Grid Int -> Int
dijkstra grid = go (V2 0 0) (M.fromList [(V2 0 0, 0)]) $ M.keysSet grid
  where
    bottomRight' = bottomRight grid
    go current tDistances unvisited
      | current == bottomRight' = tDistances M.! current
      | otherwise = ($!) go minNode newTDistances newUnvisited
      where
        children = gridOrthogonalNeighbours grid current
        unVisitedChildren = M.restrictKeys children unvisited
        distances = M.map (+ tDistances M.! current) unVisitedChildren
        newTDistances = M.unionWith min distances tDistances
        newUnvisited = S.delete current unvisited
        (minNode, value) =
          minimumValue $ M.restrictKeys newTDistances newUnvisited
```
This is pretty much the whole puzzle, except there's one weird thing in there. Did you notice the crazy `($!)` operator? If you take that out, then this recursive function grinds to a halt. It's crazy slow. What's happening is that these new maps our function is creating are all lazy. Haskell just builds up a really long list of [thunks](https://wiki.haskell.org/Thunk) - computations that it promises to evaluate if it _really_ needs to. The problem is that it will eventually need to evaluate all of them, so it's just inefficient to build them all up in memory like that.
The answer is the `($!)` operator, which forces strict evaluation for everything on the right-hand side of it. Once I realised what was happening and added that, the program went from running in about 1 minute to about 1 second for part 1.

### Day 16
Let's talk about [parser combinators!](https://en.wikipedia.org/wiki/Parser_combinator) Parser Combinators allow you build up big parsers from small ones. So you might define a simple parser that just parses a literal character like a ':'. Then another parser which parses all alphanumeric characters until it reaches a character which isn't alphanumeric. Maybe another which parses newlines. Before you know it, you have a load of parsers which can be composed to parse a YAML file.

Parsers in Haskell are monads (all the best things are). The 'effect' of this particular monad is to consume characters in the input stream. So that means the tools around monads are _really_ useful. You can do things like trigger the effect of the monad without worrying about the result (so just ignore some specific characters in the input stream). The `Parser` we're using implements the [Alternative](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Applicative.html#t:Alternative) and [MonadFail](https://hackage.haskell.org/package/base-4.14.1.0/docs/Control-Monad-Fail.html#t:MonadFail) typeclasses, so it has some notion of failing or being empty. That means we can do neat things like attempt to parse some characters, and then roll back to the last 'good' character if we failed. It's _awesome_!

To start off, we need to convert a series of two-digit hex numbers into a string of binary digits. That's our first parser:
```haskell
import Numeric.Lens ( binary, hex )
import Control.Lens ((^?), (^?!), (^.), re)
hexLookup :: Char -> String
hexLookup = pad0 4 . toBinaryString . fromHex . singleton

fromHex :: Integral a => String -> a
fromHex str = str ^?! hex

toBinaryString :: Integral s => s -> String
toBinaryString x = x ^. re binary

pad0 :: Int -> String -> String
pad0 targetLen str = replicate times '0' ++ str
  where times = targetLen - length str

parseInput :: Parser String
parseInput = do
  binaryNumbers <- some $ hexLookup <$> hexDigit
  pure $ concat binaryNumbers
```
The convenient but often complicated `Control.Lens` package has some convenience functions for converting between binary and hex.

Our model will look something like this:
```haskell
data Packet
  = PacketLiteral
      { version :: Integer
      , value   :: Integer
      }
  | PacketOperator
      { version :: Integer
      , typeId  :: TypeId
      , packets :: [Packet]
      }
  deriving (Eq, Show)

type TypeId = Finite 8
```
Finite is a nice library. TypeId is not allowed to be 8 or above.

Next, let's start off simple. We need to be able to parse a 'literal' packet - that's one where the `typeId` is 4.
```haskell
parsePacketLiteral :: Parser Packet
parsePacketLiteral = do
  version <- toDecimal <$> count 3 digit
  typeId <- toDecimal <$> count 3 digit
  guard $ typeId == 4
  groups <- toDecimal <$> parseGroups
  pure $ PacketLiteral version groups

parseGroups :: Parser String
parseGroups = do
  first <- digit
  case first of
    '0' -> count 4 digit
    '1' -> do
      thisGroup <- count 4 digit
      (thisGroup ++) <$> parseGroups
    _ -> fail $ "Unexpected non-binary digit" ++ show first
```
One of the things that is so nice about this model is the idea of the parser 'consuming' characters as it parses. Ironically, the code reads very imperatively, even though it's still pure and functional. We first parse a version, then a typeId. Then we use `guard` to check that the type id is 4 (if it's not that guard will invoke `fail` to cause the parser to fail). `ParseGroups` uses recursion to keep parsing groups of 4 digits until we get the signal to stop (the '0' indicating the last group).

The operator parsing is a little more complicated. This concept of a `LengthTypeID` means we might be parsing a specific number of subpackets, or a specific number of characters. With Trifecta, I couldn't find a nice way of doing a specific number of characters except for creating a whole new parser and handling the `Result` of the inner parser explicitly. A specific number of packets is easy though, because we just use `count`.
```haskell
data LengthTypeID
  = NumBits Integer
  | NumPackets Integer
  deriving (Eq, Show)

parsePacketOperator :: Parser Packet
parsePacketOperator = do
  version <- toDecimal <$> count 3 digit
  typeId <- finite . toDecimal <$> count 3 digit
  guard $ typeId /= 4
  lengthTypeId <- digit >>= lengthLookup
  subPackets <-
    case lengthTypeId of
      NumBits n    -> parseSection n
      NumPackets n -> count (fromInteger n) parsePacket
  pure $ PacketOperator version typeId subPackets
  where
    lengthLookup l
      | l == '0' = NumBits . toDecimal <$> count 15 digit
      | l == '1' = NumPackets . toDecimal <$> count 11 digit
      | otherwise = fail "Unexpected non-binary digit"
    parseSection lengthSubpackets = do
      sectionToParse <- count (fromInteger lengthSubpackets) digit
      let result = parseString (some parsePacket) mempty sectionToParse
      foldResult
        (\errInfo -> fail ("Inner parser failed" ++ show errInfo))
        pure
        result
```
This one starts off similarly. It then uses our `LengthTypeID` data type to figure out if we're parsing a specific number of bits or a specific number of packets. The `parseSection` internal function uses `parseString` to invoke a whole new parser, and `foldResult` to handle the result. Both branches will use `parsePacket` which we haven't defined yet. Don't worry, that's the next thing!
```haskell
parsePacket :: Parser Packet
parsePacket = do
  try parsePacketLiteral <|> try parsePacketOperator
```

Just two new concepts to understand here. `try` will take a parser and 'rollback' the curser if the parser failed. `<|>` is the shorthand for `Alternatives`. The TLDR is it will use the `Alternative` on the left if it's not empty, otherwise it will use the one on the right.

That's actually all we need to do the parsing. Okay, it's not a one-liner, but I would argue that if it weren't for that 'parse a specific number of bits' requirement it would look really simple. Part 2 of the puzzle is to evaluate all the packets according to their `typeId`, because we have now got a simple expression syntax (type id 0 means sum the inner packets together etc). This bit is pretty trivial:
```haskell
resolvePacket :: Packet -> Integer
resolvePacket (PacketLiteral version value) = value
resolvePacket pk@(PacketOperator version typeId packets) =
  case typeId of
    0 -> multiPacketOp sum pk
    1 -> multiPacketOp product pk
    2 -> multiPacketOp minimum pk
    3 -> multiPacketOp maximum pk
    5 -> twoPacketOp (>) pk
    6 -> twoPacketOp (<) pk
    7 -> twoPacketOp (==) pk
    _ -> error ("unexpected operation: " ++ show typeId)

twoPacketOp :: (Integer -> Integer -> Bool) -> Packet -> Integer
twoPacketOp fun (PacketOperator version typeId [packet1, packet2]) =
  (toInteger . fromEnum) $ resolvePacket packet1 `fun` resolvePacket packet2
twoPacketOp _ pk = error ("unexpected twoPacketOp on packet: " ++ show pk)

multiPacketOp :: ([Integer] -> Integer) -> Packet -> Integer
multiPacketOp fun packet =
  case packet of
    PacketLiteral _ value      -> value
    PacketOperator _ _ packets -> fun $ map resolvePacket packets
```
Pattern matching allows us to use `resolvePacket` recursively until we hit a literal packet.
I thoroughly enjoyed this puzzle. It feels like it was written with parser combinators in mind. I dread to think how complicated it would have been in a language without them.

### Day 17
Mechanics was not my strong suit in maths at school. I dimly remembered that there are formulae for calculating the trajectory of an object through space, but after a bit of googling I gave up and decided to just implement this one naively.

I did later find out that part 1 can be solved with an insanely simple calculation. Along the y axis, the probe's descent from its max height can be solved using triangle numbers! After all, it starts by moving 1 unit, then 2, then 3 and so on, and that's what triangle numbers are. So a simple way to do part 1 is assume that the max height will always be same no matter how far the target area is from the starting location on the x axis, then just get the triangle number of the max y value in the target area! Astonishingly simple!

That's not what I did though. I used a beautifully elegant language to solve the problem in a very unimaginative way. 
```haskell
stepVelocity :: V2 Int -> V2 Int
stepVelocity (V2 x y)
  | x == 0 = V2 x (y - 1)
  | x > 0 = V2 (x - 1) (y - 1)
  | otherwise = V2 (x + 1) (y - 1)
```
This is how the probe moves. Y is constantly decreasing using triangle numbers. X is decreasing towards 0 then stopping.

We need to be able to tell if the probe is in the target area, or indeed passed the target area completely:
```haskell
inTargetArea :: Point -> TargetArea -> Bool
inTargetArea (V2 x y) (V2 xmin ymin, V2 xmax ymax) = inX x && inY y
  where
    inX val = allPred [(>= xmin), (<= xmax)] val
    inY val = allPred [(>= ymin), (<= ymax)] val

passedTargetArea :: Point -> TargetArea -> Bool
passedTargetArea (V2 x y) (V2 xmin ymin, V2 xmax ymax) = passedX x || passedY y
  where
    passedX val = val > xmax
    passedY val = val < ymin
```

Finally, we need to be able to get the list of points hit from a single starting point, by adding the velocity to that point until we're in or passed the target area. What does that sound like? An `unfold`!
```haskell
type Velocity = V2 Int

type ProbeState = (Point, Velocity)

fireProbe' :: Velocity -> TargetArea -> [Point]
fireProbe' vel targetArea = unfoldr go (V2 0 0, vel)
  where
    go :: ProbeState -> Maybe (Point, ProbeState)
    go (current, vel@(V2 xVel yVel))
      | passedTargetArea current targetArea = Nothing
      | inTargetArea current targetArea = Nothing
      | otherwise = Just (nextPoint, next)
      where
        next@(nextPoint, _) = (current + vel, stepVelocity vel)
```
The issue with this is that we get a list of points (yay!) but we need to interrogate the final point in the list to figure out if we succeeded in hitting the target area. Not ideal, especially considering we've already done that calculation to figure out if we should stop the `unfold` operation. What we can do instead is embed the notion of failure in the the unfold itself, by using `unfoldrM` with the `Maybe` monad. That way, if we hit the target area we get our list of points, but if we didn't then we just get a big 'ol `Nothing`. Like so:
```haskell
fireProbe :: Velocity -> TargetArea -> Maybe [Point]
fireProbe vel targetArea = unfoldrM go (V2 0 0, vel)
  where
    go :: ProbeState -> Maybe (Maybe (Point, ProbeState))
    go (current, vel@(V2 xVel yVel))
      | passedTargetArea current targetArea = Nothing
      | inTargetArea current targetArea = pure Nothing
      | otherwise = pure $ Just (nextPoint, next)
      where
        next@(nextPoint, _) = (current + vel, stepVelocity vel)
```
Now `fireProbe` will only be a `Just` if we actually hit the target area. Finally, we can use `mapMaybe` to get only the `Just` values out of our possible trajectories, and ignore the `Nothing` values.
```haskell
vectorRange :: TargetArea -> [[V2 Int]]
vectorRange ta@(V2 xmin ymin, V2 xmax ymax) = mapMaybe (`fireProbe` ta) vRange
  where
    xRange = filter (\x -> triangleX x >= xmin) [1 .. xmax] --if the corresponding triangle number is not big enough, then our probe will never get far enough
    yRange = [ymin .. abs ymin]
    vRange = [V2 x y | x <- xRange, y <- yRange]
```
Nice to have a break in the difficulty curve! I think this puzzle was easier than the other ones.

### Day 18
This one was definitely my favourite puzzle so far! The story of snailfish having a different arithmetic system reminded me a lot of [one of the puzzles last year](https://adventofcode.com/2020/day/18) where you have to help a kid with their maths homework.
In this puzzle, snailfish numbers are always expressed as 'pairs', where each member of the pair can be a literal number, or another pair. Here are some of the examples from the puzzle:
```
[[1,2],3]
[[[[[9,8],1],2],3],4]
[7,[6,[5,[4,[3,2]]]]]
[[6,[5,[4,[3,2]]]],1]
```

In my view, displaying the number system like this is a bit misleading. It looks like a nested array, but actually, it's an infinite data structure called a [rose tree](https://en.wikipedia.org/wiki/Rose_tree). In fact, it's a simplified rose tree, because while rose trees can have any number of branches, this tree can only ever have two at each node. Here is my preferred way of thinking about the numbers above:


![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/simple.png)

 
![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/left_tree.png)

 
![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/right_tree.png)

 
![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/both_sides.png)


That means that modelling the way these pairs work is actually very simple. We can use an infinite data structure:
```haskell
data Tree
  = Pair Tree Tree
  | Leaf Integer
  deriving (Eq)
```
If you've done any Haskell tutorials, you will almost certainly have written a data type like this when learning about applicatives and functors, except that instead of insisting that the leaves of the trees are integers, you will have made it a higher-kinded data type. 

Snailfish addition is pretty easy. You just create a new pair out of both elements. We can do that by literally calling the data constructor for `Pair`
```haskell
add :: Tree -> Tree -> Tree
add = Pair
```

The thing that makes this puzzle tricky is the way snailfish numbers simplify, or 'reduce'. The rule is: "If any pair is nested inside four pairs, the leftmost such pair explodes." By 'explode' we mean that the left value is added to the first regular number to the left of the pair, and the right value is added to the first regular number to the right (If there is no number to add to, we just drop it entirely). So for example, 
[[[[[9,8],1],2],3],4] -> [[[[0,9],2],3],4]
The 9 has nowhere to go, so we drop it, and the 8 gets added to the 1 on the right of it. As a rose tree, it looks like this:

![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/exploded_left.png)

This kind of thing is surprisingly tricky! We can use plane old recursion to find pairs which are nested four levels deep, but once we've found that pair it's impossible to do anything sensible. Go ahead and try it if you don't believe me!
```haskell
explode' :: Tree -> Tree
explode' = go 0
  where
    go depth tree
      | depth == 4 =
        case tree of
          Leaf number        -> undefined -- What do I do with this number?
          Pair left right -> undefined -- What do I do with these subtrees?
      | otherwise = go (depth + 1) tree
```
By the time we've recursed to somewhere useful, we've lost sight of the rest of the tree, which is bad because we need access to the rest of it in order to figure out where to put our 'exploding' numbers. Fortunately, there's a concept which solves this exact problem: [zippers](https://en.wikipedia.org/wiki/Zipper_(data_structure))! 
I learned about zippers [here](http://learnyouahaskell.com/zippers).

The idea behind a zipper is that it is a data type we can use to travel around infinite data structures while keeping track of the rest of the structure. What we do is leave a trail of 'breadcrumbs' which tells us A: what directions we took to get to where we are now (the 'focus') and B: what parts of the infinite structure we ignored on the way. It's easier if you see one, I promise!
```haskell
data Direction
  = LEFT
  | RIGHT
  deriving (Eq, Show, Enum, Bounded)

data Crumb =
  Crumb
    { _direction :: Direction
    , _tree      :: Tree
    }
  deriving (Eq, Show)

type Breadcrumbs = [Crumb]

type Zipper = (Tree, Breadcrumbs)
```
So a zipper is just a tuple where the first value is the subtree or the 'focus', and the second value is a list of breadcrumbs for how we got to that focus. So now we can start writing functions for travelling through our tree. For example:
```haskell
zipDown :: Direction -> Zipper -> Maybe Zipper
zipDown direction (Pair l r, bs) =
  case direction of
    LEFT  -> Just (l, Crumb LEFT r : bs)
    RIGHT -> Just (r, Crumb RIGHT l : bs)
zipDown _ (Leaf _, _) = Nothing
```
Given a direction and a zipper, we return a new zipper where we have gone one step in that direction. Notice how we always take the subtree we ignored and put it in a breadcrumb (so if we went left, we store the subtree on the right). The thing that makes this so powerful is that we can use a zipper to retrace our steps. So we can zip back up to where we previously were in the tree:
```haskell
zipUp :: Zipper -> Maybe Zipper
zipUp (tree, bc:rest) =
  case bc of
    Crumb LEFT subTree  -> Just (Pair tree subTree, rest)
    Crumb RIGHT subTree -> Just (Pair subTree tree, rest)
zipUp (_, []) = Nothing
```
Here we use a case statement for the first breadcrumb in the list, and simply return create a new zipper using the focus we just came from and the subtree from the breadcrumb. We can zip all the way to the top of a tree like this:
```haskell
zipToTop :: Zipper -> Tree
zipToTop zipper@(tree, [])      = tree
zipToTop zipper@(tree, bc:rest) = fromJust $ zipToTop <$> zipUp zipper
```
This one doesn't need to return a zipper - the breadcrumb list would always be empty if it did.
So let's go over what we actually need to do in order to 'explode' a pair. This might be easier if you follow along the diagram for `[[6,[5,[4,[3,2]]]],1]`.

![alt ""](https://github.com/RaphaelColman/AdventOfCode2021/blob/main/res/graphs/both_sides.png)


Imagine what we have to deal with the number '2' in the nested pair. We know, intuitively, that it has to be added to the '1' at the end. but how would you encode that in an algorithm? The way to think about it is that we need to the next 'right-facing' branch along from ours, and then find the leftmost leaf on that branch. So we travel up the tree until the first breadcrumb where we went left. That one is important, because it means there was a right-branch to travel down which we didn't take. Then we travel down that one and just carry on going left until we hit a leaf. Here's the code to do it:
```haskell
neighbour :: Direction -> Zipper -> Maybe Zipper
neighbour direction zipper@(tree, bc) =
  iterateUntilM
    (\z -> previousDirection z == Just oppositeDirection)
    zipUp
    zipper >>=
  zipUp >>=
  zipDown direction >>=
  iterateUntilM isLeaf (zipDown oppositeDirection)
  where
    oppositeDirection = enumNext direction

isLeaf :: Zipper -> Bool
isLeaf (Leaf i, _) = True
isLeaf _           = False

previousDirection :: Zipper -> Maybe Direction
previousDirection (_, (Crumb direction _):_) = Just direction
previousDirection _                          = Nothing
```
Quick reminder: `>>=` allows us to chain multiple monadic functions together. So if you want to call lots of different things which return a `Maybe`, you can use `>>=` to `flatmap` each value into the next function.

This solution is generic for both directions. `iterateUntilM` will travel up the tree until we find a branch which is opposite to the direction we wish to go. Then we zip up one more time to get to the subtree containing the new branch. Then we travel down the right branch of that tree (the new branch), and then use `iterateUntilM` to keep going left until we hit a leaf. If any of the steps in this chain of actions returns a `Nothing`, then we'll just get a `Nothing` at the end.

So now we have the logic for finding our nearest neighbour for any given direction, the rest is actually quite straightforward - provided we use our zippers. We need that recursive logic to find pairs which are nested four levels deep in the tree:
```haskell
toFirstExplodable :: Tree -> Maybe Zipper
toFirstExplodable tree = go 0 (tree, [])
  where
    go :: Integer -> Zipper -> Maybe Zipper
    go depth zipper@(tree, bs)
      | depth == 4 =
        case tree of
          Leaf _   -> Nothing
          Pair l r -> Just zipper
      | otherwise = do
        let leftBranch = zipDown LEFT zipper >>= go (depth + 1)
        let rightBranch = zipDown RIGHT zipper >>= go (depth + 1)
        leftBranch <|> rightBranch

```
Here, we return a zipper if we've reached a depth of 4 AND the focus of the zipper we're on is a pair, not a leaf. If we haven't met both of those conditions, we define a trip down the left branch and a trip down the right branch, and use `Alternative` (`<|>`) to prefer the one on the left. (When used with `Maybe`, `<|>` will return the value on the left if it is a `Just`, otherwise it will return the value on the right (whatever it is)).

Finally the function we can call to 'explode' a snailfish number:
```haskell
explode :: Tree -> Maybe Tree
explode tree = do
  zipper@(Pair (Leaf l) (Leaf r), bs) <- toFirstExplodable tree
  pure $
    modifyZipper (Leaf 0) zipper & zipToTop & carry l zipper LEFT &
    carry r zipper RIGHT
  where
    carry value zipper direction tree =
      fromMaybe tree $ do
        neighbourZipper <- neighbour direction zipper
        addAtLeaf value (extractDirections neighbourZipper) tree

extractDirections :: Zipper -> [Direction]
extractDirections (_, bs) = reverse $ map _direction bs

addAtLeaf :: Integer -> [Direction] -> Tree -> Maybe Tree
addAtLeaf toAdd directions tree = do
  zipper@(atSubTree, bs) <- foldlM (flip zipDown) asZipper directions
  case atSubTree of
    Leaf i   -> pure $ modifyZipper (Leaf (i + toAdd)) zipper & zipToTop
    Pair _ _ -> Nothing
  where
    asZipper = (tree, [])

modifyZipper :: Tree -> Zipper -> Zipper
modifyZipper newValue (tree, bs) = (newValue, bs)
```
This is actually my third attempt to make it neater. I originally had a version which used the state monad in order ot manage the state of the tree. Let's just talk through what this does. We define an internal function called 'carry', which takes a value, a zipper where the focus is the pair we want to explode, a direction and a tree to modify. Carry will use the logic we wrote earlier to 'carry' the exploding value in [one direction.](https://media.giphy.com/media/KD2qs04MOsmic/giphy.gif). It uses `fromMaybe` to just default to the original tree if carrying returinged a Nothing (ie, there was no leaf to add our number to).

For the function proper, we create a zipper representing the first explodable pair, then chain a sequence of 'carries' together (one for the left, one of the right). In case you haven't seen it before, the `&` character is pretty useful for this sort of thing. It's infix, and will apply the function on the right to whatever value is on the left. Chaining them together is like function composition (`.`) except it allows you to write your functions from left-to-right rather than right-to-left.

Believe or not, the hard part is over. The other operation we have to perform is a 'split'. The rule is: "If any regular number is 10 or greater, the leftmost such regular number splits". To split a regular number, replace it with a pair; the left element of the pair should be the regular number divided by two and rounded down, while the right element of the pair should be the regular number divided by two and rounded up. For example, 10 becomes [5,5], 11 becomes [5,6], 12 becomes [6,6], and so on.

This is much easier to achieve. There's no tricky logic required to figure out what it means to find the nearest neighbour to a number. We just need a function to find the first splittable number, then another one to replace that leaf with a pair. I decied to make the first function return a zipper to be consistent with the approach we took to exploding.

```haskell
toFirstSplittable :: Tree -> Maybe Zipper
toFirstSplittable tree = go (tree, [])
  where
    go :: Zipper -> Maybe Zipper
    go zipper@(Leaf i, bs)
      | i >= 10 = Just zipper
      | otherwise = Nothing
    go zipper@(tree, bs) = do
      let leftBranch = zipDown LEFT zipper >>= go
      let rightBranch = zipDown RIGHT zipper >>= go
      leftBranch <|> rightBranch

split :: Tree -> Maybe Tree
split tree = do
  zipper@(Leaf l, bs) <- toFirstSplittable tree
  let (lft, rt) = splitInteger l
  pure $ modifyZipper (Pair (Leaf lft) (Leaf rt)) zipper & zipToTop

splitInteger :: Integer -> (Integer, Integer)
splitInteger i =
  let divided = i `div` 2
   in (divided, i - divided)
```

Finally, the puzzle introduces the idea of a 'magnitude'. The magnitude of a pair is just 3 * left + 2 * right. This is pretty simple:
```haskell
magnitude :: Tree -> Integer
magnitude = go
  where
    go (Leaf i)     = i
    go (Pair p1 p2) = (3 * go p1) + (2 * go p2)

part1 :: [Tree] -> Integer
part1 = magnitude . sumTrees

part2 :: [Tree] -> Integer
part2 = maximum . map (magnitude . sumTrees) . variate 2
```
The `variate` funtion is from the excellent `combinatorial` library. In this case, it will get every possible pair of elements from a list.
I think this puzzle lent itself to haskell quite well. Infinite data structures are easy to do, and I think immutability forces you to break each operation into simple chunks which are easier to understand.

### Day 19
Phew! This is the puzzle where I fell of the wagon of doing them every day. It's amazing how your motivation drops after that. If memory serves, it took me until just before Christmas to get around to solving this one. Ah well! Before I go on: I confess I referred to the excellent solutions in Dart in [this repo](https://github.com/alexburlton/advent-of-code-beleaguered-badger) written by a friend of mine for inspiration for some of the puzzles from here onwards. This is one of those puzzles.
The idea here is that we have a list of 'scanners' in 3d space, all of which can see a set of 'beacons'. The 'catches' are:
1. You don't know where the scanners are relative to each other
2. There are some beacons which can be seen by multiple different scanners (so their sets intersect)
3. The beacons are not all facing the same direction. They can be rotated in any of the [24 orientations you can rotate a cube in](https://garsia.math.yorku.ca/~zabrocki/math4160w03/cubesyms/).

The rule set out by the puzzle is that if you take two scanners and, after rotating both of them in all permutations, you find that they describe _at least 12_ common beacons, you can safely identify that as an overlapping area. Put another way, if at least 12 beacons are the same vector from each other according to both scanners, they must be the same beacons. Our task is to figure out how many beacons there are in total

The way I eventually went about this was:
1. Take one scanner and use it as the 'origin' (so its location is 0,0,0).
2. Put all the other scanners in in a queue.
3. Pick a scanner out of the queue. See if you can identify 12 overlapping beacons (they will all be the same distance from each other). If you can't, rotate the scanner one of the 23 remaining orientations.
4. If you still can't find 12 overlapping beacons, put this scanner in the back of the queue and try the next one.
5. If you could find 12 overlapping beacons, use this orientation to plot the new scanner relative to the origin. It's distance to the origin will be the same as the distance of any of the overlapping beacons to their counterpart in the first scanner's set.

Ok, still quite tricky to be honest, but it's a start.
The first quite useful function to define is the orientations of a cube:
```haskell
--x = forward/backwards
--y = up/down
--z = left/right
allOrientations :: Num a => V3 a -> [V3 a]
allOrientations v = concatMap allRotations allFacings
  where
    backwards (V3 x y z) = V3 (-x) y (-z)
    up (V3 x y z) = V3 y (-x) z
    down (V3 x y z) = V3 (-y) x z
    left (V3 x y z) = V3 (-z) y x
    right (V3 x y z) = V3 z y (-x)
    clockwise90 (V3 x y z) = V3 x z (-y)
    clockwise180 (V3 x y z) = V3 x (-y) (-z)
    clockwise270 (V3 x y z) = V3 x (-z) y
    allFacings = map (\f -> f v) [id, backwards, up, down, left, right]
    allRotations v' =
      map (\f -> f v') [id, clockwise90, clockwise180, clockwise270]
```
This mouthful of a function will take any point in space and convert it to a list of equivalent points if you, (the origin) were to rotate in any of the 24 orientations of a cube. So for example, the point (1,2,3) would become (3, 2, -1) if you were to rotate yourself to the right by 90 degrees (using x as forward/backwards, y as up/down and z as left/right). We get all 24 by combining all the 6 directions you can face with all 4 rotations.

As usual, it makes sense to create some models:
```haskell
type Point = V3 Integer

data Scanner =
  MkScanner
    { _beacons  :: S.Set Point
    , _location :: Point
    }
  deriving (Eq, Show, Ord)

data ScannerQueue =
  MkSQ
    { _found     :: [Scanner]
    , _remaining :: Seq.Seq Scanner
    }
  deriving (Eq, Show)
```

Before considering the solution it code, it helps to visualise how we might compare to scanners. See we are comparing an origin scanner (which is never going to be moved or rotated) and a 'new' scanner. First, we create all the 24 orientations of that new scanner (so we get 24 sets of beacons). But it's still not enough to just look for overlapping coordinates, because the beacons themselves might be different distances from their respective scanner. We need to normalise those distances, so for each rotated scanner we need to consider every possible 'transposition' of that scanner - ie that scanner moved so at at least one of its points is the same coordinate as one of the points on the origin scanner. Only then can we be sure we will spot the 12 overlapping beacons if they exist. After rotating and transposing for each orientation, we can now say the beacons are overlapping if they have the same coordinates.

```haskell
relativeNeighbour :: Scanner -> Scanner -> Maybe Scanner
relativeNeighbour base scanner = do
  let allRotations = rotateScanner scanner
  headMay $ mapMaybe (compare' base) allRotations
  where
    compare' base'@(MkScanner basePoints baseLoc) scanner'@(MkScanner points loc) =
      let allTranspositions =
            S.map (uncurry (-)) $ S.cartesianProduct basePoints points
          allTransposedScanners =
            S.map (transposeScanner scanner') allTranspositions
       in find
            (\(MkScanner points loc) ->
               length (S.intersection points basePoints) >= 12)
            allTransposedScanners

transposeScanner :: Scanner -> V3 Integer -> Scanner
transposeScanner (MkScanner points location) transposition =
  MkScanner (S.map (+ transposition) points) (location + transposition)

rotateScanner :: Scanner -> [Scanner]
rotateScanner (MkScanner points location) =
  map (flip MkScanner (V3 0 0 0) . S.fromList) $
  transpose $ map allOrientations $ S.toList points
```
The stage we're at in AoC now is suitably advanced that the challenge is not in writing Haskell, but just solving the puzzle at all. The only things of note here:
* `headMay` is a safe version of `head`. It will attempt to get the first item from a list, but return a `Nothing` if the list is empty.
* The contract of ths function is to return a `Just Scanner` if it found enoug overlapping beacons. The scanner it returns will be rotated and transposed to the orientation where the beacons overlapped (so it will already be 'plotted' relative to the origin)

So now we just have to write the queue:
```haskell
runQueue :: ScannerQueue -> Maybe [Scanner]
runQueue sq@(MkSQ found remaining)
  | null remaining = pure found
  | otherwise = stepQueue sq >>= runQueue

stepQueue :: ScannerQueue -> Maybe ScannerQueue
stepQueue (MkSQ found remaining) = do
  tryScanner <- remaining Seq.!? 0
  let restOfList = Seq.drop 1 remaining
  let orientated = headMay $ mapMaybe (`relativeNeighbour` tryScanner) found
  case orientated of
    Just o  -> pure $ MkSQ (o : found) restOfList
    Nothing -> pure $ MkSQ found (restOfList Seq.|> tryScanner)
```
In case you haven't seen it before: The `|>` operator is for `Sequences`, and will append items to the end of a sequence (so in this case it will send a scanner to the back fo the queue).
This puzzle involved much more thinking that coding. I'm glad I got all the orientations right - it would have been really hard to debug if not.

### Day 20
To be honest, I wasn't so keen on this puzzle. The 'puzzle-bit' felt more like a sneaky trap than a satisfying puzzle. You'll see what I mean when we get to the end. The puzzle asks us to implement an an algorithm. You're given a starting image (made of, as per usual '.' to represent dark pixels and '#' characters to represent light pixels) and something called an 'image enhancement algorithm', which is just a 512-length list of '.' and '#' characters. For each pixel in the starting image, you have to create a binary number out of it and its neighbours (starting top-left, finishing top-right) where dark pixels are 0 and light pixesl are 1. You then look up the corresopnding character in the image enhancement algorithm and that will be the value for the pixel in the output image. You can iterate this process as many times as you like, bearing in mind the starting image is actually infinitely big, where all unknow points are dark pixels.

Implementing this is fairly straightforward. We can make the IEA a set of points (just the light pixels). We could probably have done the same thing for the image itself, but I didn't bother, I just made it a map of coordinate -> pixel value
```haskell
type IEA = S.Set Integer

initIEA :: [Pixel] -> IEA
initIEA pixels =
  S.fromList $ map snd . filter ((== LIGHT) . fst) $ zip pixels [0 ..]

data Pixel
  = DARK
  | LIGHT
  deriving (Enum, Ord, Show, Eq)

type LightPixels = S.Set (V2 Int)

type Input = (Grid Pixel, IEA)

data ImageState =
  MkState
    { _grid      :: Grid Pixel
    , _iea       :: IEA
    , _iteration :: Integer
    }
  deriving (Eq, Show)
```
The reason I'm keeping track of 'iteration' will become clear soon.
The only difficulty in implementing this is working out what to do with pixels on the edge of our known region. The IEA in the test input starts off like this:
```
..#.#..#####
```
The first character is a light pixel. That means we can assume that any light pixel entirely surrounded by light pixels will remain light after being enhanced. That's because they'll form the number 0, which is also a light pixel. You can see this in the examples - all the surrounding pixels which are light just stay light. _However_, if you look at the actual test input, it starts off like this:
```
###.#####.
```
Sneaky! That means any light pixel surrounded by light pixels will flip to being dark. That's why I've included `_iteration` as one of the fields. For unknown pixels, we can reason that they will be light on even-numbered iterations and dark on odd-numbered ones.

Our `enhance` function will look something like this:
```haskell
enhance :: Pixel -> Grid Pixel -> IEA -> V2 Int -> Pixel
enhance default' grid iea point =
  if decimal `S.member` iea
    then LIGHT
    else DARK
  where
    pts = sortBy orderPoints $ S.toList $ S.insert point (neighbours point)
    decimal = toDecimal $ map (flip (M.findWithDefault default') grid) pts

orderPoints :: Point -> Point -> Ordering
orderPoints (V2 x1 y1) (V2 x2 y2) =
  case compare y1 y2 of
    EQ -> compare x1 x2
    o  -> o

toDecimal :: (Enum e) => [e] -> Integer
toDecimal = sum . (zipWith (*) [2 ^ n | n <- [0,1 ..]]) . reverse . asIntList
  where
    asIntList :: (Enum e) => [e] -> [Integer]
    asIntList = map (toInteger . fromEnum)
```
We have a custom `Ordering` function so that when we get all the neighbours of a point, they are ordered from top-left to bottom-right. We then convert that set of neighbouring points to a number by using the enum value of the pixel (Dark is 0 because it is the first enum). The `enhance` function takes a 'default' as a parameter, so we can control whether unknow pixels are light or dark.

As usual, we can create a loop like this by simply defining how we step from one `ImageState` to the next one:
```haskell
stepImageState :: ImageState -> ImageState
stepImageState (MkState grid iea iteration) =
  ($!) MkState enhanced iea (iteration + 1)
  where
    enhanced = M.mapWithKey enhance' $ expand default' grid
    enhance' point _ = enhance default' grid iea point
    default' =
      if even iteration
        then DARK
        else LIGHT

expand :: Pixel -> Grid Pixel -> Grid Pixel
expand default' grid =
  M.fromList $ map (\p -> (p, M.findWithDefault default' p grid)) newRange
  where
    keys = M.keysSet grid
    (V2 xtl ytl) = minimumBy orderPoints keys
    (V2 xbr ybr) = maximumBy orderPoints keys
    newRange = [V2 x y | x <- [xtl - 2 .. xbr + 2], y <- [ytl - 2 .. ybr + 2]]
```
This function will enhance the existing grid by mapping over all of its keys and using the `iteration` to control whether default pixels are light or dark. The `expand` function is quite important. The idea is that each enhancement will make the 'known' grid grow in area, so each iteration needs to consider an area of points slightly bigger than the last one. To be precise, we need to consider all the points exactly two pixels to the left, right, top and bottom of the existing grid. That way, we get all the 'unknown' pixels which still resolve to a non-zero value when enhanced.

And finally:
```haskell
runInput :: Int -> Input -> Grid Pixel
runInput times (grid, iea) =
  _grid $ iterate stepImageState initialState !! times
  where
    initialState = MkState grid iea 0
```
Hmm.. Quite a fiddly puzzle in my opinion. Ah well! They can't all be winners.

### Day 21
This one was a head-spinner! I spent quite a long time just trying to decipher what the puzzler meant. Well, what part 2 meant. Let's quickly bash out part 1 so we can get on to [the good stuff.](https://media.giphy.com/media/510KemnengFPi/giphy.gif)
We're playing a deterministic game of dice. The rule is, each player rolls three dice, adds the values together and moves that number of spaces along the board. The board wraps back around to 1 after 10. They add the value of the space they land on to their current score. Simple! Even simpler because there's no randomness. The dice are deterministic.
```haskell
data Player =
  MkPlayer
    { _value :: Integer
    , _score :: Integer
    }
  deriving (Eq, Show, Ord)

data Game =
  MkGame
    { _player1  :: Player
    , _player2  :: Player
    , _rolls    :: [Integer]
    , _numRolls :: Integer
    }
  deriving (Eq)

initGame :: Players -> Game
initGame (player1, player2) =
  MkGame (MkPlayer player1 0) (MkPlayer player2 0) rolls 0
  where
    rolls = map sum $ chunksOf 3 $ cycle [1 .. 100]
```
So each `Player` keeps track of where they are on the board (value) and their current score. In the `Game` type, we make `rolls` an infinite list of all the dice values we will ever have so we don't need to do any fiddly logic to figure out what the next dice rolls are. We also keep track of the total number of rolls we've had in the game, because we need that to multiply that by the score of the losing player.

We define how to get from one game state to the next one:
```haskell
incrementValue :: Integer -> Integer -> Integer
incrementValue value amount =
  let total = (value + amount) `mod` 10
   in if total == 0
        then 10
        else total

step :: Game -> Game
step (MkGame (MkPlayer value score) player2 (thisRoll:rest) numRolls) =
  let newValue = incrementValue value thisRoll
      newScore = score + newValue
   in MkGame player2 (MkPlayer newValue newScore) rest (numRolls + 3)
```
This is all pretty normal so far. The only thing to point out here is that the new state will have player1 and player2 swapped. That way, we can always use whichever player is in the `Player1` field as the player doing the dice rolling this turn.

A player has one when they reach a score of over 1000, but the puzzle wants us to get the loser's score, so we can simply define a function to extract the losing player's score from the game if there is one:
```haskell
loser :: Game -> Maybe (Integer, Integer)
loser (MkGame (MkPlayer _ score1) (MkPlayer _ score2) _ numRolls)
  | score1 >= 1000 = Just (score2, numRolls)
  | score2 >= 1000 = Just (score1, numRolls)
  | otherwise = Nothing
```
If there is no winner yet, we just return a `Nothing`. As usual, we can then just iterate by using recursion. We check for a loser, and if there isn't one we step the game state and try again:

```haskell
play :: Game -> Integer
play game =
  case loser game of
    Just (losingScore, numRolls) -> losingScore * numRolls
    Nothing                      -> step game & play
```

Now we get to the good bit! Part 2 changes the game - when you roll a 3-sided dice, you actually split your reality in to 3 separate universes, one which each possible value. You roll the dice three times, so all 3 versions of you will roll again, and then all 9 versions of you will roll to complete your turn. You'll have 27 universes at the end of the first turn! At the end of the other player's first turn, you'll have 3<sup>6</sup> = 729 different universes. 

Our task is to find which player wins in more universes, and how many universes they win in. Keeping track of every universe and all its children individually is pretty unworkable. I didn't bother trying it, but I suspect it would grind to a halt. The first insight I had is that even though one turn spawns 27 universes, many of those universes are actually identical. For example:

1 + 2 + 2 = 5\
2 + 2 + 1 = 5\
3 + 1 + 1 = 5\
...\
etc

So we can actually represent our dice rolls as a frequency map:
```haskell
diracDiceRolls :: M.Map Integer Integer
diracDiceRolls = freqs combos
  where
    combos = [sum [x, y, z] | x <- [1 .. 3], y <- [1 .. 3], z <- [1 .. 3]]
```
```
λ: diracDiceRolls 
fromList [(3,1),(4,3),(5,6),(6,7),(7,6),(8,3),(9,1)]
```
(so we get a total of 1 for 3 universes, a total of 3 for 4 universes etc). That means there are only 7 distinct universes, but with different counts which total 27.
The way to do this, then, is to adopt the same approach to keep track of all our turns. We'll have a frequency map of universes, where each universe is a representation of player state (player score and space) and the count is the number of universes that state is currently represented in. Then, as we update our map, we can keep checking which games have finished, and keep track of those as well.

First off: a universe will look like this:
```haskell
data Universe =
  MkU
    { _dPlayer1 :: Player --I've called it dPlayer so this field doesn't conflict with the Player function we generated earlier
    , _dPlayer2 :: Player
    }
  deriving (Eq, Show, Ord)
```
Then our map will look like this:
```haskell
type Universes = M.Map Universe Integer
```

And finally our player state will look like this:
```haskell
data DiracGame =
  MkDG
    { _universes   :: Universes
    , _firstPlayer :: Bool
    , _player1Wins :: Integer
    , _player2Wins :: Integer
    }
  deriving (Eq, Show)
```
Again, we can imagine 'stepping' the player state from one state to the next. Each step will simulate the different dice rolls, and update the map accordingly. Notice that we now need to keep track of whose turn it is using a boolean, whereas before we could just swap the places of the players.

Let's start with a `splitUniverse` function, which, given a universe, returns a map of new universes based on the dice rolls.
```haskell
newPlayer :: Integer -> Player -> Player
newPlayer value player =
  let newValue = incrementValue (_value player) value
      newScore = _score player + newValue
   in MkPlayer newValue newScore

splitUniverse :: Bool -> Universe -> Universes
splitUniverse isPlayer1 (MkU player1 player2) = M.mapKeys play diracDiceRolls
  where
    play value =
      if isPlayer1
        then MkU (newPlayer value player1) player2
        else MkU player1 (newPlayer value player2)
```
`newPlayer` is fairly self-explanatory. Given a player and value (dice roll), update the player's score and space. The `splitUniverse` function takes a boolean representing whether player1 is the current player and a current universe to split. It then creates map of universe to count by mapping over the keys of the `diracDiceRolls`

Now we can create a map of split universes from a single one, we can define how to step from one game state to the next. The way I think of this is that we need to fold through our current frequency map of universes. For each distinct kind of universe, we need to calculate the new universes and their counts, and multiply those counts by the original. In other words, say I have 2 universes where player 1 is on space 8 and has exactly 30 points. The 7 distinct new universes all have their counts multiplied by 2, because we had 2 to begin with.
```haskell
diracTurn :: DiracGame -> DiracGame
diracTurn (MkDG universes firstPlayer player1Wins player2Wins) =
  MkDG remaining (not firstPlayer) newP1Wins newP2Wins
  where
    newUniverses = M.foldrWithKey go M.empty universes
    go :: Universe -> Integer -> Universes -> Universes
    go universe count universes =
      let newUniverses = M.map (* count) $ splitUniverse firstPlayer universe
       in M.unionWith (+) newUniverses universes
    (finished, remaining) = partitionKeys partitionFinished (+) newUniverses
    newP1Wins = M.findWithDefault 0 PLAYER1 finished + player1Wins
    newP2Wins = M.findWithDefault 0 PLAYER2 finished + player2Wins

partitionFinished :: Universe -> Either Winner Universe
partitionFinished universe@(MkU (MkPlayer _ score1) (MkPlayer _ score2))
  | score1 >= 21 = Left PLAYER1
  | score2 >= 21 = Left PLAYER2
  | otherwise = Right universe


data Winner
  = PLAYER1
  | PLAYER2
  deriving (Eq, Show, Ord)
```
So this function will do exactly that. It calls `splitUniverse` on each universe, and multiplies all the counts in the result by the counts in the original. It then uses `M.unionWith` to add those universes to the map. The other thing it does is to check for games which have finished (by checking if the score is over 21) and extracts the winning player from those games. `partitionKeys` is a really useful function for this - it will divide the contents of a map into two maps based on the result from a function you give it which returns an `Either` (in this case, the `partitionFinished` function).

After that it's simple. As before, we define a recursive function in order to keep stepping the game state until there are no more universes to split, because they all consist of finished games.

```haskell
playDiracGame :: DiracGame -> Integer
playDiracGame game@(MkDG universes _ p1W p2W)
  | null universes = max p1W p2W
  | otherwise = diracTurn game & playDiracGame
```

I thought it was interesting that the puzzle description didn't step through an example for part 2. I think it was intentional - most of the puzzle was not really writing the code. It was trying to understand how to keep track of all those split universes. An example might have given the game away.

### Day 22
Here's the other puzzle where I have to credit out the author of [this repo](https://github.com/alexburlton/advent-of-code-beleaguered-badger). His solution is really clever, and I'm afraid I ended up reading it and just implementing in Haskell.

The task is to run a list of 'reboot steps' on a reactor core (a 3-dimensional space). Each instruction is the bounding coordinates of a cuboid with a toggle: 'on' or 'off'. For example:
```
on x=10..12,y=10..12,z=10..12
```
Will set all points enclosed by that cuboid to 'on'. If any points are already on then they will not change. We have to count the number of cubes which are on at the end.

My first pass at this puzzle was naive, but it worked for part 1. I just kept a set of all points that were 'on' and add or removed from the set accordingly with each instruction. That works for part 1, which restricts the area you have to deal with to only 1000000 points, so it's not much to keep track off. However, part 2 asks you to keep track of all possible points in the instruction set, which means the area gets too big to manage and that approach becomes impossibly slow.

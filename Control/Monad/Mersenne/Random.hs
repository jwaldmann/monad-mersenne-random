--------------------------------------------------------------------
-- |
-- Module    : Control.Monad.Mersenne.Random
-- Copyright : (c) Don Stewart 2010
--
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
--
-- A fast random number generator monad.
--

module Control.Monad.Mersenne.Random (

    -- * Random monad
    Rand(..),
    runRandom,
    evalRandom,

    -- * Efficient generators (Word64 is the primitive getter).
    getBool,
    getInt,
    getWord,
    getInt64,
    getWord64,
    getDouble,

    -- * Internals
    R(..),

    -- $example

 ) where

import Control.Monad
import Data.Word
import Data.Int
import System.Random.Mersenne.Pure64


-- | The state of a random monad, optimized for performance.
data R a = R !a {-# UNPACK #-}!PureMT

------------------------------------------------------------------------

-- | A basic random monad, for generating random numbers from pure mersenne twisters.
newtype Rand a = Rand { runRand :: PureMT -> R a }

instance Monad Rand where

    {-# INLINE return #-}
    return a = Rand $ \s -> R a s

    {-# INLINE (>>=) #-}
    m >>= k  = Rand $ \s -> case runRand m s of
                                R a s' -> runRand (k a) s'

    {-# INLINE (>>) #-}
    m >>  k  = Rand $ \s -> case runRand m s of
                                R _ s' -> runRand k s'

-- | Run a random computation using the generator @g@, returning the result
-- and the updated generator.
runRandom  :: Rand a -> PureMT -> (a, PureMT)
runRandom  r g = case runRand r g of R x g -> (x, g)

-- | Evaluate a random computation using the mersenne generator @g@.  Note that the
-- generator @g@ is not returned, so there's no way to recover the
-- updated version of @g@.
evalRandom :: Rand a -> PureMT -> a
evalRandom r g = case runRand r g of R x _ -> x

------------------------------------------------------------------------
-- Efficient 'get' functions.

getBool     :: Rand Bool
getBool     = Rand $ \s -> case randomInt s of (w,s') -> R (w < 0) s'

-- | Yield a new 'Int' value from the generator.
getInt      :: Rand Int
getInt      = Rand $ \s -> case randomInt s of (w,s') -> R w s'

-- | Yield a new 'Word' value from the generator.
getWord     :: Rand Word
getWord     = Rand $ \s -> case randomWord s of (w,s') -> R w s'

-- | Yield a new 'Int64' value from the generator.
getInt64    :: Rand Int64
getInt64    = Rand $ \s -> case randomInt64 s of (w,s') -> R w s'

-- | Yield a new 53-bit precise 'Double' value from the generator.
getDouble   :: Rand Double
getDouble   = Rand $ \s -> case randomDouble s of (w,s') -> R w s'

-- | Yield a new 'Word64' value from the generator.
getWord64   :: Rand Word64
getWord64   = Rand $ \s -> case randomWord64 s of (w,s') -> R w s'

------------------------------------------------------------------------
-- $example
--
-- An example from a user on Stack Overflow -- taking a random walk, and
-- printing a histogram.
--
-- > {-# LANGUAGE BangPatterns #-}
-- >
-- > import System.Environment
-- > import Text.Printf
-- > import Control.Monad.Mersenne.Random
-- > import System.Random.Mersenne.Pure64
-- >
-- > main :: IO ()
-- > main = do
-- >   (size:iters:_) <- fmap (map read) getArgs
-- >   let start = take size $ repeat 0
-- >   rnd <- newPureMT
-- >   let end = flip evalRandom rnd $ mapM (iterateM iters randStep) start
-- >   putStr . unlines $ histogram "%.2g" end 13
-- > 
-- > {-# INLINE iterateM #-}
-- > iterateM n f x = go n x
-- >     where
-- >         go 0 !x = return x
-- >         go n !x = f x >>= go (n-1)
-- > 
-- > randStep :: Double -> Rand Double
-- > randStep x = do
-- >     v <- getBool
-- >     return $! if v then x+1 else x-1
-- > 
-- >
-- > histogram :: String -> [Double] -> Int -> [String]
-- > histogram _ _ 0 = []
-- > histogram fmt xs bins =
-- >     let xmin = minimum xs
-- >         xmax = maximum xs
-- >         bsize = (xmax - xmin) / (fromIntegral bins)
-- >         bs = take bins $ zip [xmin,xmin+bsize..] [xmin+bsize,xmin+2*bsize..]
-- >         counts :: [Int]
-- >         counts = let cs = map count bs
-- >                  in  (init cs) ++ [last cs + (length $ filter (==xmax) xs)]
-- >     in  map (format (maximum counts)) $ zip bs counts
-- >   where
-- >     toD :: (Real b) => b -> Double
-- >     toD = fromRational . toRational
-- >     count (xmin, xmax) = length $ filter (\x -> x >= xmin && x < xmax) xs
-- >     format :: Int -> ((Double,Double), Int) -> String
-- >     format maxc ((lo,hi), c) = 
-- >         let cscale = 50.0 / toD maxc
-- >             hashes = take (round $ (toD c)*cscale) $ repeat '#'
-- >             label  = let los = printf fmt lo
-- >                          his = printf fmt hi
-- >                          l   = los ++ " .. " ++ his
-- >                          pad = take (20 - (length l)) $ repeat ' '
-- >                      in  pad ++ l
-- >         in  label ++ ": " ++ hashes
-- > 
--
-- Compiling this:
--
-- > $ ghc -O2 --make B.hs
--
-- And running it:
--
-- > $ time E 300 5000
-- >   -194.00 .. -164.46: 
-- >   -164.46 .. -134.92: #
-- >   -134.92 .. -105.38: ####
-- >    -105.38 .. -75.85: ###########
-- >     -75.85 .. -46.31: #########################
-- >     -46.31 .. -16.77: ##################################################
-- >      -16.77 .. 12.77: #################################################
-- >       12.77 .. 42.31: ###########################################
-- >       42.31 .. 71.85: ###########################
-- >      71.85 .. 101.38: ################
-- >     101.38 .. 130.92: #######
-- >     130.92 .. 160.46: #####
-- >     160.46 .. 190.00: #
-- > ./E 500 3000  0.03s user 0.00s system 96% cpu 0.035 total

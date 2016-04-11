{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad.Free

main :: IO ()
main = pretty program

data Toy b next =
    Output b next
  | Bell next
  | Done
  deriving (Functor)

output :: a -> Free (Toy a) ()
output x = liftF (Output x ())

bell :: Free (Toy a) ()
bell = liftF (Bell ())

done :: Free (Toy a) r
done = liftF (Done)


subroutine :: Free (Toy Char) ()
subroutine = output 'A'

program :: Free (Toy Char) r
program = do
    subroutine
    bell
    done

showProgram :: (Show a, Show r) => Free (Toy a) r -> String
showProgram (Free (Output c x)) = "output " ++ show c ++ "\n" ++ showProgram x
showProgram (Free (Bell x))     = "bell\n" ++ showProgram x
showProgram (Free Done)         = "done\n"
showProgram (Pure r)            = "return " ++ show r ++ "\n"

--pretty :: (Show a, Show r) => Free (Toy a) r -> IO ()
{- I think the reason this won't compile is that it can't deduce what r is.
   No idea why it would work in ghci, but not ghc.  -}
pretty :: (Show a) => Free (Toy a) () -> IO ()
pretty = putStr . showProgram

-- Question 1:

-- The following works:
-- stack ghci
-- λ: pretty program
-- output 'A'
-- bell
-- done
-- λ: :t pretty program
-- pretty program :: IO ()
--
-- But then `main = pretty program` fails to compile...?

-- Question 2

-- What are the practical differences/limitations between this use of Free, and the "Freer" approach described here?
-- http://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html

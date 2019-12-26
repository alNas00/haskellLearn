module Main where

import Data.Text (Text)
import Text.Pretty.Simple (pPrint)
import Text.InterpolatedString.QM (qnb)

import AST
import Parser
import Text.Megaparsec

-- test1 = [qnb| 
--      while (ar != 10):
--      if (ar % 2 == 0): 
--          ar += 2
--      elif 
--          ar += 1
--      count += 1   
-- |]


-- main :: IO ()
-- main = do
--     pPrint $ parseMaybe whileParser input
--     where
--         input  = [qnb|
--             while (ar != 10):
--               if (ar % 2 == 0): 
--                   ar += 2
--               elif 
--                   ar += 1
--               count += 1   
--         |]


main :: IO ()
main = do
  parseTest whileParser "if 3 > 4 and 4 == 4: a = 3 \n else a = 2"
  parseTest whileParser "while 3 > 2: a = 3"
  parseTest whileParser "def foo (a, b): a = 5; return b + 2"
  parseTest whileParser "List = [5, 3, 3.6]"
  parseTest whileParser "foo = main (a, b)"
  parseTest pyExpr "a += 3"
  parseTest pyExpr "a ** b"
  parseTest pyExpr "a * b"
  parseTest whileParser "a = lambda x: x * x"




{-
factorial = Function "Factorial" ["N"] 
			If ((Val $ PyTypes $ PyNum $ Int N) 'EQ' (Val $ PyTypes $ PyNum $ Int 1)) 
				(Return (Val $ PyTypes $ PyNum $ Int N))
			(Return (Multiply (Val $ PyTypes $ PyNum $ Int N) (Function "Factorial" 
				Subtract (Val $ PyTypes $ PyNum $ Int N) (Val $ PyTypes $ PyNum $ Int 1)])) 
-}


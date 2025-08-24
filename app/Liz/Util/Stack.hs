module Liz.Util.Stack (Stack (..)
                      , empty
                      , singleton
                      , push
                      , pop
                      , contains
                      ) where

data Stack a = Node a (Stack a)
  | Nil
  deriving (Show, Eq)

empty :: Stack a
empty = Nil

singleton :: a -> Stack a
singleton = flip Node Nil

push :: a -> Stack a -> Stack a
push value v@(Node _ _) = Node value v
push value Nil = Node value Nil

pop :: Stack a -> (Maybe a, Stack a)
pop (Node v rest) = (Just v, rest)
pop Nil = (Nothing, Nil)

contains :: Eq a => Stack a -> a -> Bool
contains Nil _ = False
contains (Node v rest) t = (t == v) || contains rest t

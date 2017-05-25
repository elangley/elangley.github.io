#lang pollen
◊section{
◊headline{Lambda Calculus in Haskell}

◊section{
◊h3{Some boilerplate:}

◊pre[#:class "code"]{
◊code[#:class "haskell"]{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Prelude hiding (map)
import Data.Set
}
}
}

◊section{
◊h3{Our Representation of the Lambda Calculus:}

◊pre[#:class "code"]{
◊code[#:id "Syntax" #:class "haskell"]{
data Syntax = Var Char | Abs Char Syntax | Ap Syntax Syntax
  deriving (Show,Eq,Ord)
}}}

◊p{
◊term{Syntax}: this type represents an arbitrary program in Lambda
Calculus. ◊code{Var} represents a leaf of the AST, a variable that may
either be bound or unbound. ◊code{Abs} represents one-argument function
definition. ◊code{Ap} represents the application of a function to some
arbitrary program.
}

◊section{
◊h3{Some functions for extracting interesting facts from a program:}

◊p{
◊term{sLength}: function, determines the length of an expression. 
}

◊p{
◊term{freeVars}: get the set of free variables in an expression.
}

◊p{
◊term{boundVars}: get the set of bound variables in an expression.
}

◊p{
◊term{sub}: what does this do?
}

◊pre[#:class "code"]{
◊code[#:id "sLength" #:class "haskell"]{
sLength :: Syntax -> Integer
sLength (Var x) = 1
sLength (Ap a b) = sLength a + sLength b + 1
sLength (Abs _ s) = sLength s + 1
}

◊code[#:id "freeVars" #:class "haskell"]{
freeVars :: Set Char -> Syntax -> Set Char
freeVars vars (Var char) = insert char vars
freeVars vars (Ap exp arg) = union (union (freeVars vars exp) (freeVars vars arg)) vars
freeVars vars (Abs arg body) = union vars (delete arg $ freeVars vars body) 
}

◊code[#:id "boundVars" #:class "haskell"]{
boundVars vars (Var x) = vars
boundVars vars (Ap a b) = union (boundVars vars a) (boundVars vars b)
boundVars vars (Abs a s) = insert a (boundVars vars s)
}

◊code[#:id "sub" #:class "haskell"]{
sub terms t@(Var _) = fromList [t]
sub terms t@(Abs x body) = sub (insert t terms) body
sub terms t@(Ap l r) = insert t $ union (sub terms l) (sub terms r)
}}}

◊section{
◊h3{Functions for generically finding the occurence of a variable:}

◊p{
The ◊term{PathTok} datatype represents one step in the process of
descending down a program until you reach a specified variable.
◊term{[PathTok]} represents the path to one specific variable.
}

◊p{
The ◊term{path} function takes a Syntax element and constructs a set of all
the possible paths in a given expression contains.
}

◊p{
The ◊term{occ} function takes an expression and a path and either returns the
variable specified by the path, if such exists.
}

◊pre[#:class "code"]{
◊code[#:id "PathTok" #:class "haskell"]{
data PathTok = R | L | Star
  deriving (Ord,Show,Eq)
}
◊code[#:id "path" #:class "haskell"]{
path :: Syntax -> Set [PathTok]
path (Var x) = fromList [[]]
path (Abs x s) = union (fromList [[]]) (map (\x -> Star:x) $ path s) 
path (Ap t r) = insert [] $
  union (map (\x -> L:x) $ path t)
        (map (\x -> R:x) $ path r)
}
◊code[#:id "occ" #:class "haskell"]{
occ :: Syntax -> [PathTok] -> Maybe Syntax
occ (Var t) [] = Just t
occ (Ap t r) (L:tail) = occ t tail >>= Just
occ (Ap t r) (R:tail) = occ r tail >>= Just
occ (Abs x s) (Star:tail) = occ s tail >>= Just
occ _ _ = Nothing

expand :: Char -> Syntax -> Syntax -> Syntax
expand var val (Ap exp arg) = Ap (expand var val exp) (expand var val arg)
expand var val (Abs arg body) | var /= arg = Abs arg (expand var val body)
expand var val (Abs arg body) | var == arg = undefined
expand var val (Var body) | var == body = val
expand var val body = body
}
}
}

◊section{
◊h3{Beta Reduction (incomplete):}

◊pre[#:class "code"]{
◊code[#:class "haskell"]{
betaReduce :: Syntax -> Syntax
betaReduce (Ap (Abs var body) val) = expand var val body
betaReduce x = x
}}
}

◊section{
◊h3{A couple (very useful) programs expressed in our Lambda Calculus:}

◊pre[#:class "code"]{
◊code[#:id "yComb" #:class "haskell"]{
◊def{yComb} :: Syntax
yComb = Abs 'f' (Ap (Abs 'x' (Ap (Var 'f') (Ap (Var 'x') (Var 'x'))))
                    (Abs 'x' (Ap (Var 'f') (Ap (Var 'x') (Var 'x')))))
}
◊code[#:id "succ" #:class "haskell"]{
succ'  = Abs 'n' $ Abs 'f' $ Abs 'x' $ Ap (Var 'f') (Ap (Var 'n') (Ap (Var 'f') (Var 'x')))
succ'' = Abs 'm' $ Abs 'g' $ Abs 'c' $ Ap (Var 'g') (Ap (Var 'm') (Ap (Var 'g') (Var 'c')))
}}
}

}

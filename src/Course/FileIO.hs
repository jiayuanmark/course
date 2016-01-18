{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Monad
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main = getArgs >>= pure . head >>= readFile >>= run
  where head (x :. _) = x
        head Nil      = "error: empty argument!"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run c = getFiles (lines c) >>= printFiles

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles Nil       = pure Nil
getFiles (f :. fs) = (:.) <$> getFile f <*> getFiles fs

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile fn = readFile fn >>= \c -> pure (fn, c)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil           = pure ()
printFiles ((f,c) :. fs) = printFile f c >>= \_ -> printFiles fs

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile f c = putStrLn ("============ " ++ f) >>= \_ -> putStrLn c

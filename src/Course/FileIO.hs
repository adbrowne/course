{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
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

$ runhaskell io.hs "files.txt"
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
main =
  getArgs >>= (run . head)
  where 
    head Nil = error "Empty"
    head (x:._) = x

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run masterFile =
  getFile masterFile >>= blah
  where 
    blah (_,content) = getFiles (lines content) >>= printFiles

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles xs = sequence (getFile <$> xs)

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile filePath = readFile filePath >>= (\c -> pure (filePath, c))

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles Nil = pure ()
printFiles ((filePath, chars):.xs) = printFile filePath chars >>= \_ -> printFiles xs

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile path chars =
  putStrLn ("============ " ++ path) >>= \_ ->
  putStrLn chars


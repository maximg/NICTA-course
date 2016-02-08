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
import Debug.Trace

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
main =
  -- if length <$> getArgs == 0 then
  --  error "Missing argument"
  -- else
    let fileName = take 1 <$> getArgs
        fn' = fileName >>= (\x -> trace ("filename") (pure x))
        doRun = map run <$> fn'
    in seq doRun $ pure ()

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
run p =
  let fileList = (lines . snd) <$> getFile p
  in fileList >>= getFiles >>= printFiles

getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
getFiles =
  sequence . map getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
getFile p =
  lift2 (,) (pure p) (readFile p)

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles xs =
  seq (map (\(p,c) -> printFile p c) xs) $ pure ()

printFile ::
  FilePath
  -> Chars
  -> IO ()
printFile p c =
  putStrLn ("============ " ++ p ++ "\n" ++ c)


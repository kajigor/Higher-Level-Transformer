 
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where 

import System.Console.Repline
import Control.Monad.State.Strict
import Prelude hiding (init)
import System.Directory
import System.IO
import System.FilePath.Posix

import Data.List (isPrefixOf, intercalate, sortBy)
import Data.Ord (comparing)

import Term (Term (..), parseModule, parseTerm, eval, makeFuns, showProg, Context (..), abstract, subst, free) 
import Trans (transform)

type Prog = (Term, [(String, ([String], Term))])
type IState = Maybe Prog
type Repl a = HaskelineT (StateT IState IO) a

cmd :: String -> Repl () 
cmd input = liftIO $ print input 

opts :: [(String, [String] -> Repl ())]
opts = [
    ("load", load)
  , ("prog", prog)
  , ("term", term)
  , ("eval", evaluate)
  , ("trans", translate)
  , ("quit", quit)
  , ("help", help)
  ]

load :: [String] -> Repl () 
load [name] = 
  let (dir, fileName) = splitFileName name in 
  go dir [fileName] [] [] 
  where 
    go dir [] ys ds = do 
      let ds' = makeFuns ds
      case lookup "main" ds' of
        Nothing -> liftIO $ putStrLn "No main function"
        Just (xs,t) -> put $ Just (t,ds')
    go dir (x:xs) ys ds = do 
      let qualifiedName = replaceExtension (combine dir x) ".pot"
      if   x `elem` ys
      then go dir xs ys ds
      else do 
        fileExists <- liftIO $ doesFileExist qualifiedName
        if fileExists 
        then do 
            liftIO $ putStrLn $ "Loading file: " ++ qualifiedName
            file <- liftIO $ readFile qualifiedName
            case parseModule file of 
              Left s -> 
                liftIO $ putStrLn $ "Could not parse term in file " ++ qualifiedName ++ ": " ++ show s
              Right (fs, ds2) -> do 
                go dir (xs ++ fs) (x : ys) (ds ++ ds2)
        else liftIO $ putStrLn $ "No such file: " ++ qualifiedName
load _ = 
  liftIO $ putStrLn "Please only give me one file name "

prog :: [String] -> Repl () 
prog _ = do 
  p <- get 
  (liftIO . putStrLn) (maybe "No program loaded" showProg p) 

term :: [String] -> Repl ()
term _ = do 
  p <- get 
  (liftIO . putStrLn) (maybe "No program loaded" (show . fst) p)


    -- Just (t,ds) -> f (free t) t
    --                where
    --                f [] t = do let (v,r,a) = eval t EmptyCtx ds 0 0
    --                            print v
    --                            putStrLn ("Reductions: " ++ show r)
    --                            putStrLn ("Allocations: " ++ show a)
    --                            toplevel p
    --                f (x:xs) t = do putStr (x++" = ")
    --                                hFlush stdout
    --                                l <-  getLine
    --                                case parseTerm l of
    --                                   Left s -> do putStrLn ("Could not parse term: "++ show s)
    --                                                f (x:xs) t
    --                                   Right u -> f xs (subst u (abstract t x))

evaluate :: [String] -> Repl () 
evaluate _ = do 
  p <- get
  case p of
    Nothing -> liftIO $ putStrLn "No program loaded"
    Just (t,ds) -> f (free t) t
      where
      f [] t = do 
        let (v,r,a) = eval t EmptyCtx ds 0 0
        liftIO $ print v
        liftIO $ liftIO $ putStrLn ("Reductions: " ++ show r)
        liftIO $ putStrLn ("Allocations: " ++ show a)
      f (x:xs) t = do 
        liftIO $ putStr (x++" = ")
        liftIO $ hFlush stdout
        l <- liftIO $ getLine
        case parseTerm l of
            Left s -> do 
              liftIO $ putStrLn ("Could not parse term: "++ show s)
              f (x:xs) t
            Right u -> 
              f xs (subst u (abstract t x))

translate :: [String] -> Repl () 
translate [arg] = do 
  p <- get 
  case p of 
    Nothing -> liftIO $ putStrLn "No program loaded"
    Just t -> do 
      let n = read arg :: Int
      let u = transform n t
      liftIO $ print u
      put (Just (u, []))
translate _ = 
  liftIO $ putStrLn "Please provide one argument"

quit :: [String] -> Repl () 
quit _ = abort 

help :: [String] -> Repl () 
help _ = liftIO $ putStrLn helpMessage

helpMessage = "\n:load filename\t\tTo load the given filename\n"++
               ":prog\t\t\tTo print the current program\n"++
               ":term\t\t\tTo print the current term\n"++
               ":eval\t\t\tTo evaluate the current term\n"++
               ":trans level\t\t\tTo transform the current program at the given level\n"++
               ":quit\t\t\tTo quit\n"++
               ":help\t\t\tTo print this message\n"


init :: Repl ()
init = 
  liftIO $ putStrLn ("Welcome!\n" ++ helpMessage)

-- Completion
comp :: Monad m => WordCompleter m
comp = listWordCompleter $ map (':' :) $ map fst opts 

defaultMatcher :: MonadIO m => [(String, CompletionFunc m)]
defaultMatcher = [
    (":load", fileCompleter)
  ]

repl = flip evalStateT Nothing
     $ evalRepl (pure "REPL> ") cmd opts (Just ':') (Prefix (wordCompleter comp) defaultMatcher) init

main = repl


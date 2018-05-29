module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory (getCurrentDirectory)
import Data.List (isPrefixOf, nub, sort, groupBy, intersperse)
import System.Process (readCreateProcessWithExitCode, StdStream(..), std_out, std_err, proc)


import qualified Segments

-- append x y | content x == "" = y
--            | content y == "" = x
--            | otherwise = Segment{content = joined, color = color y}
--                          where joined = render x ++ middle ++ render y
--                                cx = color x
--                                cy = color y
--                                middle = if bg cx == bg cy
--                                             then fgcolor (fg outline) ++ outline
--                                             else concat [fgcolor (bg cx),
--                                                          bgcolor (bg cy),
--                                                          separator]
--

-- external interaction
processArgs = do [arg] <- getArgs
                 let status = read arg :: Int
                 return status

git_status = result
    where process = proc "git" ["status", "--porcelain", "-b"]
          result = process{std_out = CreatePipe, std_err = CreatePipe}

main = do status <- processArgs
          cwd <- getCurrentDirectory
          home <- lookupEnv "HOME"
          git_result <- readCreateProcessWithExitCode git_status ""
          ssh_client <- lookupEnv "SSH_CLIENT"
          putStr $ Segments.render [Segments.username, Segments.ssh ssh_client,
                                    Segments.hostname, Segments.cwd cwd home,
                                    Segments.git git_result, Segments.status status]

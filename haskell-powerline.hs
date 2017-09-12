module Main where

import System.Environment (getArgs, lookupEnv)
import System.Directory (getCurrentDirectory)
import Data.List (isPrefixOf, nub, sort, groupBy, intersperse)
import System.Process (readCreateProcessWithExitCode, StdStream(..), std_out, std_err, proc)
import System.Exit (ExitCode(..))

import Text.Parsec (ParseError, parse, manyTill, try, string, anyChar, eof, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)

-- powerline symbols
branch = "\xe0a0"
lock = "\xe0a2"
separator = "\xe0b0"
outline = "\xe0b1"

detached = "\x2693"
ahead = "\x2b06"
behind = "\x2b07"
staged = "\x2714"
not_staged = "\x270e"
untracked = "\x2753"
conflicted = "\x273c"

ellipsis = "\x2026"

-- colors
data Color = Color{fg :: Int, bg :: Int}

username = Color{fg = 250, bg = 240}
hostname = Color{fg = 250, bg = 238}

home = Color{fg = 15, bg = 31}
path = Color{fg = 250, bg = 237}

outline_fg = 244
cwd_color = Color{fg = 254, bg = 237}

ssh = Color{ fg = 254, bg = 166 }

repo_clean = Color{fg = 0, bg = 148}
repo_dirty = Color{fg = 15, bg = 161}

cmd_passed = Color{fg = 15, bg = 236}
cmd_failed = Color{fg = 15, bg = 160}

git_ahead = Color{fg = 250, bg = 240}
git_behind = Color{fg = 250, bg = 240}
git_staged = Color{fg = 15, bg = 22}

git_not_staged = Color{fg = 15, bg = 130}
git_conflicted = Color{fg = 15, bg = 9}

git_untracked = Color{fg = 15, bg = 52}

virtual_env = Color{fg = 0, bg = 35}

template x = "\\[\\e" ++ x ++ "\\]"

reset = template "[0m"

fgcolor code = template $ "[38;5;" ++ show code ++ "m"
bgcolor code = template $ "[48;5;" ++ show code ++ "m"

-- segment rendering
data Segment = Segment{content :: String, color :: Color}

render (Segment{content = content, color = color}) = concat [fgcolor (fg color),
                                                             bgcolor (bg color),
                                                             content]

append x y | content x == "" = y
           | content y == "" = x
           | otherwise = Segment{content = joined, color = color y}
                         where joined = render x ++ middle ++ render y
                               cx = color x
                               cy = color y
                               middle = if bg cx == bg cy
                                            then fgcolor outline_fg ++ outline
                                            else concat [fgcolor (bg cx),
                                                         bgcolor (bg cy),
                                                         separator]

segment content color = Segment { content = " " ++ content ++ " ", color = color }

-- powerline segments
empty_segment = Segment{content = "", color = Color{ fg = 0, bg = 15 }}

username_segment = segment "\\u" username
hostname_segment = segment "\\h" hostname

status_segment status = if status == 0
                            then segment sym cmd_passed
                            else segment sym cmd_failed
                        where sym = "\\$"

ssh_segment Nothing = empty_segment
ssh_segment (Just _) = segment lock ssh

cwd_segments _ Nothing = []
cwd_segments cwd (Just home_folder) = home_seg : rest_segs
    where in_home = isPrefixOf home_folder cwd
          home_seg = if in_home then segment "~" home else empty_segment
          rest = if in_home then drop (length home_folder) cwd else cwd
          words s = case dropWhile (== '/') s of
                         "" -> []
                         s' -> w : words s'' where (w, s'') = break (== '/') s'
          pieces = words rest
          rest_segs = if pieces == []
                         then []
                         else let last_seg = segment (last pieces) cwd_color
                                  init_segs = [segment piece path | piece <- init pieces]
                                  omit_seg = segment ellipsis path
                                  trimmed segs = drop (length segs - 2) segs
                                  trim segs = if length segs < 3 then segs else omit_seg : trimmed segs
                              in trim init_segs ++ [last_seg]

data GitStatus = Untracked | Staged | NotStaged | Conflicted
                 deriving (Eq, Ord)

instance Show GitStatus where
    show Untracked = untracked
    show Conflicted = conflicted
    show NotStaged = not_staged
    show Staged = staged


git_segment (ExitSuccess, stdout, stderr) = result
   where result = segment (branch ++ " " ++ info) color
         dirty = length status_groups > 0
         color = if dirty then repo_dirty else repo_clean
         info = concat (intersperse " " (status_groups ++ [branch_name]))
         in_lines = lines stdout
         pairs ls = let len = length ls
                    in (if len == 1 then "" else (show len)) ++ show (head ls)
         status_groups = map pairs $ groupBy (==) status_info
         status_info = sort $ map (code . take 2) (drop 1 in_lines)
         branch_line = head in_lines
         branch_name = case (parse branch_info "name" branch_line) of
                           Left _ -> "unknown"
                           Right local -> local
         branch_info = do _ <- char '#'
                          _ <- char '#'
                          _ <- char ' '
                          local <- try (manyTill anyChar (try (string "..."))) <|> manyTill anyChar eof
                          return local
         code "??" = Untracked
         code "DD" = Conflicted
         code "AU" = Conflicted
         code "UD" = Conflicted
         code "UA" = Conflicted
         code "DU" = Conflicted
         code "AA" = Conflicted
         code "UU" = Conflicted
         code "MM" = Conflicted
         code (' ':_) = NotStaged
         code (_:" ") = Staged
         code x = error $ "did not understand code " ++ x

git_segment (ExitFailure _, _, _) = empty_segment


render_all xs = render one_piece ++ reset ++ fgcolor last_bg ++ separator
                where one_piece = foldl append empty_segment xs
                      last_bg = bg (color one_piece)

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
          putStr $ render_all ([username_segment,
                                ssh_segment ssh_client,
                                hostname_segment] ++ cwd_segments cwd home ++
                               [git_segment git_result, status_segment status])
          putStr $ reset ++ " "

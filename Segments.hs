module Segments where

import Data.Monoid ((<>))
import Data.List (isPrefixOf, intersperse, sort, groupBy)

import System.Exit (ExitCode(..))

import qualified Symbols
import qualified Colors
import Colors (fg, bg)

import Text.Parsec (ParseError, parse, manyTill, try, string, anyChar, eof, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)

data Segment = Segment{content :: String, left :: Colors.Color, right :: Colors.Color}
               deriving (Eq)

template x = "\\[\\e" ++ x ++ "\\]"

reset = template "[0m"

fgcolor code = template $ "[38;5;" ++ show code ++ "m"
bgcolor code = template $ "[48;5;" ++ show code ++ "m"


instance Show Segment where
    show (Segment{content = content, left = left, right = right})
        = concat [fgcolor (fg left), bgcolor (bg left), content, fgcolor (fg right), bgcolor (bg right)]

empty = Segment{content = "", left = Colors.empty, right = Colors.empty}

instance Monoid Segment where
    mempty = empty
    mappend x y | x == empty = y
                | y == empty = x
                | otherwise = Segment{content = whole, left = left x, right = right y}
                  where whole = concat [content x,
                                        fgcolor (fg stop), bgcolor (bg stop),
                                        separator,
                                        fgcolor (fg start), bgcolor (bg start),
                                        content y]
                        stop = right x
                        start = left y
                        separator = if bg stop == bg start
                                        then fgcolor (fg Colors.outline) ++ Symbols.outline
                                        else concat [fgcolor (bg stop), bgcolor (bg start),
                                                     Symbols.separator]

segment content color = Segment { content = " " ++ content ++ " ", left = color, right = color}

username = segment "\\u" Colors.username
hostname = segment "\\h" Colors.hostname

status code = if code == 0
                  then segment sym Colors.cmd_passed
                  else segment sym Colors.cmd_failed
              where sym = "\\$"

ssh Nothing = empty
ssh (Just _) = segment Symbols.lock Colors.ssh

cwd current_folder home = foldl (<>) mempty (segments home)
    where segments Nothing = rest_segs (words current_folder)
          segments (Just home_folder) = home_seg : (rest_segs (words rest))
              where
              rest = if in_home then drop (length home_folder) current_folder else current_folder
              in_home = isPrefixOf home_folder current_folder
              home_seg = if in_home then segment "~" Colors.home else empty
          words s = case dropWhile (== '/') s of
                         "" -> []
                         s' -> w : words s'' where (w, s'') = break (== '/') s'
          rest_segs [] = []
          rest_segs pieces = let last_seg = segment (last pieces) Colors.cwd
                                 init_segs = [segment piece Colors.path | piece <- init pieces]
                                 omit_seg = segment Symbols.ellipsis Colors.path
                                 trimmed segs = drop (length segs - 2) segs
                                 trim segs = if length segs < 3 then segs else omit_seg : trimmed segs
                             in trim init_segs ++ [last_seg]

data GitStatus = Untracked | Staged | NotStaged | Conflicted
                 deriving (Eq, Ord)

instance Show GitStatus where
    show Untracked = Symbols.untracked
    show Conflicted = Symbols.conflicted
    show NotStaged = Symbols.not_staged
    show Staged = Symbols.staged


git (ExitFailure _, _, _) = empty
git (ExitSuccess, out, _) = segment (Symbols.branch ++ " " ++ info) color
   where
         color = if dirty then Colors.repo_dirty else Colors.repo_clean
         dirty = length status_groups > 0
         info = concat (intersperse " " (status_groups ++ [branch_name]))
         in_lines = lines out
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
         code (' ':_) = NotStaged
         code (_:" ") = Staged
         code _ = Conflicted

render :: [Segment] -> String
render segments = show whole ++ reset ++ finish ++ Symbols.separator ++ reset ++ " "
    where whole = foldl (<>) mempty segments
          finish = fgcolor (bg (right whole))

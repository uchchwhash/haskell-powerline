module Colors where

data Color = Color{fg :: Int, bg :: Int} deriving (Eq)

empty = Color{fg = 0, bg = 15}

username = Color{fg = 250, bg = 240}
hostname = Color{fg = 250, bg = 238}

home = Color{fg = 15, bg = 31}
path = Color{fg = 250, bg = 237}

outline = Color{fg = 244, bg = 0}
cwd = Color{fg = 254, bg = 237}

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

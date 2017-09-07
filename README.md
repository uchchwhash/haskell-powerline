# haskell-powerline
A customized Haskell implementation of Powerline for Bash

This is a bash shell prompt that mimics the popular [powerline](https://github.com/powerline/powerline) style.
More specifically, it is a Haskell port of [powerline-shell](https://github.com/banga/powerline-shell) from Python.
I only kept the segments that matter to me. You can add your own.

# Installing
To produce the binary, run
```bash
$ make build
```
and test it by running
```bash
$ ./haskell-powerline 0
```
If nothing terrible happens and the result looks like ANSI PS1 gibberish with powerline symbols thrown in,
you are probably good to go, so add
```bash
source <path-to-repo>/setup.sh
```
to your `.bashrc`. If the symbols do not show up, you probably need a [powerline font](https://github.com/powerline/fonts).

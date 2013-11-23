# haskell-shell

A (minimally useful) interactive command shell in Haskell, implementing mostly Bourne-style syntax.
Adopted from coyotebush/haskell-shell

I am using this as a vehicle to note down what I want my shell to look like (commands and syntax)


# TODO
* We should be able to use multiple named streams indicated by a starting : after a pipe.

* rm should remove files given in stdin by default rather than files given in args, and it should print the names of files
it was unable to remove into stream :no.
e.g
```
| ls -1 | rm |:no | rmdir
```
* move should accept things to move in stdin by default, and destination in args
```
find /files | grep err |mv mydir
```
* Need a better case statement able to use regex (and avoid awk, and grep)
* mkdir should accept a list of files in stdin

# haskell-shell

A (minimally useful) interactive command shell in Haskell, implementing mostly Bourne-style syntax.
Adopted from coyotebush/haskell-shell

I am using this as a vehicle to note down what I want my shell to look like (commands and syntax)


# TODO
* The shell should be oriented towards interactive usage. So allow things like converting spaces in file names to nonbreaking space different from the breaking space when you press space bar. So avoid having to quote your arguments.
* Allow left assignment like in R. i.e defintion of a command should be painless, like `find /files | grep err -> mycmd` which should define a new command mycmd
* Dont go for little languages like awk and sed. Where possible incorporate that into the shell. This will help avoid the quoting hell.
* Use nesting braces for quoting strings if necessary.
* Need an RStudio like interface (or the plan9 drawterm) that can handle arbitrary drawing, but this is much later.
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

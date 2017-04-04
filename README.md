# Functional Programming in Erlang mooc
Exercises and assigments done during the [Functional Programming in Erlang mooc](https://www.futurelearn.com/courses/functional-programming-erlang/).

# Getting started
## Install erlang
The easiest thing is to install Docker, download the erlang image using `docker pull erlang` and create an image using `docker run -it -v=$(pwd):/code -w=/code erlang`. This will create a new container that will run the erlang repl by default.

## REPL tricks
- compile a file: `c(erlang_file).`
- compile a file and export all functions (really handy for testing): `c(erlang_file,[export_all]).`
- clear bindings: `f(Foobar).` or `f().` (the latter clears all bindings)

## Licence

```
MIT License

Copyright (c) 2017 Futtetennista

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

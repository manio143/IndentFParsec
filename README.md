# IndentFParsec
This is a simple extension for FParsec that allows you to parse Indentation Sensitive grammars. It's by no means an advanced tool, but rather a set of shortcuts.

I first looked at reimplementing Haskell's IndentParser for Parsec at first, but later decided to go in a slightly different direction.

The project is in .NET Standard 2.0

#### Usage
The core part of this library are four functions

* `exact pos p`
* `greater pos p`
* `atLeast pos p`
* `any p`

Each of them begins by eating whitespace and then parses `p` if it's at an appropriate position.

To help with block based languages, where all statements in a block are indented the same, I also added the `blockOf p` parser combinator, that parses 1 or more occurances of `p`.

#### Example
To demonstrate usage of this extension I chose the language from someone's previous attempt at doing IS with FParsec ([found here](https://gist.github.com/impworks/3772212)).

I've described it's grammar in terms of IS (after reading [this paper by Michael D. Adams](https://michaeldadams.org/papers/layout_parsing/LayoutParsing.pdf)) like this:

    Stmts  ->    Stmt(=)  Stmts'(=)
    Stmts' ->   Stmts(=)
    Stmts' ->
    Stmt   ->   Print(=)
    Stmt   ->    Loop(=)
    Print  -> "print"(=)  Ident(>)
    Loop   ->  "loop"(=)  Ident(>) Int(>) Int(>) Stmts(>)

Given the left side of a production starts at column `i` then the part in the right of the production must start at such column `j` that `j ~ i`, where `~` is the relation in parenthesis. The possible relations are `=`, `>`, `>=` and `*` (asterisk means any indentation). Each of those relations is described in the form of a function, listed above.

In `Test/Test.fs` you can find the parser for this language and in `Test/Run.fs` the examples I've tested it on.

#### License and stuff
The license is MIT.

If you found this repo useful, please give it a star and praise it on Twitter mentioning me - [@MDziubiak](//twitter.com/MDziubiak).
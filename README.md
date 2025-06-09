<p align="center">
    <a href="https://github.com/hearot/MICIO">
        <img src="https://i.imgur.com/CYwizQj.png" alt="MICIO" width="150"/>
    </a>
    <br>
    <i>A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3.12!</i>
    <br>
    <br>
    <a href="mailto:g.videtta1@studenti.unipi.it"><img src="https://img.shields.io/badge/-Gabriel%20Antonio%20Videtta-152ba3?style=flat-square&labelColor=152ba3&logo=mail.ru&logoColor=white" alt="Gabriel Antonio Videtta"/></a>
    <a href="mailto:a.baccioli2@studenti.unipi.it"><img src="https://img.shields.io/badge/-Avio%20Baccioli-152ba3?style=flat-square&labelColor=152ba3&logo=mail.ru&logoColor=white" alt="Avio Baccioli"/></a>
</p>

**MICIO** ([/ˈmitʃo/](https://ipa-reader.com/?text=%CB%88mit%CA%83o&voice=Nicole)) is a Python-based musical interpreter designed for chord interpretation and orchestration. It allows you to define, manipulate, and export musical compositions in the WAVE format using a simple and intuitive syntax. The goal is to make it easy to create, manipulate, and combine musical elements like notes, harmonies, and timings.
The main MICIO script fully complies with [mypy](https://github.com/python/mypy) type-checking standards.

## Table of Contents

- [Installation](#installation)
- [Syntax reference](#syntax-reference)
  - [Expressions](#expressions)
    - [Defining Notes](#defining-notes)
    - [Defining Harmonies](#defining-harmonies)
    - [Defining Pauses](#defining-pauses)
    - [Defining and Concatenating Songs](#defining-and-concatenating-songs)
    - [TRANSPOSE, CHANGETIME and REPEAT](#transpose-changetime-and-repeat)
    - [LET](#let)
    - [Applying a Function](#applying-a-function)
    - [MAP](#map)
    - [If-Else (for expressions)](#if-else-for-expressions)
  - [Commands](#commands)
    - [Assignments](#assignments)
    - [Function Declarations](#function-declarations)
    - [If-Else (for commands)](#if-else-for-commands)
    - [EXPORT](#export)
- [Basic Examples](#basic-examples)

## Installation

You can install MICIO via `pip` by cloning the repository and installing its dependencies:

```bash
$ git clone https://github.com/hearot/MICIO.git
$ cd MICIO
$ pip install -r requirements.txt
```

If you wish to perform type checks with `mypy`, you can install the necessary stubs as well:

```bash
$ pip install -r test-requirements.txt
```

You can then launch MICIO via `python`:

```bash
$ python micio.py -h
```

```text
usage: micio.py [-h] [filename]

MICIO - A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3.12!

positional arguments:
  filename    the file containing the code to execute (defaults to a REPL)

options:
  -h, --help  show this help message and exit
```

# Syntax reference

The foundational building block of MICIO is the only existing type, namely the `Song` type. A `Song` is simply a collection of `Harmony`'s and
`Pause`'s, played sequentially. We will refer to either a `Harmony` or
a `Pause` as a `Step`. A `Harmony` is a collection of `Note`s. The most important binary operation is the concatenation (`->`),
which makes a `Song` follow another one. 

- Three built-in
operations are also provided: `TRANSPOSE` to transpose by a given number of semitones, `CHANGETIME` to speed up or slow down a `Song`, and `REPEAT` to repeat a `Song` a given number of times.
- A `LET` environment is provided, which binds a value to a variable within a scope.
- A `MAP` operation is provided as well, to apply a transformation to each step (harmony or pause) of a song, producing a new song where each step is replaced by the result of the given expression.
- Finally an `IF-ELSE` expression is provided too, allowing you to choose between two expressions based on a condition.

A code consists of a sequence of commands executed sequentially. Each command may be an assignment, which binds a `Song`
to a variable or to a constant; a function declaration; an `IF-ELSE`, for conditional execution; or an `EXPORT` statement, used to export a `Song` to WAVE file.

MICIO supports line comments, which begin with `//`.

MICIO ignores whitespaces and employs the following [Lark](https://github.com/lark-parser/lark) grammar:

```text
    command_seq: command (";" command?)*
    ?command: const_assign | assign | fundecl | export | ifelse

    const_assign: "CONST" IDENTIFIER "=" expr | "CONST" IDENTIFIER ":=" expr
    assign: IDENTIFIER "=" expr | IDENTIFIER ":=" expr
    
    fundecl: "FUNCTION" IDENTIFIER "(" params ")" "=" expr | "FUNCTION" IDENTIFIER "(" params ")" ":=" expr
    params: IDENTIFIER ("," IDENTIFIER)*

    export: "EXPORT" expr "TO" "\"" FILENAME "\""
    FILENAME: /[a-zA-Z0-9_\/.-]+/

    ifelse: "IF" boolean "THEN" command_seq ("ELSE" command_seq)? "ENDIF"
    ?boolean: equal | not_equal
    equal: expr "==" expr
    not_equal: expr "!=" expr

    ?expr: concat | mono | let | ifelse_expr | map
    ?mono: step | transpose | paren | var | changetime | funapply | repeat
    ?paren: "(" expr ")"
    var: IDENTIFIER

    let: "LET" IDENTIFIER "=" expr "IN" expr
    ifelse_expr: expr "IF" boolean "ELSE" expr
    map: "MAP" IDENTIFIER "IN" expr "=>" expr

    funapply: IDENTIFIER "(" arg_list ")"
    arg_list: expr ("," expr)*

    transpose: "TRANSPOSE(" expr "," TRANSPOSE_VALUE ")"
    changetime: "CHANGETIME(" expr "," time ")"
    repeat: "REPEAT(" expr "," NUMBER ")"

    step: harmony | pause
    
    harmony: mono_note | "[" mono_note ("," mono_note)*"]"

    ?mono_note: note | note_time
    note_time: "(" note "," time ")"

    note: NOTE MODIFIER? NUMBER

    pause: "PAUSE(" time ")"

    concat: mono "->" expr | mono ">" expr
    
    time: fraction | NUMBER
    fraction: NUMBER "/" NUMBER

    TRANSPOSE_VALUE: /[-]?[0-9]+/ 

    NOTE: "A" | "B" | "C" | "D" | "E" | "F" | "G" | "Do" | "Re" | "Mi" | "Fa" | "Sol" | "So" | "La" | "Si" | "Ti" | "H"
    MODIFIER: "#" | "b"
        
    IDENTIFIER: /[a-zA-Z_][a-zA-Z0-9_]*/
    NUMBER: /[0-9]+/

    COMMENT: /\/\/[^\n]*/
    %import common.WS
    %ignore COMMENT
    %ignore WS
```

## Expressions

Parentheses can be used to group expressions and control the order of evaluation.

### Defining Notes

A `Note` in MICIO is defined by its **name**, **octave**, and optionally a **modifier**.
The name is selected from the following standard musical notes (letter notation, or fixed-Do solfège):

- `A` / `La`
- `B` / `Si` / `Ti` / `H`
- `C` / `Do`
- `D` / `Re`
- `E` / `Mi`
- `F` / `Fa`
- `G` / `Sol` / `So`

Each note must be followed by its **octave**, which is represented by a non-negative integer. Optionally, a **modifier** can be included before the octave to adjust the pitch. The modifier can be either:

- `#` for **sharp** (diesis), raising the pitch by a semitone
- `b` for **flat** (bemolle), lowering the pitch by a semitone

Additionally, a **duration** can be specified for the note, indicating how long it should sound. This is done using the format:

```text
(NOTE, TIME)
```

where `TIME` represents the duration in seconds and can either be:

- a non-negative integer,
- a fraction in the form `n/d` (e.g. `1/2`).

#### Examples

- `A4`: generates the note `A` in the 4th octave.
- `La4`: does the same as above, since `La = A` in the fixed-Do solfège.
- `A#4`: generates the note `A` in the 4th octave, with the **sharp** modifier.
- `(A#4, 2)`: generates the note `A` in the 4th octave, with the **sharp** modifier and a duration of 2 seconds.

### Defining Harmonies

A `Harmony` is an *unordered* list of `Note`s (see [Defining Notes](#defining-notes)) and is specified as:

```text
[NOTE_1, NOTE_2, ..., NOTE_n]
```

where `NOTE_i` represents a `Note` for each index `i`.

A single note `N`, when placed in the position of a harmony, will be regarded as the harmony `[N]`.

#### Examples

- `A4`: generates the harmony containing the single note `A4`.
- `[A4, B4]`: generates the harmony containing the notes `A4` and `B4`.
- `[(A4, 2), B4]`: generates the harmony containing the notes `A4` (with a duration of 2 seconds) and `B4`.

### Defining Pauses

A `Pause` represents a period of silence in the music.
It is specified using the `PAUSE` keyword followed by a duration in parentheses, as seen below:

```text
PAUSE(TIME)
```

where `TIME` represents the duration in seconds and can either be:

- a non-negative integer,
- a fraction in the form `n/d` (e.g. `1/2`).

The duration is specified in the same way as for a note, and it represents how long the pause lasts.

### Union of Harmonies or Pauses

The union operation allows you to combine two harmonies
or two pauses into a single `Step`, with the following syntax:

```text
EXPR1 + EXPR2
```

where `EXPR1` and `EXPR2` are two expressions.

When both operands are harmonies, the result is a harmony containing all notes from both. When both operands are pauses, the result is a pause whose duration is the maximum of the two.

The union operation can only be applied between songs that each contain exactly one `Step`. Mixing a harmony and a pause is not allowed!

#### Examples

- `[A4, C5] + [E4]`: produces the harmony `[A4, C5, E4]`.
- `PAUSE(1) + PAUSE(2)`: produces a pause of two seconds, i.e., `PAUSE(2)`.
- See `example_10.micio` under *examples*.

### Defining and Concatenating Songs

A `Song` is simply a concatenation of harmonies, pauses and other songs, with the base cases being harmonies and pauses. It is
built as follows:

```text
EXPR_1 -> ... -> EXPR_2
```

where `EXPR_i` is an expression.

The constant `EMPTY` is a reserved constant which represents the empty song object (i.e., a song with no harmonies nor pauses).

#### Examples

- `[A4, B4] -> PAUSE(1) -> A4`: generates a song with the harmony `[A4, B4]`, then a pause of one second, and finally a note `A` in the
    4th octave.
- `PAUSE(1) -> x -> A4`: plays a song that has a pause of one second, then the song stored in `x`, and then one note `A` in the 4th octave.

### TRANSPOSE, CHANGETIME and REPEAT

The `TRANSPOSE` operation shifts all notes in a song by a specified number of semitones. Positive values transpose the notes up, while negative values transpose them down. It is specified as follows:

```text
TRANSPOSE(EXPR, SEMITONES)
```

where `SEMITONES` is an integer and `EXPR` is an expression.

The `CHANGETIME` operation changes the duration of all notes and pauses in a song by a specified factor. This allows you to speed up or slow down the timing of a composition. It is specified as follows:

```text
CHANGETIME(EXPR, FACTOR)
```

where `FACTOR` is a positive integer or a fraction in the form `n/d` and  `EXPR` is an expression.

The `REPEAT` operation simply repeats an expression multiple times. It is specified as follows:

```text
REPEAT(EXPR, NUMBER_OF_TIMES)
```

where `NUMBER_OF_TIMES` is a positive integer and `EXPR` is an expression.

#### Examples

- `TRANSPOSE(A4, 2)`: transposes `A4` by 2 semitones.
- `CHANGETIME((A4, 2), 0.5)`: halves the duration of `(A4, 2)` (which then simply becomes `A4`).
- `REPEAT(A4, 3)`: evaluates to `A4 -> A4 -> A4`.

### LET

The `LET` expression allows you to define a variable that can hold a song.
Once defined, the variable can be used throughout the rest of your computation. A `LET`
is defined as follows:

```text
LET variable = VALUE IN EXPR
```

where `variable` is the name of the variable, `VALUE` is the value that will be mapped to `variable` and
`EXPR` is the expression that will be evaluated once `variable` has been successfully bound.

#### Examples

- `LET x = A4 in x -> x`: generates `A4 -> A4`.
- `LET x = A5 in LET y = B5 in x -> y`: generates `A5 -> B5`.

### Applying a Function

Once a function has been declared using the `FUNCTION` keyword (see [Function Declarations](#function-declarations)),
it can be applied by calling it with arguments, using the following syntax:

```
NAME(ARG1, ARG2, ..., ARGN)
```

where `ARGi` is an expression for each `i`. Applying a function always returns
a `Song` expression.

#### Examples

We use the function declared in [Function Declarations](#function-declarations).

- `INTRO(B4)`: returns `A4 -> B4`.
- `F(C4, D5)`: returns `C4 -> D5`.

### MAP

The `MAP` expression allows you to apply a transformation to each step (`Harmony` or `Pause`) of a song, producing a new song where each step is replaced by the result of the given expression. The syntax is:

```text
MAP iter_variable IN EXPR => RETURN_EXPR
```

- `iter_variable` is a name that will represent each step of the song within the iteration.
- `EXPR` is an expression that evaluates to a song; it's the song `MAP` iterates over.
- `RETURN_EXPR` is an expression that can use `iter_variable` to refer to the current step; it's the expression which gets evaluated at each step of the iteration.

#### Examples

- `MAP y IN x => TRANSPOSE(y, 2)`: produces a new song where each step of `x` is transposed up by 2 semitones; equivalent to `TRANSPOSE(x, 2)`.
- `MAP y IN x => y -> y`: produces a new song where each step is repeated twice; cf. `example_9.micio` under *examples*.

## If-Else (for expressions)

You can use `IF ... ELSE ...` as a ternary operator. It is defined as follows:

```text
IF_TRUE_EXPR IF BOOL ELSE IF_FALSE_EXPR
```

where `BOOL` is a condition, `IF_TRUE_EXPR` is the expression
that gets returned if `BOOL` is evaluated as true and `IF_FALSE_EXPR`
is the expression that gets returned otherwise.

#### Examples

- `A4 IF x == y ELSE B4`: evaluates to `A4` if `x` equals `y`, otherwise to `B4`.


## Commands

Commands in MICIO are separated by a semicolon `;`.

### Assignments

You can assign an expression with `variable := EXPR` or `variable = EXPR`, where `variable`
is a valid identifier in the grammar and `EXPR` is an expression for a `Song`.

You can define constants using either `CONST variable := EXPR` or `CONST variable = EXPR`.
Keep in mind that you can't define a constant with the same name as an existing
variable, nor can you redefine an existing constant.

The identifier `EMPTY` is already reserved to define the empty song object.

#### Examples

- `x := A4`: assigns the song `A4` to the variable `x`.
- `scale := C4 -> D4 -> E4 -> F4 -> G4 -> A4 -> B4 -> C5`: assigns the C major scale to `scale`.
-  `y := F(x)`: assigns the evaluation of `F` with argument `x` to the variable `y`.
- `CONST x := A4`: defines the constant `x` with value `A4`.

### Function Declarations

You can define a function with the following syntax:

```FUNCTION NAME(X1, X2, X3, ..., XN) = EXPR```

or

```FUNCTION NAME(X1, X2, X3, ..., XN) := EXPR```

where `NAME` is the name of the function, `X1`, ..., `XN` are
the variables employed in the expression `EXPR`, an expression for a `Song`. All
the variables must be distinct identifiers.
Functions with zero parameters are NOT allowed (you can use constants for that!), hence must have at least one
parameter. Functions do NOT allow commands.

#### Examples

- `FUNCTION INTRO(X) := A4 -> X`: declares a function `INTRO` with one parameter `X` which prepends `A4` to the song passed as argument `X`.
- `FUNCTION F(X, Y) := X -> Y`: declares a function `F` with two parameters `X` and `Y` which concatenates `X` into `Y`.

### If-Else (for commands)

You can define an `IF-ELSE` statement with the following syntax:

```text
IF condition THEN
    command_seq_if_true
ELSE
    command_seq_if_false
ENDIF
```

where `condition` is a boolean expression, `command_seq_if_true` is
a sequence of commands which gets executed if `condition` is evaluated
as true, and `command_seq_if_false` is a sequence of commands which
gets executed otherwise.

The `ELSE` branch is optional, and if omitted no command will be
executed if `condition` is evaluated as false.

Everything declared within the `IF` or `ELSE` branch is local in scope and will not be visible from outside. The state changes
persist.

#### Examples

See `example_7.micio` under *examples* for a showcase of the if-else
statement.

### EXPORT

You can export a `Song` expression to a WAVE file with the following syntax:

```EXPORT EXPR TO "filename.wav"```

where `EXPR` is an expression for a `Song` and `filename.wav` is the name of the WAVE file.

#### Examples

- `EXPORT A4 -> B4 TO "music.wav"`: exports the song `A4 -> B4` to `music.wav`.

# Basic examples

Under the *examples* folder you will find the following examples:

- `example_1a.micio` and `example_1b.micio` both generate the C major scale. The former uses letter notation, whereas the latter employs fixed-Do solfège.
- `example_2.micio` generates the refrain of *Never gonna give you up* by Rick Astley.
- `example_3a.micio` and `example_3b.micio` both generate the refrain of the theme of Dario Moccia's *Momento gaming* with three different durations. The former follows functional programming principles, whereas the latter adopts an imperative paradigm.
- `example_4.micio` generates the refrain of *Ode to Joy* by Ludwig van Beethoven.
- `example_6.micio` showcases the usage of assignment, functions and comments.
- `example_7.micio` showcases the usage of *if-else* statements.
- `example_8.micio` and `example_9.micio` showcase the usage of `MAP`.
- `example_10.micio` generates the intro and refrain of *Enemy* by Imagine Dragons, showcasing the usage of `MAP` together with the union operation (`+`) for harmonies and pauses.

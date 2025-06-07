<p align="center">
    <a href="https://github.com/hearot/MICIO">
        <img src="https://i.imgur.com/CYwizQj.png" alt="MICIO" width="150"/>
    </a>
    <br>
    <i>A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3.12!</i>
    <br>
    <br>
    <a href="mailto:g.videtta1@studenti.unipi.it">
        <img src="https://img.shields.io/badge/-Gabriel%20Antonio%20Videtta-152ba3?style=pflat-square&labelColor=152ba3&logo=mail.ru&logoColor=white&link=mailto:g.videtta1@studenti.unipi.it" alt="Gabriel Antonio Videtta"/>
    </a>
    •
    <a href="mailto:a.baccioli2@studenti.unipi.it">
        <img src="https://img.shields.io/badge/-Avio%20Baccioli-152ba3?style=pflat-square&labelColor=152ba3&logo=mail.ru&logoColor=white&link=mailto:a.baccioli2@studenti.unipi.it" alt="Avio Baccioli"/>
    </a>
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
    - [TRANSPOSE and CHANGETIME](#transpose-and-changetime)
    - [LET](#let)
  - [Commands](#commands)
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
`Pause`'s, played sequentially. A `Harmony` is a collection of `Note`s. The most important binary operation is the concatenation,
which makes a `Song` follow another one. Two built-in unary
operations are also provided: `TRANSPOSE` to tranpose by a given number of semitones, and  `CHANGETIME` to speed up or slow down a `Song`. A `LET` environment is provided, which binds a value to a variable within a scope.

A code consists of a sequence of commands executed sequentially. Each command may be an assignment, which binds a `Song`
to a variable; a function declaration; or an `EXPORT` statement, used to export a `Song` to WAVE file.

MICIO ignores whitespaces and employs the following grammar:

```text
    command_seq: command (";" command?)*
    ?command: assign | fundecl | export

    assign: IDENTIFIER "=" expr | IDENTIFIER ":=" expr
    
    fundecl: "FUNCTION" IDENTIFIER "(" params ")" "=" expr | "FUNCTION" IDENTIFIER "(" params ")" ":=" expr
    params: IDENTIFIER ("," IDENTIFIER)*

    export: "EXPORT(" expr "," "\"" FILENAME "\"" ")"
    FILENAME: /[a-zA-Z0-9_\/.-]+\.[a-zA-Z0-9]+/

    ?expr: concat | mono | let
    ?mono: step | transpose | paren | var | changetime | funapply
    ?paren: "(" expr ")"
    var: IDENTIFIER

    let: "LET" IDENTIFIER "=" expr "IN" expr

    funapply: IDENTIFIER "(" arg_list ")"
    arg_list: expr ("," expr)*

    transpose: "TRANSPOSE(" expr "," TRANSPOSE_VALUE ")"
    changetime: "CHANGETIME(" expr "," time ")"

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

A `Harmony` is a list of `Note`s (see [Defining Notes](#defining-notes)) and is specified as:

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
It is specified using the `PAUSE` keyword followed by a duration between braces, as seen below:

```text
PAUSE(TIME)
```

where `TIME` represents the duration in seconds and can either be:

- a non-negative integer,
- a fraction in the form `n/d` (e.g. `1/2`).

The duration is specified in the same way as for a note, and it represents how long the pause lasts.

### Defining and Concatenating Songs

A `Song` is simply a concatenation of harmonies, pauses and other songs, with the base cases being harmonies and pauses. It is
built as follows:

```text
EXPR_1 -> ... -> EXPR_2
```

where `EXPR_i` is a harmony, a pause, a song, a `LET` statement, a unary operation or a variable.

#### Examples

- `[A4, B4] -> PAUSE(1) -> A4`: generates a song with the harmony `[A4, B4]`, then a pause of one second, and finally a note `A` in the
    4th octave.
- `PAUSE(1) -> x -> A4`: plays a song that has a pause of one second, then the song stored in `x`, and then one note `A` in the 4th octave.

### TRANSPOSE and CHANGETIME

The `TRANSPOSE` operation shifts all notes in a song by a specified number of semitones. Positive values transpose the notes up, while negative values transpose them down. It is specified as follows:

```text
TRANSPOSE(EXPR, SEMITONES)
```

where `SEMITONES` is an integer and `EXPR` is a harmony, a pause, a song, a `LET` statement, a unary operation or a variable.

The `CHANGETIME` operation changes the duration of all notes and pauses in a song by a specified factor. This allows you to speed up or slow down the timing of a composition. It is specified as follows:

```text
CHANGETIME(EXPR, FACTOR)
```

where `FACTOR` is a positive integer of a fraction in the form `n/d` and  `EXPR` is a harmony, a pause, a song, a `LET` statement, a unary operation or a variable.

#### Examples

- `TRANSPOSE(A4, 2)`: transposes `A4` by 2 semitones.
- `CHANGETIME((A4, 2), 0.5)`: halves the duration of `(A4, 2)` (which then simply becomes `A4`).

### LET

The `LET` expression allows you to define a variable that can hold a song.
Once defined, the variable can be used throughout the rest of your computation. A `LET`
is defined as follows:

```text
LET variable = VALUE IN EXPR
```

where `variable` is the name of the variable, `VALUE` is the value that will be mapped to `variable` and
`EXPR` is the expression that will be evaluated once `variable` has been successfully binded.

#### Examples

- `LET x = A4 in x -> x`: generates `A4 -> A4`.
- `LET x = A5 in LET y = B5 in x -> y`: generates `A5 -> B5`.

## Commands

# Basic examples

In the main folder of the repository you will be able to find four examples.

- `example_1.txt` will generate the C major scale, and is the simplest example of the three.
- `example_2.txt` will generate the refrain of *Never gonna give you up* by Rick Astley.
- `example_3.txt` will generate the refrain of the theme of Dario Moccia's *Momento gaming* with three different durations.
- `example_4.txt` will generate the refrain of *Ode to Joy* by Ludwig van Beethoven

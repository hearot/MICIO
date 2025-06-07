from __future__ import annotations

from dataclasses import dataclass
from typing import cast, Iterator, Literal, Sequence

import argparse
import sys

from lark import Lark, Token, Tree

from pydub import AudioSegment
from pydub.generators import Sawtooth

type NoteName = Literal["A", "B", "C", "D", "E", "F", "G",
                        "Do", "Re", "Mi", "Fa", "Sol", "So", "La", "Si", "Ti", "H"]
type NoteModifier = Literal["#", "b"]

# 12th root of 2 -- used for shift pitch by semitones
SEMITONE_RATIO = 1.0594631

# Frequencies of the 4th octave notes (A4 up to G4)
NOTE_FREQUENCIES = {
    "A": 440.00, "B": 493.88, "C": 261.63, "D": 293.66,
    "E": 329.63, "F": 349.23, "G": 392.00,
}

# Fixed-Do solfège
NOTE_FREQUENCIES = NOTE_FREQUENCIES | {
    "Do": NOTE_FREQUENCIES["C"], "Re": NOTE_FREQUENCIES["D"],
    "Mi": NOTE_FREQUENCIES["E"], "Fa": NOTE_FREQUENCIES["F"],
    "Sol": NOTE_FREQUENCIES["G"], "So": NOTE_FREQUENCIES["G"],
    "La": NOTE_FREQUENCIES["A"], "Si": NOTE_FREQUENCIES["B"],
    "Ti": NOTE_FREQUENCIES["B"], "H": NOTE_FREQUENCIES["B"]
}


@dataclass
class Note:
    """
    A dataclass representing a musical note with a set frequency and duration (time) in milliseconds.

    Attributes:
        frequency (float): The frequency of the note in Hz.
        time (int): The duration of the note in milliseconds (default is 1000 ms).
    """
    frequency: float
    time: int = 1000  # ms

    @staticmethod
    def generate(name: NoteName, octave: int, modifier: NoteModifier | None = None):
        """
        Generate a Note object based on the given note name, octave, and optional modifier (diesis or flat). It yields the
        note according to the frequencies stored in `NOTE_FREQUENCIES`.

        Args:
            name (NoteName): The name of the note in the letter-naming system ("A", "B", "C", "D", "E", "F", or "G") or
                             the fixed-Do solfège ("Do", "Re", "Mi", "Fa", "Sol"/"So", "La", or "Si"/"Ti"/"H").
            octave (int): The octave in which the note belongs.
            modifier (NoteModifier | None): Optional modifier for the note ("#" for sharp, "b" for flat, or `None` for the normal note).

        Returns:
            Note: A Note object with the calculated frequency and default time of 1000 ms.

        Example:
            Note.generate("A", 4)  # Returns the Note object for A4.
            Note.generate("C", 4, "#")  # Returns the Note object for C#4.
        """
        frequency = NOTE_FREQUENCIES[name] * (2 ** (int(octave) - 4))

        if modifier == "#":
            frequency *= SEMITONE_RATIO
        elif modifier == "b":
            frequency /= SEMITONE_RATIO

        return Note(frequency=round(frequency, 2), time=1000)

    def transposed(self, value: int) -> Note:
        """
        Transpose the current note by a given number of semitones.

        Args:
            value (int): The number of semitones to transpose. Positive values transpose the note up, 
                         and negative values transpose it down. The new frequency is calculated as
                         `NEW_FREQUENCY = OLD_FREQUENCY * SEMITONE_RATIO ** value`.
        Returns:
            Note: A new Note object with the transposed frequency and the same duration.

        Example:
            note = Note.generate("A", 4)  # A4
            transposed_note = note.transposed(2)  # Transposes A4 to B4.
        """
        return Note(frequency=round(self.frequency * (SEMITONE_RATIO ** value), 2), time=self.time)

    def change_time(self, value: float) -> Note:
        """
        Change the duration of the note by a given factor.

        Args:
            value (float): The factor by which the duration of the note is changed. For example, a value
                           of 2.0 will double the duration, while 0.5 will halve it.
        Returns:
            Note: A new Note object with the adjusted duration and the same frequency.

        Example:
            note = Note.generate("A", 4)  # A4
            longer_note = note.change_time(2.0)  # Doubles the duration of A4.
        """
        return Note(frequency=self.frequency, time=int(self.time * value))

    def set_time(self, time: float):
        """
        Set the duration of the note to a new value.

        Args:
            time (float): The new duration of the note, in seconds. It is automatically converted to milliseconds.

        Example:
            note = Note.generate("A", 4)  # A4
            note.set_time(1.5)  # Sets the note's duration to 1.5 seconds (1500 ms).
        """
        self.time = int(time * 1000)


@dataclass
class Pause:
    """
    A dataclass representing a period of silence in a song.

    Attributes:
        time (int): The duration of the pause in milliseconds.
    """

    time: int  # ms

    def change_time(self, value: float) -> Pause:
        """
        Change the duration of the pause by a given factor.

        Args:
            value (float): The factor by which the duration of the pause is changed. For example, a value
                           of 2.0 will double the duration, while 0.5 will halve it.
        Returns:
            Pause: A new Pause object with the adjusted duration.

        Example:
            pause = Pause(1000)  # Pause for one second
            longer_pause = note.change_time(2.0)  # Doubles the duration of the pause, i.e. pause for two seconds.
        """
        return Pause(time=int(self.time * value))


@dataclass
class Harmony:
    """
    A dataclass representing a harmony, namely a sequence of
    notes to be played simultaneously.

    Attributes:
        notes (Sequence[Note]): The sequence of notes that make up the harmony.
    """

    notes: Sequence[Note]


@dataclass
class Song:
    steps: list[Harmony | Pause]

    @staticmethod
    def empty() -> Song:
        return Song(steps=cast(list[Harmony | Pause], []))

    def __add__(self, other: Song | Harmony | Pause) -> Song:
        match other:
            case Song():
                return Song(steps=self.steps + other.steps)
            case Harmony() | Pause():
                return Song(steps=self.steps + [other])
            case _:
                raise TypeError(
                    f"The right operand in Song + ... is neither a Song, nor a Harmony, nor a Pause.")

    def __iter__(self) -> Iterator[Harmony | Pause]:
        return iter(self.steps)


@dataclass
class Var:
    """
    A dataclass representing a variable that can be used in the musical expressions.

    Attributes:
        name (str): The name of the variable.
    """
    name: str


@dataclass
class Let:
    """
    A dataclass representing a 'LET' expression which binds a value to a variable within a scope.

    Attributes:
        var (Var): The variable being defined.
        value (Expression): The value being assigned to the variable.
        expr (Expression): The expression that will be evaluated using the defined variable.
    """
    var: Var
    value: Expression
    expr: Expression


@dataclass
class Concat:
    """
    A dataclass representing the concatenation of two musical expressions.
    This allows combining different parts of a song.

    Attributes:
        left (Expression): The first part of the expression.
        right (Expression): The second part of the expression.
    """
    left: Expression
    right: Expression


@dataclass
class Transpose:
    """
    A dataclass representing a transpose operation applied to a song,
    shifting all the notes by a specific number of semitones.

    Attributes:
        song (Expression): The song or part of the song to be transposed.
        value (int): The number of semitones to transpose. Positive values transpose upwards, negative values downwards.
                     The new frequency is calculated as `NEW_FREQUENCY = OLD_FREQUENCY * SEMITONE_RATIO ** value`.
    """
    song: Expression
    value: int


@dataclass
class ChangeTime:
    """
    A dataclass representing a change in the timing of a song by a given factor.

    Attributes:
        song (Expression): The song or part of the song whose timing is to be changed.
        value (float): The factor by which the duration of the note is changed.
                       For example, 2.0 doubles the time, and 0.5 halves it.
    """
    song: Expression
    value: float


@dataclass
class FunctionDecl:
    """
    A dataclass representing the declaration of a function which returns the result of
    evaluating an expression with multiple arguments.

    Attributes:
        name (str): The name of the function.
        params (Sequence[str]): The sequence of argument names required by the function.
                                For example, in FUNCTION F(X, Y) = ..., the arguments are X and Y.
        body (Expression): The expression to evaluate, with the arguments replaced by their
                           corresponding values.
    """
    name: str
    params: Sequence[str]
    body: Expression


@dataclass
class FunctionApply:
    """
    A dataclass representing the application of a function to multiple arguments.

    Attributes:
        name (str): The name of the function to call.
        args (Sequence[Expression]): The sequence of expressions passed as arguments to the function.
                                     For example, in F(X, Y) = ..., the arguments are the evaluations of
                                     X and Y.
    """
    name: str
    args: Sequence[Expression]


@dataclass
class Assign:
    """
    A dataclass representing the assignment of an expression to a variable.

    Attributes:
        var (Var): The variable to which the expression is bound.
        expr (Expression): The expression being assigned to the variable.
    """
    var: Var
    expr: Expression


@dataclass
class Export:
    """
    A dataclass representing the export of a song (defined as an expression which
    requires evaluation) to a WAVE file.

    Attributes:
        expr (Expression): The expression representing the song to be evaluated and exported.
        filename (str): The name of the WAVE file to export the song to.
    """
    expr: Expression
    filename: str


type Expression = Song | Let | Concat | Transpose | Var | ChangeTime | FunctionApply

type Command = Assign | FunctionDecl | Export
type CommandSeq = list[Command]

type Location = int
type EVal = Song
type MVal = Song
type DVal = Location | FunctionDecl  # TODO: constants?

type Environment = dict[str, DVal]


@dataclass
class State:
    store: list[MVal]
    next_loc: Location

    @staticmethod
    def empty() -> State:
        return State(store=cast(list[MVal], []), next_loc=0)

    def allocate(self, value: MVal) -> tuple[Location, State]:
        loc = self.next_loc

        return loc, State(store=self.store + [value], next_loc=loc + 1)

    def update(self, addr: Location, value: MVal) -> State:
        return State(store=self.store[:addr-1] + [value] + self.store[addr+1:], next_loc=self.next_loc)

    def access(self, addr: Location) -> MVal:
        return self.store[addr]


# The grammar defining the structure of the musical expressions
# accepted by the parser.
grammar = r"""
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
"""

parser = Lark(grammar, start="command_seq")


def export_song(song: Song, output_name: str) -> None:
    """
    Exports the given song as a `.wav` file. The function combines multiple
    notes coming from the same harmony, handles pauses, and finally generates the audio
    corresponding to the concatenation of said handles and pauses.
    It uses `AudioSegment` and `Sine` from `pydub` to create, mix and overlay the audio.

    Args:
        song (Song): The song to be exported, represented as a sequence of harmonies and pauses.
        output_name (str): The name of the output file. 
    """
    combined = AudioSegment.silent(duration=0)  # default segment

    for item in song:
        if isinstance(item, Pause):
            combined += AudioSegment.silent(
                duration=item.time)  # adds a pause to the song, according to the evaluated expression
        elif isinstance(item, Harmony):
            # get the duration of the current harmony
            duration = max(note.time for note in item.notes)
            # set the base segment which all the notes are going to be overlaid on
            harmony_audio = AudioSegment.silent(duration=duration)

            for note in item.notes:
                note_audio = Sawtooth(note.frequency).to_audio_segment(
                    # generate the wave of the note, with a fade in and fade out of 15ms
                    duration=note.time).fade_in(15).fade_out(15)
                harmony_audio = harmony_audio.overlay(note_audio)

            combined += harmony_audio  # adds the segment to the final song
        else:
            raise TypeError(f"{item} is neither a Pause nor a Harmony.")

    # add a final silent segment for 100ms
    combined += AudioSegment.silent(duration=100)

    combined.export(output_name, format="wav")


def transpose(song: Song, value: int) -> Song:
    """
    Transposes the song by shifting all notes by the specified number of semitones.

    This function iterates over the song and applies `Note.transposed` to each note, shifting its frequency 
    according to the given value.

    Args:
        song (Song): The song to be transposed.
        value (int): The number of semitones to transpose. Positive values transpose upwards, negative values downwards.

    Returns:
        Song: A new song with the transposed notes, maintaining the original structure and timing of harmonies and pauses.
    """
    transposed = Song.empty()

    for step in song:
        if isinstance(step, Harmony):
            transposed = transposed + \
                Harmony(notes=[note.transposed(value) for note in step.notes])
        elif isinstance(step, Pause):
            transposed = transposed + step
        else:
            raise TypeError(f"{step} is not a Harmony nor a Pause.")

    return transposed


def change_time(song: Song, value: float) -> Song:
    """
    Changes the duration of all notes and pauses
    in the song by multiplying their duration by the specified factor.

    Args:
        song (Song): The song to be sped up or slowed down.
        value (float): The factor by which to change the duration (e.g., 2.0 for 2x, 0.5 for 0.5x).

    Returns:
        Song: The song adjusted in speed.
    """
    changed = Song.empty()

    for step in song:
        if isinstance(step, Harmony):
            changed = changed + \
                Harmony(notes=[note.change_time(value) for note in step.notes])
        elif isinstance(step, Pause):
            changed = changed + step.change_time(value)
        else:
            raise TypeError(f"{step} is not a Harmony nor a Pause.")
    return changed


def transform_parse_commandseq_tree(tree: Tree) -> CommandSeq:
    match tree:
        case Tree(data="command_seq", children=children):
            return [transform_parse_command_tree(child) for child in children]

        case _:
            raise TypeError(
                f"Cannot parse a tree of unknown type {type(tree)}.")


def transform_parse_command_tree(tree: Tree) -> Command:
    match tree:
        case Tree(data="assign", children=[Token(type="IDENTIFIER", value=name), expr]):
            return Assign(var=Var(name=name), expr=transform_parse_expr_tree(expr))

        case Tree(data="fundecl", children=[Token(type="IDENTIFIER", value=name), Tree(data="params", children=params), expr]):
            params = [param.value for param in params]

            if len(params) > len(set(params)):  # if there are multiple instances of the same variable
                raise SyntaxError(
                    f"Multiple instances of variables found in the declaration of the function {name}")

            return FunctionDecl(name=name, params=params, body=transform_parse_expr_tree(expr))

        case Tree(data="export", children=[expr, Token(type="FILENAME", value=filename)]):
            return Export(expr=transform_parse_expr_tree(expr), filename=filename)

        case _:
            raise TypeError(
                f"Cannot parse a command of unknown type {type(tree)}.")


def transform_parse_time_tree(tree: Tree) -> float:
    match tree:
        case Tree(data="time", children=[Token(type="NUMBER", value=seconds)]):
            return float(seconds)

        case Tree(data="time", children=[Tree(data="fraction", children=[Token(type="NUMBER", value=num), Token(type="NUMBER", value=den)])]):
            return float(int(num) / int(den))

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}.")


def transform_parse_note_tree(tree: Tree) -> Note:
    match tree:
        case Tree(data="note", children=[Token("NOTE", name), Token("MODIFIER", modifier), Token("NUMBER", octave)]):
            return Note.generate(name=name, octave=octave, modifier=modifier)

        case Tree(data="note", children=[Token("NOTE", name), Token("NUMBER", octave)]):
            return Note.generate(name=name, octave=octave)

        case Tree(data="note_time", children=[note, time]):
            note = transform_parse_note_tree(note)
            note.set_time(transform_parse_time_tree(time))
            return note

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}.")


def transform_parse_step_tree(tree: Tree) -> Harmony | Pause:
    match tree:
        case Tree(data="harmony", children=children):
            return Harmony(notes=[transform_parse_note_tree(child) for child in children])

        case Tree(data="pause", children=[time]):
            return Pause(time=int(1000 * transform_parse_time_tree(time)))

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}.")


def transform_parse_expr_tree(tree: Tree) -> Expression:
    """
    Transforms the parse tree produced by the parser in an expression context
    into a structured musical expression.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from an expression (`expr`) tree.

    Returns:
        Expression: The corresponding musical expression.
    """
    match tree:
        case Tree(data="concat", children=[left, right]):
            return Concat(left=transform_parse_expr_tree(left), right=transform_parse_expr_tree(right))

        case Tree(data="let", children=[Token(type="IDENTIFIER", value=var), value, expr]):
            return Let(var=Var(name=var), value=transform_parse_expr_tree(value), expr=transform_parse_expr_tree(expr))

        case Tree(data="step", children=[subtree]):
            # A step is a single harmony or a pause, but internally is already
            # regarded as a song with that single harmony/pause, i.e. [A4] is already
            # seen as [[A4]].
            return Song.empty() + transform_parse_step_tree(subtree)

        case Tree(data="transpose", children=[subtree, Token(type="TRANSPOSE_VALUE", value=value)]):
            return Transpose(song=transform_parse_expr_tree(subtree), value=int(value))

        case Tree(data="changetime", children=[subtree, time]):
            return ChangeTime(song=transform_parse_expr_tree(subtree), value=transform_parse_time_tree(time))

        case Tree(data="funapply", children=[Token(type="IDENTIFIER", value=name), Tree(data="arg_list", children=args)]):
            return FunctionApply(name=name, args=[transform_parse_expr_tree(arg) for arg in args])

        case Tree(data="var", children=[Token(type="IDENTIFIER", value=name)]):
            return Var(name=name)

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}.")


def parse_ast(program: str) -> CommandSeq:
    """
    Parses the input string expression into a sequence of abstract syntax trees (ASTs)
    composed of dataclasses.

    Args:
        expression (str): The string expression to be parsed.

    Returns:
        CommandSeq: The resulting sequence of parsed commands as ASTs.
    """
    parse_tree = parser.parse(program)
    return transform_parse_commandseq_tree(parse_tree)


def evaluate_expr(ast: Expression, env: Environment, state: State) -> Song:
    """
    Evaluates an abstract syntax tree (AST) in the form of an `Expression` to produce a `Song`,
    using the given environment and state.

    Args:
        ast (Expression): The abstract syntax tree of the `Expression` to evaluate.
        env (Environment): The current environment, which maps variable names to denotable values.
        state (State): The current state, which maps location to `Song` object (the only memorizable type).

    Returns:
        Song: The resulting song after evaluation.
    """
    match ast:
        case Song():
            return ast

        case Let(var=var, value=value, expr=expr):
            # Creates the temporary environment and state, which will
            # be used to evaluate the expression of the `Let`
            # construct.
            loc, temporary_state = state.allocate(
                evaluate_expr(value, env, state))

            temporary_env = env.copy()
            temporary_env[var.name] = loc

            return evaluate_expr(expr, temporary_env, temporary_state)

        case Concat(left=left, right=right):
            return evaluate_expr(left, env, state) + evaluate_expr(right, env, state)

        case Transpose(song=music, value=value):
            return transpose(evaluate_expr(music, env, state), value)

        case ChangeTime(song=music, value=value):
            return change_time(evaluate_expr(music, env, state), value)

        case Var(name=name):
            if name in env.keys():  # if `name` is in the environment
                dvalue = env[name]

                # if `name` points to a Location (i.e., an integer)
                if isinstance(dvalue, int):
                    return state.access(dvalue)
                elif isinstance(dvalue, FunctionDecl):
                    raise TypeError(f"{name} is a function, not a song.")
                else:
                    raise TypeError(
                        f"Couldn't convert {name} to a proper location")

            raise NameError(f"Variable '{name}' is not defined")

        case FunctionApply(name=function_name, args=args):
            if function_name in env.keys():
                dvalue = env[function_name]

                if isinstance(dvalue, FunctionDecl):
                    # if the number of arguments matches the arity of the function
                    if len(args) == len(dvalue.params):
                        temporary_env = env.copy()

                        # Creates the temporary environment and state, and
                        # binds each argument to the corresponding variable;
                        # it's done in order to evaluate the function.
                        for arg, arg_value in zip(dvalue.params, args):
                            loc, temporary_state = state.allocate(
                                evaluate_expr(arg_value, env, state))

                            temporary_env[arg] = loc

                        return evaluate_expr(dvalue.body, temporary_env, temporary_state)
                    else:  # arity not matched
                        raise TypeError(
                            f"{function_name} takes {len(dvalue.params)} arguments but {len(args)} were given")
                else:
                    raise TypeError(f"{function_name} is not a function.")
            else:
                raise NameError(f"Function '{function_name}' is not defined")

        case _:
            raise TypeError(
                f"Cannot evaluate an expression of unknown type {type(ast)}.")


def evaluate_command(ast: Command, env: Environment, state: State) -> tuple[Environment, State]:
    """
    Evaluates an abstract syntax tree (AST) in the form of a `Command` to evaluate a command and
    produce a new environment and state.

    Args:
        ast (Expression): The abstract syntax tree of the `Command` to evaluate.
        env (Environment): The current environment, which maps variable names to denotable values.
        state (State): The current state, which maps location to `Song` object (the only memorizable type).

    Returns:
        Environment: The new environment, obtained after executing the command.
        State: The new state, obtained after executing the command.
    """
    match ast:
        case Assign(var=var, expr=expr):
            if var.name in env.keys():
                loc = env[var.name]

                if isinstance(loc, int):
                    state = state.update(loc, evaluate_expr(expr, env, state))
                else:
                    raise TypeError(
                        "Cannot assign a new value to a variable that does not reference a valid location.")
            else:
                loc, state = state.allocate(
                    evaluate_expr(expr, env, state))

                env = env.copy()
                env[var.name] = loc

            return env, state

        case FunctionDecl(name=function_name, params=params, body=body):
            env = env.copy()
            env[function_name] = ast

            return env, state

        case Export(expr=expr, filename=filename):
            try:
                # print(evaluate_expr(expr, env, state))
                export_song(evaluate_expr(expr, env, state), filename)
            except Exception as e:
                raise Exception(f"Export & conversion error: {e}")

            return env, state


def evaluate_code(code: str, env: Environment, state: State, sysexit_on_error: bool = False) -> None:
    """
    Evaluates the provided code.

    Args:
        code (str): The code to be evaluated.
        sysexit_on_error (bool): Whether to exit the program on error.
    """
    try:
        ast = parse_ast(code)
    except Exception as e:
        print(f"Syntax error: {e}")

        if sysexit_on_error:
            sys.exit(1)
        else:
            return

    try:
        for command in ast:
            # print(env)
            # print(state.store)
            # print()

            env, state = evaluate_command(command, env, state)

        # print(env)
        # print(state.store)
    except Exception as e:
        print(f"Runtime error: {e}")

        if sysexit_on_error:
            sys.exit(1)
        else:
            return


def main() -> None:
    """
    The main entry point of the program, called only if this file has been executed directly through
    the Python interpreter. It handles command-line arguments and calls the musical code evaluation.

    The program can either (depending on the command-line arguments):
    - Process an input file containing MICIO code.
    - Enter a REPL (Read-Eval-Print Loop) where the user can input MICIO commands interactively.
    """
    parser = argparse.ArgumentParser(
        description="MICIO - A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3.12!"
    )

    parser.add_argument(
        "filename", nargs="?", help="the file containing the code to be evaluated (default is a REPL)."
    )

    args = parser.parse_args()

    # The default dictionary, which maps variable names to
    # their corresponding songs.
    env: Environment = {}

    state: State = State.empty()

    if args.filename:
        try:
            with open(args.filename, "r") as file:
                code = file.read()
                evaluate_code(code, env, state, sysexit_on_error=True)
        except FileNotFoundError:
            print(f"Error: File '{args.filename}' not found.")
            sys.exit(1)
        except PermissionError:
            print(
                f"Error: You don't have permission to read '{args.filename}'.")
            sys.exit(1)
        except IOError as e:
            print(f"Unexpected I/O error: {e}")
        except Exception as e:
            print(f"Unexpected error: {e}")
            sys.exit(1)
    else:
        try:
            while True:  # REPL
                to_eval = input(
                    "Enter an expression (write 'exit' to quit).\n>>> ")

                if to_eval == "exit":
                    break
                else:
                    evaluate_code(to_eval, env, state)
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()

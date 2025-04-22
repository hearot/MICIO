from __future__ import annotations

from dataclasses import dataclass
from typing import List, Literal

import argparse
import sys

from lark import Lark, Token, Tree

from pydub import AudioSegment
from pydub.generators import Sine

type NoteName = Literal["A", "B", "C", "D", "E", "F", "G"]
type NoteModifier = Literal["#", "b"]

# Default name for the output file when no specific
# name is provided and the REPL is active.
DEFAULT_OUTPUT_NAME = "output.wav"

# 12th root of 2 -- used for shift pitch by semitones
SEMITONE_RATIO = 1.0594631

# Frequencies of the 4th octave notes (A4 up to G4)
NOTE_FREQUENCIES = {
    "A": 440.00, "B": 493.88, "C": 261.63, "D": 293.66,
    "E": 329.63, "F": 349.23, "G": 392.00
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
            name (NoteName): The name of the note ("A", "B", "C", "D", "E", "F", or "G").
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
        return Note(frequency=self.frequency, time=self.time * value)

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


type Harmony = List[Note]

# Song represents the only type MICIO handles, stores, and
# returns. Each song is simply a list of harmonies or
# pauses.
type Song = List[Harmony | Pause]


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


type Expression = Song | Let | Concat | Transpose | Var | ChangeTime

type Environment = dict[str, Song]

# A dictionary that maps variable names to
# their corresponding songs.
env: Environment = {}


# The grammar defining the structure of the musical expressions
# accepted by the parser.
grammar = r"""
    ?expr: concat | mono | let
    mono: step | transpose | paren | var | changetime
    paren: "(" expr ")"
    var: IDENTIFIER

    let: "LET" IDENTIFIER "=" expr "IN" expr
    transpose: "TRANSPOSE(" expr "," TRANSPOSE_VALUE ")"
    changetime: "CHANGETIME(" expr "," time ")"

    step: harmony | pause
    
    harmony: note | note_time | "[" sequence_notes "]"

    sequence_notes: mono_note | mono_note "," sequence_notes

    mono_note: note | note_time
    note_time: "(" note "," time ")"

    note: note_with_modifier | note_without_modifier
    note_with_modifier: NOTE MODIFIER NUMBER
    note_without_modifier: NOTE NUMBER

    pause: "PAUSE(" time ")"

    concat: mono "->" expr | mono ">" expr
    
    time: fraction | NUMBER
    fraction: NUMBER "/" NUMBER

    TRANSPOSE_VALUE: /[-]?[0-9]+/ 

    MODIFIER: "#" | "b"
    NOTE: "A" | "B" | "C" | "D" | "E" | "F" | "G" 
    
    IDENTIFIER: /[a-zA-Z_][a-zA-Z0-9_]*/
    NUMBER: /[0-9]+/

    %import common.WS
    %ignore WS
"""

parser = Lark(grammar, start="expr")


def export_song(song: Song, output_name: str | None = None) -> None:
    """
    Exports the given song as a `.wav` file. The function combines harmonies,
    handles pauses, and generates the audio for each note in the song.
    It uses `AudioSegment` and `Sine` from `pydub` to create, mix and overlay the audio.

    Args:
        song (Song): The song to be exported, represented as a sequence of harmonies and pauses.
        output_name (str | None): The name of the output file. If `None`, the `DEFAULT_OUTPUT_NAME` is used.
    """
    global DEFAULT_OUTPUT_NAME

    if not output_name:
        output_name = DEFAULT_OUTPUT_NAME

    combined = AudioSegment.silent(duration=0)

    for item in song:
        if isinstance(item, Pause):
            pause_audio = AudioSegment.silent(
                duration=item.time)
            combined += pause_audio
        else:
            duration = max(note.time for note in item)
            harmony_audio = AudioSegment.silent(duration=duration)

            for note in item:
                note_audio = Sine(note.frequency).to_audio_segment(
                    duration=note.time).fade_in(15).fade_out(15)
                harmony_audio = harmony_audio.overlay(note_audio)

            combined += harmony_audio

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
    transposed = []

    for step in song:
        if isinstance(step, list):
            transposed.append([note.transposed(value) for note in step])
        else:
            transposed.append(step)

    return transposed


def change_time(song: Song, value: float) -> Song:
    """
    Changes the duration of all notes in the song by multiplying their duration by the specified factor.

    Args:
        song (Song): The song whose note durations are to be changed.
        value (float): The factor by which to change the duration (e.g., 2.0 for 2x, 0.5 for 0.5x).

    Returns:
        Song: The song with adjusted note durations.
    """
    changed = []
    for step in song:
        if isinstance(step, list):
            changed.append([note.change_time(value) for note in step])
        else:
            changed.append(step)
    return changed


def transform_parse_tree(tree: Tree) -> Expression:
    """
    Transforms the parse tree produced by the parser into a structured musical expression.

    Args:
        tree (Tree): The parse tree generated by the Lark parser.

    Returns:
        Expression: The corresponding musical expression.
    """
    match tree:
        case Tree(data="expr", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="concat", children=[left, right]):
            return Concat(left=transform_parse_tree(left), right=transform_parse_tree(right))
        case Tree(data="mono", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="let", children=[Token(type="IDENTIFIER", value=var), value, expr]):
            return Let(var=var, value=transform_parse_tree(value), expr=transform_parse_tree(expr))
        case Tree(data="step", children=[subtree]):
            return [transform_parse_tree(subtree)]
        case Tree(data="transpose", children=[subtree, Token(type="TRANSPOSE_VALUE", value=value)]):
            return Transpose(song=transform_parse_tree(subtree), value=int(value))
        case Tree(data="changetime", children=[subtree, time]):
            return ChangeTime(song=transform_parse_tree(subtree), value=transform_parse_tree(time))
        case Tree(data="paren", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="var", children=[Token(type="IDENTIFIER", value=name)]):
            return Var(name=name)
        case Tree(data="step", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="harmony", children=[subtree]):
            match subtree:
                case Tree(data="note") | Tree(data="note_time"):
                    return [transform_parse_tree(subtree)]
                case Tree(data="sequence_notes"):
                    return transform_parse_tree(subtree)
        case Tree(data="pause", children=[time]):
            return Pause(time=int(1000 * transform_parse_tree(time)))
        case Tree(data="mono_note", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="sequence_notes", children=[mono_note, *sub_notes]):
            notes = [transform_parse_tree(mono_note)]

            if sub_notes:
                notes.extend(transform_parse_tree(sub_notes[0]))

            return notes
        case Tree(data="note", children=[subtree]):
            return transform_parse_tree(subtree)
        case Tree(data="note_time", children=[note, time]):
            note = transform_parse_tree(note)
            note.set_time(transform_parse_tree(time))
            return note
        case Tree(data="time", children=[Token(type="NUMBER", value=seconds)]):
            return int(seconds)
        case Tree(data="time", children=[Tree(data="fraction", children=[Token(type="NUMBER", value=num), Token(type="NUMBER", value=den)])]):

            return float(int(num) / int(den))
        case Tree(data="note_with_modifier", children=[
                Token(type="NOTE", value=name),
                Token(type="MODIFIER", value=modifier),
                Token(type="NUMBER", value=octave)]):
            return Note.generate(name=name, octave=int(octave), modifier=modifier)
        case Tree(data="note_without_modifier", children=[
                Token(type="NOTE", value=name),
                Token(type="NUMBER", value=octave)]):
            return Note.generate(name=name, octave=int(octave))


def parse_ast(expression: str) -> Expression:
    """
    Parses the input string expression into an abstract syntax tree (AST) made of
    dataclasses.

    Args:
        expression (str): The string expression to be parsed.

    Returns:
        Expression: The resulting abstract syntax tree representing the musical expression.
    """
    parse_tree = parser.parse(expression)
    return transform_parse_tree(parse_tree)


def evaluate(ast: Expression, env: Environment) -> Song:
    """
    Evaluates an abstract syntax tree (AST) in the form of an `Expression` to produce a song,
    using the given environment.

    Args:
        ast (Expression): The abstract syntax tree to be evaluated.
        env (Environment): The current environment, which maps variable names to song values.

    Returns:
        Song: The resulting song.
    """
    match ast:
        case Let(var=var, value=value, expr=expr):
            temporary_env = env.copy()
            temporary_env[var] = evaluate(value, env)
            return evaluate(expr, temporary_env)
        case Concat(left=left, right=right):
            return evaluate(left, env) + evaluate(right, env)
        case Transpose(song=music, value=value):
            return transpose(evaluate(music, env), value)
        case ChangeTime(song=music, value=value):
            return change_time(evaluate(music, env), value)
        case Var(name=name):
            if name in env.keys():
                return env[name]

            raise NameError(f"Variable '{name}' is not defined")
        case _:
            if isinstance(ast, list) and all(isinstance(item, Pause) or (isinstance(item, List) and all(isinstance(nested_item, Note) for nested_item in item)) for item in ast):
                return ast

            raise TypeError("The evaluated expression is not a song")


def evaluate_code(code: str, output_file: str, sysexit_on_error: bool = False) -> None:
    """
    Evaluates the provided code and outputs the resulting song to a file.
    If the evaluation is successful, the resulting song is played and exported to a `.wav` file.

    Args:
        code (str): The code to be evaluated.
        output_file (str): The name of the output file.
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
        song = evaluate(ast, env)
    except Exception as e:
        print(f"Runtime error: {e}")

        if sysexit_on_error:
            sys.exit(1)
        else:
            return

    # print(song)

    try:
        export_song(song, output_file)
    except Exception as e:
        print(f"Conversion error: {e}")

        if sysexit_on_error:
            sys.exit(1)


def main():
    """
    The main entry point of the program, called only if this file has been executed directly through
    the Python interpreter. It handles command-line arguments and calls the musical code evaluation.

    The program can either (depending on the command-line arguments):
    - Process an input file containing musical code.
    - Enter a REPL (Read-Eval-Print Loop) where the user can input musical expressions interactively.

    The program then evaluates the code and generates the corresponding song in `.wav` format.
    """
    parser = argparse.ArgumentParser(
        description="MICIO - A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3."
    )

    parser.add_argument(
        "filename", nargs="?", help="the file that needs to be converted to the Waveform Audio File (WAVE, '.wav') format (default is a REPL)."
    )

    parser.add_argument(
        "-o", "--output",
        default=DEFAULT_OUTPUT_NAME,
        help=f"set the output file name (default is '{DEFAULT_OUTPUT_NAME}' for a REPL, and 'filename.wav' for 'filename.ext' as filename); if the output file is '{DEFAULT_OUTPUT_NAME}', it is handled as the default case."
    )

    args = parser.parse_args()

    if args.output == DEFAULT_OUTPUT_NAME and args.filename:
        output = args.filename.split(".")[0] + ".wav"
    else:
        output = args.output

    if args.filename:
        try:
            with open(args.filename, "r") as file:
                code = file.read()
                evaluate_code(code, output, sysexit_on_error=True)
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
            while True:
                to_eval = input(
                    "Enter an expression (write 'exit' to quit).\n>>> ")

                if to_eval == "exit":
                    break
                else:
                    evaluate_code(to_eval, args.output)
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()

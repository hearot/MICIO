from __future__ import annotations

from dataclasses import dataclass, field
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

# EXPRESSIONS


@dataclass(order=True)
class Note:
    """
    A dataclass representing a musical note with a set frequency and duration (time) in milliseconds.

    Attributes:
        frequency (float): The frequency of the note in Hz.
        time (int): The duration of the note in milliseconds (default is 1000 ms).
    """
    frequency: float = field(compare=True)
    time: int = field(default=1000, compare=True)  # ms

    @staticmethod
    def generate(name: NoteName, octave: int, modifier: NoteModifier | None = None) -> Note:
        """
        Generate a Note object based on the given note name, octave, and optional modifier (diesis or flat). It returns the
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
            longer_pause = pause.change_time(2.0)  # Doubles the duration of the pause, i.e., pauses for two seconds.
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

    def __post_init__(self):
        self.notes = sorted(self.notes)  # order doesn't count in a harmony


@dataclass
class Song:
    """
    A dataclass representing a song, namely a sequence of
    harmonies and pauses. It is the fundamental type employed by
    MICIO.

    Attributes:
        steps (list[Harmony | Pause]): The sequence of harmonies and pauses that make up the song.
    """

    steps: list[Harmony | Pause]

    @staticmethod
    def empty() -> Song:
        """
        Creates an empty song, with no harmonies nor pauses.

        Returns:
            Song: the empty Song object.
        """
        return Song(steps=cast(list[Harmony | Pause], []))

    def __add__(self, other: Song | Harmony | Pause) -> Song:
        """
        Concatenates the current Song with another Song, or appends a Harmony or Pause to
        the end of it.

        Args:
            other: A Song, Harmony, or Pause object to append or concatenate.

        Returns:
            Song: A new Song instance resulting from the concatenation or append operation.
        """
        match other:
            case Song():
                return Song(steps=self.steps + other.steps)
            case Harmony() | Pause():
                return Song(steps=self.steps + [other])
            case _:
                raise TypeError(
                    f"The right operand in Song + ... is neither a Song, nor a Harmony, nor a Pause")

    def __eq__(self, other: object) -> bool:
        """
        Checks equality between this Song and another object.
        Equality is implemented ONLY for pair of songs. 

        Args:
            other (object): The Song to compare with.

        Returns:
            bool: True if the other object is a Song with the same steps, False otherwise.
        """
        if not isinstance(other, Song):
            return NotImplemented

        return self.steps == other.steps

    def __iter__(self) -> Iterator[Harmony | Pause]:
        """
        Compute the iterator over the elements which compose the Song.

        Returns:
            Iterator[Harmony | Pause]: An iterator over the Harmony and Pause elements in the Song.
        """
        return iter(self.steps)

    def repeat(self, times: int) -> Song:
        """
        Returns a new Song consisting of the
        original Song repeated multiple times.

        Args:
            times (int): The number of times to repeat the Song.

        Returns:
            Song: The repeated Song.
        """
        return Song(steps=self.steps * times)


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
        expr (Expression): The expression that will be evaluated with the defined variable.
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
            raise TypeError(f"{step} is not a Harmony nor a Pause")

    return transposed


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
            raise TypeError(f"{step} is not a Harmony nor a Pause")
    return changed


@dataclass
class Repeat:
    """
    A dataclass representing the repetition of a musical expression a specified number of times.

    Attributes:
        song (Expression): The expression to be repeated.
        times (int): The number of times to repeat the expression.
    """
    song: Expression
    times: int


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
class IfElseExpr:
    """
    A dataclass representing a conditional expression which evaluates
    to one of two expressions based on the result of a boolean condition.

    Attributes:
        cond (Boolean): The condition to evaluate.
        if_true (Expression): The expression to evaluate if the condition is true.
        if_false (Expression): The expression to evaluate if the condition is false.
    """
    cond: Boolean
    if_true: Expression
    if_false: Expression


@dataclass
class Map:
    """
    A dataclass representing the mapping of an expression over each step of a song.

    Attributes:
        iter_var (Var): The variable used for iteration.
        expr (Expression): The song expression to iterate over.
        return_expr (Expression): The expression to evaluate for each step,
                                  after having bound iter_var.
    """
    iter_var: Var
    expr: Expression
    return_expr: Expression


type Expression = Song | Let | Concat | Transpose | Var | ChangeTime | FunctionApply | Repeat | IfElseExpr | Map

# COMMANDS


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
class ConstAssign:
    """
    A dataclass representing the declaration of a constant.

    Attributes:
        const (str): The identifier for the constant.
        expr (Expression): The expression being bound to the constant.
    """
    const: str
    expr: Expression


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


def export_song(song: Song, output_name: str) -> None:
    """
    Exports the given song as a `.wav` file. The function combines multiple
    notes coming from the same harmony, handles pauses, and finally generates the audio
    corresponding to the concatenation of said harmonies and pauses.
    It uses `AudioSegment` and `Sawtooth` from `pydub` to create, mix and overlay the audio.

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
            raise TypeError(f"{item} is neither a Pause nor a Harmony")

    # add a final silent segment for 100ms
    combined += AudioSegment.silent(duration=100)

    combined.export(output_name, format="wav")


@dataclass
class IfElse:
    """
    A dataclass representing a conditional command sequence.

    Attributes:
        cond (Boolean): The condition to evaluate.
        if_true (CommandSeq): The sequence of commands to execute if the condition is true.
        if_false (CommandSeq): The sequence of commands to execute if the condition is false.
    """
    cond: Boolean
    if_true: CommandSeq
    if_false: CommandSeq


type Command = ConstAssign | Assign | FunctionDecl | Export | IfElse
type CommandSeq = list[Command]


# BOOLEANS


@dataclass
class Equal:
    """
    A dataclass representing a boolean equality comparison between two expressions.

    Attributes:
        left (Expression): The left-hand side expression.
        right (Expression): The right-hand side expression.
    """
    left: Expression
    right: Expression


@dataclass
class NotEqual:
    """
    A dataclass representing a boolean inequality comparison between two expressions.

    Attributes:
        left (Expression): The left-hand side expression.
        right (Expression): The right-hand side expression.
    """
    left: Expression
    right: Expression


type Boolean = Equal | NotEqual

# ENVIRONMENTS & STATES

type Location = int

# values that are memorizable in the state
type MVal = Song

# denotable values for the environment
type DVal = Song | Location | FunctionDecl


@dataclass
class Environment:
    """
    A dataclass representing an environment that maps string identifiers
    to denotable values (i.e., locations which point to a Song expression in the
    current state, or functions).

    Attributes:
        env (dict[str, DVal]): A dictionary mapping identifiers to their bound values.
    """

    env: dict[str, DVal]

    @staticmethod
    def default() -> Environment:
        """
        Creates the default environment
        (i.e., an empty environment with the empty song object
        bound to the EMPTY identifier).

        Returns:
            Environment: The default Environment instance.
        """
        return Environment(env=cast(dict[str, DVal], {})).bind("EMPTY", Song.empty())

    def bind(self, identifier: str, value: DVal) -> Environment:
        """
        Returns a new Environment with the given identifier
        bound to the specified value.

        Args:
            identifier (str): The variable name to bind.
            value (DVal): The value to associate with the identifier.

        Returns:
            Environment: A new Environment instance which 
                         includes the new binding.
        """
        new_env = self.env.copy()
        new_env[identifier] = value
        return Environment(env=new_env)

    def contains_identifier(self, identifier: str) -> bool:
        """
        Checks if the environment contains a binding
        for the given identifier.

        Args:
            identifier (str): The variable name to check.

        Returns:
            bool: True if the identifier exists in the environment, False otherwise.
        """
        return identifier in self.env.keys()

    def copy(self) -> Environment:
        """
        Creates a shallow copy of the environment.

        Returns:
            Environment: A new Environment instance with a (shallow)
                         copy of the current bindings.
        """
        return Environment(env=self.env.copy())

    def get_value(self, identifier: str) -> DVal:
        """
        Retrieves the value bound to the given identifier.

        Args:
            identifier (str): The variable name to look up.

        Returns:
            DVal: The value associated with the identifier.
        """
        return self.env[identifier]


@dataclass
class State:
    """
    A dataclass representing the state of an execution. It binds locations
    to Song expressions.

    Attributes:
        store (list[MVal]): A list representing memory storing denotable values (i.e., MVal,
                            which is exactly Song).
        next_loc (Location): The next available location in the store.
    """

    store: list[MVal]
    next_loc: Location

    @staticmethod
    def empty() -> State:
        """
        Creates an empty state with an empty store and next_loc initialized to 0.

        Returns:
            State: An empty State instance.
        """
        return State(store=cast(list[MVal], []), next_loc=0)

    def allocate(self, value: MVal) -> tuple[Location, State]:
        """
        Allocates a new location in the store for the given value.

        Args:
            value (MVal): The value to store.

        Returns:
            tuple[Location, State]: A tuple containing the location allocated for the value,
                                    and a new State instance with the value added to the store
                                    and next_loc incremented.
        """
        loc = self.next_loc

        return loc, State(store=self.store + [value], next_loc=loc + 1)

    def copy(self) -> State:
        """
        Creates a shallow copy of the current state.

        Returns:
            State: A new State instance with a (shallow) copy of the store
                   and the same next_loc.
        """
        return State(store=self.store.copy(), next_loc=self.next_loc)

    def update(self, addr: Location, value: MVal) -> State:
        """
        Updates the value stored at a given location, returning a new state.

        Args:
            addr (Location): The location to update.
            value (MVal): The new value to store.

        Returns:
            State: A new State instance with the updated store.
        """
        if not (0 <= addr < len(self.store)):
            raise IndexError(
                f"Location {addr} out of bounds for state update")

        return State(store=self.store[:addr] + [value] + self.store[addr+1:], next_loc=self.next_loc)

    def access(self, addr: Location) -> MVal:
        """
        Retrieves the value stored at the specified location.

        Args:
            addr (Location): The location to access.

        Returns:
            MVal: The value stored at the location.
        """
        return self.store[addr]


# PARSING & TRANSFORMATION OF PARSE TREES


# The grammar defining the structure of the musical expressions
# accepted by the parser.
grammar = r"""
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
"""

parser = Lark(grammar, start="command_seq")


def transform_parse_boolean_tree(tree: Tree) -> Boolean:
    """
    Transforms the parse tree produced by the parser in a boolean context
    into an actual Boolean dataclass.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from a boolean (`boolean`) tree.

    Returns:
        Boolean: The Boolean translation of the tree.
    """
    match tree:
        case Tree(data="equal", children=[left, right]):
            return Equal(left=transform_parse_expr_tree(left), right=transform_parse_expr_tree(right))

        case Tree(data="not_equal", children=[left, right]):
            return NotEqual(left=transform_parse_expr_tree(left), right=transform_parse_expr_tree(right))

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}")


def transform_parse_time_tree(tree: Tree) -> float:
    """
    Transforms the parse tree produced by the parser in a time context
    into a float instance.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from a time (`time`) tree.

    Returns:
        float: The seconds deduced by the tree.
    """
    match tree:
        case Tree(data="time", children=[Token(type="NUMBER", value=seconds)]):
            return float(seconds)

        case Tree(data="time", children=[Tree(data="fraction", children=[Token(type="NUMBER", value=num), Token(type="NUMBER", value=den)])]):
            return float(int(num) / int(den))

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}")


def transform_parse_note_tree(tree: Tree) -> Note:
    """
    Transforms the parse tree produced by the parser in a note context
    into a structured instance of Note.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from a note (`note` or `note_time`) tree.

    Returns:
        Note: The structured instance of Note, deduced by the tree.
    """
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
                f"Cannot parse an expression of unknown type {type(tree)}")


def transform_parse_step_tree(tree: Tree) -> Harmony | Pause:
    """
    Transforms the parse tree produced by the parser in a step context
    (i.e., a harmony or a pause) into a structured expression made of
    dataclasses.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from a step (`step`) tree.

    Returns:
        Harmony | Pause: The structured instance of the corresponding dataclass.
    """
    match tree:
        case Tree(data="harmony", children=children):
            return Harmony(notes=[transform_parse_note_tree(child) for child in children])

        case Tree(data="pause", children=[time]):
            return Pause(time=int(1000 * transform_parse_time_tree(time)))

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}")


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

        case Tree(data="map", children=[Token(type="IDENTIFIER", value=var), expr, return_expr]):
            return Map(iter_var=Var(name=var), expr=transform_parse_expr_tree(expr),
                       return_expr=transform_parse_expr_tree(return_expr))

        case Tree(data="ifelse_expr", children=[if_true, boolean, if_false]):
            return IfElseExpr(cond=transform_parse_boolean_tree(boolean), if_true=transform_parse_expr_tree(if_true),
                              if_false=transform_parse_expr_tree(if_false))

        case Tree(data="step", children=[subtree]):
            # A step is a single harmony or a pause, but internally is already
            # regarded as a song with that single harmony/pause, i.e., [A4] is already
            # seen as [[A4]].
            return Song.empty() + transform_parse_step_tree(subtree)

        case Tree(data="transpose", children=[subtree, Token(type="TRANSPOSE_VALUE", value=value)]):
            return Transpose(song=transform_parse_expr_tree(subtree), value=int(value))

        case Tree(data="changetime", children=[subtree, time]):
            return ChangeTime(song=transform_parse_expr_tree(subtree), value=transform_parse_time_tree(time))

        case Tree(data="repeat", children=[subtree, Token(type="NUMBER", value=value)]):
            return Repeat(song=transform_parse_expr_tree(subtree), times=int(value))

        case Tree(data="funapply", children=[Token(type="IDENTIFIER", value=name), Tree(data="arg_list", children=args)]):
            return FunctionApply(name=name, args=[transform_parse_expr_tree(arg) for arg in args])

        case Tree(data="var", children=[Token(type="IDENTIFIER", value=name)]):
            return Var(name=name)

        case _:
            raise TypeError(
                f"Cannot parse an expression of unknown type {type(tree)}")


def transform_parse_command_tree(tree: Tree) -> Command:
    """
    Transforms the parse tree produced by the parser in a command context
    into a structured dataclass.

    Args:
        tree (Tree): The parse tree generated by the Lark parser; MUST come from a command (`command`) tree.

    Returns:
        Command: The structured instance of the dataclass corresponding to the command.
    """
    match tree:
        case Tree(data="const_assign", children=[Token(type="IDENTIFIER", value=name), expr]):
            return ConstAssign(const=name, expr=transform_parse_expr_tree(expr))

        case Tree(data="assign", children=[Token(type="IDENTIFIER", value=name), expr]):
            return Assign(var=Var(name=name), expr=transform_parse_expr_tree(expr))

        case Tree(data="fundecl", children=[Token(type="IDENTIFIER", value=name), Tree(data="params", children=identifiers), expr]):
            tokens = cast(list[Token], identifiers)
            params = [token.value for token in tokens]

            # if there are multiple instances of the same variable
            if len(params) > len(set(params)):
                raise SyntaxError(
                    f"Multiple instances of variables found in the declaration of the function {name}")

            return FunctionDecl(name=name, params=params, body=transform_parse_expr_tree(expr))

        case Tree(data="export", children=[expr, Token(type="FILENAME", value=filename)]):
            return Export(expr=transform_parse_expr_tree(expr), filename=filename)

        case Tree(data="ifelse", children=[boolean, if_true_seq, if_false_seq]):
            return IfElse(cond=transform_parse_boolean_tree(boolean), if_true=transform_parse_commandseq_tree(if_true_seq),
                          if_false=transform_parse_commandseq_tree(if_false_seq))

        case Tree(data="ifelse", children=[boolean, if_true_seq]):
            return IfElse(cond=transform_parse_boolean_tree(boolean), if_true=transform_parse_commandseq_tree(if_true_seq),
                          if_false=cast(list[Command], []))

        case _:
            raise TypeError(
                f"Cannot parse a command of unknown type {type(tree)}")


def transform_parse_commandseq_tree(tree: Tree) -> CommandSeq:
    """
    Transforms the parse tree produced by the parser in the context of
    a sequence of commands into a structured sequence made of command dataclasses.

    Args:
        tree (Tree): The parse tree generated by the Lark parser;
                     MUST come from a command_seq (`command_seq`) tree.

    Returns:
        CommandSeq: The structured sequence of commands extracted from the parse tree.
    """
    match tree:
        case Tree(data="command_seq", children=children):
            return [transform_parse_command_tree(child) for child in children]

        case _:
            raise TypeError(
                f"Cannot parse a tree of unknown type {type(tree)}")


def parse_ast(program: str) -> CommandSeq:
    """
    Parses the input string expression into a sequence of abstract syntax trees (ASTs)
    composed of dataclasses.

    Args:
        program (str): The string expression to be parsed.

    Returns:
        CommandSeq: The resulting sequence of parsed commands as ASTs.
    """
    parse_tree = parser.parse(program)
    return transform_parse_commandseq_tree(parse_tree)


# EVALUATION & EXECUTION


def evaluate_bool(boolean: Boolean, env: Environment, state: State) -> bool:
    """
    Evaluates an abstract syntax tree (AST) in the form of `Boolean` to
    produce an actual `bool`, using the given environment and state.

    Args:
        boolean (Boolean): The abstract syntax tree of the `Boolean` to evaluate.
        env (Environment): The current environment, which maps variable names to denotable values.
        state (State): The current state, which maps locations to `Song` object (the only memorizable type).

    Returns:
        bool: The resulting boolean value after evaluation.
    """
    match boolean:
        case Equal(left=left, right=right):
            return evaluate_expr(left, env, state) == evaluate_expr(right, env, state)

        case NotEqual(left=left, right=right):
            return evaluate_expr(left, env, state) != evaluate_expr(right, env, state)

        case _:
            raise TypeError(
                f"Cannot deduce a boolean from an expression of unknown type {type(boolean)}")


def evaluate_expr(ast: Expression, env: Environment, state: State) -> Song:
    """
    Evaluates an abstract syntax tree (AST) in the form of an `Expression` to produce a `Song`,
    using the given environment and state.

    Args:
        ast (Expression): The abstract syntax tree of the `Expression` to evaluate.
        env (Environment): The current environment, which maps variable names to denotable values.
        state (State): The current state, which maps locations to `Song` object (the only memorizable type).

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
            temporary_env = env.bind(
                var.name, evaluate_expr(value, env, state))

            return evaluate_expr(expr, temporary_env, state)

        case Map(iter_var=iter_var, expr=expr, return_expr=return_expr):
            song = Song.empty()

            for step in evaluate_expr(expr, env, state):
                temporary_env = env.bind(iter_var.name, Song.empty() + step)
                song = song + evaluate_expr(return_expr, temporary_env, state)

            return song

        case Concat(left=left, right=right):
            return evaluate_expr(left, env, state) + evaluate_expr(right, env, state)

        case Transpose(song=music, value=value):
            return transpose(evaluate_expr(music, env, state), value)

        case ChangeTime(song=music, value=value):
            return change_time(evaluate_expr(music, env, state), value)

        case Repeat(song=music, times=times):
            return evaluate_expr(music, env, state).repeat(times)

        case Var(name=name):
            # if `name` is in the environment
            if env.contains_identifier(name):
                dvalue = env.get_value(name)

                # if `name` points to a Location (i.e., an integer)
                if isinstance(dvalue, int):
                    return state.access(dvalue)
                elif isinstance(dvalue, Song):
                    return dvalue
                elif isinstance(dvalue, FunctionDecl):
                    raise TypeError(f"{name} is a function, not a song")
                else:
                    raise TypeError(
                        f"Couldn't convert {name} to a proper location or type")

            raise NameError(f"Variable '{name}' is not defined")

        case FunctionApply(name=function_name, args=args):
            if env.contains_identifier(function_name):
                dvalue = env.get_value(function_name)

                if isinstance(dvalue, FunctionDecl):
                    # if the number of arguments matches the arity of the function
                    if len(args) == len(dvalue.params):
                        temporary_env = env.copy()
                        temporary_state = state.copy()

                        # Creates the temporary environment and state, and
                        # binds each argument to the corresponding variable;
                        # it's done in order to evaluate the function.
                        for arg, arg_value in zip(dvalue.params, args):
                            loc, temporary_state = temporary_state.allocate(
                                evaluate_expr(arg_value, env, state))

                            temporary_env = temporary_env.bind(arg, loc)

                        return evaluate_expr(dvalue.body, temporary_env, temporary_state)
                    else:  # arity not matched
                        raise TypeError(
                            f"{function_name} takes {len(dvalue.params)} arguments but {len(args)} were given")
                else:
                    raise TypeError(f"{function_name} is not a function")
            else:
                raise NameError(f"Function '{function_name}' is not defined")

        case IfElseExpr(cond=cond, if_true=if_true, if_false=if_false):
            if evaluate_bool(cond, env, state):
                return evaluate_expr(if_true, env, state)
            else:
                return evaluate_expr(if_false, env, state)

        case _:
            raise TypeError(
                f"Cannot evaluate an expression of unknown type {type(ast)}")


def evaluate_command(ast: Command, env: Environment, state: State) -> tuple[Environment, State]:
    """
    Evaluates an abstract syntax tree (AST) in the form of a `Command` to evaluate a command and
    produce a new environment and state.

    Args:
        ast (Expression): The abstract syntax tree of the `Command` to evaluate.
        env (Environment): The current environment, which maps variable names to denotable values.
        state (State): The current state, which maps locations to `Song` object (the only memorizable type).

    Returns:
        Environment: The new environment, obtained after executing the command.
        State: The new state, obtained after executing the command.
    """
    match ast:
        case ConstAssign(const=const, expr=expr):
            if env.contains_identifier(const):
                raise TypeError(
                    "Cannot make an existing identifier into a constant")
            else:
                # binds the song directly to the identifier
                # -- no location redirecting to a position in the state's storage
                env = env.bind(const, evaluate_expr(expr, env, state))

            return env, state

        case Assign(var=var, expr=expr):
            if env.contains_identifier(var.name):
                loc = env.get_value(var.name)

                if isinstance(loc, int):
                    # if var already exists, just update its value in the state's storage
                    state = state.update(loc, evaluate_expr(expr, env, state))
                elif isinstance(loc, Song):
                    raise TypeError(
                        "Cannot assign a new value to an existing constant")
                else:
                    raise TypeError(
                        "Cannot assign a new value to a variable that does not reference a valid location")
            else:
                loc, state = state.allocate(
                    # allocate the memory in the state's storage
                    evaluate_expr(expr, env, state))

                # bind the location to the identifier
                env = env.bind(var.name, loc)

            return env, state

        case FunctionDecl(name=function_name):
            # binds the function directly to its identifier
            env = env.bind(function_name, ast)

            return env, state

        case Export(expr=expr, filename=filename):
            try:
                # print(evaluate_expr(expr, env, state))
                export_song(evaluate_expr(expr, env, state), filename)
            except Exception as e:
                raise Exception(f"Export & conversion error: {e}")

            return env, state

        case IfElse(cond=cond, if_true=if_true, if_false=if_false):
            # copies the environment and saves the location
            # of the state so far.
            local_env = env.copy()
            old_loc = state.next_loc

            # the environment produced by the sequence of commands
            # within the if-else is discarded, but its state is maintained
            # (outside variables might have changed)
            if evaluate_bool(cond, env, state):
                for command in if_true:
                    local_env, state = evaluate_command(
                        command, local_env, state)
            else:
                for command in if_false:
                    local_env, state = evaluate_command(
                        command, local_env, state)

            # the next location to allocate is restored
            # in the state (i.e., the values of the inner
            # bound to inner variables are "ignored" and
            # can be overwritten in the future). the
            # environment is restored as well (inner variables
            # are forgotten).
            state.next_loc = old_loc
            return env, state


def evaluate_code(code: str, env: Environment, state: State, sysexit_on_error: bool = False) -> tuple[Environment, State]:
    """
    Executes the provided code and returns the new environment and the new state.
    If an error is encountered and sysexit_on_error is False,
    the environment and the state passed as arguments are returned (i.e., they
    are rolled back).

    Args:
        code (str): The code to be executed.
        env (Environment): The environment in which the code should start execution.
        state (State): The state in which the code should start execution.
        sysexit_on_error (bool): Whether to exit the program on error.
    """
    initial_env, initial_state = env, state

    try:
        ast = parse_ast(code)
    except Exception as e:
        print(f"Syntax error: {e}")

        if sysexit_on_error:
            sys.exit(1)
        else:
            return initial_env, initial_state

    try:
        for command in ast:
            env, state = evaluate_command(command, env, state)

        return env, state
    except Exception as e:
        print(f"Runtime error: {e}")

        if sysexit_on_error:
            sys.exit(1)
        else:
            return initial_env, initial_state


# MAIN ENTRY POINT


def main() -> None:
    """
    The main entry point of the program, called only if this file is executed directly through
    the Python interpreter. It handles command-line arguments and calls the musical code evaluation.

    The program can either (depending on the command-line arguments):
    - Process an input file containing MICIO code.
    - Enter a REPL (Read-Eval-Print Loop) where the user can input MICIO commands interactively.
    """
    parser = argparse.ArgumentParser(
        description="MICIO - A Musical Interpreter for Chord Interpretation and Orchestration, completely written in Python 3.12!"
    )

    parser.add_argument(
        "filename", nargs="?", help="the file containing the code to execute (defaults to a REPL)"
    )

    args = parser.parse_args()

    env = Environment.default()
    state = State.empty()

    if args.filename:
        try:
            with open(args.filename, "r") as file:
                code = file.read()
                evaluate_code(code, env, state, sysexit_on_error=True)
        except FileNotFoundError:
            print(f"Error: File '{args.filename}' not found")
            sys.exit(1)
        except PermissionError:
            print(
                f"Error: You don't have permission to read '{args.filename}'")
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
                    env, state = evaluate_code(to_eval, env, state)
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()

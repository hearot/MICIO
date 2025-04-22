from __future__ import annotations

from dataclasses import dataclass
from typing import List, Literal

import sys

from lark import Lark, Token, Tree

from pydub import AudioSegment
from pydub.generators import Sine

type NoteName = Literal["A", "B", "C", "D", "E", "F", "G"]
type NoteModifier = Literal["#", "b"]

note_frequencies = {
    "A": 440.00, "B": 493.88, "C": 261.63, "D": 293.66,
    "E": 329.63, "F": 349.23, "G": 392.00
}

semitone_ratio = 1.0594631

default_output_name = "output"


@dataclass
class Note:
    frequency: float
    time: int = 1000  # ms

    @staticmethod
    def generate(name: NoteName, octave: int, modifier: NoteModifier | None = None):
        frequency = note_frequencies[name] * (2 ** (int(octave) - 4))

        if modifier == "#":
            frequency *= semitone_ratio
        elif modifier == "b":
            frequency /= semitone_ratio

        return Note(frequency=round(frequency, 2), time=1000)

    def transposed(self, value: int) -> Note:
        return Note(frequency=round(self.frequency * (semitone_ratio ** value), 2), time=self.time)

    def change_time(self, value: float) -> Note:
        return Note(frequency=self.frequency, time=time.self * value)
    
    def set_time(self, time: float):
        self.time = int(time * 1000)


@dataclass
class Pause:
    time: int


type Harmony = List[Note]

type Song = List[Harmony | Pause]


@dataclass
class Var:
    name: str


@dataclass
class Let:
    var: Var
    value: Expression
    expr: Expression


@dataclass
class Concat:
    left: Expression
    right: Expression


@dataclass
class Transpose:
    song: Expression
    value: int

@dataclass 
class ChangeTime:
    song: Expression
    value: float

type Expression = Song | Let | Concat | Transpose | Var | ChangeTime

type Environment = dict[str, Song]

env = {}

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


def play(song: Song, output_name: str | None = None) -> None:
    global default_output_name

    if not output_name:
        output_name = default_output_name

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
                note_audio = Sine(note.frequency).to_audio_segment(duration=note.time).fade_in(15).fade_out(15)
                harmony_audio = harmony_audio.overlay(note_audio)

            combined += harmony_audio

    combined += AudioSegment.silent(duration=100)

    combined.export(output_name + ".wav", format="wav")


def transpose(song: Song, value: int) -> Song:
    transposed = []

    for step in song:
        if isinstance(step, list):
            transposed.append([note.transposed(value) for note in step])
        else:
            transposed.append(step)

    return transposed

def changetime(song: Song, value: float) -> Song:
    changed = []
    for step in song:
        if isinstance(step, list):
            changed.append([note.change_time(value) for note in step])
        else:
            changed.append(step)
    return changed

def transform_parse_tree(tree: Tree) -> Expression:
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
    parse_tree = parser.parse(expression)
    return transform_parse_tree(parse_tree)


def evaluate(ast: Expression, env: Environment) -> Song | None:
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
            return changetime(evaluate(music,env), value)
        case Var(name=name):
            if name in env.keys():
                return env[name]

            raise NameError(f"Variable '{name}' is not defined")
        case _:
            return ast


def main():
    try:
        with open(sys.argv[1], "r") as file:
            song = evaluate(parse_ast(file.read()), env)
            print(song)
            play(song, sys.argv[1].split(".")[0])
    except IndexError:
        while True:
            to_eval = input("Enter an expression (write 'exit' to quit).\n>>> ")

            if to_eval == "exit":
                break
            else:
                try:
                    song = evaluate(parse_ast(to_eval), env)
                    print(song)
                    play(song)
                except Exception as e:
                    print(e)
    except FileNotFoundError:
        print("File not found.")
        exit(1)


if __name__ == "__main__":
    main()

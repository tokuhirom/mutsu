use v6;
use Test;

plan 8;

# The EVAL redeclaration check lets `multi` token/rule candidates (and a `proto`
# with its candidates) share a name, as a file-level grammar does (#16356).

use MONKEY-SEE-NO-EVAL;

sub grammar-src($name, *@lines) {
    "grammar $name \{\n" ~ @lines.join("\n") ~ "\n}\n" ~ $name ~ ".parse('x') ?? 'parsed' !! 'no'"
}

lives-ok { EVAL grammar-src('G4', 'proto token a {*}', 'multi token a { x }', 'multi token a($n) { y }',
        'token TOP { <a> }') }, 'proto token with multi candidates';
lives-ok { EVAL grammar-src('G5', 'multi token a { x }', 'multi token a($n) { y }', 'token TOP { <a> }') },
    'multi token candidates alone';
lives-ok { EVAL grammar-src('G8', 'multi rule r { x }', 'multi rule r($n) { y }', 'token TOP { <r> }') },
    'multi rule candidates';

dies-ok { EVAL grammar-src('G1', 'token a { x }', 'token a { y }') }, 'two plain tokens collide';
dies-ok { EVAL grammar-src('G2', 'proto token a {*}', 'token a { y }') }, 'proto then plain collide';
dies-ok { EVAL grammar-src('G3', 'token a { x }', 'multi token a { y }') },
    'plain then multi collide';
dies-ok { EVAL grammar-src('G6', 'multi token a { x }', 'token a { y }') },
    'multi then plain collide';
dies-ok { EVAL grammar-src('G7', 'proto token a {*}', 'proto token a {*}') }, 'two protos collide';

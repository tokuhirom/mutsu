use v6;
use Test;

# A user class named like a 6.e builtin (`Formatter`, `Format`) owns its own
# methods. `Formatter.new` used to be answered by the builtin sprintf-format
# compiler before the user class was consulted, so `Formatter.new` returned a
# Sub and `.format-source(...)` on it returned a composed-method Sub
# (Template::HAML's `class Formatter`, #10638).

plan 7;

class Formatter {
    has $.config;
    method format-source(Str:D $src --> Str) { "formatted:$src" }
}

isa-ok Formatter.new, Formatter, 'Formatter.new builds the user class';
is Formatter.new(:config<x>).config, 'x', 'named args reach the user class';
is Formatter.new.format-source('%p'), 'formatted:%p', 'user method runs';

sub format-source(Str:D $src --> Str) { Formatter.new.format-source($src) }
is format-source('a'), 'formatted:a', 'a same-named sub delegating to the method';

class Format {
    has $.format;
    method count { 42 }
}

isa-ok Format.new(:format<%d>), Format, 'Format.new builds the user class';
is Format.new(:format<%d>).count, 42, 'user method on Format';
is Format.new(:format<%d>).raku, 'Format.new(format => "\%d")',
    'user Format instance is not rendered as the builtin';

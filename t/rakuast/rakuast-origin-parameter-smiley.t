use Test;

# A parameter type with a definiteness smiley (`Int:D $x`, `Str:U`, `Int:_`):
# the `Definedness` and its inner `Simple` span the whole written type, the
# `Name` only the name before the smiley, and the parameter's target declares
# the same type again, as rakudo's `visit-children` shows it (#15780).

plan 5;

use experimental :rakuast;

sub origins(Str $src) {
    my @l;
    sub w($n) { @l.push($n.^name.split('::').tail ~ "@" ~ .from ~ "-" ~ .to) with $n.origin; $n.visit-children(&w) }
    w($src.AST);
    @l.grep(/^[Definedness | AnyDefinedness | Simple | Name | Parameter]/).join(' ');
}

is origins('sub f(Int:D $x) { }'),
    'Name@4-5 Parameter@6-14 Definedness@6-11 Simple@6-11 Name@6-9 Simple@12-14 Definedness@6-11 Simple@6-11 Name@6-9',
    'Int:D $x';
is origins('sub f(Str:U $y) { }'),
    'Name@4-5 Parameter@6-14 Definedness@6-11 Simple@6-11 Name@6-9 Simple@12-14 Definedness@6-11 Simple@6-11 Name@6-9',
    'Str:U $y';
is origins('sub f(Int:_ $m) { }'),
    'Name@4-5 Parameter@6-14 AnyDefinedness@6-11 Simple@6-11 Name@6-9 Simple@12-14 AnyDefinedness@6-11 Simple@6-11 Name@6-9',
    'Int:_ $m';
is origins('sub f(Int $x) { }'),
    'Name@4-5 Parameter@6-12 Simple@6-9 Name@6-9 Simple@10-12 Simple@6-9 Name@6-9',
    'a type without a smiley is unchanged';
is origins('sub f(Int:D) { }').words.elems > 3, True, 'an anonymous parameter';

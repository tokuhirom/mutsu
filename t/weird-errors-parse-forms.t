use v6;
use Test;

# Pins for integration/weird-errors.t items 11/20/32/33: itemized-array
# colonpairs and semicolon argument slices, ::lowercase symbol lookup,
# indirect object notation, and the `hash` listop.

plan 10;

# --- :[...] is an itemized array; leading `;` in parens makes LoL slices ---
lives-ok { EVAL 'say(;:[])' }, 'say(;:[]) parses and lives';
is :[].gist, '[]', ':[] is an (itemized) empty array';
is :[1,2].raku, '$[1, 2]', ':[1,2] is the itemized array $[1, 2]';
{
    my $p = run $*EXECUTABLE, '-e', 'say(;:[])', :out, :err;
    is $p.out.slurp(:close).chomp, '()([])', 'say(;:[]) prints the two slices like rakudo';
}

# --- ::lowercase is a runtime symbol lookup (X::NoSuchSymbol) ---
throws-like { "::a".EVAL }, X::NoSuchSymbol, symbol => "a",
    '::a throws X::NoSuchSymbol';
{
    my class lowly {}
    is ::lowly.^name, 'lowly', '::name still resolves an existing lowercase type';
}

# --- indirect object notation: new Foo: ---
{
    my $p = run $*EXECUTABLE, '-e', 'class Foo {}; $ = new Foo:', :out, :err;
    is $p.exitcode, 0, 'new Foo: runs silently';
    is $p.out.slurp(:close) ~ $p.err.slurp(:close), '', 'new Foo: produces no output';
}
{
    my class Point { has $.x }
    my $pt = new Point: x => 7;
    is $pt.x, 7, 'new Type: args passes arguments to .new';
}

# --- hash listop ---
{
    sub f1 { hash a => 1 }
    is-deeply f1().<a>, 1, 'hash a => 1 listop form works in a sub';
}

done-testing;

use Test;

# #7988: constructs that rakudo parses but mutsu used to stop short of,
# leaving the rest of the line behind ("Confused. Two terms in a row").
# Each was found failing to load or parse an ecosystem distribution, which
# the comment names; the expected values are rakudo's.

plan 28;

# Math::GameTheory: a doubled `++`/`--` after whitespace is the infix
# followed by a prefix, not a postfix.
is-deeply ((0 xx 2) ++ [1]), 3, '`(...) ++ [...]` is infix + then prefix +';
is-deeply (1 -- 2), 3, '`1 -- 2` is infix - then prefix -';
{
    my $x = 1;
    is ($x ++ 2), 3, 'a spaced `++` after a variable is not its postfix';
}

# CSS::Properties: a trailing comma ends a pointy parameter list.
{
    my @seen;
    for 1, 2 -> $c, { @seen.push: $c }
    is-deeply @seen, [1, 2], '`-> $c, {` binds one parameter';
    my @pairs;
    for 1 .. 4 -> $a, $b, { @pairs.push: "$a$b" }
    is-deeply @pairs, ['12', '34'], '`-> $a, $b, {` binds two';
}

# P5reverse: a statement condition is a full EXPR, comma included.
{
    my $topic;
    with 1, 2, 3 { $topic = $_ }
    is-deeply $topic, (1, 2, 3), '`with 1, 2, 3 { }` topicalizes the list';
    my $taken = False;
    if 1, 0 { $taken = True }
    ok $taken, '`if 1, 0 { }` tests a non-empty list';
}

# PDF::Content, LibXML: `my enum <...>` is an anonymous enum.
{
    my enum <Copy Move>;
    is Move.value, 1, '`my enum <Copy Move>` declares its values';
    class WithEnum { my enum <lx ly>; method m { ly } }
    is WithEnum.m.value, 1, '... inside a class body too';
}

# Manifest::StopWar: `method!name` needs no space.
{
    class Priv { method!secret { 42 }; method reveal { self!secret } }
    is Priv.reveal, 42, '`method!name { }` declares a private method';
}

# Pakku, Pod::To::HTML: a sub declared later, called with a block argument.
{
    my @log;
    retry { @log.push: 'a' };
    try retry { @log.push: 'b' };
    sub retry(&code) { code() }
    is-deeply @log, ['a', 'b'], '`retry { ... }` calls a post-declared sub';
}

# Timezones::ZoneInfo: a word operator's `op=` on an rw accessor.
{
    class Day { has $.weekday is rw = 9 }
    my $tmp = Day.new;
    $tmp.weekday mod= 7;
    is $tmp.weekday, 2, '`$obj.attr mod= 7` writes back';
}

# CSS::Stylesheet, Spreadsheet::Libxlsxio: a postfix chain after `.=`
# applies to the new value and is sunk; the variable keeps the `.=` result.
{
    my DateTime $d .= new('1900-01-01T00:00:00').later: :days(3);
    is ~$d, '1900-01-01T00:00:00Z', 'chain after `my T $x .= new(...)` is sunk';
    class Cl { has $.c }
    my Cl $p .= new.clone;
    isa-ok $p, Cl, '`my T $x .= new.clone` declares and initializes';
}

# SQL::Abstract: a type-only parameter may carry a default.
{
    sub rf(Str $a, Int:U $s, Str = Str) { 'called' }
    is rf('a', Int), 'called', '`Str = Str` is an optional type-only parameter';
    sub g(Int = 5) { 'g' }
    is g(), 'g', '... callable without the argument';
}

# Compress::LZString: `with` / `without` statement modifiers after a call.
{
    my @out;
    sub output-w($x?) { @out.push: $x // 'none' }
    my $w = 3;
    output-w with $w;
    output-w without $w;
    is-deeply @out, ['none'], '`call with $x` / `call without $x` are modifiers';
}

# MCP: `last EXPR` is the routine form; v6.d rejects the argument at run time.
{
    throws-like { for ^2 { my $res = 3; last $res } }, Exception,
        message => /'Cannot resolve caller last'/,
        '`last $res` parses as a call to `last`';
}

# DB::Xoos: a class expression composing a parameterized role.
{
    role Param[$x] { method x { $x } }
    my $s = class :: does Param[{ :placeholder<$> }] { }.new;
    is-deeply $s.x, { :placeholder<$> }, '`class :: does R[...] { }.new`';
}

# Config::BINDish: `handles` takes a variable holding the method names.
{
    our @co;
    BEGIN { @co = <uc lc> }
    class Delegator { has Str $.p handles @co }
    is Delegator.new(p => 'aB').uc, 'AB', '`handles @var`';
}

# Trait::Env: a variable trait's argument is a whole list.
{
    my @got;
    multi trait_mod:<is>(Variable $v, :$env!) { @got.push: $env }
    my %h is env( :sep<:>, :kvsep<=> );
    is-deeply @got[0], (:sep<:>, :kvsep<=>), '`is env(:a, :b)` passes the list';
}

# Terminal::LineEditor: a parenthesized test call is an expression operand.
{
    nok(False) xx 2;
}

# REPL: `orwith EXPR -> $x is copy { }` keeps the chain going.
{
    my $l = 'a b';
    my $got;
    with $l.index('x') { $got = 'with' }
    orwith $l.rindex(' ') -> $s is copy { $s++; $got = "orwith $s" }
    elsif 1 { $got = 'elsif' }
    is $got, 'orwith 2', '`orwith ... -> $s is copy` then `elsif`';
}

# Grammar::Modelica: a bracketed escape inside a character class.
{
    ok 'A' ~~ /<[ \x[0000] .. \x[10FFFF] ] - [ " \\ ]>/,
        '`\x[...]` in a class does not end the bracket group';
    nok '"' ~~ /^ <[ \x[0000] .. \x[10FFFF] ] - [ " \\ ]> $/,
        '... and the subtracted `"` stays excluded';
}

# RPi::Device::ST7036: a class-level `my T $.x .= new(...)`.
{
    class Setup { has $.rows; my Setup $.DOGM .= new(rows => 1); method r { $.DOGM.rows } }
    is Setup.r, 1, '`my T $.attr .= new(...)` initializes the class attribute';
}

# SQL::Abstract: a role's `is ::Name` parent.
{
    role Failing is ::Exception { method message { 'boom' } }
    class Thrown does Failing { }
    throws-like { Thrown.new.throw }, Thrown, message => 'boom',
        '`role R is ::Exception` composes an exception class';
}

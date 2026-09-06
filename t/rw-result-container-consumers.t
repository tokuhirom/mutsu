use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# ADR-0067: every consumer of a returned container consults the same producer.
#
# ADR-0059 fixed the rule ("an `is rw` routine returns a container") and
# ADR-0067 built the inbound transport, but two producers were still missing:
#
#   * an `is rw` METHOD whose tail is a bare private attribute (`{ $!v }`)
#     handed back a copy of the attribute's value, so `:=`, an `is rw`/`is raw`
#     argument and an lvalue invocant all failed on it -- while the byte-
#     identical `{ @!l[$i] }` and `{ x }` tails already handed back locations;
#   * an attribute ACCESSOR read in ARGUMENT position, which produced a
#     container for a `:=` bind and for an lvalue invocant but not for
#     `g($c.v)` -- and whose `is raw` twin `f($c.v) = 9` silently dropped the
#     write.
#
# Byte-identical under `raku` and `mutsu`.

plan 36;

augment class Any {
    method mutsuRwArgSnitch(\S:) is raw { S }
}

class Acc {
    has $.v is rw;
    has Int $.n is rw;
    has $!priv = 7;
    has @.l is rw;
    method acc is rw { $!v }
    method typed is rw { $!n }
    method secret is rw { $!priv }
    method defaulting is rw { $!v //= 100; $!v }
    method plain { $!v }
    method value is rw { 42 }
    method aggregate is rw { @!l }
}

class Plain { has $.v = 42 }

sub rwparam($y is rw) { $y = 9 }
sub rawparam(\x) is raw { x }
sub copyparam($x is copy) { $x = 77; $x }
sub roparam($x) { $x }

# --- an `is rw` method's `$!attr` tail is a location -------------------------

{
    my $c = Acc.new(v => 42);
    $c.acc = 9;
    is $c.v, 9, 'assigning to an `is rw` method result still writes the attribute';
}

{
    my $c = Acc.new(v => 42);
    my $x := $c.acc;
    $x = 9;
    is $c.v, 9, 'a `:=` bind to an `is rw` method result aliases the attribute';
}

{
    my $c = Acc.new(v => 42);
    rwparam($c.acc);
    is $c.v, 9, 'an `is rw` parameter binds an `is rw` method result';
}

{
    my $c = Acc.new(v => 42);
    rawparam($c.acc) = 9;
    is $c.v, 9, 'an `is raw` parameter relays an `is rw` method result';
}

{
    my $c = Acc.new(v => 42);
    is $c.acc.VAR.^name, 'Scalar', 'an `is rw` method result reports a Scalar container';
}

{
    my $c = Acc.new(v => 42);
    $c.acc.mutsuRwArgSnitch = 9;
    is $c.v, 9, 'an `is rw` method result is a writable lvalue invocant';
}

{
    my $c = Acc.new(v => 42);
    is $c.acc, 42, 'an ordinary rvalue read of an `is rw` method is unchanged';
    is $c.acc.WHAT.^name, 'Int', 'and reports the contained value type';
}

{
    my $c = Acc.new(v => 42);
    my $copy = $c.acc;
    $copy = 100;
    is $c.v, 42, 'assigning a plain scalar from the result still copies';
}

{
    # A private-only attribute exposed by an `is rw` method.
    my $c = Acc.new;
    my $b := $c.secret;
    $b = 8;
    is $c.secret, 8, 'an `is rw` method over a private-only attribute binds it';
}

{
    # The body's own writes happen before the tail hands the location over.
    my $c = Acc.new;
    my $d := $c.defaulting;
    $d = 5;
    is $c.v, 5, 'a body that defaults the attribute first still hands back its location';
}

{
    my $c = Acc.new(n => 1);
    my $t := $c.typed;
    $t = 3;
    is $c.n, 3, 'a typed attribute binds through an `is rw` method';
    dies-ok { $t = "str" }, 'and the declared type still constrains writes through it';
    is $c.n, 3, 'and the refused write left the attribute alone';
}

{
    # An `@`-sigiled tail is already a shared container and keeps its own path.
    my $c = Acc.new(l => [1, 2]);
    $c.aggregate[0] = 9;
    is $c.l.join(','), '9,2', 'an aggregate `is rw` method tail is unchanged';
}

{
    my $c = Acc.new(v => 42);
    is-deeply ($c.acc =:= $c.acc), True, 'two reads name one container';
}

# --- an accessor read in argument position ----------------------------------

{
    my $c = Acc.new(v => 42);
    rwparam($c.v);
    is $c.v, 9, 'an `is rw` parameter binds an attribute accessor read';
}

{
    my $c = Acc.new(v => 42);
    rawparam($c.v) = 9;
    is $c.v, 9, 'an `is raw` parameter relays an attribute accessor read';
}

{
    my $c = Acc.new(v => 42);
    is copyparam($c.v), 77, 'an `is copy` parameter still gets its own copy';
    is $c.v, 42, 'and the attribute is untouched';
}

{
    my $c = Acc.new(v => 42);
    is roparam($c.v), 42, 'a read-only parameter still sees the value';
    is $c.v, 42, 'and the attribute is untouched';
}

{
    my $c = Acc.new(v => 42);
    is roparam($c.v).WHAT.^name, 'Int', 'a read-only parameter sees a decontainerized value';
}

{
    # Candidate selection must see through the container, not reject on it.
    my $c = Acc.new(v => 42);
    my @sig;
    my multi mmm(Int $y is rw) { $y = 5; @sig.push('Int') }
    my multi mmm(Str $y) { @sig.push('Str') }
    mmm($c.v);
    is $c.v, 5, 'a multi candidate with an `is rw` parameter is reached';
    is @sig.join(','), 'Int', 'and the typed candidate was the one selected';
}

{
    # The same, through some other routine's returned location.
    my $a = 7;
    my multi nnn(Int $y is rw) { $y = 5 }
    my multi nnn(Str $y) { 'str' }
    nnn(rawparam($a));
    is $a, 5, 'a container argument from an `is raw` routine also matches a typed multi';
}

{
    my $c = Acc.new(v => 42);
    rwparam($c.v);
    is $c.v, 9, 'the accessor container survives repeated argument binding (1)';
    rwparam($c.v);
    is $c.v, 9, 'the accessor container survives repeated argument binding (2)';
}

# --- controls: these must keep refusing -------------------------------------

{
    my $p = Plain.new;
    dies-ok { rwparam($p.v) }, 'a non-rw accessor is still refused as an `is rw` argument';
    is $p.v, 42, 'and the attribute is untouched';
}

{
    my $c = Acc.new(v => 42);
    dies-ok { rwparam($c.plain) }, 'a non-rw-capable method result is still refused';
    is $c.v, 42, 'and the attribute is untouched';
}

{
    my $c = Acc.new(v => 42);
    dies-ok { rwparam($c.value) }, 'an rw-capable method returning a value is still refused';
}

{
    dies-ok { rwparam(42) }, 'a literal is still refused as an `is rw` argument';
}

{
    my $c = Acc.new(v => 42);
    dies-ok { $c.plain = 1 }, 'assigning to a non-rw method result is still refused';
    is $c.v, 42, 'and the attribute is untouched';
}

done-testing;

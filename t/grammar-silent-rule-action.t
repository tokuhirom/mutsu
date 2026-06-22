use v6;
use Test;

plan 6;

# A silent subrule `<.foo>` is hidden from `.hash`, but its OWN action method
# must still fire (Rakudo dispatches actions at reduce time regardless of
# capture), as must its descendants' — matching `raku`.

grammar G {
    token TOP { <.foo> }
    token foo { <bar> }
    token bar { \w+ }
}
class A {
    has @.log;
    method foo($/) { @!log.push('foo:' ~ ~$/); }
    method bar($/) { @!log.push('bar:' ~ ~$/); }
}
{
    my $a = A.new;
    G.parse('hello', :actions($a));
    is $a.log, ['bar:hello', 'foo:hello'],
        "a silent subrule's own action fires (and its child's), bottom-up";
}

# The silent subrule and its children stay hidden from `.hash`.
{
    my $m = G.parse('hello');
    nok $m<foo>.defined, 'silent <.foo> is absent from .hash';
    nok $m<bar>.defined, "silent subrule's children are absent from .hash";
}

# A `( )` group containing silent subrules still fires their actions, and a
# repeated silent subrule fires its action once per match.
grammar H {
    token TOP { ( [ <.item> ]+ ) }
    token item { <name> ',' }
    token name { \w+ }
}
class HA {
    has @.names;
    method item($/) { @!names.push('item:' ~ $/<name>); }
    method name($/)  { @!names.push('name:' ~ ~$/); }
}
{
    my $a = HA.new;
    H.parse('a,b,c,', :actions($a));
    is $a.names.grep(* ~~ /^name/), ['name:a', 'name:b', 'name:c'],
        'name action fires once per repeated silent <.item>';
    is $a.names.grep(* ~~ /^item/).elems, 3,
        "repeated silent subrule's own action fires once per match";
}

# `.first` on a Blob iterates its element bytes (unlike list assignment, which
# keeps a Blob as a single element).
{
    my $buf = "AB\x[C8]".encode('latin1');
    is $buf.first(* > 127, :k), 2, '.first(:k) scans a Blob as its bytes';
}

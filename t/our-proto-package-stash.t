use v6;
use Test;

# A `proto` is the one *visible* name of a multi — its candidates are lexical.
# So `our` on the proto publishes the whole routine as a package symbol, even
# though every `multi` candidate is declared bare. mutsu held protos in a
# separate registry that the package stash never scanned, and each bare
# candidate additionally marked the name `my`-scoped, so `M::` was empty and
# `::('M::&f')` returned a Failure.

plan 8;

module WithOurProto {
    our proto sub f(|) { * }
    multi sub f(Int $x) { "int:$x" }
    multi sub f(Str $x) { "str:$x" }

    # A defaulted trailing parameter: the candidate is registered under its
    # declared arity, so a qualified call omitting it must still find it.
    our proto sub g(|) { * }
    multi sub g(Str $a, Version $v = Version) { "one:$a:{$v.^name}" }
    multi sub g(Str $a, Cool $c) { "two:$a:$c" }
}

module WithBareProto {
    proto sub h(|) { * }
    multi sub h(Int $x) { $x }
}

is WithOurProto::.keys.sort.join(' '), '&f &g', 'an our proto is a package stash member';
ok WithOurProto::{'&f'}:exists, 'the proto is reachable by stash subscript';
ok ::('WithOurProto::&f') !~~ Failure, "::('Pkg::&proto') resolves";

is WithBareProto::.keys.join(' '), '', 'a bare (non-our) proto stays lexical';
ok ::('WithBareProto::&h') ~~ Failure, "::('Pkg::&bare-proto') is a Failure";

is WithOurProto::f(7), 'int:7', 'qualified dispatch still picks the Int candidate';
is WithOurProto::g('foo', 2), 'two:foo:2', 'qualified dispatch with both args';
is WithOurProto::g('foo'), 'one:foo:Version',
   'qualified dispatch reaches a candidate whose trailing parameter is defaulted';

# vim: expandtab shiftwidth=4

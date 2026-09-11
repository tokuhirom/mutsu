use v6;
use Test;

# `method build handles<token node list at-rule> { ... }` — the `handles` trait
# needs no whitespace before its angle-word list, which is how Raku's own
# documentation and CSS::Grammar::Actions spell it. Requiring a space left the
# whole declaration unparsed, so even `build` itself was missing
# ("No such method 'build' for invocant of type ...").

plan 5;

class Helper {
    method greet($n) { "hi $n" }
    method at-rule   { 'AR' }
}

class NoSpace {
    method build handles<greet at-rule> { Helper }
}
class WithSpace {
    method build handles <greet at-rule> { Helper }
}

is NoSpace.new.build.^name, 'Helper', 'the `handles`-carrying method itself is declared';
is NoSpace.new.greet('x'), 'hi x', 'delegation reaches the returned object';
is NoSpace.new.at-rule, 'AR', 'a hyphenated delegated name works';
is WithSpace.new.greet('y'), 'hi y', 'the spaced spelling still works';

# The attribute form has always accepted the tight spelling; keep it pinned.
class Attr { has $.h handles<greet> = Helper.new }
is Attr.new.greet('z'), 'hi z', 'attribute `handles<...>` is unaffected';

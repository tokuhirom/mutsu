use v6;
use Test;

# A `my $.x` (class-level) attribute declared in a role composes onto the
# consuming class as a class-level attribute: its accessor works on the class
# *type object* (`C.x`), not only on instances. Regression pin for the
# ML::TriesWithFrequencies dist, whose `Trieish` role declares
# `my Str $.trieRootLabel = 'TRIEROOT'` and a CHECK-time constant reads it via
# `ML::TriesWithFrequencies::Trie.trieRootLabel` (the type object).

role R {
    my Str $.label = 'ROOT';
}
class C does R {}

is C.label, 'ROOT', 'my $.x role attr accessor works on the composed class type object';
is C.new.label, 'ROOT', 'and also on an instance of the composed class';

# The role type object itself already exposes it.
is R.label, 'ROOT', 'the role type object exposes its own class-level accessor';

# A class that declares `my $.x` directly keeps working (unchanged path).
class D {
    my Str $.tag = 'D-TAG';
}
is D.tag, 'D-TAG', 'my $.x declared directly on a class still works on the type object';

# A CHECK-time constant reading a composed class-level role attr (the dist shape).
role Trieish {
    my Str $.trieRootLabel = 'TRIEROOT';
}
class Trie does Trieish {}
constant $ROOT = Trie.trieRootLabel;
is $ROOT, 'TRIEROOT', 'a constant can read a composed class-level role attr at compile time';

# A plain per-instance `has $.x` in a role must NOT become class-level: calling
# its accessor on the type object still fails (it needs an instance).
role HR {
    has Str $.inst = 'I';
}
class HC does HR {}
is HC.new.inst, 'I', 'has $.x role attr works on an instance';
dies-ok { HC.inst }, 'has $.x role attr does NOT leak onto the type object';

done-testing;

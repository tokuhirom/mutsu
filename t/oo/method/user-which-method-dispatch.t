use Test;

# A user class's own `method WHICH` / `method WHY` is an ordinary method in
# rakudo (unlike the WHAT/HOW/WHERE macros), so it takes the user-method
# dispatch rather than the reflection fallback. Found in Red, whose
# `Red::AST.WHICH` builds every hash key and optimizer comparison.

plan 11;

class Leaf { has $.v; method WHICH { ValueObjAt.new: "Leaf|$!v" } }
my $l = Leaf.new(v => 1);
is $l.WHICH, 'Leaf|1', 'a user WHICH answers';
isa-ok $l.WHICH, ValueObjAt, 'its own return value';
ok Leaf.new(v => 1) === Leaf.new(v => 1), '=== reads the user WHICH';
nok $l === Leaf.new(v => 2), 'different values differ';
my %h{Any};
%h{$l} = 5;
is %h{Leaf.new(v => 1)}, 5, 'an object hash keys by the user WHICH';

class M {
    multi method WHICH(::?CLASS:D:) { ValueObjAt.new: "M|d" }
    multi method WHICH(::?CLASS:U:) { ValueObjAt.new: "M|u" }
}
is M.new.WHICH, 'M|d', 'a multi WHICH on an instance';
is M.WHICH, 'M|u', 'a multi WHICH on the type object';

class N is Leaf { }
is N.new(v => 3).WHICH, 'Leaf|3', 'an inherited user WHICH';

class Q { method WHICH { callsame() ~ "!" } }
ok Q.new.WHICH.Str.ends-with("!"), 'callsame reaches the built-in WHICH';

class P { method WHY { "doc" } }
is P.new.WHY, 'doc', 'a user WHY';

my @w = (^20).map({ Leaf.new(v => $_ % 2).WHICH });
is @w.unique.elems, 2, 'repeated calls agree';

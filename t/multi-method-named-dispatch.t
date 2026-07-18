use Test;

plan 15;

# Multi-method dispatch on required named parameters must narrow by the named
# argument's type and by required-named presence (META6's
# `multi method new(Str :$file!)` / `(IO::Path :$file!)` / `(IO::Handle
# :$file!)` / `(Str:D :$json!)` / `(*%items)` set was reported Ambiguous).
class C {
    multi method load(Str :$file!) { "str" }
    multi method load(IO::Path :$file!) { "path" }
    multi method load(IO::Handle :$file!) { "handle" }
    multi method load(Str:D :$json!) { "json" }
    multi method load(*%items) { "slurpy:" ~ %items.keys.sort.join(",") }
}
is C.load(file => "x".IO), "path", 'IO::Path named arg picks the IO::Path candidate';
is C.load(file => "x"), "str", 'Str named arg picks the Str candidate';
is C.load(json => "j"), "json", 'required named presence selects the json candidate';
is C.load(a => 1, b => 2), "slurpy:a,b", 'unknown named args fall to the named slurpy';
is C.load(), "slurpy:", 'no args fall to the named slurpy';

# The same set as multi method new (constructor position).
class D {
    multi method new(Str :$file!) { "str" }
    multi method new(IO::Path :$file!) { "path" }
    multi method new(*%items) { "slurpy" }
}
is D.new(file => "x".IO), "path", 'multi method new dispatches on named arg type';
is D.new(file => "s"), "str", 'multi method new picks Str candidate for Str';
is D.new(a => 1), "slurpy", 'multi method new falls to slurpy for unknown named';

# rakudo tie-break rules around named params:
# - identical positional-only candidates stay ambiguous
class T1 {
    multi method f(Int $a) { "a" }
    multi method f(Int $b) { "b" }
}
throws-like { T1.f(1) }, X::Multi::Ambiguous,
    'identical positional candidates are ambiguous';
# - candidates that differ only in named params resolve by declaration order
class T2 {
    multi method f(Int $a, :$x) { "x" }
    multi method f(Int $a, :$y) { "y" }
}
is T2.f(1), "x", 'named-differing tie resolves to the first declared candidate';
# - a candidate with an explicit named param beats one without, regardless of order
class T3 {
    multi method f(Int $a) { "plain" }
    multi method f(Int $a, :$x) { "named" }
}
is T3.f(1), "named", 'explicit named param out-narrows a plain candidate';
class T4 {
    multi method f(Int $a, :$x) { "named" }
    multi method f(Int $a) { "plain" }
}
is T4.f(1), "named", '... in either declaration order';
# - named types are not compared: declaration order wins
class T5 {
    multi method h(Any :$file!) { "any" }
    multi method h(Str :$file!) { "str" }
}
is T5.h(file => "s"), "any", 'named param types do not narrow (declaration order)';
# - incomparable positionals stay ambiguous
class T6 {
    multi method f(Int $a, Any $b) { "ia" }
    multi method f(Any $a, Int $b) { "ai" }
}
throws-like { T6.f(1, 2) }, X::Multi::Ambiguous,
    'incomparable positional candidates stay ambiguous';
# - a user-written named slurpy loses to a required named
class T7 {
    multi method h(*%items) { "slurpy" }
    multi method h(IO::Path :$file!) { "path" }
}
is T7.h(file => "x".IO), "path", 'required named beats *%slurpy regardless of order';

done-testing;

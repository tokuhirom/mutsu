use Test;

plan 8;

# A role's `multi method m(::?ROLE:U:)` / `(::?ROLE:D:)` pair must resolve by the
# invocant's definedness once composed into a class — the `:U`/`:D` smiley on the
# `::?ROLE` pseudo-type was previously read as a bogus type capture, skipping the
# invocant check and making the call ambiguous.

role R {
    proto method describe(|) {*}
    multi method describe(::?ROLE:U:) { 'type object' }
    multi method describe(::?ROLE:D:) { 'instance' }
}
class C does R { }

is C.describe,     'type object', '::?ROLE:U: selected on a type object';
is C.new.describe, 'instance',    '::?ROLE:D: selected on an instance';

# `::?CLASS:U:` / `:D:` on a plain class keeps working.
class D {
    proto method kind(|) {*}
    multi method kind(::?CLASS:U:) { 'undef' }
    multi method kind(::?CLASS:D:) { 'def' }
}
is D.kind,     'undef', '::?CLASS:U: on a type object';
is D.new.kind, 'def',   '::?CLASS:D: on an instance';

# A concrete-type invocant smiley (`C:U:` / `C:D:`) still dispatches too.
class E {
    proto method q(|) {*}
    multi method q(E:U:) { 'u' }
    multi method q(E:D:) { 'd' }
}
is E.q,     'u', 'E:U: on a type object';
is E.new.q, 'd', 'E:D: on an instance';

# A genuine `::T` type capture on the invocant is unaffected.
role Cap {
    method whoami(::T:) { T.^name }
}
class F does Cap { }
is F.whoami, 'F', '::T invocant capture binds the class';

# `.gist`-style role multi (the Hash::Agnostic shape) picks the right candidate.
role G {
    proto method gist(|) {*}
    multi method gist(::?ROLE:U:) { '(' ~ self.^name ~ ')' }
    multi method gist(::?ROLE:D:) { 'live' }
}
class H does G { }
is (H.gist, H.new.gist).join(','), '(H),live', 'U/D gist multi resolves both ways';

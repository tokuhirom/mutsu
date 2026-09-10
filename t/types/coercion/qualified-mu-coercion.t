use Test;

# A qualified coercion to a universal base type (`self.Mu::Str`, `self.Any::gist`)
# must reach the BUILT-IN default coercion, bypassing the receiver's own override.
# Regression: a role/class that overrides `.Str`/`.gist` and delegates to
# `self.Mu::Str` used to recurse forever (type object -> stack overflow) or be
# rejected as "not inherited" (defined instance). Hash::Agnostic relies on this.

plan 16;

# --- Type object (`:U:` multi delegating to self.Mu::...) --------------------
role R {
    proto method Str(|)  {*}
    multi method Str(::?ROLE:U:)  { self.Mu::Str  }
    multi method Str(::?ROLE:D:)  { "defined-str" }
    proto method gist(|) {*}
    multi method gist(::?ROLE:U:) { self.Mu::gist }
    multi method gist(::?ROLE:D:) { "defined-gist" }
}
class C does R { has $.x }

is C.Str,  '',     'type-object self.Mu::Str is the empty string (no infinite recursion)';
is C.gist, '(C)',  'type-object self.Mu::gist is the paren type name';

# Direct qualified coercions on a bare type object.
is C.Mu::Str,      '',    'C.Mu::Str is the empty string';
is C.Mu::gist,     '(C)', 'C.Mu::gist is (C)';
is C.Mu::raku,     'C',   'C.Mu::raku is the bare type name';
is C.Any::gist,    '(C)', 'C.Any::gist is (C)';
is C.Mu::Bool,     False, 'C.Mu::Bool is False';
is C.Mu::defined,  False, 'C.Mu::defined is False';

# A role-only class (`does R`, no `is`) still resolves Mu/Any in its MRO.
ok C.new(x => 1).defined, 'a defined instance is defined';

# --- Defined instance -------------------------------------------------------
is C.new(x => 5).Mu::gist,  'C.new(x => 5)', 'instance self.Mu::gist is the generic .new repr';
is C.new(x => 5).Any::gist, 'C.new(x => 5)', 'instance self.Any::gist is the generic .new repr';
is C.new(x => 5).Mu::raku,  'C.new(x => 5)', 'instance self.Mu::raku is the generic .new repr';
is C.new(x => 5).Mu::Bool,  True,            'instance self.Mu::Bool is True';

# The overriding `:D:` multi still wins for an unqualified call.
is C.new(x => 5).Str,  'defined-str',  'unqualified .Str still hits the user :D: multi';
is C.new(x => 5).gist, 'defined-gist', 'unqualified .gist still hits the user :D: multi';

# --- Cool is NOT a universal base: a non-Cool class must reject Cool:: -------
dies-ok { C.Cool::Str }, 'Cool:: on a non-Cool class still dies (Cool is not in the MRO)';

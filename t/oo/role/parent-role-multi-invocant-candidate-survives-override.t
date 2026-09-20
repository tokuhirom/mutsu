use Test;

# The invocant's own constraint is part of what makes a multi candidate
# distinct, so a child role that overrides only the `:D:` half of a
# `:U:`/`:D:` pair does NOT shadow the parent's `:U:` half.
#
# mutsu compared candidate signatures without the invocant when pruning
# role-parent-shadowed multis, so the parent's `:U:` candidate was deleted
# along with the `:D:` one it really did replace, and a call on the TYPE
# OBJECT died with "Cannot resolve caller ...(Foo:U: )". Found via the
# Hash::Ordered zef distribution: `Hash::Ordered` overrides only
# `multi method Str(::?ROLE:D:)`, while `Hash::Agnostic` supplies both halves,
# so `Hash::Ordered.Str` on the type object could not resolve.

plan 6;

role Parent {
    proto method describe(|) {*}
    multi method describe(::?ROLE:U:) { 'parent-U' }
    multi method describe(::?ROLE:D:) { 'parent-D' }
}
role Child does Parent {
    multi method describe(::?ROLE:D:) { 'child-D' }
}

class ViaClass does Child { }
is ViaClass.describe, 'parent-U',
  'the parent role keeps its :U: candidate when the child overrides only :D:';
is ViaClass.new.describe, 'child-D',
  'and the child role still wins for a defined invocant';

# A same-invocant override really is a shadow and must not be kept twice --
# the duplicate-candidate direction this pruning exists for.
role Base2 {
    proto method render(|) {*}
    multi method render(::?ROLE:D:) { 'base' }
}
role Derived2 does Base2 {
    multi method render(::?ROLE:D:) { 'derived' }
}
class ViaClass2 does Derived2 { }
is ViaClass2.new.render, 'derived',
  'a same-invocant override still shadows the parent candidate';
lives-ok { ViaClass2.new.render },
  'and does not become an ambiguous call from a kept duplicate';

# Non-invocant multi candidates are unaffected by the invocant comparison.
role Args {
    proto method take(|) {*}
    multi method take(Int $n) { "int-$n" }
    multi method take(Str $s) { "str-$s" }
}
class ViaClass3 does Args { }
is ViaClass3.new.take(3), 'int-3', 'an Int candidate from a role still dispatches';
is ViaClass3.new.take('x'), 'str-x', 'and so does the Str one';

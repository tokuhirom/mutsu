use Test;

# A role's `multi method f(::?CLASS:U: ...)` candidate must survive class
# composition even when the CONSUMING CLASS declares its own multi candidate
# of the same name with a DIFFERENT invocant definedness (`::?CLASS:D:`)
# (GH #8119). `resolve_class_stub_requirements`'s "a class multi wins over a
# matching role multi" dedup (`src/runtime/registration.rs`) compared
# candidates by `method_positional_signature`, which deliberately strips the
# invocant entirely (for a separate, correct reason: role-stub satisfaction
# ignores an invocant type marker). Reused unchanged for this dedup, it made
# `f(::?CLASS:U: Str:D)` and `f(::?CLASS:D: Str:D)` look identical, so the
# role's `:U:` candidate was wrongly dropped as "the class already provides
# this" the moment BOTH conditions held: the role declares a `:U:`-invocant
# candidate, and the class ALSO declares a same-named multi, of ANY
# definedness. `t/oo/role/role-ud-multi-dispatch.t` covers the sibling shape
# where both candidates live in the role itself; this covers the class
# overriding one of them.

plan 6;

role R {
    proto method f(|) { * };
    multi method f(::?CLASS:U: Str:D $s) { "U-invocant: $s" }
}
class C does R {
    multi method f(::?CLASS:D: Str:D $s) { "D-invocant: $s" }
}
is C.f('x'), 'U-invocant: x', "the role's :U: candidate survives a class :D: override";
is C.new.f('y'), 'D-invocant: y', "the class's own :D: candidate still dispatches";
is C.^find_method('f').candidates.elems, 2, 'both candidates are registered';

# The motivating real shape (TAP::SourceHandler): the role's :U: candidate
# re-dispatches through `self.new`, landing on the class's own :D: candidate.
role SourceHandler {
    proto method make-source(|) { * };
    multi method make-source(::?CLASS:U: Str:D $name) {
        self.new.make-source($name);
    }
}
class SourceHandler::File does SourceHandler {
    multi method make-source(::?CLASS:D: Str:D $name) { "opened: $name" }
}
is SourceHandler::File.make-source('foo.tap'), 'opened: foo.tap',
    'a :U: role candidate re-dispatching through self.new reaches the class :D: candidate';

# A class multi that genuinely re-declares the SAME invocant smiley as the
# role's still replaces it (no duplicate-candidate regression).
role G {
    multi method g(Int $x) { "role: $x" }
}
class D does G {
    multi method g(Int $x) { "class: $x" }
}
is D.new.g(5), 'class: 5', "a same-invocant class override still wins over the role's";
is D.^find_method('g').candidates.elems, 1, 'and does not leave a stale duplicate candidate';

use Test;

# An unsupplied optional named parameter must bind its OWN default (or type
# object), not inherit the caller's same-named named argument. The callee's env
# is an overlay over the caller's, so `outer(:section)` calling a sub whose
# `:$section` is unsupplied would leak the caller's `True` in — Template::Mustache
# rendered `<EarthFalse>` instead of `<Earth>` because a recursive `get(:section)`
# leaked `:section` into an inner `get(:encode)` and returned a `($result,$lambda)`
# tuple where a plain string was expected.

plan 5;

# Different callee sub with the same-named param.
sub inner(Bool :$section) { $section }
sub outer(Bool :$section) { inner() }
nok outer(:section), "unsupplied :section does not inherit the caller's value";

# Recursion into the SAME sub.
sub rec($v, Bool :$flag) {
    return $flag if $v == 0;
    rec($v - 1);          # recurse WITHOUT :flag
}
nok rec(2, :flag), "recursion without the named arg resets it to the default";

# A signature default still applies (not overwritten by a leaked value).
sub d(Str :$mode = 'def') { $mode }
sub call-d(Str :$mode) { d() }
is call-d(mode => 'outer'), 'def', "the callee's own default wins over a leaked arg";

# The value IS seen when actually passed.
is inner(:section).so, True, "an explicitly passed named arg is still bound";

# BUILD/TWEAK attribute defaults are unaffected (twigil'd keys, not the bare name).
class C { has $.x; submethod BUILD(:$x = 'battr') { $!x = $x } }
is C.new.x, 'battr', "a BUILD named param's default still applies";

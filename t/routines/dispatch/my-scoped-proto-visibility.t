use Test;

# The PROTO's scope decides the whole multi's visibility. A bare (`my`-scoped)
# `proto sub` keeps the multi lexical to its package however its candidates are
# declared, so the package stash has no `&name` and a qualified call answers
# "Could not find symbol". Only an `our proto` publishes it.
#
# mutsu marked the key `my`-scoped from a plain `sub`, and `our`-scoped from an
# `our proto`, but a BARE proto marked it neither — so a `module M { proto sub
# f($) {*}; our multi sub f(Int) {…} }` left the key unmarked and `M::f(1)`
# resolved. And once the resolver was gated, the `has_proto` probe still said
# yes, so the refusal came out as "Cannot resolve caller …; none of these
# signatures matches" — refusing for the wrong reason, and printing the
# signatures of a routine the caller cannot see.

plan 12;

module M1 { proto sub f($) {*}; our multi sub f(Int $x) { "f" } }
throws-like { M1::f(1) }, X::AdHoc,
  'a bare proto with `our` candidates is not in the package stash';
{
    my $err;
    { M1::f(1); CATCH { default { $err = .message } } }
    like $err, /"Could not find symbol '&f' in 'M1'"/, 'and it says so the way rakudo does';
}

module M2 { proto sub h($) {*}; multi sub h(Int $x) { "h" } }
{
    my $err;
    { M2::h(1); CATCH { default { $err = .message } } }
    like $err, /"Could not find symbol '&h' in 'M2'"/,
      'a bare proto with bare candidates too';
}

module M3 { sub j($) { "j" } }
{
    my $err;
    { M3::j(1); CATCH { default { $err = .message } } }
    like $err, /"Could not find symbol '&j' in 'M3'"/, 'control: a plain `sub` already did this';
}

# --- what IS published ------------------------------------------------------
module M4 { our proto sub g($) {*}; our multi sub g(Int $x) { "g" } }
is M4::g(1), 'g', 'an `our proto` publishes the multi';

module M5 { our proto sub p($) {*}; multi sub p(Int $x) { "p" } }
is M5::p(1), 'p', '... even when the candidates are bare';

module M6 { our sub k($) { "k" } }
is M6::k(1), 'k', 'control: an `our sub` is published';

# --- and the lexical name still works from inside its own package -----------
module M7 {
    proto sub q($) {*};
    our multi sub q(Int $x) { "q-int" }
    our multi sub q(Str $x) { "q-str" }
    our sub call-q($v) { q($v) }
}
is M7::call-q(1), 'q-int', 'the short name dispatches inside the package (Int)';
is M7::call-q("x"), 'q-str', '... and (Str)';
{
    my $err;
    { M7::q(1); CATCH { default { $err = .message } } }
    like $err, /"Could not find symbol '&q' in 'M7'"/,
      'while the qualified name stays hidden';
}

# --- a mainline proto is still callable under its short name ----------------
{
    proto sub top($) {*};
    multi sub top(Int $x) { "top-int" }
    multi sub top(Str $x) { "top-str" }
    is top(1), 'top-int', 'a mainline proto still dispatches (Int)';
    is top("x"), 'top-str', '... and (Str)';
}

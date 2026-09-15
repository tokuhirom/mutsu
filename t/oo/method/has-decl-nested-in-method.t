use Test;

plan 11;

# Rakudo lets `has` appear anywhere lexically inside a class body, including
# inside a method (or a control-flow block nested inside one) — it is a
# compile-time declarator, so the attribute is installed on the class and its
# default is evaluated at construction time regardless of where the `has`
# textually sits or whether the surrounding control flow ever runs (#8441).

# A `has` directly in a method body installs the attribute and its default
# takes effect at construction, not merely "the next time this method runs".
class A {
    method m { has $!g = 3; $!g }
}
is A.new.m, 3, 'has with a default nested directly in a method body installs and initializes the attribute';

# ... and the attribute is visible from an unrelated method even before the
# declaring method is ever called.
class B {
    method m { has $!g = 3; $!g }
    method n { $!g }
}
{
    my $b = B.new;
    is $b.n, 3, 'the attribute is already installed on a fresh instance before the declaring method ever runs';
    is $b.m, 3, 'the declaring method itself still reads the same value';
}

# A `has` with no default is likewise a legal (merely un-initialized)
# declaration, not "you cannot declare attribute here".
class C {
    method m { has $!g; 42 }
}
is C.new.m, 42, 'has with no default nested in a method body is a legal declaration';

# `has` nested inside control flow (`if`) within a method installs the
# attribute regardless of whether that branch is ever taken at runtime, and
# reaching the statement at runtime (the branch DOES run) is a no-op, not a
# re-declaration — matching rakudo's "declaration, not a statement" model.
class D {
    method m($go) {
        if $go {
            has $!g = 3;
        }
        "ok";
    }
    method n { $!g }
}
{
    my $d = D.new;
    is $d.n, 3, 'has nested inside an if inside a method installs the attribute even when that branch never runs';
    is $d.m(True), 'ok', 'has nested inside an if inside a method is a no-op when that branch does run';
    is $d.m(True), 'ok', 'and stays a no-op on a second call';
}

# The PDF::Font::Loader::Enc::CMap shape that motivated #8441: a `has` inside
# an `if` inside a method, using `//=` on a private attribute.
class E {
    has $.is-wide = False;
    has $!next-cid = 0;
    method allocate-cid {
        if $!next-cid >= 2 ** ($.is-wide ?? 16 !! 8) {
            has $!out-of-gas //= "exhausted";
        }
        my $cid = $!next-cid;
        $!next-cid++;
        $cid;
    }
}
lives-ok { my $e = E.new; $e.allocate-cid for ^3 },
    'has ... //= nested in an if inside a method parses and runs (#8441)';

# `has $!g //= EXPR` is not an initializer form (rakudo's `has` initializers
# are only `=`, `:=`, `::=`, `.=`): the declaration installs `$!g` with no
# default, and `//=` applies to the declared variable as an ordinary compound
# assignment written right after it.
#
# NOTE: this is a deliberate, documented divergence from rakudo, not a claim
# of parity. In real rakudo, a `has` declaration used as a TERM (as `//=`'s
# LHS here) evaluates to the attribute's type object — a symbolic constant,
# not a live reference to the container — so `//=`/`||=` against it always
# dies with "Cannot modify an immutable Nil value", REGARDLESS of twigil
# (confirmed against `raku` directly). Reproducing that exactly requires the
# "declarator usable in arbitrary term position" architecture #8441 itself
# calls out as a separate, large change; mutsu instead treats the declared
# variable as the real (mutable) attribute, which lets `//=` behave usefully
# — this is why the motivating construct (class E above, `$!out-of-gas //=
# ...`) is not exercised by PDF::Font::Loader's own test suite either way.
class G {
    method m { has $!g //= 5; $!g }
}
{
    my $g = G.new;
    is $g.m, 5, 'has $!g //= EXPR nested in a method assigns once';
    is $g.m, 5, 'and is idempotent after';
}

# A trailing infix that is genuinely a new (bad) statement must still be
# rejected as "two terms in a row" — #8441 only carves out the recognized
# compound-assignment operators, not arbitrary garbage.
throws-like 'my class F { has $.a syntax error; }', X::Syntax::Confused,
    'a bare term after an attribute is still Confused, not a new statement';

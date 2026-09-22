use Test;

# A type constraint's `__mutsu_type::<name>` metadata value is interned per
# CONSTRAINT SPELLING and shared by every registration of it (#8898), instead
# of being rebuilt on each typed declaration and each typed parameter bind.
# Sharing one immutable value between unrelated variables must stay invisible:
# each name keeps its own constraint, a scope exit still restores the one it
# shadowed, and the container forms still carry their own value/key types.

plan 18;

# --- two variables, one spelling -------------------------------------------

my Int $a = 1;
my Int $b = 2;
$a = 10;
is $a, 10, 'first Int lexical accepts an Int';
is $b, 2, 'the second is untouched by the first';
dies-ok { $a = "x" }, 'the first still enforces its constraint';
dies-ok { $b = "x" }, 'and so does the second';

# Clearing one name's constraint must not clear the other's.
{
    my $a = "now untyped";
    is $a, "now untyped", 'an inner untyped shadow takes a Str';
}
dies-ok { $b = "x" }, 'the shadow did not release the other name';

# --- a shadowing typed declaration inside a loop body ------------------------
# `save_type_meta_for_scope_exit` records the pre-declaration metadata once per
# scope and restores it on exit; the record is written on the first iteration
# and must be left alone by every later one.

my $t;
for 1..3 -> $i {
    my Str $t = "iteration $i";
    is $t, "iteration $i", "loop body's typed \$t holds its own value ($i)";
}
$t = 42;
is $t, 42, 'the outer untyped $t did not inherit the body constraint';

# --- typed parameters sharing a spelling ------------------------------------

sub takes-int(Int $n) { $n + 1 }
sub also-takes-int(Int $n) { $n * 2 }
is takes-int(4), 5, 'first typed parameter binds';
is also-takes-int(4), 8, 'second typed parameter binds';
# Through a variable, so the rejection happens at bind time rather than being
# refused by the compile-time signature check.
my $bad = "x";
dies-ok { takes-int($bad) }, 'a typed parameter still rejects a bad argument';

# A native-typed parameter goes down the light call path, which registers the
# same shared value.
sub native-sum(int $x, int $y --> int) { $x + $y }
is native-sum(3, 4), 7, 'native typed parameters still bind and compute';

# --- the container forms still carry their own types -------------------------

my Int @nums = 1, 2, 3;
is @nums.of.^name, 'Int', 'a typed array reports its element type';
dies-ok { @nums.push("x") }, 'a typed array still rejects a bad element';

my Int %by-name{Str};
%by-name{"one"} = 1;
is %by-name{"one"}, 1, 'an object hash with both types stores through';
dies-ok { %by-name{"two"} = "x" }, 'and still enforces its value type';

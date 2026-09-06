use Test;

# rakudo's rule for `$x = v` is sharper than "the bound thing is immutable":
# `$x` must be bound to a **Scalar** container, and no other container
# qualifies. A real `Array`, a `Hash`, a `Map` and a `Pair` all refuse the
# whole-value assignment, though every one of them is mutable through its own
# interface.
#
# mutsu accepted all of them, and for `my $x := @a` it did not merely accept the
# assignment -- it overwrote `@a` with the scalar.
#
# Only the whole-value `=` is refused. The aliasing these binds exist for
# (`$x.push`, `$x<k> = v`, `$x[0]`) is what they are for and must keep working;
# the last block below is the guard for that.

plan 13;

sub throws-ro(&c, $why) {
    my $threw = False;
    { c(); CATCH { when X::AdHoc { $threw = .Str eq 'Cannot assign to an immutable value' } } }
    ok $threw, $why;
}

throws-ro { my @a = 1, 2, 3; my $x := @a; $x = 5 },
    'a $ bound to a named @ refuses whole-value assignment';
throws-ro { my %h = a => 1; my $x := %h; $x = 5 },
    'a $ bound to a named % refuses whole-value assignment';
throws-ro { my @a := (1, 2, 3); my $x := @a; $x = 5 },
    'a $ bound to a name that is itself bound to a List refuses it';
throws-ro { my $x := [1, 2, 3]; $x = 5 },
    'a $ bound to an Array literal refuses it';
throws-ro { my $x := {a => 1}; $x = 5 },
    'a $ bound to a Hash literal refuses it';
throws-ro { my $x := Map.new((a => 1)); $x = 5 },
    'a $ bound to a Map refuses it';
throws-ro { my $x := (a => 1); $x = 5 },
    'a $ bound to a parenthesized Pair refuses it';
throws-ro { my $x := a => 1; $x = 5 },
    'a $ bound to a bare Pair refuses it';

# The `@a` row is the one that lost data rather than merely permitting a write.
{
    my @a = 1, 2, 3;
    my $x := @a;
    { $x = 5; CATCH { default { } } }
    is @a, [1, 2, 3], 'the refused assignment leaves the aliased array intact';
}

# What must keep working: these binds exist for aliasing.
{
    my @a = 1, 2, 3;
    my $x := @a;
    $x.push(9);
    is @a, [1, 2, 3, 9], 'a mutating method through the alias still writes through';
    is $x[0], 1, 'an element read through the alias still works';
}
{
    my %h = a => 1;
    my $x := %h;
    $x<b> = 2;
    is %h.keys.sort, ('a', 'b'), 'an element store through the alias still works';
}

# A `$` bound to an ordinary scalar keeps its own Scalar container.
{
    my $src = 1;
    my $x := $src;
    $x = 5;
    is $src, 5, 'a $ bound to another $ is still assignable, and writes through';
}

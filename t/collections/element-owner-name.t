use Test;

# A typed container's element constraint rides on the promoted element cell
# (ADR-0036 slice 4), so a wrong-typed write through any alias is refused. The
# CHECK was right everywhere; the MESSAGE named the container only on the
# `for`-loop path, because `array_slot_ref`/`hash_slot_ref` are `Value` methods
# that see the `ArrayData`/`HashData` but were told nothing about which variable
# it is reachable through — so they seeded the owner with the bare sigil.
#
# The container descriptor (ADR-0064) already records the declaring variable's
# name and travels with the container, so it is exactly the right source.

plan 12;

sub msg(&c) { my $m; { c(); CATCH { default { $m = .message } } }; $m // '' }

is msg({ my Int @a = 1, 2; my $r := @a[0]; $r = "s" }),
  'Type check failed for an element of @a; expected Int but got Str ("s")',
  'a `:=` alias of an array element names the array';

is msg({ my Int %h = a => 1; my $r := %h<a>; $r = "s" }),
  'Type check failed for an element of %h; expected Int but got Str ("s")',
  'and a hash element names the hash';

is msg({ my Int @a = 1, 2; my $p := (@a[0]:p); $p.value = "s" }),
  'Type check failed for an element of @a; expected Int but got Str ("s")',
  'so does the `:p` adverb`s value';

is msg({ my Int @a = 1, 2; for @a -> $v is rw { $v = "s" } }),
  'Type check failed for an element of @a; expected Int but got Str ("s")',
  'control: the `for`-loop alias already did';

# --- the name is the DECLARING variable, not the alias it came through ------
is msg({ sub f() { my Int @z = 1, 2; @z }; my @b := f(); my $r := @b[0]; $r = "s" }),
  'Type check failed for an element of @z; expected Int but got Str ("s")',
  'the declaring name wins over the binding it is read through';

is msg({ my Int @z = 1, 2; my @b := @z; my $r := @b[0]; $r = "s" }),
  'Type check failed for an element of @z; expected Int but got Str ("s")',
  '... including through a plain `:=` of the whole array';

# --- an anonymous container keeps the bare sigil ----------------------------
is msg({ my $x = (my Int @ = 1, 2); my $r := $x[0]; $r = "s" }),
  'Type check failed for an element of @; expected Int but got Str ("s")',
  'an anonymous array reports the bare sigil, as raku does';

is msg({ my $x = (my Int % = a => 1); my $r := $x<a>; $r = "s" }),
  'Type check failed for an element of %; expected Int but got Str ("s")',
  'and so does an anonymous hash';

# --- the check itself is unchanged ------------------------------------------
{
    my Int @a = 1, 2;
    my $r := @a[0];
    dies-ok { $r = "s" }, 'the wrong-typed write still dies';
    is-deeply @a.List, (1, 2), 'and does not land';
    $r = 9;
    is-deeply @a.List, (9, 2), 'a well-typed write through the alias still goes through';
}
{
    my @a = 1, 2;
    my $r := @a[0];
    $r = "s";
    is-deeply @a.List, ("s", 2), 'an UNtyped array takes any value';
}

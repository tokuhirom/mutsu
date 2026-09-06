use v6;
use Test;

# ADR-0042: a type constraint belongs to the CONTAINER, not to a name.
#
# This file pins the whole measured matrix the ADR and
# `news/2026-09/type-constraint-global-side-table-retired.md` were written
# around, in ONE place, so that the removal of the global name-keyed
# `Interpreter::var_type_constraints` side table cannot silently regress any
# of it. Three groups:
#
#   S* -- ADR-0042 §2.1's scalar scope matrix: a typed scalar declared in some
#         inner scope must not poison a same-named untyped MAINLINE scalar
#         after that scope has exited.
#   C* -- ADR-0042 §2.2's container scope matrix, same shape for `@`/`%`.
#   A* -- ADR-0042 §3's alias probe: enforcement reached through a
#         DIFFERENTLY-NAMED bound alias. A name-keyed store cannot supply this,
#         so every green row here is direct evidence the constraint is carried
#         on the container.
#   O* -- the converse of S*/C*: an OUTER typed declaration must keep enforcing
#         after an inner declaration of the same name has shadowed it. These
#         are the rows that would turn a loud refusal into a silent wrong
#         answer if the store were merely dropped instead of relocated.
#   B* -- behaviours that read the store on non-enforcement paths: the
#         Nil-to-type-object read, `state`, typed parameters, and the object
#         hash key type.
#
# Every expectation below was verified byte-for-byte against `raku`.

plan 46;

# --- §2.1 scalar scope matrix: the inner typed decl must not leak out ---

my $s01; sub s01f { my Str $s01 = "a"; }; s01f(); $s01 = 42;
is $s01, 42, 'scalar: routine-body my Str $x does not poison the mainline $x';

my $s02; { my Str $s02 = "a"; }; $s02 = 42;
is $s02, 42, 'scalar: bare-block my Str $x does not poison the mainline $x';

my $s03; { my Str $s03 = "a" if True; }; $s03 = 42;
is $s03, 42, 'scalar: statement-modifier if in a block does not poison the mainline $x';

my $s04; if True { my Str $s04 = "a"; }; $s04 = 42;
is $s04, 42, 'scalar: if-branch my Str $x does not poison the mainline $x';

my $s05; unless False { my Str $s05 = "a"; }; $s05 = 42;
is $s05, 42, 'scalar: unless-branch my Str $x does not poison the mainline $x';

my $s06; if False { } else { my Str $s06 = "a"; }; $s06 = 42;
is $s06, 42, 'scalar: else-branch my Str $x does not poison the mainline $x';

my $s07; my $s07i = 0; while $s07i++ < 2 { my Str $s07 = "a"; }; $s07 = 42;
is $s07, 42, 'scalar: while-body my Str $x does not poison the mainline $x';

my $s08; loop (my $s08i = 0; $s08i < 2; $s08i++) { my Str $s08 = "a"; }; $s08 = 42;
is $s08, 42, 'scalar: C-style loop-body my Str $x does not poison the mainline $x';

my $s09; my $s09i = 0; repeat { my Str $s09 = "a"; } while $s09i++ < 1; $s09 = 42;
is $s09, 42, 'scalar: repeat-body my Str $x does not poison the mainline $x';

my $s10; for 1..2 { my Str $s10 = "a"; }; $s10 = 42;
is $s10, 42, 'scalar: for-body my Str $x does not poison the mainline $x';

# --- §2.2 container scope matrix ---

my @c01; sub c01f { my Int @c01; }; c01f(); @c01.push("s");
is @c01[0], "s", 'container: routine-body my Int @a does not poison the mainline @a';

my @c02; { my Int @c02; }; @c02.push("s");
is @c02[0], "s", 'container: bare-block my Int @a does not poison the mainline @a';

my @c03; if True { my Int @c03; }; @c03.push("s");
is @c03[0], "s", 'container: if-branch my Int @a does not poison the mainline @a';

my @c04; my $c04i = 0; while $c04i++ < 2 { my Int @c04; }; @c04.push("s");
is @c04[0], "s", 'container: while-body my Int @a does not poison the mainline @a';

my @c05; for 1..2 { my Int @c05; }; @c05.push("s");
is @c05[0], "s", 'container: for-body my Int @a does not poison the mainline @a';

my %c06; { my Int %c06; }; %c06<k> = "s";
is %c06<k>, "s", 'container: bare-block my Int %h does not poison the mainline %h';

my %c07; { my %c07{Int}; }; %c07<k> = 1;
is %c07<k>, 1, 'container: bare-block my %h{Int} does not poison the mainline %h';

# --- §3 alias probe: enforcement reached through a different NAME ---

my Int @a01; my @x01 := @a01;
dies-ok { @x01.push("s") }, 'alias: fresh my Int @a enforces through a bound alias';

my Int @a02 = 1, 2; my @x02 := @a02;
dies-ok { @x02.push("s") }, 'alias: my Int @a with an initializer enforces through a bound alias';

my Int @a03; @a03 = 1, 2; my @x03 := @a03;
dies-ok { @x03.push("s") }, 'alias: my Int @a after a whole-array assign enforces through a bound alias';

my Int @a04; @a04.push(1); my @x04 := @a04;
dies-ok { @x04.push("s") }, 'alias: my Int @a after a push enforces through a bound alias';

my Int %h05; my %x05 := %h05;
dies-ok { %x05<k> = "s" }, 'alias: my Int %h enforces its value type through a bound alias';

my %h06{Int}; my %x06 := %h06;
dies-ok { %x06<bad> = 1 }, 'alias: my %h{Int} enforces its key type through a bound alias';

my Int @s07[3]; my @x07 := @s07;
dies-ok { @x07[0] = "s" }, 'alias: shaped my Int @a[3] enforces through a bound alias';

sub a08f { state Int @a08; my @x08 := @a08; @x08.push("s"); }
dies-ok { a08f() }, 'alias: state Int @a enforces through a bound alias';

my Str $sc09; my $al09 := $sc09;
dies-ok { $al09 = 42 }, 'alias: my Str $s enforces through a bound scalar alias';

my Str $sc10 = "a"; my \sl10 := $sc10;
dies-ok { sl10 = 42 }, 'alias: my Str $s enforces through a sigilless alias';

# --- the alias must still WORK for a conforming value ---

my Str $sc11; my $al11 := $sc11; $al11 = "fine";
is $sc11, "fine", 'alias: a conforming assignment through a scalar alias reaches the original';

my Int @a12; my @x12 := @a12; @x12.push(3);
is @a12[0], 3, 'alias: a conforming push through an array alias reaches the original';

# --- O*: an outer typed declaration keeps enforcing after being shadowed ---

my Int @o01; { my @o01; };
dies-ok { @o01.push("s") }, 'outer my Int @a still enforces after an untyped block shadow';

my Int %o02; { my %o02; };
dies-ok { %o02<k> = "s" }, 'outer my Int %h still enforces after an untyped block shadow';

my Str $o03 = "a"; if True { my Int $o03 = 1; };
dies-ok { $o03 = 42 }, 'outer my Str $x still enforces after a typed if-branch shadow';

my Str $o04 = "a"; my $o04i = 0; while $o04i++ < 2 { my $o04 = 1; };
dies-ok { $o04 = 42 }, 'outer my Str $x still enforces after an untyped while-body shadow';

my Str $o05 = "a"; for 1..2 { my Int $o05 = 1; };
dies-ok { $o05 = 42 }, 'outer my Str $x still enforces after a typed for-body shadow';

my Int @o06; loop (my $o06i = 0; $o06i < 2; $o06i++) { my @o06; };
dies-ok { @o06.push("s") }, 'outer my Int @a still enforces after an untyped loop-body shadow';

my Str $o07 = "a"; sub o07f { my $o07 = 1; }; o07f();
dies-ok { $o07 = 42 }, 'outer my Str $x still enforces after an untyped routine-local shadow';

# --- B*: the non-enforcement readers of the store ---

my Int $b01;
is $b01.^name, 'Int', 'an unassigned typed scalar reads back as its type object';
is $b01.defined, False, 'an unassigned typed scalar is undefined';

my Int $b02 = 5; $b02 = Nil;
is $b02.^name, 'Int', 'assigning Nil to a typed scalar resets it to the type object';

my Str $b03 = "a"; my $b03r := $b03; $b03 = "b";
is $b03r, "b", 'a scalar alias tracks later assignments to the original';

sub b04f { state Str $c = "a"; $c = 42; }
dies-ok { b04f() }, 'state Str $c enforces its constraint';

sub b05f(Str $p) { $p }
my $b05arg = 42;
dies-ok { b05f($b05arg) }, 'a typed parameter enforces its constraint';

sub b06f { my Int @r; @r.push(1); @r }
my @b06 = b06f(); @b06.push("s");
is @b06[1], "s", 'a routine-local my Int @r does not poison the callers untyped @r';

sub b07f { my %h{Int}; %h{1} = "a"; %h{"bad"} = "b"; }
dies-ok { b07f() }, 'an object hash declared inside a routine keeps its key type';

my Int $b08 = 1;
my %b08 = a => "s";
my @b08out;
for %b08.kv -> $k, $v { @b08out.push("$k=$v") }
is @b08out.join(","), "a=s", 'a multi-param for loop is not type-checked against an unrelated my Int $v';

for (1, 2) -> Int $b09 { }
my $b09 = "s";
is $b09, "s", 'a typed for-loop parameter does not poison a later same-named lexical';

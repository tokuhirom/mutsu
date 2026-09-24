use Test;

# A closure capture no longer walks a wide env tier's plain user lexicals
# (lowercase `my` names): the tier's capture-candidate memo leaves them out, and
# the capture looks up the closure's own free variables by name instead
# (`env_tier::capture_walk_skips`, `CompiledCode::capture_probe_keys`, #9170).
# `gather` bodies are captured the same way. These pin that every lexical a body
# names still reaches it, through the memoized path.
#
# The padding makes the file-scope tier wide enough (32+ keys) for the memo to be
# built at all; the loops make sure the memo is actually READ (it is built on the
# second capture from a tier, never the first).

plan 16;

my $pad01 = 1; my $pad02 = 2; my $pad03 = 3; my $pad04 = 4; my $pad05 = 5;
my $pad06 = 6; my $pad07 = 7; my $pad08 = 8; my $pad09 = 9; my $pad10 = 10;
my $pad11 = 11; my $pad12 = 12; my $pad13 = 13; my $pad14 = 14; my $pad15 = 15;
my $pad16 = 16; my $pad17 = 17; my $pad18 = 18; my $pad19 = 19; my $pad20 = 20;
my $pad21 = 21; my $pad22 = 22; my $pad23 = 23; my $pad24 = 24; my $pad25 = 25;
my $pad26 = 26; my $pad27 = 27; my $pad28 = 28; my $pad29 = 29; my $pad30 = 30;
my $pad31 = 31; my $pad32 = 32; my $pad33 = 33; my $pad34 = 34; my $pad35 = 35;
my @list = 1, 2, 3;
my %map = a => 1, b => 2;

# --- Repeated creation reads the memo; each closure keeps its free variables ---
my @made;
for ^5 -> $i {
    my $bump = $i * 10;
    @made.push: -> { $pad07 + $bump };
}
is @made.map({ .() }).join(','), '7,17,27,37,47',
    'closures created in a loop capture a wide-scope lexical and their own';

my @seen;
for ^3 { @seen.push: { $pad33 }() }
is @seen.join(','), '33,33,33', 'a plain lexical is found on every capture';

is { @list.elems + %map<b> }(), 5, 'plain @ and % lexicals are captured';

# --- From inside a routine: a scoped tier chained over the wide one -----------
sub reader() { -> { $pad21 } }
is (^4).map({ reader()() }).join(','), '21,21,21,21',
    'a closure created in a sub reads a file-scope lexical through the chain';

sub counter() { my $n = 0; -> { ++$n } }
my &c1 = counter();
my &c2 = counter();
c1(); c1();
is c1(), 3, 'a routine-local captured and mutated keeps its own cell';
is c2(), 1, 'and a sibling closure keeps a separate one';

# --- Value writes after the memo was built are seen ---------------------------
$pad07 = 700;
is { $pad07 }(), 700, 'a later value write to a wide-scope lexical is captured';

# --- A lexical declared after the memo was built ------------------------------
my $late = 'late';
for ^2 { }
is (^3).map({ { $late }() }).join(','), 'late,late,late',
    'a lowercase lexical declared after the memo is still captured';

# --- Typed plain lexical: the type metadata travels with it -------------------
my Int $count = 3;
is (^2).map({ { $count }() }).join(','), '3,3', 'a typed lowercase lexical is captured';
dies-ok { my $w = { $count = 'str' }; $w() },
    'and its type constraint is captured with it';

# --- Shadowing ----------------------------------------------------------------
{
    my $pad05 = 'inner';
    is (^2).map({ { $pad05 }() }).join(','), 'inner,inner',
        'an inner block shadow wins over the wide-scope binding';
}
is { $pad05 }(), 5, 'and the outer binding is untouched outside the block';

# --- gather captures the same way ----------------------------------------------
my @g;
for ^3 -> $k { @g.push: (gather { take $pad10 + $k }).list.head }
is @g.join(','), '10,11,12', 'a gather in a loop reads a wide-scope lexical and its own';

my $later = 1;
my $seq = gather { take $later };
$later = 2;
is $seq.list.head, 2, 'a lazily pulled gather sees a write made after it was created';

my $acc = 0;
my @forced = gather { $acc += 5; take $acc };
is @forced.head, 5, 'a gather body can write a wide-scope lexical';
is $acc, 5, 'and the write reaches the enclosing scope';

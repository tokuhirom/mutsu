use Test;
plan 30;

# --- Junction construction ---
my $a = any(1, 2, 3);
is $a.WHAT, "(Junction)", "any() returns a Junction";
is $a.elems.WHAT, "(Junction)", "any() junction .elems auto-threads to Junction";

my $b = all(1, 2, 3);
is $b.WHAT, "(Junction)", "all() returns a Junction";

my $c = one(1, 2, 3);
is $c.WHAT, "(Junction)", "one() returns a Junction";

my $d = none(1, 2, 3);
is $d.WHAT, "(Junction)", "none() returns a Junction";

# --- .gist / .Str ---
is $a.gist, "any(1, 2, 3)", ".gist shows junction contents";
is $a.Str, "any(1, 2, 3)", ".Str shows junction contents";

# --- .Bool ---
ok any(0, 1, 2).Bool, "any(0,1,2).Bool is True (at least one truthy)";
ok all(1, 2, 3).Bool, "all(1,2,3).Bool is True (all truthy)";
nok all(0, 1, 2).Bool, "all(0,1,2).Bool is False (not all truthy)";
ok one(0, 0, 1).Bool, "one(0,0,1).Bool is True (exactly one truthy)";
nok one(1, 1, 0).Bool, "one(1,1,0).Bool is False (two truthy)";
ok none(0, 0, 0).Bool, "none(0,0,0).Bool is True (none truthy)";
nok none(0, 1, 0).Bool, "none(0,1,0).Bool is False (one truthy)";

# --- Comparison auto-threading ---
ok any(1, 2, 3) == 2, "any(1,2,3) == 2 is True";
nok any(1, 2, 3) == 5, "any(1,2,3) == 5 is False";
ok all(2, 2, 2) == 2, "all(2,2,2) == 2 is True";
nok all(1, 2, 2) == 2, "all(1,2,2) == 2 is False";

ok any(1, 2, 3) != 4, "any(1,2,3) != 4 is True";
ok any(1, 2, 3) > 2, "any(1,2,3) > 2 is True";
nok all(1, 2, 3) > 2, "all(1,2,3) > 2 is False (1 is not > 2)";

# --- String comparison auto-threading ---
ok any("a", "b", "c") eq "b", 'any("a","b","c") eq "b" is True';
nok any("a", "b", "c") eq "z", 'any("a","b","c") eq "z" is False';

# --- Smartmatch ---
ok 2 ~~ any(1, 2, 3), "2 ~~ any(1,2,3) is True";
nok 5 ~~ any(1, 2, 3), "5 ~~ any(1,2,3) is False";

# --- Arithmetic auto-threading ---
is (1 + any(2, 3)).WHAT, "(Junction)", "junction arithmetic keeps junction type";
ok 1 + any(2, 3) == 4, "addition threads over RHS junction";
ok 6 % any(2, 3) == 0, "modulo threads over RHS junction";
ok all(4, 6) % 2 == 0, "modulo threads over LHS junction";

# --- if / boolean collapse ---
my $result = "";
if any(1, 2, 3) == 2 {
    $result = "matched";
}
is $result, "matched", "if with junction collapses to bool";

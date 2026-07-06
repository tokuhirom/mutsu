use Test;

# Top-level BEGIN phasers run at compile time, so a read that appears textually
# *before* the BEGIN in source order observes its side effects. mutsu otherwise
# compiled BEGIN inline and ran it in source position. These declarations must
# be at the file's top level (the mainline) — that is the scope this handles.

plan 11;

# read-before-BEGIN: array
my @arr;
my $arr-count = @arr.elems;
BEGIN { @arr = 1, 2, 3 }
is $arr-count, 3, 'array populated by later BEGIN is visible to earlier read';

# read-before-BEGIN: grouped declaration `my (@g1, @g2)`
my (@g1, @g2);
my $g1-count = @g1.elems;
BEGIN { @g1 = 1, 2, 3, 4 }
is $g1-count, 4, 'grouped array decl seeded by BEGIN';
is @g2.elems, 0, 'untouched grouped var stays empty';

# read-before-BEGIN: scalar
my $sca;
my $sca-seen = $sca;
BEGIN { $sca = 42 }
is $sca-seen, 42, 'scalar populated by later BEGIN visible to earlier read';

# read-before-BEGIN: hash
my %hsh;
my $hsh-count = %hsh.elems;
BEGIN { %hsh = (a => 1, b => 2) }
is $hsh-count, 2, 'hash populated by later BEGIN visible to earlier read';

# read-after-BEGIN still works (was already correct)
my @after;
BEGIN { @after = 5, 6, 7 }
is @after.elems, 3, 'read after BEGIN still works';

# a real runtime initializer runs at runtime and overrides the BEGIN value
my @init = 9, 9, 9;
BEGIN { @init = 1, 2, 3 }
is @init.join(','), '9,9,9', 'runtime initializer overrides compile-time BEGIN';

# multiple BEGINs all run (at compile time) before an earlier runtime read
my ($p, $q);
my $before = "$p|$q";
BEGIN { $p = 'P' }
BEGIN { $q = 'Q' }
is $before, 'P|Q', 'multiple BEGINs all run before an earlier read';

# a BEGIN that references a class declared earlier must still see it: such a
# BEGIN is left in place (barewords are not pre-executed), and the class is
# registered before it runs.
class C { method greet { 'hi' } }
my $cv;
BEGIN { $cv = C }
is $cv.^name, 'C', 'BEGIN referencing an earlier class captures the class';
is $cv.new.greet, 'hi', 'the captured class is usable';

# nested-block BEGIN reading-after still works (not hoisted, runs in place)
my $nested;
{
    my @na;
    BEGIN { @na = 1, 2 }
    $nested = @na.elems;
}
is $nested, 2, 'nested-block BEGIN read-after works';

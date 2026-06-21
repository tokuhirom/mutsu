use Test;

# A `$`-scalar variable holding a real array, passed by reference to a sub and
# mutated via `.push`/`.append`/`.unshift` *inside a `for`/`when` block body*,
# must write through the shared array so the caller's binding observes it.
#
# Regression: entering a `for`/`when` body flushes a shared clone of the by-ref
# param into env; the slow-path push used `Arc::make_mut` on that env copy, which
# detached a private copy and lost the write for the caller (and any other alias).
# Element assignment (`$t[0] = ...`) already mutated the shared interior in place,
# so only `.push`-family methods were affected.  (Template::Mustache's `push-to`
# helper hit this: `sub push-to ($t, $_) { when .defined { $t.push: $_ } }`.)

plan 16;

# --- baseline: by-ref push without a block already worked ---
sub push-plain($t, $x) { $t.push: $x }
my $a = [5];
push-plain($a, 9);
is-deeply $a, [5, 9], 'by-ref push in a plain sub body';

# --- the regression: push inside a for-body ---
sub push-for($t, $x) { for 1 { $t.push: $x } }
my $b = [5];
push-for($b, 9);
is-deeply $b, [5, 9], 'by-ref push inside a for-block body';

# --- push inside a when-body (statement-level when, topic = the param) ---
sub push-when($t, $_) { when .defined { $t.push: $_ } }
my $c = [];
push-when($c, 'one');
push-when($c, Any);     # undefined -> no branch -> no push
push-when($c, 'two');
is-deeply $c, ['one', 'two'], 'by-ref push inside a when-block body, twice';

# --- append / unshift inside a for-body ---
sub append-for($t, @xs) { for 1 { $t.append: @xs } }
my $d = [1];
append-for($d, [2, 3]);
is-deeply $d, [1, 2, 3], 'by-ref append inside a for-block body';

sub unshift-for($t, $x) { for 1 { $t.unshift: $x } }
my $e = [2];
unshift-for($e, 1);
is-deeply $e, [1, 2], 'by-ref unshift inside a for-block body';

# --- element-assign and push interleaved in the same for-body both survive ---
sub mix($t) { for 1 { $t[1] = 7; $t.push: 99 } }
my $f = [5];
mix($f);
is-deeply $f, [5, 7, 99], 'element-assign and push in one for-body both persist';

# --- given/when also propagates ---
sub push-given($t, $x) { given $x { when .defined { $t.push: $_ } } }
my $g = [1];
push-given($g, 2);
is-deeply $g, [1, 2], 'by-ref push inside a given/when body';

# --- nested block (for inside if) ---
sub push-nested($t, $x) { if $x { for 1 { $t.push: $x } } }
my $h = [1];
push-nested($h, 2);
is-deeply $h, [1, 2], 'by-ref push inside a for nested in an if';

# --- multiple pushes accumulate across separate for-body sub calls ---
sub acc($t, $x) { for 1 { $t.push: $x } }
my $i = [];
acc($i, 'a');
acc($i, 'b');
acc($i, 'c');
is-deeply $i, ['a', 'b', 'c'], 'repeated by-ref push-in-for accumulates';

# --- a `$`-scalar array directly captured by a for-body at top level ---
my $j = [];
for 1, 2, 3 { $j.push: $_ }
is-deeply $j, [1, 2, 3], 'top-level for-body push on a $-scalar array';

# --- the `when`-fallthrough helper shape from Template::Mustache ---
my $froms = [];
sub push-to ($target, $_) {
    when Positional { $target.push: |$_ }
    when .defined   { $target.push:  $_ }
}
push-to $froms, Any;          # no branch
push-to $froms, 'views';      # .defined branch
push-to $froms, ['a', 'b'];   # Positional branch -> flattened push
is-deeply $froms, ['views', 'a', 'b'], 'Template::Mustache push-to helper shape';

# --- @-array param still works (unaffected by the fix) ---
sub push-array-for(@t, $x) { for 1 { @t.push: $x } }
my @k;
push-array-for(@k, 'v');
is-deeply @k, ['v'], '@-array by-ref push inside a for-body (unchanged)';

# --- value semantics: a fresh local array is NOT shared with the caller ---
sub copies($t) { my @local = $t; for 1 { @local.push: 99 }; @local }
my $m = [1];
my @res = copies($m);
is-deeply $m, [1], 'pushing a copied-out local does not mutate the caller array';

# --- push on a non-shared scalar array (single owner) still works ---
my $n = [1];
for 1 { $n.push: 2 }
is-deeply $n, [1, 2], 'push on a singly-owned scalar array in a for-body';

# --- pop / shift inside a for-body also mutate the shared array ---
sub pop-for($t) { for 1 { $t.pop } }
my $o = [1, 2, 3];
pop-for($o);
is-deeply $o, [1, 2], 'by-ref pop inside a for-block body';

sub shift-for($t) { for 1 { $t.shift } }
my $p = [1, 2, 3];
shift-for($p);
is-deeply $p, [2, 3], 'by-ref shift inside a for-block body';

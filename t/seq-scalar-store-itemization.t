use v6;
use Test;

# A `Seq` stored into a `$` scalar container is itemized, exactly as an
# Array/List/Hash/Range already was:
#
#     my $s = (1, 2).Seq;  $s.raku   # $((1, 2).Seq)
#
# Rakudo's `Seq.raku` checks `nqp::iscont(SELF)`, so the `$(...)` wrapper is a
# property of the *container*, not of the Seq. mutsu's scalar-store
# itemization (`itemize_scalar_store_value`) handled the `.cache` List-view
# handle and every Range shape but fell through for a real `Seq`, so the
# container was invisible. Every expectation below was measured against
# rakudo 2026.07.

plan 26;

# --- the ticket's repro ------------------------------------------------
my $s = (1, 2).Seq;
is $s.raku, '$((1, 2).Seq)', 'a Seq assigned to a `my $` renders itemized';

# The Seq-producing expressions that surfaced this.
my $z = zip((1, 2), (3, 4));
is $z.raku, '$(((1, 3), (2, 4)).Seq)', 'a zip() result assigned to a `$` is itemized';

my $m = (1, 2 Z <a b>);
is $m.raku, '$(((1, "a"), (2, "b")).Seq)', 'a Z meta-op result assigned to a `$` is itemized';

# --- the other list shapes must not have moved -------------------------
my $l = (1, 2);
is $l.raku, '$(1, 2)', 'a List assigned to a `$` is still itemized';
my $a = [1, 2];
is $a.raku, '$[1, 2]', 'an Array assigned to a `$` is still itemized';
my $h = { a => 1 };
is $h.raku, '${:a(1)}', 'a Hash assigned to a `$` is still itemized';

# --- the itemization is the container's, not the value's ---------------
my $b := (3, 4).Seq;
is $b.raku, '(3, 4).Seq', 'a `:=` bind does NOT itemize';
is $b.VAR.^name, 'Seq', 'a `:=`-bound Seq has no Scalar container';

my $c;
$c = (5, 6).Seq;
is $c.raku, '$((5, 6).Seq)', 'a plain assignment to an existing scalar itemizes';

sub returns-seq() { (7, 8).Seq }
my $d = returns-seq();
is $d.raku, '$((7, 8).Seq)', 'a Seq returned from a sub is itemized by the assignment';

sub takes-one($x) { $x.raku }
is takes-one((1, 2).Seq), '$((1, 2).Seq)', 'a Seq bound to a `$` parameter is itemized';

my @seen;
for ((1, 2).Seq,) -> $y { @seen.push: $y.raku }
is @seen[0], '$((1, 2).Seq)', 'a Seq bound to a `$` loop variable is itemized';

# --- the Seq is still a Seq, and still one item ------------------------
my $t = (1, 2).Seq;
is $t.^name, 'Seq', 'the itemized value still reports Seq';
is $t.VAR.^name, 'Scalar', 'the container is a Scalar';
is $t.WHAT.^name, 'Seq', '.WHAT is still Seq';
ok $t ~~ Seq, 'it still smartmatches Seq';

my $u = (1, 2).Seq;
is $u.elems, 2, '.elems still sees through the container';

my $v = (1, 2).Seq;
my @flat = $v, 3;
is @flat.elems, 2, 'an itemized Seq is ONE element in list context';

# --- itemizing must not force a lazy source ----------------------------
my $lazy = lazy gather { take 1; take 2 };
ok $lazy.is-lazy, 'a lazy gather assigned to a `$` stays lazy';

# `$q;` in sink context must not run the gather (the sink exemption rides on
# the shared SeqBody, so the Scalar wrapper must not hide it).
my $q = gather { die "must not run" };
lives-ok { EVAL 'my $qq = gather { die "must not run" }; $qq;' },
    'sinking an itemized Seq does not force it';
ok $q.defined, 'the un-forced Seq is still there';

# --- the itemization must NOT hide the Seq from the single-argument rule
# rakudo flattens a `$`-held Seq into a `+@` slurpy but not a `$`-held
# List/Array, so the container has to be recorded on the Seq handle rather
# than as a wrapper around it.
sub slurpy(+@v) { @v.elems }
my $ss = (1, 2, 3).Seq;
is slurpy($ss), 3, 'a `$`-held Seq still flattens under the single-argument rule';
my $sl = (1, 2, 3);
is slurpy($sl), 1, 'a `$`-held List still does NOT flatten';
my $sa = [1, 2, 3];
is slurpy($sa), 1, 'a `$`-held Array still does NOT flatten';
my $sm = (1, 2, 3).Seq;
is (map { $_ + 1 }, $sm).join(','), '2,3,4', 'map over a `$`-held Seq flattens';

# --- .raku round-trips -------------------------------------------------
my $r = (1, 2).Seq;
is $r.raku.EVAL.raku, '$((1, 2).Seq)', '.raku round-trips through EVAL';

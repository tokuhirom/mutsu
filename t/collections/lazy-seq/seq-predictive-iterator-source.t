use Test;

# `Seq.new($iterator)` where the iterator `does PredictiveIterator`.
#
# mutsu used to answer an EMPTY Seq for this and keep the iterator only in an
# out-of-band table, so that `.tail`/`.Numeric` could take the `count-only`
# shortcut without draining. The cost was that the Seq had no contents at all:
# `.list` was `()` where the identical class doing plain `Iterator` produced its
# elements. A predictive Seq is now an ordinary deferred Seq that ALSO carries
# the shortcut. Found via String::Utils's `ngram`, whose NGrams iterator does
# the predictive role, so `ngram("foobar", 3)` returned nothing.

plan 6;

my class Plain does Iterator {
    has int $!n;
    method new($n) { my $s = self.CREATE; $s.set($n); $s }
    method set($n) { $!n = $n }
    method pull-one() { $!n-- ?? "x$!n" !! IterationEnd }
}

my class Predictive does PredictiveIterator {
    has int $!n;
    method new($n) { my $s = self.CREATE; $s.set($n); $s }
    method set($n) { $!n = $n }
    method pull-one() { $!n-- ?? "y$!n" !! IterationEnd }
    method count-only(--> Int:D) { $!n }
}

is-deeply Seq.new(Plain.new(3)).list.List, ("x2", "x1", "x0"),
  'a plain Iterator drives a Seq';
is-deeply Seq.new(Predictive.new(3)).list.List, ("y2", "y1", "y0"),
  'a PredictiveIterator drives a Seq the same way';

is Seq.new(Predictive.new(3)).elems, 3, 'its elems is the element count';
is Seq.new(Predictive.new(3)).iterator.count-only, 3,
  'the count-only shortcut still reaches the iterator';

# Consumption must still be single-use, and pulling must not be duplicated by
# the shortcut being present.
my $seq = Seq.new(Predictive.new(2));
is-deeply $seq.list.List, ("y1", "y0"), 'consumed once';
is-deeply Seq.new(Predictive.new(0)).list.List, (),
  'an immediately-exhausted predictive iterator gives an empty Seq';

# vim: expandtab shiftwidth=4

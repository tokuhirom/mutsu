use v6;
use Test;

# A bare block that mutates a captured `Buf` (`.push` and friends change the
# Buf in place) and then calls a sub that REASSIGNS the same captured lexical
# must leave the reassignment visible to every other holder of the lexical.
#
# mutsu keeps such a lexical in a shared cell. The Buf mutators used to re-seat
# the receiver binding with a plain `env.insert`, which replaced that cell with
# a bare value in the running block's own overlay; the nested sub's later write
# went to the cell, and the block's exit rejoin then stored the stale overlay
# Buf back over it (Terminal::ANSIParser's `finish-sequence`). The mutators now
# write through the cell, the way `$seq = ...` does. This pins Raku's answer,
# not a mutsu-specific guarantee.

plan 27;

sub mk() {
    my $seq = buf8.new;
    my sub bump()  { $seq.push(1) }
    my sub reset() { $seq = buf8.new }
    my sub elems() { $seq.elems }
    my &a := { bump(); reset() };
    my &b := { $seq.push(1); reset() };
    my &c := { reset() };
    my &d := { bump(); $seq = buf8.new };
    (&bump, &elems, &a, &b, &c, &d);
}

my ($bump, $elems, $a, $b, $c, $d) = mk();
for <a b c d> Z ($a, $b, $c, $d) -> ($name, $f) {
    $bump(); $bump();
    is $elems(), 2, "$name: the two pushes are visible before the block runs";
    $f(1);
    is $elems(), 0, "$name: the nested reset survives the block's exit";
    $c(1);
}

# NB: every scenario below names its own reset sub. Two `my sub`s of the same
# name in different scopes of one file do not resolve independently yet
# (https://github.com/tokuhirom/mutsu/issues/10391), which is unrelated to what
# this file pins.

# Every Buf mutator takes the same path: push, append, unshift, prepend, pop,
# shift, splice, reallocate and the write-* family each re-seat the receiver
# binding. One factory per mutator, so the lexical is a cell shared with the
# reader closure and the mutator's own writer is the thing under test.
sub mk-push() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-push() { $seq = buf8.new }
    my &blk := { $seq.push(4); reset-push() };
    (sub { $seq.elems }, &blk);
}
sub mk-append() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-append() { $seq = buf8.new }
    my &blk := { $seq.append(9); reset-append() };
    (sub { $seq.elems }, &blk);
}
sub mk-prepend() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-prepend() { $seq = buf8.new }
    my &blk := { $seq.prepend(8); reset-prepend() };
    (sub { $seq.elems }, &blk);
}
sub mk-unshift() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-unshift() { $seq = buf8.new }
    my &blk := { $seq.unshift(7); reset-unshift() };
    (sub { $seq.elems }, &blk);
}
sub mk-pop() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-pop() { $seq = buf8.new }
    my &blk := { $seq.pop; reset-pop() };
    (sub { $seq.elems }, &blk);
}
sub mk-shift() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-shift() { $seq = buf8.new }
    my &blk := { $seq.shift; reset-shift() };
    (sub { $seq.elems }, &blk);
}
sub mk-splice() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-splice() { $seq = buf8.new }
    my &blk := { $seq.splice(0, 1); reset-splice() };
    (sub { $seq.elems }, &blk);
}
sub mk-reallocate() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-reallocate() { $seq = buf8.new }
    my &blk := { $seq.reallocate(10); reset-reallocate() };
    (sub { $seq.elems }, &blk);
}
sub mk-write-uint16() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-write-uint16() { $seq = buf8.new }
    my &blk := { $seq.write-uint16(0, 5); reset-write-uint16() };
    (sub { $seq.elems }, &blk);
}
sub mk-write-num32() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-write-num32() { $seq = buf8.new }
    my &blk := { $seq.write-num32(0, 1.5e0); reset-write-num32() };
    (sub { $seq.elems }, &blk);
}
sub mk-write-int16() {
    my $seq = buf8.new(1, 2, 3);
    my sub reset-write-int16() { $seq = buf8.new }
    my &blk := { $seq.write-int16(1, -3); reset-write-int16() };
    (sub { $seq.elems }, &blk);
}

{
    my ($elems, $blk) = mk-push();
    $blk();
    is $elems(), 0, "push then a nested reset";
}
{
    my ($elems, $blk) = mk-append();
    $blk();
    is $elems(), 0, "append then a nested reset";
}
{
    my ($elems, $blk) = mk-prepend();
    $blk();
    is $elems(), 0, "prepend then a nested reset";
}
{
    my ($elems, $blk) = mk-unshift();
    $blk();
    is $elems(), 0, "unshift then a nested reset";
}
{
    my ($elems, $blk) = mk-pop();
    $blk();
    is $elems(), 0, "pop then a nested reset";
}
{
    my ($elems, $blk) = mk-shift();
    $blk();
    is $elems(), 0, "shift then a nested reset";
}
{
    my ($elems, $blk) = mk-splice();
    $blk();
    is $elems(), 0, "splice then a nested reset";
}
{
    my ($elems, $blk) = mk-reallocate();
    $blk();
    is $elems(), 0, "reallocate then a nested reset";
}
{
    my ($elems, $blk) = mk-write-uint16();
    $blk();
    is $elems(), 0, "write-uint16 then a nested reset";
}
{
    my ($elems, $blk) = mk-write-num32();
    $blk();
    is $elems(), 0, "write-num32 then a nested reset";
}
{
    my ($elems, $blk) = mk-write-int16();
    $blk();
    is $elems(), 0, "write-int16 then a nested reset";
}

# A second mutation after the reset lands on the NEW Buf.
{
    my $seq = buf8.new;
    my sub reset-late() { $seq = buf8.new }
    my &blk := { $seq.push(1); reset-late(); $seq.push(2); $seq.push(3) };
    blk();
    is $seq.elems, 2, "push after the nested reset extends the new Buf";
    is $seq.list.join(","), "2,3", "the new Buf holds only the later pushes";
}

# The same shape with the other in-place instance containers.
class MyArr is Array { }
class MyHash is Hash { }
class MyBag is BagHash { }

sub mk-arr() {
    my $seq = MyArr.new;
    my sub reset-arr() { $seq = MyArr.new }
    my &blk := { $seq.push(1); $seq.unshift(0); $seq.append(5, 6); reset-arr() };
    (sub { $seq.elems }, &blk);
}

sub mk-hash() {
    my $seq = MyHash.new;
    my sub reset-hash() { $seq = MyHash.new }
    my &blk := { $seq.push("a" => 1); $seq.append("b" => 2); reset-hash() };
    (sub { $seq.elems }, &blk);
}

sub mk-bag() {
    my $seq = MyBag.new;
    my sub reset-bag() { $seq = MyBag.new }
    my &blk := { $seq<a> = 2; $seq<b>++; reset-bag() };
    (sub { $seq.elems }, &blk);
}

{
    my ($elems, $blk) = mk-arr();
    $blk();
    is $elems(), 0, "an `is Array` subclass reassigned by a nested sub after push/unshift/append";
}

{
    my ($elems, $blk) = mk-hash();
    $blk();
    is $elems(), 0, "an `is Hash` subclass reassigned by a nested sub after push/append";
}

{
    my ($elems, $blk) = mk-bag();
    $blk();
    is $elems(), 0, "an `is BagHash` subclass reassigned by a nested sub after element writes";
}

# Closures handed back from a factory observe the reset through a separate
# reader closure, the way Terminal::ANSIParser's parser does.
{
    my ($elems, $go) = do {
        my $seq = buf8.new;
        my sub reset-factory() { $seq = buf8.new }
        my &go := { $seq.push(1); $seq.push(2); reset-factory() };
        (sub { $seq.elems }, &go);
    };
    $go() for ^3;
    is $elems(), 0, "a reader closure sees the reset after three runs of the block";
}

{
    my @seen;
    my ($elems, $go) = do {
        my $seq = buf8.new;
        my sub reset-finish() { $seq = buf8.new }
        my sub finish($byte) { $seq.push($byte); @seen.push($seq.elems); reset-finish() }
        my &go := { finish(7) };
        (sub { $seq.elems }, &go);
    };
    $go() for ^3;
    is-deeply @seen, [1, 1, 1], "the nested reset is not lost between runs: each run starts from an empty Buf";
    is $elems(), 0, "and the reader agrees";
}

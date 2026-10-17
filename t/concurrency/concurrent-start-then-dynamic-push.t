use Test;

# Once a thread has run, a push to a dynamic array that no thread shares
# still extends the declaring frame's array in place: it used to drop the
# running frame's binding and push onto a detached copy, which reached the
# declaring frame only through a closure call's copy-back (ADR-12529 §3,
# slice 42). Checked against rakudo.

plan 4;

await start { 1 };

sub in-block() { my @*LOG; { @*LOG.push('after') }(); @*LOG.join(',') }
is in-block(), 'after', 'a block pushes onto the caller-declared array';

sub in-pointy() { my @*LOG; -> { @*LOG.push('p') }(); @*LOG.join(',') }
is in-pointy(), 'p', 'a pointy block does too';

sub same-identity() {
    my @*LOG;
    my $before = @*LOG.WHICH;
    { @*LOG.push(1) }();
    @*LOG.WHICH eq $before
}
ok same-identity(), 'the array stays the same container';

my @plain;
{ @plain.push(1) }();
is @plain.elems, 1, 'a plain lexical array is unaffected';

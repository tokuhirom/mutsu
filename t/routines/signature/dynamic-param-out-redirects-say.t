use Test;

# A dynamic parameter (`sub f($*OUT)`) is the handle `say` / `print` / `note`
# write to, in the routine and in everything it calls, and only there (#11348).
# A dynamic scalar is bound under two env spellings (`*OUT` and `$*OUT`); the
# parameter binder now writes both, and neither leaks to the caller.

plan 8;

my class Cap is IO::Handle {
    has @.got;
    submethod TWEAK { self.encoding: 'utf8' }
    method WRITE(IO::Handle:D: Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}

sub f($*OUT) { say "to param"; print "p\n" }
my $d = Cap.new;
f($d);
is-deeply $d.got, ["to param\n", "p\n"], 'say and print write to a $*OUT parameter';

sub g($*ERR) { note "to err" }
my $e = Cap.new;
g($e);
is-deeply $e.got, ["to err\n"], 'note writes to a $*ERR parameter';

sub h { say "in h" }
sub k($*OUT) { h() }
my $d2 = Cap.new;
k($d2);
is-deeply $d2.got, ["in h\n"], 'a routine called from it writes there too';

my $after = Cap.new;
{
    my $*OUT = $after;
    f(Cap.new);
    say "back";
}
is-deeply $after.got, ["back\n"], 'the caller\'s handle is back after the call';

sub s { my $*OUT = $d2; say "my in sub" }
my $outer = Cap.new;
{
    my $*OUT = $outer;
    s();
    say "after s";
}
is-deeply $outer.got, ["after s\n"], 'a `my $*OUT` in a routine does not leak to the caller';
is $d2.got.tail, "my in sub\n", 'it is used inside that routine';

sub dyn($*X) { inner-x() }
sub inner-x { $*X }
is dyn(42), 42, 'a non-handle dynamic parameter is visible to a callee';
my $*X = 1;
dyn(2);
is $*X, 1, 'and does not leak to the caller';

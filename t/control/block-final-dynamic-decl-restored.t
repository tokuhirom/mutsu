use Test;

plan 9;

# A `my $*x` that is the LAST statement of an if branch / given body / do block
# ends with that block.
my $*x = 1;
if True { my $*x = 3; }
is $*x, 1, 'constant-condition if branch';

my $c = True;
if $c { my $*x = 3; }
is $*x, 1, 'if branch';

my $*y = 1;
do { my $*y = 3; }
is $*y, 1, 'do block';

my $*z = 1;
given 1 { my $*z = 3; }
is $*z, 1, 'given body';

# the declaration is still the block's value, and visible to callees inside it
my $*a = 1;
my $v = do { my $*a = 5 };
is $v, 5, 'a block-final declaration yields its value';
is $*a, 1, '... and does not outlive the block';

sub f { $*a }
my @seen;
if True { my $*a = 7; @seen.push: f(); }
@seen.push: f();
is-deeply @seen, [7, 1], 'a callee inside the branch sees the branch value, afterwards the outer one';

# a non-final declaration already behaved
if True { my $*x = 4; 1 }
is $*x, 1, 'non-final declaration';

# the built-in dynamics too
class Col is IO::Handle {
    has @.got;
    method WRITE(Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}
my $h = Col.new(:!chomp); $h.encoding('utf8');
my $out = Col.new(:!chomp); $out.encoding('utf8');
{
    my $*OUT = $out;
    if True { my $*OUT = $h; }
    say "kept";
}
is $out.got.join, "kept\n", 'my $*OUT in a branch is restored for the later say';

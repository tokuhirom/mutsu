use Test;

plan 4;

# A `my $*ERR` inside a sub must not leak into the caller's output routing
# after the sub returns (#11398).

sub capture-stderr(&code) {
    my $err = '';
    my $*ERR = class { method print(*@c) { $err ~= @c.join } };
    &code();
    $err
}

my $captured = capture-stderr { note "inside" };
is $captured, "inside\n", 'note inside the sub goes to the sub-local $*ERR';

# After the capture, $*ERR is the real handle again, so `note` must not
# be swallowed by the dead capture class.
my $outer = '';
{
    my $*ERR = class { method print(*@c) { $outer ~= @c.join } };
    note "outer";
}
is $outer, "outer\n", 'a later block-local $*ERR still captures';

sub h { my $*ERR = 42; 1 }
h();
is $*ERR.^name, 'IO::Handle', 'caller $*ERR is the real handle after the sub';

my $again = capture-stderr { note "second" };
is $again, "second\n", 'a second capture does not see the first call\'s state';

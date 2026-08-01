use v6;
use Test;

# X::IO::Resolve and X::IO::NotAChild are constructible from user code
# (IO::Path::ChildSecure does exactly this) and carry the rakudo messages.

plan 8;

my $r = X::IO::Resolve.new(:path("/x/y"));
isa-ok $r, X::IO::Resolve, 'X::IO::Resolve.new constructs';
is $r.message, 'Failed to completely resolve "/x/y"', 'X::IO::Resolve message';

my $n = X::IO::NotAChild.new(:path("/tmp"), :child("/foo"));
isa-ok $n, X::IO::NotAChild, 'X::IO::NotAChild.new constructs';
is $n.message, 'Path "/foo" is not a child of path "/tmp"',
    'X::IO::NotAChild message';
is $n.path, '/tmp', 'path accessor';
is $n.child, '/foo', 'child accessor';

throws-like { X::IO::NotAChild.new(:path("/a"), :child("/b")).throw },
    X::IO::NotAChild, 'throws as its own type';

my $f = Failure.new(X::IO::NotAChild.new(:path("/a"), :child("/b")));
throws-like { $f.sink }, X::IO::NotAChild, 'a sunk Failure throws the type';

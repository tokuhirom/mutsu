use v6;
use Test;

# IO::MiddleMan is an ecosystem distribution whose IO::Handle subclass keeps
# the original handle in a user attribute and reads $.nl-out while processing
# output.  The inherited getter must use IO::Handle's default on the subclass;
# it must not demand a native descriptor from the wrapper itself.

plan 1;

class WrappedHandle is IO::Handle {
    has IO::Handle $.handle;
}

my $path = 'tmp/io-handle-wrapped-subclass-nl-out-output'.IO;
END { $path.unlink if $path.e }
my $handle = $path.open: :w;
my $wrapped = WrappedHandle.bless: :handle($handle);

is $wrapped.nl-out, "\n", 'IO::Handle.nl-out has its default on a wrapped subclass';

$handle.close;
done-testing;

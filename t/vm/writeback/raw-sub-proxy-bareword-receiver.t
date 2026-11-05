use lib 't/lib';
use Test;

# A bare-word call of an `is raw` routine imported through `sub EXPORT` returns
# a Proxy; a method call on that bare word FETCHes it, exactly as the
# parenthesised call does (Universal::Errno's `errno.symbol`, #16556).

use RawProxyErrno;

plan 5;

is errno.^name, 'errno', 'bare-word receiver: .^name reads the FETCHed value';
is errno.symbol, 'ENOENT', 'bare-word receiver: a user method of the FETCHed class';
is errno().symbol, 'ENOENT', 'parenthesised call agrees';
is errno.WHAT.gist, '(errno)', '.WHAT describes the FETCHed value, not the Proxy';
my $x = errno;
is $x.symbol, 'ENOENT', 'assigning the bare word keeps working';

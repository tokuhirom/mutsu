# `$.name` in sink context is a method call on self, never a "Useless use"
# warning. Found via Test::Describe (`method subs { gather { $.take-subs } }`).
use Test;

plan 2;

my $code = q:to/CODE/;
    class M {
        has $.a = 1;
        method t { take $!a }
        method s { gather { $.t } }
    }
    print M.new.s.eager;
    CODE
my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
is $proc.err.slurp(:close), '', 'no sink warning for $.method in a gather block';
is $proc.out.slurp(:close), '1', 'and the call runs';

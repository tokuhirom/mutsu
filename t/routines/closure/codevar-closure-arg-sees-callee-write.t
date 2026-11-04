use v6;
use Test;

# #16179: a call through a code variable / callable value hands a variable and
# a closure literal to a callee that writes the variable (`is raw`, directly or
# through a `|c` forward). The closure shares the caller's variable, so it must
# see the write while the call runs. A direct sub call already did; the
# code-variable spellings took a snapshot of the old value.

plan 8;

sub root($d is raw, &block) { $d = 7; block }

my &wrap = -> |c { root |c };
my $e;
wrap $e, { is $e, 7, 'forwarded |c through a code variable: closure sees the write' };
is $e, 7, 'and the caller variable holds it afterwards';

my &direct = sub ($d is raw, &block) { $d = 8; block };
my $f;
direct $f, { is $f, 8, 'is raw parameter of a code variable: closure sees the write' };

my $g;
direct($g, { is $g, 8, 'parenthesised code-variable call: closure sees the write' });

my $h;
(-> |c { root |c })($h, { is $h, 7, 'immediately invoked pointy block: closure sees the write' });
is $h, 7, 'immediately invoked pointy block: caller variable holds it';

# A variable followed by a closure the callee never writes keeps working.
my &plain = -> $x, &b { b() };
my $n = 3;
is plain($n, { $n + 1 }), 4, 'read-only argument and closure are unaffected';
is $n, 3, 'the variable is unchanged';

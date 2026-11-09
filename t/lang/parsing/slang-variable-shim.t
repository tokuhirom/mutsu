use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang token may read a variable with the core `<variable>` rule (#13370,
# Qwiratry's `givenroot $data { ... }`).

plan 4;

use VariableShimSlang;

my $data = 5;
my $r;
bindroot $data { $r = $_ + 1 }
is $r, 6, 'a scalar variable';

my @list = 1, 2, 3;
bindroot @list { $r = .elems }
is $r, 3, 'an array variable';

my %h = a => 1, b => 2;
bindroot %h { $r = .keys.sort.join }
is $r, 'ab', 'a hash variable';

my $obj = class { has $.n = 7 }.new;
bindroot $obj { $r = .n }
is $r, 7, 'an object variable';

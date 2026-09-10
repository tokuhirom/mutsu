use Test;
use lib 't/lib';
use TraitParamOtf;

# §2 multi-dispatch VM-ization: a non-builtin module single sub with a standard
# binding-time param trait (is copy / is rw / is raw / is readonly / is required)
# now OTF-compiles to bytecode instead of tree-walking through
# call_function_fallback. The compiled binding honors the trait exactly as the
# interpreter would, and the `is rw` caller writeback (which the interpreter
# fallback silently dropped) now reaches the caller via the compile-time caller
# slot. These check byte-identical behavior against raku.

plan 9;

# 1. is copy: local mutation does not touch the caller.
is bump-copy(5), 12, 'is copy param: local mutation, doubled';

# 2-3. is rw: writes back to the caller's variable.
my $v = 10;
set-rw($v);
is $v, 99, 'is rw param writes back to caller';

my $w = 0;
set-rw($w);
is $w, 99, 'is rw param writes back again (cache hit)';

# 4. is raw: binds without decontainerizing.
is raw-id(42), 42, 'is raw param round-trips value';

# 5. is readonly (explicit).
is ro-id(7), 8, 'is readonly param';

# 6. is required.
is req-id(4), 12, 'is required param';

# 7-9. is rw with a default trailing param.
my $a = 5;
rw-or($a);
is $a, 6, 'is rw + default: default bump applied';

my $b = 5;
rw-or($b, 10);
is $b, 15, 'is rw + default: explicit bump applied';

my $c = 100;
rw-or($c, 0);
is $c, 100, 'is rw + default: zero bump';

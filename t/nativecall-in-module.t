use Test;
use lib 't/lib';
use NCPointerMod;

# A NativeCall binding distributed as a module references the builtin `Pointer`
# type. The main program below never mentions `Pointer`, so the `Pointer`
# prelude must be injected for the *module* too — otherwise the module hits an
# undeclared `Pointer` at runtime.

plan 4;

my $p = alloc-aligned(16, 256);
isa-ok $p, Pointer, 'module returned a Pointer object';
ok $p.Int > 0, 'the module allocated a non-NULL block (is rw out-parameter worked across the module boundary)';
ok $p.Int %% 16, 'the block honours the requested alignment';
lives-ok { release($p) }, 'the module can free the block';

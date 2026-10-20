use Test;
use lib $?FILE.IO.parent(3).add('lib').Str;

# The names a `sub EXPORT` hook returns are the importing block's own
# bindings (ADR-8751, #14140): a closure or sub taken in the block keeps
# them, an import hides an outer variable of the same name, and nothing is
# left visible after the block.

plan 12;

my &c = do { use HookExportsValues; -> { "$hv " ~ ht ~ ' ' ~ @hl.join(',') ~ ' ' ~ hf() } };
is c(), 'hv-val term-val a,b hf', 'a closure taken in the block keeps the hook exports';

my &d;
{ use HookExportsValues; sub inner { $hv ~ ht }; &d = &inner; }
is d(), 'hv-valterm-val', 'a sub declared in the block keeps them after it exited';

my $hv = 'outer-hv';
{ use HookExportsValues; is $hv, 'hv-val', 'the import hides an outer variable of the same name'; }
is $hv, 'outer-hv', 'the outer variable is untouched after the block';

sub callee { $hv }
{ use HookExportsValues; is callee(), 'outer-hv', 'a routine written outside the block does not see it'; }

my @seen;
for ^2 { use HookExportsValues; @seen.push: $hv }
is @seen.join(' '), 'hv-val hv-val', 'a loop body that uses the module sees it on every iteration';

{
    use HookExportsValues;
    ok MY::<$hv>:exists, 'the block pad lists the scalar';
    is ::('$hv'), 'hv-val', 'a by-name read in the block finds it';
    is (EVAL 'ht'), 'term-val', 'an EVAL in the block finds the term';
}

is (try EVAL '$hv'), 'outer-hv', 'after the block the scalar name is the outer one again';
nok (try EVAL 'ht').defined, 'the term is gone after the block';

if False { use HookExportsValues }
isnt ((try EVAL '@hl.join(",")') // ''), 'a,b', 'the BEGIN-time preload of a block-only module imports nothing';

use Test;

# A bare block `{...}` and a pointy block `-> ... {...}` are `Block`s, not `Sub`s.
# Only `sub {...}` / `sub name {...}` are `Sub`s. mutsu already smartmatched bare
# blocks as Block, but `.WHAT`/`.^name` reported `Sub`, and pointy blocks were
# mis-typed as `Sub` everywhere (so `-> $x {…} ~~ Sub` was wrongly True).

plan 12;

is {42}.WHAT.^name,          'Block', 'bare block {…} is a Block';
is {$_ + 1}.WHAT.^name,      'Block', 'bare block with implicit $_ is a Block';
is (-> {42}).WHAT.^name,     'Block', 'pointy block -> {…} is a Block';
is (-> $x {$x}).WHAT.^name,  'Block', 'pointy block with a param is a Block';
is (-> $x, $y {$x}).WHAT.^name, 'Block', 'pointy block with multiple params is a Block';
is (sub {42}).WHAT.^name,    'Sub',   'anonymous sub {…} is a Sub';
is (sub f {42}).WHAT.^name,  'Sub',   'named sub is a Sub';

# Smartmatch type relationships (Block is NOT a Sub/Routine; Sub IS-A Block).
ok  (-> $x {$x}) ~~ Block,  'pointy block ~~ Block';
nok (-> $x {$x}) ~~ Sub,    'pointy block is NOT a Sub';
nok (-> $x {$x}) ~~ Routine,'pointy block is NOT a Routine';
ok  (sub {42})   ~~ Block,  'a Sub IS-A Block (Routine subclasses Block)';
ok  {42}         ~~ Code,   'a block is Code';

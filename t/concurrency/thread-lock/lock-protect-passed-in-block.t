use Test;

# A block handed to `Lock.protect` that was created somewhere else closes over
# ITS creator's lexicals, not over the frame that calls `.protect`. The
# FINALIZER shape: `method !protect(&code) { $!lock.protect: &code }` is called
# with `{ ...push(&code) }`, whose `&code` is the CALLER's parameter.

plan 5;

class Registry {
    has $!lock = Lock.new;
    has @.blocks;
    method !protect(&code) { $!lock.protect: &code }
    method register(&code) { self!protect: { @!blocks.push(&code) }; self }
}
my $block = { 42 };
my $r = Registry.new.register($block);
ok $r.blocks[0] === $block, 'the protected block sees its creator\'s `&code`, not the callee\'s';

class Calls {
    method !protect(&code) { Lock.new.protect: &code }
    method go(&code) { self!protect: { 'saw ' ~ code() } }
}
is Calls.new.go({ 'mine' }), 'saw mine', 'calling the captured `&code` calls the right block';

sub protect-by(&blk) { Lock.new.protect: &blk }
my $x = 'outer';
is protect-by({ $x ~ '!' }), 'outer!', 'a sub forwarding a block also works';

my $lock = Lock.new;
my $count = 0;
$lock.protect: { $count++ } for ^3;
is $count, 3, 'an inline block literal still writes the enclosing lexical';

my $total = 0;
my &add = { $total += 10 };
$lock.protect: &add;
is $total, 10, 'a block stored in a variable writes the lexical it closed over';

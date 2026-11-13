use lib $*PROGRAM.parent(3).add('lib-pending-caller-writeback');
use Test;
use PendingCache;

# A closure that writes a lexical owned by a frame that is no longer on the
# call stack (a module-scope `my` written from a `Lock.protect` block, the
# shape of Monad's type cache) leaves a caller-var writeback no frame can
# claim. Built-in method calls on the mainline after that must still answer
# correctly, and writebacks owned by the mainline must still reach its slots.

my &publish = do {
    my $cache = Map.new;
    my $lock  = Lock.new;
    sub ($key, $value) {
        $lock.protect({
            my %next = $cache;
            %next{$key} = $value;
            $cache = %next.Map;
        });
        $cache
    }
};

is cached(Int), 'Int', 'the module published into its cache';
is cached(Int), 'Int', 'and answered from it the second time';
is cached(Str), 'Str', 'a second key';

my $seen = publish('a', 1);
is $seen<a>, 1, 'the closure published into the hidden cache';
$seen = publish('b', 2);
is $seen.keys.sort.join(','), 'a,b', 'the hidden cache kept both entries';

ok Any.WHICH eq Any.WHICH, 'Any.WHICH on the mainline after the write';
my class C { }
ok C.WHICH eq C.WHICH, 'a class type object .WHICH after the write';
nok C.defined, 'a type object is not defined';
ok C.new.defined, 'an instance is defined';
ok C.DEFINITE.not, 'a type object is not DEFINITE';

# A writeback the mainline does own is still claimed around a lane call.
my $count = 0;
sub bump() { $count++ }
for ^3 {
    bump();
    ok $count.defined, "the counter is defined after bump $count";
}
is $count, 3, 'every bump reached the mainline slot';

my $s = "hello world";
my $r := substr-rw($s, 0, 5);
$r = "HELLO";
ok $s.defined, 'a lane call after a substr-rw store';
is $s, 'HELLO world', 'the substr-rw store reached the variable';

done-testing;

use v6;
use lib 't/lib';
use Test;

plan 10;

# DBIish's prepared-statement path: a module is `require`d inside a method, and
# a parametric role over one of its CStruct classes is instantiated in LATER
# method calls, after every load/composition frame has unwound. Three things
# must survive that:
#   - the CStruct field type alias (`constant slotint`) resolves from the
#     declaring class's module scope when the layout is computed lazily;
#   - the role-body lexicals (`my int $sol = nativesizeof(T)`, `my \ty = T`)
#     persist as statics of the composed pun class;
#   - a SECOND construction (fresh frame, composition long gone) still sees
#     them — this is where DBIish calloc'd a 0-byte MYSQL_BIND array.

class Loader {
    has $.M;
    method install($name) {
        $!M = (require ::($name));
        True
    }
    method mk() {
        $!M.new
    }
}

my $l = Loader.new;
$l.install('DeferredCStruct');

my $expected-stride = $*KERNEL.bits == 64 ?? 16 !! 8;

my $d = $l.mk;
is $d.stride, $expected-stride,
    'the role-body stride (nativesizeof of the CStruct) is right on first construction';
like $d.arr-name, /'Linear[' .* 'NB]'/,
    'the parametric role pun keeps its type argument';
is $d.elem-type, 'DeferredCStruct::Native::NB',
    'the role-body sigilless type capture resolves';
is $d.read-a(0), 10, 'a CStruct element field write in BUILD landed (col 0)';
is $d.read-a(2), 12, 'and the last column (stride is not zero)';

# The second construction is the real pin: the composition frame is gone, so
# the role-body lexicals must come from the pun class's persisted statics.
my $d2 = $l.mk;
is $d2.stride, $expected-stride, 'the stride survives to a second construction';
is $d2.read-a(1), 11, 'and its element writes land too';
ok $d2.write-b(1, 77), 'a later method call can write an element field';
is $d2.read-b(1), 77, 'and read it back';
is $d2.read-b(0), 0, 'without touching the neighbour element';

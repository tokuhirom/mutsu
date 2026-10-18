use Test;

plan 4;

# A named parameter whose parens hold an anonymous destructure of a Pair value
# (`:value((:key($p), :value($v)))`) must be accepted by multi dispatch.
multi sub nested(Pair (Str :key($d), Pair :value((UInt :key($p), :value(&g))))) {
    "nested $d $p " ~ g()
}
multi sub nested(Pair (Str :key($d), :value(&g))) { "flat $d " ~ g() }

is nested('y' => 4 => { 'blk' }), 'nested y 4 blk', 'sub: nested pair value';
is nested('x' => { 'blk' }), 'flat x blk', 'sub: flat pair still picks the other candidate';

class T {
    multi method group(T $tester, Pair (Str :key($d), Pair :value((UInt :key($p), :value(&g))))) {
        "group $d $p " ~ g()
    }
    multi method group(T $tester, Pair (Str :key($d), :value(&g)), :$silent) {
        "plain $d " ~ g()
    }
}
my $t = T.new;
is $t.group($t, 'y' => 4 => { 'b' }), 'group y 4 b', 'method: nested pair value';
is $t.group($t, 'x' => { 'b' }), 'plain x b', 'method: flat pair value';

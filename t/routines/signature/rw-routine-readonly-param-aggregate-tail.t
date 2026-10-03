use v6;
use Test;

plan 11;

# An `is rw` routine whose tail is a readonly `$` parameter returns a value,
# not a container: assigning to the call dies even when that value is a Hash
# or an Array, instead of storing into the caller's aggregate (#11108).

{
    sub w($p) is rw { $p }
    my %r;
    throws-like { w(%r) = 1 }, Exception,
        message => 'Cannot assign to a readonly variable or a value',
        'readonly param tail holding a Hash refuses assignment';
    is-deeply %r, {}, '... and the caller Hash is untouched';

    my @r;
    throws-like { w(@r) = 1 }, Exception,
        message => 'Cannot assign to a readonly variable or a value',
        'readonly param tail holding an Array refuses assignment';
    is-deeply @r, [], '... and the caller Array is untouched';

    my $s = {};
    dies-ok { w($s) = 1 }, 'a $ variable holding a Hash refuses too';
    dies-ok { w({}) = 1 }, 'so does a Hash literal argument';
}

{
    sub w($p) is rw { return-rw $p }
    my %r;
    dies-ok { w(%r) = 1 }, 'return-rw of a readonly param refuses too';
}

{
    # Tails that do alias the caller's aggregate keep storing into it.
    sub s(\p) is rw { p }
    my %r;
    s(%r) = (a => 1);
    is-deeply %r, { a => 1 }, 'sigilless tail stores into the caller Hash';

    sub h(%p) is rw { %p }
    my %q;
    h(%q) = (b => 2);
    is-deeply %q, { b => 2 }, '% param tail stores into the caller Hash';

    sub r($p is raw) is rw { $p }
    my %z;
    r(%z) = (c => 3);
    is-deeply %z, { c => 3 }, 'is raw param tail stores into the caller Hash';

    sub rw($p is rw) is rw { $p }
    my $v = {};
    rw($v) = 4;
    is $v, 4, 'is rw param tail replaces the caller scalar';
}

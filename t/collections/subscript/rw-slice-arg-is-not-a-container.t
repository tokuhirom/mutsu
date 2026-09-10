use v6;
use Test;

# A *slice* subscript passed as an argument (`f(@a[1..*])`, `f(@a[0,1])`,
# `f(%h<a b>)`) is a list of VALUES, not a storage location. Rakudo rejects
# binding one to an `is rw` parameter with X::Parameter::RW, and the source
# container is left untouched.
#
# mutsu used to queue an `is rw` snapshot/writeback for every subscript
# argument, slices included, which was wrong twice over: the bind succeeded
# where rakudo dies, and the writeback then wrote the callee's scalar back over
# the slice ("@a[1..*]" became a single element). The writeback's own guards
# were also defeated whenever the call site re-executed later than it was
# compiled for -- a recursive descent returning a deferred `.map` Seq that is
# reified after the frame is gone died with "Cannot modify an immutable List"
# (roast/integration/99problems-21-to-30.t's P26 `group`, under the vendored
# upstream Test module).

plan 9;

sub rw-set($x is rw) { $x = 9 }

# An ordinary single-element subscript still binds and still writes back.
{
    my @a = 1, 2, 3;
    rw-set(@a[1]);
    is @a.join(','), '1,9,3', 'a single array element still binds is rw';
}

{
    my %h = a => 1, b => 2;
    rw-set(%h<b>);
    is %h<b>, 9, 'a single hash element still binds is rw';
}

# A slice cannot bind to `is rw`, and leaves its source alone.
{
    my @a = 1, 2, 3;
    dies-ok { rw-set(@a[1..*]) }, 'a range slice cannot bind to an is rw parameter';
    is @a.join(','), '1,2,3', '... and the array is untouched';
}

{
    my @a = 1, 2, 3;
    dies-ok { rw-set(@a[0,1]) }, 'an index-list slice cannot bind to an is rw parameter';
    is @a.join(','), '1,2,3', '... and the array is untouched';
}

{
    my %h = a => 1, b => 2;
    dies-ok { rw-set(%h<a b>) }, 'a hash slice cannot bind to an is rw parameter';
    is (%h<a>, %h<b>).join(','), '1,2', '... and the hash is untouched';
}

# The deferred-reification shape: `group` recurses on `@sizes[1..*]` and
# returns a lazy nested `map` Seq. Stringifying it after `group` has returned
# re-enters the map body, where the retired writeback used to fire against the
# immutable List the caller passed in.
{
    sub combination($n, @xs) {
        if $n > @xs { () }
        elsif $n == 0 { ([],) }
        elsif $n == @xs { ([@xs],) }
        else {
            combination($n - 1, @xs[1..*]).map({ [@xs[0], |$_] }).Slip,
            combination($n, @xs[1..*]).Slip
        }
    }

    sub group(@sizes, @elems) {
        return $[] if @sizes == 0;
        map -> $e {
            map -> $g {
                $[ [|@$e], |@$g ]
            }, group(@sizes[1..*], grep { not $_ === any(@$e) }, @elems)
        }, [combination(@sizes[0], @elems)]
    }

    my $got = group((2, 1), (1, 2, 3));
    is ~$got, '1 2 3 1 3 2 2 3 1',
       'a recursive slice descent reifies its deferred Seq after returning';
}

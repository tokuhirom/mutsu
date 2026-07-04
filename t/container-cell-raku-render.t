# Phase 5 leak hardening (docs/element-element-bind-plan.md): a `:=`-bound
# hash element holds a shared `ContainerRef` cell. `.raku`/`.gist` of the
# enclosing hash must itemize the held aggregate exactly like a plain hash value
# (`{:a($[1, 2])}`, not `{:a([1, 2])}`), so a bound element is indistinguishable
# from an unbound one in the rendering.
use Test;

plan 8;

{
    my %h = a => [1, 2], b => [3, 4];
    my $x := %h<a>;
    $x.push(9);
    is %h.raku, '{:a($[1, 2, 9]), :b($[3, 4])}',
        'bound array-valued hash element itemizes in .raku like an unbound one';
    is %h.gist, '{a => [1 2 9], b => [3 4]}',
        'bound array-valued hash element renders in .gist like an unbound one';
}

{
    my %h = a => [1, 2], b => { x => 5 };
    my $x := %h<a>;
    my $y := %h<b>;
    $x.push(9);
    $y<x> = 50;
    is %h.raku, '{:a($[1, 2, 9]), :b(${:x(50)})}',
        'mixed array- and hash-valued bound elements both itemize';
}

{
    # A bound cell nested one level deeper (hash-in-hash) still itemizes.
    my %g = outer => { inner => [7, 8] };
    my $z := %g<outer>;
    $z<inner>.push(9);
    is %g.raku, '{:outer(${:inner($[7, 8, 9])})}',
        'nested bound hash element itemizes its own aggregate values';
    is %g.gist, '{outer => {inner => [7 8 9]}}', 'nested bound element .gist matches';
}

{
    # An array of bound hashes: array elements de-itemize (raku strips `$`),
    # so a bound hash element renders as a plain `{...}` — NOT double-itemized.
    my @a = { p => 1 }, { q => 2 };
    my $w := @a[0];
    $w<p> = 100;
    is @a.raku, '[{:p(100)}, {:q(2)}]',
        'bound hash element inside an array is not double-itemized';
    is @a.gist, '[{p => 100} {q => 2}]', 'bound hash element in array .gist matches';
}

{
    # The bound element still behaves as an itemized Scalar container (not just
    # a rendering fix): it does not flatten in list context.
    my %h = a => [1, 2];
    my $x := %h<a>;
    my @flat = %h<a>, %h<a>;
    is @flat.elems, 2, 'bound array-valued hash element stays itemized (no flatten)';
}

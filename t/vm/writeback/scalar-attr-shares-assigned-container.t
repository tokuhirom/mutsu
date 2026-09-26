use Test;

# #9041: assigning an existing `@array` / `%hash` to a `$`-sigil attribute
# stores the SAME container (the attribute is a Scalar holding the object), so
# later mutations through either name are seen by the other -- exactly like a
# local `my $s = %src`. A whole reassignment of the attribute rebinds the
# Scalar and leaves the source alone.

plan 28;

class T {
    has $.w is rw;
    has $!p;
    method p() is rw { $!p }
    method set($v) { $!w = $v }
    method getw { $!w }
}

{
    my $t = T.new;
    my %src = x => 1;
    $t.w = %src;
    %src<y> = 2;
    is $t.w.raku, '${:x(1), :y(2)}', 'hash source mutation shows through the attribute';
    $t.w<z> = 3;
    is %src<z>, 3, 'element store through the attribute reaches the source hash';
    $t.w<x>:delete;
    is %src.raku, '{:y(2), :z(3)}', ':delete through the attribute reaches the source hash';
    %src<w> = 4;
    is $t.w.elems, 3, 'the source is still shared after the delete';
}

{
    my $t = T.new;
    my @a = 1, 2;
    $t.w = @a;
    @a.push(3);
    is $t.w.raku, '$[1, 2, 3]', 'array source push shows through the attribute';
    $t.w.push(4);
    is @a.raku, '[1, 2, 3, 4]', 'push through the attribute reaches the source array';
    $t.w[0] = 9;
    is @a.raku, '[9, 2, 3, 4]', 'element store through the attribute reaches the source array';
    @a = 7, 8;
    is $t.w.raku, '$[7, 8]', 'whole reassignment of the source shows through';
    is $t.getw.raku, '$[7, 8]', '$!w inside a method reads the shared array itemized';
    is $t.raku, 'T.new(w => $[7, 8])', 'instance .raku renders the shared value itemized';
}

{
    my $t = T.new;
    my @a = 1, 2;
    $t.w = @a;
    $t.w = 5;
    is @a.raku, '[1, 2]', 'accessor reassignment does not overwrite the source';
    is $t.w, 5, '... and rebinds the attribute';
    $t.w = @a;
    $t.set(7);
    is @a.raku, '[1, 2]', '$!w = v inside a method does not overwrite the source';
    is $t.w, 7, '... and rebinds the attribute';
}

{
    my $t = T.new;
    my @a = 1, 2;
    my @b = 8, 9;
    $t.w = @a;
    $t.w = @b;
    @b.push(10);
    @a.push(3);
    is $t.w.raku, '$[8, 9, 10]', 'reassigning to another array shares the new one';
    is @a.raku, '[1, 2, 3]', '... and leaves the first source untouched';
    is @b.raku, '[8, 9, 10]', '... and the second source intact';
}

{
    my $t = T.new;
    my @a = 1, 2;
    $t.w = @a;
    my $x := $t.w;
    is $x.raku, '$[1, 2]', 'a bound alias reads the shared value itemized';
    $x = 5;
    is @a.raku, '[1, 2]', 'assigning through a bound alias does not overwrite the source';
    is $t.w, 5, '... and rebinds the attribute';
}

{
    my $t = T.new;
    my @a = 1, 2;
    $t.p = @a;
    @a.push(3);
    is $t.p.raku, '$[1, 2, 3]', 'is rw method over $!p shares the source';
    $t.p = 4;
    is @a.raku, '[1, 2, 3]', 'is rw method reassignment leaves the source alone';
}

{
    my $t = T.new;
    my @a = 1, 2;
    $t.w = @a;
    my $c = $t.clone;
    @a.push(3);
    is $c.w.raku, '$[1, 2, 3]', 'a clone keeps sharing the source';
    is $t.w.elems, 3, '.elems sees the shared array';
}

{
    sub build { my @src = 1; my $o = T.new; $o.w = @src; @src.push(2); $o }
    is build().w.raku, '$[1, 2]', 'sharing survives the source going out of scope';
}

{
    class Typed { has Array $.a is rw; has Hash $.h is rw }
    my $o = Typed.new;
    my @a = 1, 2;
    $o.a = @a;
    @a.push(3);
    is $o.a.raku, '$[1, 2, 3]', 'typed Array attribute shares the source';
    my %h = q => 1;
    $o.h = %h;
    %h<r> = 2;
    is $o.h.raku, '${:q(1), :r(2)}', 'typed Hash attribute shares the source';
}

{
    my $t = T.new;
    my @a = 1, 2;
    my @copy = @a;
    $t.w = [5, 6];
    @a.push(3);
    is @copy.raku, '[1, 2]', 'an @-variable assignment still copies';
}

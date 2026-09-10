use Test;

# An `is rw` method that returns an *indexed* attribute element —
# `@!attr[$i]` or `%!attr{$k}` — exposes that element as a writable lvalue, so
# `$obj.at($i) = v` assigns into the attribute container. Previously this threw
# X::Assignment::RO ("rw method does not expose an assignable attribute");
# only the bare-scalar-attribute form (`{ $!x }`) worked.

plan 10;

# --- positional element ---
{
    my class C { has @.d; method at($i) is rw { @!d[$i] } }
    my $c = C.new(d => [1, 2, 3]);
    $c.at(1) = 99;
    is-deeply $c.d, [1, 99, 3], 'rw method @!d[$i] assigns into the element';

    $c.at(0) = 10;
    $c.at(2) = 30;
    is-deeply $c.d, [10, 99, 30], 'multiple rw element assignments accumulate';

    is $c.at(1), 99, 'reading the rw method still returns the element value';
}

# --- associative element ---
{
    my class H { has %.h; method at($k) is rw { %!h{$k} } }
    my $h = H.new(h => { a => 1 });
    $h.at('a') = 99;
    is $h.h<a>, 99, 'rw method %!h{$k} assigns into the value';
    $h.at('b') = 5;
    is $h.h<b>, 5, 'rw method autovivifies a new key';
}

# --- return-rw form ---
{
    my class R { has @.d; method at($i) is rw { return-rw @!d[$i] } }
    my $r = R.new(d => [1, 2, 3]);
    $r.at(1) = 42;
    is-deeply $r.d, [1, 42, 3], 'return-rw @!d[$i] assigns into the element';
}

# --- the bare-scalar-attribute rw method is unaffected ---
{
    my class S { has $.x; method getx is rw { $!x } }
    my $s = S.new(x => 1);
    $s.getx = 99;
    is $s.x, 99, 'bare scalar-attribute rw method still works';
}

# --- the `is rw` attribute accessor is unaffected ---
{
    my class A { has $.x is rw; }
    my $a = A.new(x => 1);
    $a.x = 7;
    is $a.x, 7, 'is rw attribute accessor still works';
}

# --- assigning past the end resizes the array ---
{
    my class G { has @.d; method at($i) is rw { @!d[$i] } }
    my $g = G.new(d => [1, 2]);
    $g.at(4) = 9;
    is $g.d.elems, 5, 'rw element assignment past the end resizes';
    is $g.d[4], 9, 'the new slot holds the assigned value';
}

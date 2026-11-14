use Test;

# An untaken `with %h<k> { }` (the element is undefined) must not leave its
# element-source signal pending: the next topicalizing statement modifier
# (`$_ = v without $x`) then aliased that element instead of its own topic and
# its writeback was lost. Reduced from PDF::Font::Loader::CSS, where
# URI.new("./") made CSS::Font's `$_ .= new without $!css` keep a type object.

plan 6;

{
    my %g;
    with %g<a> { }
    my $y;
    $_ = 5 without $y;
    is $y, 5, '`without $x` writes its topic back after an untaken `with %h<k>`';
    nok %g<a>:exists, 'the untaken element is not autovivified';
}

{
    my %g;
    with %g<a> -> $v { }
    my $y;
    $_ = 6 without $y;
    is $y, 6, 'same after an untaken pointy `with %h<k> -> $v`';
}

{
    my @a;
    without @a[3] { }
    my $y;
    $_ = 7 without $y;
    is $y, 7, 'same after a taken `without @a[i]`';
}

class Inner { has $.v = 42 }
class Outer {
    has Inner $.css;
    submethod TWEAK { $_ .= new without $!css }
}
sub probe(%h) { with %h<authority> { } }

probe({});
is Outer.new.css.v, 42, 'an attribute topic is written back after an untaken `with` in another routine';

{
    my %h = k => 1;
    with %h<k> { $_ = 2 }
    is %h<k>, 2, 'a taken `with %h<k>` still writes its topic back to the element';
}

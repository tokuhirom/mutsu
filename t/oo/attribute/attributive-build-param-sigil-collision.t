use v6;
use Test;

# Mux (zef distribution) declares `has %!channels; has $!channels;` and binds
# `submethod BUILD(Int :$!channels)`. The scalar bind used to overwrite the
# same-named hash attribute's slot.
plan 8;

class A {
    has %!c;
    has $!c;
    submethod BUILD(Int :$!c = 7) { }
    method scalar { $!c }
    method hash { %!c }
    method fill { %!c<k> = 1; %!c }
}

my $a = A.new(:c(2));
is $a.scalar, 2, 'scalar attributive BUILD param binds $!c';
is-deeply $a.hash, %(), '%!c keeps its own empty Hash';
is-deeply $a.fill, %(k => 1), '%!c remains a usable Hash';
is A.new.scalar, 7, 'default applies to $!c';

class B {
    has @!l;
    has $!l;
    submethod BUILD(:$!l) { }
    method scalar { $!l }
    method list { @!l }
}
my $b = B.new(:l(5));
is $b.scalar, 5, 'scalar binds next to an Array attribute of the same name';
is-deeply $b.list, [], '@!l stays an empty Array';

class C {
    has $!n;
    has %!n;
    method set($!n) { }
    method get { $!n }
    method h { %!n }
}
my $c = C.new;
$c.set(9);
is $c.get, 9, 'positional attributive param binds $!n';
is-deeply $c.h, %(), '%!n untouched by positional attributive param';

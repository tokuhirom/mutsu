use Test;

# XML::Class: a role-typed candidate declared in a package registers its role
# as `Pkg::Role`; ranking must resolve the bare spelling there, or the wider
# `Attribute` candidate wins.
plan 2;

package Decl {
    role CX { }
    role W { }
    class S { has Str $.x; has Str $.y }
    multi sub d(W $e, CX $a, $o) { "cx" }
    multi sub d(W $e, Attribute $a, Mu $o) { "generic" }
    our sub go($e, $a) { d($e, $a, Str) }
    our sub mk-s { S.new }
    our sub attrs { S.^attributes }
    our sub with-cx($a) { $a does CX; $a }
    our sub with-w($o) { $o does W; $o }
}

my ($plain, $mixed) = Decl::attrs;
$mixed = Decl::with-cx($mixed);
my $w = Decl::with-w(Decl::mk-s);
is Decl::go($w, $plain), "generic", "plain Attribute takes the generic candidate";
is Decl::go($w, $mixed), "cx", "Attribute mixed with CX takes the role-typed candidate";

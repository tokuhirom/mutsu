use lib 't/lib';
use Test;
use ModuleLexicalRwArg;

# A module's (or a package block's, or a bare block's) `my` variable passed by
# a routine that closes over it to an `is rw` parameter must be written back
# (#10372). The variable lives in a store that outlives its declaring scope,
# so the parameter has to bind that store's container, not a copy.
plan 12;

is show-typed(), 'Str', 'unit module: typed lexical starts as its type object';
lives-ok { fill-typed() }, 'unit module: typed lexical binds to an is rw parameter';
is show-typed(), 'opened', 'unit module: the write is visible to a later call';

is fill-plain(), 42, 'unit module: the write is visible in the same routine';
is show-plain(), 42, 'unit module: ...and in another routine';
is bump-twice(), 2, 'unit module: repeated rw writes accumulate';

module InlineRw {
    my $q;
    my $w;
    sub s($x is rw) { $x = 1 }
    our sub read-only-capture() { s($q); $q }
    our sub g() { s($w); $w }
    our sub h() { $w = 9; $w }
}
is InlineRw::read-only-capture(), 1, 'module block: a lexical only read by the routine';
is InlineRw::h(), 9, 'module block: plain assignment';
is InlineRw::g(), 1, 'module block: rw write after a plain assignment';

sub set-one($x is rw) { $x = 1 }
{ my $a; our sub bare-block-rw() { set-one($a); $a } }
is &OUR::bare-block-rw(), 1, 'bare block: our sub passes its captured lexical as rw';

module SameNameA { my $x = 'A'; our sub get() { $x }; our sub put() { set-one($x) } }
module SameNameB { my $x = 'B'; our sub get() { $x } }
SameNameA::put();
is SameNameA::get(), 1, 'same-named lexicals: the rw write reaches its own package';
is SameNameB::get(), 'B', 'same-named lexicals: the other package is untouched';

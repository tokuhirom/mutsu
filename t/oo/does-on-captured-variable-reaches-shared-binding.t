use Test;

# `$x does R` rebinds `$x`, as `=` does (ADR-0018, ADR-12529 phase 3). A
# closure that does it to a captured variable now counts as writing that
# variable, so the variable becomes a shared cell. Before, the closure changed
# only its own copy: a sibling closure, a `start` block's caller, and a routine
# that outlived the declaring frame kept the plain value. The routine return
# merge happened to carry the mixin back only when the closure ran inside a
# callee called from the declaring frame.
plan 5;

role R { has $.tag = 'tagged' }

sub mk { my $v = {:x}; (-> { $v does R; 1 }, -> { $v }) }
my ($set, $get) = mk();
$set();
is $get().tag, 'tagged', 'a sibling closure sees the mixin';

my $w = 42;
await start { $w does R; 1 };
is $w.tag, 'tagged', 'a start block\'s mixin reaches the caller';

my $l = {:x};
lives-ok { $l does R }, 'a mixin inside lives-ok lives';
is $l.tag, 'tagged', '...and reaches the declaring scope';

my $n = 1;
my &later = { $n does R };
sub call-later { later() }
call-later();
is $n.tag, 'tagged', 'a stored closure called from a routine reaches the variable';

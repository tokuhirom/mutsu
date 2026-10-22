use Test;
use lib 't/lib';
use SeenWalker;
use SeenListWalker;

# A module block's `my %seen{Any}` keeps its own binding when its multi subs
# run (as closures) under a caller whose unit declares a same-named
# `my %seen{Hash}`; the sub's registration-time capture must not replace it
# (found via PDF::COS::Util / PDF::COS::Dict, #12907).
plan 5;

my $w = SeenWalker.new: :data{ a => 'x', b => ['y', { c => 'z' }] };
my $ast;
lives-ok { $ast = $w.content }, 'walking a nested structure lives';
is $ast<a>, 'x', 'hash entry';
is $ast<b>[1]<c>, 'z', 'nested list and hash entries';

use SeenListWalker;
my $outer = SeenListWalker.new: :items[ 'p', $w, ['q'] ];
my $out;
lives-ok { $out = $outer.content }, 'a list walker over a hash walker lives';
is $out[1]<b>[1]<c>, 'z', 'the inner structure survives';

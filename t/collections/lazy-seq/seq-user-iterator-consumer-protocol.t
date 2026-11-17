use Test;

# Which iterator method a Seq consumer calls on a user iterator (#17628), as
# measured on rakudo: the folds and the hash/QuantHash coercions walk the Seq
# with `pull-one`; list reification (`.elems`, `.Array`, `.sort`, ...) calls
# the iterator's own `push-all`; `.sink` and the `sink` prefix call its own
# `sink-all`.

plan 26;

my class I does Iterator {
    has @.v = 1..6;
    has int $.pulls;
    has int $.pushall;
    has int $.sinkall;
    method pull-one() { $!pulls++; @!v ?? @!v.shift !! IterationEnd }
    method push-all(\target) { $!pushall++; target.push($_) for @!v; @!v = (); IterationEnd }
    method sink-all() { $!sinkall++; @!v = (); IterationEnd }
}

sub calls(&consume) {
    my $i = I.new;
    consume(Seq.new($i));
    "pulls={$i.pulls} push-all={$i.pushall} sink-all={$i.sinkall}"
}

my $pulled = 'pulls=7 push-all=0 sink-all=0';
is calls(*.sum),      $pulled, '.sum pulls one at a time';
is calls(*.join(',')), $pulled, '.join';
is calls(*.max),      $pulled, '.max';
is calls(*.min),      $pulled, '.min';
is calls(*.minmax),   $pulled, '.minmax';
is calls(*.gist),     $pulled, '.gist';
is calls(*.Set),      $pulled, '.Set';
is calls(*.Bag),      $pulled, '.Bag';
is calls(*.Mix),      $pulled, '.Mix';
is calls(*.hash),     $pulled, '.hash';
is calls(*.Map),      $pulled, '.Map';
is calls(*.classify(* %% 2)), $pulled, '.classify';

my $pushed = 'pulls=0 push-all=1 sink-all=0';
is calls(*.elems),   $pushed, '.elems reifies through push-all';
is calls(*.Array),   $pushed, '.Array';
is calls(*.sort),    $pushed, '.sort';
is calls(*.reverse), $pushed, '.reverse';
is calls(*.Str),     $pushed, '.Str';

my $sunk = 'pulls=0 push-all=0 sink-all=1';
is calls(*.sink), $sunk, '.sink calls sink-all';
is calls(-> $s { sink $s }), $sunk, 'the sink prefix on a Seq in a variable';

is Seq.new(I.new).sum, 21, 'the folds still see every value';
is Seq.new(I.new).join(','), '1,2,3,4,5,6', '... in order';

# The sink prefix is an explicit `.sink`.
my $ran = 0;
my $g = gather { $ran++ };
sink $g;
is $ran, 1, 'sink runs a gather held in a variable';
my @lazy = 1..*;
sink @lazy;
pass 'sinking a lazy array does not force it';
my $list = (1..*).list;
$list.sink;
pass '... nor does .sink on a lazy List';
my class Noisy { has $.n is rw = 0; method sink { $!n++ } }
my $noisy = Noisy.new;
sink $noisy;
is $noisy.n, 1, 'the sink prefix calls a user sink method';
sub fails { fail 'nope' }
my $f = fails();
throws-like { sink $f }, X::AdHoc, message => 'nope', 'the sink prefix throws an unhandled Failure';

use Test;

# An Iterator is a reference object: pulling from it through an array
# element (or an attribute array) advances the one shared iterator.
# Reduced from the MergeOrderedSeqs distribution, which keeps its source
# iterators in `has Iterator @.iterators` and calls
# `@!iterators[$index].pull-one`; it used to replay the first element forever.

plan 8;

my @a = 1, 3, 6;

my @its = @a.iterator,;
is @its[0].pull-one, 1, 'first pull through an array element';
is @its[0].pull-one, 3, 'second pull through an array element advances';

my $i = @a.iterator;
my @alias = $i,;
@alias[0].pull-one;
is $i.pull-one, 3, 'the variable sees a pull made through the array element';

my Iterator @typed = @a.iterator,;
@typed[0].pull-one;
is @typed[0].pull-one, 3, 'typed Iterator array element advances';

class Holder {
    has @.iters;
    method pull { @!iters[0].pull-one }
}
my $h = Holder.new(iters => (@a.iterator,));
is $h.pull, 1, 'first pull through an attribute array';
is $h.pull, 3, 'second pull through an attribute array advances';
is $h.pull, 6, 'third pull through an attribute array advances';
ok $h.pull =:= IterationEnd, 'exhausted iterator returns IterationEnd';

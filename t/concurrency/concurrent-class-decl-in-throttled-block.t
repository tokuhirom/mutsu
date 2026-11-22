use Test;

# #18157 (Proc::Q t/03-in.t): a `class` declared inline in a block that
# `Supply.throttle` runs on several threads at once. Every activation builds
# an instance of the class and sends it on; one of them used to come back with
# a `Nil` attribute because another thread was re-composing the class at the
# same moment (the shared-registry re-entry race of #18070).

plan 2;

my $c = Channel.new;
my @in = Nil, 'a', 'b', 'c', 'd', 'e', 'f', 'g', Nil, 'h', 'i', 'j';
(start await Supply.from-list(@in).throttle: 4, -> $in {
    $c.send: class Res {
        has Stringy $.out is required;
        has Mu $.tag is required;
    }.new: :out($in // ''), :tag(1);
}).then: { $c.close };
my @res = $c.list.map(*.out);

is @res.elems, 12, 'every activation sent a result';
is @res.grep(!*.defined).elems, 0, 'no result lost its attribute to a concurrent re-declaration';

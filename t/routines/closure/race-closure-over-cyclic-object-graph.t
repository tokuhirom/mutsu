use Test;

plan 2;

# Starting a worker thread walks the objects its closure can reach for IO
# handles. The walk must not hold an attribute lock while it follows a cycle
# (parent <-> child), or a worker writing one of those attributes at the same
# time blocks it for good.
class Child { has $.parent is rw; has Int $.hits = 0; method hit { $!hits++ } }
class Parent { has @.kids; method add { my $k = Child.new(parent => self); @!kids.push: $k; $k } }

my $p = Parent.new;
my @kids = (^4).map: { $p.add };

my @res;
lives-ok {
    for ^30 {
        @res = (^8).race(:batch(1)).map: -> $i {
            my $k = @kids[$i % 4];
            $k.hit;
            $k.parent.kids.elems;
        };
    }
}, 'racing closures over a cyclic object graph finishes';
is @res.unique.List, (4,), 'every worker saw the whole graph';

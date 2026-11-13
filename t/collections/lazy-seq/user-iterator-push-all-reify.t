use Test;

# Making a Seq.new($iterator) a list calls the iterator's own push-all, as
# rakudo's List.from-iterator does, when its class writes one (#17337).

plan 14;

class Counting does Iterator {
    has @.v = 1..6;
    has int $.pulls;
    has int $.push-alls;
    method pull-one() { $!pulls++; @!v ?? @!v.shift !! IterationEnd }
    method push-all(\target) {
        $!push-alls++;
        target.push($_) for @!v;
        @!v = ();
        IterationEnd
    }
}

for <list elems reverse Array sort first-end> -> $what {
    my $it = Counting.new;
    my $seq = Seq.new($it);
    my $r = do given $what {
        when 'list'      { $seq.list.elems }
        when 'elems'     { $seq.elems }
        when 'reverse'   { $seq.reverse.head }
        when 'Array'     { $seq.Array.elems }
        when 'sort'      { $seq.sort.tail }
        when 'first-end' { $seq.first(* %% 2, :end) }
    };
    is $r, 6, "$what answers from the pushed values";
    is "{$it.pulls} {$it.push-alls}", '0 1', "$what drains the iterator with one push-all";
}

class PullOnly does Iterator {
    has @.v = 1..3;
    has int $.pulls;
    method pull-one() { $!pulls++; @!v ?? @!v.shift !! IterationEnd }
}

{
    my $it = PullOnly.new;
    is-deeply Seq.new($it).list, (1, 2, 3), 'the role default push-all pulls one at a time';
    is $it.pulls, 4, 'every value and the end were pulled';
}

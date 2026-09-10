use v6;
use Test;

plan 11;

# A user class that `does Iterable` supplies its own `iterator`; `for` calls it
# and iterates the produced elements (Iterable.rakudoc / iterating.rakudoc).
{
    class DNA does Iterable {
        has $.chain;
        method iterator(DNA:D:) { $!chain.comb.rotor(3).iterator }
    }
    my $a := DNA.new(chain => 'GAATCC');
    my @got;
    @got.push($_.join) for $a;
    is @got.join('|'), 'GAA|TCC', 'for calls .iterator on a := bound Iterable';

    # Bare-expression source iterates the same way.
    my @bare;
    @bare.push($_.join) for DNA.new(chain => 'ACGTTT');
    is @bare.join('|'), 'ACG|TTT', 'for iterates a bare Iterable expression';

    # Array-assignment context reifies the iterator into the array.
    my @chain = DNA.new(chain => 'ACGTACGTT');
    is @chain.map(*.join).join('|'), 'ACG|TAC|GTT', 'my @a = Iterable reifies .iterator';
    is @chain.elems, 3, 'array-assigned Iterable has the iterated element count';
}

# A class that `does Iterable does Iterator` returning self and driving pull-one.
{
    class Count does Iterable does Iterator {
        has $.n;
        has Int $!i = 0;
        method iterator { self }
        method pull-one(--> Mu) {
            if $!i < $.n {
                $!i++;
                return $!i;
            }
            IterationEnd;
        }
    }
    my $c := Count.new(n => 4);
    my @got;
    @got.push($_) for $c;
    is @got.join(','), '1,2,3,4', 'does Iterator: pull-one drives for until IterationEnd';

    my @arr = Count.new(n => 3);
    is @arr.join(','), '1,2,3', 'does Iterator: array-assign reifies pull-one';
}

# A separate Iterator object (iterator does NOT return self) with mutable state.
{
    class Down does Iterable {
        has $.from;
        method iterator {
            class It does Iterator {
                has $.cur is rw;
                method pull-one {
                    return IterationEnd if $.cur < 1;
                    my $v = $.cur;
                    $.cur = $.cur - 1;
                    $v;
                }
            }.new(cur => $.from);
        }
    }
    my $d := Down.new(from => 3);
    my @got;
    @got.push($_) for $d;
    is @got.join(','), '3,2,1', 'separate stateful Iterator object works in for';
}

# Itemization: `:=` bind iterates, `=` assign keeps a single item.
{
    my $bound := (1, 2, 3);
    my @a;
    @a.push($_) for $bound;
    is @a.join(','), '1,2,3', ':= bound list iterates its elements in for';

    my $assigned = (1, 2, 3);
    my @b;
    @b.push($_.gist) for $assigned;
    is @b.elems, 1, '= assigned list is a single item in for';

    # Bind to an already-itemized scalar inherits the item container.
    my $it = (4, 5, 6);
    my $rebound := $it;
    my @c;
    @c.push($_) for $rebound;
    is @c.elems, 1, ':= bind to an itemized scalar stays a single item';

    # Bind to an already-non-itemized bound scalar stays non-itemized.
    my $nc := (7, 8, 9);
    my $rebound2 := $nc;
    my @d;
    @d.push($_) for $rebound2;
    is @d.join(','), '7,8,9', ':= bind to a non-itemized bound scalar iterates';
}

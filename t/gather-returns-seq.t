use v6;
use Test;

plan 12;

# A `gather` block evaluates to a `Seq` in Raku, not an `Array`.

is (gather { take 5 }).^name, 'Seq', 'gather .^name is Seq';
ok (gather { take 1; take 2 }) ~~ Seq, 'gather result does Seq';
nok (gather { take 1; take 2 }) ~~ Array, 'gather result is not an Array';
ok (gather { take 1; take 2 }) ~~ Iterable, 'gather result is Iterable';

# A `--> Seq` return constraint accepts a gather result.
{
    sub f(--> Seq) { gather { take 1; take 2; take 3 } }
    my $r = f();
    is $r.^name, 'Seq', 'routine with --> Seq returns a Seq';
    is $r.List, (1, 2, 3), 'the returned Seq reifies correctly';
}

# Method with `--> Seq` returning gather (the zef PackageRepository.search shape).
{
    class Repo {
        method search(*@names --> Seq) {
            gather for @names -> $n { take "found:$n" }
        }
    }
    my @r = Repo.new.search('a', 'b');
    is @r.elems, 2, 'method --> Seq gather yields all elements';
    is @r[0], 'found:a', 'first element correct';
    is @r[1], 'found:b', 'second element correct';
}

# Assigning a gather to an `@` variable still coerces to Array (Seq.Array).
{
    my @a = gather { take 1; take 2 };
    is @a.^name, 'Array', 'gather assigned to @ is an Array';
    is @a.elems, 2, 'assigned array has the elements';
}

# An empty gather is still a Seq.
is (gather { }).^name, 'Seq', 'empty gather is a Seq';

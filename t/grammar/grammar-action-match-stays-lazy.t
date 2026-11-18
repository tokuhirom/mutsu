use Test;

# A grammar action's `$/` is a lazy Match: binding it to the action's
# parameter, storing it in a scalar, reading it through a container and
# type-checking it must all behave as for the materialized Match.

plan 9;

grammar G {
    token TOP { <word> ' ' <word> }
    token word { \w+ }
}

my @seen;
my $kept;
class A {
    method word($/) {
        my $m = $/;                 # a scalar store of the lazy Match
        @seen.push: ~$m;
        make $m.Str.uc;
    }
    method TOP($/) {
        $kept = $/;
        make $<word>.map(*.made).join('-');
    }
}

my $r = G.parse('ab cd', :actions(A));
is $r.made, 'AB-CD', 'make through the actions';
is @seen.join(','), 'ab,cd', 'a stored $/ stringifies';
isa-ok $kept, Match, 'a kept $/ is a Match';
is $kept<word>[1].Str, 'cd', 'and reads its captures';

sub takes-any(Any $x) { $x.^name }
sub takes-mu(Mu $x) { $x.^name }
sub takes-match(Match $x) { $x.from }
class B {
    method word($/) {
        make (takes-any($/), takes-mu($/), takes-match($/)).join(',');
    }
}
my $b = G.parse('ab cd', :actions(B));
is $b<word>[0].made, 'G,G,0', 'a lazy $/ binds to Any, Mu and Match parameters';
is $b<word>[1].made, 'G,G,3', 'at its own position';

my @cells;
class C { method word($/) { @cells.push: $/ } }
G.parse('ab cd', :actions(C));
is @cells.elems, 2, 'a pushed $/';
is @cells.map(*.Str).join(','), 'ab,cd', 'keeps its text';
ok @cells[0] ~~ Match, 'and its type';

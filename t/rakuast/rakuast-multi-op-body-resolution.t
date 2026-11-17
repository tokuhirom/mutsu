use experimental :rakuast;
use Test;

# The proto rakudo derives for a scope's `multi` candidates is installed only
# once the first candidate is complete (#17793). An operator inside that first
# candidate's body therefore resolves to the outer scope's operator (here the
# setting's), not to the routine being declared; later candidates, and code
# after the declaration, see the scope's own dispatcher. A `proto` declared
# first makes the dispatcher visible to every candidate's body. Red's CHECK-time
# compiler reads these resolutions (`red-compile`).

plan 10;

my %seen;
CHECK {
    sub walk($n, $where) {
        my $here = $where;
        if $n ~~ RakuAST::Sub && $n.name {
            $here = $n.name.canonicalize ~ '#' ~ (%seen{$n.name.canonicalize ~ '#n'}++ // 0);
        }
        if $n ~~ RakuAST::Infix | RakuAST::Prefix
            && $n.operator eq '+' | '-' | '*' {
            %seen{$here ~ ' ' ~ $n.operator}.push: $n.resolution.^name;
        }
        $n.visit-children(-> $c { walk($c, $here) });
    }
    walk($*CU, 'unit');
}

{
    multi infix:<+>(Str $l, Int $r) { 100 + $r }
    multi infix:<+>(Str $l, Str $r) { 200 + $r }
    is "a" + 1, 101, 'the first candidate runs, its body adding with the outer +';
    is "a" + "2", 202, 'the second candidate runs';
}
{
    multi prefix:<->(Str $s) { -1 * $s.chars }
    is -"abc", -3, 'a block-local multi prefix runs';
}
{
    proto infix:<*>(|) {*}
    multi infix:<*>(Str $l, Int $r) { $l x (1 * $r) }
    multi infix:<*>(Int $l, Int $r) { $l + $r }
    is "ab" * 2, 'ababab', "a candidate's body under a declared proto calls the proto";
}

is %seen{'infix:<+>#0 +'}, ['RakuAST::Declaration::External::Setting'],
    "the first multi's body resolves + to the outer (setting's) operator";
isnt %seen{'infix:<+>#1 +'}[0], 'RakuAST::Declaration::External::Setting',
    "the second multi's body sees the scope's derived dispatcher";
is %seen{'prefix:<->#0 -'}, ['RakuAST::Declaration::External::Setting'],
    "a multi prefix's body resolves - to the setting's";
is %seen{'prefix:<->#0 *'}, ['RakuAST::Declaration::External::Setting'],
    "an unrelated operator in the body is the setting's";
is %seen{'infix:<*>#1 *'}.elems, 1, 'the candidate under a proto was walked';
isnt %seen{'infix:<*>#1 *'}[0], 'RakuAST::Declaration::External::Setting',
    "under a declared proto the first multi's body sees the proto";

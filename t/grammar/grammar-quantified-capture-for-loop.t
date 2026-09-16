use Test;

plan 6;

# A grammar's quantified named capture (`<name>*`) is a non-itemized List:
# `for $/<name> { ... }` must flatten to one iteration per match, exactly like
# `for $/<name>.list { ... }` — not collapse into a single iteration over the
# whole array. ANTLR4::Grammar's action class relies on this shape
# (`for $/<prequelConstruct> { ... }`).
{
    grammar G {
        token TOP { <a>* }
        token a { 'a' \d+ }
    }
    my $m = G.parse("a1a2a3");
    my $n = 0;
    my @seen;
    for $m<a> {
        $n++;
        @seen.push(~$_);
    }
    is $n, 3, 'for over a quantified named capture iterates once per match';
    is @seen.join(','), 'a1,a2,a3', 'each iteration sees one match, in order';
}

# A non-quantified single named capture stays a single item.
{
    grammar G2 {
        token TOP { <a> }
        token a { \w+ }
    }
    my $m = G2.parse("hello");
    my $n = 0;
    for $m<a> { $n++ }
    is $n, 1, 'for over a non-quantified named capture iterates once';
}

# Assigning the capture into a scalar still itemizes it (Raku: `=` always
# itemizes), so a SUBSEQUENT `for` over that scalar does NOT flatten — this
# must not regress while fixing the direct-capture case above.
{
    grammar G3 {
        token TOP { <a>* }
        token a { 'a' \d+ }
    }
    my $m = G3.parse("a1a2");
    my $cap = $m<a>;
    my $n = 0;
    for $cap { $n++ }
    is $n, 1, 'for over a scalar assigned from a quantified capture stays itemized';
}

# Ordinary array/hash element rw-aliasing writeback (what the desugaring this
# fix touches exists for) must keep working.
{
    my @arr = (1, 2, 3);
    for @arr[0] { $_ = 99 }
    is @arr[0], 99, 'for over a single array element still writes back';

    my %h = (k => "lower");
    for %h<k> { .=uc }
    is %h<k>, 'LOWER', 'for over a single hash-value element still writes back';
}

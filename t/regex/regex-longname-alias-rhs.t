use Test;

plan 5;

# `<alias=Fully::Qualified::rule>` is a legal aliased call to another
# grammar's rule: the LongName restriction applies only when the ALIAS side
# (left of `=`) is a long name (`<IO::File=bar>`). Cro::HTTP::Cookie calls
# `<dt=DateTime::Parse::Grammar::rfc1123-date>` this way.

grammar RLAR-A {
    token x { "a" }
}
grammar RLAR-B {
    token TOP { <y=RLAR-A::x> }
}

my $m = RLAR-B.parse("a");
ok $m.defined, 'aliased fully-qualified subrule call parses';
is ~$m<y>, 'a', 'capture available under the alias';
is ~$m{'RLAR-A::x'}, 'a', 'capture also available under the original long name';
is $m.hash.keys.sort.join(','), 'RLAR-A::x,y', 'both capture keys present';

# The illegal direction (long name as the alias itself) still dies.
throws-like { EVAL '"a" ~~ /<IO::File=bar>/' },
    X::Syntax::Regex::Alias::LongName,
    'long name on the alias side still fails';

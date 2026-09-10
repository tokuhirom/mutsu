use Test;

plan 8;

# A grammar *instance* (`G.new`) dispatches `.parse`/`.subparse`/`.parsefile`
# exactly like the grammar type object. (Regression: mutsu resolved these only
# on the type object, so `G.new.parse(...)` died "No such method 'parse'".)

grammar G {
    token TOP  { <word>+ % \s+ }
    token word { \w+ }
}

my $g = G.new;

ok $g.parse('hello world'), 'instance .parse matches';
is $g.parse('hello world').Str, 'hello world', 'instance .parse returns the full Match';
ok $g.subparse('hello world trailing! rest'), 'instance .subparse matches a prefix';
is $g.parse('hi', :rule<word>).Str, 'hi', 'instance .parse with :rule';
nok $g.parse('!!!'), 'instance .parse fails on non-matching input';

# subrule access on an instance-parsed Match
my $m = $g.parse('foo bar');
is $m<word>».Str.join(','), 'foo,bar', 'instance-parsed Match exposes subcaptures';

# The type-object form still works identically.
is G.parse('a b c').Str, 'a b c', 'type-object .parse still works';

# parsefile on an instance
my $tmp = $*TMPDIR.add("grammar-instance-{$*PID}.txt");
$tmp.spurt("alpha beta");
is $g.parsefile($tmp.Str).Str, 'alpha beta', 'instance .parsefile matches';
$tmp.unlink;

use Test;

plan 6;

# A `:m` (ignoremark) rule called as `<r>` over a subject without combining
# marks runs as a frame, entering its ends one at a time: a code block in the
# callee runs once per end entered, not once per end computed. Each
# expectation was checked against rakudo.

my @log;
grammar G {
    regex TOP { <r> 'x' }
    regex r { :m a+ { @log.push: $/.to } }
}
ok G.parse('aax'), 'a :m callee matches';
is @log.join(','), '2', '... and its code block ran only at the end that was entered';

@log = ();
ok G.parse('aax').<r>.Str eq 'aa', '... and captures the right span';
is @log.join(','), '2', '... again';

# Over a subject that has a mark the callee still matches mark-insensitively.
my $marked = "a\x[301]a" ~ "x";
ok G.parse($marked), 'a :m callee matches over a subject with a combining mark';
@log = ();
ok G.parse("aax"), 'and still over a plain subject afterwards';

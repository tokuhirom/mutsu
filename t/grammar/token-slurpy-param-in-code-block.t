use Test;

# A token's array/hash parameters -- a slurpy `*@x` / `*%h` above all -- are
# visible to the token body's code blocks and assertions (#18670).

plan 7;

my @log;
grammar G {
    token TOP     { <slurp(1, 2, 3)> <rest(1, 2, 3)> <named(1, :k<v>)> <seq(1, 2 ... 4)> }
    token slurp(*@x)       { { @log.push: @x.raku } . }
    token rest($f, *@r)    { { @log.push: "$f " ~ @r.raku } . }
    token named($p, *%h)   { { @log.push: %h.raku } . }
    token seq(*@s)         { { @log.push: @s.raku } . }
}
ok G.parse('abcd'), 'parses';
is @log[0], '[1, 2, 3]', 'a slurpy array in a code block';
is @log[1], '1 [2, 3]', 'a slurpy after a positional';
is @log[2], '{:k("v")}', 'a slurpy hash';
is @log[3], '[1, 2, 3, 4]', 'a sequence argument into a slurpy';

grammar H {
    token TOP { <a(<p q>)> }
    token a(*@x) { . <?{ @x.elems == 2 }> { @log.push: "@x[]" } }
}
@log = ();
ok H.parse('x'), 'a slurpy read by an assertion';
is @log[0], 'p q', 'and interpolated as a zen slice in a string';

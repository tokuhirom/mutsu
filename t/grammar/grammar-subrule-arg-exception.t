use Test;

# A subrule method that dies ends the parse with ITS exception, even when a
# later `||` branch would also run, and a subrule argument list sees the
# captures the enclosing rule already took. From Badger's
# `<?{ ~$<name> (elem) @*param-names }> <.panic: "Duplicate parameter name: $<name>">`.

plan 3;

grammar H {
    token TOP { \w+ \s* [ '%' <.panic: "hash"> || <.panic: "other"> ] }
    method panic($r) { die $r }
}
throws-like { H.parse('Foo %') }, Exception, message => 'hash',
    'the first branch panic is the one reported';

grammar G {
    token TOP { :my @*seen; <item>+ % ',' }
    token item {
        <name>
        [ <?{ ~$<name> (elem) @*seen }> <.panic: "dup $<name>">
        || { @*seen.push: ~$<name> } ]
    }
    token name { \w+ }
    method panic($r) { die $r }
}
throws-like { G.parse('a,b,a') }, Exception, message => 'dup a',
    'argument interpolates the enclosing capture';
ok G.parse('a,b,c'), 'no duplicate parses';


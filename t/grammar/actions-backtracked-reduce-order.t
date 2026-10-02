use Test;

# From URI::Template (t/020-grammar.t, t/030-spec-examples.t): an action fires
# when its rule reduces, so a reduction that a later alternative supersedes
# (`<bits>*` / `%% <bits>+` re-matching the same span) still runs, at its
# chronological position -- after the expression that precedes it, not before.

grammar G {
    rule TOP { <bits>* [ <expression>+ ]* %% <bits>+ }
    token bits { <-[{]>+ }
    regex expression { '{' <variable>+ % ',' '}' }
    regex variable { <-[\s,\}]>+ }
}

class A {
    has @.parts;
    method TOP($/) { $/.make(@!parts) }
    method bits($/) { @!parts.push("bits:" ~ $/.Str) }
    method expression($/) { @!parts.push("expr:" ~ $/.Str) }
}

my $a = A.new;
G.parse('{+path}/here', :actions($a));
is $a.parts.join(' '), 'expr:{+path} bits:/here bits:/here',
    'superseded trailing <bits> runs after the leading <expression>';

my $b = A.new;
G.parse('a{x}b{y}c', :actions($b));
is $b.parts.join(' '), 'bits:a expr:{x} bits:b expr:{y} bits:c bits:c',
    'interleaved literal and expression actions keep source order';

my $c = A.new;
my $m = G.parse('{x}', :actions($c));
is $c.parts.join(' '), 'expr:{x}', 'no superseded reduction, nothing extra';
is $m.made.join(' '), 'expr:{x}', 'TOP still sees the made value';

done-testing;

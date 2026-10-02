# Distribution: Grammar::TokenProcessing (random-sentence-generation reads a
# grammar's tokens from `.^method_table` and parses each one's `.gist`).
use Test;

plan 11;

grammar P {
    rule TOP { I <love> <lang> }
    token love { '<3' | love }
    token lang { < Raku Perl > }
    regex rx { a | 'b' }
}

my %t = P.^method_table;
is %t.keys.sort.join(' '), 'TOP lang love rx', 'tokens, rules and regexes are in the method table';
is %t<TOP>.gist.trim, "rule TOP \{ I <love> <lang> }", 'rule gist is the declaration';
is %t<love>.gist.trim, "token love \{ '<3' | love }", 'token gist keeps its quoting';
is %t<lang>.gist.trim, 'token lang { < Raku Perl > }', 'a word-list token keeps its source';
is %t<rx>.gist.trim,   "regex rx \{ a | 'b' }", 'regex gist';
is P.^lookup('love').gist.trim, "token love \{ '<3' | love }", '.^lookup shares the gist';

# A `proto token` is a method-table entry, also when it arrives through a role
# composed into another role (Inner -> Outer -> Words).
role A { proto token t {*}; token t:sym<e> { x }; token u { y } }
role B does A { token v { z } }
grammar G does B { token TOP { <v> } }
is G.^method_table.keys.sort.join(' '), 'TOP t t:sym<e> u v', 'nested role protos are listed';
is G.^method_table<t>.gist.trim, 'token t {*}', 'proto gist';

# The source text must survive the module AST cache: run it twice.
my $code = 'use GrammarTokenSource; say Words.^method_table.keys.sort.join(" "); '
    ~ 'say Words.^method_table<TOP>.gist';
my @out;
for 1..2 {
    my $p = run $*EXECUTABLE, '-I', 't/lib', '-e', $code, :out, :err;
    @out.push: $p.out.slurp(:close);
}
is @out[0].trim, "TOP noun noun:sym<English> phrase\nrule TOP \{ <phrase> }", 'module grammar table (cold)';
is @out[1], @out[0], 'and again from the AST cache';

# Hyper `.&sub` over a deferred Seq.
sub up($s) { $s.uc }
my @a = <i love>;
is-deeply (@a.grep({ True })>>.&up).List, <I LOVE>.List, '>>.&sub on a grep Seq';

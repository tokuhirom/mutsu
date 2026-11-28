# `.parse` / `.subparse` on a grammar is routed straight to the native
# Grammar row at the call site when nothing can claim the call first (#12917).
# These pin that the route answers exactly what the full method-call path
# answers, and that every case it must leave alone still reaches the user's
# own code. Values are rakudo's.
use Test;

plan 16;

grammar G { token TOP { \w+ } }
is ~G.parse('abc'), 'abc', 'a type object';
is ~G.new.parse('abc'), 'abc', 'an instance';
nok G.parse('a b'), 'a failed parse';
is ~G.subparse('ab cd'), 'ab', 'subparse';

class Holder {
    has $.text;
    method run($g) { $g.new.parse($!text) }
}
is ~Holder.new(text => 'xyz').run(G), 'xyz', 'an attribute argument from a method';

# Named arguments take the full path, and still work.
grammar R { token TOP { <num> }; token num { \d+ }; token word { \w+ } }
is ~R.parse('42', :rule<word>), '42', 'a :rule argument';
class A { method TOP($/) { make 'made' } }
is R.parse('7', :actions(A)).made, 'made', 'an :actions argument';

# A grammar's own parse wins.
grammar Own {
    token TOP { \d+ }
    method parse($s) { 'own ' ~ $s }
}
is Own.parse('5'), 'own 5', "the grammar's own parse is called";
is Own.new.parse('5'), 'own 5', 'on an instance too';

# An inherited user parse wins too, and its nextwith reaches the native one.
grammar Base { method parse($s, *%a) { nextwith($s.uc, |%a) } }
grammar Kid is Base { token TOP { <[A..Z]>+ } }
is ~Kid.parse('abc'), 'ABC', 'an inherited user parse with nextwith';

# A role-composed parse wins.
role P { method parse($s) { 'role ' ~ $s } }
grammar WithRole does P { token TOP { .+ } }
is WithRole.parse('q'), 'role q', 'a role-composed parse';

# Code blocks and outer variables still see the parse.
my $seen = 0;
grammar Blk { token TOP { \w+ { $seen++ } } }
Blk.parse('a');
Blk.new.parse('b');
is $seen, 2, 'code blocks in the routed parse write the outer variable';

# A non-grammar class with a parse method is untouched.
class NotG { method parse($x) { "class $x" } }
is NotG.parse('y'), 'class y', 'a plain class with a parse method';

# The parse result is $/ as usual.
G.parse('def');
is ~$/, 'def', '$/ is set by the routed parse';

# A Junction argument autothreads on the full path.
my $j = G.parse('ab' | 'c d');
ok $j ~~ Junction, 'a Junction argument autothreads';

# A wrapped parse is called.
grammar Wr { token TOP { \w+ } }
Wr.^find_method('parse').wrap(-> |c { 'wrapped' });
is Wr.parse('a'), 'wrapped', 'a wrapped parse is called';

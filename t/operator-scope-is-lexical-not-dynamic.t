use MONKEY-SEE-NO-EVAL;
use lib 't/lib';
use Test;
use OperatorScopeRunner;

# A user-declared operator is scoped to the compilation unit that declared it.
# That is a LEXICAL property of the code being executed, not a dynamic property
# of the call stack: a block compiled in this file keeps this file's operators
# even when a routine from another compilation unit is what invokes it.

plan 13;

class Word { has $.text is rw }

multi sub infix:<+> (Word $a, Word $b) { ($a.text, $b.text).join(' ') }
# Narrower than core's `Int:D + Int:D` (the `where` breaks the tie), so this
# is an unambiguous override rather than an ambiguous-dispatch error.
multi sub infix:<+> (Int:D $a, Int:D $b where $b == 3) { 999 }
sub infix:<+++> ($a, $b) { $a * 100 + $b }

my $hi = Word.new(text => 'hello');
my $to = Word.new(text => 'world');

# 1-2: the baseline -- the operator works where it was declared.
is $hi + $to, 'hello world', 'user infix applies in its own unit';
is 1 +++ 2, 102, 'user infix with a novel name applies in its own unit';

# 3-5: a block compiled HERE keeps this unit's operators, whoever calls it.
is run-block({ $hi + $to }), 'hello world',
    'user infix applies inside a block invoked by a module routine';
is run-block({ 1 +++ 2 }), 102,
    'novel user infix applies inside a block invoked by a module routine';
is run-block({ run-block({ $hi + $to }) }), 'hello world',
    'still applies two module frames deep';

# 6-7: a hyper metaop over a user-declared operator is the same question.
is (10 >>+++<< 14).join(','), '1014', 'hyper over a user infix in this unit';
is run-block({ (10 >>+++<< 14).join(',') }), '1014',
    'hyper over a user infix inside a block invoked by a module routine';

# 8-10: the converse, and the reason the gate exists at all -- the module's own
# arithmetic must NOT be intercepted by this unit's `infix:<+>`, not even for
# the argument types the module itself uses.
is module-concat('a', 'b'), 'ab', "a module's own concatenation is unaffected";
is 2 + 3, 999, "this unit's infix:<+> wins over built-in Int addition here";
is module-sum(2, 3), 5,
    "the same call inside a module keeps built-in Int addition";

# 11: EVAL compiles in its caller's lexical scope, so an operator declared in
# the enclosing unit is in scope for the EVAL'd code.
is EVAL(q[1 +++ 2]), 102, 'EVAL sees an operator from the enclosing unit';

# 12: an operator declared BY the EVAL'd code is in scope within that EVAL unit,
# even when the EVAL itself is performed by a module routine.
is module-eval(q[sub infix:<:+:> ($a, $b) { $a ~ '/' ~ $b }; 1 :+: 2]), '1/2',
    'an operator declared inside an EVAL applies within that EVAL unit';

# 13: ... including through a hyper metaop, which reaches the operator by name.
is module-eval(q[sub infix:<:+:> ($a, $b) { $a + $b }; (1 >>:+:<< 2).join(',')]), '3',
    'a hyper metaop reaches an operator declared inside the same EVAL unit';

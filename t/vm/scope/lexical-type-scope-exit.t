use Test;

# A lexically declared type (`my class`, `my package`, `my role`) and the stub
# package of a literal `require` belong to the scope that declared them: the
# name resolves inside it and is gone after it. A bare `{ ... }` block always
# took the name away; an `if`/`unless`/`else` branch, a loop body, a `given`
# body, a braced `try`, an `EVAL` and a routine's later calls leaked it (#10594).

plan 46;

sub gone($name) { ::($name).^name eq 'Failure' }

# --- branches ---------------------------------------------------------------

if 1 { my class IfC {}; is ::('IfC').^name, 'IfC', 'my class resolves inside an if body' }
ok gone('IfC'), 'my class does not outlive the if body';

unless 0 { my class UnlessC {}; is ::('UnlessC').^name, 'UnlessC', 'my class resolves inside an unless body' }
ok gone('UnlessC'), 'my class does not outlive the unless body';

if 0 { } else { my class ElseC {}; is ::('ElseC').^name, 'ElseC', 'my class resolves inside an else body' }
ok gone('ElseC'), 'my class does not outlive the else body';

if 1 { my package IfP {}; is ::('IfP').^name, 'IfP', 'my package resolves inside an if body' }
ok gone('IfP'), 'my package does not outlive the if body';

if 1 { my role IfR {}; is ::('IfR').^name, 'IfR', 'my role resolves inside an if body' }
ok gone('IfR'), 'my role does not outlive the if body';

given 1 { when 1 { my class WhenC {}; is ::('WhenC').^name, 'WhenC', 'my class resolves inside a when body' } }
ok gone('WhenC'), 'my class does not outlive the when body';

# --- loops ------------------------------------------------------------------

for 1..2 { my class ForC {}; is ::('ForC').^name, 'ForC', "my class resolves in iteration $_ of a for body" }
ok gone('ForC'), 'my class does not outlive the for loop';

my $i = 0;
while $i++ < 2 { my class WhileC {}; is ::('WhileC').^name, 'WhileC', "my class resolves in iteration $i of a while body" }
ok gone('WhileC'), 'my class does not outlive the while loop';

loop (my $j = 0; $j < 2; $j++) { my package LoopP {}; is ::('LoopP').^name, 'LoopP', "my package resolves in iteration $j of a loop body" }
ok gone('LoopP'), 'my package does not outlive the loop';

my $k = 0;
repeat { my class RepeatC {}; is ::('RepeatC').^name, 'RepeatC', 'my class resolves inside a repeat body' } while ++$k < 1;
ok gone('RepeatC'), 'my class does not outlive the repeat loop';

# --- a declaration that shadows an enclosing one gives it back ---------------

my class Shadow { method which { 'outer' } }
if 1 { my class Shadow { method which { 'inner' } }; is Shadow.which, 'inner', 'the inner class shadows the outer one' }
is Shadow.which, 'outer', 'the outer class is back after the if body';
for 1..2 { my class Shadow { method which { 'loop' } }; is Shadow.which, 'loop', 'a loop body class shadows the outer one' }
is Shadow.which, 'outer', 'the outer class is back after the loop';

# --- EVAL ---------------------------------------------------------------------

is (EVAL 'my class EvC {}; ::("EvC").^name'), 'EvC', 'a my class resolves inside the EVAL';
ok gone('EvC'), 'a my class does not outlive the EVAL';
is (EVAL 'my package EvP {}; ::("EvP").^name'), 'EvP', 'a my package resolves inside the EVAL';
ok gone('EvP'), 'a my package does not outlive the EVAL';
EVAL 'my class Shadow { method which { "eval" } }; 1';
is Shadow.which, 'outer', 'the outer class is back after an EVAL that shadowed it';

# --- a routine's calls ---------------------------------------------------------

sub with-package { my package SubP {}; ::('SubP').^name }
is with-package(), 'SubP', 'a my package resolves inside the routine';
ok gone('SubP'), 'a my package does not outlive the first call';
is with-package(), 'SubP', 'a my package resolves inside the second call';
ok gone('SubP'), 'a my package does not outlive the second call';

sub with-class { my class SubC {}; ::('SubC').^name }
with-class() for 1..3;
ok gone('SubC'), 'a my class does not outlive repeated calls';

# --- the stub of a literal `require` ---------------------------------------------

try { require ReqTry10594; CATCH { default { } } }
ok gone('ReqTry10594'), 'a require stub does not outlive a braced try';

my $inside = '';
try { $inside = ::('ReqTryIn10594').^name; require ReqTryIn10594; CATCH { default { } } }
is $inside, 'ReqTryIn10594', 'a require in a braced try declares its stub on entry';

if 1 { try require ReqIf10594; is ::('ReqIf10594').^name, 'ReqIf10594', 'a require stub resolves inside an if body' }
ok gone('ReqIf10594'), 'a require stub does not outlive the if body';

for 1..2 { try require ReqFor10594; is ::('ReqFor10594').^name, 'ReqFor10594', 'a require stub resolves inside a for body' if $_ == 1 }
ok gone('ReqFor10594'), 'a require stub does not outlive the for loop';

sub with-require { try require ReqSub10594; ::('ReqSub10594').^name }
is with-require(), 'ReqSub10594', 'a require stub resolves inside the routine';
with-require();
ok gone('ReqSub10594'), 'a require stub does not outlive the second call';

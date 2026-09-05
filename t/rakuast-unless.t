use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `unless`, both directions, plus the postfix `if`/`unless` statement modifiers.
#
# These were *wrongly rendered* rather than refused: mutsu stores `unless X` as
# `if !X` and rendered exactly that — a `Statement::If` over an
# `ApplyPrefix("!")`. raku has a `Statement::Unless` class (whose block field is
# `body`, not `then`, and which cannot carry `elsif`/`else`) and renders the
# *undecorated* condition. The postfix forms are different again: raku hangs the
# condition off the modified statement as a `condition-modifier` rather than
# building a conditional statement around it.
#
# mutsu now keeps an `is_unless` flag alongside the negated condition, exactly
# as it already did for `until`, so the source keyword is recoverable. Measured
# against rakudo 2026.07.
#
# Passes under BOTH mutsu and raku.

plan 16;

# --- read side: the block form -----------------------------------------------
my $unless = Q{unless 1 { 2 }}.AST.gist;
ok $unless.contains('RakuAST::Statement::Unless.new('),
    '`unless` renders as Statement::Unless';
nok $unless.contains('RakuAST::Prefix.new("!")'),
    "`unless` renders its condition without the parser's negation";
nok $unless.contains('RakuAST::Statement::If'), '`unless` is not an If';
ok $unless.contains('body'), 'Statement::Unless names its block `body`';

# `if` is untouched.
my $if = Q{if 1 { 2 }}.AST.gist;
ok $if.contains('RakuAST::Statement::If.new('), '`if` still renders as If';
ok $if.contains('then'), 'Statement::If still names its block `then`';
ok Q{if 1 { 2 } else { 3 }}.AST.gist.contains('else'),
    'an `if`/`else` chain still renders its else branch';
ok Q{if 1 { 2 } elsif 0 { 3 }}.AST.gist.contains('RakuAST::Statement::Elsif'),
    'an `elsif` chain still renders';

# --- read side: the postfix modifiers ----------------------------------------
my $mod-unless = Q{say 3 unless 1}.AST.gist;
ok $mod-unless.contains('RakuAST::StatementModifier::Unless.new('),
    'a postfix `unless` renders as StatementModifier::Unless';
nok $mod-unless.contains('RakuAST::Statement::Unless.new('),
    'a postfix `unless` builds no Statement::Unless around the statement';
nok $mod-unless.contains('RakuAST::Prefix.new("!")'),
    'a postfix `unless` renders its condition undecorated';

my $mod-if = Q{say 3 if 1}.AST.gist;
ok $mod-if.contains('RakuAST::StatementModifier::If.new('),
    'a postfix `if` renders as StatementModifier::If';
nok $mod-if.contains('RakuAST::Statement::If.new('),
    'a postfix `if` builds no Statement::If around the statement';

# --- write side ---------------------------------------------------------------
is EVAL(Q{my $n = 0; unless 0 { $n = 1 }; $n}.AST), 1,
    'an `unless` with a false condition lowers and runs its body';
is EVAL(Q{my $n = 0; unless 1 { $n = 1 }; $n}.AST), 0,
    'an `unless` with a true condition lowers and skips its body';
is EVAL(Q{my $n = 0; $n = 1 unless 0; $n}.AST), 1,
    'a postfix `unless` lowers and runs its statement';

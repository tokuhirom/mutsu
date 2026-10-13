use v6;
use Test;
use experimental :rakuast;

# `.=` whose method is a term, not a name: a routine (`.= &f`), a Callable in
# a variable (`.= $m`) or an interpolated quoted name (`.= "{…}"()`). Rakudo
# 2026.09 renders the method as a `Call::TermAsMethod` (even for `&f`, which
# is a `Call::NameAsMethod` after a plain `.`) or a `Call::QuotedMethod`, both
# as the `right` of an `ApplyDottyInfix` and as the call of a declaration's
# `Initializer::CallAssign`. A postfix chain after a declaration's call
# (`my $o .= $m.uc`) applies to the new value: the declaration is the
# operand of the chain. A shaped declaration keeps its `shape`.

plan 26;

sub double($x) { ($x // 1) * 2 }
sub opts(%h) { %(a => 1) }
sub pair-up($x, $n) { ($x // 0) => $n }
my $bang = method { self.defined ?? 'def' !! 'undef' };
my $up = method { 'hello' };

sub expr(Str $source, Int $index = 0) { $source.AST.statements[$index].expression }

# --- read direction: statements -------------------------------------------

my $stmt = expr(Q[my $s; $s .= &double], 1);
is $stmt.^name, 'RakuAST::ApplyDottyInfix', '`$s .= &f` is an ApplyDottyInfix';
is $stmt.right.^name, 'RakuAST::Call::TermAsMethod', 'its method is a Call::TermAsMethod';
is $stmt.right.callee.name, '&double', 'whose callee is the routine variable';
is expr(Q[my $s; $s .= $bang], 1).right.callee.name, '$bang',
    '`$s .= $m`: the callee is the variable';
is expr(Q[my $s; $s .= &pair-up(3)], 1).right.args.args.elems, 1,
    'the arguments stay on the call';
is expr(Q[my Int $x; $x .= "{"new"}"(42)], 1).right.^name, 'RakuAST::Call::QuotedMethod',
    'an interpolated quoted name is a Call::QuotedMethod';

# --- read direction: declarations -----------------------------------------

my $decl = expr(Q[my %h .= &double]);
is $decl.^name, 'RakuAST::VarDeclaration::Simple', '`my %h .= &f` is a declaration';
is $decl.initializer.^name, 'RakuAST::Initializer::CallAssign', 'with a CallAssign initializer';
is $decl.initializer.postfixish.^name, 'RakuAST::Call::TermAsMethod',
    'whose call is a Call::TermAsMethod';
is $decl.initializer.postfixish.callee.name, '&double', 'over the routine variable';

my $chained = expr(Q[my $o .= $up.uc.lc]);
is $chained.^name, 'RakuAST::ApplyPostfix', 'a chain after the call is an ApplyPostfix';
is $chained.postfix.name.canonicalize, 'lc', 'the outer postfix is the last call';
is $chained.operand.operand.^name, 'RakuAST::VarDeclaration::Simple',
    'and the declaration is the innermost operand';
is $chained.operand.operand.initializer.postfixish.callee.name, '$up',
    'which keeps its CallAssign call';

my $shaped = expr(Q[my @c[2;2] .= new(:shape(2,2), <a b>, <c d>)]);
is $shaped.shape.^name, 'RakuAST::SemiList', 'a shaped `.=` declaration keeps its shape';
is $shaped.initializer.postfixish.name.canonicalize, 'new', 'and its method call';

# --- write direction (EVAL) and semantics ----------------------------------

is Q[my $s = 3; $s .= &double; $s].AST.EVAL, 6, '`$s .= &f` calls the routine';
is Q[my $m = method { self ~ '!' }; my $q = 'a'; $q .= $m; $q].AST.EVAL, 'a!',
    '`$q .= $m` calls the Callable with the variable as invocant';
is Q[my $p = 1; $p .= &pair-up(3); $p].AST.EVAL, 1 => 3, 'with arguments';
is Q[my Int $x; $x .= "{"new"}"(42); $x].AST.EVAL, 42, 'an interpolated quoted name';
is Q[my %h .= &opts; %h].AST.EVAL, {a => 1}, '`my %h .= &f` assigns the routine\'s result';
is Q[my $n .= &double; $n].AST.EVAL, 2, '`my $n .= &f` declares and calls';
is Q[my $fresh .= $bang; $fresh].AST.EVAL, 'undef', '`my $q .= $m` sees the fresh variable';

my @log;
is Q[my $o .= $up.uc.&{ @log.push($_); $_ }; $o].AST.EVAL, 'hello',
    'the chain does not change the declared value';
is-deeply @log, ['HELLO'], 'but it runs on the new value';
is Q[my @c[2;2] .= new(:shape(2,2), <a b>, <c d>); @c[1;0]].AST.EVAL, 'c',
    'the shaped declaration builds its container';

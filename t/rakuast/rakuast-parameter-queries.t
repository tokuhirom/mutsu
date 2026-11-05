use Test;

# `RakuAST::Parameter`'s questions about itself (`named`, `is-positional`,
# `is-optional`, `multi-invocant`), measured on rakudo 2026.09 (#14916).

plan 22;

use experimental :rakuast;

sub q(Str $src) {
    my @p = $src.AST.statements[0].expression.signature.parameters;
    @p.map({ "{.named.Int}{.is-positional.Int}{.is-optional.Int}{.multi-invocant.Int}" }).join(",")
}

is q('sub a($a) { }'), "0101", 'a plain positional';
is q('sub b($b?) { }'), "0111", 'an optional positional';
is q('sub c($c = 3) { }'), "0111", 'a positional with a default';
is q('sub d(:$d) { }'), "1011", 'a named parameter is optional';
is q('sub e(:$e!) { }'), "1001", 'a required named parameter';
is q('sub f(*@f) { }'), "0001", 'a slurpy array';
is q('sub g(*%g) { }'), "0001", 'a slurpy hash is not named';
is q('sub h(|c) { }'), "0001", 'a capture';
is q('sub i(\j) { }'), "0101", 'a sigilless parameter';
is q('sub k(:$k = 2) { }'), "1011", 'a named parameter with a default';
is q('sub l(:$l?) { }'), "1011", 'a named parameter with ?';
is q('sub o(:$o is required) { }'), "1001", 'is required';
is q('sub n(@n) { }'), "0101", 'an array parameter';
is q('sub r(:s($t)) { }'), "1011", 'a renamed named parameter';
is q('sub u(:$v, :$w) { }'), "1011,1011", 'two named parameters';
is q('sub x($y is copy) { }'), "0101", 'a trait';
is q('multi sub m($x, $y;; $z) { }'), "0101,0101,0100", 'after ;; a parameter is not a multi invocant';
is q('multi sub m2(Int $x, Str $y) { }'), "0101,0101", 'typed multi parameters';

# Hand-built parameters.
my $named = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$x'), names => ('x',));
is $named.named, True, 'hand-built: named';
is $named.is-positional, False, 'hand-built: not positional';
my $opt = RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$y'), :optional);
is $opt.is-optional, True, 'hand-built: optional';
is RakuAST::Parameter.new(
    target => RakuAST::ParameterTarget::Var.new(name => '$z')).multi-invocant, True,
    'hand-built: multi-invocant by default';

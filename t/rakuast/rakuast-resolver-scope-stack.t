use Test;
use experimental :rakuast;
use nqp;

# A CHECK-time pass over `$*CU` (Red's `red-compile`, #17501): a clone of the
# unit's resolver pushes the scopes around a node, resolves lexicals there,
# resolves a node it built (`IMPL-BEGIN` / `IMPL-CHECK`) and rewrites an
# operand (`set-operands`); the unit then runs the rewritten tree.

plan 12;

sub marker() is export { 1 }
sub wrap($x) { "wrapped($x)" }
my $outer = 42;
my @seen;

CHECK {
    my $cu = $*CU;
    my $resolver = $cu.resolver.clone;
    my @scopes;
    my $target;
    sub walk($n, @s) {
        my @nested = @s;
        @nested.push: $n if $n ~~ RakuAST::LexicalScope;
        if $n.^name eq 'RakuAST::Var::Lexical' && $n.name eq '$inner' {
            @scopes = @nested;
        }
        if $n.^name eq 'RakuAST::ApplyPostfix'
            && $n.postfix.^name eq 'RakuAST::Call::Method'
            && $n.postfix.name.canonicalize eq 'flip' {
            $target = $n;
        }
        $n.visit-children(-> $c { walk($c, @nested) });
    }
    walk($cu, []);
    @seen.push: @scopes.map(*.^name).join(' ');
    $resolver.push-scope($_) for @scopes;
    @seen.push: so $resolver.resolve-lexical('&marker');
    @seen.push: so $resolver.resolve-lexical('&nope');
    @seen.push: $resolver.resolve-lexical('$inner').^name;
    @seen.push: $resolver.resolve-lexical('$outer').^name;
    my $var = RakuAST::Var::Lexical.new('$inner');
    $var.IMPL-BEGIN($resolver, $cu.context);
    $var.IMPL-CHECK($resolver, $cu.context);
    @seen.push: $var.is-resolved;
    @seen.push: $var.resolution.^name;
    $resolver.pop-scope for @scopes;
    @seen.push: so $resolver.resolve-lexical('$inner');
    # `"abc".flip` becomes `wrap("abc").flip`.
    my $call = RakuAST::Call::Name.new(
        name => RakuAST::Name.from-identifier('wrap'),
        args => RakuAST::ArgList.new($target.operand));
    $resolver.push-scope($cu);
    $call.IMPL-BEGIN($resolver, $cu.context);
    $call.IMPL-CHECK($resolver, $cu.context);
    $resolver.pop-scope;
    $target.set-operands(nqp::list($call));
}

is @seen[0], 'RakuAST::CompUnit RakuAST::Block', 'the scopes around the lookup';
ok @seen[1], 'an exported sub of the unit resolves';
nok @seen[2], 'an undeclared routine does not';
is @seen[3], 'RakuAST::VarDeclaration::Simple', 'a variable of a pushed block scope';
is @seen[4], 'RakuAST::VarDeclaration::Simple', 'a variable of the unit scope';
ok @seen[5], 'IMPL-CHECK resolves a node built at CHECK time';
is @seen[6], 'RakuAST::VarDeclaration::Simple', 'to the declaration in the pushed scope';
nok @seen[7], 'a popped scope no longer resolves';

for 1 { my $inner = 3; is $inner, 3, 'the block runs' }
is "abc".flip, ')cba(depparw', 'the rewritten operand runs';

my $p = RakuAST::ApplyPostfix.new(
    operand => RakuAST::IntLiteral.new(1),
    postfix => RakuAST::Call::Method.new(name => RakuAST::Name.from-identifier('succ')));
$p.set-operands(nqp::list(RakuAST::IntLiteral.new(41)));
is $p.operand.value, 41, 'set-operands replaces the operand in place';
is EVAL($p), 42, 'and the node evaluates with it';

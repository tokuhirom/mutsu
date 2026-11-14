use lib 't/lib';
use experimental :rakuast;
use Test;

# A CHECK-time pass that pushes a block's scope onto a cloned resolver
# (`$resolver.push-scope`) finds the routines a `use` inside that block
# imported, and only there. Red's `red-compile` asks
# `resolve-lexical('&__RED_OPERATOR_LOADED__')` this way to decide whether a
# compiled `.map` may use Red's operators (Red t/092-rakuast-scope).

plan 6;

my @found;
CHECK {
    my $resolver = $*CU.resolver.clone;
    sub walk($node, @scopes) {
        my @nested = @scopes;
        @nested.push: $node if $node ~~ RakuAST::LexicalScope;
        if $node ~~ RakuAST::Call::Name && $node.name.canonicalize eq 'probe' {
            $resolver.push-scope($_) for @nested;
            my $hook = $resolver.resolve-lexical('&resolver-hook-mark');
            my $trait = $resolver.resolve-lexical('&resolver-trait-mark');
            @found.push: [so($hook), so($trait), $hook.^name];
            $resolver.pop-scope for @nested;
        }
        $node.visit-children(-> $c { walk($c, @nested) });
    }
    walk($*CU, []);
}

sub probe { }
{
    use ResolverBlockHookImport;
    use ResolverBlockTraitImport;
    probe;
}
probe;

is @found.elems, 2, 'both probe sites were visited';
ok @found[0][0], 'a `sub EXPORT` import resolves in the block that uses it';
ok @found[0][1], 'an `is export` import resolves in the block that uses it';
is @found[0][2], 'RakuAST::Declaration::Import', 'it resolves to an import declaration';
nok @found[1][0], 'the hook import does not resolve outside the block';
nok @found[1][1], 'the trait import does not resolve outside the block';

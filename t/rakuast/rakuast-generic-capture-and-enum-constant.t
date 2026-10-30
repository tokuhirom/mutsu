use Test;

# static::Analyzer reads `::T` captures and core enum values at CHECK time
# (#15610). Expected values are rakudo 2026.09's.

plan 7;

sub f(::T $x --> T) { $x }
my Bool $b = True;

my (@captures, @constants);
CHECK {
    my $sub = $*CU.statement-list.statements.grep(*.can("expression")).map(*.expression).grep(RakuAST::Sub)[0];
    @captures.push: $sub.signature.parameters[0].type-captures[0].compile-time-value;
    @captures.push: $sub.compile-time-value.signature.returns;
    sub walk($node) {
        return unless $node ~~ RakuAST::Node;
        @constants.push: $node.resolution.compile-time-value if $node ~~ RakuAST::Term::Enum && $node.is-resolved;
        $node.visit-children({ walk($_) });
    }
    walk($*CU);
}

is @captures[0].^name, 'T', 'a type capture is the generic type T';
ok @captures[0] === @captures[1], '... the same type a signature returns with --> T';
ok @captures[0].^archetypes.generic, '... and it is generic';
nok Int.^archetypes.generic, 'an ordinary type is not generic';
ok @constants.elems >= 1, 'a core enum value resolves';
ok @constants.grep(* === True), 'True resolves to the value True';
ok RakuAST::Type::Capture.^can('compile-time-value'), 'Type::Capture can compile-time-value';

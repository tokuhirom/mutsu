use Test;
use experimental :rakuast;

# A `<name>` assertion looks `&name` up lexically (`get-implicit-lookups`): a
# `my regex` / `token` in scope is its resolution, a builtin rule's lookup
# stays unresolved. Red's regex compiler (#17501) inlines the rule it finds.

plan 14;

my regex foo { a }
my token bar($x) { b }

my @seen;
CHECK {
    sub walk($n) {
        if $n.^name.starts-with('RakuAST::Regex::Assertion::Named') {
            for $n.get-implicit-lookups -> $l {
                @seen.push: join ' ', $n.^name, $l.^name, $l.name, $l.is-resolved,
                    ($l.is-resolved ?? $l.resolution.^name !! '-');
            }
        }
        $n.visit-children(&walk);
    }
    walk($*CU);
}
my $m = 'ab' ~~ /<foo> <bar(1)> <ident>?/;

is @seen[0], 'RakuAST::Regex::Assertion::Named RakuAST::Var::Lexical &foo True RakuAST::RegexDeclaration',
    'a my regex resolves';
is @seen[1], 'RakuAST::Regex::Assertion::Named::Args RakuAST::Var::Lexical &bar True RakuAST::TokenDeclaration',
    'a my token with arguments resolves';
is @seen[2], 'RakuAST::Regex::Assertion::Named RakuAST::Var::Lexical &ident False -',
    'a builtin rule stays unresolved';
is ~$m, 'ab', 'and the regex matches';

# `Str.AST` resolves them too.
my $q = q[my regex pair { a a }; /<?pair> a/].AST.statements[*-1].expression;
my @lookups;
sub collect($n) {
    @lookups.append: $n.get-implicit-lookups
        if $n.^name.starts-with('RakuAST::Regex::Assertion::Named');
    $n.visit-children(&collect);
}
collect($q);
is @lookups.elems, 1, 'one lookup';
ok @lookups[0].is-resolved, 'resolved in a Str.AST tree';
is @lookups[0].resolution.^name, 'RakuAST::RegexDeclaration', 'to the my regex';

# Its compile-time value matches through the rule.
my $r = $q.compile-time-value;
is ~('aa' ~~ $r), 'a', 'the lexical rule is what <?pair> calls';
nok 'ab' ~~ $r, 'and it fails where the rule does';

my $unresolved = q[/<alpha>/].AST.statements[0].expression.body;
is $unresolved.get-implicit-lookups.map(*.is-resolved).join(','), 'False',
    'an unresolved lookup is still listed';

# `rx//` has a compile-time value too.
my $rx = q[rx:i/a/].AST.statements[0].expression;
is ~('A' ~~ $rx.compile-time-value), 'A', 'rx:i// compile-time value';

my $p = q[-> $x {}].AST.statements[0].expression.signature.parameters[0];
nok $p.signature-constraint.DEFINITE, 'a plain parameter has no signature constraint';
is $p.signature-constraint.^name, 'RakuAST::Signature', 'it answers the Signature type object';
ok RakuAST::Parameter.^can('signature-constraint'), 'Parameter can signature-constraint';

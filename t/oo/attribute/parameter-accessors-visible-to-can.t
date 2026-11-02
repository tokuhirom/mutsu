use Test;

# #15660: the accessors of a Parameter are methods Rakudo reports through
# `.can`, `.^can` and `.^methods`; calling them already worked.

my @names = <capture constraint_list constraints copy default invocant modifier
             multi-invocant name named named_names optional positional prefix raw
             readonly rw sigil signature_constraint slurpy sub_signature suffix
             twigil type type_captures usage-name>;
plan 3 + 2 * @names;

sub f(Int $x where * > 0, :$y!, *@z) {}
my $p = &f.signature.params[0];

for @names -> $m {
    is $p.can($m).elems, 1, "an instance can $m";
    ok Parameter.^can($m).elems >= 1, "the type object can $m";
}

is $p.can("no-such-parameter-method").elems, 0, 'an unknown name is not reported';
is $p.name, '$x', 'the accessor still answers';
my %declared = Parameter.^methods.map(*.name).Set;
ok %declared<name> && %declared<signature_constraint>, '^methods lists the accessors';

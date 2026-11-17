use experimental :rakuast;
use Test;

# A scope's `multi` candidates of a name an outer scope (or the setting)
# already has a dispatcher for join that family: rakudo derives a proto holding
# the outer candidates and the scope's own, and a lookup of the name inside the
# scope resolves to a `RakuAST::VarDeclaration::Implicit::Constant` whose value
# is that proto (#17826). Asked at CHECK time, before the block runs.

plan 14;

my %seen;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Call::Name && $n.name.canonicalize eq 'fam' {
            my $r = $n.resolution;
            my $code = $r.compile-time-value;
            %seen{$n.args.args[0].segments[0].value} = %(
                class => $r.^name,
                name  => $r.name,
                elems => $code.candidates.elems,
                same  => $r.value === $code || $r.value.candidates.elems == $code.candidates.elems,
                str   => $code('x'),
                int   => $code(1),
                rat   => ($code(1.5) if $code.candidates.elems > 2),
            );
        }
        if $n ~~ RakuAST::Infix && $n.operator eq '+' && $n.is-resolved
            && $n.resolution ~~ RakuAST::VarDeclaration::Implicit::Constant {
            my $code = $n.resolution.compile-time-value;
            %seen<op> = %(
                user    => $code('a', 1),
                setting => $code(1, 2),
            );
        }
        $n.visit-children(&walk);
    }
    walk($*CU);
}

multi fam(Int $x) { 'int' }
{
    multi fam(Str $x) { 'str' }
    is fam('outer'), 'str', 'the block candidate answers at run time';
    {
        multi fam(Rat $x) { 'rat' }
        is fam('inner'), 'str', 'and the outer ones in a nested block';
    }
}
{
    multi infix:<+>(Str $l, Int $r) { 100 + $r }
    is 'a' + 1, 101, 'a block-local operator candidate at run time';
}

is %seen<outer><class>, 'RakuAST::VarDeclaration::Implicit::Constant',
    'a block candidate of an outer family resolves to the derived proto';
is %seen<outer><name>, '&fam', 'which is named by the routine name';
is %seen<outer><elems>, 2, 'its value holds the outer and the block candidates';
is %seen<outer><str>, 'str', 'it dispatches to the block candidate';
is %seen<outer><int>, 'int', 'and to the outer one';
ok %seen<outer><same>, '.value is the same family';
is %seen<inner><class>, 'RakuAST::VarDeclaration::Implicit::Constant',
    'a nested block derives one too';
is %seen<inner><elems>, 3, 'holding every enclosing scope\'s candidates';
is %seen<inner><rat>, 'rat', 'including its own';
is %seen<op><user>, 101, 'a block operator candidate joins the setting operator';
is %seen<op><setting>, 3, 'which still answers the setting\'s calls';

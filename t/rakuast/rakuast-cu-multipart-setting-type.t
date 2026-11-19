use Test;

plan 6;

# In `$*CU`, a multi-part setting type name is a `Declaration::ResolvedConstant`
# holding the type object; a single-part one stays `External::Constant`
# (identical under rakudo).
my %kind;
my %value;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Type::Simple {
            my $name = $n.name.canonicalize;
            %kind{$name} = $n.resolution.^name;
            %value{$name} = $n.compile-time-value.^name;
        }
        $n.visit-children(&walk) if $n.^can("visit-children");
    }
    walk($*CU);
}
my $a = RakuAST::Lookup;
my $b = X::AdHoc;
my $c = Metamodel::ClassHOW;
my $d = Int;

is %kind<RakuAST::Lookup>, 'RakuAST::Declaration::ResolvedConstant', 'package-qualified name';
is %kind<X::AdHoc>, 'RakuAST::Declaration::ResolvedConstant', 'exception class name';
is %kind<Metamodel::ClassHOW>, 'RakuAST::Declaration::ResolvedConstant', 'aliased metamodel name';
is %kind<Int>, 'RakuAST::Declaration::External::Constant', 'single-part name is external';
is %value<RakuAST::Lookup>, 'RakuAST::Lookup', 'its compile-time value is the type object';
is %value<Metamodel::ClassHOW>, 'Perl6::Metamodel::ClassHOW', 'an alias answers the real class';

use Test;

plan 5;

# In `$*CU`, `CORE::Int` is the setting's own `Int`: an `External::Constant`
# named `Int` (identical under rakudo).
my %kind;
my %lexical;
my %value;
CHECK {
    sub walk($n) {
        if $n ~~ RakuAST::Type::Simple {
            my $name = $n.name.canonicalize;
            %kind{$name} = $n.resolution.^name;
            %lexical{$name} = $n.resolution.lexical-name;
            %value{$name} = $n.compile-time-value.^name;
        }
        $n.visit-children(&walk) if $n.^can("visit-children");
    }
    walk($*CU);
}
my $a = CORE::Int;
my $b = CORE::Str;

is %kind<CORE::Int>, 'RakuAST::Declaration::External::Constant', 'CORE::Int is an external constant';
is %lexical<CORE::Int>, 'Int', 'named by its bare name';
is %value<CORE::Int>, 'Int', 'whose value is the type object';
is %kind<CORE::Str>, 'RakuAST::Declaration::External::Constant', 'so is CORE::Str';
is %value<CORE::Str>, 'Str', 'with its own type object';

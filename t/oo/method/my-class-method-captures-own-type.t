use Test;

plan 7;

constant RIS = Int;
my $shadowed;
{
    my class RIS {
        method me { RIS.^name }
    }
    is RIS.new.me, 'RIS', 'the method sees the lexical class while its block is active';
    $shadowed = RIS.new;
}
is $shadowed.me, 'RIS', 'the method keeps its own type after the declaring block exits';
is RIS.^name, 'Int', 'the outer constant is still visible after the block';

my $unshadowed;
{
    my class Plain {
        method me { Plain.^name }
    }
    $unshadowed = Plain.new;
}
is $unshadowed.me, 'Plain', 'an unshadowed lexical class name is captured too';

my $role_instance;
{
    my role RR {
        method me { RR.^name }
    }
    my class UsesRR does RR { }
    $role_instance = UsesRR.new;
}
is $role_instance.me, 'RR', 'a composed role method keeps its lexical role name';

my $module_instance;
module LocalType {
    my class C {
        method me { C.^name }
    }
    $module_instance = C.new;
}
is $module_instance.me, 'LocalType::C',
    'a bare lexical type name remains available after unit-package qualification';

my $compound_instance;
{
    my class X::C {
        method me { X::C.^name }
    }
    $compound_instance = X::C.new;
}
is $compound_instance.me, 'X::C', 'an explicitly qualified lexical type keeps its written name';

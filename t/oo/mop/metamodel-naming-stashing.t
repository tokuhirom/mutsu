use Test;

# `Metamodel::Naming` and `Metamodel::Stashing` are real composable metaroles:
# a custom HOW does them to become a named metaobject. The worked example is
# `Type/Metamodel/Stashing.rakudoc:45`. Every assertion below was verified
# against Rakudo, and this file passes under `raku` unmodified.

plan 14;

class WithStashHOW
    does Metamodel::Naming
    does Metamodel::Stashing
{
    method new_type(WithStashHOW:_: Str:D :$name! --> Mu) {
        my WithStashHOW:D $meta := self.new;
        my Mu             $type := Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
        $meta.set_name: $type, $name;
        self.add_stash: $type
    }
}

ok WithStashHOW.^does(Metamodel::Naming),   'a custom HOW can compose Metamodel::Naming';
ok WithStashHOW.^does(Metamodel::Stashing), 'a custom HOW can compose Metamodel::Stashing';

my Mu constant WithStash = WithStashHOW.new_type: :name<WithStash>;

is WithStash.WHO.Str, 'WithStash', 'the stashed type answers its stash';
is WithStash.WHO.^name, 'Stash',   'and that stash is a Stash';
is WithStash.^name, 'WithStash',   '.^name asks the type its own metaobject';

# The name is state on the METAOBJECT, not on the type: `.^name` is
# `$type.HOW.name($type)`, so a *different* instance of the same HOW class was
# never told the name and answers the empty string.
is WithStashHOW.new.name(WithStash), '',
   'a fresh metaobject of the same HOW class knows no name';

# An un-named create_type type object answers the empty string, not a
# placeholder -- its metaobject simply has nothing recorded.
my $anon := Metamodel::Primitives.create_type(WithStashHOW.new, 'Uninstantiable');
is WithStashHOW.new.name($anon), '', 'an un-named type object has no name';

# set_name/name round-trip on one metaobject.
my $meta := WithStashHOW.new;
my $late := Metamodel::Primitives.create_type($meta, 'Uninstantiable');
is $meta.name($late), '', 'the metaobject starts un-named';
$meta.set_name($late, 'Later');
is $meta.name($late), 'Later', 'set_name records the name on the metaobject';
is $late.^name, 'Later', 'and .^name reads it back off the type';

# add_stash answers the type object it was handed -- the documented HOW ends
# `new_type` with it, so returning anything else breaks the whole idiom.
my $stashed := $meta.add_stash($late);
is $stashed.^name, 'Later', 'add_stash answers the type object';

# Naming is composable on its own, without Stashing.
class NamingOnlyHOW does Metamodel::Naming { }
ok NamingOnlyHOW.^does(Metamodel::Naming), 'Metamodel::Naming composes on its own';
my $n := NamingOnlyHOW.new;
my $nt := Metamodel::Primitives.create_type($n, 'Uninstantiable');
$n.set_name($nt, 'JustNamed');
is $nt.^name, 'JustNamed', 'a Naming-only HOW names its type';

# Ordinary types are untouched by any of this.
is Int.^name, 'Int', 'a builtin type still reports its own name';

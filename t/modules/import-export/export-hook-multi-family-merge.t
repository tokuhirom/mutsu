use Test;
use lib 't/lib';

# A `multi` a `sub EXPORT` hook hands out in its Map and a same-named `multi`
# an `is export` import brings are one family, whichever `use` comes first
# (#17828): Red::Driver::Pg's `t/schema-constraints` uses `Red:api<2>` (the
# hook) and `Red::Driver::Pg::Schema` (more `trait_mod:<is>` candidates).

plan 8;

{
    use HookFamilyHook;
    use HookFamilyOther;
    is hff(1), 'int', 'hook first: the hook\'s candidate';
    is hff('a'), 'str', 'hook first: the other import\'s candidate';
    my $attr = Attribute.new(:name('$!x'), :package(Int), :type(Mu));
    my $*HFF-SEEN;
    trait_mod:<is>($attr, :hff-index(\({ 1 }, :using<gin>)));
    is $*HFF-SEEN, 'index using', 'hook first: an operator family keeps the hook\'s candidate';
    trait_mod:<is>($attr, :hff-exclude(\({ 1 })));
    is $*HFF-SEEN, 'exclude', 'hook first: and the other import\'s';
}
{
    use HookFamilyOther;
    use HookFamilyHook;
    is hff(1), 'int', 'hook last: the hook\'s candidate';
    is hff('a'), 'str', 'hook last: the other import\'s candidate';
    my $attr = Attribute.new(:name('$!x'), :package(Int), :type(Mu));
    my $*HFF-SEEN;
    trait_mod:<is>($attr, :hff-index(\({ 1 }, :using<gin>)));
    is $*HFF-SEEN, 'index using', 'hook last: an operator family keeps the hook\'s candidate';
    trait_mod:<is>($attr, :hff-exclude(\({ 1 })));
    is $*HFF-SEEN, 'exclude', 'hook last: and the other import\'s';
}

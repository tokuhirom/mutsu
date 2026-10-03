use Test;

# A role body runs at composition, in the composing scope, but its `:=` source
# is the variable in scope where the ROLE was declared (#11087). A composer
# with its own same-named variable must neither become the bind source nor be
# clobbered by the bind.

plan 6;

my $r = 1;
role FromRoutine { my $w := $r; method m { $w } }

sub compose-shadowed {
    my $r = 99;
    my class Shadowed does FromRoutine { };
    Shadowed.new.m ~ ',' ~ $r
}
is compose-shadowed(), '1,99', "the composer's own same-named variable is not the bind source";
$r = 4;
is compose-shadowed(), '4,99', '... and the bound name keeps following the outer variable';
is $r, 4, 'composition leaves the outer variable alone';

sub compose-then-write {
    my $r = 10;
    my class Writer does FromRoutine { };
    $r++;
    Writer.new.m ~ ',' ~ $r
}
is compose-then-write(), '4,11', "the composer's variable stays writable after composition";

my $y = 1;
role SigillessShadow { my \x := $y; method m { x }; method bump { x = x + 1 } }
sub compose-sigilless {
    my $y = 50;
    my class S does SigillessShadow { };
    S.new.bump;
    S.new.m ~ ',' ~ $y
}
is compose-sigilless(), '2,50', 'a sigilless bind reaches the declaration-site variable, not the composer';
is $y, 2, '... and the write through it lands there';

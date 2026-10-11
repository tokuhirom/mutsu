unit class DoesRoleInitUnitClass;

my role Parsed { has $.getopt is required; }

multi get-from-sub(&c) { 'plain' }
multi get-from-sub(&c where Parsed) { 'parsed' }

multi sub trait_mod:<is>(Sub $sub, Bool :$getopt!) is export(:DEFAULT, :traits) {
    my $x = get-from-sub($sub);
    return $sub does Parsed($x);
}

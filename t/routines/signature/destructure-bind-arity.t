use Test;

# A declarator list bound with `:=` is a signature (#9763): a positional count
# outside its required..max range dies with rakudo's "Too few/Too many
# positionals passed", and an optional element the RHS does not reach takes
# its default. List assignment (`=`) stays lenient.

plan 16;

throws-like 'my ($p, $q) := (1,)', X::AdHoc,
    message => /'Too few positionals passed' .* 'expected 2 arguments but got 1'/,
    'too few elements for two required targets';
throws-like 'my ($a) := (1, 2)', X::AdHoc,
    message => /'Too many positionals passed' .* 'expected 1 argument but got 2'/,
    'too many elements for one target';
throws-like 'my ($a) := ()', X::AdHoc,
    message => /'expected 1 argument but got 0'/,
    'an empty RHS for a required target';
throws-like 'my ($a, $b?) := (1, 2, 3)', X::AdHoc,
    message => /'Too many positionals passed' .* 'expected 1 or 2 arguments but got 3'/,
    'an optional element widens the bound to "1 or 2"';
throws-like 'my ($a, $b?, $c?) := (1, 2, 3, 4)', X::AdHoc,
    message => /'expected 1 to 3 arguments but got 4'/,
    'two optional elements give a "1 to 3" bound';
throws-like 'my ($a, $b = 2) := ()', X::AdHoc,
    message => /'Too few positionals passed' .* 'expected 1 or 2 arguments but got 0'/,
    'a defaulted element is optional';
throws-like 'my ($a, $b, *@r) := (1,)', X::AdHoc,
    message => /'expected at least 2 arguments but got only 1'/,
    'a slurpy makes the bound open-ended';
throws-like 'my (\a, \b) := (1,)', X::AdHoc,
    message => /'Too few positionals passed'/,
    'sigilless targets are checked too';

{
    my ($x, $y?) := (1,);
    is $y.raku, 'Mu', 'an unfilled untyped optional element is Mu';
}
{
    my ($m, Int $n?) := (1,);
    is $n.raku, 'Int', 'an unfilled typed optional element is its type object';
}
{
    my ($a, $b = 5) := (1,);
    is $b, 5, 'an unfilled defaulted element takes its default';
}
{
    my ($a, $b = 5) := (1, 7);
    is $b, 7, 'a filled defaulted element takes the RHS value';
}
{
    my ($c, *@r) := (1, 2, 3);
    is-deeply @r, [2, 3], 'a slurpy takes the surplus';
}
{
    my ($a, *@r) := (1..*);
    is $a, 1, 'a lazy RHS feeding a slurpy is not reified by the check';
}
{
    my ($a, $b) = (1,);
    ok !$b.defined, 'list assignment stays lenient about a short RHS';
    my ($c) = (1, 2);
    is $c, 1, 'list assignment stays lenient about a long RHS';
}

use Test;

plan 4;

class Words {
    has %.values is rw = <a b c d>;
}

my $words = Words.new;
is-deeply $words.values, %(a => 'b', c => 'd'),
    'a hash attribute default is contextualized as pairs';

$words.values = <Z Y X W>;
is $words.values.^name, 'Hash',
    'list assignment through a rw hash accessor preserves Hash type';
is-deeply $words.values, %(Z => 'Y', X => 'W'),
    'list assignment through a rw hash accessor coerces alternating pairs';

$words.values = (left => 1, right => 2);
is-deeply $words.values, %(left => 1, right => 2),
    'Pair assignment through a rw hash accessor remains associative';

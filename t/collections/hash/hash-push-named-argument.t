use Test;

plan 5;

my %decl .= push(e => 6);
is-deeply %decl, {}, 'named Pair is not captured by Hash.push in a declaration';

my %hash = a => 1;
%hash.push(b => 2);
is-deeply %hash, { a => 1 }, 'named Pair is not captured by Hash.push';

%hash.append(c => 3);
is-deeply %hash, { a => 1 }, 'named Pair is not captured by Hash.append';

%hash.push('b' => 2);
is-deeply %hash, { a => 1, b => 2 }, 'quoted-key Pair remains positional data';

%hash.push((c => 3));
is-deeply %hash, { a => 1, b => 2, c => 3 }, 'parenthesized Pair remains positional data';

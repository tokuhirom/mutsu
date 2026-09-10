use v6;
use Test;

# Signature ~~ Signature must honour the built-in parametric role hierarchy:
# `:(Pair)` is narrower than `:(Associative)` because a Pair does Associative.
# (raku-doc Language/functions.rakudoc)
ok :( Pair ) ~~ :( Associative ), 'Pair signature matches Associative signature';
nok :( Pair ) ~~ :( Hash ),       'Pair signature does not match Hash signature';

ok  :( Array ) ~~ :( Positional ), 'Array does Positional';
ok  :( List )  ~~ :( Positional ), 'List does Positional';
nok :( Seq )   ~~ :( Positional ), 'Seq does NOT do Positional';
ok  :( Seq )   ~~ :( Iterable ),   'Seq does Iterable';
ok  :( Range ) ~~ :( Iterable ),   'Range does Iterable';
ok  :( Hash )  ~~ :( Associative ),'Hash does Associative';
ok  :( Set )   ~~ :( Associative ),'Set does Associative';

ok  :( Sub )    ~~ :( Callable ), 'Sub does Callable';
ok  :( Method ) ~~ :( Callable ), 'Method does Callable';

# The pre-existing class hierarchy still holds.
ok  :( Int ) ~~ :( Cool ),    'Int is a Cool';
ok  :( Int ) ~~ :( Numeric ), 'Int is Numeric';
nok :( Int ) ~~ :( Str ),     'Int is not a Str';

# Unrelated roles must NOT match.
nok :( Pair )  ~~ :( Positional ), 'Pair is not Positional';
nok :( Array ) ~~ :( Associative ),'Array is not Associative';

done-testing;

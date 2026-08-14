use v6;
use Test;

# `Nil` must never smart-match `UInt`: `type_matches_value`'s UInt branch had
# a stray `Nil => true` arm (added for a since-superseded TypeCheck path —
# assignment of Nil to a typed variable is already handled generically by a
# `!value.is_nil()` guard before `type_matches_value` is even called), which
# made `given $obj { when UInt { ... } }` wrongly classify a Nil value as
# UInt. Blocked Mathematica::Serializer::Encoder's `Nil`-to-`NULL` dispatch.

plan 3;

nok Nil ~~ UInt, 'Nil does not smart-match UInt';

my $classified = do given Nil {
    when UInt { 'UInt' }
    when Any  { 'Any' }
    default   { 'other' }
};
is $classified, 'Any', 'given/when does not misclassify Nil as UInt';

my UInt $u;
$u = Nil;
is-deeply $u, UInt, 'assigning Nil to a UInt-typed variable still resets to the type object';

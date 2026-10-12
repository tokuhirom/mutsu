use Test;

# #12971: `min`, `max` and `minmax` on a lazy list raise X::Cannot::Lazy, as in
# Rakudo, even when the list is finite (`.lazy` marks it lazy). A Range keeps
# answering from its bounds.

plan 9;

throws-like { (1..3).lazy.min }, X::Cannot::Lazy, message => 'Cannot .min a lazy list', '.lazy.min';
throws-like { (1..3).lazy.max }, X::Cannot::Lazy, message => 'Cannot .max a lazy list', '.lazy.max';
throws-like { (1..3).lazy.minmax }, X::Cannot::Lazy, message => 'Cannot .minmax a lazy list', '.lazy.minmax';
throws-like { (1, 2 ... *).min }, X::Cannot::Lazy, 'an infinite sequence';
is (1..*).min, 1, 'an infinite Range answers its bound';
is (1..5).max, 5, 'Range.max';
is (3, 1, 2).min, 1, 'an eager List.min';
is (1, 2, 3).map(* + 1).max, 4, 'an eager Seq.max';
is-deeply (4, 2, 9).minmax, 2..9, 'an eager List.minmax';

use Test;

plan 7;

throws-like { True.pick }, X::Multi::NoMatch, 'defined Bool.pick dies';
throws-like { False.roll }, X::Multi::NoMatch, 'defined Bool.roll dies';
throws-like { True.pick(2) }, X::Multi::NoMatch, 'defined Bool.pick(2) dies';
throws-like { 3.base-repeating(2) }, X::Method::NotFound, 'Int has no base-repeating';
throws-like { 2.5e0.base-repeating }, X::Method::NotFound, 'Num has no base-repeating';
is (1/3).base-repeating(3).raku, ("0.1", "").raku, 'Rat.base-repeating still works';
ok Bool.pick.defined, 'Bool:U.pick still answers';

use Test;
use lib 't/lib';
use SubsetHiddenType;

# From CSS::TagSet (CSS::Media): a subset's `where` names a type that is only
# visible in the declaring module (there, an imported `Resolution`); the check
# must run in the declaration scope, not the caller's.
plan 3;

lives-ok { Holder.new }, 'default value passes the subset check';
lives-ok { Holder.new(:h(make-hidden)) }, 'explicit value passes';
dies-ok { Holder.new(:h(5)) }, 'non-matching value still rejected';

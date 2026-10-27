use Test;
use lib $*PROGRAM.parent(3).add('lib');
use PrivDelegate::Runner :run;
use PrivDelegate::Acts;

# From CSS::Selector::To::XPath (CSS::Grammar::Actions): a `handles` delegate
# returns a type object, and the delegated method calls `self!private`. Reached
# through an exported method called as a sub, the receiver sat in a container
# cell and the call died with "Calling private method ... must be fully
# qualified".
plan 2;
is-deeply run(:v(5)), {:a(5)}, 'a private call on a delegated type object works via an exported method';
is-deeply PrivDelegate::Acts.new.universal(6), {:a(6)}, 'and when called directly';

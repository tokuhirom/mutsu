use Test;

# Found via Array::Rounded: `@a[1.5]:exists` on an `is Array` subclass with a
# user `postcircumfix:<[ ]>` candidate typed with that class must reach the
# user candidate (which rounds the index), not the core routine.
plan 6;

my constant &old = &postcircumfix:<[ ]>;
class R is Array { }
proto sub postcircumfix:<[ ]>($, |) {*}
multi sub postcircumfix:<[ ]>(R:D \SELF, Any:D \index, |c) { old SELF, index.round, |c }
multi sub postcircumfix:<[ ]>(Array:D \SELF, Any:D \index, |c) { old SELF, index, |c }

my @a is R = <a b c d e>;
is @a[1.5], 'c', 'the user candidate rounds a plain subscript';
is @a[1.5]:exists, True, ':exists goes to the user candidate (2 exists)';
is @a[7.5]:exists, False, ':exists of a missing rounded index';
is @a[1.5]:delete, 'c', ':delete goes to the user candidate';
is @a[1.5]:exists, False, ':exists after :delete';

my @plain = <a b c>;
is @plain[1]:exists, True, 'a plain Array keeps the core :exists';

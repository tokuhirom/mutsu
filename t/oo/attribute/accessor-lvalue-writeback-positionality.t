use v6;
use Test;

plan 6;

# `$obj<key>.accessor = v` on a class that is BOTH Positional and Associative
# must write the element back along the subscript's own axis: the angle
# subscript dispatches ASSIGN-KEY, never ASSIGN-POS("key", ...). The compiler's
# accessor-lvalue writeback hardcoded the positional axis, so Text::CSV's
# CSV::Row (whose ASSIGN-POS takes `int $i`) died with a type-check error on
# `$r<baz>.text = "A"` (91_csv_cb.t test 16).

my @pos-log;
my @key-log;

class Field {
    has Str $.text is rw = "x";
}

class Row does Positional does Associative {
    has @.names;
    has @.fields;
    method of() { Mu }
    method AT-KEY(Str $k)  { @!fields[@!names.first($k, :k)] }
    method AT-POS(int $i)  { @!fields[$i] }
    method ASSIGN-KEY(Str $k, $v) { @key-log.push($k); @!fields[@!names.first($k, :k)] = $v }
    method ASSIGN-POS(int $i, $v) { @pos-log.push($i); @!fields[$i] = $v }
    method elems { @!fields.elems }
}

my $r = Row.new(names => <foo bar baz>, fields => [Field.new, Field.new, Field.new]);

lives-ok { $r<baz>.text = "A" }, 'associative accessor-lvalue assign lives';
is $r<baz>.text, "A", 'the keyed element was updated';
is-deeply @pos-log, [], 'ASSIGN-POS was never called for the angle subscript';

lives-ok { $r[1].text = "B" }, 'positional accessor-lvalue assign lives';
is $r[1].text, "B", 'the indexed element was updated';
is-deeply @key-log.grep(* eq "1").elems, 0, 'ASSIGN-KEY was not called with a stringified index';

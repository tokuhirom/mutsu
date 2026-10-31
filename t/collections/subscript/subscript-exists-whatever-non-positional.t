use Test;
plan 4;

class Plain { method AT-POS($i) { 42 }; method elems { 3 } }
is-deeply (Plain.new[*]:exists), (True,),
    '[*]:exists on a non-Positional object is a one-element list whatever its elems';
is-deeply (Plain.new[0, 1]:exists), (True, False), 'an explicit slice still asks per index';

class Pos does Positional {
    method AT-POS($i) { 42 }
    method EXISTS-POS($i) { $i < 2 }
    method elems { 3 }
}
is-deeply (Pos.new[*]:exists), (True, True, False), 'a Positional object answers per element';
is-deeply (Pos.new[^3]:exists), (True, True, False), 'and a range slice agrees';

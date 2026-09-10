use Test;

plan 6;

# Chained colonpairs in parentheses build a list of pairs
my @list = (:a(2) :b(3) :c(4));
is @list.elems, 3, 'chained colonpairs in parens produce 3 elements';
is @list[0].key, 'a', 'first pair key';
is @list[0].value, 2, 'first pair value';
is @list[1].key, 'b', 'second pair key';
is @list[2].key, 'c', 'third pair key';

# Chained colonpairs in curlies construct hashes
is {:a(2) :b(3) :c(4)}<b>, 3, 'chained colonpairs in curlies construct hash';

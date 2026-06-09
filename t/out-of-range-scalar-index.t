use Test;

# Indexing a scalar value (a single-element list) past index 0 throws
# X::OutOfRange with what => "Index", range => "0..0" and the offending index.
# Also exercises throws-like with an adverbed regex matcher (rx:i/.../).

plan 6;

throws-like 'use fatal; "foo"[2]', X::OutOfRange,
    'scalar string index out of range',
    what => rx:i/index/, range => '0..0', got => 2;

throws-like 'use fatal; 42[1]', X::OutOfRange,
    'scalar int index out of range',
    range => '0..0', got => 1;

# Index 0 on a scalar is the value itself.
is "foo"[0], "foo", 'index 0 of a string is the string';
is 42[0], 42, 'index 0 of an int is the int';

# Date / DateTime range errors keep their structured attributes.
throws-like 'Date.new("2012-02-30")', X::OutOfRange,
    'invalid day', :message{ .match: /«1»/ & /«29»/ };
throws-like 'DateTime.new(year => 2012, month => 5, day => 22, hour => 18, minute => 3, second => 60)',
    X::OutOfRange, 'leap second', comment => /'leap second'/;

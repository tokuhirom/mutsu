use Test;

plan 9;

# Numeric methods that return their invocant unchanged answer the allomorph.
is <1>.floor.raku, 'IntStr.new(1, "1")', 'IntStr.floor';
is <1>.ceiling.raku, 'IntStr.new(1, "1")', 'IntStr.ceiling';
is <1>.round.raku, 'IntStr.new(1, "1")', 'IntStr.round';
is <1>.truncate.raku, 'IntStr.new(1, "1")', 'IntStr.truncate';
is <1>.conj.raku, 'IntStr.new(1, "1")', 'IntStr.conj';
is <1.5>.abs.raku, 'RatStr.new(1.5, "1.5")', 'RatStr.abs';
is <1e0>.Bridge.raku, 'NumStr.new(1e0, "1e0")', 'NumStr.Bridge';

# A method that changes the value answers a plain number.
is <-1>.abs.raku, '1', 'IntStr.abs of a negative number';
is <1.5>.floor.raku, '1', 'RatStr.floor';

# vim: expandtab shiftwidth=4

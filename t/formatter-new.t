use v6.e.PREVIEW;
use Test;

plan 2;

my &handle = Formatter.new("'%5s'");
is handle('foo'), "'  foo'", 'Formatter.new returns a callable string formatter';

my &zero5 = Formatter.new('%05d');
is zero5(42), '00042', 'Formatter.new supports numeric directives';

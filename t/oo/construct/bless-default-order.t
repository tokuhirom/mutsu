use Test;

# Protocol::Postgres has a custom constructor whose DecodeBuffer initializer
# reads a required attribute supplied through self.bless.
plan 1;

class BlessDefaultOrderTest {
    has Blob:D $!buffer is built is required;
    has Int $!elems = $!buffer.elems;
    has Int $!pos is built is required;

    method new(Blob $buffer, Int $pos) { self.bless(:$buffer, :$pos) }
    method measured-elems() { $!elems }
}

is BlessDefaultOrderTest.new(Blob.new(1, 2, 3), 0).measured-elems, 3,
    'bless evaluates defaults after supplied attributes are bound';

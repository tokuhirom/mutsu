use Test;

# A type object `.new` whose user `new` candidates all decline the
# arguments falls back to the default constructor (Mu.new). That fallback
# is served by the native constructor (#9494: Text::CSV's `CSV::Field.new`,
# whose only candidate takes a `Str(Cool)`), so it must build exactly what
# the full path builds: defaults, `is default`, TWEAK, `is required`, and
# subclasses inheriting the user `new`.

plan 15;
# user `new` whose candidates all decline -> default constructor
class Field {
    has Bool $.is_quoted is rw is default(False);
    has Str  $.text is rw;
    has Int  $.n = 42;
    has @.log;
    multi method new (Str(Cool) $str) { self.bless.add($str) }
    method add (Str $c) { $!text ~= $c; self }
    submethod TWEAK { @!log.push: 'tweak' }
}
my $f = Field.new;
isa-ok $f, Field;
nok $f.text.defined;
is $f.is_quoted, False;
is $f.n, 42;
is-deeply $f.log, ['tweak'];
my $g = Field.new(text => 'hi', n => 7);
is $g.text, 'hi';
is $g.n, 7;
is Field.new('abc').text, 'abc';
is Field.new(5).text, '5';
my @many = (^50).map({ Field.new });
is @many.grep(*.n == 42).elems, 50;
dies-ok { Field.new('a', 'b') };
class Req { has $.x is required; multi method new(Int $i) { self.bless(x => $i) } }
dies-ok { Req.new }, 'is required still enforced';
is Req.new(x => 3).x, 3;
class Sub is Field { has $.extra = 'e' }
is Sub.new.extra, 'e';
is Sub.new.n, 42;

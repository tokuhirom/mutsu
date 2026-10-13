use Test;

# ADR-11276 §9.93: `Mu.Str` and `Mu.Stringy` are rows. `Mu.Str` answers every
# receiver whose own type declares no `Str`; `Mu.Stringy` is `self.Str`, as
# Rakudo declares it. The answers below are Rakudo's.

plan 24;

class OnlyStr { method Str { "custom" } }
class SubHash is Hash { }
class SubStr is Str { }

is OnlyStr.new.Stringy, 'custom', 'Stringy calls a class\'s own Str';
is SubHash.new((a => 1)).Stringy, "a\t1", 'Stringy of a Hash subclass is its Str';
isa-ok SubStr.new(value => 'x').Stringy, SubStr, 'Str.Stringy keeps a Str subclass instance';
my $p = Promise.new;
is $p.Stringy, $p.Str, 'Promise.Stringy is its Str';
my $c = Channel.new;
is $c.Stringy, $c.Str, 'Channel.Stringy is its Str';
is $*IN.path.Stringy, '<STDIN>', 'IO::Special.Stringy is its Str';
is Exception.new.Stringy, 'Something went wrong in (Exception)', 'Exception.Stringy is its message';
is <1.5e0>.Stringy, '1.5e0', 'a NumStr stringifies to its string';
my $sig = :(Int $x);
is $sig.Stringy, $sig.Str, 'Signature.Stringy is its Str';
is (1|2).Str, any('1', '2'), 'a Junction stringifies each eigenstate';
my @lazy = 1..*;
is @lazy.Str, '...', 'a lazy array is "..."';
is @lazy.Stringy, '...', '... through Stringy too';
is (1..*).lazy.Stringy, '...', 'Stringy of a lazy Seq does not force it';
is (1..*).map(* + 1).Stringy, '...', '... nor of a lazy map';

throws-like { (1/0).Stringy }, X::Numeric::DivideByZero, 'Stringy of a zero-denominator Rat dies';
throws-like { [1/0].Stringy }, X::Numeric::DivideByZero, '... inside an Array too';

sub failing { fail 'boom' }
throws-like { failing().Str }, X::AdHoc, 'Str of a Failure throws';
throws-like { failing().Stringy }, X::AdHoc, 'Stringy of a Failure throws';

{
    my $warned = 0;
    my $s = do { CONTROL { when CX::Warn { $warned++; .resume } }; Int.Str };
    is $s, '', 'a built-in type object stringifies to ""';
    is $warned, 1, '... with a warning';
}

my $u = utf8.new(65);
is $u."Str"(), 'A', 'a quoted .Str call on a utf8 variable decodes';
is $u.Stringy, 'A', 'utf8.Stringy decodes';
throws-like { Blob.new(1).Stringy }, X::Buf::AsStr, 'Blob.Stringy dies';
like (try { Blob.new(1).Stringy; 1 } // $!.message), /"not done with 'Stringy'"/,
    '... naming Stringy';

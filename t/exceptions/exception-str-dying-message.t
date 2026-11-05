use Test;

# `Exception.Str` / `.gist` swallow a user `message` that dies and fall back to the
# placeholder text, as Rakudo does (#16066).
plan 7;

class A::B::C is Exception { method message { die "inner" } }

is A::B::C.new.Str, 'Something went wrong in (C)', '.Str names the last name part';
is "{A::B::C.new}", 'Something went wrong in (C)', 'string interpolation too';
is A::B::C.new.gist, 'Unthrown A::B::C with no message', '.gist of an unthrown one';
dies-ok { A::B::C.new.message }, 'calling .message directly still dies';

my $caught;
try { A::B::C.new.throw; CATCH { default { $caught = $_ } } }
is $caught.Str, 'Something went wrong in (C)', 'a thrown one stringifies to the placeholder';

class Good is Exception { method message { "fine" } }
is Good.new.Str, 'fine', 'a working message is still used by .Str';
is Good.new.gist, 'fine', 'and by .gist';

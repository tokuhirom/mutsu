use Test;

# GH-9564: a key only one side of a Hash hyper has is combined with what the
# other side's AT-KEY yields -- `Any` for a plain Hash, its `is default` value
# otherwise -- not with the operator's identity. Like rakudo, the hash hyper
# is quiet: the `Any` operand prints no "uninitialized value" warning.

plan 12;

my $warned = 0;
CONTROL { when CX::Warn { $warned++; .resume } }

my %a = a => 1;
my %b = b => 2;

is-deeply %a »*« %b, { a => 0, b => 0 }, '»*« reads a missing key as Any, not 1';
is-deeply %a »~« %b, { a => '1', b => '2' }, '»~« reads a missing key as Any';
is-deeply %a »+« %b, { a => 1, b => 2 }, '»+« reads a missing key as Any';
is-deeply %a »**« %b, { a => 1, b => 0 }, '»**« reads a missing key as Any';
is-deeply (%a »,« %b), { a => (1, Any), b => (Any, 2) }, '»,« exposes the Any';
is-deeply %a »*» %b, { a => 0 }, 'one-sided hyper reads the missing key as Any';

my %h = a => 2;
is-deeply bag(<a b>) »*« %h, ('a' => 2).Bag, 'Bag »*« Hash: the Hash side reads Any';
is-deeply %h »*« bag(<a b>), { a => 2, b => 0 }, 'Hash »*« Bag: the Hash side reads Any';

my %d is default(5) = a => 1;
is-deeply %d »*« { b => 3 }, { a => 0, b => 15 }, 'a missing key reads the is default value';

is $warned, 0, 'no warnings from the hash hyper';

sub infix:<warner>($, $) { warn 'inside'; 1 }
is-deeply { a => 1 } »warner« { a => 2 }, { a => 1 }, 'a user op in a hash hyper runs';

# GH-9607: the hash hyper suppresses warnings the same way "quietly" does
# (`push_warn_suppression`), so a `warn` a user op raises inside it must not
# reach the outer CONTROL installed above either.
is $warned, 0, 'a user op\'s warn inside a hash hyper does not reach an outer CONTROL';

use Test;

# A role that overrides the subscript protocol of a container subclass and
# defers to the native base with `nextsame` / `nextcallee` — the idiom the
# `AccountableBagHash` distribution is built on, and the shape
# `Language/subscripts.rakudoc` documents for `AT-KEY`.

plan 12;

class X::Negative is Exception {
    has $.key;
    has $.value;
    method message() { "Not allowed to set '$!key' to $!value" }
}

my role Accountable {
    multi method AT-KEY(::?CLASS:D: $key is raw) is raw {
        my &nextone := nextcallee;
        Proxy.new(
          FETCH => { nextone(self, $key) },
          STORE => -> $, Numeric() $value {
              $value >= 0
                ?? (nextone(self, $key) = $value)
                !! X::Negative.new(:$key, :$value).throw
          }
        )
    }
    multi method ASSIGN-KEY(::?CLASS:D: $key is raw, $value is raw) is raw {
        $value > 0
          ?? nextsame()
          !! X::Negative.new(:$key, :$value).throw
    }
}

class AccountableBag is BagHash does Accountable { }
class AccountableMix is MixHash does Accountable { }

my %bag is AccountableBag = a => 42, b => 666;
isa-ok %bag, AccountableBag, 'the tie built the subclass';
is %bag<a>, 42, 'the overridden AT-KEY reads the native weight';

is (%bag<a> = 48), 48, 'ASSIGN-KEY passes the value through';
is %bag<a>, 48, 'and nextsame wrote it to the backing store';

is %bag<a>++, 48, 'postfix ++ FETCHes through the Proxy';
is %bag<a>, 49, 'and STOREs the incremented weight';

{
    my $caught = False;
    CATCH {
        $caught = True;
        when X::Negative { .resume }
        default { flunk 'wrong exception'; .resume }
    }
    %bag<a> = -1;
    ok $caught, 'a rejected assignment throws out of ASSIGN-KEY';
    is %bag<a>, 49, 'and leaves the weight alone';
}

my %mix is AccountableMix = a => 3.14, b => 666;
is %mix<a>, 3.14, 'the same role composes into a MixHash subclass';
is %mix<a>++, 3.14, 'postfix ++ reads the fractional weight';
is %mix<a>, 4.14, 'and stores the incremented one';

{
    my $caught = False;
    CATCH {
        $caught = True;
        when X::Negative {
            is .message, "Not allowed to set 'a' to -1", 'the exception carries the key';
            .resume;
        }
        default { flunk 'wrong exception'; .resume }
    }
    %mix<a> = -1;
}

# vim: expandtab shiftwidth=4

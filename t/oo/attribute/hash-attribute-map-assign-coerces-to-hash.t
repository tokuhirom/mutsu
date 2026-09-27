use Test;

plan 6;

# #9708: assigning a Map to an untyped `%!`/`%.` attribute must copy its
# pairs into a fresh, mutable Hash -- exactly like `my %h = Map.new(...)`
# already does for a lexical -- rather than keeping the Map's own immutable
# container identity.

class Private {
    has %!h;
    method assign { %!h = Map.new((a => 1)) }
    method what { %!h.WHAT }
    method store { %!h<c> = 2; %!h }
}

my $p = Private.new;
$p.assign;
is $p.what, (Hash), 'assigning a Map to a private %! attribute yields a Hash';
is-deeply $p.store, %(a => 1, c => 2),
    'the Hash-coerced private attribute accepts a new element store';

class Public {
    has %.h;
    method assign { %!h = Map.new((a => 1)) }
}

my $q = Public.new;
$q.assign;
is $q.h.WHAT, (Hash), 'assigning a Map to a public %. attribute yields a Hash';
$q.h<c> = 2;
is-deeply $q.h, %(a => 1, c => 2),
    'the Hash-coerced public attribute accepts a new element store';

# A local variable's own coercion (the working baseline #9708 compares
# against) must keep working identically.
my %h = Map.new((a => 1));
is %h.WHAT, (Hash), 'assigning a Map to a lexical %h still yields a Hash';
%h<c> = 2;
is-deeply %h, %(a => 1, c => 2),
    'the Hash-coerced lexical accepts a new element store';

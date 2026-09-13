use v6;
use Test;

# A sigilless pointy parameter (`-> \index { ... }`) declares a TERM, not a
# lexical: inside the block a bare `index` IS that binding. Two things follow,
# and every control statement that takes a pointy block owes both.
#
# 1. The PARSE scope has to carry the name, or a name that also spells a listop
#    (`index`, `join`, ...) is read as a call and swallows the rest of the
#    statement — so it is the ENCLOSING construct that fails. That is how
#    Hash::Ordered (and Archive::Ar / BSON::Simple / Terminal::Tests through it)
#    reported "expected ')'" at a `method` header several lines below the real
#    cause (#7954).
# 2. The binding has to be declared AS a sigilless term, or the body's bare word
#    is a package lookup and the block reads `(Any)`.
plan 14;

# --- the construct Hash::Ordered's DELETE-KEY is built from -------------------

role Ordered {
    has %!indices;
    has @!keys;

    method load(*@k) {
        @!keys = @k;
        %!indices{@k[$_]} = $_ for ^@k.elems;
        self
    }

    method shift-from(\key) {
        with %!indices{key} -> \index {
            %!indices{@!keys[$_]}-- for index ^.. @!keys.end;
            @!keys.splice(index, 1);
            %!indices{key}:delete;
        }
        self
    }

    method order { @!keys.join(',') }
    method index-of(\key) { %!indices{key} }
}

class OrderedHash does Ordered {}

my $h = OrderedHash.new.load(<a b c d>);
is $h.order, 'a,b,c,d', 'role with a `-> \index` pointy body parses and runs';
$h.shift-from('b');
is $h.order, 'a,c,d', '... the sigilless binding drove the splice';
is $h.index-of('d'), 2, '... and the decrement loop saw it as a term, not a listop';

# --- a listop-spelled name in every pointy-block control statement ------------

my @got;
if 1 -> \index { @got.push($_) for index .. 2 }
is @got.join(','), '1,2', 'if: a listop-named sigilless param is a term';

@got = ();
unless 0 -> \index { @got.push($_) for index .. 2 }
is @got.join(','), '0,1,2', 'unless: a listop-named sigilless param is a term';

@got = ();
with 1 -> \index { @got.push($_) for index .. 2 }
is @got.join(','), '1,2', 'with: a listop-named sigilless param is a term';

@got = ();
given 1 -> \index { @got.push($_) for index .. 2 }
is @got.join(','), '1,2', 'given: a listop-named sigilless param is a term';

@got = ();
for 1 -> \index { @got.push($_) for index .. 2 }
is @got.join(','), '1,2', 'for: a listop-named sigilless param is a term';

# --- the binding itself reads back -------------------------------------------

with 7 -> \v { is v, 7, 'with: the sigilless binding reads back' }
without Any -> \v { is v.^name, 'Any', 'without: the sigilless binding reads back' }
if 8 -> \v { is v, 8, 'if: the sigilless binding reads back' }
unless 0 -> \v { is v, 0, 'unless: binds the condition value itself' }

# An `else` clause takes one too, and it binds the condition value — including
# when the THEN clause's own binding was the sigilless one whose spelling
# (`\a`) the else source had to read through.
if 0 -> \a { } else -> \v { is v, 0, 'else: a sigilless param binds the condition' }
if 0 -> \a { } else -> $v { is $v, 0, 'else: reads through a sigilless then-binding' }

done-testing;

use Test;

plan 5;

# Raku's line-ending-block rule: a closing `}` that is the last thing on its
# line terminates the statement, so a `.method` on the NEXT line starts a new
# (topic) statement instead of chaining onto the block. A `)`-final call
# keeps chaining across the newline. Cro's serializer tests rely on this:
#   my $body-stream = supply { ... }
#   .append-header(...);   # topic call on the surrounding given, NOT the Supply

# 1. supply block + next-line .method = topic method call.
my $topic-hit = False;
class LEBS-C { method m() { $topic-hit = True } }
given LEBS-C.new {
    my $x = supply {
        emit 1;
    }
    .m;
    isa-ok $x, Supply, 'the declared variable got the supply, not the chain result';
}
ok $topic-hit, 'next-line .method after supply block ran on the topic';

# 2. `}`-final hash composer behaves the same (matches rakudo).
my $h = { a => 1 }
.keys;
isa-ok $h, Hash, 'hash composer at end of line ends the statement';

# 3. `)`-final expression keeps chaining across the newline.
my @r = (1, 2, 3).map({ $_ * 2 })
.grep({ $_ > 2 });
is @r.join(","), "4,6", 'paren-final call still chains across the newline';

# 4. Same-line chaining on a block is unaffected.
is { b => 2 }.keys.join(","), "b", 'same-line .method on a hash composer still chains';

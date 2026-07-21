use v6;
use Test;

plan 8;

# A `for @c[$slice]` loop inside a `do given @c { ... }` expression must NOT
# write its (read-only) loop values back into @c. Regression: the `do given`
# topic's container tag leaked into the nested for-loop, which — having no
# source tag of its own — used it as a writeback target and shifted @c by the
# slice offset ([A,B,C,D] -> [B,C,C,D]). Root cause of CLDR::List's list
# formatter producing "B, B, C, D" instead of "A, B, C, D".

sub slice-range(*@list) { do given @list { for @list[1..2] -> $e { }; @list[0] } }
is slice-range('A'..'D'), 'A', 'do-given + for @list[range] leaves @list[0] intact';

sub slice-comma(*@list) { do given @list { for @list[1,2] -> $e { }; @list.join(',') } }
is slice-comma(<A B C D>), 'A,B,C,D', 'do-given + for @list[1,2] does not shift @list';

sub slice-whatever(*@list) { do given @list { for @list[1..*-3] -> $e { }; @list.join(',') } }
is slice-whatever(<A B C D>), 'A,B,C,D', 'do-given + for @list[1..*-3] does not shift @list';

sub nested-when(*@list) {
    do given @list {
        when * {
            for @list[1..*-3] -> $e { }
            @list[0];
        }
    }
}
is nested-when('A'..'D'), 'A', 'do-given/when + for-slice keeps @list[0]';

# The full CLDR::List-style list join must produce the right first element.
my @placeholders = <{0} {1}>;
sub format(*@list) {
    do given @list {
        when * {
            my $format = @list[*-2] ~ ', ' ~ @list[*-1];
            for @list[1..*-3] -> $element { $format = $element ~ ', ' ~ $format }
            @list[0] ~ ', ' ~ $format;
        }
    }
}
is format('A'..'D'), 'A, B, C, D', 'CLDR-style 4-element list formats correctly';

# A two-iteration middle loop (5 elements) must not corrupt the source array.
sub keeps-array(*@list) {
    do given @list { for @list[1..*-3] -> $e { } }
    @list.join(',');
}
is keeps-array('A'..'E'), 'A,B,C,D,E', 'two-iteration for-slice does not corrupt @list';

# In-place topic mutation inside `do given` must still propagate to the source
# (this was not affected by the fix, but guard against regressing it).
sub elem-assign(@l) { do given @l { $_[0] = 'Z' }; @l.join(',') }
is elem-assign(['A','B','C']), 'Z,B,C', 'do-given $_[0] = X still propagates';

sub topic-push(@l) { do given @l { .push('X') }; @l.join(',') }
is topic-push(['A','B','C']), 'A,B,C,X', 'do-given .push still propagates';

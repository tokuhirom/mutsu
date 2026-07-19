use Test;

# `proceed` inside a `default` block falls through WITHOUT the default matching:
# it leaves the block (skipping the rest of it) and, since a `default` has no
# further candidate, the enclosing `given` ends normally and execution resumes
# after it. mutsu previously propagated the `proceed` control signal as an
# unhandled runtime error. See raku-doc/doc/Language/control.rakudoc.

plan 5;

# proceed in default: rest of the block is skipped, program continues.
my @out;
given * {
    default {
        @out.push: 'in-default';
        proceed;
        @out.push: 'never';
    }
}
@out.push: 'after-given';
is @out.join(','), 'in-default,after-given', 'proceed in default skips rest and falls through';

# A when that proceeds still reaches the default.
my @seq;
given 5 {
    when Int { @seq.push: 'int'; proceed }
    default  { @seq.push: 'def' }
}
is @seq.join(','), 'int,def', 'when proceed continues to default';

# proceed in default after a non-matching when: default runs, then falls through.
my @s2;
given 'x' {
    when Int  { @s2.push: 'int' }
    default   { @s2.push: 'def'; proceed; @s2.push: 'never' }
}
@s2.push: 'done';
is @s2.join(','), 'def,done', 'default proceed leaves given normally';

# Normal default (no proceed) still matches and yields its value.
my $v = do given 5 { default { 'matched' } };
is $v, 'matched', 'plain default still returns its value';

# succeed in default still returns the given value (unaffected by the fix).
my $w = do given 5 { default { succeed 'S'; 'no' } };
is $w, 'S', 'succeed in default still returns its argument';

use Test;

plan 8;

# A `Seq` whose source has not been pulled yet reaches a string context
# (`~$s`, `$s eq ...`) through the operand coercion, not through method
# dispatch. rakudo's `Seq.Str` is `self.cache.Str`, so the coercion must
# REIFY the body (non-destructively) rather than fall back to an opaque
# placeholder or an empty join.

my $path = "tmp/seq-string-context-{$*PID}.txt".IO;
LEAVE { try $path.unlink }
$path.spurt("A\nB\nC\n");

is ~$path.open(:r).lines, 'A B C', '~ on a deferred IO lines Seq stringifies its contents';
ok $path.open(:r).lines eq 'A B C', 'eq on a deferred IO lines Seq compares its contents';
ok $path.open(:r).lines eq <A B C>, 'a deferred IO lines Seq compares eq to a List';

# Non-destructive: rakudo's `~$s` caches, so a second stringification and a
# later `.List` both still see the elements.
my $twice = $path.open(:r).lines;
is ~$twice, 'A B C', 'first stringification';
is ~$twice, 'A B C', 'a second stringification still sees the elements';
is-deeply $twice.List, ('A', 'B', 'C'), '.List after stringification still works';

# `~` on an already-reified Seq is unchanged.
is ~((1, 2, 3).Seq), '1 2 3', '~ on a reified Seq is unchanged';
ok (1, 2, 3).Seq eq '1 2 3', 'eq on a reified Seq is unchanged';

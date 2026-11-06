use Test;

# From Grammar::PrettyErrors: a role's `method new` that only acts `if self.defined`
# is reached because `Grammar.parse` calls `self.new` on its invocant -- the
# instance, when the parse is called on one.
plan 5;

my @seen;
grammar G {
    method new(|c) { @seen.push(self.defined ?? 'instance' !! 'type'); callsame }
    token TOP { a }
}
my $i = G.new;
is @seen.join(','), 'type', 'G.new on the type object';
@seen = ();
G.parse("a");
is @seen.join(','), 'type', 'a type-object parse builds from the type';
@seen = ();
$i.parse("a");
is @seen.join(','), 'instance', 'an instance parse calls new on the instance';
@seen = ();
$i.subparse("a");
is @seen.join(','), 'instance', 'so does an instance subparse';
ok $i.parse("a").defined, 'and the parse still matches';

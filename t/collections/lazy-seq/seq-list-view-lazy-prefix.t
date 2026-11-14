use Test;

# The List that `.cache`, `.list`, `@$s` and `@.meth` make of a not-yet-run
# `.map`/`.grep` reifies as it is read: `.head` runs the callback only up to
# the first result. Expected values from rakudo. Reduced from
# CSS::Font::Resources (PDF::Font::Loader::CSS), whose
# `@.sources(|c).head` must not evaluate a trailing fallback source (#17773).

plan 12;

my @log;
sub probe() { (1, 2, 3).grep({ @log.push: $_; True }) }

class C {
    method srcs { probe() }
    method via-self { self.srcs.head }
    method via-attr-sigil { @.srcs.head }
    method via-contextualizer { @(self.srcs).head }
}

@log = ();
is C.via-self, 1, 'self.meth.head';
is-deeply @log, [1], '...runs the callback once';

@log = ();
is C.via-attr-sigil, 1, '@.meth.head';
is-deeply @log, [1], '...runs the callback once';

@log = ();
is C.via-contextualizer, 1, '@(...).head';
is-deeply @log, [1], '...runs the callback once';

@log = ();
my $c = probe().cache;
is $c.head, 1, '.cache.head';
is-deeply @log, [1], '...runs the callback once';
is $c.elems, 3, 'the rest of the cached List reifies on demand';
is-deeply @log, [1, 2, 3], '...each element once';

my $t = (1, 2, 3).grep(* > 0);
is-deeply (@$t.is-lazy, @$t.elems, @$t.WHAT), (False, 3, List),
    'the List view is not lazy, counts and types as a List';
is-deeply $t.list, (1, 2, 3), 'and reads in full';

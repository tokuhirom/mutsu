use v6;
use Test;

plan 3;

# A block passed to `Lock.protect` runs inline, but `self` inside it must bind
# to the method that lexically encloses the block — not to whatever outer caller
# invoked the method. This mirrors zef's Pluggable role: `Repo.update` calls
# `$fetcher.plugins`, whose `$lock.protect: { ... self!try-load ... }` must see
# the Fetch as `self`, not the Repo.

role Pluggable {
    has @.backends;
    has $!plugins;
    has $!lock = Lock.new;
    method plugins { return self!list-plugins }
    method !list-plugins(@backends = @!backends) {
        $!lock.protect: {
            return $!plugins if $!plugins.so;
            my @plugins;
            for @backends -> $b {
                if self!try-load($b) -> $c { push @plugins, $c }
            }
            return $!plugins := @plugins;
        }
    }
    method !try-load($plugin) { return "{$plugin}-loaded"; }
}

class Fetch does Pluggable {
    method fetch { return self.plugins }
    method whoami-in-protect {
        my $lock = Lock.new;
        return $lock.protect: { self.^name };
    }
}

class Repo {
    has $.fetcher;
    method update { return $.fetcher.fetch }
    method who { return $.fetcher.whoami-in-protect }
}

my $r = Repo.new(fetcher => Fetch.new(backends => ['a', 'b']));
is-deeply $r.update, ['a-loaded', 'b-loaded'],
    'private method called via self inside a nested protect block resolves on the enclosing invocant';

is $r.who, 'Fetch',
    '`self` inside a protect block is the enclosing method invocant, not the outer caller';

# Direct call (no outer wrapper) still works.
is $r.fetcher.whoami-in-protect, 'Fetch',
    '`self` inside a protect block is correct on a direct call too';

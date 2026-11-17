# Fixture for t/modules/module-check-cu-rewrite.t: a module whose CHECK phase
# rewrites its own `$*CU` (CheckTagGrep adds `:tag(42)` to every `.grep`).
use CheckTagGrep;
unit module CheckRewrittenModule;
CHECK tag-greps;

class Lister {
    method grep(&c, *%n) { %n<tag> // 'untagged' }
}

my &doubled = -> $x { $x * 2 };
my $offset = 5;
our @log;

sub above($x) { $x > 1 }

sub exported-query() is export { Lister.grep({ above($_) }) }
our sub package-query() { Lister.grep(&doubled) }
sub above-routine() is export { &above }
sub file-scope-lexicals() is export { "{doubled(3)} {&doubled(4)} $offset" }

@log.push: "mainline: " ~ exported-query();

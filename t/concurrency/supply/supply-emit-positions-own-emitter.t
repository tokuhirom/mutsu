use Test;

plan 6;

# An `emit` anywhere in a supply block's own frame goes to that block's
# emitter, since the supply-body rewrite walks the body through the exhaustive
# mutable visitor (ADR-10499). An unrewritten `emit` falls back to the dynamic
# emitter stack, which in a pipeline is a NEIGHBOURING stage's emitter, so the
# value skipped the rest of the pipeline. Each expectation was checked against
# rakudo.

class Collector {
    method transformer(Supply $items --> Supply) {
        supply {
            whenever $items -> $item { emit "[$item]" }
        }
    }
}

# Each stage under test reads from an upstream supply stage, so while its
# `whenever` body runs, the dynamically innermost emitter is the upstream's.
sub upstream(Supply $in --> Supply) { supply whenever $in -> $x { emit $x } }

sub run-pipeline(&stage) {
    my $src = Supplier.new;
    my @got;
    Collector.new.transformer(stage(upstream($src.Supply))).tap(-> $v { @got.push($v) });
    $src.emit("hi");
    @got
}

my @declared = run-pipeline(-> $in {
    supply whenever $in -> $item { my $r = emit($item.uc) }
});
is-deeply @declared, ['[HI]'], 'an emit in a declaration initializer';

my @list = run-pipeline(-> $in {
    supply whenever $in -> $item { my @r = 1, emit($item.uc) }
});
is-deeply @list, ['[HI]'], 'an emit in a list';

my @cond = run-pipeline(-> $in {
    supply whenever $in -> $item { if emit($item.uc) { } }
});
is-deeply @cond, ['[HI]'], 'an emit in an if condition';

my @say = run-pipeline(-> $in {
    supply whenever $in -> $item { note emit($item.uc) if False; emit($item.uc) unless False }
});
is-deeply @say, ['[HI]'], 'an emit under a statement modifier';

my @ret = run-pipeline(-> $in {
    supply whenever $in -> $item { $item.uc.&{ emit $_ } }
});
is-deeply @ret, ['[HI]'], 'an emit in a block the body calls inline';

# A closure the body only builds keeps the dynamic emitter: it runs wherever
# it is called.
my @built = run-pipeline(-> $in {
    supply whenever $in -> $item { my &e = { emit $item.uc }; e() }
});
is-deeply @built, ['[HI]'], 'an emit in a closure the body builds and calls';

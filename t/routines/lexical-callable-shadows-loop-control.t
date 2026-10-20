use Test;

# From the BDD::Behave distribution (Runner.run-config-around-each and its
# around-each hooks): a lexical `&next` -- a pointy-block parameter or a
# `my &next` -- is called by `next()`; it is not the loop-control `next`.
plan 5;

my @log;
my $hook = -> &next { @log.push: 'pre'; next(); @log.push: 'post' };
$hook({ @log.push: 'body' });
is-deeply @log, ['pre', 'body', 'post'], 'pointy-block &next parameter';

sub f(&next) { next() }
@log = ();
f({ @log.push: 'sub' });
is-deeply @log, ['sub'], 'sub &next parameter';

my &chain = sub { @log.push: 'core' };
@log = ();
for <a b> -> $h {
    my &next = &chain;
    &chain = sub { @log.push: "w$h"; next() };
}
chain();
is-deeply @log, ['wb', 'wa', 'core'], 'my &next closure chain';

my @seen;
for 1..2 {
    my &last = { @seen.push: "mylast$_" };
    last();
    @seen.push: "after$_";
}
is-deeply @seen, ['mylast1', 'after1', 'mylast2', 'after2'], 'my &last';

my @plain;
for 1..3 { next if $_ == 2; @plain.push: $_ }
is-deeply @plain, [1, 3], 'loop-control next is untouched elsewhere';

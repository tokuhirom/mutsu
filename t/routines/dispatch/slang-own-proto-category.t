use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang may declare a proto category of its own (`proto token tally-op`) with
# `:sym<...>` candidates that only its own production calls. Activation used to
# refuse the candidates as unsupported overrides of the core grammar, so the
# `use` died (Polyglot::Brainfuck's `BF-command:sym<+>`).

plan 2;

use SlangOwnProtoCategory;

sub capture(&code) {
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush { } }.new;
        code();
    }
    $out
}

is capture({ tally ++-+ }), "2\n", 'the production matches the candidates of its own proto';
is capture({ tally -- }), "-2\n", 'each candidate of the category is reachable';

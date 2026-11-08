use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang's grammar role named `Grammar` with a type parameter, composed through
# `define_slang`, plus a helper token only its own production calls (#16888).

plan 2;

use SlangHelperTokenSlang;

sub capture(&code) {
    my $out = '';
    {
        my $*OUT = class { method print(*@a) { $out ~= @a.join }; method flush { } }.new;
        code();
    }
    $out
}

is capture({ shout hello; }), "SHOUT\n", 'the production built from the helper token runs';
is capture({ shout again; shout twice; }), "SHOUT\nSHOUT\n", 'and each use runs it';

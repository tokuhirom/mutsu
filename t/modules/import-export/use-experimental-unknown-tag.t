use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

plan 7;

# `use experimental` only exports the tags of the features it gates; any other
# tag dies at compile time, as on rakudo (#18983). `:macros` was removed in
# Rakudo 2026.09, so it is unknown too.
is_run 'use experimental :macros; say 1',
    { :out(''), :err(/"Error while importing from 'experimental': no such tag 'macros'"/) },
    ':macros is no longer an experimental tag';
is_run 'use experimental :nonexist; say 1',
    { :out(''), :err(/"no such tag 'nonexist'"/) },
    'an unknown tag dies';
is_run 'use experimental :pack, :macros; say 1',
    { :out(''), :err(/"no such tag 'macros'"/) },
    'an unknown tag among known ones dies';

# The known tags still import.
is_run 'use experimental :pack; say 1', { :0status, :out("1\n"), :err('') },
    ':pack imports';
is_run 'use experimental :cached; say 1', { :0status, :out("1\n"), :err('') },
    ':cached imports';
is_run 'use experimental :rakuast; say 1', { :0status, :out("1\n"), :err('') },
    ':rakuast imports';
is_run 'use experimental; say 1', { :0status, :out("1\n"), :err('') },
    'a bare use experimental imports nothing and does not die';

use v6;
use Test;

plan 2;

# An installed dist (mzef/zef layout) maps a module to its source path, which
# maps to `{ "file": <id> }`. The parse-time scan must find that source, or an
# imported qualified type is unknown to the importer: under the RakuAST frontend
# `my %h is InstQual::Hash` was refused ("declaration with traits"), as in
# Text::Emoji's `my %by-tag is Map::Match` (#15126).

my $r = run($*EXECUTABLE, '-I', 'inst#t/fixtures/installed-nested-provides', '-e',
    'use InstQual::Hash; my %h is InstQual::Hash = a => 1; say %h.^name',
    :out, :err, :env(%*ENV, MUTSU_RAKUAST => 'convert'));
is $r.out.slurp(:close).trim, 'InstQual::Hash', 'imported qualified type in a variable is-trait round-trips';
is $r.exitcode, 0, 'exit code 0';

use Test;
use experimental :rakuast;
use lib 't/lib';

plan 2;

# An END phaser of an EVAL($ast)'d unit runs at program exit and still calls
# what the unit's own top-level `use` imported (the import sets are closed by
# then).
my @ended;
my $ast = Q[use Issue8214StaticExport; END { @ended.push('end:' ~ static-shadow()) }; 1].AST;
is EVAL($ast), 1, 'the unit runs';
END is-deeply @ended, ['end:imported'], 'its END calls the unit\'s import at exit';

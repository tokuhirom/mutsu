use Test;
use lib 'roast/packages/S12-meta/lib';

plan 2;

# A module whose EXPORTHOW package uses an unknown directive
# (`BBQ::class`, where BBQ is not DECLARE/SUPERSEDE/COMPOSE) must throw
# X::EXPORTHOW::InvalidDirective carrying the offending directive.
throws-like { EVAL 'use InvalidDirective;' },
    X::EXPORTHOW::InvalidDirective, directive => 'BBQ',
    'invalid EXPORTHOW directive throws X::EXPORTHOW::InvalidDirective';

# A plain module with no EXPORTHOW package loads without error.
lives-ok { EVAL 'use Supersede1;' },
    'a module without an invalid EXPORTHOW directive loads cleanly';

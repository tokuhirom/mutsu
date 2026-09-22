use lib 't/lib';
use Test;
use ExportedMultiCodeValueFixture;

# Regression found while running CSS::Module::CSS3::Selectors 0.0.6.
plan 1;

ok run(),
    'a proto/multi code value captured inside its defining module dispatches its concrete candidate';

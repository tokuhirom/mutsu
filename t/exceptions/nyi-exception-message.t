use Test;

# X::NYI's optional attributes contribute to the same message in every context.

plan 7;

my $full = X::NYI.new(
    feature => 'widgets',
    did-you-mean => 'gadgets',
    workaround => 'use gears',
);
my $full-message = "widgets not yet implemented. Sorry.\nDid you mean: gadgets?\nWorkaround: use gears";
is $full.message, $full-message, 'message includes all three attributes';
is $full.Str, $full-message, 'stringification uses the same message';

try { $full.throw };
is $!.message, $full-message, 'a thrown NYI keeps the complete message';

is X::NYI.new.message, 'Not yet implemented. Sorry.',
    'an omitted feature uses the generic wording';
is X::NYI.new(feature => 'widgets', did-you-mean => 'gadgets').message,
    "widgets not yet implemented. Sorry.\nDid you mean: gadgets?",
    'suggestion works without a workaround';
is X::NYI.new(feature => 'widgets', workaround => 'use gears').message,
    "widgets not yet implemented. Sorry.\nWorkaround: use gears",
    'workaround works without a suggestion';
is X::NYI.new(feature => '', did-you-mean => '', workaround => '').message,
    'Not yet implemented. Sorry.', 'empty attributes do not add lines';

use Test;
plan 5;

# `emit` / `done` used outside any supply/react block must raise
# X::ControlFlow (illegal => 'emit'/'done', enclosing => 'supply or react'),
# matching Rakudo. See roast/S17-supply/syntax.t.

throws-like 'emit 42', X::ControlFlow, illegal => 'emit',
    enclosing => 'supply or react',
    'bare emit outside supply throws X::ControlFlow';
throws-like 'done', X::ControlFlow, illegal => 'done',
    enclosing => 'supply or react',
    'bare done outside supply throws X::ControlFlow';

throws-like 'emit 42', X::ControlFlow,
    message => 'emit without supply or react',
    'emit X::ControlFlow message matches Rakudo';
throws-like 'done', X::ControlFlow,
    message => 'done without supply or react',
    'done X::ControlFlow message matches Rakudo';

# Sanity: emit/done inside a supply are still ordinary control flow,
# not exceptions — `done` stops the block after the first emit.
{
    my @got;
    my $s = supply { emit 1; done; emit 2; }
    $s.tap({ @got.push($_) });
    is @got, [1], 'emit/done inside supply remain normal control flow';
}

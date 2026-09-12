use v6;
use Test;

# `use Foo:from<NQP>;` names a compunit in the NQP language. mutsu has no NQP
# compunit repository — its `nqp::` ops are native and there is nothing to
# load — so the statement is a no-op rather than a "Could not find" failure.
#
# The failure mattered because of where the idiom appears: a module that adds
# a slang writes `use NQPHLL:from<NQP>;` as the first statement of its
# `sub EXPORT` (Test::Async::Decl), and aborting there silently discarded the
# whole slang registration below it.

plan 3;

lives-ok { EVAL 'use NQPHLL:from<NQP>; 1' },
    'use NQPHLL:from<NQP> loads nothing and does not die';

is EVAL('use NQPHLL:from<NQP>; 40 + 2'), 42,
    'the statements after it still run';

# Only `:from<NQP>` is exempt: an ordinary missing module is still an error.
dies-ok { EVAL 'use No::Such::Module::Here::Really; 1' },
    'a missing Raku module is still a load failure';

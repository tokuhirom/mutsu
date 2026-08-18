use Test;
use lib $?FILE.IO.parent.add('lib').Str;
use PrivateProtoModule;

plan 5;

# A `my`-scoped (non-`our`) proto/multi is exported and callable under its
# short name, but is never a package-stash symbol -- a package-qualified
# call must stay unresolved (`resolve_proto_function`, `dispatch_proto.rs`,
# previously skipped the same `qualified_name_hidden_here` gate a plain
# `sub` already went through). See
# news/2026-08/proto-package-qualified-visibility.md.

is wrapper(5), 'int:5', 'short-name call through an in-module wrapper works';
is secret(5), 'int:5', 'short-name call from the importer works (it is exported)';

dies-ok { PrivateProtoModule::secret(5) },
    'a package-qualified call to a my-scoped proto/multi dies';

# An `our`-scoped proto/multi IS a package symbol, so a qualified call to it
# must keep working (regression guard for the fix above).
is PrivateProtoModule::public-proto(5), 'our-int:5',
    'a package-qualified call to an our-scoped proto/multi still works';

# A plain (non-multi) my-scoped sub was already correctly rejected before
# this fix; pin it here too so both shapes are covered in one file.
dies-ok { PrivateProtoModule::wrapper(5) },
    'a package-qualified call to a my-scoped plain sub still dies (regression guard)';

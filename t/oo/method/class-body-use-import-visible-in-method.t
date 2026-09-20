use Test;

# A `use Some::Module;` written directly inside a class body imports the
# module's exported subs into that class's own package, as expected -- but a
# METHOD of that same class used to be unable to call the imported sub,
# because nothing ever anchored `current_package` to the class during method
# dispatch: `has_class_scoped_subs` only tracked the class's own `sub`
# declarations, not a plain `use` import's routines. mutsu died with
# "Unknown function: greet" (#8883).

plan 2;

use lib 't/lib';
use ClassBodyUseImportUser;

is ClassBodyUseImportUser.new.run('world'), 'Hello, world!',
    'a method calls a sub imported via `use` inside its own class body';

use ClassBodyUseImportNestedUser;
is ClassBodyUseImportNestedUser::Inner.new.run('nested'), 'Hello, nested!',
    'a class nested in a module still resolves its own class-body `use` import from a method';

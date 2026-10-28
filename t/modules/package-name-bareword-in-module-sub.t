use lib 't/lib';
use Test;

plan 2;

# annotations (ecosystem): `OUR::<DEFAULT> := annotations::containers` in a
# module file named the `module` the file declares. Read from a sub of that
# file the name was taken for a call of `&containers` in `annotations`.
use PkgNameBareword::Inner;

is self-name(), 'PkgNameBareword::Inner', 'a sub names its own file\'s module as a term';
is as-term(), 'PkgNameBareword::Inner', 'the term can be bound and introspected';

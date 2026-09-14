use Test;
use lib 't/lib';

BEGIN %*ENV<MUTSU_PRECOMP> = '0';

use BareModulePrivateLexical;

plan 2;

my $value = BareModulePrivateTarget.new.value;
is $value.^name, 'Missing', 'a bare module routine sees its private type';
nok $value.defined, 'the private type keeps its custom definedness';

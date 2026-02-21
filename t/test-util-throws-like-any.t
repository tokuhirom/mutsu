use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

plan 4;

ok &throws-like-any.defined, 'throws-like-any is available as an exported function';

throws-like-any { die 'boom' }, [Exception],
  'throws-like-any accepts Callable and matches Exception';

throws-like-any { die 'kapow' }, [Exception, X::AdHoc],
  'throws-like-any accepts any of listed exception types';

throws-like-any q[die 'string-path'], [Exception],
  'throws-like-any accepts string code and EVAL path';

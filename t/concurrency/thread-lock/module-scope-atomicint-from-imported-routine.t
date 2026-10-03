use lib 't/lib';
use Test;
use ModuleScopeAtomicint;

# A module's file-scope `my atomicint`, reached from one of its exported
# routines, holds its initial value on the first atomic fetch (#11455). It
# used to read as the `atomicint` type object until the first `⚛=` store.

plan 7;

is first-fetch(), 0, 'the first ⚛ fetch sees the initializer';
is plain-read(), 0, 'a plain read sees the initializer too';
is plain-atomic-fetch(), 7, '⚛ fetch of an untyped module-scope scalar';
is mark-initialized(), 1, '⚛= stores and a later ⚛ fetch sees it';
is first-fetch(), 1, 'the stored value persists across calls';
is plain-read(), 1, 'a plain read sees the atomic store';

await (^4).map: { start { hit() for ^250 } };
is hits(), 1000, 'concurrent ⚛++ from worker threads counts every increment';

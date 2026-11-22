use Test;
use lib 't/lib-worker-require-once';

plan 2;

# ADR-17216: a compunit is loaded once per process. A `require` on a worker
# thread records the load in the one program world every thread shares, so a
# later `require` of the same module on the main thread finds it loaded and
# does not run its mainline again.
await start { require WorkerOnce; 1 };
require WorkerOnce;
is $WorkerOnce::runs, 1, 'the mainline ran once across worker and main';

await start { require WorkerOnce; 1 };
is $WorkerOnce::runs, 1, 'a second worker require does not rerun it either';

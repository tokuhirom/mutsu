// Concurrency tests for the WASM build, run against pkg/ directly (no browser).
// Run: node wasm-demo/concurrency.test.mjs
//
// Requires wasm-demo/pkg/ to be built:
//   wasm-pack build --target web --no-default-features --features wasm
//   mv pkg wasm-demo/pkg
//
// Browsers give a wasm module one thread, so `start`/`await`/`Thread`/timers run
// on the cooperative scheduler in src/runtime/wasm_sched.rs rather than on OS
// threads. These are the cases that used to die with `RuntimeError: unreachable`
// the moment anything called `std::thread::spawn`; the point of the file is that
// the queue, the pump and the virtual clock keep producing the SAME answers a
// native mutsu gives.

import { readFile } from 'node:fs/promises';
import init, { evaluate } from './pkg/mutsu.js';

const wasm = await readFile(new URL('./pkg/mutsu_bg.wasm', import.meta.url));
await init({ module_or_path: wasm });

let passed = 0;
let failed = 0;

/** Run `code` and compare its whole output with `expected`. */
function is(code, expected, name) {
  let got;
  try {
    got = evaluate(code);
  } catch (e) {
    got = `THREW: ${e}`;
  }
  if (got.trim() === expected.trim()) {
    console.log(`  PASS: ${name}`);
    passed++;
  } else {
    console.error(`  FAIL: ${name}\n    code:     ${code}\n    expected: ${JSON.stringify(expected)}\n    got:      ${JSON.stringify(got)}`);
    failed++;
  }
}

console.log('start / await');
is('my $p = Promise.start({ 40 + 2 }); say $p.result;', '42', 'Promise.start.result');
is('say await start { 1 + 1 };', '2', 'await a start block');
is('say await Promise.kept(3);', '3', 'await an already-kept promise');
is('my $p = start { 7 }; say $p.status; say await $p; say $p.status;',
   'Planned\n7\nKept', 'a start block stays Planned until awaited');
is('say (await (start { 1 }, start { 2 }, start { 3 })).join(",");',
   '1,2,3', 'await a list of promises');
is('my @p = (1..3).map({ start { $_ * 2 } }); say await @p;', '(2 4 6)', 'await mapped start blocks');
// Deterministic here, unlike native mutsu/raku where the process may exit
// before a detached thread gets to print: the queue is drained at program end.
is('start { say "from the task" }; say "mainline";',
   'mainline\nfrom the task', 'an unawaited start block still runs at program end');

console.log('promise combinators');
is('await Promise.allof(start { 1 }, start { 2 }); say "all done";', 'all done', 'Promise.allof');
is('my @p = (start { 1 }, start { 2 }); my $any = Promise.anyof(@p); await $any; say $any.status;',
   'Kept', 'Promise.anyof');
is('say await Promise.kept(2).then({ .result * 21 });', '42', 'then on a kept promise');
is('my $p = start { 6 }; say await $p.then({ .result * 7 });', '42', 'then on a pending promise');
is('my $p = start { die "boom" }; try { await $p; CATCH { default { say "caught: " ~ .message } } }',
   'caught: boom', 'a die inside start surfaces at await');

console.log('channels');
is('my $c = Channel.new; $c.send(1); $c.send(2); $c.close; say $c.list;', '(1 2)', 'send/close/list');
is('my $c = Channel.new; my $p = start { $c.send(42); $c.close }; await $p; say $c.receive;',
   '42', 'receive a value a start block sent');
is('my $c = Channel.new; start { for 1..3 { $c.send($_) }; $c.close }; say $c.list;',
   '(1 2 3)', 'a blocking receive pumps the queued producer');
is('my $c = Channel.new; $c.send("x"); $c.close; react { whenever $c -> $v { say "got $v" } }',
   'got x', 'react/whenever over a channel');

console.log('threads and locks');
is('my $t = Thread.start({ say "in thread" }); $t.finish; say "joined";',
   'in thread\njoined', 'Thread.start/finish');
is('my $l = Lock.new; my $n = 0; await (^4).map: { start { $l.protect({ $n++ }) } }; say $n;',
   '4', 'Lock.protect across start blocks');
is('my atomicint $i = 0; await (^5).map: { start { $i⚛++ } }; say $i;', '5', 'atomicint increments');

console.log('timers and the virtual clock');
is('await Promise.in(0.01); say "timer fired";', 'timer fired', 'Promise.in');
is('my $t = now; sleep 1; say (now - $t) >= 1;', 'True', 'sleep advances the clock it reports');
is('say time > 1700000000;', 'True', 'time comes from the host clock, not 0');
is('say DateTime.now.year >= 2026;', 'True', 'DateTime.now is a real date');
is('my $n = 0; react { whenever Supply.interval(0.01) { $n++; done if $n >= 3 } }; say $n;',
   '3', 'Supply.interval drives a react block');
is('my $p = start { sleep 0.05; "late" }; say await $p;', 'late', 'a start block that sleeps');

console.log('supplies');
is('Supply.from-list(1,2,3).tap({ say $_ });', '1\n2\n3', 'Supply.from-list');
is('my $s = Supplier.new; my @got; my $t = $s.Supply.tap({ @got.push($_) }); $s.emit(1); $s.emit(2); $s.done; say @got;',
   '[1 2]', 'tap a Supplier');

console.log('deadlock reporting (single-threaded limit)');
// WASM-ONLY behaviour: native mutsu and raku both block here forever (a second
// thread could still send). With one thread and an empty run queue nothing ever
// can, so the pump reports it instead of freezing the tab.
is('my $c = Channel.new; try { $c.receive; CATCH { default { say "reported" } } }',
   'reported', 'a receive that can never be satisfied errors instead of hanging');

console.log(`\n${passed} passed, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);

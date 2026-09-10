#!/usr/bin/env python3
"""Per-opcode instruction profile of a JIT-compiled mutsu chunk.

The JIT emits its bodies into anonymous memory at runtime, so a profiler has no
symbol for them and reports only a bare `???:0x...` (13.3% of `bench-fib` at
the time of writing). This joins the two halves back together.

    cargo build --profile profiling
    MUTSU_JIT_DUMP=asm MUTSU_JIT_DUMP_FILE=tmp/jit.txt MUTSU_JIT_DUMP_BIN=tmp/jitbin \
      valgrind --tool=callgrind --dump-instr=yes \
               --callgrind-out-file=tmp/cg.out \
               ./target/profiling/mutsu benchmarks/bench-fib.raku
    scripts/jit-instr-profile.py tmp/cg.out tmp/jit.txt tmp/jitbin

Reads the entry address, length and per-opcode byte spans that MUTSU_JIT_DUMP
printed, disassembles <bin-dir>/<name>.bin at the real load address, and
reports (a) a per-source-opcode Ir table and (b) the annotated listing.
Callgrind counts retired instructions deterministically, so the numbers are
load-insensitive and directly comparable across builds.

See docs/jit-codegen-inspection.md.
"""

import re
import subprocess
import sys
from collections import defaultdict

if len(sys.argv) < 4:
    sys.exit(__doc__)
cg_path, dump_path, bin_dir = sys.argv[1], sys.argv[2], sys.argv[3]
want = sys.argv[4] if len(sys.argv) > 4 else None

# --- chunk headers, per-opcode spans and the opcode listing from the dump ---
chunks, spans, oplist = [], defaultdict(list), defaultdict(dict)
cur_name = None
for line in open(dump_path):
    m = re.match(r"=== mutsu jit: (\S+) ops\[", line)
    if m:
        cur_name = m.group(1)
        continue
    m = re.match(r"\s{2,}(\d+)\s\s(\S.*)$", line)
    if m and cur_name:
        oplist[cur_name][int(m.group(1))] = m.group(2).strip()
        continue
    m = re.match(r"--- (\S+) code \((\d+) bytes\)", line)
    if m:
        cur_name = m.group(1)
        continue
    m = re.match(r"\s+span 0x([0-9a-f]+)\.\.0x([0-9a-f]+) op (\d+)", line)
    if m and cur_name:
        spans[cur_name].append((int(m.group(1), 16), int(m.group(2), 16), int(m.group(3))))
        continue
    m = re.match(r"--- (\S+) entry at 0x([0-9a-f]+) len (\d+)", line)
    if m:
        chunks.append((m.group(1), int(m.group(2), 16), int(m.group(3))))
if want:
    chunks = [c for c in chunks if c[0] == want]
if not chunks:
    sys.exit("no chunks found in %s" % dump_path)

# --- per-address Ir from the callgrind file --------------------------------
POS = r"(0x[0-9a-f]+|[+-]?\d+|\*)"
counts = defaultdict(int)
cur = None
total = 0
pending_call = False
for raw in open(cg_path):
    line = raw.rstrip("\n")
    if not line:
        continue
    if line.startswith("calls="):
        pending_call = True
        continue
    if re.match(r"^(fn|fl|ob|cfn|cfl|cob|fe|fi|jump|jcnd)=", line) or line[0] == "#":
        if re.match(r"^(fn|fl|ob)=", line):
            cur = None
        continue
    if re.match(r"^(events|positions|summary|totals|cmd|part|desc|version|creator|pid):", line):
        continue
    m = re.match(r"^%s\s+%s\s+(\d+)\s*$" % (POS, POS), line)
    if not m:
        continue
    pos, ir = m.group(1), int(m.group(3))
    if pos.startswith("0x"):
        cur = int(pos, 16)
    elif pos == "*":
        pass
    elif pos[0] in "+-":
        cur = (cur or 0) + int(pos)
    else:
        cur = int(pos)
    if pending_call:
        pending_call = False   # inclusive cost of a call, not a caller instruction
        continue
    counts[cur] += ir
    total += ir

print("callgrind: %d addresses, %d Ir self total\n" % (len(counts), total))

for name, addr, length in chunks:
    binf = "%s/%s.bin" % (bin_dir, name)
    dis = subprocess.run(
        ["objdump", "-D", "-b", "binary", "-m", "i386:x86-64", "-M", "att",
         "--adjust-vma=%#x" % addr, binf],
        capture_output=True, text=True).stdout
    body = sum(v for k, v in counts.items() if addr <= k < addr + length)
    print("=== %s  %#x..%#x (%d bytes)  self Ir=%d (%.2f%% of run) ===" %
          (name, addr, addr + length, length, body,
           100.0 * body / total if total else 0))

    # per-opcode table
    per_op, per_op_instrs, glue = defaultdict(int), defaultdict(int), 0
    for lo, hi, op in spans.get(name, []):
        for a in range(addr + lo, addr + hi):
            if a in counts:
                if op == 0xFFFFFFFF:
                    glue += counts[a]
                else:
                    per_op[op] += counts[a]
                    per_op_instrs[op] += 1
    if per_op:
        print("\n%-6s %12s %7s %8s %9s  %s" %
              ("op", "Ir", "share", "runs", "instr/run", "opcode"))
        for op in sorted(per_op, key=lambda o: -per_op[o]):
            runs = max((counts[a] for lo, hi, o in spans[name] if o == op
                        for a in range(addr + lo, addr + hi) if a in counts), default=0)
            print("%-6d %12d %6.2f%% %8d %9.1f  %s" %
                  (op, per_op[op], 100.0 * per_op[op] / body, runs,
                   per_op[op] / runs if runs else 0, oplist.get(name, {}).get(op, "?")))
        print("%-6s %12d %6.2f%% %8s %9s  %s" %
              ("glue", glue, 100.0 * glue / body, "", "", "prologue / regalloc moves"))
        print("%-6s %12d %6.2f%%" % ("total", body, 100.0))

    print("\n--- annotated listing ---")
    op_at = {}
    for lo, hi, op in spans.get(name, []):
        for a in range(addr + lo, addr + hi):
            op_at[a] = op
    for line in dis.splitlines():
        m = re.match(r"^\s*([0-9a-f]+):\s", line)
        if not m:
            continue
        a = int(m.group(1), 16)
        op = op_at.get(a)
        tag = "" if op is None or op == 0xFFFFFFFF else "op%-3d" % op
        print("%12d %-6s %s" % (counts.get(a, 0), tag, line.strip()))

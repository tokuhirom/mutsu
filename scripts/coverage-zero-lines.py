#!/usr/bin/env python3
"""List the source lines no test run executed, from an lcov export.

    scripts/coverage-zero-lines.py COV.lcov [--min N] [PATH-SUBSTRING...]

Prints, per file whose path contains one of the substrings (all files when
none is given), how many of its instrumented lines never ran, then the runs
of at least N (default 8) consecutive such lines with the enclosing `fn`
and the first line of the run. How to produce the lcov file and what a
zero run does and does not prove: docs/dead-code-coverage.md.
"""

import re
import sys
from collections import defaultdict


def read_lcov(path):
    zero, total, current = defaultdict(list), defaultdict(int), None
    with open(path) as handle:
        for line in handle:
            line = line.strip()
            if line.startswith("SF:"):
                current = line[3:]
            elif line.startswith("DA:") and current:
                number, count = line[3:].split(",")[:2]
                total[current] += 1
                if count == "0":
                    zero[current].append(int(number))
    return zero, total


def runs(numbers, gap=2):
    out = []
    for number in sorted(numbers):
        if out and number - out[-1][1] <= gap:
            out[-1][1] = number
        else:
            out.append([number, number])
    return out


def enclosing_fn(lines, number):
    for index in range(number - 1, -1, -1):
        match = re.match(r"\s*(?:pub(?:\([a-z]+\))? )?(?:const )?fn (\w+)", lines[index])
        if match:
            return match.group(1)
    return "?"


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    minimum, paths, lcov = 8, [], None
    arguments = iter(argv[1:])
    for argument in arguments:
        if argument == "--min":
            minimum = int(next(arguments))
        elif lcov is None:
            lcov = argument
        else:
            paths.append(argument)
    zero, total = read_lcov(lcov)
    for path in sorted(zero, key=lambda p: -len(zero[p])):
        if paths and not any(part in path for part in paths):
            continue
        print(f"## {path}: {len(zero[path])} of {total[path]} lines never ran")
        try:
            lines = open(path).read().split("\n")
        except OSError:
            continue
        for start, end in runs(zero[path]):
            if end - start + 1 >= minimum:
                text = lines[start - 1].strip()[:80]
                print(f"  {start}-{end} [{enclosing_fn(lines, start)}]: {text}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))

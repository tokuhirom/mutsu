#!/usr/bin/env python3
"""Measure declaring owners for mutsu's RakuAST accessor schema.

Input is `cargo test dump_rakuast_accessor_schema -- --ignored --nocapture`.
The result records missing methods explicitly; it never invents an owner.
Each type is probed in its own Rakudo process to keep measurements independent.
"""

import argparse
import json
from pathlib import Path
import re
import subprocess

ORACLE = r'''
my $type := RAKUAST_ORACLE_TYPE;
my @mro = $type.^mro;
for @*ARGS -> $method {
    my @owners = @mro.grep: -> $owner { $owner.^method_table{$method}:exists };
    if @owners {
        say "OWNER\t$method\t" ~ @owners[0].^name;
    } else {
        say "MISSING-METHOD\t$method";
    }
}
'''


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--schema-log", type=Path, required=True)
    parser.add_argument("--raku", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    classes = {}
    registered = set()
    for line in args.schema_log.read_text().splitlines():
        fields = line.split("\t")
        if fields[0] == "MODEL_ACCESSOR":
            _, model, measured, method = fields
            classes.setdefault((model, measured), set()).add(method)
        elif fields[0] == "REGISTERED_ACCESSOR":
            registered.add(tuple(fields[1:]))
    records = []
    version = subprocess.run(
        [args.raku, "-e", "say $*RAKU.compiler.version"],
        capture_output=True, text=True, check=True, timeout=30,
    ).stdout.strip()
    for (model, measured), methods in sorted(classes.items()):
        if not re.fullmatch(r"RakuAST(?:::[A-Za-z][A-Za-z0-9]*)+", measured):
            raise ValueError(f"invalid model type name: {measured}")
        result = subprocess.run(
            [args.raku, "-e", ORACLE.replace("RAKUAST_ORACLE_TYPE", measured), *sorted(methods)],
            capture_output=True, text=True, timeout=30,
        )
        if result.returncode:
            records.extend(dict(model=model, measured=measured, method=method,
                                status="PROBE-ERROR", owner=None, error=result.stderr)
                           for method in sorted(methods))
            continue
        for line in result.stdout.splitlines():
            status, method, *owner = line.split("\t")
            records.append(dict(model=model, measured=measured, method=method,
                                status=status, owner=owner[0] if owner else None))
    data = dict(version=version, records=records, registered=sorted(registered))
    args.output.write_text(json.dumps(data, indent=2) + "\n")
    missing = [row for row in records if row["status"] != "OWNER"]
    print(json.dumps(dict(version=version, classes=len(classes),
                          accessors=len(records), missing=missing), indent=2))


if __name__ == "__main__":
    main()

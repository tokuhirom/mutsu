#!/usr/bin/env python3
"""Map which parts of the source touch each `Interpreter` field (#10779 phase 2).

Analysis only: it changes nothing. It reads `pub struct Interpreter` in
`src/runtime/mod.rs`, finds every access to each field, and reports, per
field, the files and "areas" that touch it, then groups fields by the area
that owns most of their accesses.

An access is `.<field>` (not followed by `(` or `!`) in a file that implements
or handles the interpreter (`impl Interpreter`, `impl ... for Interpreter`, or
names an `interp`/`interpreter` binding), plus every call of a *trivial
accessor* -- a method of `Interpreter` whose body only reads/borrows one field
(`fn env(&self) -> &Env { &self.env }`) -- since many fields are reached only
through one. This is a textual heuristic: a field whose name is shared with
another struct's field in the same files is over-counted, and an access
through a non-trivial helper method is attributed to the helper's file. The
report states which fields have generic names so a reader can discount them.

An *area* is a group of files: `src/<top>/<subdir>/...` -> `<top>/<subdir>`,
otherwise `<top>/<first word of the file stem>` (with `vm_` / `builtins_` /
`methods_` prefixes looked through, so `vm_call_light.rs` -> `vm/call`).

Usage:
  scripts/interp-field-matrix.py                 # markdown report to stdout
  scripts/interp-field-matrix.py --json OUT.json # also dump the raw matrix
  scripts/interp-field-matrix.py --check         # the `make checks` ratchet
  scripts/interp-field-matrix.py --update        # re-cut it after a drop
  scripts/interp-field-matrix.py --self-test

`--check` is ADR-10779 D4: the number of direct `Interpreter` fields may only
go down (it is recorded in scripts/interp-fields-baseline.txt), and every field
must match a `SUBSYSTEMS` rule. New state goes into the subsystem type it
belongs to instead of onto `Interpreter`. `--check` only parses the struct, so
it needs no build and takes milliseconds.
"""

import argparse
import collections
import json
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
STRUCT_FILE = SRC / "runtime" / "mod.rs"

FIELD_RE = re.compile(r"^    (?:pub(?:\([a-z:]+\))? )?([a-z_][a-z0-9_]*):\s*(.*?),?$")
# A field read through `self.`/`interp.`/... in the right files.
HANDLES_INTERP_RE = re.compile(
    r"\bimpl(?:<[^>]*>)?\s+(?:[\w:]+\s+for\s+)?(?:crate::runtime::)?Interpreter\b"
    r"|\binterp(?:reter)?\b"
)
PREFIXES = ("vm_", "builtins_", "methods_", "native_", "nqp_", "runtime_")


def strip_comments(text):
    text = re.sub(r"/\*.*?\*/", "", text, flags=re.S)
    return re.sub(r"//[^\n]*", "", text)


def parse_fields(text=None):
    if text is None:
        text = STRUCT_FILE.read_text()
    m = re.search(r"^pub struct Interpreter \{\n(.*?)^\}", text, re.S | re.M)
    if not m:
        sys.exit("interp-field-matrix: `pub struct Interpreter` not found")
    fields = {}
    depth = 0
    for line in m.group(1).splitlines():
        if depth == 0:
            fm = FIELD_RE.match(line)
            if fm:
                fields[fm.group(1)] = fm.group(2).strip()
        depth += line.count("{") + line.count("<") - line.count("}") - line.count(">")
        depth = max(depth, 0)
    return fields


def area_of(path):
    rel = path.relative_to(SRC)
    parts = rel.parts
    if len(parts) == 1:
        return "root/" + re.split(r"_", rel.stem)[0]
    top = parts[0]
    if len(parts) > 2:
        return f"{top}/{parts[1]}"
    stem = rel.stem
    if stem == "mod":
        return f"{top}/mod"
    for p in PREFIXES:
        if stem.startswith(p) and len(stem) > len(p):
            stem = stem[len(p):]
            break
    return f"{top}/{stem.split('_')[0]}"


def trivial_accessors(fields):
    """Short methods of Interpreter that touch exactly one field.

    A method whose body is at most `ACCESSOR_MAX_LINES` lines and reads a
    single `self.<field>` (`fn registry(&self) -> RegistryReadGuard { ... }`,
    `fn env_mut(&mut self) -> &mut Env`) stands in for that field: its call
    sites are counted as accesses to the field.
    """
    acc = {}
    fn_re = re.compile(r"\bfn ([a-z_][a-z0-9_]*)\s*(?:<[^>]*>)?\s*\(\s*&(?:'\w+ )?(?:mut )?self\b")
    self_field = re.compile(r"\bself\.([a-z_][a-z0-9_]*)\b(?!\s*\()")
    for path in SRC.rglob("*.rs"):
        text = strip_comments(path.read_text(errors="replace"))
        if not re.search(r"\bimpl\s+(?:crate::runtime::)?Interpreter\b", text):
            continue
        for m in fn_re.finditer(text):
            open_at = text.find("{", m.end())
            semi = text.find(";", m.end())
            if open_at < 0 or (0 <= semi < open_at):
                continue
            depth, i = 0, open_at
            while i < len(text):
                c = text[i]
                if c == "{":
                    depth += 1
                elif c == "}":
                    depth -= 1
                    if depth == 0:
                        break
                i += 1
            body = text[open_at + 1:i]
            if body.count("\n") > ACCESSOR_MAX_LINES:
                continue
            touched = {f for f in self_field.findall(body) if f in fields}
            if len(touched) == 1:
                acc.setdefault(m.group(1), touched.pop())
    return acc


ACCESSOR_MAX_LINES = 6


BASELINE = ROOT / "scripts" / "interp-fields-baseline.txt"
BASELINE_HEADER = """\
# Direct fields of `struct Interpreter` (src/runtime/mod.rs). ADR-10779 D4:
# this number may only go down -- new state goes into the subsystem type it
# belongs to. Checked by `make check-interp-fields`; re-cut after extracting
# fields with
#   scripts/interp-field-matrix.py --update
"""


def read_baseline():
    for line in BASELINE.read_text().splitlines():
        if line.strip() and not line.startswith("#"):
            return int(line.strip())
    sys.exit(f"interp-field-matrix: no count in {BASELINE}")


def check(update):
    fields = parse_fields()
    count = len(fields)
    unclassified = sorted(f for f in fields if subsystem_of(f) == "unclassified")
    allowed = read_baseline()
    ok = True
    if unclassified:
        ok = False
        print("check-interp-fields: these Interpreter fields match no SUBSYSTEMS rule in "
              "scripts/interp-field-matrix.py; put each one in the subsystem it belongs "
              "to (ADR-10779 D2):\n  " + "\n  ".join(unclassified), file=sys.stderr)
    if count > allowed:
        ok = False
        print(f"check-interp-fields: Interpreter has {count} direct fields, the baseline "
              f"allows {allowed}. Add the new state to its subsystem's type instead of to "
              f"Interpreter (ADR-10779 D4).", file=sys.stderr)
    elif count < allowed:
        if update:
            BASELINE.write_text(BASELINE_HEADER + f"{count}\n")
            print(f"interp-fields baseline re-cut: {count} fields")
            return 0 if ok else 1
        ok = False
        print(f"check-interp-fields: Interpreter fell from {allowed} to {count} direct "
              f"fields -- re-cut:\n  scripts/interp-field-matrix.py --update", file=sys.stderr)
    if ok:
        print(f"check-interp-fields: {count} Interpreter fields (baseline {allowed}), "
              f"all classified")
    return 0 if ok else 1


def self_test():
    fixture = """\
pub struct Interpreter {
    env: Env,
    /// a doc comment: not_a_field: X,
    pub(crate) registry: Arc<RwLock<Registry>>,
    multi_line:
        HashMap<String, Vec<(u32, u32)>>,
    nested: Box<Fn(Foo { inner: u8 }) -> u8>,
    #[cfg(feature = "jit")]
    jit_thing: u32,
}
"""
    got = list(parse_fields(fixture))
    want = ["env", "registry", "multi_line", "nested", "jit_thing"]
    errors = []
    if got != want:
        errors.append(f"parse_fields: expected {want}, got {got}")
    for field, sub in [("env", "frame"), ("pending_call_arg_sources", "handoff"),
                       ("fn_resolve_cache", "caches"), ("rakuseen_active", "guards"),
                       ("no_such_field_xyz", "unclassified")]:
        if subsystem_of(field) != sub:
            errors.append(f"subsystem_of({field!r}): expected {sub}, got {subsystem_of(field)}")
    if errors:
        print("interp-field-matrix: self-test failed:\n  " + "\n  ".join(errors),
              file=sys.stderr)
        return 1
    print("interp-field-matrix: self-test ok")
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--json", help="also write the raw matrix here")
    ap.add_argument("--check", action="store_true", help="run the field-count ratchet")
    ap.add_argument("--update", action="store_true", help="re-cut the ratchet baseline")
    ap.add_argument("--self-test", action="store_true")
    args = ap.parse_args()
    if args.self_test:
        return self_test()
    if args.check or args.update:
        return check(args.update)

    fields = parse_fields()
    accessors = trivial_accessors(fields)
    names = "|".join(sorted(fields, key=len, reverse=True))
    field_re = re.compile(r"\.(" + names + r")\b(?!\s*[(!])")
    acc_names = "|".join(sorted(accessors, key=len, reverse=True)) or "(?!)"
    acc_re = re.compile(r"\.(" + acc_names + r")\s*(?:::<[^>]*>)?\s*\(")

    # field -> file -> count
    matrix = collections.defaultdict(lambda: collections.Counter())
    for path in sorted(SRC.rglob("*.rs")):
        text = strip_comments(path.read_text(errors="replace"))
        if not HANDLES_INTERP_RE.search(text):
            continue
        rel = str(path.relative_to(ROOT))
        for m in field_re.finditer(text):
            matrix[m.group(1)][rel] += 1
        for m in acc_re.finditer(text):
            matrix[accessors[m.group(1)]][rel] += 1

    # Field names that are also a field of some other struct: over-count risk.
    other_struct_fields = collections.Counter()
    struct_re = re.compile(r"^\s*(?:pub(?:\([a-z:]+\))? )?struct (\w+)[^{;]*\{\n(.*?)^\s*\}",
                           re.S | re.M)
    for path in SRC.rglob("*.rs"):
        for sm in struct_re.finditer(strip_comments(path.read_text(errors="replace"))):
            if sm.group(1) == "Interpreter":
                continue
            for m in re.finditer(r"^\s+(?:pub(?:\([a-z:]+\))? )?([a-z_][a-z0-9_]*):",
                                 sm.group(2), re.M):
                if m.group(1) in fields:
                    other_struct_fields[m.group(1)] += 1

    rows = []
    for f, ty in fields.items():
        files = matrix.get(f, collections.Counter())
        areas = collections.Counter()
        for file, n in files.items():
            areas[area_of(ROOT / file)] += n
        total = sum(areas.values())
        home, home_n = (areas.most_common(1)[0] if areas else ("-", 0))
        rows.append({
            "field": f, "type": ty, "files": len(files), "areas": len(areas),
            "accesses": total, "home": home,
            "home_share": (home_n / total) if total else 0.0,
            "generic_name": other_struct_fields[f] > 0,
            "area_counts": dict(areas.most_common()),
            "file_counts": dict(files.most_common()),
        })

    if args.json:
        pathlib.Path(args.json).write_text(json.dumps(rows, indent=1))

    report(rows, accessors)


# Hand-written subsystem rules, first match wins (#10779 phase 2). They are a
# proposal for the extraction ADR, not a decision: they group fields by what
# the state *means* (read from each field's name, type and doc comment), which
# the textual co-occurrence clusters above cannot see. A field no rule matches
# is reported as unclassified so a new field gets placed deliberately.
SUBSYSTEMS = [
    ("frame", "VM frame and execution core", r"^(env|stack|locals|call_frames|current_code|cur_source_line|upvalues|trir|trir_outer_cache|resume_ip|numeric_op_site|stack_check_countdown|nested_run_depth|outer_scope_locals|enter_result_stack|frame_authoritative|frame_owned|block_stack|block_scope_depth|routine_stack|callframe_stack|caller_env_stack|closure_env_overrides|mark_ctx|jit_error|args_scratch_pool|nqp_arg_scratch|regex_quant_scratch|call_ic|pos_light_ic_epoch|closures_created|current_unit|thread_spawn_origin|handler_fns_snapshot)$"),
    ("handoff", "Call-site side channels (implicit arguments between caller, binder and VM)", r"^(pending_(call_|rw_|caller_|skip_|raw_|sigilless|runtime_name|local_updates|alias_bind|where_exception)|literal_native_args|static_call_args|rw_return_context|accessor_ref_pending|in_lvalue_assignment|trait_mod_|rw_param_rebinds|local_bind_pairs|carrier_writes|inline_control_env_writes|recorded_free_var_writes|vardecl_init_raw|element_share_pending|array_share_active|shaped_decl_context|sigilless_bind_source|hash_autovivify|in_does_rhs|container_element_proxy|test_pending_callsite_line|subset_where_fail|type_meta_key_cache)"),
    ("topic", "Topic, given/when, for/loop bookkeeping", r"^(topic_|last_topic_value|container_ref_|element_source|quanthash_bind_params|for_param_restore_stack|given_pointy_|when_matched|loop_|active_loop_|in_smartmatch_rhs|transliterate_in_smartmatch|substitution_in_smartmatch|regex_topic_pinned)"),
    ("control", "Control flow, exceptions, phasers, program exit", r"^(control_handler|catch_handler|end_phaser|main_end_slots|check_phaser_|mainline_leave_phasers|compunit_leave_frames|begin_|once_|next_once_scope_id|let_saves|halted|exit_code|exit_status_locked|uncaught_|pending_dispatch_error|module_load_order)"),
    ("io", "Output, IO handles, process environment, TAP", r"^(output_sink|warn_|surfaced_parse_warnings|io_handles|user_io_read_buffers|newline_mode|chroot_root|program_path|tap$|test_assertion_line_stack|encoding_registry|doc_comment|why_)"),
    ("module", "Module loading, compunits, import/export, pragmas", r"^(module_|unit_(module|private|imported)|loaded_modules|lib_paths|bundled_lib_paths|cur_repo|precomp_enabled|current_distribution|package_distributions|exported_|export_(amp|term)_override_names|import_|imported_|prelude_|compunit_visible_packages|package_declaring_units|class_declaring_units|operator_import_|preload_modules|pending_(dist_selectors|use_export_args|inner_export_subs)|loading_without_import|suppress_exports|need_hidden_classes|chain_declared_packages|packages_with_deferred_use_imports|require_|use_attach_depth|strict_mode|fatal_mode|lexical_fatal_mode|attributes_pragma|monkey_typing|suppress_cross_eval|native_call_specs)"),
    ("types", "Type/package registry and declarations (MOP)", r"^(registry|type_metadata|instance_type_metadata|current_package|method_class_stack|constructing_class|defining_class|last_registered_|deferred_trait_class_rollback|build_attr_writes|open_role_group|custom_type_data|rebless_map|classes_composing_accessors|pending_declare_new_type|persistent_classes|user_declared_classes|subset_predicate_cache|inline_subset_constraints|method_fallbacks|role_pun_construction|pending_proxy_subclass_attr|class_scoped_short_names|my_scoped_package_items|our_scoped_package_items|lexical_class_|enum_scope_names|poisoned_enum_aliases|suppressed_names|package_type_aliases|package_stash_hidden|attr_var_defaults|numeric_bridge_probe|attr_type_constraint_cache|squish_iterator_meta|predictive_seq_iters)"),
    ("lexicals", "Package/unit/state/our variable storage outside frames", r"^(our_|process_dynamics|hll_syms|package_lexicals|unit_lexical|mainline_lexical_subs|lexsub_|escaped_our_|escaping_our_|state_|pending_nested_state_scope|closure_captured_state|var_dynamic_flags|var_bindings|readonly_|sigilless_alias_seen|atomic_var_seen|block_declared_vars|constant_var_names_seen|nested_capture_owners|nested_method_captures|composed_nested_method_captures|class_body_static_names|hoist_pending_cells|hoisted_unreached_decls|frame_lexical_|amp_param_shadowed_names|param_bound_aggregates|type_body_written_lexicals)"),
    ("threads", "Cross-thread shared variables and locks", r"^(shared_|critical_section_depth|thread_|transient_lane_containers|suppress_shared_publish|sigilless_attrs_active|lock_async_)"),
    ("dispatch", "Dispatch state (multi/method/wrap/samewith stacks, dispatch flags)", r"^(multi_dispatch_stack|method_dispatch_stack|samewith_context_stack|wrap_|proto_dispatch_stack|metamodel_dispatch_stack|dispatch_token_counter|dispatch_ambiguous|dispatcher_wrap_bypass|native_base_bypass|method_call_depth|pending_method_dispatch|skip_pseudo_method_native|skip_postcircumfix_overload|suppress_binding_error_enhance|method_dispatch_pure|operator_assoc|user_declared_infix_ops|empty_sig_proto_names|registered_|prepared_fn_defs|fn_keys_)"),
    ("caches", "Resolution and compile caches (derived, rebuildable)", r"(_cache|_memo|_cacheable|_gen|_lane|_lane_candidate|_lane_active|^has_proto_cache_gen$|^method_cache_generation$|^last_method_resolve$|^otf_|^imported_compiled_fns$|^unit_imported_callables$|^dispatch_multi_candidate$|^deferral_build_context_free$|^subst_repl_plans$|^whenever_body_splits$|^create_memo$|^user_method_probe_memo$|^map_grep_compile_cache$)"),
    ("async", "Supply/react/gather/lazy-pull state", r"^(supply_|react_|pending_react_subscriptions|nested_react_callbacks|active_supply_emitters|pending_promise_whenever_arms|pending_tap_closes|current_react_waker|gather_|lazy_|take_defer_to_op_end|map_grep_last_depth|rw_map_topic_capture|next_invocation_id|invocation_id_block_end)"),
    ("regex", "Regex, grammar and slang state", r"^(grammar_|rx_cursor|walk_cursors|start_invocant|in_regex_code_block|action_made|current_grammar_actions|defined_slang_|slang_declarator_hows)"),
    ("eval", "EVAL/REPL/MAIN and compile-time capture analysis", r"^(pending_eval_|repl_compiler|last_value|pending_supply_|pending_whenever_inherited_owned|last_block_my_declared|main_hidden_from_usage|explicit_run_main|nested_mode|uncaught)"),
    ("guards", "Recursion/cycle guards for .raku/.gist and friends", r"^(rakuseen_|raku_leaf_)"),
]

CORE_MIN_FILES = 30
LINK_MIN = 0.25
# Files that walk the whole interpreter state rather than use part of it:
# the thread clone copies every field a spawned thread inherits, and the GC
# root scan visits every field that can hold a `Value`. They would link every
# field to every other, so clustering ignores them; the report lists their
# fields separately (an extracted subsystem must keep both walks whole).
WHOLE_STATE_FILES = {
    "src/runtime/runtime_thread.rs": "cloned into a spawned thread",
    "src/runtime/gc_roots.rs": "visited as a GC root",
}


def jaccard(a, b):
    return len(a & b) / len(a | b) if a or b else 0.0


def cluster(rows):
    """Average-linkage agglomerative clustering on the fields' file sets.

    Two fields are similar when the same files touch them (Jaccard). Clusters
    merge while their average pairwise similarity is at least `LINK_MIN`.
    """
    sets = [set(r["file_counts"]) - WHOLE_STATE_FILES.keys() for r in rows]
    members = {i: [i] for i in range(len(rows))}
    sim = {i: {} for i in members}
    for i in members:
        for j in range(i + 1, len(rows)):
            v = jaccard(sets[i], sets[j])
            if v > 0:
                sim[i][j] = sim[j][i] = v
    while True:
        best, pair = LINK_MIN, None
        for i, row in sim.items():
            for j, v in row.items():
                if i < j and v >= best:
                    best, pair = v, (i, j)
        if pair is None:
            break
        a, b = pair
        na, nb = len(members[a]), len(members[b])
        for k in set(sim[a]) | set(sim[b]):
            if k in (a, b):
                continue
            v = (na * sim[a].get(k, 0.0) + nb * sim[b].get(k, 0.0)) / (na + nb)
            sim[k].pop(b, None)
            if v > 0:
                sim[a][k] = sim[k][a] = v
            else:
                sim[a].pop(k, None)
                sim[k].pop(a, None)
        sim[a].pop(b, None)
        del sim[b]
        members[a].extend(members.pop(b))
    return [[rows[i] for i in m] for m in members.values()]


def subsystem_of(field):
    for key, _, rx in SUBSYSTEMS:
        if re.search(rx, field):
            return key
    return "unclassified"


def subsystem_report(rows, p):
    titles = {k: t for k, t, _ in SUBSYSTEMS}
    titles["unclassified"] = "matched by no rule"
    sub_fields = collections.defaultdict(list)
    sub_files = collections.defaultdict(set)
    file_subs = collections.defaultdict(set)
    for r in rows:
        k = subsystem_of(r["field"])
        sub_fields[k].append(r)
        for f in r["file_counts"]:
            if f not in WHOLE_STATE_FILES:
                sub_files[k].add(f)
                file_subs[f].add(k)
    order = [k for k, _, _ in SUBSYSTEMS] + ["unclassified"]
    order = [k for k in order if sub_fields[k]]
    p("## Proposed subsystems (hand-written rules over field meaning)")
    p("")
    p("| subsystem | meaning | fields | files touching it | fields in 30+ files |")
    p("|---|---|---|---|---|")
    for k in order:
        wide = [r["field"] for r in sub_fields[k] if r["files"] >= CORE_MIN_FILES]
        p(f"| {k} | {titles[k]} | {len(sub_fields[k])} | {len(sub_files[k])} | "
          f"{', '.join(f'`{w}`' for w in wide) or '-'} |")
    p("")
    spread = collections.Counter(len(v) for v in file_subs.values())
    p("Files by how many subsystems they touch: " + ", ".join(
        f"{n} subsystem{'s' if n > 1 else ''}: {spread[n]}" for n in sorted(spread)) + ".")
    p("")
    p("Coupling: files touching both subsystems (row, column).")
    p("")
    p("| | " + " | ".join(order) + " |")
    p("|---|" + "---|" * len(order))
    for a in order:
        cells = []
        for b in order:
            cells.append(str(len(sub_files[a] & sub_files[b])) if a != b else
                         f"**{len(sub_files[a])}**")
        p(f"| {a} | " + " | ".join(cells) + " |")
    p("")
    for k in order:
        p(f"### {k}: {titles[k]} ({len(sub_fields[k])})")
        p("")
        p(", ".join(f"`{r['field']}`{'*' if r['generic_name'] else ''} ({r['files']})"
                    for r in sorted(sub_fields[k], key=lambda r: (-r["files"], r["field"]))))
        p("")


def report(rows, accessors):
    out = []
    p = out.append
    dist = collections.Counter()
    for r in rows:
        f = r["files"]
        dist["0" if f == 0 else "1" if f == 1 else "2-3" if f <= 3 else
             "4-9" if f < 10 else "10-29" if f < 30 else "30+"] += 1
    p(f"`Interpreter` fields: {len(rows)}. Short single-field methods counted as "
      f"accessors: {len(accessors)}.")
    p("")
    p("| files touching the field | fields |")
    p("|---|---|")
    for k in ("0", "1", "2-3", "4-9", "10-29", "30+"):
        p(f"| {k} | {dist[k]} |")
    p("")

    subsystem_report(rows, p)

    core = sorted((r for r in rows if r["files"] >= CORE_MIN_FILES),
                  key=lambda r: -r["files"])
    p(f"## Core state ({len(core)} fields touched from {CORE_MIN_FILES}+ files)")
    p("")
    p("| field | files | top areas |")
    p("|---|---|---|")
    for r in core:
        a = ", ".join(f"{k} {v}" for k, v in list(r["area_counts"].items())[:4])
        p(f"| `{r['field']}`{'*' if r['generic_name'] else ''} | {r['files']} | {a} |")
    p("")

    rest = [r for r in rows if 0 < r["files"] < CORE_MIN_FILES]
    for f, what in WHOLE_STATE_FILES.items():
        n = sum(1 for r in rows if f in r["file_counts"])
        p(f"`{f.removeprefix('src/')}` touches {n} fields ({what}); clustering ignores it.")
        p("")
    clusters = cluster(rest)
    multi = sorted((c for c in clusters if len(c) > 1), key=lambda c: -len(c))
    singles = [c[0] for c in clusters if len(c) == 1]
    p(f"## Clusters ({len(multi)} groups of 2+ fields, average-linkage Jaccard "
      f">= {LINK_MIN} on the files that touch them)")
    p("")
    for n, c in enumerate(multi, 1):
        files = collections.Counter()
        areas = collections.Counter()
        for r in c:
            own = {k: v for k, v in r["file_counts"].items() if k not in WHOLE_STATE_FILES}
            files.update(own)
            areas.update(area_of(ROOT / k) for k in own for _ in range(own[k]))
        label = ", ".join(k for k, _ in areas.most_common(3))
        p(f"### C{n}: {label} ({len(c)} fields, {len(files)} files)")
        p("")
        p(", ".join(f"`{r['field']}`" + ("*" if r["generic_name"] else "")
                    for r in sorted(c, key=lambda r: r["field"])))
        p("")
        p("Files: " + ", ".join(f"`{f.removeprefix('src/')}` {v}"
                                for f, v in files.most_common(6)))
        p("")

    homes = collections.defaultdict(list)
    for r in singles:
        homes[r["home"]].append(r)
    p(f"## Unclustered fields ({len(singles)}), by the area with most accesses")
    p("")
    for home in sorted(homes, key=lambda h: (-len(homes[h]), h)):
        rs = sorted(homes[home], key=lambda r: r["field"])
        p(f"- **{home}** ({len(rs)}): " + ", ".join(
            f"`{r['field']}`{'*' if r['generic_name'] else ''} ({r['files']})" for r in rs))
    p("")
    unused = [r["field"] for r in rows if r["files"] == 0]
    if unused:
        p("## No access found")
        p("")
        p(", ".join(f"`{f}`" for f in sorted(unused)))
        p("")
    p("`*` = the name is also a field of another struct, so its count may include")
    p("accesses to that struct.")
    print("\n".join(out))

if __name__ == "__main__":
    sys.exit(main())

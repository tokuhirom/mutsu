# DBIish upstream Pg + mysql test suites reach full raku parity

The DBIish 0.6.8 upstream test suites now match raku exactly on both drivers:
all 11 PostgreSQL files (30-pg through 38-pg-threads, measured against a live
PostgreSQL 16) and all 8 mysql files (20-mysql through 28-mysql-threads,
against MariaDB). `24-mysql-types-json` runs 25/25 on mutsu where raku skips
(mutsu bundles JSON::Tiny as a battery).

The final gap-closing round was nine general-purpose fixes, none of them
DBIish-specific:

1. **Hash-attribute type-object keys** (`builtins_multidim_assign.rs`): the
   `$obj.attr{TypeObject} = v` method-lvalue path stringified a bare type
   object with its gist (`"(Str)"`) while the lookup path coerced it to `""`
   with the "uninitialized value in string context" warning, so
   `$dbh.Converter{YesNo} = $sub` stored under a key no lookup could find.
   Assignment and `:delete` now use the same `coerce_type_object_hash_key`
   three-way branch as lookup. Pin: `t/hash-attr-type-object-key.t`.

2. **Read-only free variables always see the live caller binding**
   (`vm_closure_dispatch.rs`): the per-closure-instance frozen state
   (`closure_captured_state`) was persisted and re-applied for *every* free
   variable, so a type-constrained scalar (excluded from ContainerRef boxing)
   captured by a closure was pinned to the value it had at the previous call —
   `my Str $e = "Yes"; my $s = sub { $e }; $s(); $e = "No"; $s()` still saw
   "Yes". Both the persist loop and the override are now gated on
   `free_var_writes`/`free_var_container_writes`: only free vars the body
   actually writes carry per-instance state. Pin:
   `t/closure-readonly-freevar-live.t`.

3. **`q` quoting escape semantics** (`q_string.rs`, `quote_adverbs.rs`): in
   `q` mode a backslash only escapes itself and the active delimiters, but
   mutsu unconditionally unescaped `\'` for every delimiter, so
   `q{'fo\'o'}` lost its backslash (the mysql suite pins `$dbh.quote`
   against exactly that literal). Pin: `t/q-delimiter-escape-semantics.t`.

4. **Typed Buf element width** (`vm_var_assign_index_named.rs`): element
   assignment into a Buf masked every value with `& 0xff`, a leftover from
   the width-1-only era — `Buf[uint64]` (DBDish's `Buf[intptr]` MYSQL_BIND
   length buffers) stored lengths mod 256, which scrambled every long-string
   fetch. `encode_elems` already masks to the node's own width, so the
   assignment now passes the full value. Pin: `t/buf-typed-element-width.t`.

5. **`nativecast(CArray[T], $p)` element writes** (`cstruct_layout.rs`,
   `vm_var_assign_index_named.rs`): the read arm existed but there was no
   write arm, so `$c[0] = v` fell to the autoviv fallback and silently
   replaced the native handle with a plain Raku Array. Added
   `native_carray_element_assign` (mirror of `native_carray_element` over
   `write_field`) and an early assign arm. Same pin file.

6. **`Str.Rat` / `Str.FatRat` big-integer parsing**
   (`dispatch_core_math.rs`): `str_to_rat` parsed every component with `i64`
   and collapsed anything past it to 0 — `"18446744073709551616".Rat` was 0,
   so DECIMAL columns past int64 read back as 0. It now parses with BigInt
   and builds through `make_big_rat` (which still downcasts small values).
   Pin: `t/str-to-rat-bigint.t`.

7. **Numeric comparison of big rationals** (`utils/compare.rs`):
   `compare_values` only tried small-rat parts, so `.sort` over values past
   i64 fell to the *string* fallback and mis-ordered them. Added a
   `to_big_rat_parts`/`compare_big_rat_parts` branch. Pin:
   `t/bigrat-sort-compare.t`.

8. **`my (...) = RHS` decont semantics** (`parser/stmt/decl/destructure.rs`,
   `builtins_lvalue.rs`): Rakudo's List.STORE iterates the RHS with one level
   of decont — `my ($a, $b) = $row` where `$row` holds an itemized Array
   flattens into elements, while `= $row,` keeps the itemized value whole.
   mutsu's desugar assigned the raw RHS to the temp array (no flatten). The
   assignment form now routes the RHS through `__mutsu_list_assign_rhs`,
   which deitemizes exactly the single-itemized-container shape and passes
   everything else through — deliberately NOT a blanket `.list` call, which
   would throw on a Failure RHS (`while my ($item) = @q.shift` must store
   the exhaustion Failure, not die). Pin:
   `t/paren-decl-list-assign-flatten.t`.

9. **`[Any]` survives scalar assignment/binding** (`vm_var_assign_typed.rs`):
   a 2026-03 cross-metaop normalization collapsed any single-element
   Array/Seq/Slip holding a nilish value to Nil on scalar stores, so
   `my \r = [Any]` (DBDish's NULL row) read back as Nil and `row()` reported
   no rows. The Array arm is removed (Seq/Slip kept for the cross shape);
   `roast/S03-metaops/cross.t` still passes. Pin lives in
   `t/paren-decl-list-assign-flatten.t`.

To re-run the suites: docker `mutsu-postgres` (port 15432) and
`mutsu-mariadb` (13306, with a socat forward to 3306 — the mysql tests
hardcode the default port), fixtures and env per the sweep scripts
(`tmp/pg-sweep.sh` / `tmp/mysql-sweep.sh` shape). Next step for the database
track: bundle DBIish and its NativeLibs / NativeHelpers::Blob dependencies as
batteries (`docs/batteries/`).

use Test;

# TEMPORARY (#19025): CI-only failures of 16 t/ files that pass locally (and
# pass when spawned directly from this file). A detached loop runs each of
# them several times while the parallel prove run goes on, and captures every
# failing run's output into $MUTSU_CRASH_DIR, which the crash-report step
# prints into the job log. To be reverted once the cause is known.

plan 1;

my $files = q:to/END/;
t/collections/array/array-mutator-rows.t
t/collections/lazy-seq/lazy-array-assign-preserve.t
t/collections/subscript/index-bind-containerless-readonly-and-codevar-alias.t
t/collections/subscript/nested-whatever-index-assign.t
t/collections/subscript/whatever-index-in-subscript-chain.t
t/collections/temp-element-saves-only-the-element.t
t/oo/attribute/accessor-method-object-binds-candidate.t
t/oo/class/classify-list-plain-hash-keys.t
t/oo/method/any-interp-method-rows.t
t/routines/closure/assuming-type-capture.t
t/routines/closure/user-infix-closure-arg-writeback.t
t/routines/signature/custom-infix-rw-first-param.t
t/routines/signature/entry-bound-var-to-raw-closure-param.t
t/routines/signature/signature-accepts-user-capture-coercion.t
t/tooling/nqp-getcomp-repl-context.t
t/vm/binding/nested-element-store-fast-lane.t
END

my $dir = %*ENV<MUTSU_CRASH_DIR>;
if $dir {
    mkdir $dir;
    spurt "$dir/../diag-files.lst", $files;
    my $loop = q:to/SH/;
        dir="$1"; list="$2"
        env | grep -E '^(HARNESS|TAP|PERL|MUTSU|RAKU|HOME|XDG)' > "$dir/diag-env.txt"
        for round in 0 1 2 3 4 5 6 7 8 9 10 11; do
          while read -r f; do
            out=$(scripts/run-t-test.sh "$f" 2>&1); rc=$?
            echo "round $round rc $rc $f" >> "$dir/diag-summary.txt"
            if [ $rc -ne 0 ]; then
              printf '=== %s round %s rc %s\n%s\n' "$f" "$round" "$rc" "$out" \
                > "$dir/diag-r$round-$(basename "$f").txt"
            fi
          done < "$list"
          ls -l --full-time "${XDG_CACHE_HOME:-$HOME/.cache}/mutsu/precomp" > "$dir/diag-cache-r$round.txt" 2>&1
          sleep 15
        done
        SH
    shell "setsid nohup bash -c '{$loop.subst("'", "'\\''", :g)}' diag '$dir' '$dir/../diag-files.lst' > /dev/null 2>&1 &";
}
pass 'diagnostics started';

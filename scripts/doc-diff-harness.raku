#!/usr/bin/env raku
# doc-diff-harness.raku — differential tester for the QA / finalization campaign (PLAN.md §8.1).
#
# It extracts runnable code examples from raku-doc, runs each through the REFERENCE
# `raku` and through `mutsu`, and reports only the cases where the two disagree.
# `raku` is the oracle, in three modes depending on how raku itself behaves:
#
#   stdout parity  raku exits 0 and prints something  -> compare stdout
#   error parity   raku exits non-zero at RUN time    -> compare the failure
#   silent parity  raku exits 0 and prints nothing    -> mutsu must also succeed quietly
#
# Only the first mode existed until 2026-09-09, which left 3916 of the corpus's 7768
# blocks (50%) never compared at all: every intentional-error example -- the entire
# `Type/X*.rakudoc` corpus among them -- was discarded as "no oracle". The other two
# modes are what PLAN.md §6 calls "error / exception parity": a block where raku dies
# and mutsu happily returns 0 is a *semantic* divergence, not a wording one, and it was
# invisible. A compile-time failure (`===SORRY!===`) still means "not a runnable
# example" and is still skipped -- that is what the gate was really for.
#
# This is intentionally NOT dependent on mutsu being correct — run it with system raku.
#
# Usage:
#   raku scripts/doc-diff-harness.raku [--mutsu=PATH] [--timeout=N] [--limit=N]
#                                      [--report=FILE] [--/error-parity] [FILES-OR-DIRS ...]
# Defaults: mutsu=target/debug/mutsu, timeout=10s, corpus=raku-doc/doc/Type + Language.
# `--/error-parity` restores the old stdout-only behaviour; it is ~2x faster because the
# error/silent modes run the oracle twice on blocks that used to cost one run.

sub MAIN(
    *@paths,
    Str  :$mutsu   = 'target/debug/mutsu',
    Int  :$timeout = 10,
    Int  :$limit   = 0,                          # 0 = no cap
    Str  :$report  = 'tmp/doc-diff-report.txt',
    Bool :$verbose = False,
    Bool :$error-parity = True,
) {
    # Every run below happens with its cwd set to a scratch directory, so a relative
    # `--mutsu=` (the default `target/debug/mutsu`) would no longer resolve. Absolutize
    # it here, once, against the cwd the user actually invoked the harness from.
    my $mutsu-bin = $mutsu.IO.absolute;
    unless $mutsu-bin.IO.e {
        note "doc-diff-harness: mutsu binary not found at $mutsu-bin — build it first.";
        exit 2;
    }

    my @files = collect-files(@paths);
    note "Scanning { +@files } .rakudoc files ...";

    my @blocks;
    for @files -> $file {
        @blocks.append: extract-blocks($file);
    }
    note "Extracted { +@blocks } candidate code blocks.";

    mkdir 'tmp/ddh' unless 'tmp/ddh'.IO.d;
    # Per-process scratch file so concurrent harness invocations (a parallel sweep
    # over many files) never clobber each other's program between the raku run and
    # the mutsu run — a shared path races and yields phantom "divergences".
    # ABSOLUTE, because every run below executes with its cwd set to the scratch
    # directory rather than the repo root.
    my $prog-path = "tmp/ddh/prog-{$*PID}.raku".IO.absolute;
    # A doc example that `spurt`s or `open`s a relative path writes into its cwd.
    # Run every block inside a throwaway directory so a sweep cannot leave strays
    # in the repository root (the 2026-09-07b sweep left `bar` and `foo.txt`).
    my $scratch = "tmp/ddh/run-{$*PID}".IO.absolute;

    my %stat = skipped-marker => 0, skipped-nondet => 0,
               skipped-oracle-nondet => 0,
               no-oracle => 0, match => 0, mismatch => 0,
               mutsu-crash => 0, matches-doc => 0,
               # Error-parity (raku fails at run time) and silent-parity (raku
               # succeeds with no output). Counted separately from `match`/`mismatch`
               # ON PURPOSE: folding them in would break comparability with every
               # sweep before 2026-09-09, exactly the way retiring the
               # `raku-drift-from-doc` bucket did (see docs/doc-diff-backlog.md).
               error-match => 0, error-mismatch => 0, mutsu-accepts => 0,
               silent-match => 0, silent-crash => 0, silent-noise => 0;
    my @findings;

    my $i = 0;
    for @blocks -> %b {
        last if $limit > 0 && $i >= $limit;
        $i++;

        if %b<skip> {
            %stat<skipped-marker>++;
            next;
        }
        if nondeterministic(%b<code>) {
            %stat<skipped-nondet>++;
            next;
        }

        my $program = %b<preamble> ?? %b<preamble> ~ "\n" ~ %b<code> !! %b<code>;
        my $path = $prog-path;
        spurt $path, $program;

        my $r = run-capture('raku', $path, $timeout, $scratch);

        # Which oracle mode does raku's own behaviour put this block in?
        #   'out'    — raku exits 0 with output: compare stdout (the original mode).
        #   'error'  — raku fails at RUN time: compare the failure itself.
        #   'silent' — raku exits 0 with no output: mutsu must also succeed quietly.
        # Everything else is genuinely not a runnable example and stays `no-oracle`:
        # a compile-time `===SORRY!===` (a doc fragment, a signature shown on its own)
        # and an oracle timeout (exit 124), where we have no trustworthy expectation.
        my $mode = do {
            if    $r<exit> == 0 && $r<out>.chars > 0 && $r<err> !~~ /SORRY/  { 'out' }
            elsif $r<err> ~~ /SORRY/ || $r<exit> == 124                       { Nil  }
            elsif !$error-parity                                              { Nil  }
            elsif $r<exit> != 0 && error-message($r<err>).chars > 0           { 'error' }
            elsif $r<exit> == 0 && $r<out>.trim.chars == 0                    { 'silent' }
            else                                                              { Nil  }
        };
        without $mode {
            %stat<no-oracle>++;
            next;
        }

        # Reproducibility gate: run the ORACLE twice and drop the block unless raku
        # agrees with itself. This is the whole nondeterminism policy — one rule, no
        # pattern list. It catches what no pattern list practically can: unordered
        # container iteration (`Set`/`Bag`/`Mix`/`*Hash`/`Map`/`Hash.kv`/enum `.keys`),
        # object addresses and `WHICH` ids, thread ids, `$*DISTRO`/`$*VM`/`dir` order.
        # Those blocks used to be compared, diverge on the unreproducible token alone,
        # and then get filed under the lowest-priority bucket forever — which is how
        # nine real mutsu bugs stayed hidden (see #7587).
        my $r2 = run-capture('raku', $path, $timeout, $scratch);
        my $reproducible = do given $mode {
            when 'out'    { $r2<exit> == 0 && normalize($r2<out>) eq normalize($r<out>) }
            when 'error'  { $r2<exit> == $r<exit>
                            && error-message($r2<err>) eq error-message($r<err>) }
            when 'silent' { $r2<exit> == 0 && $r2<out>.trim.chars == 0 }
        };
        unless $reproducible {
            %stat<skipped-oracle-nondet>++;
            next;
        }

        my $m = run-capture($mutsu-bin, $path, $timeout, $scratch);

        # --- error parity: raku fails at run time -------------------------------
        # Ranked by what the divergence actually means, because the three are not
        # equally interesting. `mutsu-accepts` is a SEMANTIC divergence — mutsu ran a
        # program raku refuses — while `error-mismatch` is usually about wording or the
        # `X::` type. Both are real; only one is a silent wrong answer.
        if $mode eq 'error' {
            if $m<exit> == 0 {
                %stat<mutsu-accepts>++;
                @findings.push: finding(%b, $program, $r, $m, 'mutsu-accepts');
            }
            elsif $m<exit> == 124 {
                %stat<error-mismatch>++;
                @findings.push: finding(%b, $program, $r, $m, 'mutsu-hangs');
            }
            elsif error-message($m<err>) eq error-message($r<err>) {
                %stat<error-match>++;
            }
            else {
                %stat<error-mismatch>++;
                @findings.push: finding(%b, $program, $r, $m, 'error-mismatch');
            }
            next;
        }

        # --- silent parity: raku exits 0 with no output -------------------------
        if $mode eq 'silent' {
            if $m<exit> != 0 {
                %stat<silent-crash>++;
                @findings.push: finding(%b, $program, $r, $m, 'mutsu-error-on-silent-success');
            }
            elsif $m<out>.trim.chars > 0 {
                %stat<silent-noise>++;
                @findings.push: finding(%b, $program, $r, $m, 'mutsu-extra-output');
            }
            else {
                %stat<silent-match>++;
            }
            next;
        }

        if normalize($m<out>) eq normalize($r<out>) {
            %stat<match>++;
        }
        elsif $m<exit> != 0 {
            %stat<mutsu-crash>++;
            @findings.push: finding(%b, $program, $r, $m, 'mutsu-error');
        }
        else {
            # Every mutsu-vs-raku divergence is one finding: `output-mismatch`.
            #
            # The doc's own `# OUTPUT:` annotation used to select a SEPARATE, lower-
            # priority `raku-drift-from-doc` bucket whenever raku no longer matched the
            # doc. That read a *provenance* fact as a *priority* one: the branch is only
            # reachable once mutsu already differs from raku, so it filed real
            # divergences under "not mutsu bugs". Measured on the 2026-09-07b sweep, 67
            # of its 114 blocks were real mutsu bugs against 5 that the name described.
            # The comparison survives as an ANNOTATION on the finding.
            my $expected = doc-expected(%b<code>);
            my $matches-doc = $expected.defined
                              && normalize($expected) eq normalize($m<out>);
            %stat<matches-doc>++ if $matches-doc;
            %stat<mismatch>++;
            @findings.push: finding(%b, $program, $r, $m, 'output-mismatch', $matches-doc);
        }

        if $verbose && $i %% 50 {
            note "  [$i/{ +@blocks }] match=%stat<match> mismatch=%stat<mismatch> crash=%stat<mutsu-crash> no-oracle=%stat<no-oracle>";
        }
    }

    unlink $prog-path if $prog-path.IO.e;
    rm-rf($scratch);
    write-report($report, %stat, @findings);
    print-summary(%stat, @findings, $report);
}

#| Expand paths (files or dirs) to a list of .rakudoc files. Default corpus if empty.
sub collect-files(@paths) {
    my @roots = @paths
        ?? @paths
        !! <raku-doc/doc/Type raku-doc/doc/Language>;
    my @files;
    for @roots -> $p {
        my $io = $p.IO;
        if $io.d {
            @files.append: walk-rakudoc($io);
        }
        elsif $io.f {
            @files.push: $io.Str;
        }
    }
    @files.unique.sort;
}

sub walk-rakudoc(IO::Path $dir) {
    my @out;
    for $dir.dir -> $e {
        if $e.d {
            @out.append: walk-rakudoc($e);
        }
        elsif $e.f && $e.Str.ends-with('.rakudoc') {
            @out.push: $e.Str;
        }
    }
    @out;
}

#| Extract code blocks from one .rakudoc file.
#| Handles explicit `=begin code`/`=end code`, `=for code`, and 4-space indented blocks.
#| Returns a list of hashes: { file, line, code, preamble, skip }.
sub extract-blocks(Str $file) {
    my @lines = $file.IO.lines;
    my @blocks;
    my $n = @lines.elems;
    my $i = 0;

    # Track ranges consumed by explicit blocks so the indented-scan skips them.
    my @consumed = False xx $n;

    while $i < $n {
        my $line = @lines[$i];

        # =begin code [adverbs] ... =end code
        if $line ~~ /^ \h* '=begin' \h+ 'code' \h* $<adv>=(.*) $/ {
            my $adv = ~$<adv>;
            my $start = $i;
            my @body;
            $i++;
            while $i < $n && @lines[$i] !~~ /^ \h* '=end' \h+ 'code' / {
                @body.push: @lines[$i];
                @consumed[$i] = True;
                $i++;
            }
            $i++; # skip =end code
            @blocks.push: mk-block($file, $start + 1, dedent(@body), $adv);
            next;
        }

        # =for code [adverbs]  (paragraph until blank line or next directive)
        if $line ~~ /^ \h* '=for' \h+ 'code' \h* $<adv>=(.*) $/ {
            my $adv = ~$<adv>;
            my $start = $i;
            my @body;
            $i++;
            while $i < $n && @lines[$i].trim ne '' && @lines[$i] !~~ /^ \h* '=' / {
                @body.push: @lines[$i];
                @consumed[$i] = True;
                $i++;
            }
            @blocks.push: mk-block($file, $start + 1, dedent(@body), $adv);
            next;
        }

        $i++;
    }

    # Indented (4-space) code blocks over the not-yet-consumed lines.
    $i = 0;
    while $i < $n {
        if !@consumed[$i] && @lines[$i] ~~ /^ \h ** 4..* \S/ {
            my $start = $i;
            my @body;
            while $i < $n && (@consumed[$i].not) &&
                  (@lines[$i] ~~ /^ \h ** 4..* \S/ || @lines[$i].trim eq '') {
                # stop the group if a blank line is followed by a non-indented line
                last if @lines[$i].trim eq '' &&
                        ($i + 1 >= $n || @lines[$i + 1] !~~ /^ \h ** 4..* \S/);
                @body.push: @lines[$i];
                $i++;
            }
            # strip trailing blanks
            @body.pop while @body && @body[*-1].trim eq '';
            @blocks.push: mk-block($file, $start + 1, dedent(@body), '')
                if @body;
        }
        else {
            $i++;
        }
    }

    @blocks;
}

sub mk-block(Str $file, Int $line, Str $code, Str $adv) {
    my $skip = $adv.contains('skip-test');
    # Non-raku languages (`:lang<...>` other than raku/perl6): skip.
    $skip ||= so $adv ~~ /':lang<' <-[>]>* '>'/ && $adv !~~ /':lang<raku>'/ && $adv !~~ /':lang<perl6>'/;
    my $preamble = '';
    if $adv ~~ /':preamble<' $<p>=(<-[>]>*) '>'/ {
        $preamble = ~$<p>;
    }
    { :$file, :$line, :$code, :$preamble, :$skip };
}

#| Remove the common leading indentation from a set of lines.
sub dedent(@lines) {
    my @nonblank = @lines.grep(*.trim ne '');
    return @lines.join("\n") unless @nonblank;
    my $min = @nonblank.map({ $_ ~~ /^ $<ws>=(\h*) / ?? (~$<ws>).chars !! 0 }).min;
    @lines.map({ .chars >= $min ?? .substr($min) !! $_ }).join("\n");
}

#| Heuristic: skip code whose output is inherently non-deterministic.
#|
#| `# ERROR` / `#ERROR` used to be on this list, which meant every example that
#| DELIBERATELY fails — the point of the whole `Type/X*.rakudoc` corpus — was thrown
#| away before the oracle ever ran. Error parity is exactly the comparison those
#| blocks want, so they are no longer skipped here; when error parity is off they fall
#| out at the oracle gate instead, as they always did.
#|
#| The rest of this list is a heuristic FIRST pass only. The load-bearing
#| nondeterminism policy is the oracle-twice gate in MAIN, which needs no patterns.
sub nondeterministic(Str $code) {
    so $code ~~ /
        << 'rand' >> | '.rand' | '.pick' | '.roll' | << 'now' >> | << 'time' >> |
        'DateTime.now' | 'Instant' | '.WHERE' | 'nqp::' | 'Telemetry' |
        'BEGIN' \h* 'note' | 'CALLER' | 'Backtrace' | 'Supply' | 'react' | 'Channel' |
        'Whatever' \h* 'star' | '.raku' \h* '#' \h* 'OUTPUT' \h* '«' \h* '.'
    /;
}

#| Concatenate the expected output from a block's inline `# OUTPUT: «...»` annotations,
#| turning the ␤ newline symbol into a real newline. Returns Nil when the block has none.
sub doc-expected(Str $code) {
    my @outs;
    for $code.match(/'# OUTPUT:' \h* '«' $<body>=(<-[»]>*) '»'/, :g) -> $m {
        @outs.push: (~$m<body>).subst('␤', "\n", :g);
    }
    return Nil unless @outs;
    @outs.join;
}

#| Run a program through `timeout N bin file`, returning { out, err, exit }.
#|
#| `$cwd` is a scratch directory, recreated empty before every run: a doc example
#| that writes a relative path must not touch the repo, and the two oracle runs of
#| the reproducibility gate must each start from the same (empty) state, or a block
#| that merely appends to a file would look non-reproducible.
sub run-capture(Str $bin, Str $file, Int $timeout, Str $cwd) {
    rm-rf($cwd);
    mkdir $cwd;
    my $proc = run 'timeout', "$timeout", $bin, $file, :out, :err, :$cwd;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    { out => $out, err => $err, exit => $proc.exitcode };
}

#| Recursively delete `$dir` if it exists. Confined to the harness's own scratch
#| tree by its callers; there is no core `rmtree`.
sub rm-rf(Str $dir) {
    my $io = $dir.IO;
    return unless $io.e;
    if $io.d {
        for $io.dir -> $e {
            $e.d ?? rm-rf($e.absolute) !! unlink($e);
        }
        rmdir $io;
    }
    else {
        unlink $io;
    }
}

#| Normalize output for comparison: strip trailing whitespace on each line and overall.
sub normalize(Str $s) {
    $s.lines.map(*.trim-trailing).join("\n").trim-trailing;
}

#| The comparable part of a failure: the exception's own message, without the
#| backtrace beneath it.
#|
#| raku and mutsu both print the message first and then frames (`  in sub f at
#| /path/prog.raku line 3`), and the frame text is implementation detail that will
#| never match — comparing it would make every error-parity block a finding and the
#| mode worthless. The message itself is the spec-relevant part, so stop at the first
#| frame line. A trailing `at <path>:<line>` on the message line is stripped for the
#| same reason. Returns '' when there is no message, which is how the caller decides a
#| non-zero exit carries no usable oracle.
sub error-message(Str $err) {
    my @msg;
    for $err.lines -> $l {
        next if $l.trim eq '' && !@msg;
        last if $l ~~ /^ \h+ 'in ' /;                 # backtrace frame
        last if $l ~~ /^ \h* 'in block <unit>' /;
        last if $l ~~ /^ \h* 'in any ' /;
        # Compile-time WARNINGS precede the exception on raku's stderr and have
        # nothing to do with the failure being compared. raku heads them with
        # `WARNINGS for <file>:` and tags each with `(line N)`; mutsu emits the same
        # advice unheaded, with an `at <file>:<line>` continuation. Neither is part of
        # any exception, and including them made `Language/exceptions.rakudoc:401` a
        # finding purely because raku warns about a sink-context `so` and mutsu does
        # not.
        next if $l ~~ /^ 'WARNINGS for ' /;
        next if $l ~~ /^ \h* 'Useless use of ' /;
        next if $l ~~ /^ \h* 'Potential difficulties' /;
        next if $l ~~ /'(line ' \d+ ')' \h* $/;
        next if $l ~~ /^ \h+ 'at ' \S+ ':' \d+ \h* $/;
        @msg.push: $l.trim;
    }
    @msg.join(' ')
        .subst(/\h+ 'at ' \S+ ':' \d+ \h* $/, '')
        .subst(/\h+ 'at ' \S+ \h+ 'line' \h+ \d+ \h* $/, '')
        .trim;
}

sub finding(%b, Str $program, %raku, %mutsu, Str $kind, Bool $matches-doc = False) {
    {
        kind     => $kind,
        file     => %b<file>,
        line     => %b<line>,
        program  => $program,
        raku-out => %raku<out>,
        raku-err => %raku<err>,
        raku-exit => %raku<exit>,
        mutsu-out => %mutsu<out>,
        mutsu-err => %mutsu<err>,
        mutsu-exit => %mutsu<exit>,
        matches-doc => $matches-doc,
    };
}

#| Findings from the error/silent oracle modes, which report a failure rather than
#| stdout. Kept as one list so a reader can tell at a glance which section of a report
#| is the 2026-09-09 error-parity work.
constant ERROR-PARITY-KINDS =
    set <mutsu-accepts mutsu-hangs error-mismatch
         mutsu-error-on-silent-success mutsu-extra-output>;

#| Lines kept per captured section in the report.
#|
#| Without a cap a single example can bury the sweep: `Type/IO/Path.rakudoc:509` is a
#| `sub MAIN` that recursively `.dir`-walks its working directory and produced a
#| 131_492-line, 8.4 MB report, and `Language/ipc.rakudoc:34` shells out and captured
#| a 1.6 MB git log. The 2026-09-07b sweep could only be committed after truncating
#| 11 MB down to 412 KB BY HAND, which is what the refresh recipe in
#| `docs/doc-diff-sweep/README.md` told the reader to do. Capping here makes that
#| recipe safe as written.
constant SECTION-CAP = 40;

#| Emit `$text` under `$label`, keeping at most SECTION-CAP lines and marking the cut
#| explicitly so a truncated section is never mistaken for the whole output.
sub say-capped($fh, Str $label, Str $text) {
    $fh.say: $label;
    my @lines = $text.trim-trailing.lines;
    if @lines > SECTION-CAP {
        $fh.say: @lines.head(SECTION-CAP).join("\n");
        $fh.say: "... [truncated by doc-diff-harness: { @lines - SECTION-CAP } more line(s) of { +@lines }]";
    }
    else {
        $fh.say: @lines.join("\n");
    }
}

sub write-report(Str $report, %stat, @findings) {
    my $fh = open $report, :w;
    $fh.say: "# doc-diff-harness report";
    $fh.say: "# stats: ", %stat.sort».fmt('%s=%s').join('  ');
    $fh.say: "";
    for @findings.kv -> $idx, %f {
        $fh.say: "=" x 78;
        $fh.say: "[{ $idx + 1 }] { %f<kind> }  { %f<file> }:{ %f<line> }";
        # Provenance, not priority: this used to select a separate low-priority bucket.
        $fh.say: "note: mutsu matches the doc's own `# OUTPUT:` here; raku does not."
            if %f<matches-doc>;
        say-capped($fh, "--- program ---", %f<program>);
        if ERROR-PARITY-KINDS{%f<kind>} {
            # The oracle here is raku's FAILURE, so show it — a report that printed
            # only stdout would be blank on both sides for most of these.
            say-capped($fh, "--- raku stderr (exit { %f<raku-exit> }) ---",
                       %f<raku-err>.trim eq '' ?? '(none)' !! %f<raku-err>);
            say-capped($fh, "--- raku stdout ---",
                       %f<raku-out>.trim eq '' ?? '(none)' !! %f<raku-out>);
        }
        else {
            say-capped($fh, "--- raku stdout ---", %f<raku-out>);
        }
        say-capped($fh, "--- mutsu stdout (exit { %f<mutsu-exit> }) ---",
                   %f<mutsu-out>.trim eq '' && ERROR-PARITY-KINDS{%f<kind>}
                       ?? '(none)' !! %f<mutsu-out>);
        if %f<mutsu-err>.trim ne ''
           && (%f<kind> eq 'mutsu-error' || ERROR-PARITY-KINDS{%f<kind>}) {
            $fh.say: "--- mutsu stderr ---";
            $fh.say: %f<mutsu-err>.trim-trailing.lines.head(6).join("\n");
        }
        $fh.say: "";
    }
    $fh.close;
}

sub print-summary(%stat, @findings, Str $report) {
    my $compared = %stat<match> + %stat<mismatch> + %stat<mutsu-crash>;
    my $err-compared = %stat<error-match> + %stat<error-mismatch> + %stat<mutsu-accepts>;
    my $sil-compared = %stat<silent-match> + %stat<silent-crash> + %stat<silent-noise>;
    say "";
    say "==== doc-diff-harness summary ====";
    say "  skipped (marker):        %stat<skipped-marker>";
    say "  skipped (nondet pattern): %stat<skipped-nondet>";
    say "  skipped (oracle not reproducible): %stat<skipped-oracle-nondet>   (raku disagreed with itself — the noise floor)";
    say "  no oracle (fragment / compile error / oracle timeout): %stat<no-oracle>";
    say "  ------------------------------------";
    say "  stdout parity (raku exits 0 with output): $compared";
    say "    match:                 %stat<match>";
    say "    output mismatch (★real): %stat<mismatch>";
    say "    mutsu error/crash (★real): %stat<mutsu-crash>";
    say "      of which mutsu matches the doc: %stat<matches-doc>   (annotation only — still a real divergence)";
    say "  error parity (raku fails at run time): $err-compared";
    say "    same failure:          %stat<error-match>";
    say "    mutsu ACCEPTS it (★★semantic): %stat<mutsu-accepts>";
    say "    different failure (★real): %stat<error-mismatch>";
    say "  silent parity (raku exits 0, no output): $sil-compared";
    say "    both quiet:            %stat<silent-match>";
    say "    mutsu fails (★real):   %stat<silent-crash>";
    say "    mutsu prints extra (★real): %stat<silent-noise>";
    if $compared > 0 {
        my $real = %stat<mismatch> + %stat<mutsu-crash>;
        my $rate = (100 * $real / $compared).round(0.1);
        say "  ★high-signal divergence (stdout parity): $rate%  ($real/$compared)";
    }
    say "  findings written to:     $report";
}

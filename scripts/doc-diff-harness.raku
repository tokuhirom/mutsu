#!/usr/bin/env raku
# doc-diff-harness.raku — differential tester for the QA / finalization campaign (PLAN.md §8.1).
#
# It extracts runnable code examples from raku-doc, runs each through the REFERENCE
# `raku` and through `mutsu`, and reports only the cases where the two disagree.
# `raku` is the oracle: a block is compared *only* when raku itself runs it cleanly
# (exit 0, no compile SORRY, and some stdout), which naturally filters out
# doc fragments, intentional-error examples, and non-runnable snippets.
#
# This is intentionally NOT dependent on mutsu being correct — run it with system raku.
#
# Usage:
#   raku scripts/doc-diff-harness.raku [--mutsu=PATH] [--timeout=N] [--limit=N]
#                                      [--report=FILE] [FILES-OR-DIRS ...]
# Defaults: mutsu=target/debug/mutsu, timeout=10s, corpus=raku-doc/doc/Type + Language.

sub MAIN(
    *@paths,
    Str  :$mutsu   = 'target/debug/mutsu',
    Int  :$timeout = 10,
    Int  :$limit   = 0,                          # 0 = no cap
    Str  :$report  = 'tmp/doc-diff-report.txt',
    Bool :$verbose = False,
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
               mutsu-crash => 0, matches-doc => 0;
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
        # Oracle gate: raku must run it cleanly and produce output.
        unless $r<exit> == 0 && $r<out>.chars > 0 && $r<err> !~~ /SORRY/ {
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
        unless $r2<exit> == 0 && normalize($r2<out>) eq normalize($r<out>) {
            %stat<skipped-oracle-nondet>++;
            next;
        }

        my $m = run-capture($mutsu-bin, $path, $timeout, $scratch);

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

#| Heuristic: skip code whose output is inherently non-deterministic, or that is
#| explicitly an error-demonstration example (comment says ERROR / dies).
sub nondeterministic(Str $code) {
    so $code ~~ /
        << 'rand' >> | '.rand' | '.pick' | '.roll' | << 'now' >> | << 'time' >> |
        'DateTime.now' | 'Instant' | '.WHERE' | 'nqp::' | 'Telemetry' |
        'BEGIN' \h* 'note' | 'CALLER' | 'Backtrace' | 'Supply' | 'react' | 'Channel' |
        '# ERROR' | '#ERROR' | 'Whatever' \h* 'star' | '.raku' \h* '#' \h* 'OUTPUT' \h* '«' \h* '.'
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

sub finding(%b, Str $program, %raku, %mutsu, Str $kind, Bool $matches-doc = False) {
    {
        kind     => $kind,
        file     => %b<file>,
        line     => %b<line>,
        program  => $program,
        raku-out => %raku<out>,
        mutsu-out => %mutsu<out>,
        mutsu-err => %mutsu<err>,
        mutsu-exit => %mutsu<exit>,
        matches-doc => $matches-doc,
    };
}

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
        say-capped($fh, "--- raku stdout ---", %f<raku-out>);
        say-capped($fh, "--- mutsu stdout (exit { %f<mutsu-exit> }) ---", %f<mutsu-out>);
        if %f<kind> eq 'mutsu-error' && %f<mutsu-err>.trim ne '' {
            $fh.say: "--- mutsu stderr ---";
            $fh.say: %f<mutsu-err>.trim-trailing.lines.head(6).join("\n");
        }
        $fh.say: "";
    }
    $fh.close;
}

sub print-summary(%stat, @findings, Str $report) {
    my $compared = %stat<match> + %stat<mismatch> + %stat<mutsu-crash>;
    say "";
    say "==== doc-diff-harness summary ====";
    say "  skipped (marker):        %stat<skipped-marker>";
    say "  skipped (nondet pattern): %stat<skipped-nondet>";
    say "  skipped (oracle not reproducible): %stat<skipped-oracle-nondet>   (raku disagreed with itself — the noise floor)";
    say "  no oracle (raku unclean): %stat<no-oracle>";
    say "  ------------------------------------";
    say "  compared (raku-clean):   $compared";
    say "    match:                 %stat<match>";
    say "    output mismatch (★real): %stat<mismatch>";
    say "    mutsu error/crash (★real): %stat<mutsu-crash>";
    say "      of which mutsu matches the doc: %stat<matches-doc>   (annotation only — still a real divergence)";
    if $compared > 0 {
        my $real = %stat<mismatch> + %stat<mutsu-crash>;
        my $rate = (100 * $real / $compared).round(0.1);
        say "  ★high-signal divergence: $rate%  ($real/$compared)";
    }
    say "  findings written to:     $report";
}

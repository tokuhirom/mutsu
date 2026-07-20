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
    my $prog-path = "tmp/ddh/prog-{$*PID}.raku";

    my %stat = skipped-marker => 0, skipped-nondet => 0,
               no-oracle => 0, match => 0, mismatch => 0,
               mutsu-crash => 0, raku-drift => 0;
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

        my $r = run-capture('raku', $path, $timeout);
        # Oracle gate: raku must run it cleanly and produce output.
        unless $r<exit> == 0 && $r<out>.chars > 0 && $r<err> !~~ /SORRY/ {
            %stat<no-oracle>++;
            next;
        }

        my $m = run-capture($mutsu, $path, $timeout);

        if normalize($m<out>) eq normalize($r<out>) {
            %stat<match>++;
        }
        elsif $m<exit> != 0 {
            %stat<mutsu-crash>++;
            @findings.push: finding(%b, $program, $r, $m, 'mutsu-error');
        }
        else {
            # Cross-check against the doc's own `# OUTPUT:` annotation. When raku
            # itself no longer matches the doc, this is version drift (raku changed
            # since the doc was written) and mutsu may well match the doc — lower
            # priority than a case where raku and the doc agree but mutsu is wrong.
            my $expected = doc-expected(%b<code>);
            if $expected.defined && normalize($expected) ne normalize($r<out>) {
                %stat<raku-drift>++;
                @findings.push: finding(%b, $program, $r, $m, 'raku-drift-from-doc');
            }
            else {
                %stat<mismatch>++;
                @findings.push: finding(%b, $program, $r, $m, 'output-mismatch');
            }
        }

        if $verbose && $i %% 50 {
            note "  [$i/{ +@blocks }] match=%stat<match> mismatch=%stat<mismatch> crash=%stat<mutsu-crash> no-oracle=%stat<no-oracle>";
        }
    }

    unlink $prog-path if $prog-path.IO.e;
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
sub run-capture(Str $bin, Str $file, Int $timeout) {
    my $proc = run 'timeout', "$timeout", $bin, $file, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    { out => $out, err => $err, exit => $proc.exitcode };
}

#| Normalize output for comparison: strip trailing whitespace on each line and overall.
sub normalize(Str $s) {
    $s.lines.map(*.trim-trailing).join("\n").trim-trailing;
}

sub finding(%b, Str $program, %raku, %mutsu, Str $kind) {
    {
        kind     => $kind,
        file     => %b<file>,
        line     => %b<line>,
        program  => $program,
        raku-out => %raku<out>,
        mutsu-out => %mutsu<out>,
        mutsu-err => %mutsu<err>,
        mutsu-exit => %mutsu<exit>,
    };
}

sub write-report(Str $report, %stat, @findings) {
    my $fh = open $report, :w;
    $fh.say: "# doc-diff-harness report";
    $fh.say: "# stats: ", %stat.sort».fmt('%s=%s').join('  ');
    $fh.say: "";
    for @findings.kv -> $idx, %f {
        $fh.say: "=" x 78;
        $fh.say: "[{ $idx + 1 }] { %f<kind> }  { %f<file> }:{ %f<line> }";
        $fh.say: "--- program ---";
        $fh.say: %f<program>;
        $fh.say: "--- raku stdout ---";
        $fh.say: %f<raku-out>.trim-trailing;
        $fh.say: "--- mutsu stdout (exit { %f<mutsu-exit> }) ---";
        $fh.say: %f<mutsu-out>.trim-trailing;
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
    say "  skipped (nondet):        %stat<skipped-nondet>";
    say "  no oracle (raku unclean): %stat<no-oracle>";
    say "  ------------------------------------";
    say "  compared (raku-clean):   $compared";
    say "    match:                 %stat<match>";
    say "    output mismatch (★real): %stat<mismatch>";
    say "    mutsu error/crash (★real): %stat<mutsu-crash>";
    say "    raku drifted from doc:  %stat<raku-drift>   (lower priority — raku changed since doc)";
    if $compared > 0 {
        my $real = %stat<mismatch> + %stat<mutsu-crash>;
        my $rate = (100 * $real / $compared).round(0.1);
        say "  ★high-signal divergence: $rate%  ($real/$compared)";
    }
    say "  findings written to:     $report";
}

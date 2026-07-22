#!/usr/bin/env raku
# method-coverage.raku — per-type method-coverage matrix (PLAN.md §8.2).
#
# For each raku-doc Type/*.rakudoc file it extracts the documented method /
# routine names and probes BOTH the reference `raku` and `mutsu` with a
# zero-argument dynamic call on a sample value of the type. Only a
# "No such method" outcome counts as a hole — any other error (arity, type
# check, undefined invocant, ...) proves dispatch *found* the method. This
# surfaces the never-called holes that a diff over existing doc examples
# cannot see (a documented method with no example is invisible to §8.1).
#
# raku is the oracle: a (type, method) row is a reportable hole only when
# mutsu lacks it AND raku finds it, so doc drift never floods the report.
#
# A method whose probe crashes/hangs the interpreter mid-batch is retried
# with the crasher excluded (PROBE/RES line protocol), and reported with
# status `crash` — those rows double as §8.3 robustness findings.
#
# Usage:
#   raku scripts/method-coverage.raku [--mutsu=PATH] [--timeout=N]
#                                     [--report=FILE] [--summary=FILE]
#                                     [FILES-OR-DIRS ...]
# Defaults: mutsu=target/debug/mutsu, timeout=30s/type, corpus=raku-doc/doc/Type.

# Methods never worth probing: process-fatal, filesystem-mutating, or
# stdout-noisy calls. Reported as `skipped`.
my constant %DENY = <
    exit run shell spurt unlink rmdir mkdir rename move copy symlink link
    chdir chmod kill print put say note prompt sleep sleep-until bail-out
    done-testing plan diag flunk exec QuitProcess restore
>.map(* => True).Hash;

# Sample invocants: a defined instance where one is cheap to make (instance
# methods dispatch more faithfully than on a type object), else the type
# object via ::('Name').
my constant %SAMPLE =
    'Str'      => '""',
    'Int'      => '0',
    'Num'      => '0e0',
    'Rat'      => '0.5',
    'FatRat'   => 'FatRat.new(1, 2)',
    'Complex'  => '1i',
    'Bool'     => 'True',
    'Array'    => '[1]',
    'List'     => '(1,)',
    'Hash'     => '%(:a(1))',
    'Map'      => 'Map.new((:a(1)))',
    'Set'      => 'set(1)',
    'SetHash'  => 'SetHash.new(1)',
    'Bag'      => 'bag(1)',
    'BagHash'  => 'BagHash.new(1)',
    'Mix'      => 'mix(1)',
    'MixHash'  => 'MixHash.new(1)',
    'Range'    => '(1..2)',
    'Pair'     => '(:a(1))',
    'Seq'      => '(1, 2).Seq',
    'Slip'     => '(1, 2).Slip',
    'Date'     => 'Date.new(2000, 1, 1)',
    'DateTime' => 'DateTime.new(year => 2000)',
    'Duration' => 'Duration.new(1)',
    'Instant'  => 'now',
    'Version'  => 'v1.2',
    'IO::Path' => '"/tmp".IO',
    'Regex'    => 'rx/a/',
    'Match'    => '("a" ~~ rx/a/)',
    'Junction' => 'any(1, 2)',
;

sub MAIN(
    *@paths,
    Str  :$mutsu   = 'target/debug/mutsu',
    Int  :$timeout = 30,
    Str  :$report  = 'tmp/method-coverage.tsv',
    Str  :$summary = 'tmp/method-coverage-summary.md',
) {
    my @files = collect-files(@paths);
    note "Scanning { +@files } Type .rakudoc files ...";

    mkdir 'tmp/mcov' unless 'tmp/mcov'.IO.d;
    my $prog-path = "tmp/mcov/probe-{$*PID}.raku";

    my @rows;                     # [type, method, mutsu-status, raku-status]
    for @files.sort -> $file {
        my $type = type-name($file);
        my @methods = extract-methods($file);
        next unless @methods;
        my (%mutsu-res, %raku-res);
        %mutsu-res = probe($mutsu, $type, @methods, $prog-path, $timeout);
        %raku-res  = probe('raku', $type, @methods, $prog-path, $timeout);
        for @methods -> $m {
            @rows.push: [$type, $m, %mutsu-res{$m} // 'crash', %raku-res{$m} // 'crash'];
        }
        my $holes = @methods.grep({ hole(%mutsu-res{$_} // 'crash', %raku-res{$_} // 'crash') });
        note "  $type: { +@methods } documented, { +$holes } holes";
    }

    spurt $report, @rows.map(*.join("\t")).join("\n") ~ "\n";
    write-summary($summary, @rows);
    note "Report: $report";
    note "Summary: $summary";
}

sub hole($mutsu-status, $raku-status) {
    $mutsu-status eq 'nomethod' | 'crash' and $raku-status eq 'ok' | 'err';
}

sub collect-files(@paths) {
    my @roots = @paths ?? @paths !! ('raku-doc/doc/Type',);
    my @files;
    for @roots -> $p {
        if $p.IO.d {
            @files.append: walk($p.IO);
        }
        elsif $p.IO.f {
            @files.push: $p.IO;
        }
    }
    @files
}

sub walk(IO::Path $dir) {
    my @out;
    for $dir.dir.sort -> $e {
        if $e.d { @out.append: walk($e) }
        elsif $e.extension eq 'rakudoc' { @out.push: $e }
    }
    @out
}

sub type-name(IO::Path $file) {
    my $rel = $file.Str;
    $rel ~~ s/ .* '/Type/' //;
    $rel ~~ s/ '.rakudoc' $ //;
    $rel.subst('/', '::', :g)
}

sub extract-methods(IO::Path $file) {
    my %seen;
    my @methods;
    for $file.lines -> $line {
        next unless $line ~~ /^ '=head2' \s+ [ 'multi' \s+ ]? [ 'method' | 'routine' ] \s+ (\S+) \s* $/;
        my $name = ~$0;
        # Only plain method identifiers: no markup, no operator forms.
        next unless $name ~~ /^ <[a..z A..Z]> <[a..z A..Z 0..9 _ \-]>* <[! ?]>? $/;
        next if %seen{$name}++;
        @methods.push: $name;
    }
    @methods
}

# Run one interpreter over a type's method list; returns method => status.
# status: ok | err | nomethod | skipped | crash | notype
sub probe(Str $bin, Str $type, @methods, Str $prog-path, Int $timeout) {
    my %res;
    for @methods.grep({ %DENY{$_} }) -> $m { %res{$m} = 'skipped' }
    my @todo = @methods.grep({ !%DENY{$_} });
    my %skip;
    # Retry loop: a crash mid-batch marks the dangling PROBE method as crashed
    # and reruns the remainder without it.
    for ^(1 + @todo.elems) {
        my @batch = @todo.grep({ !%skip{$_} && !(%res{$_}:exists) });
        last unless @batch;
        my $src = probe-program($type, @batch);
        spurt $prog-path, $src;
        my $proc = run 'timeout', $timeout.Str, |$bin.words, $prog-path,
                   :out, :err, :in;
        # `.close` on a Proc pipe returns the Proc; in sink context that sunk
        # Proc throws X::Proc::Unsuccessful on a nonzero exit — bind it away.
        my $ = $proc.in.close;
        my $out = $proc.out.slurp(:close);
        my $ = $proc.err.slurp(:close);
        my $ = $proc.exitcode;
        my $pending = '';
        for $out.lines {
            when /^ 'PROBE ' (\S+) $/ { $pending = ~$0 }
            when /^ 'RES ' (\S+) ' ' (\S+) $/ { %res{~$0} = ~$1; $pending = '' }
            when /^ 'NOTYPE' $/ {
                %res{$_} = 'notype' for @batch;
                return %res;
            }
        }
        if $pending {
            %res{$pending} = 'crash';
            %skip{$pending} = True;
        }
        else {
            last;   # completed (or produced nothing parseable — avoid looping)
        }
    }
    %res
}

sub probe-program(Str $type, @methods) {
    my $sample = %SAMPLE{$type} // "::('$type')";
    my $names = @methods.map({ "'$_'" }).join(', ');
    q:to/END/.subst('%SAMPLE%', $sample).subst('%TYPE%', $type).subst('%NAMES%', $names);
    my $sample;
    {
        $sample = %SAMPLE%;
        CATCH { default { say "NOTYPE"; exit 0 } }
    }
    # mutsu's arity-keyed dispatch reports an arity miss as "No such method",
    # so a zero-arg probe alone would flag every 1+-arg method as a hole.
    # Retry across several argument shapes; the method is a hole only when
    # EVERY shape answers "no such method".
    my @shapes = \(), \(0), \('a'), \(0, 0), \('a', 'b'), \(Any);
    for (%NAMES%) -> $m {
        say "PROBE $m";
        my $status = 'nomethod';
        for @shapes -> $c {
            my $s = 'ok';
            {
                my $ = $sample."$m"(|$c);
                CATCH { default {
                    $s = .message ~~ /:i 'no such method'/ ?? 'nomethod' !! 'err';
                } }
            }
            if $s ne 'nomethod' {
                $status = $s;
                last;
            }
        }
        say "RES $m $status";
    }
    END
}

sub write-summary(Str $path, @rows) {
    my %by-type;
    for @rows -> [$type, $m, $ms, $rs] {
        %by-type{$type}.push: [$m, $ms, $rs];
    }
    my @lines = '# Per-type method-coverage matrix (PLAN.md §8.2)', '',
        'Documented = `=head2 method/routine` entries in the type\'s rakudoc.',
        'A hole = mutsu answers "No such method" (or crashes) where raku dispatches.',
        '', '| Type | Documented | Implemented | Holes |', '|---|---|---|---|';
    my @hole-lines;
    for %by-type.keys.sort -> $type {
        my @ms = %by-type{$type}.list;
        my $doc = +@ms;
        my @holes = @ms.grep({ hole(.[1], .[2]) });
        my $impl = +@ms.grep({ .[1] eq 'ok' | 'err' });
        @lines.push: "| $type | $doc | $impl | { +@holes } |";
        if @holes {
            @hole-lines.push: "- **$type**: " ~ @holes.map({
                .[1] eq 'crash' ?? "{.[0]} (CRASH)" !! .[0]
            }).join(', ');
        }
    }
    @lines.append: '', '## Holes by type', '', |@hole-lines, '';
    spurt $path, @lines.join("\n");
}

use Test;

# A resumable control signal (e.g. `warn`) raised from inside a callable invoked
# *indirectly* — `$code.(...)` / `&warn.(...)` — must let an enclosing `CONTROL {}`
# `.resume` after the call, exactly like a direct `warn`. The `CallOnValue` /
# `CallOnCodeVar` opcodes did not record a resume point before propagating the
# signal (unlike the direct-call `ExecCall` path), so `.resume` had nowhere to go
# and the program silently terminated. (Template::Mustache logs warnings through
# `$!routines{$level} = &warn; $_.(...)`, hitting exactly this path.)

plan 5;

# --- direct warn under CONTROL (already worked) ---
{
    my $out = '';
    CONTROL { default { $out ~= "[{.message}]"; .resume; } }
    warn "direct";
    is $out, '[direct]', 'direct warn is caught and resumed';
    pass 'execution continues after a direct warn';
}

# --- indirect warn via a &warn reference ---
{
    my $out = '';
    CONTROL { default { $out ~= "[{.message}]"; .resume; } }
    my $w = &warn;
    $w.("indirect");
    is $out, '[indirect]', 'indirect warn via &warn.() is caught and resumed';
    pass 'execution continues after an indirect warn';
}

# --- two indirect warns both resume (accumulation) ---
{
    my @seen;
    CONTROL { default { @seen.push(.message); .resume; } }
    my $w = &warn;
    $w.("one");
    $w.("two");
    is @seen.join(','), 'one,two', 'consecutive indirect warns each resume';
}

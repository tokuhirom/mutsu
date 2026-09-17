use Test;

# Regression pin for issue #8645, Case A: mutsu stores a `$*`-twigil dynamic
# variable under two env keys, the sigilless form (`*OUT`) and the sigilled
# form (`$*OUT`), kept in sync by the canonical by-name writer
# (`Interpreter::set_env_with_main_alias`). `say`/`print`/`note` resolve their
# destination handle through the SIGILLED key (`write_to_named_handle`'s
# `get_dynamic_handle`), while an ordinary variable read (`.^name`, etc.)
# resolves through the compiled SIGILLESS local/env slot.
#
# An `is raw`/`is rw` parameter aliasing a caller's `$*OUT` writes its final
# value back into the caller's env by name -- through `apply_rw_bindings_to_env`
# (plain sub calls, closures) or the `rw_writeback` merge in
# `vm_method_dispatch.rs` (compiled method calls, including `CALL-ME`) -- and
# neither writer originally mirrored the twigil alias, so only ONE spelling
# was updated. `say`/`print` kept writing to the ORIGINAL handle even though
# every other read of `$*OUT` correctly reported the new value.

plan 4;

class Trap {
    has str @!text;
    method print(*@_ --> True) { if @_.join -> $t { @!text.push: $t } }
    method say(*@_ --> True)   { if @_.join -> $t { @!text.push: $t ~ "\n" } }
    method text(--> str) { @!text.join }
    multi method CALL-ME(Trap:U: $one is raw) { $one = self.new; }
}

# Case A: a `CALL-ME`-dispatched raw parameter (a compiled method call).
{
    my $out;
    {
        $out = Trap(my $*OUT);
        say "Hello world!";
    }
    is $out.text, "Hello world!\n", 'CALL-ME raw-param writeback updates $*OUT for say';
}

# The same idiom through a plain sub call (`apply_rw_bindings_to_env`).
sub bind-out($one is raw) { $one = Trap.new }
{
    my $out2;
    {
        $out2 = do {
            bind-out(my $*OUT);
            say "via sub";
            $*OUT;
        };
    }
    is $out2.text, "via sub\n", 'plain-sub raw-param writeback updates $*OUT for say';
}

# The same idiom through a closure (`vm_closure_dispatch.rs`'s writeback scan).
my $blk = -> $one is raw { $one = Trap.new };
{
    my $out3;
    {
        $out3 = do {
            $blk(my $*OUT);
            say "via closure";
            $*OUT;
        };
    }
    is $out3.text, "via closure\n", 'closure raw-param writeback updates $*OUT for say';
}

# Both spellings must agree after the writeback, not just the sigilled one.
{
    my $seen;
    {
        bind-out(my $*OUT);
        $seen = $*OUT.^name;
    }
    is $seen, 'Trap', '$*OUT.^name reflects the raw-param writeback too';
}

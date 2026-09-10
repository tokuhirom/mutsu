use Test;

plan 2;

# A lexical holding a Sub must survive a nested method call made in the same
# routine -- even when an OUTER, still-live invocation of that same routine
# holds a different Sub under the same name.
#
# The env->locals write-through that keeps a caller's slots coherent after a
# method call is driven by "did the callee change this value?". That test
# (`cheaply_unchanged`) had no arm for `Sub`, so a Sub-valued env entry the
# callee had merely INHERITED from the flattened overlay was reported as
# changed, and the write-through then replaced the live local slot with the env
# copy -- which, in a routine re-entered through a Sub stored in a data
# structure, still held the OUTER frame's routine. Calling it re-entered the
# wrong closure, and each wrong dispatch recursed one level deeper until the
# process died with a stack overflow (#7729, found in Template::Jinja2's
# `{% call %}` blocks).

class Ctx {
    has @.stack;
    method resolve($n) {
        for @!stack.reverse -> %s { return %s{$n} if %s{$n}:exists }
        Nil;
    }
    method push-scope(%v) { @!stack.push: %v }
    # Deliberately not a pure accessor: the callee has to write its own frame
    # for the caller-env merge (and hence the write-through) to run at all.
    method depth() { my $d = @!stack.elems; $d }
}

class Renderer {
    method !install($ctx) {
        my $renderer = self;
        # The outer routine's `$callable` ends up holding this one ...
        my $macro = sub (*@args) {
            $ctx.push-scope({});
            '[[' ~ $renderer!Renderer::call('caller', $ctx, ['data']) ~ ']]';
        };
        # ... while the inner, re-entrant invocation resolves this one.
        my $caller-fn = sub (*@args) { "CALLED(@args[])" };
        $ctx.push-scope({ test => $macro, caller => $caller-fn });
        $renderer!Renderer::call('test', $ctx, []);
    }

    method !call($name, $ctx, @a) {
        my $callable = $ctx.resolve($name);
        # A nested method call between the assignment and the use.
        my $ignored = $ctx.depth();
        # The smartmatch flushes this frame's locals into env, which is how the
        # outer invocation's Sub becomes visible to the inner one's overlay.
        return $callable ~~ Callable ?? $callable(|@a) !! 'NOT-CALLABLE';
    }

    method run { my $ctx = Ctx.new; $ctx.push-scope({}); self!install($ctx) }
}

is Renderer.new.run, '[[CALLED(data)]]',
    're-entrant routine calls the Sub IT resolved, not its caller\'s';

# The same shape without the recursion: a Sub-valued lexical must not be
# rewound by an intervening method call.
class Plain {
    method depth() { my $d = 1; $d }
    method go() {
        my $first = sub { 'first' };
        my $chosen = $first;
        my $second = sub { 'second' };
        $chosen = $second;
        my $ignored = self.depth();
        return $chosen ~~ Callable ?? $chosen() !! 'NOT-CALLABLE';
    }
}

is Plain.new.go, 'second', 'a reassigned Sub lexical survives a nested method call';

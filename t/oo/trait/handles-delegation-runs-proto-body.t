use Test;

# A `handles` delegation into a proto method with a body runs that body,
# including an early `return` before `{*}` -- also when the delegate
# attribute has been passed as a named argument (`:$!logger`), which leaves it
# in a container. Template::Mustache's log level filter depends on it.

plan 4;

my @calls;
class T {
    class Logger {
        has Int $.level = 1;
        proto method log(Int :$level = 2, |) {
            @calls.push: 'proto';
            return unless $!level >= $level;
            {*}
        }
        multi method log(Int :$level = 2, *@msgs) { @calls.push: "logged @msgs[]" }
    }
    has $.logger handles <log>;
    submethod TWEAK { $!logger //= Logger.new }
    class Holder { has $!logger; submethod BUILD(:$!logger) { } }
    method render {
        my $h = Holder.new: :$!logger;
        self.log: :level(5), 'inside';
        self.log: :level(0), 'kept';
        'rendered'
    }
}

my $t = T.new;
is $t.render, 'rendered', 'render runs';
is-deeply @calls, ['proto', 'proto', 'logged kept'], 'proto body filters the delegated call';
@calls = ();
$t.log: :level(5), 'outside';
is-deeply @calls, ['proto'], 'proto body runs for a delegated call from outside';
$t.log: :level(1), 'ok';
is-deeply @calls, ['proto', 'proto', 'logged ok'], 'the multi runs when the filter passes';

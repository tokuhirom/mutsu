use Test;

# `%*ENV` is built on its first lookup, not when the interpreter starts
# (#19224). Its observable behaviour must not change: values are `val()`-ed,
# writes stick, and every frame, routine, thread and child process sees the
# one process-level hash.

plan 9;

my $exe = $*EXECUTABLE.absolute;

sub run-with(Str $code, *%env) {
    my %e = %*ENV;
    %e{.key} = .value for %env;
    run($exe, '-e', $code, :out, :env(%e)).out.slurp(:close).lines
}

is-deeply run-with(q[say %*ENV<MUTSU_LAZY_ENV_N>.^name; say %*ENV<MUTSU_LAZY_ENV_S>.^name],
        MUTSU_LAZY_ENV_N => '42', MUTSU_LAZY_ENV_S => 'hello'),
    ('IntStr', 'Str'),
    'values are val()-ed on the first lookup';

is run-with(q[say %*ENV.keys.grep('MUTSU_LAZY_ENV_K').elems],
        MUTSU_LAZY_ENV_K => 'x')[0], '1',
    'the first lookup can be a whole-hash method';

is run-with(q[sub f { %*ENV<MUTSU_LAZY_ENV_A> }; say f()],
        MUTSU_LAZY_ENV_A => 'in-sub')[0], 'in-sub',
    'the first lookup can come from inside a routine';

is run-with(q[say await start { %*ENV<MUTSU_LAZY_ENV_T> }],
        MUTSU_LAZY_ENV_T => 'in-thread')[0], 'in-thread',
    'the first lookup can come from another thread';

%*ENV<MUTSU_LAZY_ENV_W> = 'written';
is %*ENV<MUTSU_LAZY_ENV_W>, 'written', 'a write is seen by the next read';
sub g { %*ENV<MUTSU_LAZY_ENV_W> }
is g(), 'written', 'a write is seen inside a routine';
is (await start { %*ENV<MUTSU_LAZY_ENV_W> }), 'written', 'a write is seen by a thread';
is run('sh', '-c', 'echo $MUTSU_LAZY_ENV_W', :out).out.slurp(:close).chomp, 'written',
    'a write reaches a child process';
ok %*ENV === %*ENV, 'every lookup yields the same hash';

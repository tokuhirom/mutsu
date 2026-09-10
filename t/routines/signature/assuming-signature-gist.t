use Test;

# A primed (`.assuming`) sub's signature reflects the priming: a bound named
# parameter is shown with the bound value as its default and becomes optional;
# unprimed named parameters keep their own state.

plan 15;

sub named2(:$a, :$b) {}
is &named2.assuming(:a).signature.gist, '(:$a = Bool::True, :$b)',
    'primed named param shows bound value as default';
is &named2.assuming(:b).signature.gist, '(:$a, :$b = Bool::True)',
    'unprimed named param keeps its state';

sub required(:$a!) {}
is &required.assuming(:a).signature.gist, '(:$a = Bool::True)',
    'required named loses its ! when primed';

sub withdefault(:$a = 2, :$b = 4) {}
is &withdefault.assuming(:a).signature.gist, '(:$a = Bool::True, :$b = 4)',
    'primed value replaces the original default; other keeps it';

sub nonbool(:$a) {}
is &nonbool.assuming(a => 42).signature.gist, '(:$a = 42)',
    'bound value other than True is rendered';

# Alias names: :b(:c($a)) can be bound by either b or c.
sub aliased(:b(:c($a))!) {}
is &aliased.assuming(:b).signature.gist, '(:b(:c($a)) = Bool::True)',
    'primed by primary alias name';
is &aliased.assuming(:c).signature.gist, '(:b(:c($a)) = Bool::True)',
    'primed by nested alias name';

# Positional priming removes consumed params and resolves type captures.
sub caps(::T $a, T $b, T :$c) {}
is &caps.assuming(1, 1).signature.gist, '(Int :$c)',
    'type capture resolves for the remaining named param';

my $primed = &caps.assuming(1, 1);
nok $primed.can('Failure'), 'valid type-capture priming does not fail the bind';

# Rakudo defers type-capture constraint checks, so priming a capture-constrained
# param with a mismatching value does NOT fail at priming time.
sub caps2(::T $a, T $b) {}
nok &caps2.assuming(1, "x").can('Failure'),
    'capture-constrained bind is deferred, not eagerly failed';

# A genuine (non-capture) positional type mismatch still fails the bind.
sub typed(Int $a) {}
ok &typed.assuming("str").can('Failure'), 'real type mismatch fails the bind';
nok &typed.assuming(3).can('Failure'), 'matching positional binds fine';

# Priming still calls through correctly.
sub add($x, $y) { $x + $y }
is &add.assuming(10)(5), 15, 'primed positional then called';

sub greet(:$greeting = "Hi", :$name) { "$greeting, $name!" }
is &greet.assuming(:greeting<Yo>)(:name<Bee>), "Yo, Bee!", 'primed named then called';
is &greet.assuming(:name<Ann>).signature.gist, '(:$greeting = "Hi", :$name = "Ann")',
    'string default preserved, primed name defaulted';

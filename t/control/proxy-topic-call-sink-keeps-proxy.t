use Test;

# A statement that calls the topic (`.(args);`, `.(Type) for @blocks`) does not
# FETCH the Proxy the call hands back, as in rakudo, while a call through a
# variable (`$b(args);`) still does (#17500: Red's legacy `^migrate` runs
# `.(Type) for @blocks` over `{ .first = .original * 2 }`, whose `is rw`
# accessor answers a write-only Proxy).

plan 7;

my @log;
my \Type = Metamodel::ClassHOW.new_type: :name<T>;
Type.^add_method: 'a', method () is rw {
    Proxy.new: FETCH => method { @log.push('fetch'); 1 }, STORE => method (\data) { @log.push('store') };
}
Type.^compose;
my @blocks = { .a = 2 },;

.(Type) for @blocks;
is-deeply @log, ['store'], 'statement-modifier for over topic calls';
@log = ();

for @blocks { .(Type) }
is-deeply @log, ['store'], 'a for body calling the topic';
@log = ();

given @blocks[0] { .(Type); my $after = 1 }
is-deeply @log, ['store'], 'a given body calling the topic';
@log = ();

my $b = @blocks[0];
$b(Type);
is-deeply @log, ['store', 'fetch'], 'a call through a variable is still FETCHed when sunk';
@log = ();

@blocks[0](Type);
is-deeply @log, ['store', 'fetch'], 'and so is a call on an element';
@log = ();

my $sunk = 0;
class Sinky { method sink { $sunk++ } }
my @makers = { Sinky.new },;
.() for @makers;
is $sunk, 1, 'a user sink method still runs for a sunk topic call';

lives-ok { .(Type) for @blocks }, 'a write-only Proxy never FETCHed does not die';

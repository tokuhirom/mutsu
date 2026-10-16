use Test;

# The class methods of `Supply`, `CompUnit::RepositoryRegistry`,
# `Encoding::Registry` and the sockets are rows of the method table, found by
# their owner for the type object (ADR-11276 §9.122).

plan 13;

is Supply.from-list(1, 2, 3).list.join(','), '1,2,3', 'Supply.from-list';
my $s = Supply.on-demand(-> $p { $p.emit(42); $p.done });
is $s.list.join(','), '42', 'Supply.on-demand';
dies-ok { Supply.on-demand(42) }, 'Supply.on-demand wants a callable';
throws-like { Supply.from-list(1).on-demand(-> $p { }) }, Exception,
    message => /'must be a type object of type'/, 'on-demand refuses an instance';

# The name it answers is #13651.
isa-ok Encoding::Registry.find('utf8'), Encoding, 'Encoding::Registry.find';
is Encoding::Registry.find('UTF-8').name, Encoding::Registry.find('utf8').name,
    'find by an alternative name';

isa-ok CompUnit::RepositoryRegistry.repository-for-name('site'),
    CompUnit::Repository::Installation, 'repository-for-name of site';
isa-ok CompUnit::RepositoryRegistry.repository-for-spec('file#lib'),
    CompUnit::Repository::FileSystem, 'repository-for-spec of a file# spec';
isa-ok CompUnit::RepositoryRegistry.repository-for-spec('inst#lib'),
    CompUnit::Repository::Installation, 'repository-for-spec of an inst# spec';
nok CompUnit::RepositoryRegistry.repository-for-name('nonesuch').defined,
    'repository-for-name of an unknown name';

my $server = IO::Socket::INET.listen('127.0.0.1', 0);
ok $server.localport > 0, 'IO::Socket::INET.listen on port 0';
my $client = IO::Socket::INET.connect('127.0.0.1', $server.localport);
ok $client.defined, 'IO::Socket::INET.connect';
$client.close;
$server.close;

my $tap = IO::Socket::Async.listen('127.0.0.1', 0).tap(-> $conn { $conn.close });
ok (await $tap.socket-port) > 0, 'IO::Socket::Async.listen on port 0';
$tap.close;

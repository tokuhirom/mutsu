use v6.d;
unit module Log::Timeline::Raku::LogTimelineSchema;
use Log::Timeline::Model;

class FileOpen does Log::Timeline::Task['Raku', 'File IO', 'File Open'] { }
class AsyncSocketListen does Log::Timeline::Task['Raku', 'Async Socket', 'Listen'] { }
class AsyncSocketIncoming does Log::Timeline::Task['Raku', 'Async Socket', 'Incoming Connection'] { }
class AsyncSocketConnect does Log::Timeline::Task['Raku', 'Async Socket', 'Connect'] { }
class AsyncSocketEstablish does Log::Timeline::Task['Raku', 'Async Socket', 'Establish Connection'] { }
class RunThread does Log::Timeline::Task['Raku', 'Concurrency', 'Thread'] { }
class RunProcess does Log::Timeline::Task['Raku', 'Concurrency', 'Run Process'] { }
class Start does Log::Timeline::Task['Raku', 'Concurrency', 'Start'] { }
class StartQueued does Log::Timeline::Task['Raku', 'Concurrency', 'Start Queued'] { }
class Await does Log::Timeline::Task['Raku', 'Concurrency', 'Await'] { }

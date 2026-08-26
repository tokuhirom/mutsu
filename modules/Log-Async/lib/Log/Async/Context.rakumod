
unit class Log::Async::Context;

has @!backtrace;
has $.file;
has $.line;

method generate is hidden-from-backtrace {
  my $exception = Exception.new;
  try $exception.throw;
  if ($!) {
    @!backtrace = $exception.backtrace.grep({ !.is-hidden and !.is-setting });
    @!backtrace.shift; # remove exception creation
    @!backtrace.shift while @!backtrace[0].file.Str.contains( 'Log/Async' );
  } else {
    die "error throwing exception";
  }
  $!file = @!backtrace[0].file;
  $!line = @!backtrace[0].line;
  return self;
}

method stack is hidden-from-backtrace {
  @!backtrace;
}

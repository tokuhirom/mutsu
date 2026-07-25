#| Records the positional arguments a `use` line passes to `sub EXPORT`, and
#| exports them as `&use-args` so a test can inspect them.
sub EXPORT(*@args) {
    my @seen = @args;
    Map.new: '&use-args' => sub { @seen }
}

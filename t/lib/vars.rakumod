module vars { }

sub EXPORT(*@vars) {
    my %export;
    for @vars -> $name {
        %export{$name} := $name.starts-with('$')
          ?? my $
          !! $name.starts-with('@') ?? [] !! %();
    }
    %export
}

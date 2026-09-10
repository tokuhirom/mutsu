use Test;

plan 2;

{
    my $sunk = False;
    (class {
      method STORE(*@args) {
      }

      method foo() {
          self;
      }

      method sink() {
          $sunk = True;
      }
    }.new).=foo;

    is $sunk, False, "(.expr).=method does not sink method result";
}

{
    my $sunk = False;
    (class {
      method STORE(*@args) {
      }

      method foo() {
          self;
      }

      method sink() {
          $sunk = True;
      }
    }.new) .= foo;

    is $sunk, False, "(.expr) .= method does not sink method result";
}

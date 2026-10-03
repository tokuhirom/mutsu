my role Kind { method kind { 'words' } }
my class Helper { method help { 'helped' } }
my sub greet() { 'hi' }

my sub EXPORT(*@names) {
    Map.new: @names
      ?? @names.map({ $_ => UNIT::{$_} })
      !! UNIT::.grep({ .key eq 'Kind' || .key eq '&greet' })
}

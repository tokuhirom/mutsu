use Test;

plan 3;

my @utf8 =
  utf8.new(100,111,103),
  utf8.new(106,117,109,112,115),
  utf8.new(111,118,101,114,13),
  utf8.new(10,116,104,101),
  utf8.new(108,97,122,121),
  utf8.new(102,111,120)
;
my @utf16 =
  utf16.new(100,111,103),
  utf16.new(106,117,109,112,115),
  utf16.new(111,118,101,114,13),
  utf16.new(10,116,104,101),
  utf16.new(108,97,122,121),
  utf16.new(102,111,120)
;
my @str = <<do gjump sover "\r\nth" elaz yfo x>>;

is-deeply Supply.from-list(@utf8).decode.list, @str,
  "Supply.decode handles utf8 chunks with grapheme carry";

is-deeply Supply.from-list(@utf16).decode("utf16").list, @str,
  "Supply.decode handles utf16 chunks with grapheme carry";

is Supply.from-list(utf8.new(13),utf8.new(10)).decode.Seq[0].chars, 1,
  "Supply.decode keeps CRLF as one grapheme across chunk boundary";

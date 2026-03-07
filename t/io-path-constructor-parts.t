use Test;

plan 3;

is IO::Path.new(:basename<bar.txt>).path, "bar.txt", "IO::Path.new accepts :basename";
is IO::Path.new(:dirname</foo>, :basename<bar.txt>).path,
   "/foo/bar.txt",
   "IO::Path.new composes path from :dirname and :basename";
is IO::Path.new(:dirname</foo>, :basename<bar.txt>).cleanup.path,
   "/foo/bar.txt",
   "cleanup keeps normalized composed path";

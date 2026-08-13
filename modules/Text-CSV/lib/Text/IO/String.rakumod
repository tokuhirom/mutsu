use Slang::Tuxic; # Need it for space before parenthesis

unit class Text::IO::String is IO::Handle;

has      $.nl-in   is rw;
has      $.nl-out  is rw;
has Bool $.ro      is rw is default(False);
has Str  $!str;
has Str  @!content;

# my $fh = Text::IO::String.new ($foo);
multi method new (Str $str! is rw, *%init) {
    my \obj = self.new ($str.Str, |%init);
    obj.bind-str ($str);
    obj;
    }

# my $fh = Text::IO::String.new ("foo");
multi method new (Str $str!, *%init) {
    my \obj = self.bless;
    obj.nl-in  = $*IN.nl-in;
    obj.nl-out = $*OUT.nl-out;
    obj.ro     = %init<ro>     if %init<ro>:exists;
    obj.nl-in  = %init<nl-in>  if %init<nl-in>:exists;
    obj.nl-out = %init<nl-out> if %init<nl-out>:exists;
    obj.print ($str);
    obj;
    }

method bind-str (Str $s is rw) {
    $!str := $s;
    }

method print (*@what) {
    if (my Str $str = @what.join ("")) {
        my Str @x = $str eq "" || !$.nl-in.defined
            ??  $str
            !! |$str.split ($.nl-in, :v).map (-> $a, $b? --> Str { $a ~ ($b // "") });
        @x.elems > 1 && @x.tail eq "" and @x.pop;
        @!content.push: |@x;
        }
    self;
    }

method print-nl {
    self.print ($.nl-out);
    }

method get {
    @!content ?? @!content.shift !! Str;
    }

method close {
    $!str.defined && !$.ro and $!str = ~ self;
    }

method Str {
    @!content.join ("");
    }

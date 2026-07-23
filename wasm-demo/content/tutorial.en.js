/**
 * English prose for the tutorial.  Code and expected output live in
 * content/lessons.txt; the keys here must match its "<chapter>/<lesson>" ids.
 *
 * The explanations follow the official Raku documentation (Raku/doc), vendored
 * in this repository under raku-doc/ and used under the Artistic License 2.0.
 */

export default {
  title: 'A tour of Raku',
  intro: 'Every lesson is a real program. Edit it, press Run, and the ' +
    'interpreter in your browser executes it — nothing is sent to a server.',
  source: 'Written against the official Raku documentation (Raku/doc).',

  chapters: {
    basics: 'Getting started',
    operators: 'Operators',
    control: 'Control flow',
    lists: 'Lists and hashes',
    subs: 'Subs and signatures',
    objects: 'Objects',
    regex: 'Regexes and grammars',
    concurrency: 'Concurrency',
    'wrapping-up': 'Wrapping up',
  },

  lessons: {
    'basics/hello': {
      title: 'Hello, World',
      body: `
        <p>A Raku program is a sequence of statements, each ended by a semicolon.
        <code>say</code> prints its argument followed by a newline.</p>
        <p>Almost everything in Raku is an object with methods, including
        literals: <code>"Hello".uc</code> calls the <code>uc</code> (uppercase)
        method on a string literal.</p>
        <p>Comments start with <code>#</code> and run to the end of the line.</p>`,
    },
    'basics/variables': {
      title: 'Variables and sigils',
      body: `
        <p><code>my</code> declares a lexical variable — one visible from its
        declaration to the end of the enclosing block.</p>
        <p>The leading punctuation is called a <strong>sigil</strong>, and it tells
        you the shape of what is inside:</p>
        <ul>
          <li><code>$</code> — a single item (any object at all, including a list
              treated as one thing)</li>
          <li><code>@</code> — a positional container, indexed by number</li>
          <li><code>%</code> — an associative container, indexed by key</li>
          <li><code>&amp;</code> — a piece of code</li>
        </ul>
        <p><code>&lt;Raku Perl Rust&gt;</code> is the quote-word list: a whitespace
        separated list of strings, without quotes or commas.</p>`,
    },
    'basics/types': {
      title: 'Types',
      body: `
        <p>Raku is gradually typed: you can leave types off, and they are still
        there at run time. <code>.^name</code> asks an object's metaobject for its
        type name — the <code>.^</code> operator is a method call on the
        <em>metaclass</em> rather than on the object.</p>
        <p><code>~~</code> is the smartmatch operator. Against a type it answers
        "does this value conform to that type?". Types form a tree, so an
        <code>Int</code> is also a <code>Cool</code> (the "convenient object
        language" types that helpfully convert between numbers and strings), an
        <code>Any</code>, and a <code>Mu</code>.</p>`,
    },
    'basics/interpolation': {
      title: 'String interpolation',
      body: `
        <p>Double-quoted strings interpolate variables. They also interpolate
        <strong>arbitrary code</strong> in curly braces, which is how most
        formatting gets done — there is no separate template mini-language.</p>
        <p>An array interpolates its elements when it is followed by a subscript,
        so <code>"@xs[]"</code> means "all of <code>@xs</code>, space separated".
        A bare <code>@xs</code> inside a string stays literal, so email addresses
        do not surprise you.</p>
        <p>Single-quoted strings interpolate nothing at all.</p>`,
    },

    'operators/numbers': {
      title: 'Numbers that behave',
      body: `
        <p>Division of two integers produces a <code>Rat</code> — an exact
        rational, stored as a numerator and a denominator. That is why
        <code>0.1 + 0.2 == 0.3</code> is <code>True</code> in Raku while it is
        false in most languages: no binary floating point was involved.</p>
        <p><code>div</code> is integer division, <code>%</code> is modulus, and
        <code>%%</code> asks "is divisible by" directly, so you never write
        <code>x % n == 0</code> again.</p>
        <p><code>Int</code> is arbitrary precision — <code>2 ** 100</code> is
        exact, not an overflow.</p>`,
    },
    'operators/strings': {
      title: 'Strings and comparison',
      body: `
        <p>Raku keeps numeric and string operators separate, so an operator's
        meaning never depends on its operands: <code>~</code> concatenates,
        <code>+</code> adds, <code>x</code> repeats a string, and <code>eq lt
        gt</code> compare as strings while <code>== &lt; &gt;</code> compare as
        numbers.</p>
        <p>The three-way comparators return an enum rather than a number:
        <code>&lt;=&gt;</code> compares numerically and <code>cmp</code> compares
        by type-appropriate order, both yielding <code>Less</code>,
        <code>Same</code>, or <code>More</code>.</p>`,
    },
    'operators/ranges': {
      title: 'Ranges and sequences',
      body: `
        <p><code>..</code> builds a <code>Range</code>. A caret on either side
        excludes that endpoint: <code>1..^5</code> is 1 through 4. Ranges work on
        strings too, incrementing them the way an odometer would.</p>
        <p><code>...</code> is the sequence operator. Give it the first few
        elements and it infers the rule — arithmetic, geometric, or a closure you
        supply. <code>* + *</code> is a closure taking two arguments, so
        <code>1, 1, * + * ... *</code> is the Fibonacci sequence, generated
        lazily and forever.</p>`,
    },
    'operators/meta': {
      title: 'Metaoperators',
      body: `
        <p>Metaoperators build new operators out of existing ones, so you learn
        one rule instead of a hundred function names.</p>
        <ul>
          <li><code>[op]</code> — reduce a list with <code>op</code>:
              <code>[+]</code> sums, <code>[*]</code> multiplies,
              <code>[~]</code> concatenates.</li>
          <li><code>&gt;&gt;op&lt;&lt;</code> — apply <code>op</code> element-wise
              ("hyper"), and <code>&gt;&gt;.method</code> calls a method on every
              element.</li>
          <li><code>Z</code> zips two lists together; <code>X</code> takes their
              cross product.</li>
        </ul>
        <p>These compose with any operator, including ones you define yourself.</p>`,
    },
    'operators/junctions': {
      title: 'Junctions',
      body: `
        <p>A junction is a single value that is several values at once. Comparing
        against it threads the comparison over every member and collapses the
        result according to the junction's kind: <code>any</code>,
        <code>all</code>, <code>one</code>, or <code>none</code>.</p>
        <p><code>|</code> is shorthand for <code>any</code>. Junctions collapse to
        a plain <code>Bool</code> in boolean context, and <code>so</code> forces
        that collapse so you can print it.</p>`,
    },

    'control/conditionals': {
      title: 'Conditionals',
      body: `
        <p><code>if</code>/<code>elsif</code>/<code>else</code> work as you expect,
        and need no parentheses around the condition. <code>unless</code> is the
        negated form.</p>
        <p><code>with</code> is <code>if</code>'s definedness twin: it runs its
        block when the value is <em>defined</em> (not merely true), and binds it
        to the topic variable <code>$_</code>. <code>without</code> is its negation.</p>
        <p>The ternary is spelled <code>?? !!</code>, keeping <code>?</code> and
        <code>:</code> free for other jobs.</p>`,
    },
    'control/loops': {
      title: 'Loops',
      body: `
        <p><code>for</code> iterates a list. The pointy block
        <code>-&gt; $i { ... }</code> names the current element; without one, the
        element lands in the topic variable <code>$_</code>.</p>
        <p>A pointy block can take several parameters, and <code>for</code> will
        pull that many elements per iteration — which is what makes
        <code>.kv</code> (index, value pairs) read so naturally.</p>
        <p>Any statement can take a trailing <code>for</code>, <code>if</code>,
        <code>while</code> or <code>unless</code> modifier when a full block would
        be noise.</p>`,
    },
    'control/given-when': {
      title: 'given / when',
      body: `
        <p><code>given</code> sets the topic <code>$_</code>, and each
        <code>when</code> smartmatches against it. Because smartmatch is
        polymorphic, one <code>given</code> block can dispatch on a type, an exact
        value, a regex, or a predicate — <code>when * &gt; 100</code> matches a
        closure.</p>
        <p>A matching <code>when</code> exits the block with its value, so no
        <code>break</code> is needed. <code>default</code> catches the rest.</p>`,
    },
    'control/loop-control': {
      title: 'Loop control',
      body: `
        <p><code>next</code> skips to the next iteration, <code>last</code> leaves
        the loop, and <code>redo</code> repeats the current iteration without
        re-evaluating the list.</p>
        <p>Loops are expressions too: a <code>for</code> loop in a value context
        returns the list of its iterations' values, so gathering results rarely
        needs an accumulator variable.</p>`,
    },

    'lists/arrays': {
      title: 'Arrays and slices',
      body: `
        <p><code>@a[$i]</code> indexes; a <em>list</em> of indices returns a slice,
        and a <code>Range</code> is just such a list.</p>
        <p>Inside a subscript, <code>*</code> stands for the number of elements, so
        <code>@a[*-1]</code> is the last element and <code>@a[*-2]</code> the one
        before it.</p>
        <p>Arrays are objects: <code>.push</code>, <code>.elems</code>,
        <code>.reverse</code>, <code>.rotate</code>, <code>.pick</code> and the
        rest are methods on them.</p>`,
    },
    'lists/map-grep-sort': {
      title: 'map, grep, sort',
      body: `
        <p><code>.map</code> transforms every element, <code>.grep</code> keeps the
        ones matching a test, and <code>.sort</code> orders them.</p>
        <p>Blocks can be written three ways, all equivalent:
        <code>{ $_ * 2 }</code> using the topic, <code>{ $^a &lt;=&gt; $^b }</code>
        using self-declared placeholder parameters (they sort into alphabetical
        order), or <code>* ** 2</code> using the Whatever star, which turns an
        expression into a closure.</p>
        <p><code>.sort</code> given a one-argument block sorts by that key rather
        than by the element itself, so <code>.sort(*.chars)</code> is
        "shortest first".</p>`,
    },
    'lists/hashes': {
      title: 'Hashes',
      body: `
        <p>A hash maps keys to values. <code>%h&lt;key&gt;</code> is the
        literal-key subscript (no quotes needed) and <code>%h{$k}</code> takes an
        expression.</p>
        <p>Iterating a hash yields <code>Pair</code> objects with
        <code>.key</code> and <code>.value</code>. Hash order is deliberately
        randomized, so sort by whatever you actually mean before printing.</p>
        <p>Subscript adverbs ask structural questions:
        <code>:exists</code>, <code>:delete</code>, <code>:k</code> (keys),
        <code>:v</code> (values), <code>:p</code> (pairs).</p>`,
    },
    'lists/lazy': {
      title: 'Laziness',
      body: `
        <p>Sequences are lazy: elements are produced only when something asks for
        them. That makes an infinite list an ordinary value you can pass around —
        <code>(1..Inf).grep(*.is-prime)</code> is every prime, and
        <code>.head(5)</code> computes exactly five of them.</p>
        <p><code>gather</code>/<code>take</code> builds a lazy list from arbitrary
        control flow: run the block, and every <code>take</code> contributes an
        element. It is a coroutine in the shape of a loop.</p>`,
    },

    'subs/basics': {
      title: 'Subroutines',
      body: `
        <p><code>sub</code> declares a subroutine. The last expression evaluated is
        the return value, so an explicit <code>return</code> is optional.</p>
        <p>Parameters may have defaults, and a default may refer to an earlier
        parameter (<code>$h = $w</code> makes <code>area(5)</code> a square).</p>
        <p>A <code>*@rest</code> parameter is <em>slurpy</em>: it soaks up every
        remaining positional argument.</p>`,
    },
    'subs/signatures': {
      title: 'Signatures',
      body: `
        <p>A signature can constrain types (<code>Str $host</code>), name
        parameters (<code>:$port</code>, passed as <code>:port(8080)</code> or
        <code>port =&gt; 8080</code>), and declare a return type after
        <code>--&gt;</code>.</p>
        <p>Named parameters are order-independent and optional by default;
        positional ones are required by default. Adding <code>!</code> or
        <code>?</code> flips that.</p>
        <p>Signatures are not only for subs: blocks, <code>for</code> loops,
        <code>catch</code> handlers and destructuring assignments all use the same
        syntax.</p>`,
    },
    'subs/multi': {
      title: 'Multiple dispatch',
      body: `
        <p><code>multi</code> declares one of several candidates sharing a name.
        At each call Raku picks the <em>narrowest</em> candidate that accepts the
        arguments — dispatching on arity, on types, on exact literal values, and
        on <code>where</code> constraints.</p>
        <p>That turns recursion base cases into declarations
        (<code>multi fact(0) { 1 }</code>) and replaces long chains of type
        checks with separate, individually readable definitions.</p>`,
    },
    'subs/closures': {
      title: 'Blocks and closures',
      body: `
        <p>Blocks are first-class values. <code>-&gt; $a, $b { ... }</code> is a
        block with a signature; <code>{ ... }</code> is one that takes
        <code>$_</code>.</p>
        <p>Every block closes over the lexical variables it can see, so the
        <code>counter</code> sub below hands back a function with its own private
        <code>$n</code> that survives the call.</p>
        <p>The Whatever star builds a closure out of an expression:
        <code>* * 2</code> means <code>{ $_ * 2 }</code>. Use it when the
        expression is short enough that naming the parameter would only add
        noise.</p>`,
    },

    'objects/classes': {
      title: 'Classes',
      body: `
        <p><code>class</code> declares a type. <code>has $.x</code> declares an
        attribute <em>with</em> a public read accessor; <code>has $!x</code>
        declares a private one. Inside the class both are reachable as
        <code>$!x</code>.</p>
        <p>Every class gets a <code>.new</code> that takes named arguments for its
        public attributes. Attributes can have defaults, and
        <code>is required</code> makes one mandatory.</p>
        <p>Defining a <code>Str</code> method is how <code>~$p</code> and string
        interpolation learn to render your object.</p>`,
    },
    'objects/inheritance': {
      title: 'Inheritance',
      body: `
        <p><code>is</code> sets a superclass. Methods are looked up along the
        method resolution order, so a subclass's <code>speak</code> wins, and the
        inherited <code>intro</code> still calls the <em>right</em> one — that is
        dynamic dispatch.</p>
        <p><code>self</code> is the invocant. <code>$.name</code> is shorthand for
        <code>self.name</code>, meaning it goes through the accessor method and
        respects overriding.</p>`,
    },
    'objects/roles': {
      title: 'Roles',
      body: `
        <p>A role is a bundle of behaviour that gets <em>composed</em> into a class
        with <code>does</code>. Composition is flat: the role's methods become the
        class's own methods, and a conflict is a compile-time error rather than a
        silently chosen winner.</p>
        <p>Prefer roles when you want shared behaviour without claiming an
        is-a relationship. A class still <code>~~</code>-matches every role it
        does, so roles work as interfaces too.</p>`,
    },
    'objects/subsets': {
      title: 'Subsets and enums',
      body: `
        <p><code>subset</code> names a type plus a predicate. It is a real type:
        usable in signatures, in <code>where</code> clauses, and in smartmatches,
        and it is checked when a value is bound.</p>
        <p><code>enum</code> declares a set of named constants that are also
        values of a new type, ordered by their numeric value.</p>
        <p><code>try</code> runs a block and returns <code>Nil</code> instead of
        dying when it fails, so <code>//</code> (defined-or) can supply a
        fallback.</p>`,
    },

    'regex/matching': {
      title: 'Matching',
      body: `
        <p>Raku regexes are their own sub-language, redesigned rather than
        inherited. Whitespace inside a pattern is insignificant, so you can space
        a pattern out for readability. Literal text goes in quotes.</p>
        <p><code>**</code> is the repetition quantifier — <code>\\d ** 4</code> is
        exactly four digits — and character classes are written
        <code>&lt;[a..z]&gt;</code>.</p>
        <p>A successful match sets <code>$/</code>, the match object, which
        stringifies to the matched text.</p>`,
    },
    'regex/captures': {
      title: 'Captures',
      body: `
        <p>Parentheses capture positionally into <code>$0</code>,
        <code>$1</code>, … (numbered from zero). <code>$&lt;name&gt;=[...]</code>
        captures by name into <code>$&lt;name&gt;</code>.</p>
        <p>Named regexes are declared with <code>my regex name { ... }</code> and
        called from inside another pattern as <code>&lt;name&gt;</code>. That is
        the whole idea behind grammars: patterns that call other patterns.</p>`,
    },
    'regex/substitution': {
      title: 'Substitution',
      body: `
        <p><code>.subst</code> returns a new string. Its replacement can be a
        closure, which receives the match — <code>*.tc</code> title-cases each
        one. The <code>:g</code> adverb replaces every occurrence instead of the
        first.</p>
        <p><code>s///</code> modifies a variable in place, and <code>.trans</code>
        does character-by-character translation.</p>`,
    },
    'regex/grammars': {
      title: 'Grammars',
      body: `
        <p>A grammar is a class whose methods are named patterns. Parsing starts at
        <code>TOP</code>, and each pattern may call others by name.</p>
        <p><code>token</code> is a pattern with backtracking disabled — the right
        default for lexical pieces — and <code>rule</code> is a token that also
        treats whitespace in the pattern as "allow whitespace here".</p>
        <p><code>.parse</code> returns a match object indexed by the sub-patterns
        that matched, or <code>Nil</code> when the whole input does not parse.</p>`,
    },

    'concurrency/promises': {
      title: 'Promises',
      body: `
        <p><code>start</code> runs a block on the thread pool and immediately
        returns a <code>Promise</code>. <code>await</code> waits for one, or for a
        list of them, and gives back the results in order.</p>
        <p>A <code>Promise</code> can also be created and kept (or broken) by hand,
        which is how you bridge callback-shaped APIs into this model.</p>`,
    },
    'concurrency/channels': {
      title: 'Channels',
      body: `
        <p>A <code>Channel</code> is a thread-safe queue: producers
        <code>.send</code>, consumers <code>.receive</code>, and
        <code>.close</code> signals that no more values are coming.</p>
        <p><code>.list</code> reads the channel until it closes, which makes a
        producer/consumer pipeline read like ordinary list processing.</p>`,
    },
    'concurrency/supplies': {
      title: 'Supplies',
      body: `
        <p>A <code>Supply</code> is an asynchronous stream of values. Where a
        <code>Channel</code> is pull-based, a supply pushes to whoever is tapping
        it.</p>
        <p><code>react</code> sets up a block that stays alive while its
        <code>whenever</code> handlers are still expecting values, and finishes
        when the streams are done. Supplies can be mapped, grepped and merged just
        like lists.</p>`,
    },

    'wrapping-up/exceptions': {
      title: 'Exceptions',
      body: `
        <p><code>die</code> throws. A <code>try</code> block returns
        <code>Nil</code> on failure and puts the exception in <code>$!</code>.</p>
        <p>A <code>CATCH</code> block inside any block handles exceptions thrown in
        it, using <code>when</code> to match by exception type. Handled means
        handled: control resumes after the enclosing block.</p>
        <p>Your own exception types come from subclassing <code>Exception</code>
        and providing a <code>message</code> method.</p>`,
    },
    'wrapping-up/phasers': {
      title: 'Phasers',
      body: `
        <p>Phasers are blocks that run at a defined moment rather than where they
        are written, so setup and teardown can live next to the code they belong
        to.</p>
        <p><code>ENTER</code>/<code>LEAVE</code> fire on entering and leaving a
        block — <code>LEAVE</code> even when an exception unwinds it.
        <code>FIRST</code>/<code>LAST</code> fire on the first and last iteration
        of a loop. <code>BEGIN</code> runs at compile time.</p>`,
    },
    'wrapping-up/introspection': {
      title: 'Introspection',
      body: `
        <p>The <code>.^</code> operator calls a method on an object's
        <em>metaobject</em> — the object describing its type. That is the whole
        metaobject protocol, and it is available at run time.</p>
        <p><code>.^name</code>, <code>.^attributes</code>, <code>.^methods</code>,
        <code>.^mro</code> (the method resolution order) and <code>.^can</code>
        let a program inspect types
        it was not written against, which is how serializers and test frameworks
        are built.</p>`,
    },
    'wrapping-up/putting-it-together': {
      title: 'Putting it together',
      body: `
        <p>A word-frequency count, using pieces from every chapter: a
        <code>for</code> modifier, auto-vivifying hash elements, sorting by a
        computed key (negated to sort descending, then by key to break ties), and
        interpolated method calls.</p>
        <p>That is the tour. From here, the
        <a href="https://docs.raku.org/" rel="noopener">official documentation</a>
        goes deeper on every topic here, and the
        <a href="playground.html">playground</a> gives you a REPL that keeps its
        state between runs.</p>`,
    },
  },
};

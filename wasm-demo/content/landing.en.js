/**
 * English prose for the landing page.  Snippet code lives in
 * content/highlights.txt; keys must match its "<group>/<id>" ids.
 */

export default {
  hero: {
    title: 'mutsu — Raku, implemented in Rust',
    tagline: 'A Raku interpreter with a bytecode VM, a baseline JIT and a cycle-collecting ' +
      'GC — shipped as a single binary with a package manager in the box.',
    sub: 'It also compiles to WebAssembly, which is what is running this page: every ' +
      'example below executes locally in your browser, with no server behind it.',
    install: 'Install it',
    openPlayground: 'Try it in the browser',
    repo: 'GitHub',
  },

  stats: {
    roastLabel: 'Roast spec files passing in full',
    roastNote: '{pass} of {total} files in the official Raku test suite',
    binaryValue: 'Single binary',
    binaryLabel: 'No runtime to install',
    binaryNote: '<code>mutsu</code> plus the bundled <code>mzef</code> package manager',
    wasmValue: 'WebAssembly',
    wasmLabel: 'The same interpreter, in a tab',
    wasmNote: 'this page is running it — nothing is sent to a server',
  },

  install: {
    heading: 'Install',
    lede: 'Prebuilt binaries for Linux and macOS are published on every release.',
    copy: 'Copy',
    copied: 'Copied',
    notes: {
      mise: 'The release archive is self-contained — <code>bin/mutsu</code>, ' +
        '<code>bin/mzef</code> and the vendored Zef tree — so ' +
        '<code>mzef install &lt;dist&gt;</code> works with no further setup.',
      docker: 'The image carries both binaries. Mount a named volume at ' +
        '<code>$HOME</code> to keep modules installed by <code>mzef</code> across runs.',
      source: 'Needs Rust 1.92+ and a C compiler. <code>make test</code> runs the local ' +
        'suite, <code>make roast</code> the official spec tests.',
    },
  },

  features: {
    heading: 'What is inside',
    lede: 'mutsu is not a subset interpreter with a fixed feature list — it is built ' +
      'against the official spec suite and tries to be the whole language.',
    cards: [
      {
        title: 'A bytecode VM, not a tree walker',
        body: 'Source is parsed to an AST, compiled to bytecode and executed by a VM ' +
          'with around 340 instructions. There is no separate interpreter to fall back ' +
          'to — the VM is the only execution engine.',
      },
      {
        title: 'Baseline JIT',
        body: 'Once a chunk of bytecode is hot it is compiled to native machine code ' +
          'through Cranelift. It is on by default, and the interpreter-only path stays ' +
          'benchmarked next to it on every commit.',
      },
      {
        title: 'A real garbage collector',
        body: 'Reference counting alone leaks cycles, so mutsu adds a cycle collector. ' +
          'Only container-shaped values take part in it, which keeps numeric and string ' +
          'code out of the collector entirely.',
      },
      {
        title: 'mzef, the package manager, is bundled',
        body: 'Upstream <a href="https://github.com/ugexe/zef">Zef</a> ships with mutsu ' +
          'and runs on it. A distribution from the ecosystem downloads, installs into ' +
          'the site repository and is then <code>use</code>-able — with no second Raku ' +
          'installation involved.',
      },
      {
        title: 'NativeCall',
        body: 'Call into C libraries from Raku: native types, <code>CArray[T]</code> ' +
          'buffers, and pointers including <code>is rw</code> out-parameters — enough ' +
          'to drive SQLite through its C API. Structs and callbacks are still to come.',
      },
      {
        title: 'Driven by the official test suite',
        body: 'Roast is the specification, and progress is measured in whole files ' +
          'passing — never by special-casing a test. Anything that regresses a passing ' +
          'file is a bug, not a tradeoff.',
      },
    ],
  },

  why: {
    heading: 'Why Raku',
    lede: 'Eight things that are ordinary in Raku and awkward almost everywhere else. ' +
      'Every one of them runs right here — edit it and see.',
  },

  next: {
    heading: 'Where to go next',
    lede: 'Pick a path.',
    cards: [
      {
        title: 'Take the tour',
        body: 'Nine chapters, from <code>say "hello"</code> to grammars and ' +
          'concurrency. Every lesson is an editable, runnable program.',
        href: 'tutorial.html',
        cta: 'Start the tutorial →',
      },
      {
        title: 'Write a program',
        body: 'The playground: an editor, a Run button, and the output — plus a ' +
          'shareable permalink for whatever you write.',
        href: 'playground.html',
        cta: 'Open the playground →',
      },
      {
        title: 'Poke at it line by line',
        body: 'The REPL keeps a session, so what you declare on one line is still ' +
          'there on the next. Unfinished lines wait for the rest.',
        href: 'repl.html',
        cta: 'Open the REPL →',
      },
      {
        title: 'Read the real docs',
        body: 'The official Raku documentation is the authority on the language, ' +
          'and it is excellent. This site is an introduction, not a replacement.',
        href: 'https://docs.raku.org/',
        cta: 'docs.raku.org →',
      },
    ],
  },

  snippets: {
    'why/rationals': {
      title: 'Arithmetic that means it',
      body: 'Dividing two integers gives an exact rational, not a float, so ' +
        '<code>0.1 + 0.2 == 0.3</code> is simply true. Integers are arbitrary ' +
        'precision, so nothing silently overflows.',
    },
    'why/junctions': {
      title: 'Junctions',
      body: 'One value that is several values at once. Comparisons thread over ' +
        'every member and collapse to a single answer, so a chain of ' +
        '<code>||</code> becomes <code>3 | 7 | 9</code>.',
    },
    'why/lazy': {
      title: 'Infinite lists, finite work',
      body: 'Sequences are lazy, so "all the primes" is an ordinary value you can ' +
        'pass to <code>.grep</code> and <code>.head</code>. Only the elements you ' +
        'look at are ever computed.',
    },
    'why/multi': {
      title: 'Multiple dispatch',
      body: 'Candidates share a name and the narrowest one wins — dispatching on ' +
        'types, on arity, and on arbitrary <code>where</code> constraints. ' +
        'FizzBuzz becomes four declarations and no conditionals.',
    },
    'why/grammars': {
      title: 'Grammars in the language',
      body: 'A parser is a class whose methods are named patterns, callable from ' +
        'each other. Parsing is a first-class feature, not a library you bolt on.',
    },
    'why/operators': {
      title: 'Operators you can build',
      body: 'Metaoperators turn any operator into a reduction (<code>[+]</code>), ' +
        'an element-wise version (<code>&gt;&gt;*&gt;&gt;</code>), or a zip — ' +
        'including operators you define yourself.',
    },
    'why/concurrency': {
      title: 'Concurrency in the core',
      body: '<code>start</code> returns a promise, <code>await</code> collects ' +
        'results, and supplies give you reactive streams — all in the language ' +
        'proper rather than in a framework.',
    },
    'why/unicode': {
      title: 'Unicode all the way down',
      body: 'Source, identifiers, strings and operators are Unicode. Strings are ' +
        'sequences of graphemes, so what you count is what a reader sees.',
    },
  },
};

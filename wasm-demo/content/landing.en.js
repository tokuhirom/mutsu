/**
 * English prose for the landing page.  Snippet code lives in
 * content/highlights.txt; keys must match its "<group>/<id>" ids.
 */

export default {
  hero: {
    title: 'Raku, running in this tab',
    tagline: 'A language built for expressiveness — gradual types, real grammars, ' +
      'lazy lists and multiple dispatch — with an interpreter small enough to ship ' +
      'to your browser.',
    sub: 'mutsu is a Raku implementation written in Rust. Everything on this site ' +
      'runs locally as WebAssembly: no server, no round trip.',
    startTutorial: 'Start the tutorial',
    openPlayground: 'Open the playground',
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
        title: 'Just try things',
        body: 'A playground with a REPL that keeps its state between runs, ' +
          'plus a shareable permalink for whatever you write.',
        href: 'playground.html',
        cta: 'Open the playground →',
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

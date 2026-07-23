/**
 * The install recipes shown on the landing page.
 *
 * Like the snippet corpora, the *code* lives here once and is shared between
 * languages; only the prose in content/landing.<lang>.js is translated.  `id`
 * keys the note in `install.notes` of those modules.
 */

export default [
  {
    id: 'mise',
    label: 'mise',
    code: [
      '# latest release (or pin: @0.7.0)',
      'mise use -g github:tokuhirom/mutsu',
      '',
      "mutsu -e 'say \"Hello, World!\"'",
      'mzef --version',
    ].join('\n'),
  },
  {
    id: 'docker',
    label: 'Docker',
    code: [
      '# REPL',
      'docker run --rm -it ghcr.io/tokuhirom/mutsu',
      '',
      '# one-liner',
      "docker run --rm ghcr.io/tokuhirom/mutsu mutsu -e 'say (^10).sum'",
    ].join('\n'),
  },
  {
    id: 'source',
    label: 'From source',
    code: [
      'git clone https://github.com/tokuhirom/mutsu',
      'cd mutsu',
      'cargo build --release',
      '',
      "./target/release/mutsu -e 'say \"Hello, World!\"'",
    ].join('\n'),
  },
];

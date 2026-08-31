import { readFile, writeFile } from 'node:fs/promises';

const [manifestPath] = process.argv.slice(2);
if (!manifestPath) throw new Error('usage: prepare-npm-package.mjs <package.json>');

const manifest = JSON.parse(await readFile(manifestPath, 'utf8'));
manifest.name = '@tokuhirom/mutsu';
manifest.description = 'Run the mutsu Raku interpreter in browsers with WebAssembly';
manifest.repository = {
  type: 'git',
  url: 'git+https://github.com/tokuhirom/mutsu.git',
};
manifest.homepage = 'https://tokuhirom.github.io/mutsu/';
manifest.keywords = ['raku', 'perl6', 'webassembly', 'wasm', 'interpreter'];
manifest.files = [...new Set([...(manifest.files || []), 'embed.js', 'README.md'])];
manifest.exports = {
  '.': {
    types: './mutsu.d.ts',
    import: './mutsu.js',
  },
  './element': './embed.js',
};

await writeFile(manifestPath, `${JSON.stringify(manifest, null, 2)}\n`);

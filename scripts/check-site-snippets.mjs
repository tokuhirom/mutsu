#!/usr/bin/env node
//
// Verifies every code snippet shipped on the static site (wasm-demo/).
//
//   node scripts/check-site-snippets.mjs             # check, exit 1 on drift
//   node scripts/check-site-snippets.mjs --update    # (re)write the expectations
//
// Each snippet is run under mutsu.  When `raku` is on PATH it is run there too
// and the two outputs must agree -- the tutorial is only allowed to teach
// behaviour that real Raku actually has.  In --update mode an expectation is
// written only for snippets where both agree (or where raku is unavailable).
//
// MUTSU_BIN selects the interpreter (default: target/debug/mutsu).

import { spawnSync } from 'child_process';
import { readFileSync, writeFileSync, mkdtempSync, rmSync } from 'fs';
import { tmpdir } from 'os';
import { join, dirname } from 'path';
import { fileURLToPath } from 'url';

import { parseCorpus, renderCorpus } from '../wasm-demo/assets/corpus.js';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..');
const MUTSU = process.env.MUTSU_BIN || join(ROOT, 'target/debug/mutsu');
const UPDATE = process.argv.includes('--update');
const TIMEOUT = 60_000;

const CORPORA = ['wasm-demo/content/lessons.txt', 'wasm-demo/content/highlights.txt'];

const scratch = mkdtempSync(join(tmpdir(), 'mutsu-site-'));

function run(bin, file) {
  const res = spawnSync(bin, [file], { encoding: 'utf8', timeout: TIMEOUT });
  if (res.error) return { ok: false, text: `${bin}: ${res.error.message}` };
  const text = (res.stdout || '') + (res.stderr || '');
  return { ok: res.status === 0, text: text.replace(/\s+$/, '') };
}

function haveRaku() {
  const res = spawnSync('raku', ['--version'], { encoding: 'utf8', timeout: TIMEOUT });
  return !res.error && res.status === 0;
}

const RAKU = haveRaku();
if (!RAKU) console.warn('warning: `raku` not found on PATH -- skipping the cross-check');

let failed = 0;
let checked = 0;

for (const rel of CORPORA) {
  const path = join(ROOT, rel);
  const text = readFileSync(path, 'utf8');
  const entries = parseCorpus(text);
  const header = text.slice(0, text.search(/^#== /m));
  const expectations = new Map();

  console.log(`\n=== ${rel} (${entries.length} snippets)`);

  for (const e of entries) {
    checked++;
    const file = join(scratch, `${e.key.replace(/\//g, '-')}.raku`);
    writeFileSync(file, e.code + '\n');

    const got = run(MUTSU, file);
    const ref = RAKU ? run('raku', file) : null;

    const problems = [];
    if (!got.ok) problems.push(`mutsu exited non-zero:\n${indent(got.text)}`);
    if (ref && !ref.ok) problems.push(`raku exited non-zero:\n${indent(ref.text)}`);
    if (ref && ref.ok && got.ok && ref.text !== got.text) {
      problems.push(`mutsu and raku disagree:\n--- raku\n${indent(ref.text)}\n--- mutsu\n${indent(got.text)}`);
    }
    if (!problems.length && !UPDATE && e.expect !== got.text) {
      problems.push(`output drifted from the recorded expectation:\n--- expected\n${indent(e.expect)}\n--- got\n${indent(got.text)}`);
    }

    if (problems.length) {
      failed++;
      console.log(`FAIL ${e.key}`);
      for (const p of problems) console.log(indent(p, '     '));
    } else {
      console.log(`ok   ${e.key}`);
      expectations.set(e.key, got.text);
    }
  }

  if (UPDATE) {
    writeFileSync(path, renderCorpus(header, entries, expectations));
    console.log(`updated ${rel}`);
  }
}

function indent(text, pad = '       ') {
  return text.split('\n').map(l => pad + l).join('\n');
}

rmSync(scratch, { recursive: true, force: true });

/* ------------------------------------------------------------------ *
 * Every snippet must have prose in every language, and no prose may be
 * left behind pointing at a snippet that no longer exists.
 * ------------------------------------------------------------------ */

console.log('\n=== translations');

const LANGS = ['en', 'ja'];
const tutorial = {};
const landing = {};
for (const lang of LANGS) {
  tutorial[lang] = (await import(`../wasm-demo/content/tutorial.${lang}.js`)).default;
  landing[lang] = (await import(`../wasm-demo/content/landing.${lang}.js`)).default;
}

const lessons = parseCorpus(readFileSync(join(ROOT, 'wasm-demo/content/lessons.txt'), 'utf8'));
const highlights = parseCorpus(readFileSync(join(ROOT, 'wasm-demo/content/highlights.txt'), 'utf8'));

let missing = 0;
function want(cond, what) {
  if (!cond) { console.log(`FAIL ${what}`); missing++; }
}

for (const lang of LANGS) {
  for (const lesson of lessons) {
    want(tutorial[lang].lessons[lesson.key], `tutorial.${lang}: no prose for ${lesson.key}`);
    want(tutorial[lang].chapters[lesson.group], `tutorial.${lang}: no title for chapter ${lesson.group}`);
  }
  for (const key of Object.keys(tutorial[lang].lessons)) {
    want(lessons.some(l => l.key === key), `tutorial.${lang}: prose for unknown lesson ${key}`);
  }
  for (const h of highlights) {
    want(landing[lang].snippets[h.key], `landing.${lang}: no prose for ${h.key}`);
  }
  for (const key of Object.keys(landing[lang].snippets)) {
    want(highlights.some(h => h.key === key), `landing.${lang}: prose for unknown snippet ${key}`);
  }
}
console.log(missing
  ? `${missing} missing/orphaned translations`
  : `ok   ${lessons.length} lessons + ${highlights.length} highlights, prose present in ${LANGS.join('/')}`);

console.log(`\n${checked - failed}/${checked} snippets ok`);
// Snippet drift is what --update exists to fix, so it is not an error there.
// A missing translation is never regenerable, so it fails either way.
process.exit(missing || (failed && !UPDATE) ? 1 : 0);

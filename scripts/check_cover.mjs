#!/usr/bin/env node

// Copy and route contract for the GitHub Pages cover (docs/index.html):
// no em dash in visible text, <title>, or meta/og/twitter content; a <title>
// and <html lang="en">; exactly one link to the NEON Cross-Product Response
// Atlas, labelled with its name. Run: node scripts/check_cover.mjs

import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const path = "docs/index.html";
const html = readFileSync(resolve(root, path), "utf8");
const errors = [];
const EM = /—|&mdash;|&#8212;|&#x2014;/i;
const ATLAS_NAME = "NEON Cross-Product Response Atlas";

function lineOf(index) {
  return html.slice(0, index).split("\n").length;
}

function snippet(text, at) {
  return text.slice(Math.max(0, at - 30), at + 30).replace(/\s+/g, " ").trim();
}

// <title> and <html lang="en">
const title = html.match(/<title>([^<]*)<\/title>/i);
if (!title || !title[1].trim()) errors.push("missing or empty <title>");
else if (EM.test(title[1])) errors.push(`em dash in <title>: "${title[1]}"`);
if (!/<html\b[^>]*\blang="en"/i.test(html)) errors.push('missing <html lang="en">');

// Attribute copy a reader or a link preview sees.
for (const m of html.matchAll(/\b(content|alt|aria-label|title)\s*=\s*"([^"]*)"/gi)) {
  const hit = m[2].search(EM);
  if (hit >= 0) {
    errors.push(`line ${lineOf(m.index)}: em dash in ${m[1]}="...": "${snippet(m[2], hit)}"`);
  }
}

// Visible body text: drop style, script, comments and tags, keep offsets.
const blank = (s) => s.replace(/[^\n]/g, " ");
const visible = html
  .replace(/<style\b[\s\S]*?<\/style>/gi, blank)
  .replace(/<script\b[\s\S]*?<\/script>/gi, blank)
  .replace(/<!--[\s\S]*?-->/g, blank)
  .replace(/<[^>]*>/g, blank);
for (const m of visible.matchAll(new RegExp(EM.source, "gi"))) {
  errors.push(`line ${lineOf(m.index)}: em dash in visible text: "${snippet(visible, m.index)}"`);
}

// Exactly one Atlas route, named as the Atlas.
const atlasLinks = [...html.matchAll(/<a\b[^>]*href\s*=\s*"([^"]*)"[^>]*>([\s\S]*?)<\/a>/gi)]
  .filter((m) => m[1].includes("NEON-Driver-Cascade"));
if (atlasLinks.length !== 1) {
  errors.push(`${atlasLinks.length} Atlas links (want exactly one)`);
}
for (const m of atlasLinks) {
  const text = m[2].replace(/<[^>]*>/g, " ").replace(/\s+/g, " ").trim();
  if (!text.includes(ATLAS_NAME)) {
    errors.push(`line ${lineOf(m.index)}: Atlas link text "${text}" does not name "${ATLAS_NAME}"`);
  }
}
const cascade = visible.search(/Driver Cascade/i);
if (cascade >= 0) errors.push(`line ${lineOf(cascade)}: visible text says "Driver Cascade"`);

if (errors.length) {
  console.error(`${path} cover check failed:`);
  for (const e of errors) console.error(`- ${e}`);
  process.exit(1);
}
console.log(`${path} cover check passed: no em dashes, title and lang present, one Atlas link.`);

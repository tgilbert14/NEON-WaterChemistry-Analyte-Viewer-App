#!/usr/bin/env node

// A real browser session against the Water Chemistry app: a deep link, three
// tabs, a site change through the picker map, and the map's markers. The curl
// smoke proves the HTML arrived; this proves the Shiny session works.
// Pattern from NEON-My-Little-Inverts scripts/post_deploy_browser.mjs.
//
// Every check that depends on the server applying a state change (the ?site=
// deep link, the mapExplore site change) polls for up to 90 s instead of
// reading the page once. On a cold start right after a deploy, Actions run
// 40776451098 (2026-10-28) found Shiny idle while the context band still named
// the default site SYCA; the deep link landed later and a warm re-run passed.
//
// usage: node scripts/browser_smoke.mjs <base-url>
//        (or BROWSER_SMOKE_URL=<base-url> node scripts/browser_smoke.mjs)

import { chromium } from "playwright";

const base = process.argv[2] || process.env.BROWSER_SMOKE_URL;
const expectedTitle = "NEON Water Chemistry · Analyte Viewer";
const snagText = "This view hit a snag";
const tabs = ["Compare", "Relationship", "Seasonal pattern"];
const minMarkers = 30;
const timeout = Number(process.env.BROWSER_SMOKE_TIMEOUT_MS || 180_000);
const stateTimeout = Number(process.env.BROWSER_SMOKE_STATE_TIMEOUT_MS || 90_000);

function fail(message) {
  throw new Error(`Water Chemistry browser smoke failed: ${message}`);
}

if (!base) {
  console.error("usage: node scripts/browser_smoke.mjs <base-url>");
  process.exit(2);
}

// Shiny is idle once html.shiny-busy has stayed off for a quiet window; one
// idle tick between two renders is not enough.
async function waitIdle(page, label) {
  await page.evaluate(() => { window.__wcIdleSince = undefined; });
  try {
    await page.waitForFunction(
      () => {
        if (!window.Shiny || !window.Shiny.shinyapp
            || !window.Shiny.shinyapp.isConnected()) return false;
        const busy = document.documentElement.classList.contains("shiny-busy");
        const now = Date.now();
        if (busy || window.__wcIdleSince === undefined) {
          window.__wcIdleSince = busy ? undefined : now;
          return false;
        }
        return now - window.__wcIdleSince >= 1500;
      },
      null,
      { timeout, polling: 250 },
    );
  } catch {
    fail(`${label}: Shiny did not reach idle within ${timeout / 1000} s`);
  }
  if (await page.locator("#shiny-disconnected-overlay").count()) {
    fail(`${label}: the Shiny session disconnected`);
  }
}

// Poll until the context band's site code names `code`; the server applies
// the deep link and site changes after the first idle tick on a cold start.
async function waitForBandSite(page, code, label) {
  const started = Date.now();
  try {
    await page.waitForFunction(
      (want) => (document.querySelector(".context-band .cb-site-code")?.textContent || "").includes(want),
      code,
      { timeout: stateTimeout, polling: 250 },
    );
  } catch {
    const band = await page.locator(".context-band .cb-site-code").textContent().catch(() => "");
    const waited = ((Date.now() - started) / 1000).toFixed(1);
    fail(`${label}: the context band names "${(band || "").trim()}", not ${code}, after waiting ${waited} s`);
  }
}

async function requireNoSnag(page, label) {
  const hit = await page.evaluate(
    (text) => document.body.textContent.includes(text),
    snagText,
  );
  if (hit) fail(`${label}: the page shows "${snagText}"`);
}

function requireNoPageErrors(errors, label) {
  if (errors.length) fail(`${label}: pageerror ${errors.join(" | ")}`);
}

async function clickChangeSite(page, label) {
  const link = page.locator("#changeSite");
  try {
    await link.waitFor({ state: "visible", timeout });
  } catch {
    fail(`${label}: #changeSite is not visible`);
  }
  await link.click();
  await waitIdle(page, label);
}

const browser = await chromium.launch({ headless: true });
try {
  const page = await browser.newPage({ viewport: { width: 1280, height: 900 } });
  page.setDefaultTimeout(timeout);
  const pageErrors = [];
  page.on("pageerror", (error) => pageErrors.push(error.message));

  const url = new URL(base);
  url.searchParams.set("site", "REDB");
  const response = await page.goto(url.href, { waitUntil: "domcontentloaded", timeout });
  if (!response || !response.ok()) {
    fail(`navigation to ${url.href} returned ${response ? response.status() : "no response"}`);
  }
  await waitIdle(page, "REDB deep link");

  const shell = await page.evaluate(() => ({
    title: document.title,
    lang: document.documentElement.getAttribute("lang"),
    heavy: typeof window.WC_HEAVY,
  }));
  if (shell.heavy !== "object") fail(`typeof window.WC_HEAVY is ${shell.heavy}, not object`);
  if (shell.title !== expectedTitle) fail(`document.title is "${shell.title}", not "${expectedTitle}"`);
  if (shell.lang !== "en") fail(`<html lang> is "${shell.lang}", not "en"`);
  await waitForBandSite(page, "REDB", "REDB deep link");
  await waitIdle(page, "REDB deep link applied");
  await requireNoSnag(page, "REDB deep link");

  for (const tab of tabs) {
    const link = page.locator(`#main_tabs a[data-value="${tab}"]`);
    if (!(await link.count())) fail(`no "${tab}" tab`);
    await link.click();
    await waitIdle(page, `REDB ${tab} tab`);
    await requireNoSnag(page, `REDB ${tab} tab`);
  }
  requireNoPageErrors(pageErrors, "REDB tabs");

  await clickChangeSite(page, "first Change site");
  await page.evaluate(() => window.Shiny.setInputValue("mapExplore", "ARIK", { priority: "event" }));
  await waitForBandSite(page, "ARIK", "ARIK via mapExplore");
  await waitIdle(page, "ARIK via mapExplore");
  const arik = await page.locator(".context-band").textContent().catch(() => "");
  if (!arik || !arik.includes("Arikaree River")) {
    fail(`after mapExplore ARIK the context band reads "${(arik || "").trim().slice(0, 120)}"`);
  }
  await requireNoSnag(page, "ARIK via mapExplore");

  await clickChangeSite(page, "second Change site");
  let markers = 0;
  try {
    await page.waitForFunction(
      (min) => {
        const el = document.getElementById("map");
        const widget = el && window.HTMLWidgets && window.HTMLWidgets.find("#map");
        const map = widget && widget.getMap && widget.getMap();
        if (!map || !window.L) return false;
        let n = 0;
        map.eachLayer((layer) => { if (layer instanceof window.L.CircleMarker) n += 1; });
        window.__wcMarkers = n;
        return n >= min;
      },
      minMarkers,
      { timeout: 60_000, polling: 500 },
    );
  } catch {
    // fall through to the count below
  }
  markers = await page.evaluate(() => window.__wcMarkers || 0);
  if (markers < minMarkers) fail(`the picker map has ${markers} CircleMarker layers, fewer than ${minMarkers}`);
  requireNoPageErrors(pageErrors, "the session");

  console.log(
    `OK: ${base} · REDB deep link, ${tabs.join(", ")} tabs, ARIK via mapExplore, ${markers} map markers, no pageerror.`,
  );
} finally {
  await browser.close();
}

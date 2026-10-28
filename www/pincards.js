/* =========================================================================
   pincards.js: copied from NEON-Small-Mammal-Tracker-App@c1a229d (www/pincards.js),
   ported to the Water Chemistry Analyte Viewer (namespace wc-, two charts).
   Tap a plotly point to pin its card, then export the box with the pins baked in.
   ========================================================================= */
/* What changed in the port (comments only; the flagship's design is unchanged):
   - Binds every `.wc-pinnable` box that holds a plotly div (Compare `ts` and
     Relationship `reg`). The card HTML rides in each point's `customdata`
     (pin_card_html() strings built server-side, already escaped); points whose
     customdata is empty, null or "NA" are ignored, so fit lines never pin.
   - Anchors are stored as DATA coordinates plus the point's axis names, and are
     linearized with the axis's d2l() before l2p(): the Compare x axis is a date
     axis, and l2p() of a raw date string is NaN. Dual-axis mode puts a trace on
     yaxis2, so the point's own y axis is used, not always `yaxis`.
   - A Shiny re-render (date window, analyte, mode) runs purge+newPlot on the
     same div and wipes gd.on() listeners, so a MutationObserver re-binds after
     every render. Instead of clearing every pin when the data changes, each pin
     is re-found by its exact card HTML in the new traces: a point still plotted
     keeps its card (re-anchored), a point that left the chart drops its card, so
     a pinned card never points at a value that is not on the chart.
   - Keyboard: each card is focusable (tabindex 0, role group with a label);
     Escape or Delete removes it, arrow keys move it, the close button is a real
     button. Toolbar actions are real <button>s wired by a delegated listener.
   - No SweetAlert in this app: a small built-in toast (role status) reports
     progress and failures, including a missing capture library. */
(function () {
  "use strict";
  var NS = "http://www.w3.org/2000/svg";

  function boxOf(node) { return node ? node.closest(".wc-pinnable") : null; }
  function bgColor() {
    var c = window.getComputedStyle(document.body).backgroundColor;
    if (!c || c === "transparent" || c === "rgba(0, 0, 0, 0)") {
      var dark = document.documentElement.getAttribute("data-bs-theme") === "dark" ||
                 document.body.getAttribute("data-bs-theme") === "dark";
      return dark ? "#0f1b2d" : "#ffffff";
    }
    return c;
  }

  /* plotly axis ids ("x", "y2") to layout keys ("xaxis", "yaxis2") */
  function axKey(id, letter) {
    if (!id) return letter + "axis";
    var n = String(id).replace(/^[xy]/, "");
    return letter + "axis" + (n === "1" ? "" : n);
  }

  /* map a data point to box-relative pixels via plotly's live axes, so a pin's
     leader line follows the dot through any resize/relayout. d2l() first: on a
     date axis the data value is a date string and l2p() needs milliseconds. */
  function anchorPx(gd, box, dx, dy, xk, yk) {
    var fl = gd && gd._fullLayout;
    if (!fl) return null;
    var xa = fl[xk || "xaxis"], ya = fl[yk || "yaxis"];
    if (!xa || !ya || typeof xa.l2p !== "function" || typeof ya.l2p !== "function") return null;
    var lx = typeof xa.d2l === "function" ? xa.d2l(dx) : dx;
    var ly = typeof ya.d2l === "function" ? ya.d2l(dy) : dy;
    var px = xa.l2p(lx), py = ya.l2p(ly);
    if (!isFinite(px) || !isFinite(py)) return null;
    var gdR = gd.getBoundingClientRect(), boxR = box.getBoundingClientRect();
    return {
      ax: px + xa._offset + (gdR.left - boxR.left),
      ay: py + ya._offset + (gdR.top - boxR.top)
    };
  }

  function linesLayer(box) {
    var s = box.querySelector(":scope > svg.wc-pin-lines");
    if (!s) {
      s = document.createElementNS(NS, "svg");
      s.setAttribute("class", "wc-pin-lines");
      s.setAttribute("aria-hidden", "true");
      box.appendChild(s);
    }
    return s;
  }
  /* x2/y2 chase the card centre (honours its scale() transform via getBoundingClientRect) */
  function updateLine(pin) {
    if (!pin.__line) return;
    var r = pin.getBoundingClientRect(), b = pin.__box.getBoundingClientRect();
    pin.__line.setAttribute("x2", r.left - b.left + r.width / 2);
    pin.__line.setAttribute("y2", r.top - b.top + r.height / 2);
  }

  function removePin(pin) {
    if (pin.__line) pin.__line.remove();
    if (pin.__dot) pin.__dot.remove();
    pin.remove();
  }
  function clearBox(box) {
    if (!box) return;
    box.querySelectorAll(".wc-pin").forEach(removePin);
    var ll = box.querySelector("svg.wc-pin-lines"); if (ll) ll.innerHTML = "";
  }
  /* the pin box a toolbar button belongs to (the toolbar sits beside the box,
     inside .wc-pin-wrap, so the capture never has to drop it) */
  function boxFor(el) {
    if (!el || !el.closest) return null;
    var wrap = el.closest(".wc-pin-wrap");
    return wrap ? wrap.querySelector(".wc-pinnable") : el.closest(".wc-pinnable");
  }
  /* wcClearPins(el): clear the box of el; with no argument, clear every box */
  window.wcClearPins = function (el) {
    var box = boxFor(el);
    if (box) { clearBox(box); return; }
    document.querySelectorAll(".wc-pinnable").forEach(clearBox);
  };

  /* re-anchor every pin's leader line + dot from its stored DATA coords */
  function repositionAll(gd, box) {
    box.querySelectorAll(".wc-pin").forEach(function (pin) {
      if (pin.__dx == null || !pin.__line) return;
      var a = anchorPx(gd, box, pin.__dx, pin.__dy, pin.__xk, pin.__yk);
      if (!a) return;
      pin.__line.setAttribute("x1", a.ax); pin.__line.setAttribute("y1", a.ay);
      if (pin.__dot) { pin.__dot.setAttribute("cx", a.ax); pin.__dot.setAttribute("cy", a.ay); }
      updateLine(pin);
    });
  }

  /* keep a card fully inside the box on both axes: html-to-image renders only
     the box's own bounds, so an off-box card would be cropped from the PNG */
  function clampPin(pin, left, top) {
    var box = pin.__box, r = pin.getBoundingClientRect();
    var maxL = Math.max(4, box.clientWidth - r.width - 4);
    var maxT = Math.max(4, box.clientHeight - r.height - 4);
    pin.style.left = Math.max(4, Math.min(left, maxL)) + "px";
    pin.style.top = Math.max(4, Math.min(top, maxT)) + "px";
  }

  function cardTitle(html) {
    var m = html.match(/class='wc-pin-t'>([^<]*)</);
    var ta = document.createElement("textarea");
    ta.innerHTML = m ? m[1] : "";
    return ta.value || "data point";
  }

  function makePin(gd, box, html, pt) {
    var bR = box.getBoundingClientRect();
    var a = anchorPx(gd, box, pt.dx, pt.dy, pt.xk, pt.yk) || { ax: bR.width / 2, ay: bR.height / 2 };
    var pin = document.createElement("div");
    pin.className = "wc-pin";
    pin.__box = box; pin.__key = html;
    pin.__dx = pt.dx; pin.__dy = pt.dy; pin.__xk = pt.xk; pin.__yk = pt.yk;
    pin.setAttribute("tabindex", "0");
    pin.setAttribute("role", "group");
    pin.setAttribute("aria-label", "Pinned card: " + cardTitle(html) + ". Escape removes it, arrow keys move it.");
    // the html is server-built by pin_card_html(), which escapes every value
    pin.innerHTML = "<button class='wc-pin-close' type='button' title='Remove card' aria-label='Remove card'>&times;</button>" + html;
    var grip = document.createElement("div");
    grip.className = "wc-pin-resize";
    grip.title = "Resize card; double-click to reset";
    grip.setAttribute("aria-hidden", "true");
    pin.appendChild(grip);

    box.appendChild(pin);
    // spawn-side clamp measures the real card after it is in the DOM
    clampPin(pin, a.ax + 24, a.ay + 14);

    var layer = linesLayer(box);
    var ln = document.createElementNS(NS, "line");
    ln.setAttribute("x1", a.ax); ln.setAttribute("y1", a.ay);
    ln.setAttribute("class", "wc-pin-line");
    ln.setAttribute("stroke-width", "2.5");
    ln.setAttribute("stroke-linecap", "round");
    layer.appendChild(ln);
    var dot = document.createElementNS(NS, "circle");
    dot.setAttribute("cx", a.ax); dot.setAttribute("cy", a.ay); dot.setAttribute("r", "4.5");
    dot.setAttribute("class", "wc-pin-dot");
    dot.setAttribute("stroke-width", "1.5");
    layer.appendChild(dot);
    pin.__line = ln; pin.__dot = dot;
    updateLine(pin);

    function closeAndRefocus() {
      removePin(pin);
      // keep focus inside the box: the next card, else the Clear button
      var wrap = box.closest(".wc-pin-wrap");
      var target = box.querySelector(".wc-pin") ||
                   (wrap && wrap.querySelector("[data-wc-action='clear']"));
      if (target) target.focus();
    }
    pin.querySelector(".wc-pin-close").addEventListener("click", function (e) {
      e.stopPropagation(); closeAndRefocus();
    });
    pin.addEventListener("keydown", function (e) {
      if (e.target !== pin) return;              // let the close button handle its own keys
      if (e.key === "Escape" || e.key === "Delete" || e.key === "Backspace") {
        e.preventDefault(); e.stopPropagation(); closeAndRefocus(); return;
      }
      var step = e.shiftKey ? 40 : 10, dxk = 0, dyk = 0;
      if (e.key === "ArrowLeft") dxk = -step;
      else if (e.key === "ArrowRight") dxk = step;
      else if (e.key === "ArrowUp") dyk = -step;
      else if (e.key === "ArrowDown") dyk = step;
      else return;
      e.preventDefault();
      clampPin(pin, pin.offsetLeft + dxk, pin.offsetTop + dyk);
      updateLine(pin);
    });

    /* drag-to-move, clamped to the box. A 4px move threshold keeps a near-miss
       tap a tap. move/up/cancel are bound on WINDOW and capture is released on
       every terminal event, so a pointercancel can never leave the card stuck. */
    pin.addEventListener("pointerdown", function (ev) {
      if (ev.target.closest("a, button, .wc-pin-resize")) return;
      var sx = ev.clientX - pin.offsetLeft, sy = ev.clientY - pin.offsetTop;
      var startX = ev.clientX, startY = ev.clientY, dragging = false;
      function mv(em) {
        if (!dragging) {
          if (Math.abs(em.clientX - startX) < 4 && Math.abs(em.clientY - startY) < 4) return;
          dragging = true;
          try { pin.setPointerCapture(ev.pointerId); } catch (e) {}
        }
        em.preventDefault();
        clampPin(pin, em.clientX - sx, em.clientY - sy);
        updateLine(pin);
      }
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        pin.removeEventListener("lostpointercapture", up);
        try { pin.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
      pin.addEventListener("lostpointercapture", up);
    });

    /* grip-to-resize via scale() */
    grip.addEventListener("pointerdown", function (ev) {
      ev.preventDefault(); ev.stopPropagation();
      try { grip.setPointerCapture(ev.pointerId); } catch (e) {}
      var startX = ev.clientX, startScale = pin.__scale || 1;
      var baseW = pin.getBoundingClientRect().width / startScale;
      function mv(em) {
        var s = Math.min(2.4, Math.max(0.5, startScale + (em.clientX - startX) / baseW));
        pin.__scale = s; pin.style.transform = "scale(" + s + ")"; updateLine(pin);
      }
      function up() {
        window.removeEventListener("pointermove", mv);
        window.removeEventListener("pointerup", up);
        window.removeEventListener("pointercancel", up);
        grip.removeEventListener("lostpointercapture", up);
        try { grip.releasePointerCapture(ev.pointerId); } catch (e) {}
      }
      window.addEventListener("pointermove", mv);
      window.addEventListener("pointerup", up);
      window.addEventListener("pointercancel", up);
      grip.addEventListener("lostpointercapture", up);
    });
    grip.addEventListener("dblclick", function () {
      pin.__scale = 1; pin.style.transform = ""; updateLine(pin);
    });
    return pin;
  }

  /* a point's card html, or "" when it carries none (fit line, NA, empty) */
  function cardOf(cd) {
    if (Array.isArray(cd)) cd = cd.filter(function (v) { return typeof v === "string"; })[0];
    if (typeof cd !== "string") return "";
    var s = cd.trim();
    return (s && s !== "NA") ? cd : "";
  }

  function handleClick(gd, box, d) {
    if (!d || !d.points || !d.points.length) return;
    var p = d.points[0];
    var html = cardOf(p.customdata);
    if (!html) return;
    var dup = Array.prototype.find.call(box.querySelectorAll(".wc-pin"),
      function (q) { return q.__key === html; });
    if (dup) { dup.classList.remove("wc-pulse"); void dup.offsetWidth; dup.classList.add("wc-pulse"); return; }
    var tr = p.data || (gd.data && gd.data[p.curveNumber]) || {};
    /* anchor from the DATA point (not the finger): exact, and touch-safe */
    makePin(gd, box, html, { dx: p.x, dy: p.y, xk: axKey(tr.xaxis, "x"), yk: axKey(tr.yaxis, "y") });
  }

  /* after a re-render, find each pin's point again by its exact card html */
  function locate(gd, html) {
    var data = gd.data || [];
    for (var t = 0; t < data.length; t++) {
      var tr = data[t], cd = tr.customdata;
      if (!cd || tr.visible === false) continue;
      for (var i = 0; i < cd.length; i++) {
        if (cardOf(cd[i]) === html) {
          return { dx: tr.x ? tr.x[i] : null, dy: tr.y ? tr.y[i] : null,
                   xk: axKey(tr.xaxis, "x"), yk: axKey(tr.yaxis, "y") };
        }
      }
    }
    return null;
  }
  function reconcile(gd, box) {
    // mid-render (purged, not yet re-plotted): wait for plotly_afterplot
    if (!gd.data || !gd.data.length || !gd._fullLayout) return;
    gd.__wcData = gd.data;
    box.querySelectorAll(".wc-pin").forEach(function (pin) {
      var hit = locate(gd, pin.__key);
      if (!hit || hit.dx == null || hit.dy == null) { removePin(pin); return; }
      pin.__dx = hit.dx; pin.__dy = hit.dy; pin.__xk = hit.xk; pin.__yk = hit.yk;
    });
    repositionAll(gd, box);
  }

  /* (re)bind a plotly graph div. Safe to call repeatedly: it removes any prior
     handler first, and only reconciles pins when the plot is a new render. */
  function bindPlot(gd) {
    if (!gd || typeof gd.on !== "function") return;
    var box = boxOf(gd); if (!box) return;
    // newPlot assigns a new data array; reconcile() records it once it has run
    if (gd.__wcData !== gd.data) reconcile(gd, box);

    if (gd.__wcClick && gd.removeListener) { try { gd.removeListener("plotly_click", gd.__wcClick); } catch (e) {} }
    gd.__wcClick = function (d) { handleClick(gd, box, d); };
    gd.on("plotly_click", gd.__wcClick);

    if (gd.__wcRelayout && gd.removeListener) { try { gd.removeListener("plotly_relayout", gd.__wcRelayout); } catch (e) {} }
    gd.__wcRelayout = function () { repositionAll(gd, box); };
    gd.on("plotly_relayout", gd.__wcRelayout);

    if (gd.__wcAfter && gd.removeListener) { try { gd.removeListener("plotly_afterplot", gd.__wcAfter); } catch (e) {} }
    gd.__wcAfter = function () {
      if (gd.__wcData !== gd.data) reconcile(gd, box);
      else repositionAll(gd, box);
    };
    gd.on("plotly_afterplot", gd.__wcAfter);

    if (!box.__wcRO && window.ResizeObserver) {
      box.__wcRO = new ResizeObserver(function () {
        var g = box.querySelector(".js-plotly-plot"); if (g) repositionAll(g, box);
      });
      box.__wcRO.observe(box);
    }
  }

  function scan() {
    document.querySelectorAll(".wc-pinnable .js-plotly-plot").forEach(bindPlot);
  }
  var scanPending = false;
  function scanSoon() {
    if (scanPending) return;
    scanPending = true;
    requestAnimationFrame(function () { scanPending = false; scan(); });
  }

  /* ---- toast ------------------------------------------------------------ */
  var toastTimer = null;
  function toast(msg, isErr, sticky) {
    var t = document.querySelector(".wc-toast");
    if (!t) {
      t = document.createElement("div");
      t.className = "wc-toast";
      t.setAttribute("role", "status");
      t.setAttribute("aria-live", "polite");
      document.body.appendChild(t);
    }
    if (toastTimer) { clearTimeout(toastTimer); toastTimer = null; }
    t.textContent = msg;
    t.classList.toggle("wc-toast-err", !!isErr);
    t.classList.add("wc-toast-show");
    if (!sticky) toastTimer = setTimeout(function () { t.classList.remove("wc-toast-show"); }, isErr ? 5000 : 2200);
  }

  /* ---- exports ---------------------------------------------------------- */
  function downloadUrl(url, name) {
    var a = document.createElement("a"); a.download = name; a.href = url;
    document.body.appendChild(a); a.click(); a.remove();
  }
  /* chrome that must never bake into an exported PNG: one shared list */
  var HIDE = ["wc-pin-close", "wc-pin-resize", "wc-card-actions", "wc-toast",
              "modebar-container"];
  function chromeFilter(n) {
    return !(n.classList && HIDE.some(function (c) { return n.classList.contains(c); }));
  }
  var saving = false;
  function snap(node, name) {
    if (saving || !node) return;
    if (typeof htmlToImage === "undefined") {
      toast("The image library did not load. Check the network or an ad blocker, then reload.", true);
      return;
    }
    saving = true;
    var gd = node.querySelector ? node.querySelector(".js-plotly-plot") : null;
    if (gd && window.Plotly && Plotly.Plots) { try { Plotly.Plots.resize(gd); } catch (e) {} }
    node.querySelectorAll(".wc-pin.wc-pulse").forEach(function (p) { p.classList.remove("wc-pulse"); });
    toast("Rendering image", false, true);
    setTimeout(function () {
      htmlToImage.toPng(node, { pixelRatio: 2, backgroundColor: bgColor(), cacheBust: true,
                                skipFonts: true, filter: chromeFilter })
        .then(function (url) { downloadUrl(url, name); toast("Saved " + name); })
        .catch(function () { toast("The image could not be rendered. Try again.", true); })
        .then(function () { saving = false; });
    }, 90);
  }
  function clean(s) { return (s || "").replace(/[^A-Za-z0-9]+/g, "-").replace(/^-+|-+$/g, ""); }
  /* the site code: stamped on the card caption at the source, else the site input */
  function siteCode() {
    var cap = document.querySelector("[data-wc-site]");
    var s = cap ? cap.getAttribute("data-wc-site") : "";
    if (!s) { var sel = document.getElementById("site"); s = sel ? sel.value : ""; }
    return clean(s).toUpperCase();
  }
  function snapName(facet) {
    return ["neon-water", siteCode(), clean(facet)].filter(Boolean).join("-") + ".png";
  }
  window.wcSaveChart = function (el) {
    var box = boxFor(el) || document.querySelector(".wc-pinnable");
    if (!box) return;
    snap(box, snapName(box.getAttribute("data-wc-facet") || "chart"));
  };
  window.wcSaveCard = function () {
    snap(document.getElementById("wcCardNode"), snapName("summary"));
  };

  /* one delegated listener for the toolbar buttons (real <button>s, so Enter and
     Space already activate them) */
  document.addEventListener("click", function (e) {
    var b = e.target.closest && e.target.closest("[data-wc-action]");
    if (!b) return;
    var act = b.getAttribute("data-wc-action");
    if (act === "clear") window.wcClearPins(b);
    else if (act === "save-chart") window.wcSaveChart(b);
    else if (act === "save-card") window.wcSaveCard();
  });

  function start() {
    var mo = new MutationObserver(scanSoon);
    mo.observe(document.body, { childList: true, subtree: true });
    scan();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", start);
  else start();
  /* plotly in a hidden tab can render late; a tab show also relayouts it */
  document.addEventListener("shown.bs.tab", function () { setTimeout(scan, 80); });
})();

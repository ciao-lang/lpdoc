/*
 * LPdoc UI basic elements
 *
 * (c) 2018-2024 The Ciao Development Team
 * 
 * This is a JavaScript support library for LPdoc HTML backend and
 * Ciao playgrounds.
 *
 * NOTE: Keep this library as simple as possible so that documents
 * loads quickly.
 */

/* =========================================================================== */
/* Dynamic imports */

function importScript(src, is_async = false) {
  let el = document.createElement('script');
  el.src = src;
  el.async = is_async;
  document.head.appendChild(el);
}
/* (as a promise) */
function tryImportScript(src, is_async = false) {
  return new Promise((resolve, reject) => {
    let el = document.createElement('script');
    el.onload = () => resolve(el);
    el.onerror = () => reject(new Error(`could not load ${src}`));
    el.async = is_async;
    el.src = src; // (make sure onload is set before, it may be cached)
    document.head.append(el);
  });
}

function importCSS(src) {
  let el = document.createElement('link');
  el.rel = 'stylesheet';
  el.type = "text/css";
  el.href = src;
  document.head.appendChild(el);
}

/* =========================================================================== */
/* DOM helper */

/* element from str */
function elem_from_str(str) {
  const el = document.createElement("div");
  el.innerHTML = str;
  return el.firstElementChild;
}
/* element with class */
function elem_cn(kind, cn) {
  const el = document.createElement(kind);
  el.className = cn;
  return el;
}

/* create a button */
function btn(style, title, text, onclick) {
  const e = elem_cn('button', style);
  e.title = title;
  e.innerHTML = text;
  e.onclick = onclick;
  return e;
}

/* =========================================================================== */
/* UI - Dropdown menus */

// TODO: avoid globals?
var clicked_dropdown = false;
var active_dropdown = null;

/* close dropdown menu */
window.addEventListener('click', e => {
  if (active_dropdown === null) return;
  if (clicked_dropdown === true) { // skip first click
    clicked_dropdown = false;
    return;
  }
  if (active_dropdown.classList.contains('show')) {
    active_dropdown.classList.remove('show');
    active_dropdown = null;
  }
});

class DropdownButton {
  constructor(menu, title, btn_text_el, items, onchange) {
    var c = elem_cn('div', 'dropdown');
    var dropdown = elem_cn('div', 'dropdown-content');
    var b = elem_cn('button', 'dropdown-btn');
    this.btn_el = b;
    b.title = title;
    b.onclick = e => { 
      if (active_dropdown !== null) { // close other
        active_dropdown.classList.toggle("show");
      }
      if (active_dropdown !== dropdown) { // open new
        dropdown.classList.toggle("show");
        active_dropdown = dropdown;
      } else { // keep closed
        active_dropdown = null;
      }
      clicked_dropdown = true;
    };
    b.appendChild(btn_text_el);
    c.appendChild(b);
    // Add items and create links
    this.values = [];
    for (const i of items) {
      let item = { k:i.k, n:i.n };
      var li = document.createElement('a');
      li.innerHTML = item.n;
      li.onclick = () => {
        onchange(item.k);
      };
      item.el = li;
      this.values.push(item);
      dropdown.appendChild(item.el);
    }
    c.appendChild(dropdown);
    menu.appendChild(c);
  }

  get_item(value) {
    for (const item of this.values) {
      if (item.k === value) {
        return item;
      }
    }
    return null;
  }

  highlight(value) {
    for (const item of this.values) {
      item.el.style.fontWeight = 'normal';
    }
    this.get_item(value).el.style.fontWeight = 'bold';
  }

  update_marks(filter) {
    for (const item of this.values) {
      const mark = filter(item.k) ? "&#10004;" : "&nbsp;";
      item.el.innerHTML = "<span class='dropdown-mark'>" + mark + "</span> " + item.n;
    }
  }
}

/* =========================================================================== */
/* SVGs (Note: clone with .cloneNode(true) if needed) */

const theme_svg = elem_from_str(`<svg class="header-icon-img" viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg">
<rect height="3" width="2" x="15" y="2" fill="currentColor"/>
<rect height="2" width="3" x="27" y="15" fill="currentColor"/>
<rect height="3" width="2" x="15" y="27" fill="currentColor"/>
<rect height="2" width="3" x="2" y="15" fill="currentColor"/>
<rect height="3" transform="translate(-3 7.23) rotate(-45)" width="2" x="6.22" y="5.73" fill="currentColor"/>
<rect height="2" transform="translate(2.14 19.63) rotate(-45)" width="3" x="23.27" y="6.23" fill="currentColor"/>
<rect height="3" transform="translate(-10.26 24.77) rotate(-45)" width="2" x="23.77" y="23.27" fill="currentColor"/>
<polygon points="5.47 25.13 7.59 23 9 24.42 6.88 26.54 5.47 25.13" fill="currentColor"/>
<path d="M16,8a8,8,0,1,0,8,8A8,8,0,0,0,16,8Zm0,14a6,6,0,0,1,0-12Z" fill="currentColor"/>
</svg>`);
const slide_svg = elem_from_str(`<svg class="header-icon-img" xmlns="http://www.w3.org/2000/svg" x="0px" y="0px" width="50" height="50" viewBox="0 0 48 48">
<path d="M 11.5 6 C 8.4802259 6 6 8.4802259 6 11.5 L 6 36.5 C 6 39.519774 8.4802259 42 11.5 42 L 36.5 42 C 39.519774 42 42 39.519774 42 36.5 L 42 11.5 C 42 8.4802259 39.519774 6 36.5 6 L 11.5 6 z M 11.5 9 L 36.5 9 C 37.898226 9 39 10.101774 39 11.5 L 39 13 L 9 13 L 9 11.5 C 9 10.101774 10.101774 9 11.5 9 z M 9 16 L 39 16 L 39 32 L 9 32 L 9 16 z M 9 35 L 39 35 L 39 36.5 C 39 37.898226 37.898226 39 36.5 39 L 11.5 39 C 10.101774 39 9 37.898226 9 36.5 L 9 35 z" fill="currentColor"></path>
</svg>`);
const search_svg = elem_from_str(`<svg class="header-icon-img" version="1.1" viewBox="0 0 512 512" xml:space="preserve" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<path d="M344.5,298c15-23.6,23.8-51.6,23.8-81.7c0-84.1-68.1-152.3-152.1-152.3C132.1,64,64,132.2,64,216.3  c0,84.1,68.1,152.3,152.1,152.3c30.5,0,58.9-9,82.7-24.4l6.9-4.8L414.3,448l33.7-34.3L339.5,305.1L344.5,298z M301.4,131.2  c22.7,22.7,35.2,52.9,35.2,85c0,32.1-12.5,62.3-35.2,85c-22.7,22.7-52.9,35.2-85,35.2c-32.1,0-62.3-12.5-85-35.2  c-22.7-22.7-35.2-52.9-35.2-85c0-32.1,12.5-62.3,35.2-85c22.7-22.7,52.9-35.2,85-35.2C248.5,96,278.7,108.5,301.4,131.2z" fill="currentColor"/>
</svg>`);
search_svg.style.marginRight = '0px';

/* =========================================================================== */
/* UI - Themes */

/* CSS UI themes (see lpdoc.css) */
var theme_list = [
  { k:'system', n:'System' },
  { k:'light', n:'Light' },
  { k:'dark', n:'Dark' }
];

/* Hook function to be called when theme needs to be updated */
var update_theme_hook = null;

/* Update theme when on dark/light events */
window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
  update_theme_hook();
});
/* Update as soon as DOM is there */
window.addEventListener('DOMContentLoaded', (event) => {
  update_theme_hook(); // TODO: this happens too late and causes flickering with ciao_playground.js
});

/* Get theme from local storage */
function theme_get_value() {
  try {
    var v = window.localStorage.getItem('theme');
    if (v === null) return 'system';
    return v;
  } catch (e) {
    return 'system';
  }
}

/* Update theme in local storage */
function theme_set_value(value) {
  try {
    if (value === 'system') {
      window.localStorage.removeItem('theme');
    } else {
      window.localStorage.setItem('theme', value);
    }
  } catch (e) {
    return;
  }
}

/* Get actual theme (based on selection and user UI preferences) */
function get_actual_theme() {
  var theme = theme_get_value();
  if (theme === 'system') {
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      theme = 'dark';
    } else {
      theme = 'light';
    }
  }
  return theme;
}

function update_css_theme() {
  document.documentElement.setAttribute('data-theme', get_actual_theme());
}

/* (default theme update hook) */
update_theme_hook = () => {
  update_css_theme();
  if (typeof github_stars_el !== 'undefined') {
    update_github_stars_theme();
  }
};

function new_theme_button(base_el) {
  const theme_button =
        new DropdownButton(base_el,
                           "Change theme",
                           theme_svg.cloneNode(true),
                           theme_list,
                           value => {
                             theme_button.highlight(value);
                             theme_set_value(value);
                             update_theme_hook();
                           });
  theme_button.btn_el.style.fontSize = '14px'; // TODO: tweak for alignment
  theme_button.highlight(theme_get_value());
  return theme_button;
}

/* =========================================================================== */
/* (LPdoc specific) */

// TODO: Generate better HTML, avoid patching
(function() {
  /* --------------------------------------------------------------------------- */
  /* LPdoc - Toogle sidebar, adjust lpdoc-nav, add theme button, patch search button */

  /* Toogle sidebar (for mobile-friendly) */
  /* NOTE: need sidebar and sidebar-toogle-button */
  function setup_nav() {
    var trigger = document.getElementById('sidebar-toggle-button');
    if (trigger === null) {
      return; /* no trigger */
    }
    /* Detect element to be toggled and its class */
    var toggle_el;
    var toggle_class;
    var nav_el;
    /* LPdoc sidebar? */
    toggle_el = document.getElementById('sidebar');
    if (toggle_el !== null) { /* ok */
      toggle_class = 'sidebar-toggled';
      /* find nav */
      var nav_el = document.getElementsByClassName('lpdoc-nav')[0];
      if (nav_el !== null) {
        /* ensure that it appears in the main div */
        if (nav_el.parentNode === toggle_el) {
          /* move nav to main */
          var main_el = document.getElementsByClassName('lpdoc-main')[0];
          main_el.prepend(nav_el);
          /* remove leftover <hr> from sidebar */
          if (toggle_el.firstChild.tagName === 'HR') {
            toggle_el.removeChild(toggle_el.firstChild);
          }
        }
        nav_el.classList.add('lpdoc-sidebar');
        nav_el.style.padding="10px 0px 0px 0px"; /* amend padding */

        /* Insert theme selection button */
        let dummy = document.createElement('div');
        let thm_btn_el = new_theme_button(dummy);
        nav_el.prepend(dummy.firstChild);
      }
    } else {
      /* Horiz menu (e.g., website layout)? */
      toggle_el = document.getElementsByClassName('lpdoc-horiz-menu')[0];
      if (toggle_el !== null) { /* ok */
        toggle_class = 'lpdoc-horiz-menu-toggled';
        /* Insert theme selection button */
        let li = document.createElement('li');
        let thm_btn_el = new_theme_button(li);
        toggle_el.prepend(li);
        setup_github_stars(toggle_el);
      } else {
	return; /* Nothing to be toggled */
      }
    }
    trigger.addEventListener('click', function(e) {
      e.preventDefault();
      toggle_el.classList.toggle(toggle_class); 
      return false;
    });
    /* Patch search button */
    function patch_search(e) { if (e.innerHTML === "🔍") { e.innerHTML = ""; e.appendChild(search_svg.cloneNode(true)); } }
    for (const e of document.getElementsByClassName('lpdoc-navbutton')) { patch_search(e); }
    for (const e of document.getElementsByClassName('lpdoc-searchmenu')) { patch_search(e); }
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_nav();
  });
})();

/* =========================================================================== */
/* LPdoc - interactive index search */

(function() {
  /* Index search */
  var timeout=null;
  var results_div=null;
  var text_input=null;

  function do_search(text) {
    var ntext = text.toLowerCase();
    results_div.innerHTML = ""; // clear results (so that query selector works)
    if (ntext == "") return;

    var time0 = new Date();
    var elems = document.querySelectorAll('a[search_kwd]');
    var what;
    var res = [];
    if (ntext == "_") { /* all */
      elems.forEach(function(elem) {
	res.push(elem.parentNode);
      });
      what = "showing all entries";
    } else if (ntext.match(/^[a-z0-9]$/i) !== null) { /* single alphanum character, only prefix */
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (kwd.startsWith(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      what = "only prefixes";
    } else { /* first prefix, then substring */
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (kwd.startsWith(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (!kwd.startsWith(ntext) && kwd.includes(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      what = "prefixes and substrings";
    }
    var time1 = new Date();
    var time_diff = time1-time0; // in ms

    if (res.length === 0) {
      results_div.innerHTML = "<br><b>No results found</b>";
    } else {
      var rbody = "";
      res.forEach(function(elem) {
	rbody += "<li>" + elem.innerHTML + "</li>";
      });
      results_div.innerHTML =
	"<br>" + 
	"<b>" + res.length + " results (" + time_diff + " ms)</b> " +
	"for " + what + ":" +
	"<ul>"+rbody+"</ul>";
    }
  }

  function setup_search() {
    /* Find search divs */
    text_input = document.getElementById('search-input');
    results_div = document.getElementById("search-results");
    idx_div = document.getElementById("search-index");

    if (text_input == null) return; /* No search div */

    for(var i=0,els=idx_div.querySelectorAll('a'); i<els.length; i++) {
      var elid = els[i].id;
      if (elid != "") {
	els[i].setAttribute('search_kwd',elid.toLowerCase());
      }
    }

    /* Search from hash (if any) */
    var hash=decodeURIComponent(window.location.hash.substring(1));
    text_input.value = hash;
    do_search(hash);

    text_input.select(); /* select input automatically */

    /* Search as we type */
    text_input.onkeydown = function (e) {
      clearTimeout(timeout);
      timeout = setTimeout(function() {
	do_search(text_input.value); }, 100); 
    };
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_search();
  });
})();

/* =========================================================================== */

/* Official github buttons (See https://buttons.github.io/) */
/* Hack to support light/dark themes: we create two buttons and set visibility based on theme */
function setup_github_stars(base_el) {
  if (typeof github_stars_el !== 'undefined') return; /* already setup */
  importScript('https://buttons.github.io/buttons.js', true);
  function b(m) { // Append an initially hidden gh star to base_el with style 'm' and
    const elb = elem_from_str(`<a class='github-button' style='display: none;' href='https://github.com/ciao-lang/ciao' data-color-scheme='no-preference: ${m}; light: ${m}; dark: ${m};' data-size='large' data-show-count='true' aria-label='Star ciao-lang/ciao on GitHub'>Star</a>`);
    const el = document.createElement('div');
    el.style.padding = '6px 0 0 0';
    el.style.marginLeft = '16px';
    el.style.display = 'none';
    el.appendChild(elb);
    base_el.appendChild(el);
    return el;
  }
  window.github_stars_el = {};
  window.github_stars_el['dark'] = b('dark');
  window.github_stars_el['light'] = b('light');
  update_github_stars_theme();
}

function update_github_stars_theme() {
  const dark = get_actual_theme() === 'dark';
  window.github_stars_el['dark'].style.display = dark ? "inline-block" : "none";
  window.github_stars_el['light'].style.display = (!dark) ? "inline-block" : "none";
}

/* =========================================================================== */
/* Load playground functionality if needed */

var urlPREFIX = null; /* global */
var urlVERS = "?v=3"; /* NOTE: 1.24.0 -> 3; internal patch number, increment on every change to avoid unwanted browser cache problems */

/* resolve a URL: add urlPREFIX for different roots, and urlVERS
   version suffix to avoid browser caching */
function res_URL(url) {
  return urlPREFIX+url+urlVERS;
}

function load_playground() {
  (async() => {
    const pg = '/playground/js/ciao_playground.js';
    const prefixes = [];
    // Default prefix (assume playground is fetched from the same location)
    // Due to CORS restrictions this only works properly when accessing from HTTP
    // (not file:///).
    prefixes.push('');
    // // Site symlink from local docs // TODO: due to CORS restrictions loading wasm from file:/// may not work
    // prefixes.push('site');
    // Local server (for development)
    if (window.location.protocol === 'file:') {
      prefixes.push('http://localhost:8001'); // (see ciao-serve-mt)
    }
    // ciao-lang site
    prefixes.push('https://ciao-lang.org'); // TODO: missing version // TODO: add some CDN
    for (const p of prefixes) {
      urlPREFIX = p;
      try {
        await tryImportScript(res_URL(pg));
        console.log(`{loaded playground from '${res_URL(pg)}'}`);
        break;
      } catch(e) {
      }
    }
  })();
}

/* =========================================================================== */
/* Document view modes */

/** Viewer with SlideShow support */
class SlideShow {
  constructor() {
    this.enabled = false;
    this.curr_slide = 0;
    this.base_el = null;
    this.update_dimensions_hook = () => {};
    this.has_focus_hook = () => { return false; };
  }

  prepare(base_el) {
    this.auto_slides(base_el);
    //slideshow.set_slide_mode(true);
    this.base_el.classList.remove('lpdoc-slide-mode'); // make sure slide mode is off
    this.base_el.classList.remove('lpdoc-slide-scroll');
    this.add_slide_button();
    this.add_slide_bindings();
  }

  /** Patch the lpdoc document at base_el for slide mode */
  //  - LPdoc output sections into separate div with different ids.
  //  - No visual changes unless lpdoc-slide-mode class is added to the parent (dynamically).
  //  - Automatically identify them and add the slide class.
  // TODO: TOO complicated, do from LPdoc output
  auto_slides(base_el) {
    this.base_el = base_el;
    if (base_el.querySelectorAll('.lpdoc-slide').length > 0) {
      // Return if the document has been processed
      return;
    }
    if (base_el.querySelectorAll('.lpdoc-horiz-menu').length > 0) {
      // Do nothing for website layout
      return;
    }
    let headers;
    // Add slide class to all subsections
    headers = base_el.querySelectorAll('h2, h3, h4, h5, h6');
    headers.forEach((e) => {
      let p = e.closest('div'); // for h2, etc., just parent div
      if (p && (!p.classList.contains('lpdoc-main') /* TODO: hack for "Parts of this manual" */))
        p.classList.add('lpdoc-slide');
    });
    // function to flatten slides (any nested slide is moved as sibling, recursively)
    function move_up(p) {
      let curr = p.firstChild;
      // locate first nested slide
      while (true) {
        if (!curr) break;
        if (curr.classList && curr.classList.contains('lpdoc-slide')) break;
        curr = curr.nextSibling;
      }
      let parent = p.parentElement;
      let end = p.nextSibling;
      while (true) {
        if (!curr) break;
        if (curr.classList && curr.classList.contains('lpdoc-slide')) {
          move_up(curr); // recursive if it is a slide
        }
        let next = curr.nextSibling;
        parent.insertBefore(curr, end);
        curr = next;
      }
    }
    // Add slide class to cover (assume only one h1)
    headers = base_el.querySelectorAll('h1');
    if (headers.length >= 1) {
      let e = headers[0];
      let p = e.closest('div');
      if (e.classList.contains('lpdoc-cover-h1')) {
        // for h1 in cover we need parent of parent
        if (p) p = p.parentElement.closest('div');
        if (p) p = p.closest('div');
        if (p) {
          p.classList.add('lpdoc-slide');
//          // Then re-insert all orphan nodes into this recover some of the
//          // orphan divs between the first and the second slide
//          let curr = p.nextSibling;
//          while (true) {
//            if (!curr) break;
//            if (curr.classList && curr.classList.contains('lpdoc-slide')) break;
//            let next = curr.nextSibling;
//            p.appendChild(curr);
//            curr = next;
//          }
        }
      } else { // h1 not in cover
        p.classList.add('lpdoc-slide');
      }
    }
    //
    base_el.querySelectorAll('.lpdoc-slide').forEach((e) => {
      move_up(e);
    });
    // Then re-insert all orphan nodes after each slide
    // (for h1 in cover or separate ":- doc(module, ...)" without sections)
    base_el.querySelectorAll('.lpdoc-slide').forEach((e) => {
      let curr = e.nextSibling;
      while (true) {
        if (!curr) break;
        if (curr.classList && curr.classList.contains('lpdoc-slide')) break;
        let next = curr.nextSibling;
        e.appendChild(curr);
        curr = next;
      }
    });
  }

  #slide_els() {
    return this.base_el.getElementsByClassName("lpdoc-slide");
  }
  get_num_slides() {
    return this.#slide_els().length;
  }

  // Get current 'position' (view state and slide number)
  get_pos() {
    return [this.enabled, this.curr_slide];
  }
  // Restore position (see above), fix if needed
  restore_pos(pos) {
    let slide = pos[1];
    let num_slides = this.get_num_slides();
    if (slide >= num_slides) slide = num_slides - 1;
    this.curr_slide = slide;
    this.set_slide_mode(pos[0]);
  }

  set_slide_mode(enable) {
    if (this.enabled == enable) return;
    this.enabled = enable;
    if (enable) { /* enable */
      this.base_el.classList.add('lpdoc-slide-mode'); // turn slide mode
      this.base_el.classList.add('lpdoc-slide-scroll');
      if (this.base_el.classList.contains('preview-container')) {
        // TODO: needed for playground
        this.base_el.style.removeProperty('overflow-x');
      }
      this.set_visible_slide(this.curr_slide);
    } else { /* disable */
      this.base_el.classList.remove('lpdoc-slide-mode');
      this.base_el.classList.remove('lpdoc-slide-scroll');
      if (this.base_el.classList.contains('preview-container')) {
        // TODO: needed for playground
        this.base_el.style.overflowX = 'auto';
      }
      let slides = this.#slide_els();
      for (let i = 0; i < slides.length; i++) {
        slides[i].style.display = "block";  
      }
      // Disable slide container scaling
      let div = this.base_el.getElementsByClassName("lpdoc-main")[0];
      div.style.removeProperty('zoom');
    }
    this.update_dimensions();
  }

  // Insert slide mode button
  add_slide_button() {
    let nav_el = null;

    // Use lpdoc-nav (not in playground preview mode)
    let nav_els = this.base_el.getElementsByClassName('lpdoc-nav');
    if (nav_els.length > 0) nav_el = nav_els[0];
    
    if (nav_el === null) return;

    let li = document.createElement('a');
    li.href = '';
    li.innerHTML = ""; li.appendChild(slide_svg.cloneNode(true));
    li.onclick = () => {
      this.set_slide_mode(true);
      this.base_el.focus(); // TODO: handy for playground
      return false;
    };
    nav_el.prepend(li);
  }

  // Add some key bindings for slides
  // TODO: Document:
  //  - Escape: exit slide mode
  //  - Left/right arrows: previous/next slide
  //  - P/N: previous/next slide
  //  - ',' / '.' : first/last slide
  add_slide_bindings() {
    this.base_el.addEventListener("keydown", (event) => {
      if (!this.enabled) return;
      if (this.has_focus()) return; // exit if some element has the focus
      if (event.code === "Escape") { // turn off slide mode
        this.set_slide_mode(false);
        return;
      }
      let prev_slide = this.curr_slide;
      let nslides = this.get_num_slides();
      if (!event.altKey && !event.ctrlKey && !event.metaKey) { // no modifier keys
        if (event.code === "ArrowLeft") { // event.code === "ArrowUp"
          this.curr_slide -= 1;
        } else if (event.code === "ArrowRight") { // event.code === "ArrowDown"
          this.curr_slide += 1;
        } else if (event.code === "Space") {
          this.curr_slide += 1;
        } else if (event.code === "KeyN") {
          this.curr_slide += 1;
        } else if (event.code === "KeyP") {
          this.curr_slide -= 1;
        } else if (event.code === "Comma") { // Actually, we mean < but same as , 
          this.curr_slide = 0;
        } else if (event.code === "Period") { // Actually, we mean > but same as .
          this.curr_slide = nslides - 1;
        }
      }
      if (this.curr_slide >= nslides) { this.curr_slide = nslides-1; }
      if (this.curr_slide < 0) { this.curr_slide = 0; }
      if (this.curr_slide !== prev_slide) {
        this.set_visible_slide(this.curr_slide);
        this.update_dimensions();
      }
    });
  }

  set_visible_slide(n) {
    if (!this.enabled) return; // slide mode disabled
    let slides = this.#slide_els();
    for (let i = 0; i < slides.length; i++) {
      // Modify style.display via a CSS property (useful for "@media print")
      if (i == n) {
        slides[n].classList.remove('lpdoc-slide-hide');
      } else {
        slides[i].classList.add('lpdoc-slide-hide');
      }
    }
  }

  update_dimensions() {
    this.update_dimensions_hook();
    // TODO: custom scaling per slide?
    if (this.enabled) {
      // Scale slide container
      /* NOTE: Scaling with auto scrollbars is complex. Simply
         calculating clientWidth/w as scale value does not work, since
         it may be affected by the size of the scrollbar. As height
         crosses the ideal size to fit the whole slide, there is a
         region where the scale value is not monotous: no scrollbar
         makes it taller, which exceeds the available height and again
         makes the scrollbar necessary, etc. We opt here for using a
         thin scrollbar (see .lpdoc-slide-scroll) and scale ignoring
         the scrollbar width. This looks aesthetically nicer than any
         other tried solution. */
      let div = this.base_el.getElementsByClassName("lpdoc-main")[0];
      // const w = 800; // see lpdoc-main width in lpdoc.css
      const w = div.offsetWidth;
      // Compute scale with the whole clientWidth
      div.style.display = 'none'; // hide to obtain clientWidth without scrollbar
      let scaleValue = this.base_el.clientWidth / w;
      div.style.removeProperty('display'); // unhide
      div.style.zoom = `${scaleValue}`;
    }
  }
  has_focus() {
    return this.has_focus_hook();
  }
}

/** Plain viewer */
class PlainView {
  constructor() {
    this.update_dimensions_hook = () => {};
  }
  prepare(base_el) {
  }
  update_dimensions() {
    this.update_dimensions_hook();
  }
  has_focus() {
    return false;
  }
}

// TODO: avoid globals?
let main_doc = null;

window.addEventListener("resize", () => {
  if (main_doc) main_doc.update_dimensions();
});

// ---------------------------------------------------------------------------

// Heuristic for main_doc
if (typeof lpdocPG !== 'undefined') {
  if (lpdocPG === 'runnable') {
    main_doc = new SlideShow(); // optional slideshow viewer
  } else {
    main_doc = new PlainView(); // plain viewer (e.g. full playground)
  }
} else {
  main_doc = new SlideShow(); // optional slideshow viewer
}

// Wait for document to be loaded, prepare, and load playground if needed
document.addEventListener("DOMContentLoaded", function() {
  main_doc.prepare(document.body);
  // Force 'runnable' mode if runnables are detected
  // TODO: needed now? lpdoc emits lpdocPG='runnable' when needed? good for custom HTML?
  if (typeof lpdocPG === 'undefined' &&
      document.getElementsByClassName("lpdoc-codeblock-runnable").length > 0) {
    lpdocPG = 'runnable';
  }
  // Enable playground if needed
  if (typeof lpdocPG !== 'undefined') load_playground();
});

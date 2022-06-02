/*
 * Auxiliary functionality for LPdoc generated documentation
 *
 * (c) 2018-2022 The Ciao Development Team
 */

/* --------------------------------------------------------------------------- */
/* LPdoc - setup theme */

(function() {
  /* TODO: merge with ciao_playground.js theme selection */
  function update_css_theme() {
    var theme = ((window.matchMedia &&
                  window.matchMedia('(prefers-color-scheme: dark)').matches) ?
                 'dark' : 'light');
    document.documentElement.setAttribute('data-theme', theme);
  }
  // Update theme when on dark/light events
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
    update_css_theme();
  });
  // Update as soon as DOM is there
  window.addEventListener('DOMContentLoaded', (event) => {
    update_css_theme();
  });
})();

/* --------------------------------------------------------------------------- */
/* LPdoc - Toogle sidebar */

(function() {
  /* Toogle sidebar (for mobile-friendly) */
  /* NOTE: need sidebar and sidebar-toogle-button */
  function setup_toggle() {
    var trigger = document.getElementById('sidebar-toggle-button');
    /* Detect element to be toggled */
    var element;
    var toggle_class;
    element = document.getElementById('sidebar');
    toggle_class = 'sidebar-toggled';
    if (element == null) {
      element = document.getElementsByClassName('lpdoc-horiz-menu')[0];
      toggle_class = 'lpdoc-horiz-menu-toggled';
      if (element == null) {
	return; /* Nothing to be toggled */
      }
    }
    trigger.addEventListener('click', function(e) {
      e.preventDefault();
      element.classList.toggle(toggle_class); 
      return false;
    });
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_toggle();
  });
})();

/* --------------------------------------------------------------------------- */
/* LPdoc - seachbox */

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

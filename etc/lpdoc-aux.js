/*
 * Auxiliary functionality for LPdoc generated documentation
 *
 * (c) 2018 Jose F. Morales
 */

(function() {
  /* Toogle sidebar (for mobile-friendly) */
  /* NOTE: need sidebar and sidebar-toogle-button */
  function setup_toggle() {
    var element = document.getElementById('sidebar');
    var trigger = document.getElementById('sidebar-toggle-button');

    trigger.addEventListener('click', function(e) {
      e.preventDefault();
      element.classList.toggle('sidebar-toggled'); 
      return false;
    });
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_toggle();
  });
})();

(function() {
  /* Index search */
  var timeout=null;
  var results_div=null;
  var text_input=null;

  function do_search(text) {
    var ntext = text.toLowerCase();
    results_div.innerHTML = ""; // clear results (so that query selector works)
    if (ntext == "") return;

    var sel;
    if (ntext == "_") {
      sel = 'a[search_kwd]';
    } else {
      sel = 'a[search_kwd^="'+ntext+'"]'; /* TODO: escape ntext? */
    }
    var elems = document.querySelectorAll(sel); /* TODO: escape? */
    if (elems.length === 0) {
      results_div.innerHTML = "<br><b>No results found</b>";
    } else {
      var rbody = "";
      elems.forEach(function(elem) {
	var e = elem.parentNode;
	rbody += "<li>" + e.innerHTML + "</li>";
      });
      results_div.innerHTML = "<br><b>" + elems.length + " results:</b> <ul>"+rbody+"</ul>";
    }
  }

  window.onload = function() {
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
  };
})();

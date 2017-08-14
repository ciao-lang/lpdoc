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


$( '#sidebar' ).ready(function() {
  var new_els = ['dq_provider_level', 'dq_timeliness', 'dq_region_level','client_counts', 'utilization', 'prioritization', 'vet_active_list']
  new_els.map(function(id) {
    var checkExist = setInterval(function() {
   if ($('#'+"tab-"+ id).length) {
      var el = document.getElementById("tab-" + id)
    el.innerHTML += '&nbsp<span class="badge badge-success" style="float:right;">New</span>'
    el = el.setAttribute("style", "display: inline-flex;align-items: center;")
      clearInterval(checkExist);
   }
}, 100); 
  })
})
$( document ).ready(function() {
  var new_els = ['dq_provider_level', 'dq_timeliness', 'dq_region_level','client_counts', 'utilization', 'prioritization']
  new_els.map(function(id) {
    var el = document.getElementById("tab-" + id)
    el.innerHTML += '&nbsp<span class="badge badge-success">New</span>'
  })
})
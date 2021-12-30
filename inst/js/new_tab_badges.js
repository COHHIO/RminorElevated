$( '.main-sidebar' ).ready(function() {
  var new_els = ['dq_provider_level', 'dq_timeliness', 'dq_region_level','client_counts', 'utilization', 'prioritization', 'vet_active_list', 'dq_system_summary', "qpr_community_need_ph", "qpr_community_need_lh", "qpr_length_of_stay"]
  new_els.map(function(id) {
    var checkExist = setInterval(function() {
   if ($('#'+"tab-"+ id).length) {
      var el = $("#tab-" + id).append('<span class="badge badge-success" style="float:right;">New</span>')
    
      clearInterval(checkExist);
   }
}, 100); 
  })
})
$( '.main-sidebar' ).ready(function() {
  var new_els = ['welcome', 'dq_program_level', 'dq_timeliness', 'dq_region_level','client_counts', 'utilization', 'prioritization', 'vet_active_list', 'coc_competition', 'coc_competition_mahoning', 'dq_system_summary', "qpr_community_need_ph", "qpr_community_need_lh", "qpr_length_of_stay", "qpr_noncash_benefits", "qpr_permanent_housing", "qpr_income_growth", "qpr_health_insurance", "qpr_rrh_placement", "qpr_rrh_spending", "program_lookup"]
  new_els.map(function(id) {
    var checkExist = setInterval(function() {
   if ($('#'+"tab-"+ id).length) {
      var el = $("#tab-" + id)
      el.children("i.fa").addClass('text-success')
      if (id.match('^qpr|^dq'))
        var a = el.parents(".nav-item.has-treeview").find("i.fa").first().addClass('text-success')
      clearInterval(checkExist);
   }
}, 100); 
  })
})
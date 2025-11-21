$(document).ready(function() {
  var new_els = ['welcome', 'dq_program_level', 'dq_timeliness', 'dq_region_level', 'client_counts', 'utilization', 'prioritization', 'coc_competition', 'vet_active_list', 'dq_system_summary', 'mpo', 'qpr_community_need_ph', 'qpr_community_need_lh', 'qpr_length_of_stay', 'qpr_noncash_benefits', 'qpr_permanent_housing', 'qpr_income_growth', 'qpr_health_insurance', 'qpr_rrh_placement', 'qpr_rrh_spending', 'program_lookup', 'news'];

  new_els.map(function(id) {
    var checkExist = setInterval(function() {
      var el = $('#tab-' + id); // Select the sidebar tab element
      
      if (el.length) {
        // Debugging: Log element to confirm it is found
        console.log('Element found:', el);

        // Add the green color class to the icon element
        el.children("i.fas, i.far").addClass('text-success');

        // For sub-items under qpr and dq, add green to their parent icon as well
        if (id.match('^qpr|^dq')) {
          var a = el.parents(".nav-item.has-treeview").find("i.fas").first().addClass('text-success');
          // Debugging: Log parent icon change
          console.log('Parent icon colored green:', a);
        }
        
        // Stop the interval once the element is found
        clearInterval(checkExist);
      } else {
        // Debugging: Log if the element isn't found
        console.log('Waiting for element:', id);
      }
    }, 100); 
  });
});

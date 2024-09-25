$(document).ready(function() {
  var new_els = ['welcome', 'dq_program_level', 'dq', 'dq_timeliness', 'dq_region_level', 'client_counts', 'utilization', 'prioritization', 'vet_active_list', 'coc_competition', 'coc_competition_mahoning', 'dq_system_summary', 'mpo', 'program_lookup', 'program_details', 'youth_program_details'];

  new_els.map(function(id) {
    var checkExist = setInterval(function() {
      var el = $('#tab-' + id); // Select the sidebar tab element
      
      if (el.length) {
        // Debugging: Log element to confirm it is found
        console.log('Element found:', el);

        // Add the green color class to the icon element
        el.children("i.fas").addClass('text-success');

        // If it's a parent item (like Ohio BoS Performance or Ohio Youth Performance), also color the parent icon
        if (id === 'program_details' || id === 'youth_program_details' ||
        id === 'dq_program_level' || id === 'dq_timeliness' || id === 'dq_region_level' ||
        id === 'dq_system_summary') {
          el.parents(".nav-item.has-treeview").find("i.fas").first().addClass('text-success');
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

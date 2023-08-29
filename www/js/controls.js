function fixControls() {
  $('.well input[type="checkbox"]')
    .each(function () {
      const $input = $(this);
      const id = $input.attr('id');

      $input.siblings('div').find('label')
        .attr('for', id);
    });

  $('[role="presentation"][aria-label]')
    .removeAttr('aria-label');

  $('#gbif_name')
    .attr('aria-describedby', 'gbif-hint');

  $('#powo_id')
    .attr('aria-describedby', 'powo-hint');

  Shiny.addCustomMessageHandler("fileuploaded", function(fileName){
    $('#csv-block .file-text').text(fileName);
  });
}


export { fixControls };

function fixControls() {
  // Sort out matching of inputs and labels
  $('.well input[type="checkbox"]')
    .each(function () {
      const $input = $(this);
      const id = $input.attr('id');

      $input.siblings('div').find('label')
        .attr('for', id);
    });

  // Remove aria-label from dolwoad-button icon
  // Element should not have a presentation role *and* a label
  // The button text already describes the button's function
  // so the icon is simply presentational
  $('[role="presentation"][aria-label]')
    .removeAttr('aria-label');

  // Associate GBIF example text with input
  $('#gbif_name')
    .attr('aria-describedby', 'gbif-hint');

  //Associate POWO example text with input
  $('#powo_id')
    .attr('aria-describedby', 'powo-hint');

  // Update file text whenever a file has been uploaded
  Shiny.addCustomMessageHandler("fileuploaded", function(fileName){
    $('#csv-block .file-text').text(fileName);
  });
}


export { fixControls };

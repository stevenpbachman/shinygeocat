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

  Shiny.addCustomMessageHandler("fileuploaded", function(fileName){
    $('#csv-block .file-text').text(fileName);
  });
}


export { fixControls };

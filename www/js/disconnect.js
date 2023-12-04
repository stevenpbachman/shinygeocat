function fixDisconnect(diconnectMinutes = 15) {
  $(document).on('shiny:disconnected', function() {
    const $dialog = $('#ss-connect-dialog')
    
    const message = `Your session has has unexpectedly ended, or timed out because of ${diconnectMinutes} minutes of inactivity.`;
    $dialog.find('label').text(message);

    $dialog.find('a')
      .attr('role', 'button')
      .text('Click to reload')
      .focus();
  });
}


export { fixDisconnect };



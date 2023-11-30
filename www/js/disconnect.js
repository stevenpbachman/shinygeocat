const $html = $(document.documentElement);
const $body = $(document.body);
const $offscreenItems = $('#offscreen-items');
const $disconnectWarningDialog = $('#disconnect-warning-dialog');
const $disconnectDialog = $('#disconnect-dialog');
let oldFocus;


function showModalDialog ($dialog) {
  hideModalDialog();
  $body.children('.container-fluid').attr('inert', 'inert');

  $dialog
    .appendTo($('body'))
    .find('button')
    .focus();
}


function hideModalDialog($dialog = $('body > [aria-modal="true"]')) {
  if (!$body.find($dialog)) { return false; }
  $offscreenItems.append($dialog);
  $body.children('.container-fluid').removeAttr('inert');
  return true;
}


const showDisconnectWarningDialog = function() {
  if (document.activeElement) { oldFocus = $(document.activeElement); }
  showModalDialog($disconnectWarningDialog);
}


const showDisconnectDialog = function() {
  $offscreenItems.append($('#ss-connect-dialog'));
  $('#ss-reload-link').attr('tabindex', -1);
  $('#ss-overlay, #shiny-disconnect-overlay').remove();
  showModalDialog($disconnectDialog);
};


function createIdleTimer(idleTimeoutMins = 15) {
  function minsToMillseconds(mins) {
    return mins * 60 * 1000;
  }

  let idleTimer;
  const idleWarningTime = minsToMillseconds(idleTimeoutMins - 1);

  return function() {
    if (idleTimer) { window.clearTimeout(idleTimer); }
    const dialogRemoved = hideModalDialog($disconnectWarningDialog);
    if (dialogRemoved && oldFocus) { oldFocus.focus(); }
    idleTimer = setTimeout(showDisconnectWarningDialog, idleWarningTime);
  }
}

const restartIdleTimer = createIdleTimer($html.attr('data-timeout-mins'));

$(document).on('shiny:message shiny:inputchanged', restartIdleTimer);
restartIdleTimer();


function fixDisconnect() {
  $(document).on('shiny:disconnected', showDisconnectDialog);
  $disconnectDialog.on('click', () => $('#ss-reload-link').click());
  $disconnectWarningDialog.find('button').on('click', hideModalDialog);
}


export { fixDisconnect };


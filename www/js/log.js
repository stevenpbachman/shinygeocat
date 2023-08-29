const $messageLog = $('#message-log');
const options = { top: 0, left: 0, behavior:  'smooth' };


function elementAppendedMutation() {
  $messageLog[0].scroll(options);
}


function fixLog() {
  const elementAppendedObserver = new MutationObserver(elementAppendedMutation);
  const config = { subtree: true, childList: true };
  elementAppendedObserver.observe($messageLog[0], config);
}


export { fixLog };
const tabSelector = 'main [role="tabpanel"][tabindex]';


function elementUpdatedMutation() {
  $(tabSelector).removeAttr('tabindex');
}


function elementAppendedMutation(_, observer) {
  const $selection = $(tabSelector);
  if ($selection.length < $('main .tabpane').length) { return; }
  observer.disconnect();
  elementUpdatedMutation();
  const config = { attributes: true };
  const elementUpdatedObserver = new MutationObserver(elementUpdatedMutation, config);
  
  $selection.each(function() {
    elementUpdatedObserver.observe(this, config);
  });
}


function fixTabs() {
  const elementAppendedObserver = new MutationObserver(elementAppendedMutation);
  const config = { subtree: true, childList: true };
  elementAppendedObserver.observe($('main .tab-content')[0], config);
}


export { fixTabs };

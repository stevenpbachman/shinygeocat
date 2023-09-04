
function fixA11yIssues () {
  // Search bar should be of type search rather than have that role
  $('.search-input')
    .attr('type', 'search')
    .removeAttr('role');

  // The rest of this function makes sure tabbing around works properly.
  // Specifically, the focus doesn't get lost when opening or closing
  // the layers popup
  const $searchButton = $('.leaflet-control-search')
    .find('a.search-button');

  const $layerControl = $('#mymap .leaflet-control-layers');

  const $layerButton = $layerControl
    .find('a.leaflet-control-layers-toggle');

  const openClass = 'leaflet-control-layers-expanded';

  const elementMutatedCallback = function () {
    if (document.activeElement !== $layerButton[0]) { return; }
    if (!$layerControl.hasClass(openClass)) { return; }
    $layerControl.find('input[type="radio"]:checked')[0]?.focus();
  }

  const elementMutatedObserver = new MutationObserver(elementMutatedCallback);
  const config = { attributes: true };
  elementMutatedObserver.observe($layerControl[0], config);

  $layerControl.find('input[type="radio"]')
    .on('keydown', function (evt) {
      const { key, shiftKey } = evt;
      if (!['Enter', 'Escape', 'Tab'].includes(key)) { return; }
      $layerControl.removeClass(openClass);
      if (key === 'Tab' && !shiftKey) { return; }
      const $target = key === 'Tab' ? $searchButton : $layerButton;
      $target[0].focus();
      evt.preventDefault();
    });
}


function elementAppendedMutation(mutationList, observer) {
  const mutation = mutationList[0];
  const $target = $(mutation.target);
  // If the added item isn't the map we just ignore the mutation of body
  if ($target.attr('id') !== 'mymap') { return; }
  // If we're here then the map has been added. We can stop observing...
  observer.disconnect();
  // ...and fix the issues
  fixA11yIssues();
};


function fixMap() {
  // We need to wait for the map to be added to the body before we do anything
  const elementAppendedObserver = new MutationObserver(elementAppendedMutation);
  const config = { subtree: true, childList: true };
  elementAppendedObserver.observe(document.body, config);
}


export { fixMap };

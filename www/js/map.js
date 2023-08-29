
function fixA11yIssues () {
  $('.search-input')
    .attr('type', 'search')
    .removeAttr('role');

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
  if ($target.attr('id') !== 'mymap') { return; }
  observer.disconnect();
  fixA11yIssues();
};


function fixMap() {
  const elementAppendedObserver = new MutationObserver(elementAppendedMutation);
  const config = { subtree: true, childList: true };
  elementAppendedObserver.observe(document.body, config);
}


export { fixMap };

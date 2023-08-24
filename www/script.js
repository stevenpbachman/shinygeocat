$(function () {
  $('.well input[type="checkbox"]')
    .each(function () {
      const $input = $(this);
      const id = $input.attr('id');

      $input.siblings('div').find('label')
        .attr('for', id);
    });

  $('[role="presentation"][aria-label]')
    .removeAttr('aria-label');


  const mapReady = function () {
    const $searchButton = $('.leaflet-control-search')
      .find('a.search-button');

    const $layerControl = $('#mymap .leaflet-control-layers');

    const $layerButton = $layerControl
      .find('a.leaflet-control-layers-toggle');

    const openClass = 'leaflet-control-layers-expanded';

    const mutationCallback = function () {
      if (document.activeElement !== $layerButton[0]) { return; }
      if (!$layerControl.hasClass(openClass)) { return; }
      $layerControl.find('input[type="radio"]:checked')[0]?.focus();
    }

    const observer = new MutationObserver(mutationCallback);
    const config = { attributes: true };
    observer.observe($layerControl[0], config);

    $layerControl.find('input[type="radio"]')
      .on('keydown', function (evt) {
        const { key, shiftKey } = evt;
        // if (key === 'Enter') debugger;
        if (!['Enter', 'Escape', 'Tab'].includes(key)) { return; }
        $layerControl.removeClass(openClass);
        if (key === 'Tab' && !shiftKey) { return; }
        const $target = key === 'Tab' ? $searchButton : $layerButton;
        $target[0].focus();
        evt.preventDefault();
      });
  };


  const mutationCallback = function (mutationList, observer) {
    const mutation = mutationList[0];
    const $target = $(mutation.target);
    if ($target.attr('id') !== 'mymap') { return; }
    observer.disconnect();
    mapReady();
  };


  const elementAdded = new MutationObserver(mutationCallback)
  const config = { subtree: true, childList: true };
  elementAdded.observe(document.body, config);
});
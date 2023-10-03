
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


  const setupFloatingDialog = function($control, $dialog) {
    let clicking = false;

    $control
      .attr('aria-expanded', false)
      .attr('aria-controls', $dialog.attr('id'));

    $control.add($dialog)
      .on('mousedown', () => clicking = true)
      .on('mouseup', () => clicking = false);

    window.addEventListener('focusin', function() {
      if (clicking) { return; }

      const $target = $(document.activeElement);

      if ($control.attr('aria-expanded') === 'false') {
        if ($target.is($control)) {
          $dialog.insertAfter($control);
          $control.attr('aria-expanded', true);
        }
      }
      else {
        if (!$target.is($control) && !$dialog.find($target).length) {
          $control.attr('aria-expanded', false);
        }
      }
    });
  }

  const keyboardDialogs = [
    { control: '.leaflet-draw-draw-circlemarker', dialog: '#key-custom-point'  }
  ];

  keyboardDialogs.forEach(function({control, dialog}) {
    setupFloatingDialog($(control), $(dialog));
  });

  const createClamp = function(min, max) {
    return val => Math.min(Math.max(val, min), max);
  }  

  $('.floating-item input[type="number"]')
    .each(function() {
      const $input = $(this);
      const min = parseFloat($input.attr('min'));
      const max = parseFloat($input.attr('max'));
      const clamp = createClamp(min, max);
      $input.on('blur', () => $input.val(clamp($input.val())));
    });

  $('#key-add-point').on('click', function() {
    if (document.activeElement !== this) { return; }
    $('#custom-long')[0].focus();
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

function fixSearchInput() {
  // Search bar should be of type search rather than have that role
  $('.search-input')
  .attr('type', 'search')
  .removeAttr('role');
}


function fixKeyboard() {
  // Make sure tabbing around works properly.
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

  // Add floating dialog for keyboard users
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


function fixShapes() {
  const $html = $(document.documentElement);
  const circleRadius = parseFloat($html.attr('data-circle-radius'));

  const area = Math.PI * Math.pow(circleRadius, 2);


  const createPathTransformer = function(edgeLength, coords) {
    const getPathMid = path => `M${path.split(/[A-Z]/i)[1]} m ${circleRadius}, 0`; 

    const end = coords.map(function([x, y], i) {
      if (i === 0) {
        return `m ${x * edgeLength}, ${y * edgeLength}`;
      }
      const [px, py] = coords[i-1];
      const dx = (x - px) * edgeLength;
      const dy = (y - py) * edgeLength;
      const z = i === (coords.length - 1) ? 'Z' : ''; 
      return `l ${dx}, ${dy} ${z}`;
    }, '');

    return function(path) {
      const start = getPathMid(path);
      return `${start} ${end}`;
    }
  }


  const squarifyPath = (function() {
    const edgeLength = Math.sqrt(area);

    const coords = [
      [-1/2, -1/2],
      [1/2, -1/2],
      [1/2, 1/2],
      [-1/2, 1/2]
    ];

    return createPathTransformer(edgeLength, coords);
  })();


  const hexifyPath = (function() {
    const edgeLength = Math.sqrt((2*area) / (3*Math.sqrt(3)));
    const rt3o2 = Math.sqrt(3) / 2;

    const coords = [
      [-1/2, -rt3o2],
      [1/2, -rt3o2],
      [1, 0],
      [1/2, rt3o2],
      [-1/2, rt3o2],
      [-1, 0]
    ];

    return createPathTransformer(edgeLength, coords);
  })();

  const transformMarkerPath = function(marker, shape) {
    const path = marker.getAttribute('d');
    if (path === "M0 0") { return; } // offscreen
    if (path === marker.savedPath) { return; } // already altered shape
    const pathTransformer = { square: squarifyPath, hexagon: hexifyPath }[shape];
    marker.savedPath = pathTransformer(path);
    marker.setAttribute('d', marker.savedPath);
  }

  const childListMutateCallback = function (mutationList) {
    mutationList.forEach(function(d) {
      const mutatedElements = d.type === 'childList' ? d.addedNodes : [d.target];
      mutatedElements.forEach(function(el) {
        const $el = $(el);
        if ($el.hasClass('shape-square')) { transformMarkerPath(el, 'square'); }
        else if ($el.hasClass('shape-hexagon')) { transformMarkerPath(el, 'hexagon'); }
      });
    });
  }

  const $panes = $('#mymap :is(.leaflet-mappoints-pane, .leaflet-unmappoints-pane)');

  const elementMutatedObserver = new MutationObserver(childListMutateCallback);
  const config = { childList: true, subtree: true, attributes: true, attributeList: ['d'] };
  $panes.each(function() { elementMutatedObserver.observe(this, config); });

}


function fixA11yIssues () {
  fixSearchInput();  
  fixKeyboard();
  fixShapes();
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

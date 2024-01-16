//
// multislider.js
//
// definition of multislider mouse and event handling in the client.
// For this to work, slider.js needs to have been
// loaded. multislider() gets called with the container of the sliders
// already containing the sliders as uninitialized div elements with
// their idx set in the data-idx attribute.
//
// **********************************************************************
// Copyright (c) 2023 Orm Finnendahl <orm.finnendahl@selma.hfmdk-frankfurt.de>
//
// Revision history: See git repository.
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the Gnu Public License, version 2 or
// later. See https://www.gnu.org/licenses/gpl-2.0.html for the text
// of this agreement.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
//
// **********************************************************************


class MultiSliderElement extends HTMLElement {
//  static observedAttributes = ["color", "size"];

    constructor() {
        // Always call super first in constructor
        super();
//        console.log("o-multislider constructed: " + this );
    }

    connectedCallback() {
//        console.log("o-multislider added to page: " + this );
        multislider(this);
    }

    disconnectedCallback() {
        $(this).trigger("data", { close: true });
        console.log("o-multislider removed from page.");
    }

    adoptedCallback() {
//        console.log("o-multislider moved to new page.");
    }

    attributeChangedCallback(name, oldValue, newValue) {
        console.log(`Attribute ${name} has changed.`);
    }
}

customElements.define("o-multislider", MultiSliderElement );

function multislider(elem, config) {
    var thumb         =  'false';
    const pxRegex = /([0-9]+).*/
    var colors;
    var valueRange;
    var idx;
    var multislider = elem;
    var val = multislider.getAttribute('value');
    var offsetTop = multislider.offsetTop;
    var offsetLeft = multislider.offsetLeft;
    var multisliderHeight, multisliderWidth;
    var minValue, maxValue, value;
    var mapping;
    var direction;
    var clipZero = multislider.getAttribute('clip-zero');
    var mouseDownListener, mouseMoveListener;
    var sliders;
    var sliderType; // mvslider or mhslider, depending on direction
    var getFraction, getIdxFraction;  // functions for calculating the
                                      // Fractions on mousemove depending on
                                      // direction of the sliders.
    var innerBorder;     // we set one of the borders between the
                         // sliders to none except for the border of
    // the first slider. Depending on the direction of the sliders,
    // this is either border-top or border-left.
    
    var style = window.getComputedStyle(multislider, null);

    var numSliders = multislider.getAttribute('num-sliders');
//    var colors = JSON.parse(multislider.getAttribute('colors'));
    if (multislider.getAttribute('colors'))
        colors = multislider.getAttribute('colors').split(',');
    else colors = ['black'];

    // console.log('colors: ', colors);
    var numColors = colors.length;
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function disableDrag (elem) {
        elem.ondragstart = () => { return false; }
    }
    
    function initSlider (slider) {
        slider.setAttribute('class', sliderType);
        slider.setAttribute('style', 'border: 1px solid black;flex: 1 0 auto;');
        slider.setAttribute('min', minValue);
        slider.setAttribute('max', maxValue);
        slider.setAttribute('val', val);
        slider.setAttribute('clip-zero', clipZero);
        slider.setAttribute('mapping', mapping);
        slider.setAttribute('direction', direction);
        slider.style.setProperty('background-color', 'transparent');
        //        slider.style.setProperty('--thumb-color', 'black');
        slider.style.width = null;
        slider.style.height = null;
    }

    multislider.initSliders = function (num) {
        let currSlider;
//        console.log('initSliders');
        for (let i = 0; i < num; i++) {
            currSlider = multislider.children[i];
            initSlider(currSlider);
            currSlider.setAttribute('idx', i);
//            multislider.appendChild(currSlider);
//            console.log("color: " + colors[i%numColors]);
//            console.log("colors: ", colors);
            currSlider.setAttribute('bar-color', colors[i%numColors]);
            currSlider.firstChild.style.background =  colors[i%numColors];
            if(currSlider.getAttribute('thumb-color') == 'transparent')
                currSlider.firstChild.style.border = "none";
            else {
                currSlider.firstChild.style.borderStyle = "solid none none";
                currSlider.firstChild.style.borderWidth = "0.1em";
            }
            
            if (i > 0) currSlider.style.setProperty(innerBorder, 'none');
            currSlider.removeMouseDownListener();
        }
        sliders = multislider.children;
    }
        
    function setMinMaxMapping() {
        minValue      = parseFloat(multislider.getAttribute('min')) || 0.0;
        maxValue      = parseFloat(multislider.getAttribute('max')) || 1.0;
        mapping       = multislider.getAttribute('mapping') || 'lin';
        if (mapping == 'log') {
            if ((minValue == 0) && (maxValue == 0)) {
                minValue = 0.01;
                maxValue = 1;
                multislider.setAttribute('min', minvalue);
                multislider.setAttribute('max', maxValue);
            }
            else {
                if (minValue == 0) {
                    minValue = maxValue / 100;
                    multislider.setAttribute('min', minValue);
                }
                else {
                    if (maxValue == 0) {
                        maxValue = minValue / 100;
                        multislider.setAttribute('max', maxValue);
                    }
                }
            }
            valueRatio = maxValue/minValue;
        }
        else {
            valueRange = maxValue-minValue;
        }
        setDirection();
    }
    
    function setSliderHeightVal () {
        multisliderHeight = parseFloat(style.height.match(pxRegex)[1]);
    }

    function setSliderWidthVal () {
        multisliderWidth = parseFloat(style.width.match(pxRegex)[1]);
    }

    // Utils
    
    function getValFraction (val) {
        return ((val - minValue) / valueRange);
    }

    function getYFraction (event) {
        let boundingClientRect = multislider.getBoundingClientRect();
        let localYFrac = (boundingClientRect.height + boundingClientRect.top - event.clientY) / boundingClientRect.height;
        return clamp(localYFrac, 0, 1);
    }

    function getXFraction (event) {
        let boundingClientRect = multislider.getBoundingClientRect();
        let localXFrac = (event.clientX - boundingClientRect.left) / boundingClientRect.width;
        return clamp(localXFrac, 0, 1);
    }

    function getYFractionRev (event) {
        return (1 - getYFraction(event));
    }

    function getXFractionRev (event) {
        return (1 - getXFraction(event));
    }

    function setDirection () {
        direction = multislider.getAttribute('direction') || 'up';
        switch (direction) {
        case 'right':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = 'column';
            getFraction = getXFraction;
            getIdxFraction = getYFractionRev;
            break;
        case 'left':
            sliderType = 'mhslider';
            innerBorder = 'border-top';
            multislider.style.flexDirection = 'column';
            getFraction = getXFractionRev;
            getIdxFraction = getYFractionRev;
            break;
        case 'down':
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            getFraction = getYFractionRev;
            getIdxFraction = getXFraction;
            break;
        default: // 'up'
            sliderType = 'mvslider';
            innerBorder = 'border-left';
            getFraction = getYFraction;
            getIdxFraction = getXFraction;
        }
    }

    // mouse handling
    
    var moved = false;
    var lastIdx = false;
    var lastFraction = false;
    
    function mouseDownListener (event) {
        moved = false;
        let idxFraction = getIdxFraction(event);
        let valFraction = getFraction(event);
        idx = Math.floor(idxFraction*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        multislider.children[idx].setBarSize(valFraction);
        lastFraction = valFraction;
        lastIdx = idx;
        document.addEventListener('mousemove', mouseMoveListener);
        document.addEventListener('mouseup', mouseUpListener);
    }

    function interpolateSetBarSize (idx, fraction) {
        let dIdx = idx - lastIdx;
        let dFraction = fraction - lastFraction;
        if (lastIdx && (idx != lastIdx)) {
            if (idx > lastIdx) {
                for (let i = idx; i > lastIdx;i--) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                }
            }
            else {
                for (let i = idx; i < lastIdx;i++) {
                    let f = fraction + ((i-idx)/dIdx * dFraction);
                    sliders[i].setBarSize(f);
                }
            }
        }
        else sliders[idx].setBarSize(fraction);
    }
    
    function mouseMoveListener (event) {
        moved = true;
        let valFraction = getFraction(event);
        let idxFraction = getIdxFraction(event);
        idx = Math.floor(idxFraction*numSliders);
        if (idx >= numSliders) idx = numSliders - 1;
        if ((idx != lastIdx) || (valFraction != lastFraction)) {
            interpolateSetBarSize(idx, valFraction);
            lastFraction = valFraction;
            lastIdx = idx;
        }
    }

    function mouseUpListener (event){
        document.removeEventListener('mousemove', mouseMoveListener);
        document.removeEventListener('mouseup', mouseUpListener);
    }

    function init () {
        disableDrag(multislider);
        setSliderHeightVal();
        setSliderWidthVal();
        setMinMaxMapping();
        setDirection();
//        sliders = createSliders(numSliders, multislider);
        multislider.sliders = sliders;
        multislider.colors = colors;
        multislider.addEventListener('mousedown', mouseDownListener);
    }

    init();
}

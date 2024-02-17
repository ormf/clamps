//
// svg.js
//
// definition of a svg display.
//
//
// WARNING: Currently only the db-value attribute can be changed after
// initialization. All other attribute or style changes after
// initialization probably have no or detrimental effects.
//
// **********************************************************************
// Copyright (c) 2023/24 Orm Finnendahl
// <orm.finnendahl@selma.hfmdk-frankfurt.de>
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

class SvgElement extends HTMLElement {
    static observedAttributes = ['data',
                                 'cursor-pos', 'shift-x', 'shift-y', 'scale', 'piano-roll', 'staff-systems', 'bar-lines'
    ];

  constructor() {
    // Always call super first in constructor
      super();
      svg(this);
  }

    connectedCallback() {
//        console.log("o-svg added to page: " + this.value );
    }

  disconnectedCallback() {
      $(svg).trigger("data", {close: true});
      console.log("svg removed from page.");
  }

  adoptedCallback() {
    console.log("Custom element moved to new page.");
  }

    attributeChangedCallback(name, oldValue, newValue) {
        switch (name) {
        case 'data':
            this.setSVG(newValue);
            break;
        case 'cursor-pos':
            this.setPos(newValue);
            break;
        case 'shift-x':
            this.shiftX(newValue);
            break;
        case 'shift-y':
            this.shiftY(newValue);
            break;
        case 'scale':
            this.doScale(newValue);
            break;
        case 'piano-roll':
            this.doPianoRoll(newValue);
            break;
        case 'staff-systems':
            this.doStaffSystems(newValue);
            break;
        case 'bar-lines':
            this.doBarLines(newValue);
            break;
        }
    }
}


customElements.define("o-svg", SvgElement, { } );


function svg(elem){
//    console.log(elem, Date.now())
    // Settings

    var svg = elem;

    var svgContent
    var svgCursor;

    svg.setPos = function(pos) {
        svgCursor.style.left = Math.round((pos*100)) + '%';
    }

    svg.shiftX = function(translate) {
        console.log((-120 + -1*parseFloat(translate)));
        svgContent.style.transform = 'translate(' + (60 + -1*parseFloat(translate)) + 'em)';
    }

    svg.shiftY = function(translate) {
        svgContent.style.transform = 'translate( 0px, ' + -1*parseFloat(translate) + 'em)';
    }

    svg.doScale = function(scale) {
        svg.scale = scale;
        if (svgContent.firstChild) {
            svgContent.firstChild.setAttribute('width', scale*100 + '%');
            svgContent.firstChild.setAttribute('height', '100%');
        }
    }


    svg.doPianoRoll = function(value) {
        let svgImage = svgContent.firstChild;
        if (svgImage && document.getElementById('layer-2')) {
            if (value == 0)
                document.getElementById('layer-2').style.display = 'none';
            else
                document.getElementById('layer-2').style.display = '';
        }
    }


    svg.doStaffSystems = function(value) {
        let svgImage = svgContent.firstChild;
        if (svgImage && document.getElementById('layer-1')) {
            if (value == 0)
                document.getElementById('layer-1').style.display = 'none';
            else
                document.getElementById('layer-1').style.display = '';
        }
    }

    svg.doBarLines = function(value) {
        let svgImage = svgContent.firstChild;
        if (svgImage && document.getElementById('layer3')) {
            if (value == 0)
                document.getElementById('layer3').style.display = 'none';
            else
                document.getElementById('layer3').style.display = '';
        }
    }

    svg.setSVG = function(url) {
        svgContent.data = url;
        loadSVG(svgContent);
    }

    function parseViewBox(viewBoxString, asNumbers = false) {
        let values = viewBoxString.split(/[ ,]/).filter(Boolean); // filter removes empty strings   
        return asNumbers ? values.map(Number) : values;
    }

    async function loadSVG(svgContent) {
        let svgURL = svgContent.data;
        fetch(svgURL)
            .then((response) => response.text())
            .then((text) => {
                const parser = new DOMParser();
                const doc = parser.parseFromString(text, "text/xml");
                while (svgContent.firstChild) {
                    svgContent.removeChild(svgContent.lastChild);
                }           
                svgContent.appendChild(doc.documentElement);
                let [xmin, ymin, width, height]  = parseViewBox(svgContent.firstChild.getAttribute('viewBox'), true);
//                console.log(xmin, ymin, width, height);
                svg.doScale(svg.scale);
                svg.setPos(svg.getAttribute('cursor-pos'));
                svg.doPianoRoll(svg.getAttribute('piano-roll'));
                svg.doStaffSystems(svg.getAttribute('staff-systems'));
                svg.doBarLines(svg.getAttribute('bar-lines'));
                console.log(svg.getAttribute('cursor-pos'));
                $(svg).trigger("data", {width: (width/3.7179809)});
            });
    }
    


    
    function init() {
//        svg.style.background = 'var(--vu-background)';
        svg.style.position = 'relative';
        svg.style.display = 'flex';
        svg.style.alignItems = 'stretch';

        svgContent = document.createElement("object");
        svgContent.className="svg";
        svgContent.style.transform = 'translate(0px)';
        svgContent.style.background = '#fff';
        svgContent.data = svg.getAttribute('data') || '/html-display.svg';
        svgContent.type = 'image/svg.xml';
        svg.appendChild(svgContent);
        svgCursor  = document.createElement("object");
        svgCursor.className="cursor";
        svg.appendChild(svgCursor);
        loadSVG(svgContent);
    }
    
    init();

}

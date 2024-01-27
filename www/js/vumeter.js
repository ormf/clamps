//
// vumeter.js
//
// definition of a vumeter display.
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

class VuMeterElement extends HTMLElement {
    static observedAttributes = ['db-value'];

  constructor() {
    // Always call super first in constructor
      super();
      vumeter(this);
  }

    connectedCallback() {
//        console.log("o-vumeter added to page: " + this.value );
    }

  disconnectedCallback() {
      $(vumeter).trigger("data", {close: true});
      console.log("vumeter removed from page.");
  }

  adoptedCallback() {
    console.log("Custom element moved to new page.");
  }

    attributeChangedCallback(name, oldValue, newValue) {
        switch (name) {
        case 'db-value':
            this.value = Math.max(0, Math.min(parseFloat(newValue), 112)).toFixed(0);
            this.drawVu();
            break;
        }
    }

    
}


customElements.define("o-vumeter", VuMeterElement, { } );

function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }


function vumeter(elem){
//    console.log(elem, Date.now())
    // Settings
    var max             = elem.getAttribute('max') || 100;
    var boxCount        = elem.getAttribute('box-count') || 40;
    var ledColors       = elem.getAttribute('led-colors') || 'green';
    var barColor        = elem.getAttribute('bar-color') || 'rgba(60,60,255,1.0)';
    var vuType          = elem.getAttribute('vu-type') || 'led';
    var ledMapping      = elem.getAttribute('led-mapping') || 'db-lin';
    var vuDirection     = elem.getAttribute('direction') || 'up';
    var vuInnerPadding = elem.getAttribute('inner-padding') || '2px';
    var vuInnerPaddingBottom = elem.getAttribute('inner-padding-bottom') || '2px';

//    console.log(config);
    
    // Colours
    var redOn     = 'rgba(255,47,30,1.0)';
    var redOff    = 'rgba(64,12,8,1.0)';
    var yellowOn  = 'rgba(255,215,5,1.0)';
    var yellowOff = 'rgba(64,53,0,1.0)';
    var orangeOn  = 'rgba(215,215,5,1.0)';
    var orangeOff = 'rgba(53,53,0,1.0)';
    var greenOn   = 'rgba(53,255,30,1.0)';
    var greenOff  = 'rgba(13,64,8,1.0)';

    var PdPurple  = 'rgba(244,48,240,1.0)';
    var PdRed     = 'rgba(252,40,40,1.0)';
    var PdOrange  = 'rgba(250,171,71,1.0)';
    var PdYellow  = 'rgba(232,232,40,1.0)';
    var PdGreen   = 'rgba(20,232,20,1.0)';
    var colBlue1 = 'rgba(0, 85, 100, 1.0)';
    var colBlue2 = 'rgba(0, 102, 128, 1.0)';
    var colBlue3 = 'rgba(0, 136, 170, 1.0)';
    var colBlue4 = 'rgba(0, 170, 212, 1.0)';
    var colBlue5 = 'rgba(0, 190, 245, 1.0)';
    var colBlue6 = 'rgba(50, 202, 255, 1.0)';
    var colBlue7 = 'rgba(85, 211, 255, 1.0)';
    var colBlue8 = 'rgba(128, 222, 255, 1.0)';
    var colBlue9 = 'rgba(170, 235, 255, 1.0)';
    var colBlue10 = 'rgba(213, 246, 255, 1.0)';

    var colGreen1 = 'rgba(0, 85, 0, 1.0)';
    var colGreen2 = 'rgba(0, 128, 0, 1.0)';
    var colGreen3 = 'rgba(0, 170, 0, 1.0)';
    var colGreen4 = 'rgba(0, 212, 0, 1.0)';
    var colGreen5 = 'rgba(0, 255, 0, 1.0)';
    var colGreen6 = 'rgba(42, 255, 42, 1.0)';
    var colGreen7 = 'rgba(85, 255, 85, 1.0)';
    var colGreen8 = 'rgba(128, 255, 128, 1.0)';
    var colGreen9 = 'rgba(170, 255, 170, 1.0)';
    var colGreen10 = 'rgba(213, 255, 213, 1.0)';

    var colRed1 = 'rgba(85, 0, 0, 1.0)';
    var colRed2 = 'rgba(128, 0, 0, 1.0)';
    var colRed3 = 'rgba(170, 0, 0, 1.0)';
    var colRed4 = 'rgba(213, 0, 0, 1.0)';
    var colRed5 = 'rgba(255, 0, 0, 1.0)';
    var colRed6 = 'rgba(255, 42, 42, 1.0)';
    var colRed7 = 'rgba(255, 85, 85, 1.0)';
    var colRed8 = 'rgba(255, 128, 128, 1.0)';
    var colRed9 = 'rgba(255, 170, 170, 1.0)';
    var colRed10 = 'rgba(255, 213, 213, 1.0)';

    // Derived and starting values

    var colors = new Array(40);

    var vuHeight;
    var vuWidth;
    var vuBar;
    var setBarSize;
    
    var vuMeter = elem;
    var drawVu;

//    console.log('vuMeter: ' + vuMeter);
//    console.log('ledColors: ' + ledColors);

    var style = window.getComputedStyle(vuMeter, null);

    var lastVal = 0;
    var dbLedIdxLookup;

function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }
    
    // function createLedContainer (parent) {
    //     let vuLedContainer = document.createElement("div");
    //     vuLedContainer.style.height = "100%";
    //     vuLedContainer.style.width = "100%";
    //     vuLedContainer.style.padding = "2px";
    //     vuLedContainer.style.display = "flex";
    //     vuLedContainer.style.flexDirection = "column";
    //     vuLedContainer.style.padding = vuInnerPadding;
    //     vuLedContainer.style.paddingBottom = vuInnerPaddingBottom;
    //     vuLedContainer.style.justifyContent = "space-between";
    //     parent.appendChild(vuLedContainer);
    //     parent = vuLedContainer;
    // }

    function createLeds (parent) {
        let leds = document.createElement("div");
        leds.setAttribute("class", "vubar");
        leds.style.width = "100%";
//        leds.style.height = "100%";
        leds.style.backgroundSize = "0.5em 8em";
        leds.style.backgroundImage = "repeating-linear-gradient(to top, #000 0%, rgba(0,0,0,0) 0.1em, rgba(0,0,0,0) 0.25em),linear-gradient(to top, rgba(0, 85, 100, 1.0) 0%, rgba(0, 102, 128, 1.0) 2%, rgba(0, 136, 170, 1.0) 10%, rgba(0, 170, 212, 1.0) 17.5%, rgba(0, 190, 245, 1.0) 25.0%, rgba(50, 202, 255, 1.0) 32.5%, rgba(85, 211, 255, 1.0) 40.0%, rgba(128, 222, 255, 1.0) 47.5%, rgba(170, 235, 255, 1.0) 55.0%, rgba(213, 246, 255, 1.0) 62.5%, rgba(255, 170, 170, 1.0) 70.0%, rgba(255, 128, 128, 1.0) 77.5%, rgba(255, 85, 85, 1.0) 85.0%, rgba(255, 42, 42, 1.0) 92.5%, rgba(255, 0, 42, 1.0) 100%)";
        leds.style.backgroundPosition = "bottom";

        leds.style.border = "thin solid var(--vu-background)";
        leds.style.backgroundColor = "transparent";
        leds.style.position = "absolute";
        leds.style.left = "0";
        leds.style.bottom = "0";
        
        parent.appendChild(leds);
        parent.vuBar = leds;
    }


    function createBar (parent) {
        createLedContainer(parent);
        let vuBar = document.createElement("span");
        vuBar.style.height = "100%";
        vuBar.style.width = "100%";
//        vuBar.style.border = "thin solid var(--vu-background)";
        vuBar.style.border = "thin solid var(--vu-background)";
        vuBar.style.backgroundColor = "var(--vu-background)";
//        vuBar.style.display = "flex";
//        vuBar.style.flexDirection = "column";
//        vuBar.style.justifyContent = "space-between";
        parent.appendChild(vuBar);
        parent.vuBar = vuBar;

    }

    function setBarSizeY(db) {
//        console.log(((db/112)*vuHeight) + 'px');
        vuMeter.vuBar.style.height = dbLedIdxLookup[Math.round(db)] + '%';
    }
    
    function setBarSizeX(db) {
        vuMeter.vuBar.style.width = dbLedIdxLookup[Math.round(db)] + '%';
    }
    
    function drawBar () {
        var targetDB = clamp((100+parseInt(vuMeter.getAttribute("db-value"), 10)), 0, 140);
//        console.log('drawBar!' + targetDB + ' ' + colors[targetDB]);
        setBarSize(targetDB);
//        vuMeter.vuBar.style.backgroundColor = colors[dbLedIdxLookup[targetDB]];
    }

    function drawLed () {
        let leds = vuMeter.leds;
        var targetDB = clamp((100+parseInt(vuMeter.getAttribute("db-value"), 10)), 0, 140);
        setBarSize(targetDB);        
        lastVal = targetDB;
    };
    
//    const mySetAttribute = vuMeter.setAttribute;
    // override setAttribte

    function setPdColors () {
        let i;
        for (i = 0;i<16;i++) { colors[i] = PdGreen; }
        for (i = 16;i<26;i++) { colors[i] = PdYellow; }
        for (i = 26;i<28;i++) { colors[i] = PdOrange; }
        for (i = 28;i<39;i++) { colors[i] = PdRed; }
        colors[39] = PdPurple;
    }

    
    function setBlueColors () {
        let i;
        for (i = 0;i<1;i++) { colors[i] = colBlue1; }
        for (i = 1;i<4;i++) { colors[i] = colBlue2; }
        for (i = 4;i<7;i++) { colors[i] = colBlue3; }
        for (i = 7;i<10;i++) { colors[i] = colBlue4; }
        for (i = 10;i<13;i++) { colors[i] = colBlue5; }
        for (i = 13;i<16;i++) { colors[i] = colBlue6; }
        for (i = 16;i<19;i++) { colors[i] = colBlue7; }
        for (i = 19;i<22;i++) { colors[i] = colBlue8; }
        for (i = 22;i<25;i++) { colors[i] = colBlue9; }
        for (i = 25;i<28;i++) { colors[i] = colBlue10; }
        for (i = 28;i<31;i++) { colors[i] = colRed9; }
        for (i = 31;i<34;i++) { colors[i] = colRed8; }
        for (i = 34;i<37;i++) { colors[i] = colRed7; }
        for (i = 37;i<40;i++) { colors[i] = colRed6; }
     }

    function setGreenColors () {
        let i;
        for (i = 0;i<1;i++) { colors[i] = colGreen1; }
        for (i = 1;i<4;i++) { colors[i] = colGreen2; }
        for (i = 4;i<7;i++) { colors[i] = colGreen3; }
        for (i = 7;i<10;i++) { colors[i] = colGreen4; }
        for (i = 10;i<13;i++) { colors[i] = colGreen5; }
        for (i = 13;i<16;i++) { colors[i] = colGreen6; }
        for (i = 16;i<19;i++) { colors[i] = colGreen7; }
        for (i = 19;i<22;i++) { colors[i] = colGreen8; }
        for (i = 22;i<25;i++) { colors[i] = colGreen9; }
        for (i = 25;i<28;i++) { colors[i] = colGreen10; }
        for (i = 28;i<31;i++) { colors[i] = colRed9; }
        for (i = 31;i<34;i++) { colors[i] = colRed8; }
        for (i = 34;i<37;i++) { colors[i] = colRed7; }
        for (i = 37;i<40;i++) { colors[i] = colRed6; }
    }

    function setCustomColors (cols) {
        let i;
        for (i = 0;i<1;i++) { colors[i] = cols[0]; }
        for (i = 1;i<7;i++) { colors[i] = cols[1]; }
        for (i = 4;i<7;i++) { colors[i] = cols[2]; }
        for (i = 7;i<10;i++) { colors[i] = cols[3]; }
        for (i = 10;i<13;i++) { colors[i] = cols[4]; }
        for (i = 13;i<16;i++) { colors[i] = cols[5]; }
        for (i = 16;i<19;i++) { colors[i] = cols[6]; }
        for (i = 19;i<22;i++) { colors[i] = cols[7]; }
        for (i = 22;i<25;i++) { colors[i] = cols[8]; }
        for (i = 25;i<28;i++) { colors[i] = cols[9]; }
        for (i = 28;i<31;i++) { colors[i] = cols[10]; }
        for (i = 31;i<34;i++) { colors[i] = cols[11]; }
        for (i = 34;i<37;i++) { colors[i] = cols[12]; }
        for (i = 37;i<40;i++) { colors[i] = cols[13]; }
     }

    function setLedMapping () {
//        console.log('ledMapping: ' + ledMapping);
        switch (ledMapping) {
        case 'pd' :
            dbLedIdxLookup = [0.00, 2.50, 2.63, 2.76, 2.89, 3.03, 3.16, 3.29, 3.42, 3.55, 3.68, 3.82, 3.95, 4.08, 4.21, 4.34, 4.47, 4.61, 4.74, 4.87, 5.00, 5.13, 5.25, 5.38, 5.50, 5.63, 5.75, 5.88, 6.00, 6.13, 6.25, 6.38, 6.50, 6.63, 6.75, 6.88, 7.00, 7.13, 7.25, 7.38, 7.50, 8.00, 8.50, 9.00, 9.50, 10.00, 10.50, 11.00, 11.50, 12.00, 12.50, 13.00, 13.50, 14.00, 14.50, 15.00, 15.50, 16.00, 16.50, 17.00, 17.50, 18.00, 18.50, 19.00, 19.50, 20.00, 20.50, 21.00, 21.50, 22.00, 22.50, 23.33, 24.17, 25.00, 26.25, 27.50, 28.33, 29.17, 30.00, 31.25, 32.50, 33.75, 35.00, 36.25, 37.50, 38.75, 40.00, 41.25, 42.50, 43.75, 45.00, 47.50, 48.75, 50.00, 52.50, 55.00, 57.50, 60.00, 62.50, 67.50, 72.50, 77.50, 82.50, 85.00, 87.50, 90.00, 92.50, 93.75, 95.00, 97.50, 98.33, 99.17, 100.00];            
            break;
        case 'db-lin' :
            dbLedIdxLookup = [0.00, 0.73, 1.45, 2.17, 2.90, 3.63, 4.35, 5.07, 5.80, 6.53, 7.25, 7.97, 8.70, 9.43, 10.15, 10.88, 11.60, 12.32, 13.05, 13.77, 14.50, 15.23, 15.95, 16.67, 17.40, 18.13, 18.85, 19.58, 20.30, 21.02, 21.75, 22.48, 23.20, 23.92, 24.65, 25.38, 26.10, 26.83, 27.55, 28.27, 29.00, 29.73, 30.45, 31.17, 31.90, 32.63, 33.35, 34.08, 34.80, 35.53, 36.25, 36.97, 37.70, 38.42, 39.15, 39.88, 40.60, 41.33, 42.05, 42.78, 43.50, 44.22, 44.95, 45.67, 46.40, 47.13, 47.85, 48.58, 49.30, 50.03, 50.75, 51.47, 52.20, 52.92, 53.65, 54.38, 55.10, 55.83, 56.55, 57.28, 58.00, 58.72, 59.45, 60.17, 60.90, 61.63, 62.35, 63.08, 63.80, 64.53, 65.25, 65.97, 66.70, 67.43, 68.15, 68.88, 69.60, 70.32, 71.05, 71.78, 72.50, 74.79, 77.08, 79.38, 81.67, 83.96, 86.25, 88.54, 90.83, 93.13, 95.42, 97.71, 100.00];
            break;
        }
    }
    
    function setBarDirection () {
        switch (vuDirection) {
        case 'right' :
            setBarSize = setBarSizeX;
            vuMeter.style.flexDirection = "row";
            break;
        case 'left' :
            setBarSize = setBarSizeX;
            vuMeter.style.flexDirection = "row-reverse";
            break;
        case 'down' :
            setBarSize = setBarSizeY;
            vuMeter.style.flexDirection = "column";
            break;
        default : // 'up'
            setBarSize = setBarSizeY;
            vuMeter.style.flexDirection = "column-reverse";
            break;
        }
    }

    function init() {
        vuMeter.style.background = 'var(--vu-background)';
        vuMeter.style.position = 'relative';
        vuHeight = parseFloat(style.height);
        vuWidth = parseFloat(style.width);
//        console.log('height: ' + vuHeight + ', width: ' + vuWidth + ' ' + vuType);
        switch (ledColors) {
        case "green":
            setGreenColors();
            break;
        case "blue":
            setBlueColors();
            break;
        case "pd":
            setPdColors();
            break;
        default:
            if (ledColors.length == 14) {
                setCustomColors(ledColors);
            }
            else setGreenColors();
            break;
        }
        setLedMapping();
        switch(vuType) {
        case 'led' :
//            console.log('vuType: led');
            vuMeter.drawVu = drawLed;
            createLeds(vuMeter);
            break;
        case 'bar' :
//            console.log('vuType: bar');
            createBar(vuMeter);
            vuMeter.drawVu = drawBar;
            break;
        }
        setBarDirection();
        vuMeter.drawVu();
    }
    
    init();

}

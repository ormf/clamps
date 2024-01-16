// preact is a library for javascript frontend
import { render, createRef, createContext } from 'preact';
import { useSignal, useComputed, useSignalEffect } from "@preact/signals";
import { html } from 'htm/preact';
import register from 'preact-custom-element';

// this is implemented by me
import { onMouseDownHandler, onTouchStartHandler } from './utils.js';

function getPrecision (num) {
    let absNum = Math.abs(num);
    let fraction = (100 * (absNum - Math.floor(absNum)));
    if ((fraction == 0) || (fraction == 100)) return 0;
    if (fraction%10 == 0) return 1;
    return 2}

function formatValue (value, precision) {
    return value.toFixed(Math.min(precision, getPrecision(value)));
}


// this is an preact component. It is basically an advanced html element.
// It gets the html properties as input and returns a DOM like object.
// It gets reevaluated every time props change.
export function knob(props) {
  // createRef allows us to access html elements, we will use that later.
  const svg = createRef();
  const elem = createRef();
  // beforeunload gets called before we close the window.
  addEventListener("beforeunload", (event) => {
      elem.current ? $(elem.current.parentNode).trigger("data", {close: true}) : false;
  });

  // these are our properties: value, min and max
    // useSignal() returns a singleton so we get the same signal
    // everytime.
    const val = useSignal();
    val.value = parseFloat(props.value);
    const min = useSignal();
    min.value = parseFloat(props.min || 0);
    const max = useSignal();
    max.value = parseFloat(props.max || 100);
    const unit = (props.unit === "") ? "" : " " + props.unit;

    // this is similar to watch. Again in a singleton version. This function gets called every time some (relevant) signal changes.
  useSignalEffect(() => {
    let value = val.value;
    elem.current ? $(elem.current.parentNode).trigger("data", {value: value}) : ""
  });

    function polarToCartesian(centerX, centerY, radius, degrees) {
        var angleInRadians = (Math.PI * (degrees + 90) / 180);

        return {
            x: centerX + (radius * Math.cos(angleInRadians)),
            y: centerY + (radius * Math.sin(angleInRadians))
        };
    }
    
  // these are also singletons. These are derived values that get updated everytime a relevant signal changes.
    const range = useComputed(() => max.value - min.value)
    const percentage = useComputed(() => (val.value-min.value) / range.value)
    const step = useSignal(parseFloat(props.step || 1));
    const sensitivity = useSignal(parseFloat(props.sensitivity || 200));
    const precision = useSignal(parseFloat(props.precision || 2));
    const startpoint = polarToCartesian(8,8,8,18);
    const endpoint = polarToCartesian(8,8,8,342);
    const xa = useComputed(() => (8*Math.cos(Math.PI * ((percentage.value*324)+108) / 180))+8)
    const ya = useComputed(() => (8*Math.sin(Math.PI * ((percentage.value*324)+108) / 180))+8)
    const xb = useComputed(() => (8*Math.cos(Math.PI * Math.min(432, ((percentage.value*324)+116)) / 180))+8)
    const yb = useComputed(() => (8*Math.sin(Math.PI * Math.min(432, ((percentage.value*324)+116)) / 180))+8)
    const xl = useComputed(() => -9*Math.cos((0.05 + (percentage.value * 0.9))*Math.PI*2-(Math.PI/2))+8)
    const yl = useComputed(() => -9*Math.sin((0.05 + (percentage.value * 0.9))*Math.PI*2-(Math.PI/2))+8)
    const largeArcFlag1 = useComputed(() => percentage < 0.56 ? 0 : 1)
    const largeArcFlag2 = useComputed(() => percentage < 0.42 ? 1 : 0)
    const d2 = useComputed(() => `M ${endpoint.x} ${endpoint.y} A 8 8 0 ${largeArcFlag2} 0 ${xb} ${yb}`)
    const d1 = useComputed(() => `M ${xa} ${ya} A 8 8 0 ${largeArcFlag1} 0 ${startpoint.x} ${startpoint.y}`)

//    console.log('precision: ' + precision);
    const valString = useComputed(() => formatValue(val.value, precision))
    // this is a weird construct I usually use it to avoid writing my mousemove eventhandlers over and over again.
    // it is basically a getter and a setter that gets called whenever the mouse moves (after it got registered see below).
    const mm = {
        _y: 0,
        _x: 0,
        _lastx: 0,
        _lasty: 0,
        startvalue: 0,
        get x() {
            this.startvalue = val.value;
            return 0;
        },
        set x(value) {
            this._x = value;
            return true;
        },
        get y() {
            this.startvalue = val.value;
            return 0;
        },
        set y(value) {
            let diff = (-1 * value) - this._y + this._x - this._lastx;
//            console.log('diff: ' + diff);
            this._lastx = this._x;
            this._y = (-1 * value);
            let n = diff*range/sensitivity;
//            console.log('n: ' + n);
            val.value = Math.max(min.value, Math.min(max.value, val.value + n));

            return true;
        }
    }
    
 
  // html parses the string and turns it into a DOM like object. `Backtick strings and ${jscode} are JavaScripts template strings`.
  // More info: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals
  // and: https://github.com/developit/htm for the used tag function

//        <path d="${d1}" stroke="#444444" fill="none" stroke-width="2" />

    return html`
    <div class="knob" ref=${elem}>
      <svg ref=${svg} xmlns="http://www.w3.org/2000/svg" onMouseDown=${onMouseDownHandler(mm)} onTouchStart=${onTouchStartHandler(mm)} viewBox="0 0 16 16">
        <path d="${d1}" stroke="lightgreen" fill="none" stroke-width="2" />
        <path d="${d2}" stroke="#222" fill="none" stroke-width="2" />
        <line x1="50%" y1="50%" x2=${xl} y2=${yl} id="pointer" />
      </svg>
      <span class="value">${valString}${unit}</span>
    </div>
  `
}

// this turns our component function into a custom element so now we can use <o-knob></o-knob> in html.
// Whenever value changes the function gets reevaluated and the dom might change.
// shadow is an option. Shadow DOM is a new feature that we do not want to use.
register(knob, 'o-knob', ['value'], { shadow: false });

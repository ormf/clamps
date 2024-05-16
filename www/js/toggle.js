// toggle and radiobox

class ToggleElement extends HTMLElement {
    static observedAttributes = ['label-off', 'label-on', 'value'];

    constructor() {
        // Always call super first in constructor
        super();
        toggle(this, { unloadEvent: true });
//        this.addEventListener('mousedown', this.mouseDownListener);
//        console.log('o-toggle constructed: ' + this );
    }

    connectedCallback() {
//        console.log('o-toggle added to page: ' + this );
//        toggle(this, { unloadEvent: true });
    }

    disconnectedCallback() {
        $(this).trigger('data', { close: true });
//        console.log('o-toggle removed from page.');
    }

    adoptedCallback() {
//        console.log('o-toggle moved to new page.');
    }

    attributeChangedCallback(name, oldValue, newValue) {
//        console.log('now: ', JSON.stringify(this));        
        switch (name) {
        case 'label-off':
            this.drawToggle(this.getAttribute('value'));
            break;
        case 'label-on':
            this.drawToggle(this.getAttribute('value'));
            break;

        case 'value':
            let value;
            if (this.externalValueChange == true) {
                value = parseFloat(newValue);
                // if (value != valueOff) value = valueOn;
            }
            else value = newValue;
            this.drawToggle(value);
            if (this.externalValueChange == false) {
                $(this).trigger('data', { value: value });
                this.externalValueChange = true;
            }
            break;
        }
//        console.log(`Attribute ${name} has changed.`);
    }
}

customElements.define('o-toggle', ToggleElement );

function toggle (elem, config) {

    var myToggle;

    var unloadEvent = config.unloadEvent;
    if (elem.nodeType == undefined)
        myToggle = elem.get(0);
    else
        myToggle = elem;

    myToggle.drawToggle = function (val) {
        if (val != valueOff) {
            myToggle.textContent = myToggle.getAttribute('label-on');
            myToggle.style.color = colorOn;
            myToggle.style.background = backgroundOn;
        }
        else {
            myToggle.textContent = myToggle.getAttribute('label-off');
            myToggle.style.color = colorOff;
            myToggle.style.background = backgroundOff;
        }
    }
    myToggle.draw = myToggle.drawToggle;

    
    var colorOff         = myToggle.getAttribute('color-off') || 'black';
    var colorOn          = myToggle.getAttribute('color-on') || 'black';
    var backgroundOff    = myToggle.getAttribute('background-off') || 'white';
    var backgroundOn     = myToggle.getAttribute('background-on') || 'black';
    var labelOff         = myToggle.getAttribute('label-off') || '';
    var labelOn          = myToggle.getAttribute('label-on') || '';
    

    var valueOff         = myToggle.getAttribute('value-off') || '0';
    var valueOn          = myToggle.getAttribute('value-on') || '1';

    // myToggle.externalValueChange = false;
    // 
    // override setAttribute
//    const mySetAttribute = myToggle.setAttribute;
    
    myToggle.colorOff = colorOff;
    myToggle.backgroundOff = backgroundOff;
    myToggle.labelOff = labelOff;
    myToggle.valueOff = valueOff;
    myToggle.colorOn = colorOn;
    myToggle.backgroundOn = backgroundOn;
    myToggle.labelOn = labelOn;
    myToggle.valueOn = valueOn;
    

    // myToggle.setAttribute = function (key, value) {
    //     console.log('attr-change: ' + key + ', ' + value);
    //     if (key == 'value') {
    //         if (myToggle.externalValueChange == true) {
    //             value = parseFloat(value);
    //             // if (value != valueOff) value = valueOn;
    //         }
    //         mySetAttribute.call(myToggle, key, value);
    //         myToggle.drawToggle(value);
    //         if (myToggle.externalValueChange == false) {
    //             $(myToggle).trigger('data', { value: value });
    //             myToggle.externalValueChange = true;
    //         }
    //     }
    // }
    

    function mouseDownListener (event) {
        myToggle.externalValueChange = false;
        let val = (myToggle.getAttribute('value') == myToggle.valueOff)? valueOn : valueOff;
        myToggle.setAttribute('value', val);
    }

    myToggle.removeMouseDownListener = () => {
        myToggle.removeEventListener('mousedown', mouseDownListener);
    }


    function disable () { return false };
    
    function init () {
//        console.log('init: ', myToggle);
        myToggle.colorOff = colorOff;
        myToggle.backgroundOff = backgroundOff;
        myToggle.labelOff = labelOff;
        myToggle.valueOff = valueOff;
        myToggle.colorOn = colorOn;
        myToggle.backgroundOn = backgroundOn;
        myToggle.labelOn = labelOn;
        myToggle.valueOn = valueOn;
        myToggle.ondragstart = () => { return false; }
        myToggle.removeMouseDownListener();
        myToggle.addEventListener('mousedown', mouseDownListener);
        if (unloadEvent)
            addEventListener('beforeunload', (event) => {
                $(myToggle).trigger("data", {close: true})});
        myToggle.onselectstart = disable;
        // myToggle.addEventListener('dblclick', function(event) {
        //     event.preventDefault();
        //     event.stopPropagation();
        // }, true);
        let val = parseFloat(myToggle.getAttribute('value')).toFixed(0);
        if ((val != valueOff) && (val != valueOn)) {
            val = valueOff;
//            mySetAttribute.call(myToggle, 'value', val);
        }
        myToggle.drawToggle(val);
        myToggle.externalValueChange = true;
    }

    init();
//     myToggle.addEventListener('mousedown', mouseDownListener);

}

// RADIO Button

class RadioElement extends HTMLElement {
//  static observedAttributes = ['color', 'size'];

    constructor() {
        // Always call super first in constructor
        super();
//        console.log('o-radio constructed: ' + this );
    }

    connectedCallback() {
//        console.log('o-radio added to page: ' + this );
        radio(this);
    }

    disconnectedCallback() {
        $(this).trigger('data', { close: true });
        console.log('o-radio removed from page.');
    }

    adoptedCallback() {
        console.log('o-radio moved to new page.');
    }

    attributeChangedCallback(name, oldValue, newValue) {
        console.log(`Attribute ${name} has changed.`);
    }
}

customElements.define('o-radio', RadioElement );

function radio (elem) {

    var myRadio;

    if (elem.nodeType == undefined)
        myRadio = elem.get(0);
    else
        myRadio = elem;

    var colorOff;
    if (myRadio.getAttribute('color-off'))
        colorOff = myRadio.getAttribute('color-off').split(',');
    else colorOff = ['black'];

    var colorOn;
    if (myRadio.getAttribute('color-on'))
        colorOn = myRadio.getAttribute('color-on').split(',');
    else colorOn = ['black'];

    var backgroundOff;
    if (myRadio.getAttribute('background-off')) {
        backgroundOff = myRadio.getAttribute('background-off').split(',');
    }
    else backgroundOff = ['white'];

    var backgroundOn;
    if (myRadio.getAttribute('background-on'))
        backgroundOn = myRadio.getAttribute('background-on').split(',');
    else backgroundOn = ['black'];

    var labelOff;
    if (myRadio.getAttribute('label-off'))
        labelOff = myRadio.getAttribute('label-off').split(',');
    else labelOff = [''];

    var labelOn;
    if (myRadio.getAttribute('label-on'))
        labelOn = myRadio.getAttribute('label-on').split(',');
    else labelOn = [''];



//    console.log('colorOff: ' + colorOff);
//    console.log('colorOn: ' + colorOn);
//    console.log('backgroundOff: ' + backgroundOff);
//    console.log(backgroundOff);
//    console.log('backgroundOn: ' + backgroundOn);
//    console.log('labelOff: ' + labelOff);
//    console.log('labelOn: ' + labelOn);



    var direction     = myRadio.getAttribute('direction') || 'right';

    var numButtons           = myRadio.getAttribute('data-num') || ['1.0'];

    var lenColOn = colorOn.length;
    var lenColOff = colorOff.length;
    var lenBgOn = backgroundOn.length;
    var lenBgOff = backgroundOff.length;
    var lenLbOn = labelOn.length;
    var lenLbOff = labelOff.length;

    const pxRegex = /([0-9]+).*/
    const valChangeEvent = new Event('valuechange');
    var sliderType, moved, idx, innerBorder, mouseDownListener, oldValue;

    var getFraction;

    myRadio.externalValueChange = true;
    
    var style = window.getComputedStyle(myRadio, null);

//    myRadio.height = parseFloat(style.height.match(pxRegex)[1]);
//    myRadio.width = parseFloat(style.width.match(pxRegex)[1]);

    // Utils
    
    function clamp(number, min, max) {
        return Math.max(min, Math.min(number, max));
    }

    function makeToggle () {
        let div = document.createElement('div');
        div.setAttribute('class', 'mvradio');
        div.setAttribute('style', 'border: 1px solid black;flex: 1 1 auto;position: relative;min-width 0; min-height: 0');
        div.style.width = '';
        div.style.height = '';
        div.style.flex = '1 0 auto';
        div.style.display = 'flex';
        div.style.justifyContent = 'center';
        div.style.pointerEvents = 'none';
        return div;
    }

    function createToggles (num, parent) {
        let toggles = new Array(num);
        let currSlider;
        let idx;
        for (let i = 0; i < num; i++) {
            let currToggle = makeToggle();
            parent.appendChild(currToggle);
            idx = ((direction == 'up') || (direction == 'left'))? (num - i - 1) : i;
            currToggle.setAttribute('data-idx', idx);
            currToggle.setAttribute('color-off', colorOff[idx%lenColOff]);
            currToggle.setAttribute('background-off', backgroundOff[idx%lenBgOff]);
            currToggle.setAttribute('label-off', labelOff[idx%lenLbOff]);
            currToggle.setAttribute('color-on', colorOn[idx%lenColOn]);
            currToggle.setAttribute('background-on', backgroundOn[idx%lenBgOn]);
            currToggle.setAttribute('label-on', labelOn[idx%lenLbOn]);
            if (i > 0) currToggle.style.setProperty(innerBorder, 'none');
            toggles[idx] = currToggle;
            toggle(currToggle, { unloadEvent: false });
            if (idx == oldValue) {
                currToggle.setAttribute('value', 1);
                currToggle.draw(1);
            }
            else {
                currToggle.setAttribute('value', 0);
                currToggle.draw(0);
            }
            currToggle.removeMouseDownListener();
        }
        return toggles;
    }

    function getYFraction (event) {
        let rect = myRadio.getBoundingClientRect();
        let localYFrac = (rect.height + rect.top - event.clientY) / rect.height;
//        console.log(rect.height + ', ' + rect.top +  ', ' + event.clientY + ', ' + localYFrac)
        return clamp(localYFrac, 0, 1);

    }

    function getYFractionRev (event) {
         return (1 - getYFraction(event));
    }

    function getXFraction (event) {
        let rect = myRadio.getBoundingClientRect();
        let localXFrac = (event.clientX - rect.left) / rect.width;
        return clamp(localXFrac, 0, 1);
    }

   function getXFractionRev (event) {
         return (1 - getXFraction(event));
    }
    
    function mouseDownListener (event) {
//        console.log('mousedown');
        moved = false;
        let Fraction = getFraction(event);
//        console.log(Fraction);
        idx = clamp(Math.floor(Fraction*numButtons), 0, numButtons - 1);
        myRadio.externalValueChange = false;
        myRadio.setAttribute('value', idx);
    }

    function drawRadio (val) {
        myRadio.toggles[oldValue].setAttribute('value', 0);
        myRadio.toggles[oldValue].draw(0);
        myRadio.toggles[val].setAttribute('value', 1);
        myRadio.toggles[val].draw(1);
        oldValue = val;
    }
    
    // overwrite setAttribute

    const mySetAttribute = myRadio.setAttribute;
    
    myRadio.setAttribute = function (key, value) {
        if (key == 'value') {
            if (myRadio.externalValueChange == true) {
                value = clamp(parseFloat(value).toFixed(0), 0, numButtons - 1);
            }
            mySetAttribute.call(myRadio, key, value);
            drawRadio(value);
            if (myRadio.externalValueChange == false) {
                //                myRadio.dispatchEvent(valChangeEvent);
                $(myRadio).trigger('data', { value: value });
                myRadio.externalValueChange = true;
            }
        }
    }

    function setDirection () {
        switch (direction) {
        case 'down':
            sliderType = 'vradio';
            getFraction = getYFractionRev;
            innerBorder = 'border-top';
            myRadio.style.flexDirection = 'column';
            break;
        case 'up':
            sliderType = 'vradio';
            getFraction = getYFraction;
            innerBorder = 'border-top';
            myRadio.style.flexDirection = 'column';
            break;
        case 'left':
            sliderType = 'hradio';
            getFraction = getXFractionRev;
            innerBorder = 'border-left';
            break;
        default: // right
            sliderType = 'hradio';
            getFraction = getXFraction;
            innerBorder = 'border-left';
            break;
        }
    }

    function disable () { return false };


    function init () {
        setDirection();
//        console.log(backgroundOff.split(','));
        oldValue = clamp(parseFloat(myRadio.getAttribute('value')).toFixed(0), 0, numButtons - 1);
        myRadio.toggles = createToggles(numButtons, myRadio);
        myRadio.style.display = 'flex';
        myRadio.addEventListener('mousedown', mouseDownListener);
        addEventListener('beforeunload', (event) => {
            $(myRadio).trigger("data", {close: true})});
        myRadio.onselectstart = disable;
    }
    
    init();

}

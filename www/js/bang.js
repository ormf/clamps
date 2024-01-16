class BangElement extends HTMLElement {
//  static observedAttributes = ["color", "size"];

    constructor() {
        // Always call super first in constructor
        super();
//        console.log("o-bang constructed: " + this );
    }

    connectedCallback() {
//        console.log("o-bang added to page: " + this );
        bang(this);
    }

    disconnectedCallback() {
//        console.log("Custom element removed from page.");
    }

    adoptedCallback() {
//        console.log("Custom element moved to new page.");
    }

    attributeChangedCallback(name, oldValue, newValue) {
        console.log(`Attribute ${name} has changed.`);
    }
}

customElements.define("o-bang", BangElement );

function bang (elem) {

    var myBang;
    
    if (elem.nodeType == undefined)
        myBang = elem.get(0);
    else
        myBang = elem;
    let style = getComputedStyle(elem);
//    console.log(style.colorOn);
    var flashTime        = myBang.getAttribute('flash-time') || '150'; // flash time in ms
//    var pulseTime        = config.pulseTime || '250'; // pulse time in ms
    var colorOn          = myBang.getAttribute('color-on') || 'black';
    var colorOff         = myBang.getAttribute('color-off') || 'black';
    var backgroundOn     = myBang.getAttribute('background-on') || 'black';
    var backgroundOff    = myBang.getAttribute('background-off') || 'white';
    var labelOn          = myBang.getAttribute('label-on') || '';
    var labelOff         = myBang.getAttribute('label-off') || '';


    // override setAttribute
//    const mySetAttribute = myBang.setAttribute;

    
    // myBang.setAttribute = function (key, value) {
    //     if (key == 'data-val') {
    //         if (myBang.externalValueChange == true) {
    //             value = parseFloat(value).toFixed(0);
    //             if (value != valueOff) value = valueOn;
    //         }
    //         mySetAttribute.call(myBang, key, value);
    //         drawBang(value);
    //         if (myBang.externalValueChange == false) {
    //             myBang.dispatchEvent(valChangeEvent);
    //             myBang.externalValueChange = true;
    //         }
    //     }
    // }
    function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
    }

    function bang() {
//        console.log('bang')
        flashBang();
        if (myBang.externalValueChange == false) {
            $(myBang).trigger('data', { bang: true })

//            myBang.dispatchEvent(bangEvent);
        }
    }

    function pulseOn(ms) {
        myBang.pulseActive = true;
        console.log("pulseTime: ", ms);
        pulseBang(ms);
    }
 
    function pulseOff() {
        myBang.pulseActive = false;
        myBang.textContent = myBang.labelOff;
        myBang.style.color = myBang.colorOff;
        myBang.style.background = myBang.backgroundOff;
     }

    
    async function pulseBang(ms) {
        if (myBang.pulseActive == true) {
            if (myBang.pulseState == false) {
                myBang.textContent = myBang.labelOn;
                myBang.style.color = myBang.colorOn;
                myBang.style.background = myBang.backgroundOn;
                myBang.pulseState = true;
            }
            else {
                myBang.textContent = myBang.labelOff;
                myBang.style.color = myBang.colorOff;
                myBang.style.background = myBang.backgroundOff;
                myBang.pulseState = false;
            }
            await sleep(ms);
            pulseBang(ms);
        }
    }

 
    
    async function flashBang() {
        if (myBang.flashTime > 0) {
            myBang.textContent = myBang.labelOn;
            myBang.style.color = myBang.colorOn;
            myBang.style.background = myBang.backgroundOn;
            
            await sleep(myBang.flashTime);

            myBang.textContent = myBang.labelOff;
            myBang.style.color = myBang.colorOff;
            myBang.style.background = myBang.backgroundOff;
        }
    }

    function highlight (v) {
        if (v == 0)
            myBang.style.background = myBang.backgroundOff;
        else
            myBang.style.background = myBang.backgroundOn;
    }

    
    function mouseDownListener (event) {
        myBang.externalValueChange = false;
        bang();
        myBang.externalValueChange = true;
    }

    myBang.removeMouseDownListener = () => {
        myBang.removeEventListener('mousedown', mouseDownListener);
    }

//    myBang.draw = drawBang;

    function disable () { return false };
    
    function init () {
        myBang.flashTime = flashTime;
//        myBang.pulseTime = pulseTime;
        myBang.externalValueChange = true;
        myBang.pulseState = false;
        myBang.pulseActive = false;
        myBang.colorOff = colorOff;
        myBang.backgroundOff = backgroundOff;
        myBang.labelOff = labelOff;
        myBang.colorOn = colorOn;
        myBang.backgroundOn = backgroundOn;
        myBang.labelOn = labelOn;
        myBang.ondragstart = disable;
        myBang.addEventListener('mousedown', mouseDownListener);
        addEventListener('beforeunload', (event) => {
            $(myBang).trigger("data", {close: true})});
        myBang.onselectstart = disable;
        myBang.textContent = myBang.labelOff;
        myBang.style.color = myBang.colorOff;
        myBang.style.background = myBang.backgroundOff;
        // myBang.addEventListener('dblclick', function(event) {
        //     event.preventDefault();
        //     event.stopPropagation();
        // }, true);
//        let val = parseFloat(myBang.getAttribute('data-val')).toFixed(0);
        // if ((val != valueOff) && (val != valueOn)) {
        //     val = valueOff;
        //     mySetAttribute.call(myBang, 'data-val', val);
        // }
//        drawBang(val);
        myBang.externalValueChange = true;
        myBang.bang = bang;
        myBang.pulseOn = pulseOn;
        myBang.pulseOff = pulseOff
        myBang.highlight = highlight;
    }

    init();
}

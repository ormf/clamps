class BangElement extends HTMLElement {
    static observedAttributes = ['highlight', 'label-off', 'label-on'];

    constructor() {
        // Always call super first in constructor
        super();
        bang(this);
//        console.log("o-bang constructed: " + this );
    }

    connectedCallback() {
//        console.log("o-bang added to page: " + this );
    }

    disconnectedCallback() {
//        console.log("Custom element removed from page.");
    }

    adoptedCallback() {
//        console.log("Custom element moved to new page.");
    }

    attributeChangedCallback(name, oldValue, newValue) {
//        console.log(`Attribute ${name} has changed: ~{newValue}`)
        switch (name) {
        case 'highlight':
            this.highlight(parseInt(newValue));
            break;
        case 'label-off':
            this.textContent = newValue;
            break;
        case 'label-on':
            this.textContent = newValue;
            break;
        }
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
    var flashTime        = myBang.getAttribute('flash-time') || '150'; // flash time in ms

    function sleep(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));
    }

    function bang() {
//        console.log('bang')
        flashBang();
        if (myBang.externalValueChange == false) {
            $(myBang).trigger('data', { bang: true })
        }
    }

    function pulseOn(ms) {
        myBang.pulseActive = true;
        if (myBang.highlightState === myBang.getAttribute('background-off'))
            (myBang.pulseState == false);
        else
            (myBang.pulseState == true);
        pulseBang(ms);
    }

    function setBang(v) {
        if (v == 0) {
            myBang.textContent = myBang.getAttribute('label-off') || '';
            myBang.style.color = myBang.getAttribute('color-off') || 'black';
            myBang.style.background = myBang.getAttribute('background-off') || 'white';
        }
        else {
            myBang.textContent = myBang.getAttribute('label-on') || '';
            myBang.style.color = myBang.getAttribute('color-on') || 'black';
            myBang.style.background = myBang.getAttribute('background-on') || 'black';
        }
    }
    
    function pulseOff() {
        myBang.pulseActive = false;
     }

    
    async function pulseBang(ms) {
        if (myBang.pulseActive == true) {
            if (myBang.pulseState == false) {
                setBang(1);
                myBang.pulseState = true;
            }
            else {
                setBang(0);
                myBang.pulseState = false;
            }
            await sleep(ms);
            pulseBang(ms);
        }
    }
 
    async function flashBang() {
        if (myBang.getAttribute('flash') != '0') {
            if (myBang.flashTime > 0) {
                bang.flashing = true;
                if (myBang.highlightState == 0)
                    setBang(1);
                else
                    setBang(0);
                await sleep(myBang.flashTime);
                setBang(myBang.highlightState);
                bang.flashing = false;

            }
        }
    }

    function highlight(v) {
        myBang.highlightState = v;
        switch (v) {
        case 0: myBang.pulseOff();
            if (!bang.flashing)
                setBang(0);
            break;
        case 1: myBang.pulseOff();
            if (!bang.flashing)
                setBang(1);
            break;
        case 2:
            myBang.pulseOn(250);
            break;
        }
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
        myBang.highlightState = 0;
        myBang.ondragstart = disable;
        myBang.addEventListener('mousedown', mouseDownListener);
        addEventListener('beforeunload', (event) => {
            $(myBang).trigger("data", {close: true})});
        myBang.onselectstart = disable;
        myBang.textContent = myBang.labelOff;
        myBang.style.color = myBang.colorOff;
        myBang.style.background = myBang.backgroundOff;
        myBang.externalValueChange = true;
        myBang.bang = bang;
        myBang.pulseOn = pulseOn;
        myBang.pulseOff = pulseOff
        myBang.highlight = highlight;
        setBang(0);
    }

    init();
}

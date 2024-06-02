class ScopeElement extends HTMLElement {
    static observedAttributes = ['values'];

    constructor() {
        // Always call super first in constructor
        super();
//        console.log("o-scope constructed: " + this );
    }

    connectedCallback() {
        scope(this);
//        console.log("o-scope added to page: " + this );
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
        case 'values':
            console.log(newValue);
//            this.highlight(parseInt(newValue));
            break;
        }
    }
}

customElements.define("o-scope", ScopeElement );

function scope (elem) {

    var scope;
    
    if (elem.nodeType == undefined)
        scope = elem.get(0);
    else
        scope = elem;
    let style = getComputedStyle(elem);

//    scope.draw = drawScope;

    function disable () { return false };

    function redrawScope() {
        let path = 'M ';
        let length = 100;
        let width = scope.offsetWidth;
        let halfHeight = scope.offsetHeight/-2;

        if (scope.buffer) {
            length = scope.buffer.length
            scope.buffer.forEach(function(point,i) {
                path += (((width + (width / length))/ length) * i) + ' '
                    + ((halfHeight * 0.95 * point) - halfHeight) + ', ';
            }.bind(scope))
            scope.wave.setAttribute('d', path);
            //        console.log(path);
        }
    }
    
    scope.setValues = function (values) {
        scope.buffer = values;
        redrawScope();
//        console.log(scope.buffer);
    }

    const onresize = (dom_elem, callback) => {
        const resizeObserver = new ResizeObserver(() => callback() );
        resizeObserver.observe(dom_elem);
//        console.log('resize');
    };

    function makeScope () {
//        scope.width = "18em";
//        scope.height = "12em";
        // 
        // // Create the oscilloscope svg element
        let wave = document.createElementNS("http://www.w3.org/2000/svg", 'path');
        wave.setAttribute('class', 'oscilloscope_wave');
        let svg = document.createElementNS("http://www.w3.org/2000/svg", "svg");
//        let svg = document.createElement("object", "svg");
        svg.setAttribute('class', 'oscilloscope_svg');
        svg.setAttributeNS("http://www.w3.org/2000/xmlns/", "xmlns:xlink", "http://www.w3.org/1999/xlink");
        svg.appendChild(wave);
        scope.svg = svg;
        scope.wave = wave;

        scope.appendChild(svg);
    }
    
    function init () {
//        scope.addEventListener('mousedown touchstart', mouseDownListener);
//        addEventListener('beforeunload', (event) => {
        //            $(scope).trigger("data", {close: true})});
        makeScope();
        onresize(scope, redrawScope);
    }

    init();
}

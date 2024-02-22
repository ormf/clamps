class SvgElement extends HTMLElement {
    static observedAttributes = ['data',
                                 'cursor-pos', 'shift-x', 'shift-y', 'scale', 'piano-roll', 'staff-systems', 'bar-lines', 'global-x-scale', 'inverse'
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
        case 'inverse':
            this.doInverse(newValue);
            break;
        }
    }
}


customElements.define("o-svg", SvgElement, { } );


function svg(elem){
//    console.log(elem, Date.now())
    // Settings

    var globalScale;
    var svg = elem;

    var svgContent
    var svgCursor;

    svg.setPos = function(pos) {
        svgCursor.style.left = Math.round((pos*100)) + '%';
    }

    svg.shiftX = function(translate) {
//        console.log((-120 + -1*parseFloat(translate)));
        svgContent.style.transform = 'translate(' + (60 + -120*(svg.scale/100)*svg.scaleXAdjust*(parseFloat(translate)*100)/svg.width) + 'em)';
    }

    svg.shiftY = function(translate) {
        svgContent.style.transform = 'translate( 0px, ' + -1*parseFloat(translate) + 'em)';
    }

    svg.doGlobalXScale = function(scale) {
        svg.scaleXAdjust = scale;
        svg.shiftX(parseFloat(svg.getAttribute('shift-x')));
    }

    svg.doInverse = function(value) {
        if (value == 0) {
            svgCursor.style.background = 'black';
            svg.style.background = 'white';
            svgContent.style.background = 'white';
        }
        else {
            svgCursor.style.background = 'white';
            svg.style.background = 'black';
            svgContent.style.background = 'black';
        }
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
        if (svgImage && svg.pianoRoll) {
            if (value == 0)
                svg.pianoRoll.style.display = 'none';
            else
                svg.pianoRoll.style.display = '';
        }
    }


    svg.doStaffSystems = function(value) {
        let svgImage = svgContent.firstChild;
        if (svgImage && svg.staffLines) {
            if (value == 0)
                svg.staffLines.style.display = 'none';
            else
                svg.staffLines.style.display = '';
        }
    }

    svg.doBarLines = function(value) {
        let svgImage = svgContent.firstChild;
        if (svgImage && svg.barLines) {
            if (value == 0)
                svg.barLines.style.display = 'none';
            else
                svg.barLines.style.display = '';
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
  //      console.log(svgURL);
        if (svgURL) {
            fetch(svgURL)
                .then((response) => response.text())
                .then((text) => {
                    const parser = new DOMParser();
                    const doc = parser.parseFromString(text, "text/xml");
                    while (svgContent.firstChild) {
                        svgContent.removeChild(svgContent.lastChild);
                    }
                    svg.svgImage = doc.documentElement;
                    svgContent.appendChild(svg.svgImage);
                    let [xmin, ymin, width, height]  = parseViewBox(svgContent.firstChild.getAttribute('viewBox'), true);
                    //                console.log(xmin, ymin, width, height);
                    svg.doScale(svg.scale);
                    svg.setPos(svg.getAttribute('cursor-pos'));
                    let groups = Array.from(svg.querySelectorAll("g"));
                    svg.staffLines = groups.filter(g => g.getAttribute('inkscape:label') === 'Stafflines')[0];
                    svg.pianoRoll = groups.filter(g => g.getAttribute('inkscape:label') === 'PianoRoll')[0];
                    svg.barLines = groups.filter(g => g.getAttribute('inkscape:label') === 'Barlines')[0];
                    svg.doPianoRoll(svg.getAttribute('piano-roll'));
                    svg.doStaffSystems(svg.getAttribute('staff-systems'));
                    svg.doBarLines(svg.getAttribute('bar-lines'));
                    //                console.log(svg.getAttribute('cursor-pos'));
                    svg.width = width;
//                    console.log('width: ', width);
                    svg.setAttribute('width', width);
                    svg.shiftX(svg.getAttribute('shift-x'));
                    $(svg).trigger("data", {width: (width)});
                });
        }
    }
    
    const onresize = (dom_elem, callback) => {
        const resizeObserver = new ResizeObserver(() => callback() );
        resizeObserver.observe(dom_elem);
    };
    
    function resize() {
//        console.log('resize', svg.getBoundingClientRect().width)
    }
    
    function init() {
//        svg.style.background = 'var(--vu-background)';
        svg.scaleXAdjust = 1;
        svg.style.position = 'relative';
        svg.style.display = 'flex';
        svg.style.alignItems = 'stretch';

        let data = svg.getAttribute('data') || false;
//        console.log('svgContent.data: ', data);
        svgContent = document.createElement("object");
        svgContent.className="svg";
        svgContent.style.transform = 'translate(0px)';
        svgContent.style.background = '#fff';
        svgContent.data = data;
        svgContent.type = 'image/svg.xml';
        svg.appendChild(svgContent);
        svgCursor  = document.createElement("object");
        svgCursor.className="cursor";
        svg.appendChild(svgCursor);
        if (data) loadSVG(svgContent);
        onresize(svg, resize);
    }
    
    init();

}

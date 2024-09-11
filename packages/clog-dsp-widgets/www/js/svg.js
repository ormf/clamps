class SvgElement extends HTMLElement {
    static observedAttributes = ['svg-file', 'cursor-pos', 'shift-x', 'shift-y', 'scale',
                                 'piano-roll', 'staff-systems', 'bar-lines',
                                 'global-x-scale', 'inverse', 'crosshairs', 'bandwidth', 'ats-contrast', 'mousepos'];

  constructor() {
    // Always call super first in constructor
      super();
      svg(this);
  }

    connectedCallback() {
//        console.log("o-svg added to page: " + this.value );
    }

  disconnectedCallback() {
      $(svg).trigger('data', {close: true});
      console.log('svg removed from page.');
  }

  adoptedCallback() {
    console.log('Custom element moved to new page.');
  }

    attributeChangedCallback(name, oldValue, newValue) {
        switch (name) {
        case 'svg-file':
            console.log('resetting svg...', newValue);
            this.setSVG('svg/' + newValue);
//            console.log('done');
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
        case 'crosshairs':
            this.doCrossHairs(newValue);
            break;
        case 'bandwidth':
            this.doBandWidth(newValue);
            break;
        case 'ats-contrast':
            this.doAtsContrast(newValue);
            break;
        case 'mousepos' :
            let newPos = JSON.parse('[' + newValue.replace(/[()]/g, '').replace(/ +/g, ', ').replace(/d0/g, '') + ']');
            this.doMousePos(newPos);
        }
    }
}

customElements.define('o-svg', SvgElement, { } );

function svg(elem){
//    console.log(elem, Date.now())
    // Settings

    const clamp = (val, min, max) => Math.min(Math.max(val, min), max)
    
    var externalValueChange = true;

    var globalScale;
    var svg = elem;

    var svgCursor;
    var svgHCross;
    var svgVCross;

    var svgUpperReject;
    var svgLowerReject;
    var svgLeftReject;
    var svgRightReject;
    var svgAtsRect;

//    svg.body = document.getElementsByTagName('body')[0];
    
    svg.setPos = function(pos) {
        svgCursor.style.left = Math.round((pos*100)) + '%';
    }

    svg.shiftX = function(translate) {
        let style = getComputedStyle(svg);
        let winWidth = svg.offsetWidth / parseFloat(style.getPropertyValue('font-size'));
        let svgEmWidth = winWidth * svg.baseScale;
        svg.baseShift = winWidth/2;
//        console.log('svg.baseShift: ' + svg.baseShift);
        svg.shiftXVal = parseFloat(translate);
//        console.log('svg.width: ' + svg.width);
        svg.svgContent.style.transform =
            'translate(' + (svg.baseShift + -1 * svgEmWidth * 0.9969y
                            * (svg.shiftXVal/svg.width)) + 'em)';
    }

    svg.shiftY = function(translate) {
        svg.svgContent.style.transform = 'translate( 0px, ' + -1*parseFloat(translate) + 'em)';
    }

    svg.doGlobalXScale = function(scale) {
        svg.scaleXAdjust = scale;
        svg.shiftX(parseFloat(svg.getAttribute('shift-x')));
    }

    svg.doInverse = function(value) {
        if (value == 0) {
            svgCursor.style.background = 'black';
            svg.style.background = 'white';
            if (svg.svgImage) svg.svgImage.style.backgroundColor = 'white';
        }
        else {
            svgCursor.style.background = 'white';
            svg.style.background = 'black';
            if (svg.svgImage) svg.svgImage.style.backgroundColor = 'black';
        }
    }

    
    svg.doScale = function(scale) {
        if (svg.contentData) {
//            console.log('doScale', scale);
            svg.scale = scale;
            if (svg.svgContent.firstChild) {
                svg.svgImage.setAttribute('width', Math.round(svg.baseScale*scale*100) + '%');
                svg.svgImage.setAttribute('height', '100%');
            }
            
            let shiftX = parseFloat(svg.getAttribute('shift-x'));
//            console.log('shiftX', shiftX, 'shiftXVal', svg.shiftXVal);
            {
                svg.shiftX(shiftX);
//                console.log('shiftX', shiftX, 'shiftXVal', svg.shiftXVal);
                //        $(svg).trigger('data', {shift-x: shiftX});
            }
        }
    }

    svg.doPianoRoll = function(value) {
        let svgImage = svg.svgContent.firstChild;
        if (svgImage && svg.pianoRoll) {
            if (value == 0)
                svg.pianoRoll.style.display = 'none';
            else
                svg.pianoRoll.style.display = '';
        }
    }

    svg.doStaffSystems = function(value) {
        let svgImage = svg.svgContent.firstChild;
        if (svgImage && svg.staffLines) {
            if (value == 0)
                svg.staffLines.style.display = 'none';
            else
                svg.staffLines.style.display = '';
        }
    }

    svg.doBarLines = function(value) {
        let svgImage = svg.svgContent.firstChild;
        if (svgImage && svg.barLines) {
            if (value == 0)
                svg.barLines.style.display = 'none';
            else
                svg.barLines.style.display = '';
        }
    }
    
    svg.doCrossHairs = function(value) {
        if (value == 0) {
            svgVCross.style.display = 'none';
            svgHCross.style.display = 'none';
            svgUpperReject.style.display = 'none';
            svgLowerReject.style.display = 'none';
            svgLeftReject.style.display = 'none';
            svgRightReject.style.display = 'none';
            svgAtsRect.style.display = 'none';
            svg.removeEventListener('dblclick', dblClickMouseHandler);
        }
        else {
            svg.MouseActive = false;
            svg.addEventListener('dblclick', startDblClickMouseHandler);
        }
    }

    svg.doBandWidth = function(value) {
        svg.bandWidth = parseFloat(value);
        redrawCoords();
    }

    svg.doAtsContrast = function(value) {
        svg.contrast = parseFloat(value);
        svgUpperReject.style.opacity = svg.contrast;        
        svgLowerReject.style.opacity = svg.contrast;        
        svgLeftReject.style.opacity = svg.contrast;        
        svgRightReject.style.opacity = svg.contrast;        
//        redrawCoords();
    }

    svg.doMousePos = function(value) {
//        console.log('doMousePos: ', value);
        if (externalValueChange) {
            svg.XPos = value[0];
            svg.YPos = value[1];
        }
        else {
            $(svg).trigger('data', { mousepos: [svg.XPos, svg.YPos]});
        }
        redrawCoords ();
    }
    

    function startDblClickMouseHandler (e) {
        if (svg.getAttribute('crosshairs') != 0) {
            svgVCross.style.display = '';
            svgHCross.style.display = '';
            svgUpperReject.style.display = '';
            svgLowerReject.style.display = '';
            svgLeftReject.style.display = '';
            svgRightReject.style.display = '';
            svgAtsRect.style.display = '';
            dblClickMouseHandler(e);
        }
    }

    
    function dblClickMouseHandler (e) {
        if (svg.MouseActive) {
            svg.removeEventListener('mousemove', crossHairsMouseHandler);
            svg.style.cursor = '';
            svg.MouseActive = false;
        }
        else {
            svg.MouseActive = true;
            svg.style.cursor = 'none';
            svg.addEventListener('mousemove', crossHairsMouseHandler);
            crossHairsMouseHandler(e);
        }
    }

    function redrawCoords () {
        let x = svg.XPos;
        let y = 1-svg.YPos;
        let upperHeight = (svg.bandWidth >= 1)? 0 : 100*clamp((y - (svg.bandWidth/2)), 0, 1);
        let lowerHeight = (svg.bandWidth >= 1)? 0 : 100*clamp(1 - (y + (svg.bandWidth/2)), 0, 1);
        let rejectHeight = (svg.bandWidth >= 1)? 100 : (y < svg.bandWidth/2)? 100 * (y + (svg.bandWidth/2)) : 100 * svg.bandWidth;
        let atsRectHeight = (svg.bandWidth >= 1)? 100 : (1-y < (svg.bandWidth/2)) ? 100 * (svg.bandWidth/2 + 1 - y) : rejectHeight;
        svgHCross.style.top = y * 100 + '%';
        svgVCross.style.left = x * 100 + '%';
//        console.log('bandwidth: ', svg.bandWidth, 'y: ', y);
        svgUpperReject.style.height = upperHeight + '%';
        svgLowerReject.style.height = lowerHeight  + '%';
        svgLeftReject.style.width = 100 * clamp((x - 0.005), 0, 1) + '%';
        svgRightReject.style.width = 100 * clamp(1 - (x + 0.005), 0, 1) + '%'; + '%';
        svgLeftReject.style.height = rejectHeight + '%';
        svgRightReject.style.height = rejectHeight + '%';
        svgLeftReject.style.top =  upperHeight + '%';
        svgRightReject.style.top = upperHeight + '%';
        svgAtsRect.style.width = 1 + '%';
        svgAtsRect.style.height = atsRectHeight + '%';
        svgAtsRect.style.bottom = lowerHeight + '%';
        svgAtsRect.style.left = 100 * clamp((x - 0.005), 0, 1) + '%';
    }
    
    function crossHairsMouseHandler (e) {
        let rect = svg.getBoundingClientRect();
        svg.XPos = (e.clientX - rect.left) / rect.width; //x position within the element.
        svg.YPos = 1-((e.clientY - rect.top) / rect.height);  //y position within the element.
//        console.log('Left? : ' + x + ' ; Top? : ' + y + '.');
        externalValueChange = false;
        svg.setAttribute('mousepos', [svg.XPos, svg.YPos]);        
        externalValueChange = true;
    }
    
    svg.setSVG = function(url) {
//        console.log('url: ', url);
        svg.svgContent.data = url;
        svg.contentData = url;
        svg.reload = true;
        loadSVG(svg.svgContent);
    }
    
    svg.setSVG = function(url) {
//        console.log('url: ', url);
        svg.svgContent.data = url;
        svg.contentData = url;
        svg.reload = true;
        loadSVG(svg.svgContent);
    }

    
    function parseViewBox(viewBoxString, asNumbers = false) {
        let values = viewBoxString.split(/[ ,]/).filter(Boolean); // filter removes empty strings   
        return asNumbers ? values.map(Number) : values;
    }

    async function loadSVG(svgContent) {
        let svgURL = svgContent.data;
//        console.log('svgURL: ', svgURL);
        if (svgURL) {
            fetch(svgURL)
                .then((response) => response.text())
                .then((text) => {
                    const parser = new DOMParser();
                    const doc = parser.parseFromString(text, 'text/xml');
                    while (svgContent.firstChild) {
                        svgContent.removeChild(svgContent.lastChild);
                    }
                    svg.svgImage = doc.documentElement;
//                    console.log(svg.svgImage);
                    svgContent.appendChild(svg.svgImage);
                    let [xmin, ymin, width, height]  = parseViewBox(svgContent.firstChild.getAttribute('viewBox'), true);
                    //                console.log(xmin, ymin, width, height);
                    svg.baseScale = width/300;
                    svg.doScale(svg.scale);
                    svg.setPos(svg.getAttribute('cursor-pos'));
                    let groups = Array.from(svg.querySelectorAll('g'));
                    svg.staffLines = groups.filter(g => g.getAttribute('inkscape:label') === 'Stafflines')[0];
                    svg.pianoRoll = groups.filter(g => g.getAttribute('inkscape:label') === 'PianoRoll')[0];
                    svg.barLines = groups.filter(g => g.getAttribute('inkscape:label') === 'Barlines')[0];
                    svg.doPianoRoll(svg.getAttribute('piano-roll'));
                    svg.doStaffSystems(svg.getAttribute('staff-systems'));
                    svg.doBarLines(svg.getAttribute('bar-lines'));
                    svg.doInverse(svg.getAttribute('inverse'));
                    //                console.log(svg.getAttribute('cursor-pos'));
//                    console.log('svg.width: ', svg.width, 'width: ', width);
                    if (svg.width != width) {
                        svg.width = width;
                        svg.setAttribute('width', width);
                        svg.shiftX(svg.getAttribute('shift-x'));
                        $(svg).trigger('data', {width: (width)});
                    }
                });
        }
    }
    
    const onresize = (dom_elem, callback) => {
        const resizeObserver = new ResizeObserver(() => callback() );
        resizeObserver.observe(dom_elem);
//        console.log('resize');
    };
    
    function resize() {
//        console.log('resize', svg.getBoundingClientRect().width);
        svg.shiftX(svg.getAttribute('shift-x'));
    }
    
    function init() {
//        svg.style.background = 'var(--vu-background)';
        svg.reload = false;
        svg.scaleXAdjust = 1;
        svg.style.position = 'relative';
        svg.style.display = 'flex';
        svg.style.alignItems = 'stretch';

        let data = svg.getAttribute('data') || false;
//        console.log('svgContent.data: ', data);
        svg.svgContent = document.createElement('object');
        svg.svgContent.className='svg';
//        svg.svgContent.style.transform = 'translate(0px)';
        svg.svgContent.style.background = '#fff';
        svg.svgContent.type = 'image/svg.xml';
        svg.appendChild(svg.svgContent);
        svgCursor  = document.createElement('div');
        svgCursor.className='cursor';
        svg.appendChild(svgCursor);
        svgHCross  = document.createElement('div');
        svgHCross.className='hcross';
        svg.appendChild(svgHCross);
        svgVCross  = document.createElement('div');
        svgVCross.className='vcross';
        svg.appendChild(svgVCross);
        svgVCross.style.display = 'none';
        svgHCross.style.display = 'none';
        svgUpperReject  = document.createElement('div');
        svgUpperReject.className='upperreject';
        svgLowerReject  = document.createElement('div');
        svgLowerReject.className='lowerreject';
        svgLeftReject  = document.createElement('div');
        svgLeftReject.className='leftreject';
        svgRightReject  = document.createElement('div');
        svgRightReject.className='rightreject';
        svgAtsRect  = document.createElement('div');
        svgAtsRect.className='atsrect';
        svg.appendChild(svgUpperReject);
        svg.appendChild(svgLowerReject);
        svg.appendChild(svgLeftReject);
        svg.appendChild(svgRightReject);
        svg.appendChild(svgAtsRect);
        svgRightReject.style.display = 'none';
        svgLeftReject.style.display = 'none';
        svgUpperReject.style.display = 'none';
        svgLowerReject.style.display = 'none';
        svgAtsRect.style.display = 'none';
        svg.contentData = data;
        if (data) loadSVG(data);
        onresize(svg, resize);
    }
    
    init();

}

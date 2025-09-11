//
// sensors.js
//
// definition of motion/accelerometer sensors event handling for
// mobile devices.
//
// **********************************************************************
// Copyright (c) 2025 Orm Finnendahl
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

// Attributes: min, max, mapping, clip-zero, thumb-color, bar-color

class SensorElement extends HTMLElement {
    static observedAttributes = [ 'interval', 'sensor-data' ];


    constructor() {
        // Always call super first in constructor
        super();
        sensors(this);
//        console.log("o-slider constructed: " + this );
    }

    connectedCallback() {
//        console.log("o-slider added to page: " + this );
    }

    disconnectedCallback() {
        $(this).trigger("data", { close: true });
//        console.log("o-sensors removed from page.");

    }

    adoptedCallback() {
//        console.log("o-slider moved to new page.");
    }

    attributeChangedCallback(name, oldValue, newValue) {
        // console.log(`Attribute ${name} has changed: ${newValue}`)
        switch (name) {
        case 'interval':
            this.setInterval(parseFloat(newValue));
            break;
        case 'sensor-data':
            this.setSensorData(newValue);
            break;
        }
    }
}

customElements.define("o-sensors", SensorElement );

var os = "";

if(navigator.userAgent.match(/Android/i)) {
    os = "Android";
} else if (navigator.userAgent.match(/iPhone/i)) {
    os = "iPhone";
} else {
    console.log("NonMobile OS");
};

function isNumber(number) {
    return typeof number === 'number' && !isNaN(number)
}

function clamp(number, lower, upper) {
    if (!isNumber(number) || !isNumber(lower) || !isNumber(upper)) {
        throw new Error('All inputs must be valid numbers.')
    }

    return Math.min(Math.max(number, lower), upper)
}

function parseBool(value){
    if (value === 'false')
        return false;
    else
        return true;
}

function sensors(elem) {
    
    var sensor;
    if (elem.nodeType == undefined)
        sensor = elem.get(0);
    else
        sensor = elem;


//    console.log('sensor: ' + sensor);

    function disableDrag (elem) {
       // elem.ondragstart = undefined
    }

    sensor.prepend(document.createElement('br'));
    var button = document.createElement('button');
    button.className = "btn btn-lg btn-success py-1";
    button.setAttribute("id", "start_demo");
    button.setAttribute("type", "button");
    button.innerHTML = "Start Demo";
    button.style.fontSize = "3em";
    //    sensor.appendChild(button);
    sensor.prepend(button);
    sensor.externalValueChange = true;

    var interval = 50;
    var id = false;
    var internalValueChange = false;

    var sensorDataVals = [];
    var oa = 0;
    var ob = 0;
    var og = 0;

    var x = 0;
    var y = 0;
    var z = 0;

    var gx = 0;
    var gy = 0;
    var gz = 0;

    var gyrox = 0;
    var gyroy = 0;
    var gyroz = 0;

    function setInterval(value) {
        interval = value;
    }

    function setId(value) {
        id = value;
    }

    const myRe0 = /[\n\r]+/g;
    //    const myRe1 = /#S\(sensor-data :oa (.+)\)/g;


    var gAccScalar = 1.0;

    if(os == "iPhone") {
        gAccScalar = 2.0;
    } else if(os == "Android") {
        gAccScalar = 20.0;
    };

    
    function normalizeGXY(value) {
        if(os==""){
            return value;
        } else {
            let val = (value / gAccScalar) + 0.5;
            return clamp(val, 0.0, 1.0);
        }
    };

    function normalizeGZ(value) {
        if(os==""){
            return value;
        } else {
            let val = value / gAccScalar;
            return clamp(val, 0.0, 1.0);
        }
    };

    function setSensorData(value) {

        if (internalValueChange == false) {
            //            console.log (value.replaceAll(myRe0, " "));
            
            //            console.log (value.replaceAll(myRe0, " ").replaceAll(/\((.+)\)/g, "[$1]").replaceAll(/ +/g, ", "));
            sensorDataVals = JSON.parse(value.replaceAll(myRe0, " ").replaceAll(/\((.+)\)/g, "[$1]").replaceAll(/ +/g, ", "));
            
            oa = sensorDataVals[0];
            ob = sensorDataVals[1];
            og = sensorDataVals[2];
            
            x = sensorDataVals[3];
            y = sensorDataVals[4];
            z = sensorDataVals[5];

            gx = sensorDataVals[6];
            gy = sensorDataVals[7];
            gz = sensorDataVals[8];
            
            gyrox = sensorDataVals[9];
            gyroy = sensorDataVals[10];
            gyroz = sensorDataVals[11];
        }
        else
            sensorDataVals = value;

        //        console.log ( sensorDataVals );


        updateFieldIfNotNull('Orientation_a', oa);
        updateFieldIfNotNull('Orientation_b', ob);
        updateFieldIfNotNull('Orientation_g', og);
        updateFieldIfNotNull('Accelerometer_x', x);
        updateFieldIfNotNull('Accelerometer_y', y);
        updateFieldIfNotNull('Accelerometer_z', z);
        updateFieldIfNotNull('Accelerometer_gx', gx);
        updateFieldIfNotNull('Accelerometer_gy', gy);
        updateFieldIfNotNull('Accelerometer_gz', gz);
        updateFieldIfNotNull('Gyroscope_x', gyrox);
        updateFieldIfNotNull('Gyroscope_y', gyroy);
        updateFieldIfNotNull('Gyroscope_z', gyroz);
        if (sensor.internalValueChange)
            $(sensor).trigger( "data", { "sensor-data": sensorDataVals } );
        //   [ oa, ob, og, x, y, z, gx, gy, gz, gyrox, gyroy, gyroz ]
        sensor.internalValueChange = false;
    }

    function handleOrientation(event) {
//        internalValueChange = true;
        oa = event.alpha;
        ob = event.beta;
        og = event.gamma;
//        internalValueChange = false;
//        incrementEventCount();
    }

    function incrementEventCount(){
        let counterElement = document.getElementById("num-observed-events")
        let eventCount = parseInt(counterElement.innerHTML)
        counterElement.innerHTML = eventCount + 1;
    }

    function updateFieldIfNotNull(fieldName, value, precision=10){
        if (value != null)
            document.getElementById(fieldName).innerHTML = value.toFixed(precision);
    }

    
    function sendData () {
//        const d = new Date();
        //        button.innerHTML = d.toLocaleTimeString();
        const data = "(" + oa + " " + ob + " " + og + " " + x + " " + y + " " + z + " " + gx + " " + gy + " " + gz + " " + gyrox + " " + gyroy + " " + gyroz + ")";
//        console.log(data);
        sensor.internalValueChange = true;
        sensor.setAttribute(
            'sensor-data', data );
        sendDataTimer = setTimeout( sendData, interval );
    }

    function handleMotion(event) {
        //        internalValueChange = true;
        gx = normalizeGXY(event.accelerationIncludingGravity.x);
        gy = normalizeGXY(event.accelerationIncludingGravity.y);
        gz = normalizeGZ(event.accelerationIncludingGravity.z);
            x = event.acceleration.x;
            y = event.acceleration.y;
            z = event.acceleration.z;
            gyrox = event.rotationRate.alpha;
            gyroy = event.rotationRate.beta;
            gyroz = event.rotationRate.gamma;
    }

    var is_running = false;
    var sendDataTimer = false;
    
    function doButtonClick(e) {
//        alert("click, is_running: " + is_running);
        e.preventDefault();
        
//        Request permission for iOS 13+ devices
        if (
            DeviceMotionEvent &&
                typeof DeviceMotionEvent.requestPermission === "function"
        ) {
            DeviceMotionEvent.requestPermission();
        }

//        alert("click, is_running: " + is_running);
        
        if (is_running){
//            alert("stop");
            window.removeEventListener("deviceorientation", handleOrientation);
            button.innerHTML = "Start demo";
            button.classList.add('btn-success');
            button.classList.remove('btn-danger');
            clearTimeout(sendDataTimer);
            internalValueChange = true;
            is_running = false;
        }else{
//            alert("start");
            window.addEventListener("devicemotion", handleMotion);
            window.addEventListener("deviceorientation", handleOrientation);
            button.innerHTML = "Stop demo";
            button.classList.remove('btn-success');
            button.classList.add('btn-danger');
            internalValueChange = true;
            sendDataTimer = setTimeout( sendData, interval );
            is_running = true;
        }
    }


    
// initialization

    function initSensors () {
        // console.log('initialising Sensors');
        // sensor.addEventListener('mousedown', mouseDownListener)
        // addEventListener('beforeunload', (event) => {
        //     $(sensor).trigger("data", {close: true})});
        // sensor.externalValueChange = true;

        sensor.set
        
        sensor.internalValueChange = internalValueChange;
        sensor.setInterval = setInterval;
        sensor.setId = setId;
        sensor.interval = interval;
        sensor.id = id;
        sensor.sensorDataVals = sensorDataVals;
        sensor.sendData = sendData;

        sensor.oa = oa;
        sensor.ob = ob;
        sensor.og = og;

        sensor.x = x;
        sensor.y = y;
        sensor.z = z;

        sensor.gx = gx;
        sensor.gy = gy;
        sensor.gz = gz;

        sensor.gyrox = gyrox;
        sensor.gyroy = gyroy;
        sensor.gyroz = gyroz;
        
        sensor.setSensorData = setSensorData;

        sensor.updateFieldIfNotNull = updateFieldIfNotNull;

        sensor.button = button;
        sensor.doButtonClick = doButtonClick;
        
        button.addEventListener("click", doButtonClick);
        button.addEventListener("ontouchstart", doButtonClick);
        sensor.is_running = is_running;
    }

    initSensors();
}

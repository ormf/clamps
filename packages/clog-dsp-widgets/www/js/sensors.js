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

class SensorElement extends HTMLElement {
    static observedAttributes = [ 'interval', 'sensor-data', 'sensor-trigger', 'trigger-active',
                                  'trigger-timeout', 'trigger-threshold' ];

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
        case 'sensor-trigger':
            this.setTrigger(newValue);
            break;
        case 'trigger-active':
            this.setTriggerActive(newValue);
            break;
        case 'trigger-threshold':
            this.setTriggerThreshold(newValue);
            break;
        case 'trigger-timeout':
            this.setTriggerTimeout(newValue);
            break;
        }
    }
}

customElements.define("o-sensor", SensorElement );

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
    if ((value == 'false' || value == 'nil' ))
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
    button.innerHTML = "Stopped";
    button.style.fontSize = "3em";
    button.style.backgroundColor = "#ccc";
    //    sensor.appendChild(button);
    sensor.prepend(button);
    sensor.externalValueChange = true;

    var interval = 50;
    var id = false;
    var internalValueChange = false;

    var sensorDataVals = [1,1,1,1,1,1,1,1,1,1,1,1,1];
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

    var deltag = 0;

    var trigger = 1;
    var triggerPending = false;
    var triggerActive = true;
    var triggerThreshold = 40;
    var triggerTimeout = 100;    

    var triggerTimeoutFn = false;
    
    var oldgx = 0;
    var oldgy = 0;
    var oldgz = 0;

//    alert('delta-g-neu: ' + deltag);

    
    function setInterval(value) {
        interval = value;
    }

    function setTrigger(value) {
        trigger = parseFloat(value);
//        console.log('trigger: ' + trigger);
        if (trigger == 0)
            document.getElementById('trigger').style.background = "transparent";
        else
            document.getElementById('trigger').style.background = "orange";
    }

    function setTriggerActive(value) {
        triggerActive = parseBool(value);
        if (triggerActive)
            document.getElementById('trigger').style.background = "transparent";
        else
            document.getElementById('trigger').style.background = "#aaa";
        document.getElementById('Trigger_active').innerHTML = triggerActive;
    }

    function setTriggerThreshold(value) {
        if (internalValueChange == false) {
            triggerThreshold = parseFloat(value);
//            console.log('threshold: ' + triggerThreshold);
            updateFieldIfNotNull('Trigger_threshold', triggerThreshold);
        }
    }

    function setTriggerTimeout(value) {
        triggerTimeout = parseFloat(value);
 //           console.log('threshold: ' + triggerTimeout);
        updateFieldIfNotNull('Trigger_timeout', triggerTimeout);
    }

    function iOS() {
        return [
            'iPad Simulator',
            'iPhone Simulator',
            'iPod Simulator',
            'iPad',
            'iPhone',
            'iPod'
        ].includes(navigator.platform)
        // iPad on iOS 13 detection
            || (navigator.userAgent.includes("Mac") && "ontouchend" in document)
    }

    
    function setId(value) {
        id = value;
    }

    const myRe0 = /[\n\r]+/g;
    const myRe1 = /\((.+)\)/g;

    var gAccScalar = 1.0;

    if( iOS ) {
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

            deltag = sensorDataVals[12];
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
        updateFieldIfNotNull('Delta_g', deltag);
        if ( internalValueChange ) {
            $(sensor).trigger( "data", { "sensor-data": sensorDataVals } );
            //   [ oa, ob, og, x, y, z, gx, gy, gz, gyrox, gyroy, gyroz deltag ]
            
            if ((deltag > triggerThreshold) && (!triggerPending)) {
                sensor.setAttribute('sensor-trigger', '1');
                $(sensor).trigger( "data", { "sensor-trigger": 1 } );
                triggerPending = true;
                triggerTimeoutFn = setTimeout(function() {
                    sensor.setAttribute('sensor-trigger', '0');
                    $(sensor).trigger( "data", { "sensor-trigger": 0 } );
                    triggerPending = false;
                }, triggerTimeout);
            }
        }
        internalValueChange = false;
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
        const data = "(" + oa + " " + ob + " " + og + " " + x + " " + y + " " + z + " " + gx + " " + gy + " " + gz + " " + gyrox + " " + gyroy + " " + gyroz + " " + deltag + ")";
//        console.log(data);
        internalValueChange = true;
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
            if (triggerActive) {
                deltag = Math.abs(gx - oldgx)
                    + Math.abs(gy - oldgy)
                    + Math.abs(gz - oldgz);
                oldgx = gx;
                oldgy = gy;
                oldgz = gz;
            }
    }

    var is_running = false;
    var sendDataTimer = false;
    
    function doButtonClick(e) {
//        alert("click, is_running: " + is_running);
        e.preventDefault();
        
        //        Request permission for iOS 13+ devices

        



//        alert("click, is_running: " + is_running);
        
        if (is_running){
//            alert("stop");
            window.removeEventListener("deviceorientation", handleOrientation);
            button.innerHTML = "Stopped";
            button.style.backgroundColor = "#ccc";
            button.classList.add('btn-success');
            button.classList.remove('btn-danger');
            clearTimeout(sendDataTimer);
            internalValueChange = true;
            is_running = false;
        }else{
            //            alert("start");
            if ( os == "iPhone") {
                DeviceOrientationEvent.requestPermission().then(response => {
                    if (response === "granted") {
                        window.addEventListener('deviceorientation', handleOrientation);
                        window.addEventListener("devicemotion", handleMotion);
                        console.log("DeviceOrientationEvent permission granted.");
                    } else {
                        console.error("DeviceOrientationEvent permission denied.");
                    };
                });
            } else {
                window.addEventListener("devicemotion", handleMotion);
                window.addEventListener("deviceorientation", handleOrientation);
            }
            button.innerHTML = "Running";
            button.style.backgroundColor = "#cfc";
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
        sensor.setTriggerActive = setTriggerActive;
        sensor.setTriggerThreshold = setTriggerThreshold;
        sensor.setTriggerTimeout = setTriggerTimeout;
        sensor.setTrigger = setTrigger;

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

        
        sensor.triggerActive = triggerActive;
        sensor.triggerThreshold = triggerThreshold;
        sensor.triggerTimeout = triggerTimeout;
        sensor.triggerPending = triggerPending;
        sensor.trigger = trigger;


        
        sensor.button = button;
        sensor.doButtonClick = doButtonClick;
        
        button.addEventListener("click", doButtonClick);
        button.addEventListener("ontouchstart", doButtonClick);
        sensor.is_running = is_running;
    }

    initSensors();
}

var anchorProps;
var anchorPoint;

function changer(props) {
  function move(event) {
    if (event.touches) {
      var targetPoint = {
        x: event.touches[0].clientX,
        y: event.touches[0].clientY,
      }
    } else {
      var targetPoint = {
        x: event.clientX,
        y: event.clientY
      };
    }
      const deltaX = (targetPoint.x - anchorPoint.x);
      const deltaY = (targetPoint.y - anchorPoint.y);
      const newX = anchorProps.x + deltaX;
      const newY = anchorProps.y + deltaY;

    if ('x' in props){
      props.x = newX;
      props.y = newY;
    } else {
      props[0] = newX;
      props[1] = newY;
    }
  }
  return move;
}

export function cancelMouse(move) {
  function cancel(e) {
    window.removeEventListener('mousemove', move);
    window.removeEventListener('mouseup', cancel);
    anchorPoint = undefined;
    anchorProps = undefined;
  }
  return cancel;
}

export function onMouseDownHandler(pt) {
  function f(event) {
      anchorPoint = {
          x: event.clientX,
          y: event.clientY
      };
      anchorProps = {}
      if ('x' in pt){
          anchorProps.x = pt.x;
          anchorProps.y = pt.y;
      } else {
          anchorProps.x = pt[0];
          anchorProps.y = pt[1];
      }
      const fun = changer(pt);
      window.addEventListener('mousemove', fun);
      window.addEventListener('mouseup', cancelMouse(fun));
  }
  return f;
}

function cancelTouch(move) {
  function cancel(e) {
    window.removeEventListener('touchmove', move);
    window.removeEventListener('touchend', cancel);
    window.removeEventListener('touchcancel', cancel);
    anchorPoint = undefined;
    anchorProps = undefined;
  }
  return cancel;
}

export function onTouchStartHandler(pt) {
  function f(event) {
      anchorPoint = [event.clientX, event.clientY];
      anchorProps = pt;
      const fun = changer(pt);
      window.addEventListener('touchmove', fun);
      window.addEventListener('touchend', cancelTouch(fun));
      window.addEventListener('touchcancel', cancelTouch(fun));
  }
  return f;
}

package;

// a comment

import hx.sys;

/* another
 * comment
 */

@:allow(MainTest)
class Main {
  var single:String = 'single';
  var double:String = "double";
  var regexp:EReg   = ~/world/;

  function main():Void {
    #if debug
    trace("hello, world");
    #end

    var map = [1 => 101, 2 => 102, 3 => 103];
    for (key => value in map) {
      trace(key, value);
    }
  }
}

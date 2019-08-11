// a comment

package;

/* another
 * comment
 */

@:allow(MainTest)
class Main {

  var name:String;

  function main():Void {
    name = "carl" + 'olson';

    var regexp:EReg = ~/world/;

    #if debug
    trace("hello");
    #end

    var map = [1 => 101, 2 => 102, 3 => 103];
    for (key => value in map) {
      trace(key, value);
    }
  }

}

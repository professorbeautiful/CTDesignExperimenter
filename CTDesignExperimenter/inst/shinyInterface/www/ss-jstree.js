//input binding for jstree

//alert("In my ss-jstree.js");
var ss_jstree = new Shiny.InputBinding();
var previousHTML;
var newHTML;
var message;
var theEL;
var tree=function() {return($("#jstreeScenario"));}
var Trefresh=function() { return(tree().jstree('refresh'));} ;

$.extend(ss_jstree, {

  receiveMessage: function(el, inputMessage) {
    theEl = el;
  
    message = inputMessage;
    // clean up the message
    message = message.replace(/..."is.logical..."/g, "is.logical");
    message = message.replace(/..."is.numeric..."/g, "is.numeric");
    message = message.replace(/\\"/g, '"');
    message = message.replace(/'/g, "");
    message = message.replace(/^ *\[1\] *"/, "");
    message = message.replace(/.$/, "");
    console.log("message is " + message);
    jsonMessage = eval(message);
    theJsonMessage = jsonMessage;

    treeData = theJsonMessage;
//    $('#newStuff').jstree('refresh');
    console.log(jsonMessage);
    $("#jstreeScenario").jstree('refresh');
    console.log("jstreeScenario is refreshed");
    $("li.treeclass_1 > ").css("color", "blue");
    $("li.treeclass_2 > ").css("color", "darkred");
    $("li.treeclass_3 > ").css("color", "green");
    $("li.treeclass_1  ").css("color", "blue");
    $("li.treeclass_2  ").css("color", "darkred");
    $("li.treeclass_3  ").css("color", "green");
    
  },
    /**/

  find: function(scope) {
    return $(scope).find(".ss-jstree");
  },
  
  getValue: function(el) {
	  var tree = $(el).jstree();
  	var leaves = tree.get_selected(); // an array of node id's, like "j1_5".
  	var i, j, r = [];
  	var mynode;
  	var pathlength;
  	var mynode_data = [];
	  var mynode_li_attr;

	  // R-parse-able output string;
    for (i = 0, j = leaves.length; i < j; i++) {
      mynode = tree.get_node(leaves[i]);
      mynode_li_attr = mynode.li_attr;
    	mynode_li_attr["id"] = mynode.id;
      mynode_li_attr["text"] = mynode.text;
    	r.push([mynode_li_attr]);
    }
    return r;
  },

  setValue: function(el, value) {
    alert("jsTree setValue: " + value);
  },
  subscribe: function(el, callback) {
    $(el).on("changed.jstree", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".ss_jstree");
  }
});

Shiny.inputBindings.register(ss_jstree);

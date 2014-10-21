//input binding for jstree

//alert("In my ss-jstree.js");
var ss_jstree = new Shiny.InputBinding();
var previousHTML;
var newHTML;
var message;

$.extend(ss_jstree, {

  receiveMessage: function(el, inputMessage) {
    message = inputMessage;

/**
    //  Show the inserts in the message, as JS sees them.  Yes, deleted inserts not here.
    //start = message[4].children[0].children[0];    // Valid if the ENTIRE jstree() is sent.
    //start = message[0].children[0];    // Valid if only the html is sent.
    start = message.children[0];    // Valid if only the html is sent.

    $.each(start,  function(){
        console.log(  $(this)[0].children[0][0] ); 
        if($(this)[0].children[0].length == 2) { 
          $.each($(this)[0].children[0][1].children[0], 
          function(){ console.log("     " + $(this)[0].children[0][0]); } ) }} ) ;      
    **/
    /**/
    $('#jstreeScenario').on("changed.jstree", function (e, data) {
      console.log("tree changes, length of data is " + data.length);
    });
    //console.log("el is " + el);
    // we presume for now that el is "#scenarioTree"
    if(message.length > 0) {
      console.log("jstree receiving message: DOING IT. length of message is " + message.length);
      previousHTML = $("#jstreeScenario").html();
      console.log("length of previousHTML is " + $("#jstreeScenario").html().length
          + " Changing the HTML next line.");
      // $(el).jstree()._parse_model_from_html(message); //private; no can do.
      //$("#jstreeScenario").jstree(true).html(data);  // error?
      // testmessage = '<ul><li><a href="#">Node 1</a>   <ul>   <li><a href="#">Node 1.1</a></li><li><a href="#">Node 1.2</a><ul><li><a href="#">Node 1.2.1</a></li> </ul></li></ul> </li><li><a href="#">Node 2</a></li></ul>';

      // $("#jstreeScenario").replaceWith(message);   // where message contains the whole jstree div:

    $("#jstreeScenario").jstree(true).destroy();
    $("#jstreeScenario").html(message);
    $("#jstreeScenario").jstree().addClass("shiny-bound-input").addClass("ss-jstree").removeClass("ss-");

      
      // if message contains only the html tree content:
      //$("#jstreeScenario").jstree({"html_data": {"data": [  message  ] }});
      //$("#jstreeScenario").jstree({"core": {"data": message   }});
      //$("#jstreeScenario").jstree({"core": {"html_data": {"data": message   }}});
      //$("#jstreeScenario").jstree("destroy").jstree({"core": {"html_data": {"data": message   }}});
      
//      console.log("Number of inserts in message is " + message[4].children[0].children[0].length);
//      console.log("Number of inserts in inputMessage is " + inputMessage[4].children[0].children[0].length);
      
      newHTML = $("#jstreeScenario").html();
      console.log("newHTML length is " + $("#jstreeScenario").html().length);
      // This show the CORRECT html.  But it doesn't show up.
        console.log("Before jstree trigger change, length is " + $("#jstreeScenario").html().length);        
        $("#jstreeScenario").jstree().trigger('change');
        console.log("After jstree trigger change, length is " + $("#jstreeScenario").html().length);        
    //  In R:   length(rValues$currentScenario@inserts)  # has been reduced
//  alert("Skipping jstree refresh");        
//        $("#jstreeScenario").jstree('refresh');
    }
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

  setValue: function(el, value) {},
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

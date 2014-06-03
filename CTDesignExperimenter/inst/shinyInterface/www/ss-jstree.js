//input binding for jstree

alert("In my ss-jstree.js");

//Shiny.inputBindings.unregister(ss_jstree);

//var ss_jstree = new Shiny.InputBinding();

// syntax ok here, but never done.
$("#jstree1").bind(
        "select_node.jstree", function(evt, data){
            //selected node object: data.inst.get_json()[0];
            //selected node text: data.inst.get_json()[0].data
		alert("select_node.jstree");
        }
);

// syntax ok here, but never done.
$.bind("select_node.jstree", function (event, data) {  
    //`data.rslt.obj` is the jquery extended node that was clicked          
    alert("Selected node = "+ data.rslt.obj.attr("id"));
    alert("Parent of Selected node = "+ data.inst._get_parent(data.rslt.obj).attr("id"))
 });

//.on('changed.jstree', function (e, data) {
  //var p = [], i, j;
   // alert("ID: " + data.instance.get_path(data.selected[i], '/', true)); 
  //for(i = 0, j = data.selected.length; i < j; i++) {
   // p.push(data.instance.get_path(data.selected[i], '/', true)); 
	// take a look at get_path here: 
	//http://www.jstree.com/api/#/?q=get_path&f=get_path%28obj%20[,%20glue,%20ids]%29
  //}
  // send p to the server with an AJAX call however you need (implode, JSON.stringify, etc)
//});

$.extend(ss_jstree, {
// This bind paragraph has not worked at all.
// bind: "select_node",function(e,data) {
//	var inst=data.inst;
//	var level=inst.get_path().length;
//	var selected=inst.get_selected();
//    	var id=selected.attr('id');
//      var name=selected.prop('tagName');
//      var theclass=selected.prop('class');
//	return theclass;
//	return ['SELECTED'];
// },
  find: function(scope) {
    return $(scope).find(".ss-jstree");
  },
  getValue: function(el) {
  //This alert is called.
   //alert("getValue");
	var tree = $(el).jstree();
	var leaves = tree.get_selected();
	var i, j, r = [];
	var answerstring = "";
	var mynode;
	var pathlength;

   //This next alert happens.
    //if(leaves.length > 0) alert("HERE! #leaves = " + leaves.length); 

// TRY THIS SOME TIME: document.getElementById("demo").innerHTML = fruits;

    for (i = 0, j = leaves.length; i < j; i++) {
      mynode = tree.get_node(leaves[i]);
      r.push(mynode.text);

      //BAD JS?  answerstring = answerstring + " _" + mynode.attr('class') + "_: " + mynode.text;
      // BUSTS LOOP: answerstring = answerstring + " _" + mynode.rslt.attr('class') + "_: " + mynode.text;
      //answerstring = answerstring + " _" + mynode.rslt.obj.attr('class') + "_: " + mynode.text;
      //Next line is OK
	 answerstring = answerstring +  // wrapping is OK.
			 " _" + i + "_: " + mynode.text;
//      r.push(mynode.attr('id'));
//      r.push(mynode.rslt.obj.attr('class'));
// All j alerts happen, if nothing else goes wrong.


// THIS BUSTS THE LOOP. pathlength = mynode.get_path().length;

     //alert("INSIDE LOOP class " + answerstring);

	//r.push("L=" + pathlength + ". ");
// Adding this causes the alert to fail!  + tree.get_node(leaves[i]).attr('class'));
//      r.push(tree.get_node(leaves[i]).get_json()[0].class);
    }
    //alert("Result: (answerstring)" + answerstring + "///");
    //alert("Result: (r)" + r + "///");
// OK, works for multiple selects.
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

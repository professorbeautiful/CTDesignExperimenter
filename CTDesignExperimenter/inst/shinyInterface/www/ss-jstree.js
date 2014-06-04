//input binding for jstree

//alert("In my ss-jstree.js");

$.extend(ss_jstree, {
  find: function(scope) {
    return $(scope).find(".ss-jstree");
  },
  getValue: function(el) {
  //This alert is called if all is well.
   //alert("getValue");
   // $("#jstree1").jstree().get_selected()[0].parents.length;
   // tree.get_node($("#jstree1").jstree().get_selected()[0]).parents.length;
	var tree = $(el).jstree();
	var leaves = tree.get_selected();
	var i, j, r = [];
	var answerstring = "";
	var mynode;
	var pathlength;

    for (i = 0, j = leaves.length; i < j; i++) {
      mynode = tree.get_node(leaves[i]);
      pathlength = mynode.parents.length;
      r.push(pathlength + "_:_" + mynode.text);

//      r.push(mynode.attr('id'));
//      r.push(mynode.rslt.obj.attr('class'));
// All j alerts happen, if nothing else goes wrong.


// THIS BUSTS THE LOOP. pathlength = mynode.get_path().length;

     //alert("INSIDE LOOP i= " + i);

	//r.push("L=" + pathlength + ". ");
// Adding this causes the alert to fail!  + tree.get_node(leaves[i]).attr('class'));
//      r.push(tree.get_node(leaves[i]).get_json()[0].class);
    }
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

(function(){
    var tree, currentIconMode;
    
    function changeIconMode(){
        var newVal = parseInt(this.value);
        if (newVal != currentIconMode) {
            currentIconMode = newVal;
        }
        buildTree();
    }
    
    function loadNodeData(node, fnLoadComplete){
        var nodeLabel = encodeURI(node.label);
        var sUrl = "/erlflow/nets";
        var callback = {
            success: function(oResponse){
                YAHOO.log("XHR transaction was successful.", "info", "example");
                YAHOO.log(oResponse.responseText);
                var oResults = YAHOO.lang.JSON.parse(oResponse.responseText);
                
                for (var i = 0, j = oResults.networks.length; i < j; i++) {
                    var obj = new Object();
                    obj.id = oResults.networks[i].id;
                    obj.label = oResults.networks[i].name;
                    var tempNode = new YAHOO.widget.TextNode(obj, node, false);
                    tempNode.isLeaf = true;
                }
                
                oResponse.argument.fnLoadComplete();
            },
            
            failure: function(oResponse){
                YAHOO.log("Failed to process XHR transaction.", "info", "example");
                oResponse.argument.fnLoadComplete();
            },
            argument: {
                "node": node,
                "fnLoadComplete": fnLoadComplete
            },
            timeout: 7000
        };
        
        YAHOO.util.Connect.asyncRequest('GET', sUrl, callback);
    }
    
    function buildTree(){
        tree = new YAHOO.widget.TreeView("processesDiv");
        tree.setDynamicLoad(loadNodeData, currentIconMode);
        var root = tree.getRoot();
        
        var tempNode = new YAHOO.widget.TextNode("Procesos", root, false);
        tempNode
        tree.subscribe("labelClick", function(node){
            if (node.data.id) 
                erlflow.app.new_case_dialog.case_id = node.data.id;
        });
        tree.draw();
    }
    
    
    return buildTree();
    
})();

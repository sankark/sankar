(function(){
    var handleSubmit = function(){
        this.hide();
    };
    
    var handleCancel = function(){
        this.cancel();
    };
    
    var handleSuccess = function(o){
        var response = o.responseText;
        response = response.split("<!")[0];
        document.getElementById("resp").innerHTML = response;
    };
    var handleFailure = function(o){
        alert("Submission failed: " + o.status);
    };
    
    // Instantiate the Dialog
    var new_case = new YAHOO.widget.Dialog("new_case", {
        width: "30em",
        fixedcenter: true,
        visible: false,
        constraintoviewport: true,
        buttons: [{
            text: "Submit",
            handler: handleSubmit,
            isDefault: true,
            disabled: true
        }, {
            text: "Cancel",
            handler: handleCancel
        }]
    });
    
    // Wire up the success and failure handlers
    var callback = {
        success: handleSuccess,
        failure: handleFailure
    };
    
    // Render the Dialog
    new_case.render();
    
    YAHOO.util.Get.script('/assets/js/ef/nets_tree.js');
})();


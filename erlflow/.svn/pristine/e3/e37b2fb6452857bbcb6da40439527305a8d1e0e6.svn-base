(function(){
    YAHOO.log('case_editor.js file loaded', 'info', 'case_editor.js');
    YAHOO.log('Inject some HTML for the Compose Window', 'info', 'case_editor.js');
    //YAHOO.util.Dom.get('composeViewEl').innerHTML = '<div id="top2"><div id="inboxToolbar"></div><div id="standard"></div></div><div id="center2"><div class="yui-layout-bd"><div id="preview"><p><strong></strong></p><p></p></div></div></div>';
    YAHOO.util.Dom.get('composeViewEl').innerHTML = '<div id="composeBarWrap"><div id="composeBar"></div></div><div id="caseEditorView" width="100%" height="100%"" align="center" valign="middle"></div>';
    //Use loader to load the Editor
    var loader = new YAHOO.util.YUILoader({
        base: '../../build/',
        require: ['autocomplete', 'editor'],
        ignore: ['containercore'],
        onSuccess: function(){
            YAHOO.log('Create a Toolbar above the To/From Fields', 'info', 'case_editor.js');
            erlflow.app.composeToolbar = new YAHOO.widget.Toolbar('composeBar', {
                buttons: [{
                    id: 'tb_delete',
                    type: 'push',
                    label: 'Enviar',
                    value: 'send'
                }, {
                    id: 'tb_forward',
                    type: 'push',
                    label: 'Cancelar',
                    value: 'cancel'
                }]
            });
            //Show an alert message with the button they clicked            
            erlflow.app.composeToolbar.on('buttonClick', function(ev){
                erlflow.app.alert('You clicked: ' + ev.button.label);
            });
            
            erlflow.app.destroyEditor = function(){
                YAHOO.log('Destroying the Editor instance and HTML', 'info', 'editor.js');
                erlflow.app.editor = null;
            };
			
			var callback2 = {
                success: function(oResponse){
                    YAHOO.log("XHR transaction was successful.", "info", "example");
                    YAHOO.log(oResponse.responseText);

                    var oResults = YAHOO.lang.JSON.parse(oResponse.responseText);
					
					var innerStr = "<table><tr>";
					for (var field in oResults.fields) {
						innerStr += "<td>";
						innerStr += "<label for='" + oResults.fields[field].id + "'>" + oResults.fields[field].name +":</label>";
						innerStr += "</td>";
						innerStr += "<td>";
						innerStr += "<input type='textbox' name='" + oResults.fields[field].id + "' />";
						innerStr += "</td>";
					};
					
					erlflow.app.caseEditorView = new YAHOO.widget.Module("caseEditorView");
            		erlflow.app.caseEditorView.setBody(innerStr);
            		erlflow.app.caseEditorView.render();
                },
                
                failure: function(oResponse){
                    YAHOO.log("Failed to process XHR transaction.", "info", "example");
                },
                timeout: 7000
            };
			
            var callback = {
                success: function(oResponse){
                    YAHOO.log("XHR transaction was successful.", "info", "example");
                    YAHOO.log(oResponse.responseText);
                    var oResults = YAHOO.lang.JSON.parse(oResponse.responseText);
					
					var sUrl = "/erlflow/activity/" + oResults.places[0].id;
					YAHOO.util.Connect.asyncRequest('GET', sUrl, callback2);
                },
                
                failure: function(oResponse){
                    YAHOO.log("Failed to process XHR transaction.", "info", "example");
                },
                timeout: 7000
            };
			
			var sUrl = "/erlflow/net/" + erlflow.app.new_case.case_id;
            YAHOO.util.Connect.asyncRequest('GET', sUrl, callback);
        }
    });
    //Have loader only insert the js files..
    loader.insert({}, 'js');
})();

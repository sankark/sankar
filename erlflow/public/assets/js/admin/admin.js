(function(){
    erlflow = new Object();
    erlflow.admin = new Object();
    
    YAHOO.util.Event.onDOMReady(function(){
        MainMenu();
        WelcomePanel();
        //LogviewerPanel();
        init_UploadProcessDialog();
    });
    
    function MainMenu(){
        function NewProcess_onClick(){
            YAHOO.log('NewProcess_onClick called.');
            ProcessesPanel();
        };
        
        function UploadProcess_onClick(){
            YAHOO.log('UploadProcess_onClick called.');
            erlflow.admin.dialog1.show();
        };
		function EditParticipants_onClick(){
            YAHOO.log('EditParticipants_onClick called.');
            ParticipantsPanel();
        };
        function LogViewer_onClick(){
            YAHOO.log('UploadProcess_onClick called.');
            LogviewerPanel();
        };
        var aItemData = [{
            text: "<em id=\"yahoolabel\">ErlFlow!</em>",
            submenu: {
                id: "yahoo",
                itemdata: ["About ErlFlow!", "Preferences"]
            }
        
        }, {
            text: "Archivo",
            submenu: {
                id: "filemenu",
                itemdata: [{
                    text: "Ver procesos...",
                    helptext: "Ctrl + P",
                    onclick: {
                        fn: NewProcess_onClick
                    }
                }, {
                    text: "Subir procesos...",
                    helptext: "Ctrl + E",
                    onclick: {
                        fn: UploadProcess_onClick
                    }
                }, {
                    text: "Editar participantes...",
                    helptext: "Ctrl + U",
					onclick: {
                        fn: EditParticipants_onClick
                    }
                }]
            }
        
        }, {
            text: "Ver",
            submenu: {
                id: "viewmenu",
                itemdata: [[{
                    text: "Procesos",
                    helptext: "Ctrl + V"
                }, {
                    id: "logviewer",
                    text: "Registro de sucesos",
                    helptext: "Ctrl + Y",
                    fn: LogViewer_onClick
                }], [{
                    text: "Rendimiento",
                    helptext: "Ctrl + X",
                }]]
            }
        
        }, {
            text: "Herramientas",
            submenu: {
                id: "toolsmenu",
                itemdata: [[{
                    text: "Descargar",
                    helptext: "Ctrl + Z",
                    submenu: {
                        id: "descargar",
                        itemdata: ["Editor de procesos", "Editor de reportes"]
                    }
                }]]
            }
        
        }, "Ay&uacute;da"];
        
        var oMenuBar = new YAHOO.widget.MenuBar("mymenubar", {
            lazyload: true,
            itemdata: aItemData
        });
        YAHOO.log('Rendering main menu.', 'info', 'admin.js');
        oMenuBar.render(document.body);
    };
    
    function WelcomePanel(){
        var oPanel = new YAHOO.widget.Panel("welcomeinfo", {
            constraintoviewport: true,
            fixedcenter: true,
            width: "400px",
            zIndex: 1
        });
        
        oPanel.setHeader("C&oacute;nsola de Administraci&oacute;n de ErlFlow");
        oPanel.setBody("Bienvenido a la c&oacute;nsola de administraci&oacute;n de ErlFlow.<br>Aqu&iacute; Ud. podr&aacute; configurar los par&aacute;metros de ejecuci&oacute;n del sistema. Si no est&aacute; seguro de lo que est&aacute; haciendo, por favor contacte al Administrador del Sistema.");
        YAHOO.log('Rendering welcome panel.', 'info', 'admin.js');
        oPanel.render(document.body);
    }
    
    function LogviewerPanel(){
        var oPanel = new YAHOO.widget.Panel("logviewerpanel", {
            constraintoviewport: true,
            fixedcenter: false,
            width: "350px",
            zIndex: 1,
            xy: [850, 40]
        });
        
        oPanel.setHeader("Visor de sucesos.");
        oPanel.setBody("<div id='logviewer' align='left'></div>");
        YAHOO.log('Rendering logviewer panel.', 'info', 'admin.js');
        oPanel.render(document.body);
        var loader = new YAHOO.util.YUILoader();
        loader.insert({
            require: ['fonts', 'dragdrop', 'logger'],
            base: '/build/',
            
            onSuccess: function(loader){
                // Put a LogReader on your page
                this.myLogReader = new YAHOO.widget.LogReader("logviewer", {
                    logReaderEnabled: true,
                    newestOnTop: true
                });
            }
        });
    }
    
    function ProcessesPanel(){
        var oPanel = new YAHOO.widget.Panel("processselect", {
            constraintoviewport: true,
            fixedcenter: false,
            width: "400px",
            zIndex: 1,
            xy: [20, 40]
        });
        
        oPanel.setHeader("Procesos configurados en Erlflow.");
        oPanel.setBody("<div id='processesDiv'></div>");
        YAHOO.log('Rendering processes panel.', 'info', 'admin.js');
        oPanel.render(document.body);
        YAHOO.util.Get.script('/assets/js/ef/nets_table.js');
        
    }
	
	function ParticipantsPanel(netID){
        var oPanel = new YAHOO.widget.Panel("participantsList", {
            constraintoviewport: true,
            fixedcenter: false,
            width: "400px",
            zIndex: 1,
            xy: [20, 140]
        });
        
        oPanel.setHeader("Participantes en Definici—n de Procesos.");
        oPanel.setBody("<div id='participantsDiv'></div>");
        YAHOO.log('Rendering participants panel.', 'info', 'admin.js');
        oPanel.render(document.body);
        participants_table("minamb_wp1"); //TODO: Quitar hard-code
        
    }
    
    function init_UploadProcessDialog(){
        var handleSubmit = function(){
            var uploadHandler = {
                upload: function(o){
                    YAHOO.log(o.responseText);
                    YAHOO.util.Dom.setStyle('indicator', 'visibility', 'hidden');
                    var r = eval('(' + o.responseText + ')');
                    if (r.hasError) {
                        var errorString = '';
                        for (var i = 0; i < r.errors.length; i++) {
                            errorString += r.errors[i];
                        }
                        alert(errorString);
                    }
                    else {
                        this.hide();
                    }
                }
            };
            YAHOO.util.Dom.setStyle('indicator', 'visibility', 'visible');
            
            YAHOO.util.Connect.setForm('uploadForm', true);
            YAHOO.util.Connect.asyncRequest('POST', '/upload.yaws', uploadHandler);
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
        erlflow.admin.dialog1 = new YAHOO.widget.Dialog("dialog1", {
            width: "30em",
            fixedcenter: true,
            visible: false,
            constraintoviewport: true,
            buttons: [{
                text: "Submit",
                handler: handleSubmit,
                isDefault: true
            }, {
                text: "Cancel",
                handler: handleCancel
            }]
        });
        
        // Wire up the success and failure handlers
        erlflow.admin.dialog1.callback = {
            success: handleSuccess,
            failure: handleFailure
        };
        
        // Render the Dialog
        erlflow.admin.dialog1.render();
        
        YAHOO.util.Event.addListener("show", "click", erlflow.admin.dialog1.show, erlflow.admin.dialog1, true);
        YAHOO.util.Event.addListener("hide", "click", erlflow.admin.dialog1.hide, erlflow.admin.dialog1, true);
        
    }
	
	function participants_table(netID){
        function onEventSelectRow(){
            //process_info(this.getRecord(this.getSelectedRows()[0]));
        };
        
        var myColumnDefs = [{
            key: "id",
            label: "Id",
            sortable: true,
            label: "Id",
            sortable: true
        }, {
            key: "name"
        }];
        YAHOO.log('Setting datasource.', 'info', 'admin.js');
        this.myDataSource = new YAHOO.util.DataSource("/erlflow/net/" + netID);
        this.myDataSource.responseType = YAHOO.util.DataSource.TYPE_JSON;
        this.myDataSource.connXhrMode = "queueRequests";
        this.myDataSource.responseSchema = {
            resultsList: "participants",
            fields: ["id", "name"]
        };
        
        this.myDataTable = new YAHOO.widget.DataTable("participantsDiv", myColumnDefs, this.myDataSource);
        
        var callback1 = {
            success: this.myDataTable.onDataReturnAppendRows,
            failure: this.myDataTable.onDataReturnAppendRows,
            scope: this.myDataTable
        };
        
        this.myDataTable.subscribe("rowClickEvent", this.myDataTable.onEventSelectRow);
        this.myDataTable.subscribe("rowClickEvent", onEventSelectRow);
        YAHOO.log('Sending request.', 'info', 'admin.js');
        this.myDataSource.sendRequest("", callback1);
    }
})();

(function(){
    erlflow = new Object();
    erlflow.app = {
        inboxLoaded: false,
        inboxLoading: false,
        feedURL: '/erlflow/nets',
        getFeed: function(u){
            if (!erlflow.app.inboxLoading) {
                var reload = true;
                erlflow.app.inboxLoading = true;
                if (u) {
                    if (erlflow.app.feedURL === (u + '?count=50')) {
                        reload = false;
                    }
                    erlflow.app.feedURL = u + '?count=50';
                }
                YAHOO.util.Dom.addClass(erlflow.app.tabView._tabParent, 'loading');
                if (!erlflow.app.inboxLoaded) {
                    var transactionObj = YAHOO.util.Get.script('/assets/js/inbox.js', {
                        autopurge: true
                    });
                }
                else {
                    if (reload) {
                        erlflow.app.reloadData(u);
                    }
                    else {
                        YAHOO.util.Dom.removeClass(erlflow.app.tabView._tabParent, 'loading');
                        erlflow.app.inboxLoading = false;
                    }
                }
            }
        }
    };
    
    //Call loader the first time
    var loader = new YAHOO.util.YUILoader({
        base: 'build/',
        //Get these modules
        require: ['reset-fonts-grids', 'utilities', 'logger', 'button', 'container', 'tabview', 'selector', 'resize', 'layout'],
        rollup: true,
        onSuccess: function(){
            //Load the global CSS file.
            YAHOO.log('Main files loaded..', 'info', 'main.js');
            YAHOO.util.Get.css('/assets/css/example1.css');
            
            YAHOO.log('Create the first layout on the page', 'info', 'main.js');
            erlflow.app.layout = new YAHOO.widget.Layout({
                minWidth: 1000,
                units: [{
                    position: 'top',
                    height: 45,
                    resize: false,
                    body: 'top1'
                }, {
                    position: 'right',
                    width: 300,
                    body: '',
                    header: 'Logger Console',
                    collapse: true
                }, {
                    position: 'left',
                    width: 190,
                    resize: true,
                    body: 'left1',
                    gutter: '0 5 0 5px',
                    minWidth: 190
                }, {
                    position: 'center',
                    gutter: '0 5px 0 2'
                }]
            });
            //On resize, resize the left and right column content
            erlflow.app.layout.on('resize', function(){
                var l = this.getUnitByPosition('left');
                var th = l.get('height') - YAHOO.util.Dom.get('folder_top').offsetHeight;
                var h = th - 4; //Borders around the 2 areas
                h = h - 9; //Padding between the 2 parts
                YAHOO.util.Dom.setStyle('folder_list', 'height', h + 'px');
            }, erlflow.app.layout, true);
            //On render, load tabview.js and button.js
            erlflow.app.layout.on('render', function(){
                window.setTimeout(function(){
                    YAHOO.util.Get.script('assets/js/logger.js');
                    YAHOO.util.Get.script('assets/js/tabview.js');
                    YAHOO.util.Get.script('assets/js/buttons.js');
                    YAHOO.util.Get.script('assets/js/calendar.js');
                }, 0);
                
                erlflow.app.layout.getUnitByPosition('right').set('animate', false);
                erlflow.app.layout.getUnitByPosition('right').collapse();
                YAHOO.util.Dom.setStyle(document.body, 'visibility', 'visible');
                setTimeout(function(){
                    erlflow.app.layout.resize();
                    erlflow.app.layout.getUnitByPosition('right').set('animate', true);
                }, 1000);
            });
            //Render the layout
            erlflow.app.layout.render();
            //Setup the click listeners on the folder list
            YAHOO.util.Event.on('folder_list', 'click', function(ev){
                var tar = YAHOO.util.Event.getTarget(ev);
                if (tar.tagName.toLowerCase() != 'a') {
                    tar = null;
                }
                //Make sure we are a link in the list's 
                if (tar && YAHOO.util.Selector.test(tar, '#folder_list ul li a')) {
                    //if the href is a '#' then select the proper tab and change it's label
                    if (tar && tar.getAttribute('href', 2) == '#') {
                        YAHOO.util.Dom.removeClass(YAHOO.util.Selector.query('#folder_list li'), 'selected');
                        var feedName = tar.parentNode.className;
                        YAHOO.util.Dom.addClass(tar.parentNode, 'selected');
                        YAHOO.util.Event.stopEvent(ev);
                        var title = tar.innerHTML;
                        var t = erlflow.app.tabView.get('tabs');
                        for (var i = 0; i < t.length; i++) {
                            if (t[i].get('id') == 'inboxView') {
                                t[i].set('label', title);
                                var u = false;
                                if (feedName.indexOf('-') != -1) {
                                    u = 'http:/' + '/rss.groups.yahoo.com/group/' + feedName + '/rss';
                                }
                                if (feedName.indexOf('inbox') != -1) {
                                    u = 'http:/' + '/rss.groups.yahoo.com/group/ydn-javascript/rss';
                                }
                                erlflow.app.getFeed(u);
                                erlflow.app.tabView.set('activeTab', t[i]);
                            }
                        }
                    }
                }
            });
            
            init_LoginDialog();
            erlflow.app.login_dialog.bringToTop();
            erlflow.app.login_dialog.show();
            
            function fnCallback(e){
                //YAHOO.util.Cookie.remove("erlflow");
                //init_LoginDialog();
                erlflow.app.login_dialog.bringToTop();
                erlflow.app.login_dialog.show();
            }
            
            YAHOO.util.Event.addListener("close_session", "click", fnCallback);
        }
    });
    loader.insert();
    
    var init_LoginDialog = function(){
        var handleSubmit = function(){
        
            var handleSuccess2 = function(o){
            
                YAHOO.log("The success handler was called.  tId: " + o.tId + ".", "info", "example");
                
                if (o.responseText == "login accepted") {
                    var username_caption = YAHOO.util.Dom.get("username_caption");
                    username_caption.innerHTML = o.argument.username;
                    erlflow.app.login_dialog.hide();
                    //alert(o.responseText);
                }
                else {
                    YAHOO.util.Cookie.remove("erlflow");
					erlflow.app.login_dialog.bringToTop();
               		erlflow.app.login_dialog.show();
                }
            }
            
            var handleFailure2 = function(o){
            
                YAHOO.log("The failure handler was called.  tId: " + o.tId + ".", "info", "example");
                
                if (o.responseText !== undefined) {
                    YAHOO.util.Cookie.remove("erlflow");
					erlflow.app.login_dialog.bringToTop();
                	erlflow.app.login_dialog.show();
                }
            }
            
            var callback2 = {
                success: handleSuccess2,
                failure: handleFailure2,
                argument: {
                    username: this.form.username.value
                }
            };
            
            
            var sUrl = "/erlflow/user/" + this.form.username.value + "?password=" + this.form.password.value;
            var request = YAHOO.util.Connect.asyncRequest('GET', sUrl, callback2);
            
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
        erlflow.app.login_dialog = new YAHOO.widget.Dialog("login_dialog", {
            width: "30em",
            fixedcenter: true,
            visible: false,
            modal: true,
            constraintoviewport: true,
            buttons: [{
                text: "Submit",
                handler: handleSubmit,
                isDefault: true
            }]
        });
        
        // Wire up the success and failure handlers
        erlflow.app.login_dialog.callback = {
            success: handleSuccess,
            failure: handleFailure
        };
        
        YAHOO.util.Cookie.remove("erlflow");
        
        // Render the Dialog
        erlflow.app.login_dialog.render();
        
        YAHOO.util.Event.addListener("show", "click", erlflow.app.login_dialog.show, erlflow.app.login_dialog, true);
        YAHOO.util.Event.addListener("hide", "click", erlflow.app.login_dialog.hide, erlflow.app.login_dialog, true);
        
    }
})();

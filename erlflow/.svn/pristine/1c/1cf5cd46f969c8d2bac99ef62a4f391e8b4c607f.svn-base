(function() {
    var Dom = YAHOO.util.Dom,
        Event = YAHOO.util.Event;
    
    YAHOO.log('calendar.js file loaded..', 'info', 'calendar.js');
    //Create this loader instance and ask for the Calendar module
    var loader = new YAHOO.util.YUILoader({
        base: '../../build/',
        require: ['calendar'],
        onSuccess: function() {
            //Set a flag to show if the calendar is open or not
            erlflow.app.calOpen = false;
            YAHOO.log('Create the new calendar', 'info', 'calendar.js');
            erlflow.app.calendar = new YAHOO.widget.Calendar('cal');
            erlflow.app.calendar.selectEvent.subscribe(function(ev, args) {
                var d = args[0][0];
                erlflow.app.alert('You selected this date: ' + d[1] + '/' + d[2] + '/' + d[0]);
            });
            
            erlflow.app.calendar.render();
            
            //Method to toggle the animation of the calendar on and off
            erlflow.app.toggleCal = function() {
                YAHOO.log('Toggle the calendar popup window', 'info', 'calendar.js');
                //set the initial height to the offsetHeight of the calendar element
                var attr = {
                        height: {
                            to: Dom.get('cal').offsetHeight
                        }
                    };
                //If it's open, set the height to 0
                if (erlflow.app.calOpen) {
                    attr.height.to = 0;
                }
                //setup the animation instance
                var anim = new YAHOO.util.Anim('calContainer', attr);
                anim.animate();
                //Toggle the flag
                erlflow.app.calOpen = !erlflow.app.calOpen;
            };
            //Handle the click event on the cal box at the bottom
            Event.on('calBox', 'click', function(ev) {
                Event.stopEvent(ev);
                erlflow.app.toggleCal();
            });
            YAHOO.log('Hijack the calendar link and make it toggle the calendar', 'info', 'calendar.js');
            var c = YAHOO.util.Selector.query('#folder_list li.calendar a')[0];
            if (c) {
                Event.on(c, 'click', function(ev) {
                    Event.stopEvent(ev);
                    erlflow.app.toggleCal();
                });
            }
        }
    });
    //Call insert, only choosing the JS files, so the skin doesn't over write my custom css
    loader.insert({}, 'js');
})();

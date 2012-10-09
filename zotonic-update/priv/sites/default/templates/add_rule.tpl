 {% with edit_rule as r %}       
 {% wire id="add_rule" type="submit" delegate="resource_rules" postback="add_rule"%}
<form id="add_rule" method="post" action="postback" class="row">
	   <div class="row">
    <div class="control-group span3">
	<label class="control-label" for="name_first">{_ Rule Name _}</label>
        <div class="controls">
	    <input id="rule_name" type="text" name="rule_name" value="{{ r.rule_name }}" style="width: 100%" />
        </div>
    </div>

    <div class="control-group span4">
	<label class="control-label" for="name_middle">{_ Pattern _}</label>
        <div class="controls">
	    <input id="pattern" type="text" name="pattern" value="{{ r.pattern }}" style="width: 100%" />
        </div>
    </div>

    <div class="control-group span4">
	<label class="control-label" for="name_surname_prefix">{_ Condition _}</label>
        <div class="controls">
	    <input id="condition" type="text" name="condition" value="{{ r.condition }}" style="width: 100%" />
        </div>
    </div>
    
     <div class="control-group span1">
	<label class="control-label" for="name_surname">{_ Salience _}</label>
        <div class="controls">
	    <input id="salience" type="text" name="salience" value="{{ r.salience }}" style="width: 20%" />
        </div>
    </div>
    
     <div class="control-group span4">
	<label class="control-label" for="name_surname">{_ Rule Definition _}</label>
        <div class="controls">
	   <textarea name="action" id="action" cols="40" rows="10" class="span12 intro" >{{r.action}}</textarea><br>
        </div>
    </div>
  
  
</div>
  <div class="well">
        {% button class="btn btn-primary"  type="submit" text=_"Add Rule" %}
    </div>
    </form>
{% endwith %}
{% extends "admin_base.tpl" %}

{% block title %} Rules {% endblock %}

{% block content %}
<div class="edit-header">
    
    <h2>{_ Rules _}</h2>

   <div class="tab-pane" id="{{ #tab }}-search">
	    <div class="control-group">

                <label for="input" class="control-label">{_ Use the autocompleter to search pattern. _}</label>
                <div class="controls">
                    <input id="input" class="autocompleter span5 do_autofocus" type="text" value="" />
                    <ul id="{{#suggestions}}" class="suggestions-list"></ul>
                </div>
            </div>

            {% wire id="input"
                type="click" 
                action={search
                target=#suggestions 
                action_with_id={with_args action={link subject_id=subject_id predicate="depiction" element_id=element_id} arg={object_id select_id}
                }
	        action={postback postback={reload_media rsc_id=id div_id=media_div_id} delegate="resource_admin_edit"}
                action_with_id={with_args action={zmedia_has_chosen} arg={id select_id}}
                action={dialog_close}

                cat=m.predicate.object_category["depiction"]
	        }
            %}
                
	</div>
		


   <table class="table table-striped do_adminLinkedTable">
        <thead>
            <tr>
                <th width="20%">{_ RuleName _}</th>
                <th width="20%">{_ Pattern _}</th>
                <th width="20%">{_ Condition _}</th>
                <th width="20%">{_ Action _}</th>
                <th width="20%">{_ ClientState _}</th>
                 <th width="10%">{_ Priority _}</th>
                <th width="20%">&nbsp;</th>
            </tr>
        </thead>

        <tbody>
        
        
 {% wire id="add_rule" type="submit" postback="add_rule"%}
<form id="add_rule" method="post" action="postback" class="row">
	   <div class="row">
    <div class="control-group span2">
	<label class="control-label" for="name_first">{_ Rule Name _}</label>
        <div class="controls">
	    <input id="rule_name" type="text" name="rule_name" value="{{ r.rule_name }}" style="width: 100%" />
        </div>
    </div>

    <div class="control-group span3">
	<label class="control-label" for="name_middle">{_ Pattern _}</label>
        <div class="controls">
	    <input id="pattern" type="text" name="pattern" value="{{ r.pattern }}" style="width: 100%" />
        </div>
    </div>

    <div class="control-group span2">
	<label class="control-label" for="name_surname_prefix">{_ Condition _}</label>
        <div class="controls">
	    <input id="condition" type="text" name="condition" value="{{ r.condition }}" style="width: 100%" />
        </div>
    </div>

    <div class="control-group span2">
	<label class="control-label" for="name_surname">{_ Action _}</label>
        <div class="controls">
	    <input id="action" type="text" name="action" value="{{ r.action }}" style="width: 100%" />
        </div>
    </div>
    
     <div class="control-group span2">
	<label class="control-label" for="name_surname">{_ Client State _}</label>
        <div class="controls">
	    <input id="client_state" type="text" name="client_state" value="{{ r.client_state }}" style="width: 90%" />
        </div>
    </div>
    
      <div class="control-group span1">
	<label class="control-label" for="name_surname">{_ Salience _}</label>
        <div class="controls">
	    <input id="salience" type="text" name="salience" value="{{ r.salience }}" style="width: 20%" />
        </div>
    </div>
</div>
	  <div class="well">
        {% button class="btn btn-primary"  type="submit" text=_"Add Rule" %}
    </div>
    </form>
     {% wire id="test" type="submit" postback="add_rule"%}
<form id="test" method="post" action="postback" class="row">

	<input type="text" id="test_rule" name="test_rule" value=""/>
    <div class="well">
	   {% button type="submit" class="btn btn-primary" text=_"Test" title=_"Test" %}
     </div>    
	
	
</form>
 <div id="querypreview" class="well">
{% include "test_result.tpl" %}
	    </div>
  </div>


            {% for p in result %}
	    
            <tr id="{{ #li.name }}" data-href="{% url admin_edit_rsc id=p.id %}">
                <td>{{ p.rule_name|default:"&nbsp;" }}</td>
                <td>{{ p.pattern|default:"&nbsp;" }}</td>
                <td>{{ p.condition|default:"&nbsp;" }}</td>
                <td>{{ p.action|default:"&nbsp;" }}</td>
                <td>{{ p.client_state|default:"&nbsp;" }}</td>
                 <td>{{ p.salience|default:"&nbsp;" }}</td>
                <td>
                    <div class="pull-right">
                        {% button class="btn btn-mini" disabled=p.is_protected text="delete" action={delete id=p.id on_success={slide_fade_out target=#li.name}} %}
                        <a href="{% url admin_edit_rsc id=p.id %}" class="btn btn-mini">{_ edit _}</a>
                    </div>                        
                    {{ p.reversed|yesno:"reversed,&nbsp;" }}
                </td>
            </li>
            {% empty %}
            
            {% endfor %}
        </ul>

    </div>
</div>
{% endblock %}

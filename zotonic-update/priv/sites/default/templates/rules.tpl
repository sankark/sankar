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
                type="keyup" 
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
		
  <div id="add">  
  {% include "add_rule.tpl" %}
  </div>

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
<div id="rules_list">
{% include "rules_list.tpl" result=result %}
</div>

</div>
{% endblock %}

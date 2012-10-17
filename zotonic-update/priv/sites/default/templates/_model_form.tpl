  {% with edit_rule as r %}    
  {% wire id="model_form" type="submit" postback="add_model"%}
<form id="model_form" method="post" action="postback" class="row">
 <label for="message">{_ Model Name _}</label>
<input type="text" id="model_name" name="model_name" value="{{ r.model_name }}"/>
 <label for="message">{_ Model Definition _}</label>
	    <textarea name="model_def" id="model_def" cols="120" rows="25" >{{r.model_def}}</textarea><br>
	   {% button type="submit" class="btn btn-primary" text=_"Add Model" title=_"Test" %}
</form>
{% endwith %}
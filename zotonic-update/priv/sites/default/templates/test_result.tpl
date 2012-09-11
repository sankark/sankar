 <label for="message">{_ Client State _}</label>
	    <textarea name="test_result" id="test_result" cols="180" rows="3" >{% for p in test_result %}
{{ p}}{% endfor %}
	    </textarea>
	    
	     <label for="message">{_ Knowledge Base _}</label>
	    <textarea name="kb" id="kb" cols="180" rows="3" >{% for p in kb %}
{{ p}}{% endfor %}
	    </textarea>


-module (fipa_ontology).
-include ("fipa_ontology.hrl").
-export ([is_class/1, is_a/2, 'acl-message'/1,'action-specification'/1,'AMS-OR-DF-agent-description'/1,'result-specification'/1, childof/1]).

is_class ('agent-identifier') -> true;
is_class ('acl-message') -> true;
is_class ('ACCEPT-PROPOSAL') -> true;
is_class ('AGREE') -> true;
is_class ('CANCEL') -> true;
is_class ('CFP') -> true;
is_class ('CONFIRM') -> true;
is_class ('DISCONFIRM') -> true;
is_class ('INFORM') -> true;
is_class ('INFORM-IF') -> true;
is_class ('INFORM-REF') -> true;
is_class ('NOT-UNDERSTOOD') -> true;
is_class ('PROPAGATE') -> true;
is_class ('PROPOSE') -> true;
is_class ('PROXY') -> true;
is_class ('QUERY-IF') -> true;
is_class ('QUERY-REF') -> true;
is_class ('REFUSE') -> true;
is_class ('REJECT-PROPOSAL') -> true;
is_class ('REQUEST') -> true;
is_class ('REQUEST-WHEN') -> true;
is_class ('REQUEST-WHENEVER') -> true;
is_class ('SUBSCRIBE') -> true;
is_class ('action') -> true;
is_class ('action-specification') -> true;
is_class ('get-description') -> true;
is_class ('search') -> true;
is_class ('AMS-OR-DF-agent-description') -> true;
is_class ('ams-agent-description') -> true;
is_class ('search-constraints') -> true;
is_class ('result') -> true;
is_class ('result-specification') -> true;
is_class ('ap-service') -> true;
is_class ('ap-description') -> true;
is_class (_) -> false.

is_a ('ACCEPT-PROPOSAL','acl-message') -> true;
is_a ('AGREE','acl-message') -> true;
is_a ('CANCEL','acl-message') -> true;
is_a ('CFP','acl-message') -> true;
is_a ('CONFIRM','acl-message') -> true;
is_a ('DISCONFIRM','acl-message') -> true;
is_a ('INFORM','acl-message') -> true;
is_a ('INFORM-IF','acl-message') -> true;
is_a ('INFORM-REF','acl-message') -> true;
is_a ('NOT-UNDERSTOOD','acl-message') -> true;
is_a ('PROPAGATE','acl-message') -> true;
is_a ('PROPOSE','acl-message') -> true;
is_a ('PROXY','acl-message') -> true;
is_a ('QUERY-IF','acl-message') -> true;
is_a ('QUERY-REF','acl-message') -> true;
is_a ('REFUSE','acl-message') -> true;
is_a ('REJECT-PROPOSAL','acl-message') -> true;
is_a ('REQUEST','acl-message') -> true;
is_a ('REQUEST-WHEN','acl-message') -> true;
is_a ('REQUEST-WHENEVER','acl-message') -> true;
is_a ('SUBSCRIBE','acl-message') -> true;
is_a ('get-description','action-specification') -> true;
is_a ('search','action-specification') -> true;
is_a ('ams-agent-description','AMS-OR-DF-agent-description') -> true;
is_a ('ap-description','result-specification') -> true;
is_a (_,_) -> false.

childof ('agent-identifier') -> [];
childof ('acl-message') -> ['ACCEPT-PROPOSAL','AGREE','CANCEL','CFP',
                            'CONFIRM','DISCONFIRM','INFORM','INFORM-IF',
                            'INFORM-REF','NOT-UNDERSTOOD','PROPAGATE',
                            'PROPOSE','PROXY','QUERY-IF','QUERY-REF','REFUSE',
                            'REJECT-PROPOSAL','REQUEST','REQUEST-WHEN',
                            'REQUEST-WHENEVER','SUBSCRIBE'];
childof ('ACCEPT-PROPOSAL') -> [];
childof ('AGREE') -> [];
childof ('CANCEL') -> [];
childof ('CFP') -> [];
childof ('CONFIRM') -> [];
childof ('DISCONFIRM') -> [];
childof ('INFORM') -> [];
childof ('INFORM-IF') -> [];
childof ('INFORM-REF') -> [];
childof ('NOT-UNDERSTOOD') -> [];
childof ('PROPAGATE') -> [];
childof ('PROPOSE') -> [];
childof ('PROXY') -> [];
childof ('QUERY-IF') -> [];
childof ('QUERY-REF') -> [];
childof ('REFUSE') -> [];
childof ('REJECT-PROPOSAL') -> [];
childof ('REQUEST') -> [];
childof ('REQUEST-WHEN') -> [];
childof ('REQUEST-WHENEVER') -> [];
childof ('SUBSCRIBE') -> [];
childof ('action') -> [];
childof ('action-specification') -> ['get-description',search];
childof ('get-description') -> [];
childof ('search') -> [];
childof ('AMS-OR-DF-agent-description') -> ['ams-agent-description'];
childof ('ams-agent-description') -> [];
childof ('search-constraints') -> [];
childof ('result') -> [];
childof ('result-specification') -> ['ap-description'];
childof ('ap-service') -> [];
childof ('ap-description') -> [];
childof (_) -> exit (undef_class).

'acl-message' (X = #'ACCEPT-PROPOSAL'{}) ->
  #'acl-message'{
    'sender' = X#'ACCEPT-PROPOSAL'.'sender',
    'receiver' = X#'ACCEPT-PROPOSAL'.'receiver',
    'reply-to' = X#'ACCEPT-PROPOSAL'.'reply-to',
    'content' = X#'ACCEPT-PROPOSAL'.'content',
    'language' = X#'ACCEPT-PROPOSAL'.'language',
    'encoding' = X#'ACCEPT-PROPOSAL'.'encoding',
    'ontology' = X#'ACCEPT-PROPOSAL'.'ontology',
    'protocol' = X#'ACCEPT-PROPOSAL'.'protocol',
    'conversation-id' = X#'ACCEPT-PROPOSAL'.'conversation-id',
    'reply-with' = X#'ACCEPT-PROPOSAL'.'reply-with',
    'in-reply-to' = X#'ACCEPT-PROPOSAL'.'in-reply-to',
    'reply-by' = X#'ACCEPT-PROPOSAL'.'reply-by'};

'acl-message' (X = #'AGREE'{}) ->
  #'acl-message'{
    'sender' = X#'AGREE'.'sender',
    'receiver' = X#'AGREE'.'receiver',
    'reply-to' = X#'AGREE'.'reply-to',
    'content' = X#'AGREE'.'content',
    'language' = X#'AGREE'.'language',
    'encoding' = X#'AGREE'.'encoding',
    'ontology' = X#'AGREE'.'ontology',
    'protocol' = X#'AGREE'.'protocol',
    'conversation-id' = X#'AGREE'.'conversation-id',
    'reply-with' = X#'AGREE'.'reply-with',
    'in-reply-to' = X#'AGREE'.'in-reply-to',
    'reply-by' = X#'AGREE'.'reply-by'};

'acl-message' (X = #'CANCEL'{}) ->
  #'acl-message'{
    'sender' = X#'CANCEL'.'sender',
    'receiver' = X#'CANCEL'.'receiver',
    'reply-to' = X#'CANCEL'.'reply-to',
    'content' = X#'CANCEL'.'content',
    'language' = X#'CANCEL'.'language',
    'encoding' = X#'CANCEL'.'encoding',
    'ontology' = X#'CANCEL'.'ontology',
    'protocol' = X#'CANCEL'.'protocol',
    'conversation-id' = X#'CANCEL'.'conversation-id',
    'reply-with' = X#'CANCEL'.'reply-with',
    'in-reply-to' = X#'CANCEL'.'in-reply-to',
    'reply-by' = X#'CANCEL'.'reply-by'};

'acl-message' (X = #'CFP'{}) ->
  #'acl-message'{
    'sender' = X#'CFP'.'sender',
    'receiver' = X#'CFP'.'receiver',
    'reply-to' = X#'CFP'.'reply-to',
    'content' = X#'CFP'.'content',
    'language' = X#'CFP'.'language',
    'encoding' = X#'CFP'.'encoding',
    'ontology' = X#'CFP'.'ontology',
    'protocol' = X#'CFP'.'protocol',
    'conversation-id' = X#'CFP'.'conversation-id',
    'reply-with' = X#'CFP'.'reply-with',
    'in-reply-to' = X#'CFP'.'in-reply-to',
    'reply-by' = X#'CFP'.'reply-by'};

'acl-message' (X = #'CONFIRM'{}) ->
  #'acl-message'{
    'sender' = X#'CONFIRM'.'sender',
    'receiver' = X#'CONFIRM'.'receiver',
    'reply-to' = X#'CONFIRM'.'reply-to',
    'content' = X#'CONFIRM'.'content',
    'language' = X#'CONFIRM'.'language',
    'encoding' = X#'CONFIRM'.'encoding',
    'ontology' = X#'CONFIRM'.'ontology',
    'protocol' = X#'CONFIRM'.'protocol',
    'conversation-id' = X#'CONFIRM'.'conversation-id',
    'reply-with' = X#'CONFIRM'.'reply-with',
    'in-reply-to' = X#'CONFIRM'.'in-reply-to',
    'reply-by' = X#'CONFIRM'.'reply-by'};

'acl-message' (X = #'DISCONFIRM'{}) ->
  #'acl-message'{
    'sender' = X#'DISCONFIRM'.'sender',
    'receiver' = X#'DISCONFIRM'.'receiver',
    'reply-to' = X#'DISCONFIRM'.'reply-to',
    'content' = X#'DISCONFIRM'.'content',
    'language' = X#'DISCONFIRM'.'language',
    'encoding' = X#'DISCONFIRM'.'encoding',
    'ontology' = X#'DISCONFIRM'.'ontology',
    'protocol' = X#'DISCONFIRM'.'protocol',
    'conversation-id' = X#'DISCONFIRM'.'conversation-id',
    'reply-with' = X#'DISCONFIRM'.'reply-with',
    'in-reply-to' = X#'DISCONFIRM'.'in-reply-to',
    'reply-by' = X#'DISCONFIRM'.'reply-by'};

'acl-message' (X = #'INFORM'{}) ->
  #'acl-message'{
    'sender' = X#'INFORM'.'sender',
    'receiver' = X#'INFORM'.'receiver',
    'reply-to' = X#'INFORM'.'reply-to',
    'content' = X#'INFORM'.'content',
    'language' = X#'INFORM'.'language',
    'encoding' = X#'INFORM'.'encoding',
    'ontology' = X#'INFORM'.'ontology',
    'protocol' = X#'INFORM'.'protocol',
    'conversation-id' = X#'INFORM'.'conversation-id',
    'reply-with' = X#'INFORM'.'reply-with',
    'in-reply-to' = X#'INFORM'.'in-reply-to',
    'reply-by' = X#'INFORM'.'reply-by'};

'acl-message' (X = #'INFORM-IF'{}) ->
  #'acl-message'{
    'sender' = X#'INFORM-IF'.'sender',
    'receiver' = X#'INFORM-IF'.'receiver',
    'reply-to' = X#'INFORM-IF'.'reply-to',
    'content' = X#'INFORM-IF'.'content',
    'language' = X#'INFORM-IF'.'language',
    'encoding' = X#'INFORM-IF'.'encoding',
    'ontology' = X#'INFORM-IF'.'ontology',
    'protocol' = X#'INFORM-IF'.'protocol',
    'conversation-id' = X#'INFORM-IF'.'conversation-id',
    'reply-with' = X#'INFORM-IF'.'reply-with',
    'in-reply-to' = X#'INFORM-IF'.'in-reply-to',
    'reply-by' = X#'INFORM-IF'.'reply-by'};

'acl-message' (X = #'INFORM-REF'{}) ->
  #'acl-message'{
    'sender' = X#'INFORM-REF'.'sender',
    'receiver' = X#'INFORM-REF'.'receiver',
    'reply-to' = X#'INFORM-REF'.'reply-to',
    'content' = X#'INFORM-REF'.'content',
    'language' = X#'INFORM-REF'.'language',
    'encoding' = X#'INFORM-REF'.'encoding',
    'ontology' = X#'INFORM-REF'.'ontology',
    'protocol' = X#'INFORM-REF'.'protocol',
    'conversation-id' = X#'INFORM-REF'.'conversation-id',
    'reply-with' = X#'INFORM-REF'.'reply-with',
    'in-reply-to' = X#'INFORM-REF'.'in-reply-to',
    'reply-by' = X#'INFORM-REF'.'reply-by'};

'acl-message' (X = #'NOT-UNDERSTOOD'{}) ->
  #'acl-message'{
    'sender' = X#'NOT-UNDERSTOOD'.'sender',
    'receiver' = X#'NOT-UNDERSTOOD'.'receiver',
    'reply-to' = X#'NOT-UNDERSTOOD'.'reply-to',
    'content' = X#'NOT-UNDERSTOOD'.'content',
    'language' = X#'NOT-UNDERSTOOD'.'language',
    'encoding' = X#'NOT-UNDERSTOOD'.'encoding',
    'ontology' = X#'NOT-UNDERSTOOD'.'ontology',
    'protocol' = X#'NOT-UNDERSTOOD'.'protocol',
    'conversation-id' = X#'NOT-UNDERSTOOD'.'conversation-id',
    'reply-with' = X#'NOT-UNDERSTOOD'.'reply-with',
    'in-reply-to' = X#'NOT-UNDERSTOOD'.'in-reply-to',
    'reply-by' = X#'NOT-UNDERSTOOD'.'reply-by'};

'acl-message' (X = #'PROPAGATE'{}) ->
  #'acl-message'{
    'sender' = X#'PROPAGATE'.'sender',
    'receiver' = X#'PROPAGATE'.'receiver',
    'reply-to' = X#'PROPAGATE'.'reply-to',
    'content' = X#'PROPAGATE'.'content',
    'language' = X#'PROPAGATE'.'language',
    'encoding' = X#'PROPAGATE'.'encoding',
    'ontology' = X#'PROPAGATE'.'ontology',
    'protocol' = X#'PROPAGATE'.'protocol',
    'conversation-id' = X#'PROPAGATE'.'conversation-id',
    'reply-with' = X#'PROPAGATE'.'reply-with',
    'in-reply-to' = X#'PROPAGATE'.'in-reply-to',
    'reply-by' = X#'PROPAGATE'.'reply-by'};

'acl-message' (X = #'PROPOSE'{}) ->
  #'acl-message'{
    'sender' = X#'PROPOSE'.'sender',
    'receiver' = X#'PROPOSE'.'receiver',
    'reply-to' = X#'PROPOSE'.'reply-to',
    'content' = X#'PROPOSE'.'content',
    'language' = X#'PROPOSE'.'language',
    'encoding' = X#'PROPOSE'.'encoding',
    'ontology' = X#'PROPOSE'.'ontology',
    'protocol' = X#'PROPOSE'.'protocol',
    'conversation-id' = X#'PROPOSE'.'conversation-id',
    'reply-with' = X#'PROPOSE'.'reply-with',
    'in-reply-to' = X#'PROPOSE'.'in-reply-to',
    'reply-by' = X#'PROPOSE'.'reply-by'};

'acl-message' (X = #'PROXY'{}) ->
  #'acl-message'{
    'sender' = X#'PROXY'.'sender',
    'receiver' = X#'PROXY'.'receiver',
    'reply-to' = X#'PROXY'.'reply-to',
    'content' = X#'PROXY'.'content',
    'language' = X#'PROXY'.'language',
    'encoding' = X#'PROXY'.'encoding',
    'ontology' = X#'PROXY'.'ontology',
    'protocol' = X#'PROXY'.'protocol',
    'conversation-id' = X#'PROXY'.'conversation-id',
    'reply-with' = X#'PROXY'.'reply-with',
    'in-reply-to' = X#'PROXY'.'in-reply-to',
    'reply-by' = X#'PROXY'.'reply-by'};

'acl-message' (X = #'QUERY-IF'{}) ->
  #'acl-message'{
    'sender' = X#'QUERY-IF'.'sender',
    'receiver' = X#'QUERY-IF'.'receiver',
    'reply-to' = X#'QUERY-IF'.'reply-to',
    'content' = X#'QUERY-IF'.'content',
    'language' = X#'QUERY-IF'.'language',
    'encoding' = X#'QUERY-IF'.'encoding',
    'ontology' = X#'QUERY-IF'.'ontology',
    'protocol' = X#'QUERY-IF'.'protocol',
    'conversation-id' = X#'QUERY-IF'.'conversation-id',
    'reply-with' = X#'QUERY-IF'.'reply-with',
    'in-reply-to' = X#'QUERY-IF'.'in-reply-to',
    'reply-by' = X#'QUERY-IF'.'reply-by'};

'acl-message' (X = #'QUERY-REF'{}) ->
  #'acl-message'{
    'sender' = X#'QUERY-REF'.'sender',
    'receiver' = X#'QUERY-REF'.'receiver',
    'reply-to' = X#'QUERY-REF'.'reply-to',
    'content' = X#'QUERY-REF'.'content',
    'language' = X#'QUERY-REF'.'language',
    'encoding' = X#'QUERY-REF'.'encoding',
    'ontology' = X#'QUERY-REF'.'ontology',
    'protocol' = X#'QUERY-REF'.'protocol',
    'conversation-id' = X#'QUERY-REF'.'conversation-id',
    'reply-with' = X#'QUERY-REF'.'reply-with',
    'in-reply-to' = X#'QUERY-REF'.'in-reply-to',
    'reply-by' = X#'QUERY-REF'.'reply-by'};

'acl-message' (X = #'REFUSE'{}) ->
  #'acl-message'{
    'sender' = X#'REFUSE'.'sender',
    'receiver' = X#'REFUSE'.'receiver',
    'reply-to' = X#'REFUSE'.'reply-to',
    'content' = X#'REFUSE'.'content',
    'language' = X#'REFUSE'.'language',
    'encoding' = X#'REFUSE'.'encoding',
    'ontology' = X#'REFUSE'.'ontology',
    'protocol' = X#'REFUSE'.'protocol',
    'conversation-id' = X#'REFUSE'.'conversation-id',
    'reply-with' = X#'REFUSE'.'reply-with',
    'in-reply-to' = X#'REFUSE'.'in-reply-to',
    'reply-by' = X#'REFUSE'.'reply-by'};

'acl-message' (X = #'REJECT-PROPOSAL'{}) ->
  #'acl-message'{
    'sender' = X#'REJECT-PROPOSAL'.'sender',
    'receiver' = X#'REJECT-PROPOSAL'.'receiver',
    'reply-to' = X#'REJECT-PROPOSAL'.'reply-to',
    'content' = X#'REJECT-PROPOSAL'.'content',
    'language' = X#'REJECT-PROPOSAL'.'language',
    'encoding' = X#'REJECT-PROPOSAL'.'encoding',
    'ontology' = X#'REJECT-PROPOSAL'.'ontology',
    'protocol' = X#'REJECT-PROPOSAL'.'protocol',
    'conversation-id' = X#'REJECT-PROPOSAL'.'conversation-id',
    'reply-with' = X#'REJECT-PROPOSAL'.'reply-with',
    'in-reply-to' = X#'REJECT-PROPOSAL'.'in-reply-to',
    'reply-by' = X#'REJECT-PROPOSAL'.'reply-by'};

'acl-message' (X = #'REQUEST'{}) ->
  #'acl-message'{
    'sender' = X#'REQUEST'.'sender',
    'receiver' = X#'REQUEST'.'receiver',
    'reply-to' = X#'REQUEST'.'reply-to',
    'content' = X#'REQUEST'.'content',
    'language' = X#'REQUEST'.'language',
    'encoding' = X#'REQUEST'.'encoding',
    'ontology' = X#'REQUEST'.'ontology',
    'protocol' = X#'REQUEST'.'protocol',
    'conversation-id' = X#'REQUEST'.'conversation-id',
    'reply-with' = X#'REQUEST'.'reply-with',
    'in-reply-to' = X#'REQUEST'.'in-reply-to',
    'reply-by' = X#'REQUEST'.'reply-by'};

'acl-message' (X = #'REQUEST-WHEN'{}) ->
  #'acl-message'{
    'sender' = X#'REQUEST-WHEN'.'sender',
    'receiver' = X#'REQUEST-WHEN'.'receiver',
    'reply-to' = X#'REQUEST-WHEN'.'reply-to',
    'content' = X#'REQUEST-WHEN'.'content',
    'language' = X#'REQUEST-WHEN'.'language',
    'encoding' = X#'REQUEST-WHEN'.'encoding',
    'ontology' = X#'REQUEST-WHEN'.'ontology',
    'protocol' = X#'REQUEST-WHEN'.'protocol',
    'conversation-id' = X#'REQUEST-WHEN'.'conversation-id',
    'reply-with' = X#'REQUEST-WHEN'.'reply-with',
    'in-reply-to' = X#'REQUEST-WHEN'.'in-reply-to',
    'reply-by' = X#'REQUEST-WHEN'.'reply-by'};

'acl-message' (X = #'REQUEST-WHENEVER'{}) ->
  #'acl-message'{
    'sender' = X#'REQUEST-WHENEVER'.'sender',
    'receiver' = X#'REQUEST-WHENEVER'.'receiver',
    'reply-to' = X#'REQUEST-WHENEVER'.'reply-to',
    'content' = X#'REQUEST-WHENEVER'.'content',
    'language' = X#'REQUEST-WHENEVER'.'language',
    'encoding' = X#'REQUEST-WHENEVER'.'encoding',
    'ontology' = X#'REQUEST-WHENEVER'.'ontology',
    'protocol' = X#'REQUEST-WHENEVER'.'protocol',
    'conversation-id' = X#'REQUEST-WHENEVER'.'conversation-id',
    'reply-with' = X#'REQUEST-WHENEVER'.'reply-with',
    'in-reply-to' = X#'REQUEST-WHENEVER'.'in-reply-to',
    'reply-by' = X#'REQUEST-WHENEVER'.'reply-by'};

'acl-message' (X = #'SUBSCRIBE'{}) ->
  #'acl-message'{
    'sender' = X#'SUBSCRIBE'.'sender',
    'receiver' = X#'SUBSCRIBE'.'receiver',
    'reply-to' = X#'SUBSCRIBE'.'reply-to',
    'content' = X#'SUBSCRIBE'.'content',
    'language' = X#'SUBSCRIBE'.'language',
    'encoding' = X#'SUBSCRIBE'.'encoding',
    'ontology' = X#'SUBSCRIBE'.'ontology',
    'protocol' = X#'SUBSCRIBE'.'protocol',
    'conversation-id' = X#'SUBSCRIBE'.'conversation-id',
    'reply-with' = X#'SUBSCRIBE'.'reply-with',
    'in-reply-to' = X#'SUBSCRIBE'.'in-reply-to',
    'reply-by' = X#'SUBSCRIBE'.'reply-by'}.

'action-specification' (X = #'get-description'{}) ->
  #'action-specification'{
};

'action-specification' (X = #'search'{}) ->
  #'action-specification'{
}.

'AMS-OR-DF-agent-description' (X = #'ams-agent-description'{}) ->
  #'AMS-OR-DF-agent-description'{
}.

'result-specification' (X = #'ap-description'{}) ->
  #'result-specification'{
}.


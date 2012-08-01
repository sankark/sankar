#!/bin/sh
erlc erlflow.erl 
erlc erlflow_net.erl
erlc erlflow_xpdl_parser.erl
erlc erlflow_pnml_parser.erl   
erl -noshell -s erlflow start -s init stop 

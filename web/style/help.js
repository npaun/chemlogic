// help.js:
// This file is from Chemlogic, a logic programming computer chemistry system
// <http://icebergsystems.ca/chemlogic>
// (C) Copyright 2012-2016 Nicholas Paun


function set_paren_placeholder()
 {
	var e = document.getElementById('molar_unit');
	var selected = e.options[e.selectedIndex].value;
	var input = document.getElementById('molar_tail_input');

	if (selected == 'M')
	 {
    		input.setAttribute('placeholder','e.g. 1.43 L');
   		input.setAttribute('style','visibility: visible;');
		input.required = true;
     	 }
   	else if (selected == 'L(sol)')
   	 {
   		input.setAttribute('placeholder','e.g. 0.333 M');
   		input.setAttribute('style','visibility: visible;');
		input.required = true;
   	 }
   	else
   	 {
   		input.setAttribute('style','visibility: hidden');
		input.required = false;
		input.value = "";
   	 }
 }

function set_placeholder(module)
 {
 	var e = document.getElementById(module + '_type');
	var selected = e.options[e.selectedIndex].value;
	var input = document.getElementById(module + '_input');


	if (module == 'compounder')
	 {
		if (selected == "name")
	 		input.setAttribute('placeholder','e.g. copper(II) sulfate pentahydrate');
		else
			input.setAttribute('placeholder','e.g. H2SO4');
	 }
	 else if (module == 'molar')
	  {
	  	if (selected == 'name')
			input.setAttribute('placeholder','e.g. 0.500 L of 3.0 M hydrochloric acid');
		else
			input.setAttribute('placeholder','e.g. 46.93 g ethanol');

	 }
	else if (module == 'balancer')
	 {
	 	if (selected == 'symbolic')
			input.setAttribute('placeholder','e.g. Cu + HNO3 --> Cu(NO3)2 + NO2 + H2O');
		else
			input.setAttribute('placeholder','e.g. nonanol + oxygen --> carbon dioxide + water');
	 }
	else if (module == 'stoichiometer')
	 {
		if (selected == 'symbolic')
			input.setAttribute('placeholder','e.g. 15.00 L CH4 + 20.00 L O2 --> CO2 + H2O');
		else
			input.setAttribute('placeholder','e.g oxalic acid + 0.125 L of 1.000 M sodium hydroxide --> sodium oxalate + 300 g water');
	}
 }

function set_handler()
 {
 	var module = window.location.pathname.split(/[\\/]/).pop();

	if (!module)
		module = 'balancer';

 	document.getElementById(module + '_type').addEventListener('change', function() {
		set_placeholder(module)
	});
	set_placeholder(module);

	if (module == 'molar')
	 {
		document.getElementById('molar_unit').addEventListener('change',set_paren_placeholder);
		set_paren_placeholder();
	 }
 }

set_handler();

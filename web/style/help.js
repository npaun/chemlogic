// help.js: Switches form placeholders to provide users with examples for the currently selected mode
// This file is from Chemlogic, a logic programming computer chemistry system
// <http://icebergsystems.ca/chemlogic>
// (C) Copyright 2012-2016 Nicholas Paun


function set_paren_placeholder() {
	var e = document.getElementById('molar_unit');
	var selected = e.options[e.selectedIndex].value;
	var input = document.getElementById('molar_tail_input');

	if (selected == 'M') {
		input.setAttribute('placeholder','e.g. 1.43 L');
		input.setAttribute('style','visibility: visible;');
		input.required = true;
     	 } else if (selected == 'L(sol)') {
		input.setAttribute('placeholder','e.g. 0.333 M');
		input.setAttribute('style','visibility: visible;');
		input.required = true;
	} else {
		input.setAttribute('style','visibility: hidden;');
		input.required = false;
		input.value = "";
	}
 }

function set_placeholder(module) {
	var e = document.getElementById(module + '_type');
	var selected = e.options[e.selectedIndex].value;
	var input = document.getElementById(module + '_input');

	var placeholders = {
		compounder: {
			name: 'copper(II) sulfate pentahydrate',
			formula: 'H2SO4',
		},
		molar: {
			name: '0.500 L of 3.0 M hydrochloric acid',
			formula: '46.93 g ethanol',
		},
		balancer: {
			word: 'nonanol + oxygen --> carbon dioxide + water',
			symbolic: 'Cu + HNO3 --> Cu(NO3)2 + NO2 + H2O',
		},
		stoichiometer: {
			word: 'oxalic acid + 0.125 L of 1.000 M sodium hydroxide --> sodium oxalate + 300 g water',
			symbolic: '15.00 L CH4 + 20.00 L O2 --> CO2 + H2O',
		},
	};
	
	input.setAttribute('placeholder', 'e.g. ' + placeholders[module][selected]);			
 }

function set_handler() {
	var module = window.location.pathname.split(/\//).pop() || 'balancer';

	document.getElementById(module + '_type').addEventListener('change', function() {
		set_placeholder(module);
	});
	set_placeholder(module);

	if (module == 'molar') {
		document.getElementById('molar_unit').addEventListener('change',set_paren_placeholder);
		set_paren_placeholder();
	}
}

set_handler();

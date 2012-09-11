$(function() {
		var width = $('ul.tweets p').outerWidth();

		// Looping through the p elements
		// and calling the splitLines plugin

		$('ul.tweets p').each(function() {
			$(this).addClass('sliced').splitLines({	width : width});
		});
	});
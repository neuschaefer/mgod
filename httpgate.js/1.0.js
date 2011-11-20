$(document).bind("mobileinit", function() {
	$.mobile.touchOverflowEnabled = true;
	$.mobile.defaultPageTransition = 'none';
	$.mobile.loadingMessage = 'loading ~';
	$("li").unbind('mouseover mouseout');
});

var selectPage = function(sel, links, page) {
	$.mobile.changePage("?1" + sel + ":" + (links * page));
};

var setPager = function(curpg, maxpg, links, sel) {
	var res = '<div data-type="horizontal" data-role="controlgroup">';
	var i;

	if(curpg > 0) {
		res += '<a href="?1' + sel + ':' + ((curpg-1)*links) + '" data-role="button">&lt;&lt;</a>';
	}

	res += '<select name="page-selector" class="select" onchange="selectPage(\'' + sel + '\', ' + links + ', this.selectedIndex)">';
	for(i=0; i<maxpg; i++) {
		res += '<option value="' + i +'" ';
		if(i == curpg) res += ' selected';
		res += '>' + (i+1) + '</option>';
	}
	res += '</select>';

	if(curpg < maxpg-1) {
		res += '<a href="?1' + sel + ':' + ((curpg+1)*links) + '" data-role="button">&gt;&gt;</a>';
	}

	res += '</div>';
	$("div.pager").replaceWith(res);
	$("div.pager").show();
};

# usage:
# 1. edit rss settings below
# 2. arrange for some http request to get the result of:
# echo | /path/to/mgod <settings> | awk -f /path/to/rss.awk

BEGIN {
	FS="\t"

	title = "k-zed phlogja"
	base = "gopher://hactar.net/"
	at = "blog"
	desc = "k-zed phlogja"
	itemlimit = 16

	print "<?xml version=\"1.0\"?>"
	print "<rss version=\"2.0\">"
	print "<channel>"
	print " <title>" title "</title>"
	print " <link>" base "1" at "</link>"
	print " <description>" desc "</description>"
	print " <generator>awk :)</generator>"
}

{
	if(itemlimit > 0) {
		itemlimit --

		type = substr($1, 1, 1)
		title = substr($1, 2)
		selector = $2

		print " <item>"
		print "  <title>" title "</title>"
		print "  <link>" base type at "/" selector "</link>"
		print " </item>"
	}
}

END {
	print "</channel>"
	print "</rss>"
}

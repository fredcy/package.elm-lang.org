watch:
	fswatch $$(find src/frontend -name "*.elm") | \
	while read f; do \
	    echo; echo; \
	    echo $$f; \
	    elm make src/frontend/Page/PackageOverview.elm --yes --output=artifacts/Page-PackageOverview.js; \
	    (cd test; elm make Test.elm --yes); \
	done

browse:
	browser-sync start --server --startPath tmp/elm-signal-extra.html --files artifacts/Page-PackageOverview.js
